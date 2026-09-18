-- | Merge: rewrite the immutable data files keeping only live records.
--
-- Merge runs concurrently with readers /and/ writers. It never takes the write
-- lock for longer than it takes to roll the active file, and it never blocks a
-- read at all. Two things make that work.
--
-- The first is the compare-on-location update. When merge copies a record it
-- installs the new location with 'atomicModifyIORef'' only if the keydir still
-- points at the old one. A 'Database.Bitcask.put' that raced the merge has
-- already moved the entry, so the compare fails and the copied record is simply
-- dead on arrival.
--
-- The second is file id allocation. Merge output holds records that are /older/
-- than anything written while the merge was running, so on the next open it has
-- to replay first. Data file ids are a @(base, sub)@ pair for exactly this
-- reason: merge allocates @(base of the newest input, next sub)@, which sorts
-- after every input and strictly before the active file. A single flat counter
-- has no id to give it, and the store would come back from a restart with stale
-- values. See @DESIGN.md@ §3.1.
--
-- v1 merges every immutable file at once, which is what makes dropping
-- tombstones trivially correct: a tombstone can only be dropped once no older
-- file can still hold a value for that key, and after a full merge there are no
-- older files. Merging a subset is a later refinement and needs tombstone
-- retention rules.
module Database.Bitcask.Internal.Merge
  ( mergeStore
  , shouldMerge
  ) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad (foldM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.List as L
import Data.Word (Word64)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Set as Set

import Database.Bitcask.Internal.File (dataPath, hintPath, listDataFiles, readPending, writePending)
import Database.Bitcask.Internal.Hint (HintEntry (..), encodeHintEntry)
import qualified Database.Bitcask.Internal.Keydir as KD
import Database.Bitcask.Internal.Platform (appendBytes, preadAt, removeOpen)
import Database.Bitcask.Internal.Record (Record (..), decodeRecord)
import Database.Bitcask.Internal.Store
import Database.Bitcask.Types

-- | Should the automatic merge policy fire?
shouldMerge :: Store -> IO Bool
shouldMerge st = case mergePolicy (stOpts st) of
  MergeManual -> pure False
  MergeAuto ratio _ -> do
    active <- readMVar (stActive st)
    let activeFid = fmap acFileId active
    totals <- readIORef (stTotal st)
    dead <- readIORef (stDead st)
    let immutable = M.filterWithKey (\f _ -> Just f /= activeFid) totals
        total = sum (M.elems immutable)
        deadBytes = sum (M.elems (M.intersection dead immutable))
    pure (total > 0 && fromIntegral deadBytes / fromIntegral total >= ratio)

mergeStore :: Store -> IO MergeStats
mergeStore st = withMVar (stMergeGate st) $ \() -> do
  assertOpen st
  when (readOnly (stOpts st)) $ throwIO WriteToReadOnly

  -- Roll first, so that every file we are about to read is immutable for the
  -- whole merge. This is the only point where merge touches the write path.
  activeFid <- modifyMVar (stActive st) $ \case
    Nothing -> throwIO WriteToReadOnly
    Just ac -> do
      ac' <- if acOffset ac > 0 then rollActive st ac else pure ac
      pure (Just ac', acFileId ac')

  allFids <- listDataFiles (stDir st)
  let inputs = filter (< activeFid) allFids
      inputSet = Set.fromList inputs
  if null inputs
    then pure (MergeStats 0 0 0)
    else do
      totalsBefore <- readIORef (stTotal st)
      let reclaimable = sum [M.findWithDefault 0 f totalsBefore | f <- inputs]
          outBase = fileBase (maximum inputs)
          usedSubs = [fileSub f | f <- allFids, fileBase f == outBase]
          firstSub = if null usedSubs then 1 else maximum usedSubs + 1

      -- Every live record that still lives in an input file. Reading the keydir
      -- rather than scanning the inputs means dead records and tombstones are
      -- never even looked at.
      --
      -- They are read with 'sweepRecords', which visits them roughly in file
      -- and offset order a window at a time. The keydir is a hash table, so its
      -- own order is random, and reading the inputs in it would turn a sequential
      -- pass over each file into a random read per record.
      kd <- readIORef (stKeydir st)
      let todo = [(k, l) | (k, l) <- KD.toList kd, locFileId l `Set.member` inputSet]

      out0 <- openActive (stDir st) (mkFileId outBase firstSub)
      bumpTotal st (acFileId out0) 0
      out <- sweepRecords st (copyOne st) (Out out0 [] [] 0 0) todo >>= flushOut st
      closeActive st (outActive out)

      -- Nothing in the keydir can reference an input any more: new writes only
      -- ever land in the active file, so every reference either moved above or
      -- was superseded while we worked.
      removeInputs st inputs

      pure
        MergeStats
          { mergedFiles = length inputs
          , mergedRecords = outCopied out
          , reclaimedBytes = reclaimable
          }

-- | The merge output, and the copies written to it that are not yet visible.
--
-- Copied records are written in blocks rather than one at a time, for the same
-- reason reads are swept: the system call per record, not the copying, is what a
-- record-at-a-time merge spends its time on. A copy must be on disk before the
-- keydir is pointed at it, so the keydir claims for a block are made when the
-- block is written, all in one update.
data Out = Out
  { outActive :: !Active
  -- ^ its offset already counts the pending bytes
  , outPending :: ![Claim]
  -- ^ newest first
  , outPendingBytes :: ![ByteString]
  -- ^ newest first
  , outPendingLen :: !Int
  , outCopied :: !Int
  }

-- | Move a key from one location to another, if it is still at the first.
data Claim = Claim !ByteString !Loc !Loc

-- | Copy one live record into the merge output.
copyOne :: Store -> Out -> ByteString -> Loc -> Maybe ByteString -> IO Out
copyOne st out0 k loc swept = do
  -- A sweep read that failed or came up short gets one careful retry on its
  -- own; that one is allowed to throw.
  bytes <- case swept of
    Just b -> pure b
    Nothing -> withReader st (locFileId loc) $ \rh ->
      preadAt rh (locPos loc) (fromIntegral (locSize loc))
  if BS.length bytes < fromIntegral (locSize loc)
    then pure out0 -- the entry went stale under us; the newer write wins
    else case decodeRecord (verifyChecksums (stOpts st)) bytes of
      Left err -> throwIO (CorruptRecord (dataPath (stDir st) (locFileId loc)) (locPos loc) err)
      Right r
        | recKey r /= k || isNothing (recValue r) -> pure out0
        | otherwise -> do
            out <-
              if needsRoll (outActive out0) (BS.length bytes)
                then do
                  o <- flushOut st out0
                  a <- rollMergeOutput st (outActive o)
                  pure o {outActive = a}
                else pure out0
            let ac = outActive out
                off = acOffset ac
                hintBytes =
                  encodeHintEntry
                    HintEntry
                      { hintTstamp = recTstamp r
                      , hintTombstone = False
                      , hintKey = k
                      , hintValSize = maybe 0 (fromIntegral . BS.length) (recValue r)
                      , hintPos = off
                      , hintRecSize = locSize loc
                      }
                newLoc = Loc (acFileId ac) off (locSize loc) (recTstamp r)
            ac' <- appendHint ac {acOffset = off + fromIntegral (BS.length bytes)} hintBytes
            let out' =
                  out
                    { outActive = ac'
                    , outPending = Claim k loc newLoc : outPending out
                    , outPendingBytes = bytes : outPendingBytes out
                    , outPendingLen = outPendingLen out + BS.length bytes
                    , outCopied = outCopied out + 1
                    }
            if outPendingLen out' >= mergeBlockSize then flushOut st out' else pure out'
  where
    needsRoll ac len =
      acOffset ac > 0 && acOffset ac + fromIntegral len > maxFileSize (stOpts st)

-- | Write the pending copies, then point the keydir at them.
flushOut :: Store -> Out -> IO Out
flushOut st out
  | null (outPending out) = pure out
  | otherwise = do
      let ac = outActive out
          fid = acFileId ac
      appendBytes (acData ac) (BS.concat (reverse (outPendingBytes out)))
      -- Compare-on-location: install each new place only if nobody moved the key
      -- while we were copying it. A put that got there first wins, and the copy
      -- is dead on arrival.
      lost <- atomicModifyIORef' (stKeydir st) $ \kd0 ->
        let claim (kd, dead) (Claim k old new) = case KD.replaceIf k old new kd of
              (kd', True) -> (kd', dead)
              (_, False) -> (kd, dead + fromIntegral (locSize new))
         in L.foldl' claim (kd0, 0 :: Word64) (reverse (outPending out))
      bumpTotal st fid (fromIntegral (outPendingLen out))
      when (lost > 0) $ bumpDead st fid lost
      pure out {outPending = [], outPendingBytes = [], outPendingLen = 0}

-- | How much copied data merge collects before writing it out.
mergeBlockSize :: Int
mergeBlockSize = 256 * 1024

-- | Roll the merge output to the next sub-sequence id at the same base, which
-- keeps it sorting before the active file.
rollMergeOutput :: Store -> Active -> IO Active
rollMergeOutput st out = do
  closeActive st out
  let fid = acFileId out
      next = mkFileId (fileBase fid) (fileSub fid + 1)
  out' <- openActive (stDir st) next
  bumpTotal st next 0
  pure out'

-- | Retire the merged-away files.
--
-- Their read handles are /not/ closed here. A reader may be inside a positional
-- read on one right now, and closing the descriptor under it would at best fail
-- and at worst read a recycled descriptor. They are retired from the lookup map
-- and closed when the store closes.
--
-- On POSIX the unlink then succeeds regardless, because the inode outlives the
-- last descriptor. On Windows it does not, so the file id goes into
-- @bitcask.pending@ and is swept at the next open.
removeInputs :: Store -> [FileId] -> IO ()
removeInputs st inputs = do
  stuck <- foldM removeOne [] inputs
  atomicModifyIORef' (stTotal st) (\m -> (foldr M.delete m inputs, ()))
  atomicModifyIORef' (stDead st) (\m -> (foldr M.delete m inputs, ()))
  when (not (null stuck)) $ do
    existing <- readPending (stDir st)
    writePending (stDir st) (existing <> reverse stuck)
  where
    removeOne acc fid = do
      retireReader st fid
      okHint <- removeOpen (hintPath (stDir st) fid)
      okData <- removeOpen (dataPath (stDir st) fid)
      pure (if okData && okHint then acc else fid : acc)
