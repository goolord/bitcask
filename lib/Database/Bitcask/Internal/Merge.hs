-- | Merge: rewrite the immutable data files keeping only live records.
--
-- Merge runs alongside readers and writers. It only holds the write lock long
-- enough to roll the active file, and never blocks reads.
--
-- Copied records are installed with a compare-on-location update: the keydir
-- is only changed if it still points at the old location. If a
-- 'Database.Bitcask.put' got there first, the copy is just dead.
--
-- Merge output holds records older than anything written during the merge, so
-- it has to be replayed first on open. That's why file ids are @(base, sub)@:
-- merge uses @(base of newest input, next sub)@, which sorts after all inputs
-- and before the active file. With a flat counter there'd be no id that works,
-- and a restart would bring back stale values. See @DESIGN.md@ §3.1.
--
-- Every immutable file is merged at once. That makes dropping tombstones safe,
-- since no older file can still have a value for the key. Partial merges would
-- need tombstone retention rules.
module Database.Bitcask.Internal.Merge
  ( mergeStore
  , shouldMerge
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, mask_, onException, throwIO, try)
import Control.Monad (foldM, void, when)
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
import Database.Bitcask.Internal.Platform (closeAppend, preadAt, removeOpen)
import Database.Bitcask.Internal.Record (Record (..), decodeRecord)
import Database.Bitcask.Internal.Store
import Database.Bitcask.Types

-- | Whether 'MergeAuto' should run a merge now.
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

  -- Roll first so every input stays immutable for the whole merge. This is the
  -- only time merge touches the write path. Fails on a broken store.
  activeFid <- withActive st $ \ac ->
    if acOffset ac > 0
      then do
        (ac', err) <- rollActive st ac
        pure (ac', maybe (Right (acFileId ac')) Left err)
      else pure (ac, Right (acFileId ac))

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

      -- Live records in the input files. Going from the keydir instead of
      -- scanning the inputs skips dead records and tombstones.
      --
      -- 'sweepRecords' reads them roughly in file order. Keydir order is
      -- random, which would mean a random read per record.
      kd <- readIORef (stKeydir st)
      let todo = [(k, l) | (k, l) <- KD.toList kd, locFileId l `Set.member` inputSet]

      -- The open output file, so a failed merge can close it; see
      -- 'abandonOutput'.
      current <- newIORef Nothing
      out <-
        ( do
            out0 <- mask_ $ do
              o <- openActive (stDir st) (mkFileId outBase firstSub)
              writeIORef current (Just o)
              pure o
            bumpTotal st (acFileId out0) 0
            out <- sweepRecords st (copyOne st current) (Out out0 [] [] 0 0) todo >>= flushOut st
            finishOutput st current (outActive out)
            pure out
        )
          `onException` (readIORef current >>= mapM_ (abandonOutput st))

      -- The output is durable ('finishOutput' throws otherwise). Nothing in the
      -- keydir points at an input now: every entry was either moved above or
      -- overwritten by a write to the active file.
      removeInputs st inputs

      pure
        MergeStats
          { mergedFiles = length inputs
          , mergedRecords = outCopied out
          , reclaimedBytes = reclaimable
          }

-- | The merge output and the copies not yet written to it.
--
-- Copies are written in blocks since the per-record syscall dominates
-- otherwise. A copy has to be written before the keydir points at it, so each
-- block's claims are applied in one update right after the block is written.
data Out = Out
  { outActive :: !Active
  -- ^ offset includes the pending bytes
  , outPending :: ![Claim]
  -- ^ newest first
  , outPendingBytes :: ![ByteString]
  -- ^ newest first
  , outPendingLen :: !Int
  , outCopied :: !Int
  }

-- | Move a key from one location to another if it's still at the first.
data Claim = Claim !ByteString !Loc !Loc

-- | Copy one live record into the merge output.
copyOne :: Store -> IORef (Maybe Active) -> Out -> ByteString -> Loc -> Maybe ByteString -> IO Out
copyOne st current out0 k loc swept = do
  -- If the sweep read failed or was short, retry this record on its own. That
  -- one can throw.
  bytes <- case swept of
    Just b -> pure b
    Nothing -> withReader st (locFileId loc) $ \rh ->
      preadAt rh (locPos loc) (fromIntegral (locSize loc))
  if BS.length bytes < fromIntegral (locSize loc)
    then pure out0 -- stale entry; the newer write wins
    else case decodeRecord (verifyChecksums (stOpts st)) bytes of
      Left err -> throwIO (CorruptRecord (dataPath (stDir st) (locFileId loc)) (locPos loc) err)
      Right r
        | recKey r /= k || isNothing (recValue r) -> pure out0
        | otherwise -> do
            out <-
              if needsRoll (outActive out0) (BS.length bytes)
                then do
                  o <- flushOut st out0
                  a <- rollMergeOutput st current (outActive o)
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
            ac' <- appendHint st ac {acOffset = off + fromIntegral (BS.length bytes)} hintBytes
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
      appendData st (acData ac) (BS.concat (reverse (outPendingBytes out)))
      -- Only move keys that haven't changed since we copied them. If a put got
      -- there first, the copy is dead.
      lost <- atomicModifyIORef' (stKeydir st) $ \kd0 ->
        let claim (kd, dead) (Claim k old new) = case KD.replaceIf k old new kd of
              (kd', True) -> (kd', dead)
              (_, False) -> (kd, dead + fromIntegral (locSize new))
         in L.foldl' claim (kd0, 0 :: Word64) (reverse (outPending out))
      bumpTotal st fid (fromIntegral (outPendingLen out))
      when (lost > 0) $ bumpDead st fid lost
      pure out {outPending = [], outPendingBytes = [], outPendingLen = 0}

-- | How much copied data to buffer before writing.
mergeBlockSize :: Int
mergeBlockSize = 256 * 1024

-- | Roll the merge output to the next sub id at the same base, so it still
-- sorts before the active file.
rollMergeOutput :: Store -> IORef (Maybe Active) -> Active -> IO Active
rollMergeOutput st current out = do
  finishOutput st current out
  let fid = acFileId out
      next = mkFileId (fileBase fid) (fileSub fid + 1)
  out' <- mask_ $ do
    o <- openActive (stDir st) next
    writeIORef current (Just o)
    pure o
  bumpTotal st next 0
  pure out'

-- | Finish a merge output file. Throws if it couldn't be synced.
--
-- Cleared from @current@ before closing, since 'closeActive' always closes the
-- handles and 'abandonOutput' mustn't close them again.
finishOutput :: Store -> IORef (Maybe Active) -> Active -> IO ()
finishOutput st current out = do
  failed <- mask_ $ do
    writeIORef current Nothing
    closeActive st out
  mapM_ throwIO failed

-- | Close a merge output after the merge failed.
--
-- The data file stays, since some of its records are already in the keydir.
-- The hint file is deleted: hint entries are added before their block is
-- written, so it may list records that never made it. Without a hint, the next
-- open scans the file and stops at the bad tail.
abandonOutput :: Store -> Active -> IO ()
abandonOutput st out = do
  quietly (closeAppend (acHint out))
  quietly (closeAppend (acData out))
  _ <- removeOpen (hintPath (stDir st) (acFileId out))
  pure ()
  where
    quietly act = void (try act :: IO (Either SomeException ()))

-- | Delete the merged input files.
--
-- Read handles aren't closed here since a reader could be mid-pread; see
-- 'retireReader'. They're closed when the store closes.
--
-- On POSIX the unlink works anyway. On Windows it can fail, so the file id goes
-- into @bitcask.pending@ and gets cleaned up on the next open.
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
