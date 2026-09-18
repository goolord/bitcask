-- | The store handle and the operations on raw encoded bytes.
--
-- Everything here works in terms of already-encoded keys and values; the typed
-- layer in "Database.Bitcask" is a thin wrapper that encodes on the way in and
-- decodes on the way out.
module Database.Bitcask.Internal.Store
  ( Store (..)
  , Active (..)
  , Readers

    -- * Lifecycle
  , openStore
  , closeStore

    -- * Operations
  , getRaw
  , putRaw
  , deleteRaw
  , memberRaw
  , keysRaw
  , foldRaw
  , foldRefsRaw
  , syncStore
  , statsStore

    -- * For "Database.Bitcask.Internal.Merge"
  , withReader
  , retireReader
  , bumpDead
  , fetchAt
  , sweepRecords
  , rollActive
  , openActive
  , closeActive
  , appendHint
  , withActive
  , markBroken
  , appendData
  , bumpTotal
  , setDead
  , assertOpen

    -- * Fault injection, for tests
  , Fault (..)
  , injectFault
  ) where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Applicative ((<|>))
import Control.Exception (IOException, SomeAsyncException, SomeException, bracketOnError, fromException, throwIO, toException, try)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Either (isRight)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.Word (Word32, Word64)
import System.Directory (createDirectoryIfMissing, getFileSize, removeFile)

import Database.Bitcask.Internal.CRC32 (crc32Update)
import Database.Bitcask.Internal.File
import Database.Bitcask.Internal.Hint (HintEntry (..), encodeHintEntry, encodeHintTrailer)
import Database.Bitcask.Internal.Keydir (Keydir)
import qualified Database.Bitcask.Internal.Keydir as KD
import Database.Bitcask.Internal.Lock (LockMode (..), StoreLock, acquireLock, releaseLock)
import Database.Bitcask.Internal.Platform hiding (LockMode (..), dropLock, takeLock)
import Database.Bitcask.Internal.Record (Record (..), decodeRecord, encodeRecord)
import Database.Bitcask.Types

-- | The active data file: the one being appended to, and its hint file.
data Active = Active
  { acFileId :: !FileId
  , acData :: !AppendHandle
  , acHint :: !AppendHandle
  , acOffset :: !Word64
  , acHintCrc :: !Word32
  , acHintCount :: !Word64
  , acHintBuf :: ![ByteString]
  -- ^ hint entries not yet written, newest first; see 'appendHint'
  , acHintBufLen :: !Int
  , acHintOk :: !Bool
  -- ^ cleared if a hint write fails; the file then gets no hint, see 'flushHint'
  }

-- | Read handles for the data files, by id.
--
-- The map is read without a lock, because every 'Database.Bitcask.get' needs a
-- handle and taking a lock there would serialise all readers on it. The lock is
-- only for changing the map, so that two threads that miss at once do not both
-- open the file.
data Readers = Readers
  { rdMap :: !(IORef (Map FileId ReadHandle))
  , rdLock :: !(MVar ())
  }

data Store = Store
  { stDir :: !FilePath
  , stOpts :: !OpenOptions
  , stKeydir :: !(IORef Keydir)
  -- ^ read without a lock; updated with 'atomicModifyIORef''
  , stActive :: !(MVar (Maybe Active))
  -- ^ 'Nothing' for a read-only store; taking it serialises writes
  , stReaders :: !Readers
  , stLock :: !StoreLock
  , stClosed :: !(IORef Bool)
  , stTotal :: !(IORef (Map FileId Word64))
  -- ^ bytes written per data file
  , stDead :: !(IORef (Map FileId Word64))
  -- ^ bytes per data file no longer reachable from the keydir; a heuristic that
  -- only drives the merge trigger
  , stWrites :: !(IORef Int)
  , stMergeGate :: !(MVar ())
  -- ^ one merge at a time
  , stThreads :: !(IORef [ThreadId])
  , stRetired :: !(IORef [ReadHandle])
  -- ^ handles for files merge has removed; see 'retireReader'
  , stBroken :: !(IORef (Maybe String))
  -- ^ why writes are refused, once they are; see 'markBroken'
  , stFaults :: !(IORef [Fault])
  -- ^ armed test faults; see 'Fault'
  }

nowNanos :: IO Word64
nowNanos = do
  MkSystemTime s ns <- getSystemTime
  pure (fromIntegral s * 1000000000 + fromIntegral ns)

assertOpen :: Store -> IO ()
assertOpen st = do
  closed <- readIORef (stClosed st)
  when closed $ throwIO UseAfterClose

-- ---------------------------------------------------------------------------
-- Opening

-- | Open (or create) a store.
--
-- A fresh active file is started on every open rather than appending to the
-- previous one. It costs one small file per open — merge folds them away — and
-- buys a simple invariant: a file that is not the active file is never appended
-- to again, so a torn tail can only ever exist at the end of one file.
openStore :: FilePath -> OpenOptions -> IO Store
openStore dir opts = do
  createDirectoryIfMissing True dir
  checkMeta dir opts
  lockRes <- acquireLock dir (if readOnly opts then LockShared else LockExclusive)
  lock <- case lockRes of
    Left holder -> throwIO (LockHeld dir holder)
    Right l -> pure l
  bracketOnError (pure lock) releaseLock $ \_ -> do
    unless (readOnly opts) (sweepPending dir)
    readers <- Readers <$> newIORef M.empty <*> newMVar ()
    fids <- listDataFiles dir
    sizes <- mapM (\f -> (,) f . fromIntegral <$> getFileSize (dataPath dir f)) fids
    let totals = M.fromList sizes
    kd <- rebuild dir opts readers fids
    kdRef <- newIORef kd
    totalRef <- newIORef totals
    deadRef <- newIORef (deadFrom totals kd)
    closedRef <- newIORef False
    writesRef <- newIORef 0
    gate <- newMVar ()
    threads <- newIORef []
    retired <- newIORef []
    broken <- newIORef Nothing
    faults <- newIORef []
    active <-
      if readOnly opts
        then newMVar Nothing
        else do
          let base = if null fids then 1 else fileBase (maximum fids) + 1
          ac <- openActive dir (mkFileId base 0)
          modifyIORef' totalRef (M.insert (acFileId ac) 0)
          newMVar (Just ac)
    pure
      Store
        { stDir = dir
        , stOpts = opts
        , stKeydir = kdRef
        , stActive = active
        , stReaders = readers
        , stLock = lock
        , stClosed = closedRef
        , stTotal = totalRef
        , stDead = deadRef
        , stWrites = writesRef
        , stMergeGate = gate
        , stThreads = threads
        , stRetired = retired
        , stBroken = broken
        , stFaults = faults
        }

-- | Bytes per file that the keydir does not reach.
deadFrom :: Map FileId Word64 -> Keydir -> Map FileId Word64
deadFrom totals kd = M.unionWith sub totals live
  where
    live = KD.foldlLocs' add M.empty kd
    add m l = M.insertWith (+) (locFileId l) (fromIntegral (locSize l)) m
    sub total alive = if total > alive then total - alive else 0

checkMeta :: FilePath -> OpenOptions -> IO ()
checkMeta dir opts = do
  existing <- readMeta dir
  case existing of
    Nothing -> unless (readOnly opts) (writeMeta dir (storeTag opts))
    Just (ver, tag) -> do
      when (ver /= formatVersion) $ throwIO (UnsupportedFormat dir ver)
      case (storeTag opts, tag) of
        (Just want, found)
          | Just want /= found ->
              throwIO (SchemaMismatch dir want (fromMaybe T.empty found))
        _ -> pure ()

-- | Rebuild the keydir, preferring hint files and falling back to a full scan.
--
-- Files are visited in ascending id order, which is write order, so the last ref
-- for a key simply wins. See "Database.Bitcask.Internal.Keydir".
rebuild :: FilePath -> OpenOptions -> Readers -> [FileId] -> IO Keydir
rebuild dir opts readers fids = foldM one KD.empty (zip fids (repeat ()))
  where
    lastFid = if null fids then Nothing else Just (maximum fids)
    one kd (fid, ()) = do
      mhint <- readHintRefs dir fid
      case mhint of
        Just refs -> pure $! L.foldl' (flip KD.applyRef) kd refs
        Nothing -> do
          rh <- cachedReader dir readers fid
          size <- fromIntegral <$> getFileSize (dataPath dir fid)
          (kd', torn) <-
            foldDataFile
              (verifyChecksums opts)
              (dataPath dir fid)
              fid
              rh
              size
              (flip KD.applyRef)
              kd
          forM_ torn $ \at ->
            if repairTruncated opts && not (readOnly opts) && Just fid == lastFid
              then do
                -- Only the newest file can hold a torn write, and only a writer
                -- may repair it.
                closeReaderFor readers fid
                truncateAt (dataPath dir fid) at
              else pure ()
          pure $! kd'

-- | The read handle for a file, opening it on first use. Lock-free when the
-- handle is already open, which is every time but the first.
cachedReader :: FilePath -> Readers -> FileId -> IO ReadHandle
cachedReader dir rd fid = do
  m <- readIORef (rdMap rd)
  case M.lookup fid m of
    Just rh -> pure rh
    Nothing -> withMVar (rdLock rd) $ \() -> do
      -- Someone may have opened it while we waited for the lock.
      m' <- readIORef (rdMap rd)
      case M.lookup fid m' of
        Just rh -> pure rh
        Nothing -> do
          rh <- openRead (dataPath dir fid)
          atomicModifyIORef' (rdMap rd) (\m'' -> (M.insert fid rh m'', ()))
          pure rh

-- | Forget a file's read handle, handing it back if there was one.
dropReader :: Readers -> FileId -> IO (Maybe ReadHandle)
dropReader rd fid = withMVar (rdLock rd) $ \() ->
  atomicModifyIORef' (rdMap rd) (\m -> (M.delete fid m, M.lookup fid m))

closeReaderFor :: Readers -> FileId -> IO ()
closeReaderFor rd fid = dropReader rd fid >>= mapM_ closeRead

-- | Delete files a previous merge could not remove because a reader still held
-- them open. Only Windows ever leaves these behind.
sweepPending :: FilePath -> IO ()
sweepPending dir = do
  fids <- readPending dir
  unless (null fids) $ do
    forM_ fids $ \fid -> do
      _ <- removeOpen (dataPath dir fid)
      _ <- removeOpen (hintPath dir fid)
      pure ()
    _ <- try (removeFile (pendingPath dir)) :: IO (Either IOException ())
    pure ()

openActive :: FilePath -> FileId -> IO Active
openActive dir fid = do
  (dh, off) <- openAppend (dataPath dir fid)
  (hh, _) <- openAppend (hintPath dir fid)
  syncDir dir
  pure
    Active
      { acFileId = fid
      , acData = dh
      , acHint = hh
      , acOffset = off
      , acHintCrc = 0
      , acHintCount = 0
      , acHintBuf = []
      , acHintBufLen = 0
      , acHintOk = True
      }

-- | Add an entry to the active file's hint file.
--
-- Hint entries are buffered and written in blocks, which saves the write path one
-- system call per record — roughly half of what a 'Database.Bitcask.put' costs.
-- Unlike the data file, nothing reads a hint file until the file is finished and
-- its trailer written, so there is nobody to see the difference; and a crash
-- that loses the buffer loses nothing, because a hint file without its trailer is
-- ignored in favour of scanning the data file.
--
-- Throws nothing but asynchronous exceptions: see 'flushHint'.
appendHint :: Store -> Active -> ByteString -> IO Active
appendHint st ac bytes
  | not (acHintOk ac) = pure ac
  | otherwise = do
      let ac' =
            ac
              { acHintCrc = crc32Update (acHintCrc ac) bytes
              , acHintCount = acHintCount ac + 1
              , acHintBuf = bytes : acHintBuf ac
              , acHintBufLen = acHintBufLen ac + BS.length bytes
              }
      if acHintBufLen ac' >= hintBufferSize then flushHint st ac' else pure ac'

-- | Write out buffered hint entries.
--
-- A hint file is a cache, so a failure to write one must never fail the write
-- that caused it. If a hint write fails the file is simply given up on: nothing
-- more is buffered for it, it gets no trailer, and it is deleted when the data
-- file is finished. The next open scans that data file instead.
flushHint :: Store -> Active -> IO Active
flushHint st ac
  | not (acHintOk ac) || null (acHintBuf ac) = pure ac
  | otherwise = do
      r <- try $ do
        injected <- fault st FaultHintWrite
        when injected $ throwIO (userError "injected fault: hint write")
        appendBytes (acHint ac) (BS.concat (reverse (acHintBuf ac)))
      case r of
        Right () -> pure ac {acHintBuf = [], acHintBufLen = 0}
        Left (e :: SomeException)
          -- Only a failed write costs the hint. An asynchronous exception —
          -- a merge thread being killed by 'closeStore', say — must go on
          -- being delivered, or the thread would carry on as if nothing had
          -- happened.
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> pure ac {acHintBuf = [], acHintBufLen = 0, acHintOk = False}

hintBufferSize :: Int
hintBufferSize = 64 * 1024

-- | Finish a data file: sync it, cap its hint file with the trailer that makes
-- the hint usable, and close both.
--
-- This always closes both handles, even when something fails, so that nothing
-- ever tries to use or close them again. It reports a failure to make the data
-- durable rather than throwing it, having already marked the store broken; a
-- failure to finish the hint only costs the hint.
--
-- The data is synced before the hint trailer is written, and a store that is
-- broken gets no trailer at all. A hint describes only the writes that
-- succeeded, so on a broken store it would be accurate — but it would also make
-- the next open trust the file without scanning it, and the scan is what finds
-- and repairs whatever a failed write left at the end.
closeActive :: Store -> Active -> IO (Maybe SomeException)
closeActive st ac0 = do
  ac <- flushHint st ac0
  synced <- try (syncData st (acData ac))
  forM_ (leftToMaybe synced) $ \e ->
    markBroken st ("sync of " <> dataPath (stDir st) (acFileId ac) <> " failed: " <> show e)
  healthy <- isNothing <$> readIORef (stBroken st)
  hinted <-
    if acHintOk ac && healthy
      then isRight <$> (try (do
        appendBytes (acHint ac) (encodeHintTrailer (acHintCount ac) (acHintCrc ac))
        syncFile (acHint ac)) :: IO (Either SomeException ()))
      else pure False
  quietly (closeAppend (acHint ac))
  quietly (closeAppend (acData ac))
  -- A hint without a trailer is ignored on open anyway; removing it just keeps
  -- the directory honest.
  unless hinted $ void (removeOpen (hintPath (stDir st) (acFileId ac)))
  -- The directory entry for the new file is part of its durability too.
  dirSynced <- try (syncDir (stDir st))
  forM_ (leftToMaybe dirSynced) $ \e ->
    markBroken st ("sync of directory " <> stDir st <> " failed: " <> show e)
  pure (leftToMaybe synced <|> leftToMaybe dirSynced)
  where
    quietly act = void (try act :: IO (Either SomeException ()))

-- | Roll to a fresh active file.
--
-- The new file is opened /before/ the old one is closed, so that failing to
-- open it — no space, no file descriptors — leaves the old file active and the
-- store perfectly usable. Returns the file to carry on with, and the error if
-- there was one.
rollActive :: Store -> Active -> IO (Active, Maybe SomeException)
rollActive st ac = do
  opened <- try (openActive (stDir st) (mkFileId (fileBase (acFileId ac) + 1) 0))
  case opened of
    Left e -> pure (ac, Just e)
    Right ac' -> do
      bumpTotal st (acFileId ac') 0
      closed <- closeActive st ac
      pure (ac', closed)

-- ---------------------------------------------------------------------------
-- Write safety

-- | Run one step of the write path against the active file.
--
-- The step runs with asynchronous exceptions masked. Without that, a
-- 'System.Timeout.timeout' or 'Control.Concurrent.killThread' landing between
-- an append and the bookkeeping after it would put the old 'Active' back while
-- the bytes stayed on disk, and every later write would record its location at
-- the wrong offset — silently, since the writes themselves would succeed.
--
-- The step reports failure by returning it, alongside the 'Active' that is now
-- true, because on failure the file may still have changed (it may have rolled)
-- and the MVar must hold what is actually there. A step that throws anyway has
-- hit something unforeseen, so the store is marked broken rather than trusted.
withActive :: Store -> (Active -> IO (Active, Either SomeException a)) -> IO a
withActive st step = do
  r <- modifyMVarMasked (stActive st) $ \case
    Nothing -> pure (Nothing, Left (toException WriteToReadOnly))
    Just ac -> do
      broken <- readIORef (stBroken st)
      case broken of
        Just why -> pure (Just ac, Left (toException (StoreBroken why)))
        Nothing -> do
          res <- try (step ac)
          case res of
            Right (ac', out) -> pure (Just ac', out)
            Left e -> do
              markBroken st ("unexpected failure in the write path: " <> show e)
              pure (Just ac, Left e)
  either throwIO pure r

-- | Refuse all further writes, for the given reason. The first reason sticks.
markBroken :: Store -> String -> IO ()
markBroken st why = atomicModifyIORef' (stBroken st) (\b -> (b <|> Just why, ()))

-- | Append to a data file, in a way the tests can make fail: an armed
-- 'FaultDataWrite' writes half the bytes and then throws, which is the worst a
-- real failed write can do.
appendData :: Store -> AppendHandle -> ByteString -> IO ()
appendData st h bytes = do
  injected <- fault st FaultDataWrite
  if injected
    then do
      appendBytes h (BS.take (BS.length bytes `div` 2) bytes)
      throwIO (userError "injected fault: data write")
    else appendBytes h bytes

-- | @fsync@ a data file, in a way the tests can make fail.
syncData :: Store -> AppendHandle -> IO ()
syncData st h = do
  injected <- fault st FaultSync
  when injected $ throwIO (userError "injected fault: sync")
  syncFile h

-- | Failures the test suite can arm, to reach the paths that a real disk only
-- takes when it is full or failing. Each fires once, at its next opportunity.
data Fault
  = -- | an append to the active data file writes half the record, then fails
    FaultDataWrite
  | -- | undoing a failed append fails
    FaultUndo
  | -- | an @fsync@ of a data file fails
    FaultSync
  | -- | writing out buffered hint entries fails
    FaultHintWrite
  deriving stock (Eq, Show)

-- | Arm a fault. For tests only.
injectFault :: Store -> Fault -> IO ()
injectFault st f = atomicModifyIORef' (stFaults st) (\fs -> (f : fs, ()))

-- | Whether a fault is armed, disarming it if so. Costs one read when none is.
fault :: Store -> Fault -> IO Bool
fault st f = do
  armed <- readIORef (stFaults st)
  if null armed
    then pure False
    else atomicModifyIORef' (stFaults st) $ \fs ->
      if f `elem` fs then (L.delete f fs, True) else (fs, False)

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

-- ---------------------------------------------------------------------------
-- Readers

withReader :: Store -> FileId -> (ReadHandle -> IO a) -> IO a
withReader st fid act = act =<< cachedReader (stDir st) (stReaders st) fid

-- | Stop handing out the read handle for a file, without closing it.
--
-- Merge calls this for a file it is about to remove. Closing the descriptor here
-- would race a reader that is inside a positional read on it right now — at best
-- the read fails, at worst the descriptor has already been recycled. The handle
-- is parked instead and closed when the store closes.
retireReader :: Store -> FileId -> IO ()
retireReader st fid = do
  old <- dropReader (stReaders st) fid
  forM_ old $ \rh -> atomicModifyIORef' (stRetired st) (\rs -> (rh : rs, ()))

-- ---------------------------------------------------------------------------
-- Reads

getRaw :: Store -> ByteString -> IO (Maybe ByteString)
getRaw st k = do
  assertOpen st
  kd <- readIORef (stKeydir st)
  case KD.lookup k kd of
    Nothing -> pure Nothing
    Just loc -> fetchAt st k loc 0

-- | Read the record at a location and hand back its value.
--
-- The retries exist because a location read out of the keydir can go stale under
-- a concurrent merge: the file may have been removed, or the offset may now hold
-- a different record. Both are detectable — a failed read, a short read, or a
-- record whose key is not the one we asked for — and the answer to all three is
-- to look the key up again.
fetchAt :: Store -> ByteString -> Loc -> Int -> IO (Maybe ByteString)
fetchAt st k loc attempt = do
  res <- try (withReader st (locFileId loc) $ \rh -> preadAt rh (locPos loc) (fromIntegral (locSize loc)))
  case res of
    Left (e :: IOException)
      | attempt < maxAttempts -> again
      | otherwise -> throwIO e
    Right bs
      | BS.length bs < fromIntegral (locSize loc) ->
          if attempt < maxAttempts then again else pure Nothing
      | otherwise -> case decodeRecord (verifyChecksums (stOpts st)) bs of
          Left err
            | attempt < maxAttempts -> again
            | otherwise -> throwIO (CorruptRecord (dataPath (stDir st) (locFileId loc)) (locPos loc) err)
          Right r
            | recKey r /= k ->
                if attempt < maxAttempts then again else pure Nothing
            | otherwise -> pure (recValue r)
  where
    maxAttempts = 3 :: Int
    again = do
      kd <- readIORef (stKeydir st)
      case KD.lookup k kd of
        Nothing -> pure Nothing
        Just loc' -> fetchAt st k loc' (attempt + 1)

memberRaw :: Store -> ByteString -> IO Bool
memberRaw st k = do
  assertOpen st
  KD.member k <$> readIORef (stKeydir st)

keysRaw :: Store -> IO [ByteString]
keysRaw st = do
  assertOpen st
  KD.keys <$> readIORef (stKeydir st)

-- | Strict left fold over every live key and value.
--
-- The keydir is snapshotted first, so the fold sees a consistent set of keys;
-- values are read as it goes, and a key deleted mid-fold is skipped rather than
-- reported.
foldRaw :: Store -> (a -> ByteString -> ByteString -> IO a) -> a -> IO a
foldRaw st f z = do
  assertOpen st
  kd <- readIORef (stKeydir st)
  sweepRecords st step z (KD.toList kd)
  where
    step acc k loc mbytes = do
      mv <- case decodeRecord (verifyChecksums (stOpts st)) <$> mbytes of
        Just (Right r)
          | recKey r == k, Just v <- recValue r ->
              -- A copy, so that a caller holding on to values does not hold on
              -- to the whole read window each one was sliced from.
              pure (Just (BS.copy v))
        -- Anything unexpected goes the slow way, which knows how to retry
        -- around a concurrent merge and how to report real corruption.
        _ -> fetchAt st k loc 0
      maybe (pure acc) (f acc k) mv

-- | Visit the records at a set of locations, reading each file in large
-- windows instead of making one positional read per record.
--
-- A positional read costs a system call whatever its size, and for the ~100-byte
-- records Bitcask is built for the call is nearly all of the cost. So the
-- locations are bucketed by file and by which 'sweepWindow'-sized stretch of the
-- file they start in, each bucket is fetched with a single read spanning its
-- records, and the records are sliced out of that. Buckets are visited in file
-- and offset order, so each file is read front to back; within a bucket the
-- order is unspecified. Bucketing rather than sorting matters: sorting every
-- live key by location cost more than all the reads it saved.
--
-- The callback gets exactly the record's bytes, or 'Nothing' if the read failed
-- or came up short; it decides what to do about that, typically by falling back
-- to the careful per-record path.
sweepRecords
  :: Store
  -> (a -> ByteString -> Loc -> Maybe ByteString -> IO a)
  -> a
  -> [(ByteString, Loc)]
  -> IO a
sweepRecords st f z items = foldM bucket z (M.toAscList buckets)
  where
    buckets =
      M.fromListWith
        (++)
        [((locFileId l, locPos l `quot` sweepWindow), [x]) | x@(_, l) <- items]

    bucket acc ((fid, _), batch) = do
      let start = minimum [locPos l | (_, l) <- batch]
          end = maximum [locPos l + fromIntegral (locSize l) | (_, l) <- batch]
      r <- try (withReader st fid $ \rh -> preadAt rh start (fromIntegral (end - start)))
      case r of
        Left (_ :: IOException) -> foldM (\a (k, l) -> f a k l Nothing) acc batch
        Right buf -> foldM (\a (k, l) -> f a k l (slice buf start l)) acc batch

    slice buf start l
      | o + n <= BS.length buf = Just (BSU.unsafeTake n (BSU.unsafeDrop o buf))
      | otherwise = Nothing
      where
        o = fromIntegral (locPos l - start)
        n = fromIntegral (locSize l)

-- | How much of a file one sweep read covers: records are bucketed by which
-- stretch of this size they start in.
sweepWindow :: Word64
sweepWindow = 1024 * 1024

-- | Fold over keys and locations without reading, or decoding, any values.
foldRefsRaw :: Store -> (a -> ByteString -> Loc -> IO a) -> a -> IO a
foldRefsRaw st f z = do
  assertOpen st
  kd <- readIORef (stKeydir st)
  foldM (\acc (k, loc) -> f acc k loc) z (KD.toList kd)

-- ---------------------------------------------------------------------------
-- Writes

putRaw :: Store -> ByteString -> ByteString -> IO ()
putRaw st k v = do
  when (BS.length v > maxValueSize) $ throwIO (ValueTooLarge (BS.length v))
  appendRecord st k (Just v)

deleteRaw :: Store -> ByteString -> IO ()
deleteRaw st k = appendRecord st k Nothing

-- | Append one record and point the keydir at it.
--
-- What a caller can rely on, however this ends:
--
-- * If it returns, the record is in the file and visible to readers.
--
-- * If the append fails, the file is cut back to where it was and the call
--   throws: the write did not happen, and the store carries on.
--
-- * If the append fails and the file cannot be cut back, or an @fsync@ that the
--   'SyncPolicy' asked for fails, the call throws and the store is marked
--   broken: every later write fails with 'StoreBroken' until the store is
--   reopened. A failed @fsync@ comes after the record was written and indexed,
--   so that record is visible now and may or may not survive a reopen.
--
-- * If it is interrupted by an asynchronous exception, it either happened or
--   did not; see 'withActive'.
appendRecord :: Store -> ByteString -> Maybe ByteString -> IO ()
appendRecord st k mv = do
  assertOpen st
  when (readOnly (stOpts st)) $ throwIO WriteToReadOnly
  when (BS.length k > maxKeySize) $ throwIO (KeyTooLarge (BS.length k))
  ts <- nowNanos
  let bytes = encodeRecord ts k mv
      n = BS.length bytes
  withActive st $ \ac0 -> do
    (ac, rollErr) <-
      if needsRoll ac0 n then rollActive st ac0 else pure (ac0, Nothing)
    case rollErr of
      Just e -> pure (ac, Left e)
      Nothing -> do
        let off = acOffset ac
        written <- try (appendData st (acData ac) bytes)
        case written of
          Left e -> do
            undoWrite ac e
            pure (ac, Left e)
          Right () -> do
            let hintBytes =
                  encodeHintEntry
                    HintEntry
                      { hintTstamp = ts
                      , hintTombstone = isNothing mv
                      , hintKey = k
                      , hintValSize = maybe 0 (fromIntegral . BS.length) mv
                      , hintPos = off
                      , hintRecSize = fromIntegral n
                      }
            ac' <- appendHint st ac {acOffset = off + fromIntegral n} hintBytes
            let loc = Loc (acFileId ac) off (fromIntegral n) ts
            old <- atomicModifyIORef' (stKeydir st) $
              KD.replace k (if isNothing mv then Nothing else Just loc)
            forM_ old $ \o -> bumpDead st (locFileId o) (fromIntegral (locSize o))
            -- A tombstone is never reachable from the keydir, so it is dead the
            -- instant it is written.
            when (isNothing mv) $ bumpDead st (acFileId ac) (fromIntegral n)
            bumpTotal st (acFileId ac) (fromIntegral n)
            synced <- try (maybeSync st ac')
            forM_ (leftToMaybe synced) $ \e ->
              markBroken st ("sync of " <> dataPath (stDir st) (acFileId ac) <> " failed: " <> show e)
            pure (ac', synced)
  where
    needsRoll ac n =
      acOffset ac > 0 && acOffset ac + fromIntegral n > maxFileSize (stOpts st)

    -- Some of the record may have reached the file. Cut it off, so that the
    -- file ends where 'acOffset' says it does.
    undoWrite ac (e :: SomeException) = do
      undone <- try $ do
        injected <- fault st FaultUndo
        when injected $ throwIO (userError "injected fault: undo")
        truncateAppend (acData ac) (acOffset ac)
      forM_ (leftToMaybe undone) $ \(e2 :: SomeException) ->
        markBroken st $
          "a write to "
            <> dataPath (stDir st) (acFileId ac)
            <> " failed ("
            <> show e
            <> ") and could not be undone ("
            <> show e2
            <> ")"

maybeSync :: Store -> Active -> IO ()
maybeSync st ac = case syncPolicy (stOpts st) of
  SyncNever -> pure ()
  SyncEveryMicros _ -> pure () -- handled by a background thread
  SyncOnPut -> syncData st (acData ac)
  SyncEvery n -> do
    c <- atomicModifyIORef' (stWrites st) (\w -> let w' = w + 1 in (w', w'))
    when (n > 0 && c `mod` n == 0) $ syncData st (acData ac)

bumpDead :: Store -> FileId -> Word64 -> IO ()
bumpDead st fid n = atomicModifyIORef' (stDead st) (\m -> (M.insertWith (+) fid n m, ()))

setDead :: Store -> FileId -> Word64 -> IO ()
setDead st fid n = atomicModifyIORef' (stDead st) (\m -> (M.insert fid n m, ()))

bumpTotal :: Store -> FileId -> Word64 -> IO ()
bumpTotal st fid n = atomicModifyIORef' (stTotal st) (\m -> (M.insertWith (+) fid n m, ()))

-- ---------------------------------------------------------------------------
-- Sync, stats, close

syncStore :: Store -> IO ()
syncStore st = do
  assertOpen st
  withMVar (stActive st) $ \case
    Nothing -> pure ()
    -- Only the data file. The hint file is not worth an fsync of its own: until
    -- its trailer is written at close it is ignored on open anyway, and
    -- 'closeActive' syncs it then.
    Just ac -> do
      -- A sync on a broken store cannot promise anything, so it does not
      -- pretend to.
      readIORef (stBroken st) >>= mapM_ (throwIO . StoreBroken)
      synced <- try (syncData st (acData ac))
      case synced of
        Right () -> pure ()
        Left (e :: SomeException) -> do
          markBroken st ("sync of " <> dataPath (stDir st) (acFileId ac) <> " failed: " <> show e)
          throwIO e

statsStore :: Store -> IO Stats
statsStore st = do
  assertOpen st
  kd <- readIORef (stKeydir st)
  totals <- readIORef (stTotal st)
  pure
    Stats
      { statsKeys = KD.size kd
      , statsDataFiles = M.size totals
      , statsLiveBytes = KD.liveBytes kd
      , statsTotalBytes = sum (M.elems totals)
      }

-- | Finish the active file, stop background work and release every handle and
-- the lock. Everything is released even if finishing the active file fails;
-- that failure is thrown afterwards.
closeStore :: Store -> IO ()
closeStore st = do
  already <- atomicModifyIORef' (stClosed st) (\c -> (True, c))
  unless already $ do
    readIORef (stThreads st) >>= mapM_ killThread
    failed <- modifyMVarMasked (stActive st) $ \case
      Nothing -> pure (Nothing, Nothing)
      Just ac -> (,) Nothing <$> closeActive st ac
    let rd = stReaders st
    withMVar (rdLock rd) $ \() ->
      atomicModifyIORef' (rdMap rd) (\m -> (M.empty, m)) >>= mapM_ closeRead . M.elems
    readIORef (stRetired st) >>= mapM_ closeRead
    writeIORef (stRetired st) []
    releaseLock (stLock st)
    mapM_ throwIO failed
