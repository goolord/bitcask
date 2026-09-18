-- | The store handle and operations on encoded keys and values.
-- "Database.Bitcask" wraps these with encoding and decoding.
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
-- Reads of the map are lock-free since every 'Database.Bitcask.get' needs one.
-- The lock is only taken to insert, so two threads missing at once don't both
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
  -- ^ unreachable bytes per data file; only used for the merge trigger
  , stWrites :: !(IORef Int)
  , stMergeGate :: !(MVar ())
  -- ^ one merge at a time
  , stThreads :: !(IORef [ThreadId])
  , stRetired :: !(IORef [ReadHandle])
  -- ^ handles for files merge has removed; see 'retireReader'
  , stBroken :: !(IORef (Maybe String))
  -- ^ set once writes are refused; see 'markBroken'
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
-- Every open starts a new active file instead of appending to the last one.
-- That costs a small file per open (merge cleans them up), but it means only
-- the active file is ever appended to, so a torn tail can only be in one place.
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
-- Files are visited in id order, which is write order, so the last ref for a
-- key wins. See "Database.Bitcask.Internal.Keydir".
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
                -- Only the newest file can have a torn write, and only a writer
                -- repairs it.
                closeReaderFor readers fid
                truncateAt (dataPath dir fid) at
              else pure ()
          pure $! kd'

-- | The read handle for a file, opened on first use. Lock-free after that.
cachedReader :: FilePath -> Readers -> FileId -> IO ReadHandle
cachedReader dir rd fid = do
  m <- readIORef (rdMap rd)
  case M.lookup fid m of
    Just rh -> pure rh
    Nothing -> withMVar (rdLock rd) $ \() -> do
      -- Another thread may have opened it while we waited.
      m' <- readIORef (rdMap rd)
      case M.lookup fid m' of
        Just rh -> pure rh
        Nothing -> do
          rh <- openRead (dataPath dir fid)
          atomicModifyIORef' (rdMap rd) (\m'' -> (M.insert fid rh m'', ()))
          pure rh

-- | Remove a file's read handle from the map and return it.
dropReader :: Readers -> FileId -> IO (Maybe ReadHandle)
dropReader rd fid = withMVar (rdLock rd) $ \() ->
  atomicModifyIORef' (rdMap rd) (\m -> (M.delete fid m, M.lookup fid m))

closeReaderFor :: Readers -> FileId -> IO ()
closeReaderFor rd fid = dropReader rd fid >>= mapM_ closeRead

-- | Delete files a previous merge couldn't remove because a reader had them
-- open. Only happens on Windows.
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
-- Entries are buffered and written in blocks. That saves a syscall per record,
-- about half the cost of a 'Database.Bitcask.put'. Nothing reads a hint file
-- until its trailer is written, and a hint file with no trailer is ignored on
-- open, so losing the buffer in a crash is harmless.
--
-- Only throws asynchronous exceptions; see 'flushHint'.
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
-- Hint files are a cache, so a failed hint write must not fail the put. On
-- failure we give up on the hint: no more buffering, no trailer, and it's
-- deleted when the data file is finished. The next open scans the data file.
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
          -- Rethrow async exceptions (e.g. 'closeStore' killing a merge
          -- thread), otherwise the thread would just keep going.
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> pure ac {acHintBuf = [], acHintBufLen = 0, acHintOk = False}

hintBufferSize :: Int
hintBufferSize = 64 * 1024

-- | Finish a data file: sync it, write the hint trailer, close both.
--
-- Both handles are always closed. A failed data sync marks the store broken
-- and is returned rather than thrown. A failed hint just loses the hint.
--
-- The data is synced before the trailer is written. A broken store gets no
-- trailer: the hint would be accurate, but it would stop the next open from
-- scanning the file, and the scan is what repairs a bad tail.
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
  -- A hint with no trailer is ignored on open anyway. Remove it to tidy up.
  unless hinted $ void (removeOpen (hintPath (stDir st) (acFileId ac)))
  -- The new file's directory entry needs syncing too.
  dirSynced <- try (syncDir (stDir st))
  forM_ (leftToMaybe dirSynced) $ \e ->
    markBroken st ("sync of directory " <> stDir st <> " failed: " <> show e)
  pure (leftToMaybe synced <|> leftToMaybe dirSynced)
  where
    quietly act = void (try act :: IO (Either SomeException ()))

-- | Roll to a fresh active file.
--
-- The new file is opened before the old one is closed, so if the open fails
-- (disk full, out of fds) the old file stays active and the store still works.
-- Returns the active file to continue with and the error, if any.
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
-- Async exceptions are masked. Otherwise a 'System.Timeout.timeout' or
-- 'Control.Concurrent.killThread' between an append and its bookkeeping would
-- restore the old 'Active' with the bytes still on disk, and every later write
-- would silently record the wrong offset.
--
-- The step returns its failure along with the current 'Active', since the file
-- may have changed (e.g. rolled) even when the step failed. If the step throws
-- instead, something unexpected happened and the store is marked broken.
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

-- | Refuse all further writes. Keeps the first reason given.
markBroken :: Store -> String -> IO ()
markBroken st why = atomicModifyIORef' (stBroken st) (\b -> (b <|> Just why, ()))

-- | Append to a data file. With 'FaultDataWrite' armed, writes half the bytes
-- and throws, which is the worst a real failed write can do.
appendData :: Store -> AppendHandle -> ByteString -> IO ()
appendData st h bytes = do
  injected <- fault st FaultDataWrite
  if injected
    then do
      appendBytes h (BS.take (BS.length bytes `div` 2) bytes)
      throwIO (userError "injected fault: data write")
    else appendBytes h bytes

-- | @fsync@ a data file. Fails if 'FaultSync' is armed.
syncData :: Store -> AppendHandle -> IO ()
syncData st h = do
  injected <- fault st FaultSync
  when injected $ throwIO (userError "injected fault: sync")
  syncFile h

-- | Failures the tests can arm to hit paths a real disk only takes when full
-- or failing. Each fires once.
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

-- | Check and disarm a fault. One IORef read when nothing is armed.
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

-- | Stop handing out a file's read handle, but don't close it.
--
-- Merge calls this before removing a file. Closing here could race a reader
-- mid-pread, which would fail or, worse, hit a recycled descriptor. The handle
-- is kept and closed when the store closes.
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
-- A location can go stale under a concurrent merge: the file may be gone, or
-- the offset may hold a different record. That shows up as a failed read, a
-- short read, or the wrong key, and in each case we look the key up again.
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
-- Folds over a snapshot of the keydir. Values are read as it goes, and a key
-- deleted mid-fold is skipped.
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
              -- Copy so a retained value doesn't retain the whole read window.
              pure (Just (BS.copy v))
        -- Otherwise take the slow path, which retries around a concurrent
        -- merge and reports real corruption.
        _ -> fetchAt st k loc 0
      maybe (pure acc) (f acc k) mv

-- | Visit the records at a set of locations, reading files in large windows
-- instead of one pread per record.
--
-- For small records the syscall is most of the cost of a read. Locations are
-- bucketed by file and by which 'sweepWindow' chunk they start in, and each
-- bucket is fetched with one read. Buckets go in file and offset order; order
-- within a bucket is unspecified. We bucket instead of sorting because sorting
-- every live key by location cost more than the reads it saved.
--
-- The callback gets the record's bytes, or 'Nothing' if the read failed or was
-- short. Callers usually fall back to the per-record path then.
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

-- | Bucket size for 'sweepRecords'.
sweepWindow :: Word64
sweepWindow = 1024 * 1024

-- | Fold over keys and locations without reading any values.
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
-- * Returns: the record is in the file and visible to readers.
--
-- * Append fails: the file is truncated back and the call throws. The write
--   didn't happen and the store is fine.
--
-- * Truncate fails, or an @fsync@ required by the 'SyncPolicy' fails: throws
--   and marks the store broken, so later writes fail with 'StoreBroken' until
--   reopened. A failed @fsync@ happens after the record is indexed, so it's
--   visible now and may or may not survive a reopen.
--
-- * Async exception: the write either happened or didn't; see 'withActive'.
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
            -- Tombstones aren't in the keydir, so they're dead immediately.
            when (isNothing mv) $ bumpDead st (acFileId ac) (fromIntegral n)
            bumpTotal st (acFileId ac) (fromIntegral n)
            synced <- try (maybeSync st ac')
            forM_ (leftToMaybe synced) $ \e ->
              markBroken st ("sync of " <> dataPath (stDir st) (acFileId ac) <> " failed: " <> show e)
            pure (ac', synced)
  where
    needsRoll ac n =
      acOffset ac > 0 && acOffset ac + fromIntegral n > maxFileSize (stOpts st)

    -- Part of the record may have been written. Truncate so the file ends at
    -- 'acOffset'.
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
    -- Data file only. The hint is ignored on open until its trailer is
    -- written, and 'closeActive' syncs it then.
    Just ac -> do
      -- Syncing a broken store can't guarantee anything.
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

-- | Finish the active file, stop background threads, release handles and the
-- lock. If finishing the active file fails, everything is still released and
-- the error is thrown at the end.
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
