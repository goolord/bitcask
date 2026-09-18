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
  , bumpTotal
  , setDead
  , assertOpen
  ) where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Exception (IOException, bracketOnError, throwIO, try)
import Control.Monad (foldM, forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
      }

-- | Add an entry to the active file's hint file.
--
-- Hint entries are buffered and written in blocks, which saves the write path one
-- system call per record — roughly half of what a 'Database.Bitcask.put' costs.
-- Unlike the data file, nothing reads a hint file until the file is finished and
-- its trailer written, so there is nobody to see the difference; and a crash
-- that loses the buffer loses nothing, because a hint file without its trailer is
-- ignored in favour of scanning the data file.
appendHint :: Active -> ByteString -> IO Active
appendHint ac bytes = do
  let ac' =
        ac
          { acHintCrc = crc32Update (acHintCrc ac) bytes
          , acHintCount = acHintCount ac + 1
          , acHintBuf = bytes : acHintBuf ac
          , acHintBufLen = acHintBufLen ac + BS.length bytes
          }
  if acHintBufLen ac' >= hintBufferSize then flushHint ac' else pure ac'

-- | Write out buffered hint entries.
flushHint :: Active -> IO Active
flushHint ac
  | null (acHintBuf ac) = pure ac
  | otherwise = do
      appendBytes (acHint ac) (BS.concat (reverse (acHintBuf ac)))
      pure ac {acHintBuf = [], acHintBufLen = 0}

hintBufferSize :: Int
hintBufferSize = 64 * 1024

-- | Finish the active file: cap its hint file with the trailer that makes it
-- usable, sync the data, and close both.
closeActive :: Store -> Active -> IO ()
closeActive st ac0 = do
  ac <- flushHint ac0
  appendBytes (acHint ac) (encodeHintTrailer (acHintCount ac) (acHintCrc ac))
  syncFile (acHint ac)
  syncFile (acData ac)
  closeAppend (acHint ac)
  closeAppend (acData ac)
  syncDir (stDir st)

-- | Roll to a fresh active file.
rollActive :: Store -> Active -> IO Active
rollActive st ac = do
  closeActive st ac
  ac' <- openActive (stDir st) (mkFileId (fileBase (acFileId ac) + 1) 0)
  bumpTotal st (acFileId ac') 0
  pure ac'

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

appendRecord :: Store -> ByteString -> Maybe ByteString -> IO ()
appendRecord st k mv = do
  assertOpen st
  when (readOnly (stOpts st)) $ throwIO WriteToReadOnly
  when (BS.length k > maxKeySize) $ throwIO (KeyTooLarge (BS.length k))
  ts <- nowNanos
  let bytes = encodeRecord ts k mv
      n = BS.length bytes
  modifyMVar_ (stActive st) $ \case
    Nothing -> throwIO WriteToReadOnly
    Just ac0 -> do
      ac <- if needsRoll ac0 n then rollActive st ac0 else pure ac0
      let off = acOffset ac
      appendBytes (acData ac) bytes
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
      -- Before the keydir update: a hint flush that throws must leave the put
      -- invisible, as a failed put should be.
      ac' <- appendHint ac {acOffset = off + fromIntegral n} hintBytes
      let loc = Loc (acFileId ac) off (fromIntegral n) ts
      old <- atomicModifyIORef' (stKeydir st) $
        KD.replace k (if isNothing mv then Nothing else Just loc)
      forM_ old $ \o -> bumpDead st (locFileId o) (fromIntegral (locSize o))
      -- A tombstone is never reachable from the keydir, so it is dead the
      -- instant it is written.
      when (isNothing mv) $ bumpDead st (acFileId ac) (fromIntegral n)
      bumpTotal st (acFileId ac) (fromIntegral n)
      maybeSync st ac'
      pure (Just ac')
  where
    needsRoll ac n =
      acOffset ac > 0 && acOffset ac + fromIntegral n > maxFileSize (stOpts st)

maybeSync :: Store -> Active -> IO ()
maybeSync st ac = case syncPolicy (stOpts st) of
  SyncNever -> pure ()
  SyncEveryMicros _ -> pure () -- handled by a background thread
  SyncOnPut -> syncFile (acData ac)
  SyncEvery n -> do
    c <- atomicModifyIORef' (stWrites st) (\w -> let w' = w + 1 in (w', w'))
    when (n > 0 && c `mod` n == 0) $ syncFile (acData ac)

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
    Just ac -> syncFile (acData ac)

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

closeStore :: Store -> IO ()
closeStore st = do
  already <- atomicModifyIORef' (stClosed st) (\c -> (True, c))
  unless already $ do
    readIORef (stThreads st) >>= mapM_ killThread
    modifyMVar_ (stActive st) $ \case
      Nothing -> pure Nothing
      Just ac -> closeActive st ac >> pure Nothing
    let rd = stReaders st
    withMVar (rdLock rd) $ \() ->
      atomicModifyIORef' (rdMap rd) (\m -> (M.empty, m)) >>= mapM_ closeRead . M.elems
    readIORef (stRetired st) >>= mapM_ closeRead
    writeIORef (stRetired st) []
    releaseLock (stLock st)
