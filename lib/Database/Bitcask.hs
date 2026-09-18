-- | A Bitcask store: an append-only log of key\/value records with an in-memory
-- index, as described in Sheehy & Smith, /Bitcask: A Log-Structured Hash Table
-- for Fast Key\/Value Data/.
--
-- A read is one hash lookup and one positional read. A write is one append.
-- __Every key lives in RAM__, so this is a bad fit for a hundred million small
-- keys.
--
-- == Using it
--
-- > {-# LANGUAGE DerivingVia, DeriveAnyClass, DeriveGeneric, TypeApplications #-}
-- > import Database.Bitcask
-- > import Data.Serialize (Serialize)
-- > import Data.Text (Text)
-- > import GHC.Generics (Generic)
-- >
-- > data User = User { userName :: String, userAge :: Int }
-- >   deriving stock (Show, Generic)
-- >   deriving anyclass (Serialize)
-- >   deriving Codec via (AsSerialize User)
-- >
-- > main :: IO ()
-- > main = withBitcask @Text @User "users" defaultOptions $ \bc -> do
-- >   put bc "nano" (User "nano" 33)
-- >   print =<< get bc "nano"
--
-- == Concurrency
--
-- Only one process can have a store open for writing (enforced with a lock on
-- the directory). Within a process everything is thread-safe: reads are
-- lock-free, writes are serialised, and 'merge' blocks neither.
--
-- == Durability
--
-- The default 'SyncPolicy' is 'SyncNever', same as the paper. Writes go to the
-- OS immediately but only survive a machine crash after 'sync'. 'withBitcask'
-- and 'close' sync on exit. Set 'syncPolicy' for more.
--
-- == When a write fails
--
-- If 'put' or 'delete' returns, the write happened. If it throws, it didn't,
-- except in two cases, which also mark the store /broken/:
--
-- * The append failed and truncating the file back also failed. The write may
--   or may not show up after a reopen.
--
-- * An @fsync@ failed, either from 'sync' or from 'syncPolicy'. The preceding
--   write is visible, but it's unknown whether it (or anything since the last
--   good @fsync@) is on disk.
--
-- A broken store throws 'StoreBroken' on every write, 'sync' and 'merge'.
-- Reads still work. To recover, 'close' and 'open' it again; opening scans the
-- file and repairs the tail.
--
-- A write interrupted by an async exception ('System.Timeout.timeout',
-- 'Control.Concurrent.killThread') either happened or didn't, and doesn't
-- damage the store. A failed hint write never fails a put; that file just gets
-- scanned on the next open.
module Database.Bitcask
  ( -- * Handles
    Bitcask
  , open
  , withBitcask
  , close

    -- * Options
  , OpenOptions (..)
  , defaultOptions
  , SyncPolicy (..)
  , MergePolicy (..)

    -- * Reading and writing
  , get
  , put
  , delete
  , member

    -- * Traversal
  , keys
  , fold
  , foldRefs

    -- * Maintenance
  , sync
  , merge
  , mergeDirectory
  , stats

    -- * Encoding
  , Codec (..)
  , encodeStrict
  , AsSerialize (..)

    -- * Errors and metadata
  , BitcaskError (..)
  , RecordError (..)
  , Field (..)
  , Loc (..)
  , FileId
  , Stats (..)
  , MergeStats (..)
  , maxKeySize
  , maxValueSize
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.IORef (atomicModifyIORef')

import Database.Bitcask.Codec
import Database.Bitcask.Internal.Merge (mergeStore, shouldMerge)
import Database.Bitcask.Internal.Store
import Database.Bitcask.Types

-- | An open store, tagged with the types of its keys and values.
--
-- The types are phantom. Nothing below this module knows about @k@ or @v@, so
-- opening the same directory at different types will just fail to decode.
-- Use 'storeTag' to catch that.
newtype Bitcask k v = Bitcask Store

-- | Open a store, creating the directory if it does not exist.
--
-- Takes the lock, rebuilds the keydir (from hint files where possible), repairs
-- a torn write at the end of the newest file if 'repairTruncated' is set, and
-- starts a new active file.
--
-- Pick the types at the call site: @'open' \@Text \@User dir opts@.
open :: FilePath -> OpenOptions -> IO (Bitcask k v)
open dir opts = do
  st <- openStore dir opts
  startBackground st
  pure (Bitcask st)

-- | 'open' a store, use it, and 'close' it even if the body throws.
withBitcask :: FilePath -> OpenOptions -> (Bitcask k v -> IO a) -> IO a
withBitcask dir opts = bracket (open dir opts) close

-- | Finish the active file, release the lock, close all handles. Idempotent.
close :: Bitcask k v -> IO ()
close (Bitcask st) = closeStore st

-- | Look up a key.
--
-- Throws 'DecodeFailure' if the value doesn't decode, which usually means the
-- store was opened at the wrong types.
get :: (Codec k, Codec v) => Bitcask k v -> k -> IO (Maybe v)
get (Bitcask st) k = do
  mbs <- getRaw st (encodeStrict k)
  case mbs of
    Nothing -> pure Nothing
    Just bs -> either (throwIO . DecodeFailure ValueField) (pure . Just) (fromBytes bs)

-- | Write a value, replacing any previous one. One append.
put :: (Codec k, Codec v) => Bitcask k v -> k -> v -> IO ()
put (Bitcask st) k v = putRaw st (encodeStrict k) (encodeStrict v)

-- | Delete a key by appending a tombstone. 'merge' reclaims the space.
delete :: (Codec k) => Bitcask k v -> k -> IO ()
delete (Bitcask st) k = deleteRaw st (encodeStrict k)

-- | Check for a key without reading its value.
member :: (Codec k) => Bitcask k v -> k -> IO Bool
member (Bitcask st) k = memberRaw st (encodeStrict k)

-- | All live keys. The paper's @list_keys@.
keys :: (Codec k) => Bitcask k v -> IO [k]
keys (Bitcask st) = mapM decodeKey =<< keysRaw st

-- | Strict left fold over every live key and value.
fold :: (Codec k, Codec v) => Bitcask k v -> (a -> k -> v -> IO a) -> a -> IO a
fold (Bitcask st) f z = foldRaw st step z
  where
    step acc kb vb = do
      k <- decodeKey kb
      v <- either (throwIO . DecodeFailure ValueField) pure (fromBytes vb)
      f acc k v

-- | Fold over keys and locations without reading any values. Not in the paper.
-- Useful for space accounting or building an external index.
foldRefs :: (Codec k) => Bitcask k v -> (a -> k -> Loc -> IO a) -> a -> IO a
foldRefs (Bitcask st) f z = foldRefsRaw st step z
  where
    step acc kb loc = do
      k <- decodeKey kb
      f acc k loc

decodeKey :: (Codec k) => ByteString -> IO k
decodeKey = either (throwIO . DecodeFailure KeyField) pure . fromBytes

-- | @fsync@ everything written so far.
sync :: Bitcask k v -> IO ()
sync (Bitcask st) = syncStore st

-- | Rewrite the immutable data files with only live records, plus hint files.
-- Runs concurrently with reads and writes.
merge :: Bitcask k v -> IO MergeStats
merge (Bitcask st) = mergeStore st

-- | Merge a store that isn't open. The paper's @merge(DirName)@.
mergeDirectory :: FilePath -> IO MergeStats
mergeDirectory dir =
  bracket (open dir defaultOptions) close (merge @() @())

stats :: Bitcask k v -> IO Stats
stats (Bitcask st) = statsStore st

-- | Start background threads for merge and sync policies.
startBackground :: Store -> IO ()
startBackground st = do
  when (not (readOnly (stOpts st))) $ case mergePolicy (stOpts st) of
    MergeAuto _ micros | micros > 0 -> spawn $ forever $ do
      threadDelay micros
      due <- shouldMerge st
      when due (ignoring (void (mergeStore st)))
    _ -> pure ()
  case syncPolicy (stOpts st) of
    SyncEveryMicros micros | micros > 0 -> spawn $ forever $ do
      threadDelay micros
      ignoring (syncStore st)
    _ -> pure ()
  where
    spawn act = do
      tid <- forkIO act
      atomicModifyIORef' (stThreads st) (\ts -> (tid : ts, ()))
    -- Don't let the worker die. Real errors will show up on the next
    -- foreground call anyway.
    ignoring act = void (try act :: IO (Either SomeException ()))
