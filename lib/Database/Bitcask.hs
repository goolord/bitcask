-- | A Bitcask store: an append-only log of key\/value records with an in-memory
-- index, as described in Sheehy & Smith, /Bitcask: A Log-Structured Hash Table
-- for Fast Key\/Value Data/.
--
-- A read is one hash lookup and one positional read. A write is one append. The
-- price is memory: __every key in the store lives in RAM, permanently__. Bitcask
-- trades memory for a one-seek read, and a store with a hundred million small
-- keys is not a Bitcask.
--
-- == Using it
--
-- > {-# LANGUAGE DerivingVia, DeriveAnyClass, DeriveGeneric, TypeApplications #-}
-- > import Database.Bitcask
-- > import Data.Binary (Binary)
-- > import Data.Text (Text)
-- > import GHC.Generics (Generic)
-- >
-- > data User = User { userName :: Text, userAge :: Int }
-- >   deriving stock (Show, Generic)
-- >   deriving anyclass (Binary)
-- >   deriving Codec via (AsBinary User)
-- >
-- > main :: IO ()
-- > main = withBitcask @Text @User "users" defaultOptions $ \bc -> do
-- >   put bc "nano" (User "nano" 33)
-- >   print =<< get bc "nano"
--
-- == Concurrency
--
-- One process may hold a store open for writing, enforced by a lock on the store
-- directory. Within that process everything here is thread-safe: reads take no
-- lock at all, writes are serialised, and 'merge' blocks neither.
--
-- == Durability
--
-- The default 'SyncPolicy' is 'SyncNever', which matches the paper. Records
-- reach the operating system as they are written, but survive a machine crash
-- only once 'sync' has run. 'withBitcask' and 'close' sync on the way out. If
-- you need more, set 'syncPolicy'.
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
  , AsBinary (..)

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
-- The tag is a phantom: the bytes on disk are whatever the 'Codec' instances
-- produce, and nothing below this module knows about @k@ or @v@. Two handles
-- opened at different types against the same directory will happily read each
-- other's bytes and fail to decode them, which is what 'storeTag' is for.
newtype Bitcask k v = Bitcask Store

-- | Open a store, creating the directory if it does not exist.
--
-- Takes the store lock, replays the data files into the keydir (using hint files
-- where they are intact), repairs a torn write at the end of the newest file if
-- 'repairTruncated' is set, and starts a fresh active file.
--
-- Choose the types at the call site: @'open' \@Text \@User dir opts@.
open :: FilePath -> OpenOptions -> IO (Bitcask k v)
open dir opts = do
  st <- openStore dir opts
  startBackground st
  pure (Bitcask st)

-- | 'open' a store, use it, and 'close' it even if the body throws.
withBitcask :: FilePath -> OpenOptions -> (Bitcask k v -> IO a) -> IO a
withBitcask dir opts = bracket (open dir opts) close

-- | Finish the active file, release the store lock, and close every file handle.
-- Idempotent.
close :: Bitcask k v -> IO ()
close (Bitcask st) = closeStore st

-- | The value for a key, or 'Nothing' if there isn't one.
--
-- A missing key is not an error and never throws. A value whose checksum passes
-- but which will not decode throws 'DecodeFailure', which nearly always means
-- the store was opened at the wrong types.
get :: (Codec k, Codec v) => Bitcask k v -> k -> IO (Maybe v)
get (Bitcask st) k = do
  mbs <- getRaw st (encodeStrict k)
  case mbs of
    Nothing -> pure Nothing
    Just bs -> either (throwIO . DecodeFailure ValueField) (pure . Just) (fromBytes bs)

-- | Write a value, replacing any previous one. One append.
put :: (Codec k, Codec v) => Bitcask k v -> k -> v -> IO ()
put (Bitcask st) k v = putRaw st (encodeStrict k) (encodeStrict v)

-- | Remove a key by appending a tombstone. The space is reclaimed by the next
-- 'merge'.
delete :: (Codec k) => Bitcask k v -> k -> IO ()
delete (Bitcask st) k = deleteRaw st (encodeStrict k)

-- | Whether a key has a value, without reading it.
member :: (Codec k) => Bitcask k v -> k -> IO Bool
member (Bitcask st) k = memberRaw st (encodeStrict k)

-- | Every live key. This is the paper's @list_keys@.
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

-- | Fold over keys and their locations without reading, or decoding, any values.
--
-- Not in the paper, but cheap and useful: this is what you want for \"how much
-- space is this using\" or for building an external index.
foldRefs :: (Codec k) => Bitcask k v -> (a -> k -> Loc -> IO a) -> a -> IO a
foldRefs (Bitcask st) f z = foldRefsRaw st step z
  where
    step acc kb loc = do
      k <- decodeKey kb
      f acc k loc

decodeKey :: (Codec k) => ByteString -> IO k
decodeKey = either (throwIO . DecodeFailure KeyField) pure . fromBytes

-- | Push everything written so far through to the disk.
sync :: Bitcask k v -> IO ()
sync (Bitcask st) = syncStore st

-- | Rewrite the immutable data files, keeping only live records and writing hint
-- files beside them. Runs concurrently with reads and writes.
merge :: Bitcask k v -> IO MergeStats
merge (Bitcask st) = mergeStore st

-- | Compact a store that nothing currently has open. This is the paper's
-- out-of-process @merge(DirName)@.
mergeDirectory :: FilePath -> IO MergeStats
mergeDirectory dir =
  bracket (open dir defaultOptions) close (merge @() @())

stats :: Bitcask k v -> IO Stats
stats (Bitcask st) = statsStore st

-- | Background workers for the policies that need one.
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
    -- A background worker that dies takes its policy with it, so swallow what it
    -- throws; anything real will surface again on the next foreground call.
    ignoring act = void (try act :: IO (Either SomeException ()))
