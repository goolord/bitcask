-- | Types shared across the whole library: file identity, keydir entries,
-- options and errors.
module Database.Bitcask.Types
  ( -- * Files and locations
    FileId
  , mkFileId
  , fileBase
  , fileSub
  , unFileId
  , Offset
  , Loc (..)

    -- * Options
  , OpenOptions (..)
  , defaultOptions
  , SyncPolicy (..)
  , MergePolicy (..)

    -- * Statistics
  , Stats (..)
  , MergeStats (..)

    -- * Errors
  , BitcaskError (..)
  , RecordError (..)
  , Field (..)

    -- * Limits
  , maxKeySize
  , maxValueSize
  ) where

import Control.Exception (Exception)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)

-- | Identifies one data file within a store.
--
-- A 'FileId' is a pair of 32-bit counters packed into a 'Word64': the high half
-- is the /base/, bumped every time the active file rolls, and the low half is a
-- /sub/ sequence used only by merge.
--
-- The point of the pair is that @('FileId', offset)@ is a total order on writes
-- that does not depend on the system clock, /and/ that merge can allocate ids
-- which sort strictly before the current active file. Merge output is older than
-- anything written while the merge was running, so it must replay first; with a
-- single flat counter there is no id to give it. See @DESIGN.md@ §3.1.
newtype FileId = FileId {unFileId :: Word64}
  deriving stock (Eq, Ord)

instance Show FileId where
  show f = show (fileBase f) <> "-" <> show (fileSub f)

mkFileId :: Word32 -> Word32 -> FileId
mkFileId base sub = FileId ((fromIntegral base `shiftL` 32) .|. fromIntegral sub)

fileBase :: FileId -> Word32
fileBase (FileId w) = fromIntegral (w `shiftR` 32)

fileSub :: FileId -> Word32
fileSub (FileId w) = fromIntegral (w .&. 0xFFFFFFFF)

-- | A byte offset within a data file.
type Offset = Word64

-- | Where the live record for a key lives. This is the value side of the keydir,
-- and there is one of these in memory for every key in the store.
--
-- 'locPos' is the offset of the /record/, not of the value: reading the whole
-- record costs @19 + keySize@ extra bytes and lets 'Database.Bitcask.get' verify
-- the checksum before handing anything back.
data Loc = Loc
  { locFileId :: {-# UNPACK #-} !FileId
  , locPos :: {-# UNPACK #-} !Offset
  , locSize :: {-# UNPACK #-} !Word32
  -- ^ total size of the record on disk, header included
  , locTstamp :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

-- | When to push writes through to the disk.
--
-- The default is 'SyncNever', which matches the paper (its @o_sync@ is off by
-- default) and is the honest one: Bitcask's durability story is \"call
-- 'Database.Bitcask.sync', or accept losing the tail\".
data SyncPolicy
  = SyncNever
  | SyncOnPut
  | SyncEvery !Int
  -- ^ sync after every N writes
  | SyncEveryMicros !Int
  -- ^ sync from a background thread on an interval
  deriving stock (Eq, Show)

-- | When to merge.
data MergePolicy
  = MergeManual
  | MergeAuto
      { mergeDeadRatio :: !Double
      -- ^ merge once this fraction of the immutable bytes is dead
      , mergeCheckMicros :: !Int
      -- ^ how often to check
      }
  deriving stock (Eq, Show)

data OpenOptions = OpenOptions
  { maxFileSize :: !Word64
  -- ^ roll the active file once it passes this size
  , syncPolicy :: !SyncPolicy
  , readOnly :: !Bool
  -- ^ open for reading only, taking a shared lock
  , verifyChecksums :: !Bool
  -- ^ verify the CRC of every record read
  , repairTruncated :: !Bool
  -- ^ truncate a torn record at the end of the active file instead of failing
  , mergePolicy :: !MergePolicy
  , storeTag :: !(Maybe Text)
  -- ^ if set, recorded in @bitcask.meta@ and checked on every open; use it to
  -- catch opening a store with the wrong key\/value types
  }
  deriving stock (Eq, Show)

defaultOptions :: OpenOptions
defaultOptions =
  OpenOptions
    { maxFileSize = 2 * 1024 * 1024 * 1024
    , syncPolicy = SyncNever
    , readOnly = False
    , verifyChecksums = True
    , repairTruncated = True
    , mergePolicy = MergeManual
    , storeTag = Nothing
    }

data Stats = Stats
  { statsKeys :: !Int
  , statsDataFiles :: !Int
  , statsLiveBytes :: !Word64
  -- ^ bytes reachable from the keydir
  , statsTotalBytes :: !Word64
  -- ^ bytes on disk across all data files
  }
  deriving stock (Eq, Show)

data MergeStats = MergeStats
  { mergedFiles :: !Int
  , mergedRecords :: !Int
  , reclaimedBytes :: !Word64
  }
  deriving stock (Eq, Show)

-- | Which half of a record failed to decode.
data Field = KeyField | ValueField
  deriving stock (Eq, Show)

-- | Something wrong with the bytes of a single record.
data RecordError
  = -- | expected, available
    TruncatedRecord !Int !Int
  | -- | stored, computed
    ChecksumMismatch !Word32 !Word32
  | BadFlags !Word8
  | -- | a tombstone with a non-empty value
    MalformedTombstone
  | BadHintTrailer
  deriving stock (Eq, Show)

data BitcaskError
  = -- | another process holds the lock, and its pid if we can tell
    LockHeld !FilePath !(Maybe Word32)
  | CorruptRecord !FilePath !Offset !RecordError
  | -- | the CRC was fine but the bytes would not decode, which almost always
    -- means the store was opened at the wrong key\/value types
    DecodeFailure !Field !String
  | -- | expected, found
    SchemaMismatch !FilePath !Text !Text
  | UnsupportedFormat !FilePath !Word32
  | KeyTooLarge !Int
  | ValueTooLarge !Int
  | NotAStore !FilePath
  | WriteToReadOnly
  | UseAfterClose
  | -- | A write failed in a way that leaves the end of the active data file in
    -- an unknown state: an @fsync@ failed, or a failed append could not be
    -- undone. Every later write fails with this rather than risk recording
    -- data at the wrong place. Reads still work. Close and reopen the store to
    -- recover; reopening repairs the file. The message says what failed.
    StoreBroken !String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Encoded keys may not exceed 64 KiB, because @ksz@ is a 'Word16'.
maxKeySize :: Int
maxKeySize = 0xFFFF

-- | Encoded values may not exceed 4 GiB, because @vsz@ is a 'Word32'.
maxValueSize :: Int
maxValueSize = 0xFFFFFFFF
