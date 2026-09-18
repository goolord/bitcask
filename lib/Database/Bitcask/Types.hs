-- | Shared types: file ids, keydir entries, options and errors.
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

-- | A data file's id within a store.
--
-- Two 32-bit counters packed into a 'Word64'. The high half is the /base/,
-- bumped each time the active file rolls. The low half is the /sub/, only used
-- by merge.
--
-- @('FileId', offset)@ orders writes without relying on the clock, and merge
-- can pick ids that sort before the active file. Merge output is older than
-- anything written during the merge, so it has to replay first, and a flat
-- counter has no id for it. See @DESIGN.md@ §3.1.
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

-- | Where a key's live record is. One per key in the keydir.
--
-- 'locPos' is the offset of the record, not the value. Reading the whole
-- record is @19 + keySize@ extra bytes and lets 'Database.Bitcask.get' check
-- the CRC.
data Loc = Loc
  { locFileId :: {-# UNPACK #-} !FileId
  , locPos :: {-# UNPACK #-} !Offset
  , locSize :: {-# UNPACK #-} !Word32
  -- ^ total size of the record on disk, header included
  , locTstamp :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

-- | When to @fsync@.
--
-- Defaults to 'SyncNever', like the paper (@o_sync@ is off by default). Call
-- 'Database.Bitcask.sync' or accept losing the tail on a crash.
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
  -- ^ stored in @bitcask.meta@ and checked on open; catches opening a store
  -- at the wrong key\/value types
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
  | -- | CRC passed but the bytes didn't decode; usually means the store was
    -- opened at the wrong key\/value types
    DecodeFailure !Field !String
  | -- | expected, found
    SchemaMismatch !FilePath !Text !Text
  | UnsupportedFormat !FilePath !Word32
  | KeyTooLarge !Int
  | ValueTooLarge !Int
  | NotAStore !FilePath
  | WriteToReadOnly
  | UseAfterClose
  | -- | An @fsync@ failed, or a failed append couldn't be undone, so the end of
    -- the active file is unknown. All later writes fail with this. Reads still
    -- work. Close and reopen to recover. The message says what failed.
    StoreBroken !String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Max encoded key size, 64 KiB (@ksz@ is a 'Word16').
maxKeySize :: Int
maxKeySize = 0xFFFF

-- | Max encoded value size, 4 GiB (@vsz@ is a 'Word32').
maxValueSize :: Int
maxValueSize = 0xFFFFFFFF
