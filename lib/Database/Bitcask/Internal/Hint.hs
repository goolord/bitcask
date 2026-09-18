-- | Hint files: the same records as a data file, minus the values, plus the
-- position of each record. They exist so that opening a store does not have to
-- read every value off disk to rebuild the keydir.
--
-- > tstamp:8 | flags:1 | ksz:2 | vsz:4 | recordPos:8 | key:ksz
--
-- and a 12-byte trailer after the last entry:
--
-- > recordCount:8 | crc:4
--
-- The trailer is what distinguishes a complete hint file from one a crash cut
-- short: without it, or with a checksum that does not match, the hint file is
-- ignored and the data file is scanned instead. Hint files are pure cache —
-- deleting every one of them costs startup time and nothing else.
module Database.Bitcask.Internal.Hint
  ( HintEntry (..)
  , hintEntrySize
  , hintTrailerSize
  , encodeHintEntry
  , encodeHintTrailer
  , decodeHintFile
  ) where

import Data.Bits (testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word32, Word64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)

import Database.Bitcask.Internal.Bytes
import Database.Bitcask.Internal.CRC32 (crc32)
import Database.Bitcask.Internal.Record (tombstoneFlag)
import Database.Bitcask.Types (Offset, RecordError (..))

data HintEntry = HintEntry
  { hintTstamp :: !Word64
  , hintTombstone :: !Bool
  , hintKey :: !ByteString
  , hintValSize :: !Word32
  , hintPos :: !Offset
  -- ^ offset of the /record/ in the data file
  , hintRecSize :: !Word32
  -- ^ total on-disk size of the record
  }
  deriving stock (Eq, Show)

-- | Fixed part of a hint entry, before the key bytes.
hintEntrySize :: Int
hintEntrySize = 23

hintTrailerSize :: Int
hintTrailerSize = 12

encodeHintEntry :: HintEntry -> ByteString
encodeHintEntry e = BSI.unsafeCreate (hintEntrySize + klen) $ \p -> do
  pokeBE64 p 0 (hintTstamp e)
  pokeByteOff p 8 (if hintTombstone e then tombstoneFlag else 0)
  pokeBE16 p 9 (fromIntegral klen)
  pokeBE32 p 11 (hintValSize e)
  pokeBE64 p 15 (hintPos e)
  BSU.unsafeUseAsCString (hintKey e) $ \kp ->
    copyBytes (p `plusPtr` hintEntrySize) (castPtr kp) klen
  where
    klen = BS.length (hintKey e)

-- | The trailer, given the number of entries and the CRC accumulated over all
-- the entry bytes that precede it.
encodeHintTrailer :: Word64 -> Word32 -> ByteString
encodeHintTrailer n c = BSI.unsafeCreate hintTrailerSize $ \p -> do
  pokeBE64 p 0 n
  pokeBE32 p 8 c

-- | Every entry of a hint file, in order, or a refusal. Refusing is always safe:
-- the caller falls back to scanning the data file.
--
-- The whole file is checked — checksum, entry framing, entry count — before
-- anything is returned, so a refused file never yields a partial result. The
-- list itself is then produced lazily, so a consumer that streams it (the keydir
-- rebuild does) never holds more than one entry at a time. Keys are slices of
-- the input; copy them if they are to outlive it.
decodeHintFile :: ByteString -> Either RecordError [HintEntry]
decodeHintFile bs
  | BS.length bs < hintTrailerSize = Left BadHintTrailer
  | crc32 body /= storedCrc = Left BadHintTrailer
  | not (framed 0 0) = Left BadHintTrailer
  | otherwise = Right (entries 0)
  where
    (body, trailer) = BS.splitAt (BS.length bs - hintTrailerSize) bs
    storedCount = indexBE64 trailer 0
    storedCrc = indexBE32 trailer 8
    len = BS.length body

    keySize off = fromIntegral (indexBE16 body (off + 9)) :: Int

    -- Every entry lies wholly inside the body, and there are as many as the
    -- trailer says.
    framed :: Int -> Word64 -> Bool
    framed !off !n
      | off == len = n == storedCount
      | len - off < hintEntrySize = False
      | otherwise =
          let total = hintEntrySize + keySize off
           in len - off >= total && framed (off + total) (n + 1)

    entries off
      | off >= len = []
      | otherwise =
          let ksz = keySize off
              vsz = indexBE32 body (off + 11)
              e =
                HintEntry
                  { hintTstamp = indexBE64 body off
                  , hintTombstone = testBit (BSU.unsafeIndex body (off + 8)) 0
                  , hintKey = BSU.unsafeTake ksz (BSU.unsafeDrop (off + hintEntrySize) body)
                  , hintValSize = vsz
                  , hintPos = indexBE64 body (off + 15)
                  , hintRecSize = fromIntegral (19 + ksz) + vsz
                  }
           in e : entries (off + hintEntrySize + ksz)
