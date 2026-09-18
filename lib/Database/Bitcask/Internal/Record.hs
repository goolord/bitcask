-- | The on-disk record codec. Pure, total, and the single source of truth for
-- the file format.
--
-- > offset  size  field
-- >      0     4  crc     CRC-32 over bytes 4..end
-- >      4     8  tstamp  Word64, nanoseconds since the Unix epoch
-- >     12     1  flags   bit 0 = tombstone; bits 1-7 reserved, must be 0
-- >     13     2  ksz     Word16
-- >     15     4  vsz     Word32
-- >     19   ksz  key
-- > 19+ksz   vsz  value
--
-- All integers are big-endian.
--
-- Two deliberate divergences from the paper. Tombstones get a flag bit rather
-- than the paper's \"special tombstone value\", which keeps the value space total
-- — you can store the empty string, and there is no magic byte string that is
-- secretly unstorable. And the sizes are fixed-width rather than varints, which
-- caps keys at 64 KiB and values at 4 GiB but makes the header a constant 19
-- bytes, so a reader can pull a header without parsing anything first.
module Database.Bitcask.Internal.Record
  ( -- * Layout
    headerSize
  , tombstoneFlag
  , recordSize

    -- * Types
  , Header (..)
  , Record (..)

    -- * Encoding
  , encodeRecord

    -- * Decoding
  , decodeHeader
  , decodeRecord
  ) where

import Control.Monad (when)
import Data.Bits (complement, testBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)

import Database.Bitcask.Internal.Bytes
import Database.Bitcask.Internal.CRC32 (crc32, crc32Ptr)
import Database.Bitcask.Types (RecordError (..))

-- | Size of the fixed record header, in bytes.
headerSize :: Int
headerSize = 19

-- | Bit 0 of the flags byte.
tombstoneFlag :: Word8
tombstoneFlag = 0x01

data Header = Header
  { hdrCrc :: !Word32
  , hdrTstamp :: !Word64
  , hdrFlags :: !Word8
  , hdrKeySize :: !Int
  , hdrValSize :: !Int
  }
  deriving stock (Eq, Show)

data Record = Record
  { recTstamp :: !Word64
  , recKey :: !ByteString
  , recValue :: !(Maybe ByteString)
  -- ^ 'Nothing' is a tombstone
  }
  deriving stock (Eq, Show)

-- | Total on-disk size of the record a header describes.
recordSize :: Header -> Int
recordSize h = headerSize + hdrKeySize h + hdrValSize h

-- | Encode one record. Passing 'Nothing' for the value writes a tombstone.
--
-- One allocation of exactly the record's size: the fields are poked in, then the
-- CRC is computed over the buffer in place and poked in front of them.
encodeRecord :: Word64 -> ByteString -> Maybe ByteString -> ByteString
encodeRecord ts k mv = BSI.unsafeCreate total $ \p -> do
  pokeBE64 p 4 ts
  pokeByteOff p 12 flags
  pokeBE16 p 13 (fromIntegral klen)
  pokeBE32 p 15 (fromIntegral vlen)
  BSU.unsafeUseAsCString k $ \kp -> copyBytes (p `plusPtr` headerSize) (castPtr kp) klen
  BSU.unsafeUseAsCString v $ \vp -> copyBytes (p `plusPtr` (headerSize + klen)) (castPtr vp) vlen
  crc <- crc32Ptr 0 (p `plusPtr` 4) (total - 4)
  pokeBE32 p 0 crc
  where
    v = fromMaybe BS.empty mv
    flags = maybe tombstoneFlag (const 0) mv
    klen = BS.length k
    vlen = BS.length v
    total = headerSize + klen + vlen

-- | Decode a header from at least 'headerSize' bytes.
decodeHeader :: ByteString -> Either RecordError Header
decodeHeader bs
  | BS.length bs < headerSize = Left (TruncatedRecord headerSize (BS.length bs))
  | otherwise =
      Right
        Header
          { hdrCrc = indexBE32 bs 0
          , hdrTstamp = indexBE64 bs 4
          , hdrFlags = BSU.unsafeIndex bs 12
          , hdrKeySize = fromIntegral (indexBE16 bs 13)
          , hdrValSize = fromIntegral (indexBE32 bs 15)
          }

-- | Decode one record from a buffer whose first byte is the start of the record.
-- Trailing bytes beyond the record are ignored, so this can be pointed straight
-- at a scan buffer.
--
-- Pass 'False' to skip checksum verification.
decodeRecord :: Bool -> ByteString -> Either RecordError Record
decodeRecord verify bs = do
  h <- decodeHeader bs
  let total = recordSize h
  when (BS.length bs < total) $ Left (TruncatedRecord total (BS.length bs))
  when (hdrFlags h .&. complement tombstoneFlag /= 0) $ Left (BadFlags (hdrFlags h))
  let isTomb = testBit (hdrFlags h) 0
  when (isTomb && hdrValSize h /= 0) $ Left MalformedTombstone
  when verify $ do
    let actual = crc32 (BSU.unsafeTake (total - 4) (BSU.unsafeDrop 4 bs))
    when (actual /= hdrCrc h) $ Left (ChecksumMismatch (hdrCrc h) actual)
  let k = BSU.unsafeTake (hdrKeySize h) (BSU.unsafeDrop headerSize bs)
      v = BSU.unsafeTake (hdrValSize h) (BSU.unsafeDrop (headerSize + hdrKeySize h) bs)
  pure
    Record
      { recTstamp = hdrTstamp h
      , recKey = k
      , recValue = if isTomb then Nothing else Just v
      }
