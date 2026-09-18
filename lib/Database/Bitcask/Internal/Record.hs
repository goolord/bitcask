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

import Control.Monad (unless, when)
import Data.Bits (complement, shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word16, Word32, Word64, Word8)

import Database.Bitcask.Internal.CRC32 (crc32)
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
-- The body is materialised strictly because the CRC covers it; there is no way
-- to write the checksum first and stream the rest.
encodeRecord :: Word64 -> ByteString -> Maybe ByteString -> ByteString
encodeRecord ts k mv = BS.concat [beWord32 (crc32 body), body]
  where
    v = maybe BS.empty id mv
    flags = maybe tombstoneFlag (const 0) mv
    body =
      BL.toStrict . BB.toLazyByteString $
        BB.word64BE ts
          <> BB.word8 flags
          <> BB.word16BE (fromIntegral (BS.length k) :: Word16)
          <> BB.word32BE (fromIntegral (BS.length v) :: Word32)
          <> BB.byteString k
          <> BB.byteString v

-- | Decode a header from at least 'headerSize' bytes.
decodeHeader :: ByteString -> Either RecordError Header
decodeHeader bs
  | BS.length bs < headerSize = Left (TruncatedRecord headerSize (BS.length bs))
  | otherwise =
      Right
        Header
          { hdrCrc = be32 bs 0
          , hdrTstamp = be64 bs 4
          , hdrFlags = BSU.unsafeIndex bs 12
          , hdrKeySize = fromIntegral (be16 bs 13)
          , hdrValSize = fromIntegral (be32 bs 15)
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
  let body = BS.take (total - 4) (BS.drop 4 bs)
      actual = crc32 body
  unless (not verify || actual == hdrCrc h) $ Left (ChecksumMismatch (hdrCrc h) actual)
  let k = BS.take (hdrKeySize h) (BS.drop headerSize bs)
      v = BS.take (hdrValSize h) (BS.drop (headerSize + hdrKeySize h) bs)
  pure
    Record
      { recTstamp = hdrTstamp h
      , recKey = k
      , recValue = if isTomb then Nothing else Just v
      }

beWord32 :: Word32 -> ByteString
beWord32 = BL.toStrict . BB.toLazyByteString . BB.word32BE

be16 :: ByteString -> Int -> Word16
be16 bs o =
  (fromIntegral (BSU.unsafeIndex bs o) `shiftL` 8)
    .|. fromIntegral (BSU.unsafeIndex bs (o + 1))

be32 :: ByteString -> Int -> Word32
be32 bs o = go 0 0
  where
    go :: Int -> Word32 -> Word32
    go i acc
      | i == 4 = acc
      | otherwise = go (i + 1) ((acc `shiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs (o + i)))

be64 :: ByteString -> Int -> Word64
be64 bs o = go 0 0
  where
    go :: Int -> Word64 -> Word64
    go i acc
      | i == 8 = acc
      | otherwise = go (i + 1) ((acc `shiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs (o + i)))
