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

import Data.Bits (shiftL, testBit, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word16, Word32, Word64)

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
encodeHintEntry e =
  BL.toStrict . BB.toLazyByteString $
    BB.word64BE (hintTstamp e)
      <> BB.word8 (if hintTombstone e then tombstoneFlag else 0)
      <> BB.word16BE (fromIntegral (BS.length (hintKey e)) :: Word16)
      <> BB.word32BE (hintValSize e)
      <> BB.word64BE (hintPos e)
      <> BB.byteString (hintKey e)

-- | The trailer, given the number of entries and the CRC accumulated over all
-- the entry bytes that precede it.
encodeHintTrailer :: Word64 -> Word32 -> ByteString
encodeHintTrailer n c =
  BL.toStrict . BB.toLazyByteString $ BB.word64BE n <> BB.word32BE c

-- | Decode a whole hint file, or refuse it. Refusing is always safe: the caller
-- falls back to scanning the data file.
decodeHintFile :: ByteString -> Either RecordError [HintEntry]
decodeHintFile bs
  | BS.length bs < hintTrailerSize = Left BadHintTrailer
  | crc32 body /= storedCrc = Left BadHintTrailer
  | otherwise = do
      es <- go body
      if fromIntegral (length es) == storedCount
        then Right es
        else Left BadHintTrailer
  where
    (body, trailer) = BS.splitAt (BS.length bs - hintTrailerSize) bs
    storedCount = be64 trailer 0
    storedCrc = be32 trailer 8

    go :: ByteString -> Either RecordError [HintEntry]
    go rest
      | BS.null rest = Right []
      | BS.length rest < hintEntrySize = Left BadHintTrailer
      | otherwise =
          let ksz = fromIntegral (be16 rest 9) :: Int
              total = hintEntrySize + ksz
           in if BS.length rest < total
                then Left BadHintTrailer
                else do
                  let flags = BSU.unsafeIndex rest 8
                      e =
                        HintEntry
                          { hintTstamp = be64 rest 0
                          , hintTombstone = testBit flags 0
                          , hintKey = BS.take ksz (BS.drop hintEntrySize rest)
                          , hintValSize = be32 rest 11
                          , hintPos = be64 rest 15
                          , hintRecSize = fromIntegral (19 + ksz + fromIntegral (be32 rest 11))
                          }
                  (e :) <$> go (BS.drop total rest)

be16 :: ByteString -> Int -> Word16
be16 bs o =
  (fromIntegral (BSU.unsafeIndex bs o) `shiftL` 8)
    .|. fromIntegral (BSU.unsafeIndex bs (o + 1))

be32 :: ByteString -> Int -> Word32
be32 bs o = foldl (\acc i -> (acc `shiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs (o + i))) 0 [0 .. 3]

be64 :: ByteString -> Int -> Word64
be64 bs o = foldl (\acc i -> (acc `shiftL` 8) .|. fromIntegral (BSU.unsafeIndex bs (o + i))) 0 [0 .. 7]
