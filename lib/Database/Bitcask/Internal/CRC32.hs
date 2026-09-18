-- | CRC-32 (IEEE 802.3, the reflected @0xEDB88320@ polynomial — the same
-- checksum zlib computes).
--
-- This is implemented here rather than pulled from @digest@ deliberately:
-- @digest@ binds to zlib, and requiring a C library to be present is exactly the
-- kind of friction that makes a package painful to install on Windows.
--
-- It is the \"slicing-by-8\" variant: eight 256-entry tables let the inner loop
-- consume eight bytes per iteration with eight independent table lookups, rather
-- than one byte per iteration with a dependency on the previous step. Every
-- 'Database.Bitcask.get' checksums the record it reads, so this is on the read
-- path, and it is several times faster than the byte-at-a-time table. The tail
-- that does not fill a whole eight-byte step falls back to the classic loop.
module Database.Bitcask.Internal.CRC32
  ( crc32
  , crc32Update
  , crc32Ptr
  ) where

import Data.Bits (complement, shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayFromListN)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Word (byteSwap32)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | CRC-32 of a byte string. @'crc32' ""@ is @0@.
crc32 :: BS.ByteString -> Word32
crc32 = crc32Update 0

-- | Continue a CRC-32 over another chunk, so a checksum can be computed
-- incrementally while streaming bytes out to a file.
crc32Update :: Word32 -> BS.ByteString -> Word32
crc32Update seed bs =
  unsafeDupablePerformIO . BSU.unsafeUseAsCStringLen bs $ \(p, n) -> crc32Ptr seed (castPtr p) n

-- | 'crc32Update' over raw memory, for checksumming a buffer while it is being
-- built.
crc32Ptr :: Word32 -> Ptr Word8 -> Int -> IO Word32
crc32Ptr seed p n = do
  let !mid = p `plusPtr` (n - n `rem` 8)
  c <- slice8 p mid (complement seed)
  complement <$> bytewise mid (p `plusPtr` n) c

-- | Eight bytes at a time, up to (but not including) @end@.
slice8 :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO Word32
slice8 !p !end !c
  | p >= end = pure c
  | otherwise = do
      lo <- le32 p
      hi <- le32 (p `plusPtr` 4)
      let x = c `xor` lo
          c' =
            tab 7 x
              `xor` tab 6 (x `shiftR` 8)
              `xor` tab 5 (x `shiftR` 16)
              `xor` tab 4 (x `shiftR` 24)
              `xor` tab 3 hi
              `xor` tab 2 (hi `shiftR` 8)
              `xor` tab 1 (hi `shiftR` 16)
              `xor` tab 0 (hi `shiftR` 24)
      slice8 (p `plusPtr` 8) end c'

-- | One byte at a time, for the tail.
bytewise :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO Word32
bytewise !p !end !c
  | p >= end = pure c
  | otherwise = do
      b <- peek p
      bytewise (p `plusPtr` 1) end (tab 0 (c `xor` fromIntegral (b :: Word8)) `xor` (c `shiftR` 8))

-- | Table @k@, indexed by the low byte of the argument.
tab :: Int -> Word32 -> Word32
tab k i = indexPrimArray tables (k * 256 + fromIntegral (i .&. 0xFF))
{-# INLINE tab #-}

le32 :: Ptr Word8 -> IO Word32
le32 p = do
  w <- peekByteOff p 0
  pure $! if targetByteOrder == LittleEndian then w else byteSwap32 w
{-# INLINE le32 #-}

-- | The eight tables, back to back. Table 0 is the classic byte-at-a-time table;
-- table @k@ advances a byte through @k@ further zero bytes.
tables :: PrimArray Word32
tables = primArrayFromListN (8 * 256) (concat (take 8 (iterate next t0)))
  where
    t0 = [entry (fromIntegral n) | n <- [0 :: Int .. 255]]
    next t = [(c `shiftR` 8) `xor` (t0 !! fromIntegral (c .&. 0xFF)) | c <- t]
    entry :: Word32 -> Word32
    entry = go (8 :: Int)
    go 0 c = c
    go k c
      | c .&. 1 /= 0 = go (k - 1) (0xEDB88320 `xor` (c `shiftR` 1))
      | otherwise = go (k - 1) (c `shiftR` 1)
{-# NOINLINE tables #-}
