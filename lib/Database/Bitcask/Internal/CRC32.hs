-- | CRC-32 (IEEE 802.3, the reflected @0xEDB88320@ polynomial — the same
-- checksum zlib computes).
--
-- This is implemented here rather than pulled from @digest@ deliberately:
-- @digest@ binds to zlib, and requiring a C library to be present is exactly the
-- kind of friction that makes a package painful to install on Windows. The
-- table-driven version below is pure, has no build dependencies and is easy to
-- check against the standard test vectors.
module Database.Bitcask.Internal.CRC32
  ( crc32
  , crc32Update
  ) where

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (complement, shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Word (Word32, Word8)

-- | CRC-32 of a byte string. @'crc32' ""@ is @0@.
crc32 :: BS.ByteString -> Word32
crc32 = crc32Update 0

-- | Continue a CRC-32 over another chunk, so a checksum can be computed
-- incrementally while streaming bytes out to a file.
crc32Update :: Word32 -> BS.ByteString -> Word32
crc32Update seed = complement . BS.foldl' step (complement seed)
  where
    step :: Word32 -> Word8 -> Word32
    step c b = (table ! fromIntegral ((c `xor` fromIntegral b) .&. 0xFF)) `xor` (c `shiftR` 8)
{-# INLINE crc32Update #-}

table :: UArray Word8 Word32
table = listArray (0, 255) [entry (fromIntegral n) | n <- [0 :: Int .. 255]]
  where
    entry :: Word32 -> Word32
    entry = go (8 :: Int)
    go 0 c = c
    go k c
      | c .&. 1 /= 0 = go (k - 1) (0xEDB88320 `xor` (c `shiftR` 1))
      | otherwise = go (k - 1) (c `shiftR` 1)
{-# NOINLINE table #-}
