-- | Big-endian integers in and out of raw memory, for the record and hint codecs.
--
-- The codecs used to go through a 'Data.ByteString.Builder' and then copy the
-- result into a strict 'ByteString', which for a record of a hundred bytes cost
-- more than the write system call it was preparing. Every encoded thing in the
-- file format has its size known up front, so the codecs now allocate exactly
-- once and poke fields straight into the buffer with these.
module Database.Bitcask.Internal.Bytes
  ( pokeBE16
  , pokeBE32
  , pokeBE64
  , peekBE16
  , peekBE32
  , peekBE64
  , indexBE16
  , indexBE32
  , indexBE64
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Word (byteSwap16, byteSwap32, byteSwap64)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- Unaligned loads and stores are fine on every platform GHC targets that we care
-- about (x86-64 and AArch64), and GHC compiles 'peek' at these types to a single
-- load.

be16 :: Word16 -> Word16
be16 = if targetByteOrder == LittleEndian then byteSwap16 else id
{-# INLINE be16 #-}

be32 :: Word32 -> Word32
be32 = if targetByteOrder == LittleEndian then byteSwap32 else id
{-# INLINE be32 #-}

be64 :: Word64 -> Word64
be64 = if targetByteOrder == LittleEndian then byteSwap64 else id
{-# INLINE be64 #-}

pokeBE16 :: Ptr Word8 -> Int -> Word16 -> IO ()
pokeBE16 p o = poke (castPtr (p `plusPtr` o)) . be16
{-# INLINE pokeBE16 #-}

pokeBE32 :: Ptr Word8 -> Int -> Word32 -> IO ()
pokeBE32 p o = poke (castPtr (p `plusPtr` o)) . be32
{-# INLINE pokeBE32 #-}

pokeBE64 :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeBE64 p o = poke (castPtr (p `plusPtr` o)) . be64
{-# INLINE pokeBE64 #-}

peekBE16 :: Ptr Word8 -> Int -> IO Word16
peekBE16 p o = be16 <$> peek (castPtr (p `plusPtr` o))
{-# INLINE peekBE16 #-}

peekBE32 :: Ptr Word8 -> Int -> IO Word32
peekBE32 p o = be32 <$> peek (castPtr (p `plusPtr` o))
{-# INLINE peekBE32 #-}

peekBE64 :: Ptr Word8 -> Int -> IO Word64
peekBE64 p o = be64 <$> peek (castPtr (p `plusPtr` o))
{-# INLINE peekBE64 #-}

-- | Read a field out of a 'ByteString'. The caller checks the bounds.
indexBE16 :: ByteString -> Int -> Word16
indexBE16 bs o = unsafeDupablePerformIO (BSU.unsafeUseAsCString bs (\p -> peekBE16 (castPtr p) o))
{-# INLINE indexBE16 #-}

indexBE32 :: ByteString -> Int -> Word32
indexBE32 bs o = unsafeDupablePerformIO (BSU.unsafeUseAsCString bs (\p -> peekBE32 (castPtr p) o))
{-# INLINE indexBE32 #-}

indexBE64 :: ByteString -> Int -> Word64
indexBE64 bs o = unsafeDupablePerformIO (BSU.unsafeUseAsCString bs (\p -> peekBE64 (castPtr p) o))
{-# INLINE indexBE64 #-}
