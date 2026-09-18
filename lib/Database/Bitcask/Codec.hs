-- | Encoding keys and values to bytes.
--
-- Everything below @'Database.Bitcask.Bitcask' k v@ works on bytes. This is
-- one encode on write and one decode on read.
module Database.Bitcask.Codec
  ( Codec (..)
  , encodeStrict

    -- * Deriving a codec from a library you already use
    -- $deriving
  , AsSerialize (..)
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.ByteString.Lazy as BL
import Data.Bits (Bits, shiftL)
import Data.Int (Int32, Int64)
import qualified Data.Serialize as Cereal
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64, Word8)
import GHC.Float (castWord64ToDouble)

-- | Encoding and decoding for keys and values.
--
-- Law:
--
-- prop> fromBytes (encodeStrict x) == Right x
--
-- This makes 'toBytes' injective, which is what keys need: two keys that
-- encoded the same would silently become one. Every instance here is
-- property-tested against it.
--
-- Key encodings also need to be:
--
-- * /Deterministic./ Anything whose bytes depend on iteration order (a
--   @HashMap@, say) is not a valid key.
--
-- * /Stable across versions of your program./ If the encoding changes, every
--   existing key is orphaned with no error.
class Codec a where
  toBytes :: a -> Builder
  fromBytes :: ByteString -> Either String a

-- | Run 'toBytes' and get strict bytes.
--
-- Every 'Database.Bitcask.put' calls this twice, so it's tuned for small
-- inputs. The default strategy starts with a 4 KiB buffer and trims, which is
-- two allocations and a copy for a 16-byte key. This starts with a small
-- buffer and doesn't trim, so if the encoding fits it's just a slice.
encodeStrict :: (Codec a) => a -> ByteString
encodeStrict =
  BL.toStrict
    . BBE.toLazyByteStringWith (BBE.untrimmedStrategy 128 BBE.smallChunkSize) BL.empty
    . toBytes
{-# INLINE encodeStrict #-}

-- $deriving
--
-- Use a serialisation library you already have. 'AsSerialize' covers @cereal@:
--
-- > data User = User { userName :: String, userAge :: Int }
-- >   deriving stock (Show, Generic)
-- >   deriving anyclass (Serialize)
-- >   deriving Codec via (AsSerialize User)
--
-- Adapters for @binary@, @store@ or @serialise@ are a few lines each:
--
-- > newtype AsBinary a = AsBinary a
-- > instance Binary a => Codec (AsBinary a) where
-- >   toBytes (AsBinary a) = Binary.execPut (Binary.put a)
-- >   fromBytes bs = case Binary.decodeOrFail (BL.fromStrict bs) of
-- >     Right (rest, _, a) | BL.null rest -> Right (AsBinary a)
-- >     _ -> Left "AsBinary: decode failed"

-- | 'Codec' from a @cereal@ 'Cereal.Serialize' instance, for @DerivingVia@.
newtype AsSerialize a = AsSerialize {unAsSerialize :: a}

instance (Cereal.Serialize a) => Codec (AsSerialize a) where
  toBytes (AsSerialize a) = BB.byteString (Cereal.encode a)
  -- Not 'Cereal.decode', which ignores trailing bytes.
  fromBytes = fmap AsSerialize . Cereal.runGet (Cereal.get <* end)
    where
      end = do
        done <- Cereal.isEmpty
        unless done $ fail "AsSerialize: trailing bytes after a complete value"

-- All instances below reject trailing bytes. Otherwise two different byte
-- strings could decode to the same key.

instance Codec ByteString where
  toBytes = BB.byteString
  fromBytes = Right

instance Codec BL.ByteString where
  toBytes = BB.lazyByteString
  fromBytes = Right . BL.fromStrict

instance Codec Text where
  toBytes = TE.encodeUtf8Builder
  fromBytes bs = case TE.decodeUtf8' bs of
    Left err -> Left ("Codec Text: " <> show err)
    Right t -> Right t

instance Codec () where
  toBytes () = mempty
  fromBytes bs
    | BS.null bs = Right ()
    | otherwise = Left "Codec (): expected no bytes"

instance Codec Bool where
  toBytes b = BB.word8 (if b then 1 else 0)
  fromBytes bs = case BS.unpack bs of
    [0] -> Right False
    [1] -> Right True
    _ -> Left "Codec Bool: expected a single 0x00 or 0x01 byte"

fixed :: Int -> (ByteString -> a) -> String -> ByteString -> Either String a
fixed n f name bs
  | BS.length bs == n = Right (f bs)
  | otherwise = Left (name <> ": expected " <> show n <> " bytes, got " <> show (BS.length bs))

-- | Big-endian decode of a fixed-width unsigned integer.
be :: (Bits a, Num a) => ByteString -> a
be = BS.foldl' (\acc b -> (acc `shiftL` 8) + fromIntegral b) 0

instance Codec Word8 where
  toBytes = BB.word8
  fromBytes = fixed 1 BS.head "Codec Word8"

instance Codec Word32 where
  toBytes = BB.word32BE
  fromBytes = fixed 4 be "Codec Word32"

instance Codec Word64 where
  toBytes = BB.word64BE
  fromBytes = fixed 8 be "Codec Word64"

instance Codec Int32 where
  toBytes = BB.int32BE
  fromBytes = fixed 4 (fromIntegral @Word32 . be) "Codec Int32"

instance Codec Int64 where
  toBytes = BB.int64BE
  fromBytes = fixed 8 (fromIntegral @Word64 . be) "Codec Int64"

-- | Big-endian 'Int64', so the encoding doesn't depend on word size.
instance Codec Int where
  toBytes = BB.int64BE . fromIntegral
  fromBytes = fmap (fromIntegral @Int64) . fromBytes

-- | IEEE-754 big-endian. Careful using these as keys: different @NaN@ payloads
-- are different keys, and @0@ and @-0@ are two keys that compare equal.
instance Codec Double where
  toBytes = BB.doubleBE
  fromBytes = fmap castWord64ToDouble . fromBytes
