-- | How a Haskell value becomes the bytes that go on disk.
--
-- The store is parameterised over its key and value types
-- (@'Database.Bitcask.Bitcask' k v@) and everything below this module works in
-- terms of encoded bytes, so this is purely a boundary: one encode on write, one
-- decode on read, and nothing else changes.
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

-- | Encoding and decoding for something a Bitcask store can hold.
--
-- The law is a plain roundtrip:
--
-- prop> fromBytes (encodeStrict x) == Right x
--
-- It is worth saying why that single law is enough for keys. A law of that shape
-- forces 'toBytes' to be /injective/: if two distinct values encoded to the same
-- bytes, 'fromBytes' could not return both. Injectivity is exactly what a key
-- needs — two keys that encoded alike would silently become one key, and nothing
-- would ever report it. So the law is the correctness condition for the key side
-- rather than decoration, and every instance here is property-tested against it.
--
-- Two further obligations that the type cannot express, and that the haddocks for
-- your own instances should repeat:
--
-- * A key encoding must be /deterministic/. Anything whose byte layout depends
--   on iteration order — a @HashMap@, a @Set@ built in a different order — is
--   not a lawful key.
--
-- * A key encoding must be /stable across releases of your program/. Change it
--   and every key already on disk is orphaned, with no error to tell you.
class Codec a where
  toBytes :: a -> Builder
  fromBytes :: ByteString -> Either String a

-- | Run 'toBytes' and get the strict bytes that would be written.
--
-- Every 'Database.Bitcask.put' runs this twice, so the allocation strategy is
-- tuned for what keys and values usually are: small. The default strategy starts
-- with a four-kilobyte buffer and then copies the result out of it if it came
-- out much smaller, which for a 16-byte key is two allocations and a copy to
-- produce 16 bytes. This starts with a buffer just big enough for a typical
-- small key or value, and does not trim: when the whole encoding fits the
-- first buffer, the result is a slice of it and there is no copy at all.
encodeStrict :: (Codec a) => a -> ByteString
encodeStrict =
  BL.toStrict
    . BBE.toLazyByteStringWith (BBE.untrimmedStrategy 128 BBE.smallChunkSize) BL.empty
    . toBytes
{-# INLINE encodeStrict #-}

-- $deriving
--
-- Rather than inventing another serialisation format, adapt one you already
-- have. 'AsSerialize' covers @cereal@:
--
-- > data User = User { userName :: String, userAge :: Int }
-- >   deriving stock (Show, Generic)
-- >   deriving anyclass (Serialize)
-- >   deriving Codec via (AsSerialize User)
--
-- Adapters for @binary@, @store@ or @serialise@ are a few lines each and belong
-- in your project rather than behind a cabal flag on this one:
--
-- > newtype AsBinary a = AsBinary a
-- > instance Binary a => Codec (AsBinary a) where
-- >   toBytes (AsBinary a) = Binary.execPut (Binary.put a)
-- >   fromBytes bs = case Binary.decodeOrFail (BL.fromStrict bs) of
-- >     Right (rest, _, a) | BL.null rest -> Right (AsBinary a)
-- >     _ -> Left "AsBinary: decode failed"

-- | Derive a 'Codec' from a @cereal@ 'Cereal.Serialize' instance, via
-- @DerivingVia@.
newtype AsSerialize a = AsSerialize {unAsSerialize :: a}

instance (Cereal.Serialize a) => Codec (AsSerialize a) where
  toBytes (AsSerialize a) = BB.byteString (Cereal.encode a)
  -- Not 'Cereal.decode': it ignores whatever follows a complete value.
  fromBytes = fmap AsSerialize . Cereal.runGet (Cereal.get <* end)
    where
      end = do
        done <- Cereal.isEmpty
        unless done $ fail "AsSerialize: trailing bytes after a complete value"

-- Every instance below rejects trailing bytes. A decoder that ignores them would
-- break the roundtrip law in the other direction and, for keys, would make two
-- different byte strings decode to the same value.

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

-- | Encoded as a big-endian 'Int64', so a store written on a 64-bit machine
-- reads on a 32-bit one.
instance Codec Int where
  toBytes = BB.int64BE . fromIntegral
  fromBytes = fmap (fromIntegral @Int64) . fromBytes

-- | Encoded as IEEE-754 big-endian. Beware as a /key/: distinct @NaN@ payloads
-- are distinct keys, and @0@ and @-0@ are two keys that compare equal.
instance Codec Double where
  toBytes = BB.doubleBE
  fromBytes = fmap castWord64ToDouble . fromBytes
