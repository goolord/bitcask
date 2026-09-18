module Test.Bitcask.Codec (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.QuickCheck

import Database.Bitcask

import Test.Bitcask.Util (smallBytes)

tests :: TestTree
tests =
  testGroup
    "Codec"
    [ testGroup
        "roundtrip law"
        [ testProperty "ByteString" $ forAll smallBytes law
        , testProperty "Text" $ \s -> law (T.pack s)
        , testProperty "Int" $ \(n :: Int) -> law n
        , testProperty "Int64" $ \(n :: Int64) -> law n
        , testProperty "Word32" $ \(n :: Word32) -> law n
        , testProperty "Word64" $ \(n :: Word64) -> law n
        , testProperty "Bool" $ \(b :: Bool) -> law b
        , testProperty "()" $ law ()
        , testProperty "AsSerialize" $ \n s -> lawVia (User s n)
        ]
    , -- The roundtrip law is what forces 'toBytes' to be injective, which is the
      -- property keys actually need: two keys that encoded alike would silently
      -- become one key. Checking it directly on a small domain is cheap.
      testProperty "key encodings are injective (ByteString)" $
        forAll smallBytes $ \a ->
          forAll smallBytes $ \b ->
            (encodeStrict a == encodeStrict b) === (a == (b :: ByteString))
    , testProperty "key encodings are injective (Int)" $ \(a :: Int) b ->
        (encodeStrict a == encodeStrict b) === (a == b)
    , testProperty "decoders reject trailing bytes" $ \(n :: Word64) ->
        let bs = encodeStrict n <> BS.singleton 0
         in case fromBytes bs :: Either String Word64 of
              Left _ -> True
              Right _ -> False
    ]
  where
    law :: (Codec a, Eq a, Show a) => a -> Property
    law x = fromBytes (encodeStrict x) === Right x

    lawVia :: User -> Property
    lawVia x =
      (unAsSerialize <$> fromBytes (encodeStrict (AsSerialize x))) === Right x

data User = User {userName :: String, userAge :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

instance Eq (AsSerialize User) where
  AsSerialize a == AsSerialize b = a == b

instance Show (AsSerialize User) where
  show (AsSerialize a) = show a
