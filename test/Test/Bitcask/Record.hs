module Test.Bitcask.Record (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits (xor)
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Database.Bitcask.Internal.CRC32 (crc32)
import Database.Bitcask.Internal.Hint
import Database.Bitcask.Internal.Record

import Test.Bitcask.Util (smallBytes)

tests :: TestTree
tests =
  testGroup
    "Record"
    [ testGroup
        "CRC-32"
        [ testCase "empty" $ crc32 BS.empty @?= 0
        , testCase "check value" $ crc32 (BC.pack "123456789") @?= 0xCBF43926
        , testCase "the quick brown fox" $
            crc32 (BC.pack "The quick brown fox jumps over the lazy dog") @?= 0x414FA339
        ]
    , testProperty "encode/decode roundtrip" $
        forAll smallBytes $ \k ->
          forAll (oneof [Just <$> smallBytes, pure Nothing]) $ \mv ->
            \(ts :: Word64) ->
              decodeRecord True (encodeRecord ts k mv)
                === Right (Record ts k mv)
    , testProperty "an empty value is not a tombstone" $
        forAll smallBytes $ \k ->
          let r = decodeRecord True (encodeRecord 0 k (Just BS.empty))
           in r === Right (Record 0 k (Just BS.empty))
    , testProperty "header size is fixed" $
        forAll smallBytes $ \k ->
          forAll smallBytes $ \v ->
            BS.length (encodeRecord 0 k (Just v)) === headerSize + BS.length k + BS.length v
    , -- Flipping any single bit anywhere in a record has to be caught. This is
      -- the property the whole on-disk format exists to support.
      testProperty "any single-bit flip is detected" $
        forAll smallBytes $ \k ->
          forAll smallBytes $ \v ->
            let bytes = encodeRecord 7 k (Just v)
             in forAll (choose (0, BS.length bytes - 1)) $ \i ->
                  forAll (choose (0, 7)) $ \b ->
                    let flipped = flipBit bytes i b
                     in flipped /= bytes ==> case decodeRecord True flipped of
                          Left _ -> property True
                          Right r -> property (r == Record 7 k (Just v))
    , testProperty "a truncated record does not decode" $
        forAll smallBytes $ \k ->
          forAll smallBytes $ \v ->
            let bytes = encodeRecord 7 k (Just v)
             in forAll (choose (0, BS.length bytes - 1)) $ \n ->
                  case decodeRecord True (BS.take n bytes) of
                    Left _ -> True
                    Right _ -> False
    , testGroup
        "hint files"
        [ testProperty "roundtrip with a trailer" $
            forAll (listOf hintEntry) $ \es ->
              let body = BS.concat (map encodeHintEntry es)
                  file = body <> encodeHintTrailer (fromIntegral (length es)) (crc32 body)
               in decodeHintFile file === Right es
        , testProperty "a hint file with no trailer is refused" $
            forAll (listOf1 hintEntry) $ \es ->
              let body = BS.concat (map encodeHintEntry es)
               in case decodeHintFile body of
                    Left _ -> True
                    Right _ -> False
        ]
    ]

flipBit :: ByteString -> Int -> Int -> ByteString
flipBit bs i b =
  BS.concat
    [ BS.take i bs
    , BS.singleton (BS.index bs i `xor` (1 `rotateTo` b))
    , BS.drop (i + 1) bs
    ]
  where
    rotateTo x n = x * (2 ^ n)

hintEntry :: Gen HintEntry
hintEntry = do
  ts <- arbitrary
  tomb <- arbitrary
  k <- smallBytes
  vsz <- if tomb then pure 0 else fromIntegral <$> choose (0 :: Int, 100)
  pos <- fromIntegral <$> choose (0 :: Int, 10000)
  pure
    HintEntry
      { hintTstamp = ts
      , hintTombstone = tomb
      , hintKey = k
      , hintValSize = vsz
      , hintPos = pos
      , hintRecSize = fromIntegral (headerSize + BS.length k) + vsz
      }
