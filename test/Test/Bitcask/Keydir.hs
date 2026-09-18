-- Orphan Arbitrary instances for the library's own types. Test-only, and the
-- alternative — newtype wrappers around every type under test — buys nothing.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bitcask.Keydir (tests) where

import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.QuickCheck

import Database.Bitcask.Internal.Keydir (Ref (..))
import qualified Database.Bitcask.Internal.Keydir as KD
import Database.Bitcask.Types

import Test.Bitcask.Util (Key (..))

-- | The keydir is a pure fold over refs in write order, so it can be checked
-- against the obvious model — a 'Data.Map' with the same fold applied — without
-- touching a filesystem.
tests :: TestTree
tests =
  testGroup
    "Keydir"
    [ testProperty "replay agrees with a Map" $ \refs ->
        let want = foldl' model M.empty refs
            got = KD.replay refs
         in M.toAscList want === sortOn' (KD.toList got)
    , testProperty "the last ref for a key wins" $ \(Key k) a b ->
        let refs = [Ref k a False, Ref k b False]
         in KD.lookup k (KD.replay refs) === Just b
    , testProperty "a tombstone removes the key" $ \(Key k) a ->
        KD.lookup k (KD.replay [Ref k a False, Ref k a True]) === Nothing
    , testProperty "a write after a tombstone brings the key back" $ \(Key k) a ->
        KD.lookup k (KD.replay [Ref k a False, Ref k a True, Ref k a False]) === Just a
    , testProperty "liveBytes is the sum of the live records" $ \refs ->
        let kd = KD.replay refs
         in KD.liveBytes kd === sum [fromIntegral (locSize l) | (_, l) <- KD.toList kd]
    ]
  where
    model m r
      | refTombstone r = M.delete (refKey r) m
      | otherwise = M.insert (refKey r) (refLoc r) m
    sortOn' = M.toAscList . M.fromList

instance Arbitrary Loc where
  arbitrary = Loc <$> genFileId <*> (fromIntegral <$> (arbitrary :: Gen Word)) <*> arbitrary <*> arbitrary
    where
      genFileId = mkFileId <$> arbitrary <*> arbitrary

instance Arbitrary Ref where
  arbitrary = do
    Key k <- arbitrary
    Ref (BS.copy k) <$> arbitrary <*> arbitrary
