module Test.Bitcask.Util
  ( withStore
  , withStoreOpts
  , Key (..)
  , Val (..)
  , smallBytes
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck

import Database.Bitcask
import Database.Bitcask.Raw (Raw)

withStore :: (FilePath -> Raw -> IO a) -> IO a
withStore = withStoreOpts defaultOptions

withStoreOpts :: OpenOptions -> (FilePath -> Raw -> IO a) -> IO a
withStoreOpts opts act =
  withSystemTempDirectory "bitcask-test" $ \dir ->
    withBitcask dir opts (act dir)

-- | Small key space so generated programs overwrite and delete existing keys.
newtype Key = Key {unKey :: ByteString}
  deriving stock (Eq, Ord, Show)

instance Arbitrary Key where
  arbitrary = Key . BC.pack . ("k" <>) . show <$> choose (0 :: Int, 15)

newtype Val = Val {unVal :: ByteString}
  deriving stock (Eq, Ord, Show)

instance Arbitrary Val where
  arbitrary = Val <$> smallBytes

-- | Short byte strings, including empty. Empty values are allowed here (the
-- paper's tombstone value would break this).
smallBytes :: Gen ByteString
smallBytes = BS.pack <$> (choose (0, 24) >>= \n -> vectorOf n arbitrary)
