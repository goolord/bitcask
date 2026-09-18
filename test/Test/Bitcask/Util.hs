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

-- | Keys are drawn from a small space on purpose, so that generated programs
-- actually overwrite and delete the same key rather than only ever appending
-- fresh ones.
newtype Key = Key {unKey :: ByteString}
  deriving stock (Eq, Ord, Show)

instance Arbitrary Key where
  arbitrary = Key . BC.pack . ("k" <>) . show <$> choose (0 :: Int, 15)

newtype Val = Val {unVal :: ByteString}
  deriving stock (Eq, Ord, Show)

instance Arbitrary Val where
  arbitrary = Val <$> smallBytes

-- | Short byte strings, including the empty one — storing an empty value is
-- legal here and is the case the paper's sentinel tombstone would have broken.
smallBytes :: Gen ByteString
smallBytes = BS.pack <$> (choose (0, 24) >>= \n -> vectorOf n arbitrary)
