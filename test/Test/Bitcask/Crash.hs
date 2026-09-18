module Test.Bitcask.Crash (tests) where

import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath ((</>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Database.Bitcask
import Database.Bitcask.Internal.Record (headerSize)
import Database.Bitcask.Raw (Raw)

-- | A process killed mid-append leaves a partial record at the end of the
-- active file. Truncate a real store at every byte offset and check that
-- reopening recovers exactly the records that fit in the prefix.
--
-- The hint file is removed too, since a crash would have left it without a
-- trailer.
tests :: TestTree
tests =
  testGroup
    "Crash"
    [ testCase "recovery from a truncation at every offset" $
        withSystemTempDirectory "bitcask-crash" $ \dir -> do
          withBitcask dir defaultOptions $ \(bc :: Raw) ->
            forM_ writes $ \w -> case w of
              (k, Just v) -> put bc k v
              (k, Nothing) -> delete bc k
          dataFile <- soleDataFile dir
          size <- fromIntegral <$> getFileSize (dir </> dataFile)
          forM_ [0 .. size] $ \cut ->
            withSystemTempDirectory "bitcask-crash-copy" $ \copy -> do
              copyStore dir copy
              truncateFile (copy </> dataFile) cut
              removeHints copy
              got <- withBitcask copy defaultOptions $ \(bc :: Raw) -> do
                ks <- keys bc
                M.fromList <$> forM ks (\k -> (,) k <$> (maybe "" id <$> get bc k))
              let want = modelAt cut
              unless (got == want) $
                assertFailure $
                  "truncated at " <> show cut <> ": store " <> show (M.toList got)
                    <> " expected "
                    <> show (M.toList want)
    ]

-- | A short program with overwrites and a delete.
writes :: [(ByteString, Maybe ByteString)]
writes =
  [ ("a", Just "1")
  , ("b", Just "22")
  , ("a", Just "333")
  , ("c", Just "")
  , ("b", Nothing)
  , ("d", Just "4444")
  , ("a", Nothing)
  , ("e", Just "5")
  , ("a", Just "6")
  ]

-- | Expected contents after truncating to @n@ bytes: the writes that fit whole.
modelAt :: Int -> M.Map ByteString ByteString
modelAt n = snd (foldl step (0 :: Int, M.empty) writes)
  where
    step (off, m) (k, mv) =
      let len = headerSize + BS.length k + maybe 0 BS.length mv
          off' = off + len
       in if off' > n
            then (off', m)
            else (off', maybe (M.delete k m) (\v -> M.insert k v m) mv)

soleDataFile :: FilePath -> IO FilePath
soleDataFile dir = do
  es <- listDirectory dir
  case filter (".data" `isSuffixOf`) es of
    [f] -> pure f
    fs -> assertFailure ("expected one data file, found " <> show fs)

copyStore :: FilePath -> FilePath -> IO ()
copyStore from to = do
  es <- listDirectory from
  forM_ es $ \e -> copyFile (from </> e) (to </> e)

truncateFile :: FilePath -> Int -> IO ()
truncateFile p n = withBinaryFile p ReadWriteMode $ \h -> hSetFileSize h (fromIntegral n)

removeHints :: FilePath -> IO ()
removeHints dir = do
  es <- listDirectory dir
  forM_ (filter (".hint" `isSuffixOf`) es) $ \e -> removeFile (dir </> e)
