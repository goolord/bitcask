-- | Benchmarks.
--
-- /codec/ covers the pure parts: CRC, record and hint codecs, 'encodeStrict'.
-- /store/ runs the public operations against a real store in a temp directory.
-- Nothing syncs, so it measures the page cache, not the disk.
--
-- > cabal bench bitcask-bench
-- > cabal bench bitcask-bench --benchmark-options='--csv before.csv'
-- > cabal bench bitcask-bench --benchmark-options='--baseline before.csv --fail-if-slower 10'
--
-- The last one fails if anything is more than 10% slower than the saved CSV.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Tasty (withResource)
import Test.Tasty.Bench

import Database.Bitcask
import Database.Bitcask.Internal.CRC32 (crc32)
import Database.Bitcask.Internal.Hint
import Database.Bitcask.Internal.Record
import Database.Bitcask.Raw (Raw)

main :: IO ()
main =
  defaultMain
    [ bgroup "codec" codecBenches
    , bgroup "store" storeBenches
    ]

-- ---------------------------------------------------------------------------
-- Pure codecs

codecBenches :: [Benchmark]
codecBenches =
  [ bgroup
      "crc32"
      [ bench (show n) $ nf crc32 (BS.replicate n 0x5a)
      | n <- [64, 1024, 65536]
      ]
  , bgroup
      "encodeRecord"
      [ bench (show n) $ nf (encodeRecord 1234 key16) (Just (value n))
      | n <- [100, 4096]
      ]
  , bgroup
      "decodeRecord"
      [ bench (show n) $
          nf (either (error . show) recValue . decodeRecord True) (encodeRecord 1234 key16 (Just (value n)))
      | n <- [100, 4096]
      ]
  , bench "encodeHintEntry" $ nf encodeHintEntry (hintFor 0 key16)
  , env (evaluate hintFile10k) $ \file ->
      bench "decodeHintFile/10000" $ nf (either (error . show) length . decodeHintFile) file
  , bgroup
      "encodeStrict"
      [ bench "ByteString/100" $ nf encodeStrict (value 100)
      , bench "Text/32" $ nf encodeStrict (T.replicate 32 (T.singleton 'k'))
      , bench "Int" $ nf encodeStrict (42 :: Int)
      ]
  ]

key16 :: ByteString
key16 = BC.pack "user:00000000042"

value :: Int -> ByteString
value n = BS.replicate n 0x61

hintFor :: Int -> ByteString -> HintEntry
hintFor i k =
  HintEntry
    { hintTstamp = 1234
    , hintTombstone = False
    , hintKey = k
    , hintValSize = 100
    , hintPos = fromIntegral i * 135
    , hintRecSize = 135
    }

hintFile10k :: ByteString
hintFile10k = body <> encodeHintTrailer 10000 (crc32 body)
  where
    body = BS.concat [encodeHintEntry (hintFor i (keyOf i)) | i <- [0 .. 9999]]

-- ---------------------------------------------------------------------------
-- A real store

-- | Keys in the populated stores.
population :: Int
population = 100000

keyOf :: Int -> ByteString
keyOf i = BC.pack ("key:" <> pad (show i))
  where
    pad s = replicate (10 - length s) '0' <> s

storeBenches :: [Benchmark]
storeBenches =
  [ withStore "write" (\_ -> pure ()) $ \getEnv ->
      bgroup
        "write"
        [ bench "put/100" $ counted getEnv $ \bc i -> put bc (keyOf (i `mod` population)) (value 100)
        , bench "put/4096" $ counted getEnv $ \bc i -> put bc (keyOf (i `mod` population)) (value 4096)
        , bench "delete" $ counted getEnv $ \bc i -> delete bc (keyOf (i `mod` population))
        , bench "put/Text Int" $ counted getEnv $ \bc i ->
            put (retype bc) (T.pack (show (i `mod` population))) i
        ]
  , withStore "read" populate $ \getEnv ->
      bgroup
        "read"
        [ bench "get/hit" $ counted getEnv $ \bc i ->
            void . evaluate =<< get bc (keyOf ((i * 7919) `mod` population))
        , bench "get/miss" $ counted getEnv $ \bc i ->
            void . evaluate =<< get bc (keyOf (population + i))
        , bench "member" $ counted getEnv $ \bc i ->
            void . evaluate =<< member bc (keyOf ((i * 7919) `mod` population))
        , bench "fold/100000" $ whnfIO $ do
            Env _ bc _ <- getEnv
            fold bc (\n _ v -> pure $! n + BS.length v) 0
        , bench "keys/100000" $ nfIO $ do
            Env _ bc _ <- getEnv
            keys bc
        ]
  , withDir "hints" (populateDir False) $ \getDir ->
      bench "open/100000 from hints" $ whnfIO $ do
        dir <- getDir
        withBitcask dir defaultOptions {readOnly = True} $ \(bc :: Raw) -> stats bc
  , withDir "scan" (populateDir True) $ \getDir ->
      bench "open/100000 from data" $ whnfIO $ do
        dir <- getDir
        withBitcask dir defaultOptions {readOnly = True} $ \(bc :: Raw) -> stats bc
  , withStore "merge" (\_ -> pure ()) $ \getEnv ->
      bench "merge/overwrite 10000 then merge" $ whnfIO $ do
        Env _ bc _ <- getEnv
        forM_ [0 .. 9999] $ \i -> put bc (keyOf i) (value 100)
        merge bc
  ]

-- | The same store at other types. The types are phantom, so this is free.
retype :: Raw -> Bitcask Text Int
retype = coerce

populate :: Raw -> IO ()
populate bc = forM_ [0 .. population - 1] $ \i -> put bc (keyOf i) (value 100)

-- | Fill a store and close it. Optionally delete the hint files so the next
-- open has to scan.
populateDir :: Bool -> FilePath -> IO ()
populateDir dropHints dir = do
  withBitcask dir defaultOptions populate
  -- Merge away the per-open active files so every open reads the same files.
  void (mergeDirectory dir)
  when dropHints $ do
    names <- listDirectory dir
    forM_ [n | n <- names, ".hint" `isSuffixOf` n] $ \n -> removeFile (dir </> n)

-- | An open store and a counter so each iteration uses a different key.
data Env = Env !FilePath !Raw !(IORef Int)

counted :: IO Env -> (Raw -> Int -> IO ()) -> Benchmarkable
counted getEnv f = whnfIO $ do
  Env _ bc ctr <- getEnv
  i <- atomicModifyIORef' ctr (\n -> (n + 1, n))
  f bc i

withStore :: String -> (Raw -> IO ()) -> (IO Env -> Benchmark) -> Benchmark
withStore name setup = withResource acquire release
  where
    acquire = do
      dir <- tempDir name
      bc <- open dir defaultOptions
      setup bc
      Env dir bc <$> newIORef 0
    release (Env dir bc _) = close bc >> removeDirectoryRecursive dir

withDir :: String -> (FilePath -> IO ()) -> (IO FilePath -> Benchmark) -> Benchmark
withDir name setup = withResource acquire removeDirectoryRecursive
  where
    acquire = do
      dir <- tempDir name
      setup dir
      pure dir

tempDir :: String -> IO FilePath
tempDir name = do
  tmp <- getTemporaryDirectory
  createTempDirectory tmp ("bitcask-bench-" <> name)
