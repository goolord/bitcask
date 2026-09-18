-- | A fixed, end-to-end workload to profile against.
--
-- The benchmark suite answers \"how long does one operation take\"; this answers
-- \"where does the time and the memory go\" across a realistic run: a bulk load,
-- random reads from several threads, an overwrite-heavy phase, a merge, and a
-- reopen. Each phase prints its wall time and throughput.
--
-- > cabal run bitcask-workload -- [keys] [value bytes] [reader threads]
--
-- For a cost-centre profile, a heap profile, or an eventlog, see the
-- \"Profiling\" section of @README.md@.
module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, void)
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import System.Directory (getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getArgs)
import System.IO.Temp (createTempDirectory)
import Text.Printf (printf)

import Database.Bitcask
import Database.Bitcask.Raw (Raw)

main :: IO ()
main = do
  args <- getArgs
  caps <- getNumCapabilities
  let (nKeys, vSize, nReaders) = case map read args of
        [a, b, c] -> (a, b, c)
        [a, b] -> (a, b, caps)
        [a] -> (a, 100, caps)
        _ -> (200000, 100, caps)
  printf "keys=%d value=%dB readers=%d capabilities=%d\n" nKeys vSize nReaders caps
  tmp <- getTemporaryDirectory
  dir <- createTempDirectory tmp "bitcask-workload"
  let val = BS.replicate vSize 0x61
      reads' = nKeys * 2

  withBitcask dir defaultOptions $ \(bc :: Raw) -> do
    phase "load" nKeys $ forM_ [0 .. nKeys - 1] $ \i -> put bc (keyOf i) val

    phase "get (1 thread)" reads' $ forM_ [0 .. reads' - 1] $ \i ->
      void . evaluate =<< get bc (keyOf (scramble i nKeys))

    phase (printf "get (%d threads)" nReaders) reads' $ do
      let per = reads' `div` nReaders
      dones <- replicateM nReaders newEmptyMVar
      forM_ (zip [0 ..] dones) $ \(t, done) -> forkIO $ do
        forM_ [0 .. per - 1] $ \i ->
          void . evaluate =<< get bc (keyOf (scramble (t * per + i) nKeys))
        putMVar done ()
      mapM_ takeMVar dones

    phase "overwrite half" (nKeys `div` 2) $
      forM_ [0, 2 .. nKeys - 1] $ \i -> put bc (keyOf i) val

    phase "fold" nKeys $ void $ fold bc (\n _ v -> pure $! n + BS.length v) (0 :: Int)

    phase "merge" nKeys $ void (merge bc)

  reopen "reopen (hints)" dir nKeys

  -- Without hint files, open has to scan every data file.
  names <- listDirectory dir
  forM_ [n | n <- names, ".hint" `isSuffixOf` n] $ \n -> removeFile (dir </> n)
  reopen "reopen (scan)" dir nKeys

  removeDirectoryRecursive dir

-- | Open read-only, and report how much heap the open store holds on to — which
-- is, to a close approximation, the keydir.
reopen :: String -> FilePath -> Int -> IO ()
reopen name dir nKeys = do
  before <- liveBytes
  opened <- newEmptyMVar
  -- Force the stats, which forces the keydir: an open that left it as a thunk
  -- would look free here and be paid for by the first read.
  phase name nKeys $ do
    bc <- open dir defaultOptions {readOnly = True} :: IO Raw
    _ <- evaluate =<< stats bc
    putMVar opened bc
  bc <- takeMVar opened
  after <- liveBytes
  printf "%-20s %9.1f MB live  %8.0f bytes/key\n" "" (mb (after - before)) (perKey (after - before))
  close bc
  where
    mb b = fromIntegral b / (1024 * 1024) :: Double
    perKey b = fromIntegral b / fromIntegral nKeys :: Double

-- | Live heap after a major GC. Zero unless the RTS is collecting statistics,
-- which the default RTS options for this program turn on.
liveBytes :: IO Int
liveBytes = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then pure 0
    else do
      performMajorGC
      fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats

phase :: String -> Int -> IO () -> IO ()
phase name ops act = do
  t0 <- getMonotonicTimeNSec
  act
  t1 <- getMonotonicTimeNSec
  let secs = fromIntegral (t1 - t0) / 1e9 :: Double
  printf "%-20s %9.1f ms  %12.0f ops/s\n" name (secs * 1000) (fromIntegral ops / secs)

keyOf :: Int -> ByteString
keyOf i = BC.pack ("key:" <> pad (show i))
  where
    pad s = replicate (10 - length s) '0' <> s

-- | A cheap permutation-ish spread over the key space, so reads are not in
-- write order.
scramble :: Int -> Int -> Int
scramble i n = (i * 7919) `mod` n
