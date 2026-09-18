module Test.Bitcask.Concurrency (tests) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM, unless, void)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Database.Bitcask
import Database.Bitcask.Raw (Raw)

-- | Readers, a writer and a merge all at once.
--
-- Every value is tagged with the key that owns it, so a read that returned the
-- wrong record — a stale location after a merge moved it, a torn read — shows up
-- as a value belonging to a different key rather than as a silent pass. This is
-- the test that would catch a broken positional read on a platform where
-- concurrent reads share a file pointer.
tests :: TestTree
tests =
  testGroup
    "Concurrency"
    [ testCase "reads stay consistent under a concurrent writer and merge" $
        withSystemTempDirectory "bitcask-conc" $ \dir ->
          withBitcask dir defaultOptions {maxFileSize = 4096} $ \(bc :: Raw) -> do
            forM_ ks $ \k -> put bc k (tagged k 0)
            errs <- newIORef []
            stop <- newIORef False
            reads' <- newIORef (0 :: Int)
            merges <- newIORef (0 :: Int)
            done <- replicateM (readers + 2) newEmptyMVar

            forM_ [1 .. readers] $ \i ->
              forkFinally' (done !! (i - 1)) errs $ readerLoop bc stop reads'

            forkFinally' (done !! readers) errs $ writerLoop bc
            forkFinally' (done !! (readers + 1)) errs $ mergeLoop bc stop merges

            -- The writer finishes first, then everyone else winds down.
            takeMVar (done !! readers)
            writeIORef stop True
            forM_ [0 .. readers - 1] $ \i -> takeMVar (done !! i)
            takeMVar (done !! (readers + 1))

            problems <- readIORef errs
            unless (null problems) $ assertFailure (unlines (take 5 problems))

            -- A test that raced nothing proves nothing, so make it say so.
            nReads <- readIORef reads'
            nMerges <- readIORef merges
            assertBool "readers did no work" (nReads > 100)
            assertBool "no merge ran alongside the readers" (nMerges > 0)

            forM_ ks $ \k ->
              get bc k >>= (@?= Just (tagged k rounds))
    ]
  where
    readers = 4 :: Int
    rounds = 200 :: Int
    ks = [BC.pack ("key" <> show i) | i <- [0 .. 7 :: Int]]

    tagged k n = k <> BC.pack ("-" <> show (n :: Int))

    readerLoop bc stop counter = go
      where
        go = do
          halt <- readIORef stop
          unless halt $ do
            forM_ ks $ \k -> do
              mv <- get bc k
              atomicModifyIORef' counter (\n -> (n + 1, ()))
              case mv of
                Nothing -> fail ("key vanished: " <> show k)
                Just v ->
                  unless ((k <> "-") `BC.isPrefixOf` v) $
                    fail ("value " <> show v <> " does not belong to key " <> show k)
            yield
            go

    writerLoop bc =
      forM_ [1 .. rounds] $ \n ->
        forM_ ks $ \k -> put bc k (tagged k n)

    mergeLoop bc stop counter = go
      where
        go = do
          halt <- readIORef stop
          unless halt $ do
            ms <- merge bc
            unless (mergedFiles ms == 0) $
              atomicModifyIORef' counter (\n -> (n + 1, ()))
            threadDelay 1000
            go

-- | Fork an action, record anything it throws, and signal completion either way.
forkFinally' :: MVar () -> IORef [String] -> IO () -> IO ()
forkFinally' done errs act = void . forkIO $ do
  r <- try act
  case r of
    Left (e :: SomeException) -> atomicModifyIORef' errs (\es -> (show e : es, ()))
    Right () -> pure ()
  putMVar done ()
