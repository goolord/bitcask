-- | What happens to the store when a write does not finish.
--
-- The invariant under test: the active data file holds exactly the writes that
-- succeeded, and the store's idea of where that file ends matches the file. A
-- write that is interrupted or fails must not break that, because every later
-- write records its location relative to it — and a store that got it wrong
-- would go on accepting writes and silently losing them.
module Test.Bitcask.WriteSafety (tests) where

import Control.Exception (IOException, SomeException, finally, try)
import Control.Monad (forM, forM_, unless, void)
import Data.ByteString (ByteString)
import Data.List (isInfixOf, isSuffixOf)
import System.Directory (listDirectory)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Database.Bitcask
import Database.Bitcask.Internal.Merge (mergeStore)
import qualified Database.Bitcask.Internal.Store as S
import Database.Bitcask.Raw (Raw)

tests :: TestTree
tests =
  testGroup
    "WriteSafety"
    [ testCase "an interrupted put never corrupts the ones after it" $
        withSystemTempDirectory "bitcask-interrupt" $ \dir -> do
          -- Puts under timeouts short enough to land anywhere inside them,
          -- including between the append and the bookkeeping that follows it.
          done <- withBitcask dir defaultOptions $ \(bc :: Raw) -> do
            finished <- forM [0 .. interrupted - 1] $ \i -> do
              r <- timeout (i `mod` 40) (put bc (key "t" i) (val i))
              pure (maybe Nothing (const (Just i)) r)
            -- Then plain puts, which must all land where the store says.
            forM_ [0 .. plain - 1] $ \i -> put bc (key "p" i) (val i)
            checkAll bc (catMaybes finished)
            pure (catMaybes finished)
          -- And the same again from disk.
          withBitcask dir defaultOptions $ \(bc :: Raw) -> checkAll bc done
    , testCase "an interrupted put either happened or did not" $
        withSystemTempDirectory "bitcask-interrupt-atomic" $ \dir ->
          withBitcask dir defaultOptions $ \(bc :: Raw) ->
            forM_ [0 .. interrupted - 1] $ \i -> do
              _ <- timeout (i `mod` 40) (put bc (key "t" i) (val i))
              got <- get bc (key "t" i)
              unless (got == Nothing || got == Just (val i)) $
                assertFailure ("put " <> show i <> " left " <> show got)
    , testGroup "injected failures" faultTests
    ]
  where
    interrupted = 3000
    plain = 500

    checkAll :: Raw -> [Int] -> IO ()
    checkAll bc finished = do
      forM_ finished $ \i -> do
        got <- get bc (key "t" i)
        got @?= Just (val i)
      forM_ [0 .. plain - 1] $ \i -> do
        got <- try (get bc (key "p" i))
        case got of
          Right (Just v) | v == val i -> pure ()
          other ->
            assertFailure ("plain put " <> show i <> " read back as " <> show (fmap (fmap BC.unpack) (either (\e -> Left (show (e :: BitcaskError))) Right other)))


-- | The failures a real disk only produces when it is full or failing, reached
-- through the store's fault hooks. These work below the typed API, on
-- "Database.Bitcask.Internal.Store", because that is where the hooks are.
faultTests :: [TestTree]
faultTests =
  [ testCase "a failed append is undone, and the store carries on" $
      withRawStore $ \dir st -> do
        S.putRaw st "a" "1"
        S.injectFault st S.FaultDataWrite
        failed <- try (S.putRaw st "b" "2")
        assertIOError failed
        -- Half of "b" reached the file and was cut off again, so this lands
        -- where the store thinks it does.
        S.putRaw st "c" "3"
        expect st [("a", Just "1"), ("b", Nothing), ("c", Just "3")]
        S.closeStore st
        reopened dir $ \st' -> expect st' [("a", Just "1"), ("b", Nothing), ("c", Just "3")]
  , testCase "an append that cannot be undone breaks the store until reopened" $
      withRawStore $ \dir st -> do
        S.putRaw st "a" "1"
        S.injectFault st S.FaultDataWrite
        S.injectFault st S.FaultUndo
        assertIOError =<< try (S.putRaw st "b" "2")
        assertBroken =<< try (S.putRaw st "c" "3")
        assertBroken =<< try (S.deleteRaw st "a")
        assertBroken =<< try (S.syncStore st)
        assertBroken =<< try (void (mergeStore st))
        -- Reads are unaffected.
        expect st [("a", Just "1"), ("b", Nothing)]
        S.closeStore st
        -- Reopening scans the file, cuts off the half record and starts over.
        reopened dir $ \st' -> do
          expect st' [("a", Just "1"), ("b", Nothing), ("c", Nothing)]
          S.putRaw st' "d" "4"
          expect st' [("d", Just "4")]
  , testCase "a failed fsync breaks the store; the write it followed stands" $
      withSystemTempDirectory "bitcask-faults" $ \dir -> do
        st <- S.openStore dir defaultOptions {syncPolicy = SyncOnPut}
        S.putRaw st "a" "1"
        S.injectFault st S.FaultSync
        assertIOError =<< try (S.putRaw st "b" "2")
        assertBroken =<< try (S.putRaw st "c" "3")
        -- The record was written and indexed before the sync; whether it is
        -- durable is what is unknown.
        expect st [("a", Just "1"), ("b", Just "2")]
        S.closeStore st
        reopened dir $ \st' -> do
          expect st' [("a", Just "1"), ("b", Just "2"), ("c", Nothing)]
          S.putRaw st' "c" "3"
          expect st' [("c", Just "3")]
  , testCase "a failed explicit sync breaks the store" $
      withRawStore $ \_ st -> do
        S.putRaw st "a" "1"
        S.injectFault st S.FaultSync
        assertIOError =<< try (S.syncStore st)
        assertBroken =<< try (S.putRaw st "b" "2")
  , testCase "a failed hint write costs the hint, never a write" $
      withRawStore $ \dir st -> do
        S.injectFault st S.FaultHintWrite
        -- Enough entries to fill the hint buffer, so that it is flushed, and
        -- the flush fails, part way through.
        forM_ [0 .. hintFillers - 1] $ \i -> S.putRaw st (key "h" i) (val i)
        S.closeStore st
        -- The store's only file lost its hint, so there is no hint at all.
        hints <- filter (".hint" `isSuffixOf`) <$> listDirectory dir
        hints @?= []
        reopened dir $ \st' -> forM_ [0 .. hintFillers - 1] $ \i ->
          S.getRaw st' (key "h" i) >>= (@?= Just (val i))
  , -- The first sync a merge does finishes the active file it rolls away from.
    testCase "a merge that cannot finish the active file does nothing" $
      mergeFailure (pure . snd) S.FaultSync
  , -- On a freshly opened store the active file is empty, so merge does not roll
    -- it, and the first sync is the one that finishes the merge output.
    testCase "a merge whose output cannot be synced removes nothing" $
      mergeFailure reopenFirst S.FaultSync
  , -- Likewise, the first data write is the merge's first block of output.
    testCase "a merge whose output write fails removes nothing" $
      mergeFailure reopenFirst S.FaultDataWrite
  ]
  where
    -- Overwrite 100 keys, so that a merge has inputs; optionally reopen; arm a
    -- fault; merge. The merge must fail and leave every value where it was,
    -- both now and after a reopen.
    mergeFailure prepare f = withSystemTempDirectory "bitcask-faults" $ \dir -> do
      st0 <- S.openStore dir defaultOptions
      forM_ [0 .. 199 :: Int] $ \i -> S.putRaw st0 (key "m" (i `mod` 100)) (val i)
      st <- prepare (dir, st0)
      flip finally (S.closeStore st) $ do
        S.injectFault st f
        failed <- try (void (mergeStore st))
        case failed of
          Left (e :: SomeException) ->
            unless ("injected fault" `isInfixOf` show e) $
              assertFailure ("merge failed, but not from the fault: " <> show e)
          Right () -> assertFailure "merge succeeded despite the fault"
        latest st
      reopened dir latest

    reopenFirst (dir, st) = S.closeStore st >> S.openStore dir defaultOptions

    latest st = forM_ [0 .. 99 :: Int] $ \i ->
      S.getRaw st (key "m" i) >>= (@?= Just (val (i + 100)))

    -- Hint entries here are 23 bytes plus the key; this is comfortably more
    -- than one 64 KiB buffer's worth.
    hintFillers = 5000 :: Int

    withRawStore act = withSystemTempDirectory "bitcask-faults" $ \dir -> do
      st <- S.openStore dir defaultOptions
      act dir st `finally` S.closeStore st

    reopened dir act = do
      st <- S.openStore dir defaultOptions
      act st `finally` S.closeStore st

    expect st kvs = forM_ kvs $ \(k, v) -> do
      got <- S.getRaw st k
      assertEqual ("value of " <> show k) v got

    assertIOError :: Either IOException () -> Assertion
    assertIOError = either (const (pure ())) (const (assertFailure "expected the write to fail"))

    assertBroken :: Either BitcaskError () -> Assertion
    assertBroken = \case
      Left (StoreBroken _) -> pure ()
      other -> assertFailure ("expected StoreBroken, got " <> show other)

key :: String -> Int -> ByteString
key p i = BC.pack (p <> show i)

val :: Int -> ByteString
val i = BC.pack ("value-" <> show i)
