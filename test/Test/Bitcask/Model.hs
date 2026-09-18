module Test.Bitcask.Model (tests) where

import Control.Exception (try)
import Control.Monad (foldM, forM_)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Database.Bitcask
import Database.Bitcask.Raw (Raw)

import Test.Bitcask.Util

-- | One step of a generated program.
data Op
  = OpPut ByteString ByteString
  | OpDelete ByteString
  | OpMerge
  | OpReopen
  deriving stock (Show)

instance Arbitrary Op where
  arbitrary =
    frequency
      [ (10, OpPut . unKey <$> arbitrary <*> (unVal <$> arbitrary))
      , (4, OpDelete . unKey <$> arbitrary)
      , (1, pure OpMerge)
      , (2, pure OpReopen)
      ]

tests :: TestTree
tests =
  testGroup
    "Model"
    [ testProperty "a store agrees with a Map, across merges and reopens" prop_model
    , testCase "an empty value is not a deletion" $
        withStore $ \_ bc -> do
          put bc "k" ""
          get bc "k" >>= (@?= Just "")
          delete bc "k"
          get bc "k" >>= (@?= Nothing)
    , testCase "values survive a reopen" $
        withSystemTempDirectory "bitcask-reopen" $ \dir -> do
          withBitcask dir defaultOptions $ \(bc :: Raw) -> do
            put bc "a" "1"
            put bc "b" "2"
            delete bc "a"
          withBitcask dir defaultOptions $ \(bc :: Raw) -> do
            get bc "a" >>= (@?= Nothing)
            get bc "b" >>= (@?= Just "2")
    , testCase "merge reclaims the dead bytes" $
        withSystemTempDirectory "bitcask-merge" $ \dir ->
          withBitcask dir defaultOptions {maxFileSize = 512} $ \(bc :: Raw) -> do
            forM_ [1 .. 200 :: Int] $ \i -> put bc "hot" (encodeStrict i)
            before <- stats bc
            ms <- merge bc
            afterwards <- stats bc
            assertBool "merge saw some files" (mergedFiles ms > 0)
            assertBool
              ("total bytes should shrink: " <> show (before, afterwards))
              (statsTotalBytes afterwards < statsTotalBytes before)
            get bc "hot" >>= (@?= Just (encodeStrict (200 :: Int)))
    , testCase "a wrong store tag is caught at open, not at the first get" $
        withSystemTempDirectory "bitcask-tag" $ \dir -> do
          withBitcask dir defaultOptions {storeTag = Just (T.pack "users/v1")} $
            \(bc :: Raw) -> put bc "a" "1"
          r <-
            try $
              withBitcask dir defaultOptions {storeTag = Just (T.pack "users/v2")} $
                \(bc :: Raw) -> get bc "a"
          case r of
            Left (SchemaMismatch _ want found) -> do
              want @?= T.pack "users/v2"
              found @?= T.pack "users/v1"
            Left e -> assertFailure ("wrong error: " <> show e)
            Right _ -> assertFailure "expected a SchemaMismatch"
    , testCase "a second writer is refused while the first holds the store" $
        withSystemTempDirectory "bitcask-lock" $ \dir ->
          withBitcask dir defaultOptions $ \(_ :: Raw) -> do
            r <- try (open dir defaultOptions :: IO Raw)
            case r of
              Left (LockHeld _ _) -> pure ()
              Left e -> assertFailure ("wrong error: " <> show e)
              Right _ -> assertFailure "expected LockHeld"
    ]

-- | Run a generated program against a real store and against a 'Data.Map', and
-- require them to agree after every single step.
--
-- @OpReopen@ is what makes this a recovery test as well: the store is closed and
-- reopened mid-program, so every reopen has to rebuild a keydir that still
-- matches the model.
prop_model :: [Op] -> Property
prop_model ops = ioProperty $
  withSystemTempDirectory "bitcask-model" $ \dir -> do
    bc0 <- open dir smallFiles
    (bc, model, bad) <- foldM (step dir) (bc0, M.empty, Nothing) ops
    final <- checkAll bc model
    close bc
    pure $ case bad of
      Just err -> counterexample err False
      Nothing -> case final of
        Just err -> counterexample err False
        Nothing -> property True
  where
    -- Small files so that a short program still rolls the active file and gives
    -- merge something to do.
    smallFiles = defaultOptions {maxFileSize = 256}

    step _ acc@(_, _, Just _) _ = pure acc
    step dir (bc, model, Nothing) op = case op of
      OpPut k v -> do
        put bc k v
        let model' = M.insert k v model
        (,,) bc model' <$> checkAll bc model'
      OpDelete k -> do
        delete bc k
        let model' = M.delete k model
        (,,) bc model' <$> checkAll bc model'
      OpMerge -> do
        _ <- merge bc
        (,,) bc model <$> checkAll bc model
      OpReopen -> do
        close bc
        bc' <- open dir smallFiles
        (,,) bc' model <$> checkAll bc' model

    checkAll :: Raw -> M.Map ByteString ByteString -> IO (Maybe String)
    checkAll bc model = do
      ks <- keys bc
      let got = M.fromList (zip ks (repeat ()))
          want = M.map (const ()) model
      if M.keys got /= M.keys want
        then
          pure . Just $
            "keys differ: store " <> show (M.keys got) <> " model " <> show (M.keys want)
        else go (M.toList model)
      where
        go [] = pure Nothing
        go ((k, v) : rest) = do
          mv <- get bc k
          if mv == Just v
            then go rest
            else pure . Just $ "value for " <> show k <> ": store " <> show mv <> " model " <> show (Just v)
