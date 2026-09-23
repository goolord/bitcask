-- | The store lock.
--
-- The OS lock only stops other processes. POSIX locks belong to the process:
-- a second lock in the same process converts the first instead of failing,
-- and closing any descriptor for the file drops them all. So each directory
-- gets at most one OS lock per process, recorded in a process-local registry.
-- Read-only handles share it; anything involving a writer is refused.
module Database.Bitcask.Internal.Lock
  ( StoreLock
  , LockMode (..)
  , acquireLock
  , releaseLock
  ) where

import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import System.Directory (canonicalizePath)
import System.IO.Unsafe (unsafePerformIO)

import Database.Bitcask.Internal.File (lockPath)
import Database.Bitcask.Internal.Platform (LockHandle, LockMode (..), dropLock, takeLock)

-- | Store directories this process has locked: the mode, how many open
-- stores share the lock, and the OS lock itself.
openStores :: MVar (Map FilePath (LockMode, Int, LockHandle))
openStores = unsafePerformIO (newMVar M.empty)
{-# NOINLINE openStores #-}

-- | A claim on the lock for a directory, by canonical path.
newtype StoreLock = StoreLock FilePath

-- | Take the lock on a store directory, or report who holds it.
--
-- 'Left' has the holder's pid if the platform knows it. There's no pid when
-- the conflict is within this process.
acquireLock :: FilePath -> LockMode -> IO (Either (Maybe Word32) StoreLock)
acquireLock dir mode = do
  canon <- canonicalizePath dir
  modifyMVar openStores $ \m -> case M.lookup canon m of
    Just (LockShared, n, h)
      | mode == LockShared ->
          pure (M.insert canon (LockShared, n + 1, h) m, Right (StoreLock canon))
    Just _ -> pure (m, Left Nothing)
    Nothing -> do
      r <- takeLock (lockPath dir) mode
      pure $ case r of
        Left holder -> (m, Left holder)
        Right h -> (M.insert canon (mode, 1, h) m, Right (StoreLock canon))

-- | Release a claim. The OS lock is dropped with the last one.
releaseLock :: StoreLock -> IO ()
releaseLock (StoreLock canon) = do
  lastOne <- modifyMVar openStores $ \m -> pure $ case M.lookup canon m of
    Just (mode, n, h)
      | n > 1 -> (M.insert canon (mode, n - 1, h) m, Nothing)
      | otherwise -> (M.delete canon m, Just h)
    Nothing -> (m, Nothing)
  mapM_ dropLock lastOne
