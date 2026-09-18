-- | The store lock.
--
-- The OS lock only stops other processes. POSIX locks are per process, so a
-- second 'takeLock' in the same process succeeds, and two handles in one
-- program would both think they're the writer. So there's also a
-- process-local registry in front of it.
module Database.Bitcask.Internal.Lock
  ( StoreLock
  , LockMode (..)
  , acquireLock
  , releaseLock
  ) where

import Control.Concurrent.MVar
import Control.Exception (onException)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import System.Directory (canonicalizePath)
import System.IO.Unsafe (unsafePerformIO)

import Database.Bitcask.Internal.Platform (LockHandle, LockMode (..), dropLock, takeLock)

-- | Store directories this process has open for writing.
openStores :: MVar (Set FilePath)
openStores = unsafePerformIO (newMVar Set.empty)
{-# NOINLINE openStores #-}

data StoreLock = StoreLock
  { slPath :: !(Maybe FilePath)
  -- ^ registry entry to release, for an exclusive lock
  , slHandle :: !LockHandle
  }

-- | Take the lock on a store directory, or report who holds it.
--
-- 'Left' has the holder's pid if the platform knows it. There's no pid when
-- the conflict is within this process.
acquireLock :: FilePath -> LockMode -> IO (Either (Maybe Word32) StoreLock)
acquireLock dir mode = case mode of
  LockShared -> fmap (StoreLock Nothing) <$> takeLock (lockFile dir) mode
  LockExclusive -> do
    canon <- canonicalizePath dir
    claimed <- modifyMVar openStores $ \s ->
      if Set.member canon s
        then pure (s, False)
        else pure (Set.insert canon s, True)
    if not claimed
      then pure (Left Nothing)
      else
        (`onException` unclaim canon) $ do
          r <- takeLock (lockFile dir) mode
          case r of
            Left holder -> unclaim canon >> pure (Left holder)
            Right h -> pure (Right (StoreLock (Just canon) h))

releaseLock :: StoreLock -> IO ()
releaseLock sl = do
  dropLock (slHandle sl)
  maybe (pure ()) unclaim (slPath sl)

unclaim :: FilePath -> IO ()
unclaim canon = modifyMVar_ openStores (pure . Set.delete canon)

lockFile :: FilePath -> FilePath
lockFile dir = dir <> "/bitcask.lock"
