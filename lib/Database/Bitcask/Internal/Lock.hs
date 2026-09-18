-- | The store lock.
--
-- The paper's requirement is that only one operating system /process/ opens a
-- store for writing. The platform layer provides exactly that, and only that:
-- POSIX advisory locks are owned by the process, so a second 'takeLock' from
-- within the same process succeeds. In Haskell that is the easy mistake to make
-- — two handles on one directory in one program, both convinced they are the
-- writer, both allocating the same data file ids — so there is a process-local
-- registry in front of the OS lock as well.
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

-- | Store directories this process currently holds for writing.
openStores :: MVar (Set FilePath)
openStores = unsafePerformIO (newMVar Set.empty)
{-# NOINLINE openStores #-}

data StoreLock = StoreLock
  { slPath :: !(Maybe FilePath)
  -- ^ the registry entry to give back, for an exclusive lock
  , slHandle :: !LockHandle
  }

-- | Take the lock on a store directory, or report who holds it.
--
-- 'Left' carries the holder's pid where the platform can tell us; a conflict
-- with another handle in this same process has no pid to report.
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
