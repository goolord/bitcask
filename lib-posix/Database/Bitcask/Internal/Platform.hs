{-# LANGUAGE CApiFFI #-}

-- | POSIX implementation of the platform layer.
--
-- @lib-windows@ has the same interface; cabal picks one with @os(windows)@.
-- That's why this is in @other-modules@. See @DESIGN.md@ §2.
--
-- Reads call @pread(2)@ through the FFI instead of @unix@'s @fdPread@, which
-- needs @unix-2.8@. @pread@ takes no lock and doesn't move the file pointer, so
-- threads can share a descriptor, and 'Database.Bitcask.get' can be lock-free.
module Database.Bitcask.Internal.Platform
  ( platformName
  , ReadHandle
  , openRead
  , preadAt
  , closeRead
  , AppendHandle
  , openAppend
  , appendBytes
  , syncFile
  , closeAppend
  , truncateAppend
  , syncDir
  , truncateAt
  , LockMode (..)
  , LockHandle
  , takeLock
  , dropLock
  , removeOpen
  ) where

import Control.Exception (IOException, finally, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Error (eINTR, getErrno, throwErrno, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.Directory (removeFile)
import System.IO (IOMode (..), SeekMode (..), openBinaryFile)
import System.Posix.Files (setFdSize, setFileSize)
import System.Posix.IO
  ( FileLock
  , LockRequest (..)
  , closeFd
  , fdSeek
  , fdWriteBuf
  , getLock
  , handleToFd
  , setLock
  )
import System.Posix.Types (COff (..), CSsize (..), Fd (..), ProcessID)
import System.Posix.Unistd (fileSynchronise)

platformName :: String
platformName = "posix"

foreign import capi unsafe "unistd.h pread"
  c_pread :: CInt -> Ptr Word8 -> CSize -> COff -> IO CSsize

foreign import capi unsafe "fcntl.h open"
  c_open :: CString -> CInt -> IO CInt

foreign import capi unsafe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import capi unsafe "unistd.h fsync"
  c_fsync :: CInt -> IO CInt

-- | A data file opened for reading. Safe to share across threads.
newtype ReadHandle = ReadHandle Fd

openRead :: FilePath -> IO ReadHandle
openRead p = ReadHandle <$> (handleToFd =<< openBinaryFile p ReadMode)

-- | Read @n@ bytes at an offset. Returns fewer at end of file.
preadAt :: ReadHandle -> Word64 -> Int -> IO ByteString
preadAt (ReadHandle (Fd fd)) off n
  | n <= 0 = pure BS.empty
  | otherwise = BSI.createAndTrim n (\p -> go p 0)
  where
    go :: Ptr Word8 -> Int -> IO Int
    go p done
      | done >= n = pure done
      | otherwise = do
          r <-
            c_pread
              fd
              (p `plusPtr` done)
              (fromIntegral (n - done))
              (fromIntegral (off + fromIntegral done))
          case compare r 0 of
            GT -> go p (done + fromIntegral r)
            EQ -> pure done -- end of file
            LT -> do
              errno <- getErrno
              if errno == eINTR then go p done else throwErrno "bitcask: pread"

closeRead :: ReadHandle -> IO ()
closeRead (ReadHandle fd) = closeFd fd

-- | A data file opened for appending. All writes are serialised by the caller.
newtype AppendHandle = AppendHandle Fd

-- | Open for appending, returning the handle and the current size.
openAppend :: FilePath -> IO (AppendHandle, Word64)
openAppend p = do
  fd <- handleToFd =<< openBinaryFile p ReadWriteMode
  sz <- fdSeek fd SeekFromEnd 0
  pure (AppendHandle fd, fromIntegral sz)

appendBytes :: AppendHandle -> ByteString -> IO ()
appendBytes (AppendHandle fd) bs =
  BSU.unsafeUseAsCStringLen bs $ \(p, len) -> go (castPtr p) len
  where
    go _ 0 = pure ()
    go p len = do
      n <- fdWriteBuf fd p (fromIntegral len)
      go (p `plusPtr` fromIntegral n) (len - fromIntegral n)

-- | @fsync@ the file.
syncFile :: AppendHandle -> IO ()
syncFile (AppendHandle fd) = fileSynchronise fd

closeAppend :: AppendHandle -> IO ()
closeAppend (AppendHandle fd) = closeFd fd

-- | Truncate to @n@ bytes and seek there, to undo a partial append. The seek
-- matters: appends go to the file position, which the partial write moved, so
-- without it the next append would leave a hole.
truncateAppend :: AppendHandle -> Word64 -> IO ()
truncateAppend (AppendHandle fd) n = do
  setFdSize fd (fromIntegral n)
  _ <- fdSeek fd AbsoluteSeek (fromIntegral n)
  pure ()

-- | @fsync@ the directory so newly created files are durable. Not needed on
-- Windows.
syncDir :: FilePath -> IO ()
syncDir dir = withCString dir $ \cs -> do
  -- O_RDONLY is 0 everywhere. Not worth hsc2hs for one constant.
  fd <- throwErrnoIfMinus1 "bitcask: open directory" (c_open cs 0)
  throwErrnoIfMinus1_ "bitcask: fsync directory" (c_fsync fd)
    `finally` c_close fd

truncateAt :: FilePath -> Word64 -> IO ()
truncateAt p n = setFileSize p (fromIntegral n)

data LockMode = LockExclusive | LockShared
  deriving stock (Eq, Show)

newtype LockHandle = LockHandle Fd

-- | Take the store lock, or report who holds it.
--
-- Uses an advisory @fcntl@ lock instead of an @O_EXCL@ create, since the kernel
-- drops it when the process dies and a crashed writer doesn't leave a stale
-- lock behind.
takeLock :: FilePath -> LockMode -> IO (Either (Maybe Word32) LockHandle)
takeLock p mode = do
  opened <- try (handleToFd =<< openBinaryFile p ReadWriteMode)
  case opened of
    Left (_ :: IOException) -> pure (Left Nothing)
    Right fd -> do
      taken <- try (setLock fd lock)
      case taken of
        Right () -> pure (Right (LockHandle fd))
        Left (_ :: IOException) -> do
          held <- try (getLock fd lock) :: IO (Either IOException (Maybe (ProcessID, FileLock)))
          closeFd fd
          pure . Left $ case held of
            Right (Just (pid, _)) -> Just (fromIntegral pid)
            _ -> Nothing
  where
    lock = (req, AbsoluteSeek, 0, 0)
    req = case mode of
      LockExclusive -> WriteLock
      LockShared -> ReadLock

dropLock :: LockHandle -> IO ()
dropLock (LockHandle fd) = closeFd fd

-- | Remove a file that readers may still have open. Always works on POSIX; the
-- inode lives until the last descriptor closes.
removeOpen :: FilePath -> IO Bool
removeOpen p = do
  r <- try (removeFile p)
  pure $ case r of
    Right () -> True
    Left (_ :: IOException) -> False
