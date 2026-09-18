-- | Windows implementation of the platform layer.
--
-- Same interface as the POSIX module under @lib-posix@; cabal picks one by
-- @os(windows)@. See @DESIGN.md@ §2.
--
-- Three notes on how this differs, because the differences are the whole reason
-- this module exists:
--
-- * __Positional reads__ use a small pool of handles guarded by an 'MVar', which
--   is the fallback @DESIGN.md@ §2.1 describes rather than its first choice.
--   Windows has no @pread@; @ReadFile@ takes an offset through an @OVERLAPPED@
--   structure, but on a handle opened without @FILE_FLAG_OVERLAPPED@ it also
--   moves the shared file pointer, so concurrent positional reads on one handle
--   race. Doing it properly means overlapped handles and a per-call @OVERLAPPED@
--   with its own event, which is worth doing and is not worth doing blind — the
--   pool is correct, portable and costs only some contention under many
--   concurrent readers.
--
-- * __Deleting a merged-away file__ that a reader still holds open fails here,
--   where POSIX allows it. 'removeOpen' therefore reports 'False' instead of
--   throwing, and the caller records the file in @bitcask.pending@ and sweeps it
--   at the next open. (Opening every read handle with @FILE_SHARE_DELETE@ would
--   give POSIX semantics and is the natural follow-up to the overlapped work.)
--
-- * __There is no directory sync__, and none is needed: @FlushFileBuffers@ on the
--   file handle is the durability primitive, so 'syncDir' is a no-op.
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
  , syncDir
  , truncateAt
  , LockMode (..)
  , LockHandle
  , takeLock
  , dropLock
  , removeOpen
  ) where

import Control.Concurrent.MVar
import Control.Exception (IOException, bracket, try)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)
import System.Directory (removeFile)
import System.IO
import System.Win32.File
  ( closeHandle
  , createFile
  , fILE_ATTRIBUTE_NORMAL
  , flushFileBuffers
  , gENERIC_READ
  , gENERIC_WRITE
  , oPEN_ALWAYS
  )
import System.Win32.Types (HANDLE, withHandleToHANDLE)

platformName :: String
platformName = "windows"

-- | How many read handles to keep open per data file.
poolMax :: Int
poolMax = 8

-- | A data file opened for reading: a path plus a pool of handles. Safe to share
-- across threads.
data ReadHandle = ReadHandle !FilePath !(MVar [Handle])

openRead :: FilePath -> IO ReadHandle
openRead p = do
  h <- openBinaryFile p ReadMode
  ReadHandle p <$> newMVar [h]

-- | Read @n@ bytes at an absolute offset. Returns fewer bytes at end of file.
preadAt :: ReadHandle -> Word64 -> Int -> IO ByteString
preadAt rh@(ReadHandle _ _) off n
  | n <= 0 = pure BS.empty
  | otherwise = bracket (acquire rh) (release rh) $ \h -> do
      hSeek h AbsoluteSeek (fromIntegral off)
      BS.hGet h n

acquire :: ReadHandle -> IO Handle
acquire (ReadHandle p pool) = do
  taken <- modifyMVar pool $ \hs -> case hs of
    (h : rest) -> pure (rest, Just h)
    [] -> pure ([], Nothing)
  maybe (openBinaryFile p ReadMode) pure taken

release :: ReadHandle -> Handle -> IO ()
release (ReadHandle _ pool) h = modifyMVar_ pool $ \hs ->
  if length hs >= poolMax
    then hClose h >> pure hs
    else pure (h : hs)

closeRead :: ReadHandle -> IO ()
closeRead (ReadHandle _ pool) = modifyMVar_ pool $ \hs -> mapM_ hClose hs >> pure []

-- | A data file opened for appending. All writes are serialised by the caller.
newtype AppendHandle = AppendHandle Handle

openAppend :: FilePath -> IO (AppendHandle, Word64)
openAppend p = do
  h <- openBinaryFile p ReadWriteMode
  -- NoBuffering, not block buffering: a record sitting in a Haskell-side buffer
  -- is invisible to the separate read handles that 'preadAt' uses, so a 'get' of
  -- a key that was just 'put' would miss it.
  hSetBuffering h NoBuffering
  hSeek h SeekFromEnd 0
  sz <- hFileSize h
  pure (AppendHandle h, fromIntegral sz)

appendBytes :: AppendHandle -> ByteString -> IO ()
appendBytes (AppendHandle h) = BS.hPut h

-- | Flush the Haskell buffer, then ask the OS to commit. @hFlush@ alone is not
-- durability.
syncFile :: AppendHandle -> IO ()
syncFile (AppendHandle h) = do
  hFlush h
  withHandleToHANDLE h flushFileBuffers

closeAppend :: AppendHandle -> IO ()
closeAppend (AppendHandle h) = hClose h

-- | No-op: see the module header.
syncDir :: FilePath -> IO ()
syncDir _ = pure ()

truncateAt :: FilePath -> Word64 -> IO ()
truncateAt p n =
  bracket (openBinaryFile p ReadWriteMode) hClose $ \h ->
    hSetFileSize h (fromIntegral n)

data LockMode = LockExclusive | LockShared
  deriving stock (Eq, Show)

newtype LockHandle = LockHandle HANDLE

-- | Take the store lock, or report that someone else holds it.
--
-- An exclusive @CreateFile@ with a share mode of zero /is/ the lock, and Windows
-- releases it when the process exits — the same self-healing property the POSIX
-- side gets from an advisory @fcntl@ lock. Windows will not tell us which
-- process holds the file, hence the 'Nothing'.
takeLock :: FilePath -> LockMode -> IO (Either (Maybe Word32) LockHandle)
takeLock p mode = do
  r <-
    try $
      createFile
        p
        access
        0 -- FILE_SHARE_NONE: the open is the lock
        Nothing
        oPEN_ALWAYS
        fILE_ATTRIBUTE_NORMAL
        Nothing
  pure $ case r of
    Right h -> Right (LockHandle h)
    Left (_ :: IOException) -> Left Nothing
  where
    access = case mode of
      LockExclusive -> gENERIC_READ .|. gENERIC_WRITE
      LockShared -> gENERIC_READ

dropLock :: LockHandle -> IO ()
dropLock (LockHandle h) = closeHandle h

-- | Remove a file that readers may still have open. Unlike POSIX, this fails on
-- Windows while any handle is open; the caller falls back to @bitcask.pending@.
removeOpen :: FilePath -> IO Bool
removeOpen p = do
  r <- try (removeFile p)
  pure $ case r of
    Right () -> True
    Left (_ :: IOException) -> False
