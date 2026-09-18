-- | Windows implementation of the platform layer.
--
-- Same interface as the POSIX module under @lib-posix@; cabal picks one by
-- @os(windows)@. See @DESIGN.md@ §2.
--
-- Three notes on how this differs, because the differences are the whole reason
-- this module exists:
--
-- * __Positional reads__ go through a raw Win32 @HANDLE@ and @ReadFile@ with
--   the offset in an @OVERLAPPED@ structure. Windows has no @pread@, but that is
--   its equivalent: each call names its own offset, so nothing depends on the
--   handle's shared file pointer. A GHC 'Handle' is not an option here — see
--   'ReadHandle'.
--
-- * __Deleting a merged-away file__ that a reader still holds open works, as on
--   POSIX, because every read handle is opened with @FILE_SHARE_DELETE@. If a
--   delete fails anyway (another process has the file open, say), 'removeOpen'
--   reports 'False' instead of throwing, and the caller records the file in
--   @bitcask.pending@ and sweeps it at the next open. That fallback is slow —
--   @removeFile@ retries a sharing violation for about two seconds before giving
--   up — which is why it must not be the common path.
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

import Control.Exception (IOException, bracket, try)
import Data.Bits (shiftR, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (peek, pokeByteOff, sizeOf)
import System.Directory (removeFile)
import System.IO
import System.Win32.File
  ( closeHandle
  , createFile
  , fILE_ATTRIBUTE_NORMAL
  , fILE_SHARE_DELETE
  , fILE_SHARE_READ
  , fILE_SHARE_WRITE
  , flushFileBuffers
  , gENERIC_READ
  , gENERIC_WRITE
  , oPEN_ALWAYS
  , oPEN_EXISTING
  )
import System.Win32.Types (BOOL, DWORD, HANDLE, failWith, getLastError, withHandleToHANDLE)

platformName :: String
platformName = "windows"

-- | A data file opened for reading. Safe to share across threads.
--
-- This is a raw Win32 @HANDLE@, not a GHC 'Handle', on purpose. GHC enforces
-- its own single-writer/multi-reader lock per file within a process, so a
-- read-mode 'Handle' on the active file is refused ("resource busy") while its
-- append handle is open, and every 'get' of a freshly written key would fail.
data ReadHandle = ReadHandle !FilePath !HANDLE

openRead :: FilePath -> IO ReadHandle
openRead p =
  ReadHandle p
    <$> createFile
      p
      gENERIC_READ
      (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
      Nothing
      oPEN_EXISTING
      fILE_ATTRIBUTE_NORMAL
      Nothing

-- | Read @n@ bytes at an absolute offset. Returns fewer bytes at end of file.
--
-- Every @ReadFile@ carries its own offset in an @OVERLAPPED@, so no call relies
-- on the handle's shared file pointer and concurrent reads on one handle do not
-- interfere with each other.
preadAt :: ReadHandle -> Word64 -> Int -> IO ByteString
preadAt (ReadHandle _ h) off n
  | n <= 0 = pure BS.empty
  | otherwise = BSI.createUptoN n $ \buf -> go buf 0
  where
    go buf got
      | got >= n = pure got
      | otherwise = do
          r <- readChunkAt h (off + fromIntegral got) (buf `plusPtr` got) (n - got)
          if r == 0 then pure got else go buf (got + r)

-- | One @ReadFile@ at an offset. @0@ means end of file.
readChunkAt :: HANDLE -> Word64 -> Ptr Word8 -> Int -> IO Int
readChunkAt h off buf n =
  allocaBytes ovlSize $ \ovl -> alloca $ \pRead -> do
    fillBytes ovl 0 ovlSize
    pokeByteOff ovl offsetAt (fromIntegral off :: DWORD)
    pokeByteOff ovl (offsetAt + 4) (fromIntegral (off `shiftR` 32) :: DWORD)
    ok <- c_ReadFile h buf (fromIntegral (min n maxChunk)) pRead ovl
    if ok
      then fromIntegral <$> peek pRead
      else do
        err <- getLastError
        -- A synchronous read that starts at or past the end fails with
        -- ERROR_HANDLE_EOF rather than reading zero bytes.
        if err == eRROR_HANDLE_EOF then pure 0 else failWith "ReadFile" err
  where
    -- OVERLAPPED is { ULONG_PTR Internal, InternalHigh; DWORD Offset, OffsetHigh;
    -- HANDLE hEvent }. Win32 exports the type but no Storable instance.
    ptrSize = sizeOf nullPtr
    offsetAt = 2 * ptrSize
    ovlSize = 3 * ptrSize + 8
    maxChunk = 0x40000000
    eRROR_HANDLE_EOF = 38

foreign import ccall safe "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr Word8 -> DWORD -> Ptr DWORD -> Ptr () -> IO BOOL

closeRead :: ReadHandle -> IO ()
closeRead (ReadHandle _ h) = closeHandle h

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
