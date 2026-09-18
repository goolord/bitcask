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
  , truncateAppend
  , syncDir
  , truncateAt
  , LockMode (..)
  , LockHandle
  , takeLock
  , dropLock
  , removeOpen
  ) where

import Control.Concurrent (getNumCapabilities, myThreadId, threadCapability)
import Control.Exception (IOException, bracket, onException, try)
import Control.Monad (unless)
import Data.Bits (shiftR, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int64)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromListN)
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, pokeByteOff, sizeOf)
import System.Directory (removeFile)
import System.IO (IOMode (..), hClose, hSetFileSize, openBinaryFile)
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
import System.Win32.Types (BOOL, DWORD, HANDLE, failWith, getLastError)

platformName :: String
platformName = "windows"

-- | A data file opened for reading. Safe to share across threads.
--
-- This is raw Win32 @HANDLE@s, not a GHC 'Handle', on purpose. GHC enforces
-- its own single-writer/multi-reader lock per file within a process, so a
-- read-mode 'Handle' on the active file is refused ("resource busy") while its
-- append handle is open, and every 'get' of a freshly written key would fail.
--
-- And it is several handles, not one. Windows serialises I\/O on a handle that
-- was opened for synchronous access: two threads reading through the same
-- handle at once take turns, however independent their offsets. With one handle
-- per file, adding reader threads made reads /slower/. So each file gets a small
-- pool, and a read uses the one belonging to the capability it runs on; since a
-- capability runs one Haskell thread at a time and these reads are @unsafe@
-- calls, two reads on the same capability never overlap.
data ReadHandle = ReadHandle !FilePath !(SmallArray HANDLE)

openRead :: FilePath -> IO ReadHandle
openRead p = do
  caps <- getNumCapabilities
  let n = max 1 (min maxReadHandles caps)
  hs <- go n []
  pure (ReadHandle p (smallArrayFromListN n hs))
  where
    go :: Int -> [HANDLE] -> IO [HANDLE]
    go 0 acc = pure acc
    go k acc = do
      h <- onException' acc $
        createFile
          p
          gENERIC_READ
          (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
          Nothing
          oPEN_EXISTING
          fILE_ATTRIBUTE_NORMAL
          Nothing
      go (k - 1) (h : acc)
    -- Do not leak the handles already opened if a later one fails.
    onException' acc act = act `onException` mapM_ closeHandle acc

-- | The most handles one file is opened with for reading.
maxReadHandles :: Int
maxReadHandles = 16

-- | The handle for the capability the calling thread is on.
pickHandle :: SmallArray HANDLE -> IO HANDLE
pickHandle hs = do
  (cap, _) <- threadCapability =<< myThreadId
  pure $! indexSmallArray hs (cap `rem` sizeofSmallArray hs)
{-# INLINE pickHandle #-}

-- | Read @n@ bytes at an absolute offset. Returns fewer bytes at end of file.
--
-- Every @ReadFile@ carries its own offset in an @OVERLAPPED@, so no call relies
-- on the handle's shared file pointer and concurrent reads on one handle do not
-- interfere with each other.
preadAt :: ReadHandle -> Word64 -> Int -> IO ByteString
preadAt (ReadHandle _ hs) off n
  | n <= 0 = pure BS.empty
  | otherwise = do
      h <- pickHandle hs
      BSI.createUptoN n $ \buf -> go h buf 0
  where
    go h buf got
      | got >= n = pure got
      | otherwise = do
          r <- readChunkAt h (off + fromIntegral got) (buf `plusPtr` got) (n - got)
          if r == 0 then pure got else go h buf (got + r)

-- | One @ReadFile@ at an offset. @0@ means end of file.
readChunkAt :: HANDLE -> Word64 -> Ptr Word8 -> Int -> IO Int
readChunkAt h off buf n =
  withOverlapped off $ \ovl pDone -> do
    ok <- c_ReadFile h buf (fromIntegral (min n maxChunk)) pDone ovl
    if ok
      then fromIntegral <$> peek pDone
      else do
        err <- getLastError
        -- A synchronous read that starts at or past the end fails with
        -- ERROR_HANDLE_EOF rather than reading zero bytes.
        if err == eRROR_HANDLE_EOF then pure 0 else failWith "ReadFile" err
  where
    eRROR_HANDLE_EOF = 38

-- | The largest single @ReadFile@ or @WriteFile@ we issue; the length is a
-- 'DWORD', and callers loop.
maxChunk :: Int
maxChunk = 0x40000000

-- | An @OVERLAPPED@ naming a file offset, and a 'DWORD' for the byte count, in
-- one allocation.
--
-- @OVERLAPPED@ is @{ ULONG_PTR Internal, InternalHigh; DWORD Offset, OffsetHigh;
-- HANDLE hEvent }@. Win32 exports the type but no 'Storable' instance.
withOverlapped :: Word64 -> (Ptr () -> Ptr DWORD -> IO a) -> IO a
withOverlapped off act =
  allocaBytes (ovlSize + 8) $ \ovl -> do
    fillBytes ovl 0 ovlSize
    pokeByteOff ovl offsetAt (fromIntegral off :: DWORD)
    pokeByteOff ovl (offsetAt + 4) (fromIntegral (off `shiftR` 32) :: DWORD)
    act ovl (ovl `plusPtr` ovlSize)
  where
    ptrSize = sizeOf nullPtr
    offsetAt = 2 * ptrSize
    ovlSize = 3 * ptrSize + 8

-- These are @unsafe@ calls. A @safe@ call releases the capability so other
-- Haskell threads can run while it blocks, which is right for a call that may
-- block for a long time, and costs a few hundred nanoseconds per call for the
-- hand-off. A positional read or an append of a record is almost always served
-- by the page cache in a microsecond or two, so the hand-off would dominate.
-- The POSIX layer makes the same choice for @pread@.
foreign import ccall unsafe "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr Word8 -> DWORD -> Ptr DWORD -> Ptr () -> IO BOOL

foreign import ccall unsafe "windows.h WriteFile"
  c_WriteFile :: HANDLE -> Ptr Word8 -> DWORD -> Ptr DWORD -> Ptr () -> IO BOOL

foreign import ccall unsafe "windows.h GetFileSizeEx"
  c_GetFileSizeEx :: HANDLE -> Ptr Int64 -> IO BOOL

closeRead :: ReadHandle -> IO ()
closeRead (ReadHandle _ hs) = mapM_ closeHandle hs

-- | A data file opened for appending. All writes are serialised by the caller.
--
-- A raw @HANDLE@ for the same reason as 'ReadHandle', and one more: a write
-- through a GHC 'Handle' goes through the handle's lock and the I\/O manager,
-- which cost several times what the @WriteFile@ underneath does. There is no
-- buffering on this side at all: a record sitting in a user-space buffer would
-- be invisible to the separate read handles that 'preadAt' uses, so a 'get' of
-- a key that was just 'put' would miss it.
newtype AppendHandle = AppendHandle HANDLE

openAppend :: FilePath -> IO (AppendHandle, Word64)
openAppend p = do
  h <-
    createFile
      p
      gENERIC_WRITE
      (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
      Nothing
      oPEN_ALWAYS
      fILE_ATTRIBUTE_NORMAL
      Nothing
  sz <- alloca $ \pSize -> do
    ok <- c_GetFileSizeEx h pSize
    if ok
      then peek pSize
      else do
        -- Read the error before CloseHandle can overwrite it.
        err <- getLastError
        closeHandle h
        failWith "GetFileSizeEx" err
  pure (AppendHandle h, fromIntegral sz)

-- | Append at the end of the file.
--
-- An @OVERLAPPED@ offset of all ones means \"the current end of file\", which is
-- the documented equivalent of opening with @FILE_APPEND_DATA@ — and unlike that
-- access right, it leaves the handle able to @FlushFileBuffers@.
appendBytes :: AppendHandle -> ByteString -> IO ()
appendBytes (AppendHandle h) bs =
  BSU.unsafeUseAsCStringLen bs $ \(p, len) -> go (castPtr p) len
  where
    go _ 0 = pure ()
    go p len = do
      done <- withOverlapped maxBound $ \ovl pDone -> do
        ok <- c_WriteFile h p (fromIntegral (min len maxChunk)) pDone ovl
        if ok then fromIntegral <$> peek pDone else failWith "WriteFile" =<< getLastError
      go (p `plusPtr` done) (len - done)

-- | Ask the OS to commit everything written through this handle.
syncFile :: AppendHandle -> IO ()
syncFile (AppendHandle h) = flushFileBuffers h

closeAppend :: AppendHandle -> IO ()
closeAppend (AppendHandle h) = closeHandle h

-- | Cut the file back to @n@ bytes, undoing an append that failed part-way.
--
-- @SetEndOfFile@ truncates at the file pointer, so the pointer is moved there
-- first. Nothing else uses the pointer: 'appendBytes' always writes at the
-- end of the file, wherever that now is.
truncateAppend :: AppendHandle -> Word64 -> IO ()
truncateAppend (AppendHandle h) n = do
  moved <- c_SetFilePointerEx h (fromIntegral n) nullPtr 0 -- FILE_BEGIN
  unless moved $ failWith "SetFilePointerEx" =<< getLastError
  cut <- c_SetEndOfFile h
  unless cut $ failWith "SetEndOfFile" =<< getLastError

foreign import ccall unsafe "windows.h SetFilePointerEx"
  c_SetFilePointerEx :: HANDLE -> Int64 -> Ptr Int64 -> DWORD -> IO BOOL

foreign import ccall unsafe "windows.h SetEndOfFile"
  c_SetEndOfFile :: HANDLE -> IO BOOL

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
