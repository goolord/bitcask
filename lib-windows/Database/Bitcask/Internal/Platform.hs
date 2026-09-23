-- | Windows implementation of the platform layer.
--
-- Same interface as @lib-posix@; cabal picks one with @os(windows)@.
--
-- Differences from POSIX:
--
-- * __Positional reads__ use a raw Win32 @HANDLE@ and @ReadFile@ with the
--   offset in an @OVERLAPPED@. That's the Windows @pread@: the offset is per
--   call, not the handle's file pointer. A GHC 'Handle' doesn't work here; see
--   'ReadHandle'.
--
-- * __Deleting a merged file__ while a reader has it open works because read
--   handles are opened with @FILE_SHARE_DELETE@. If a delete still fails (e.g.
--   another process has it open), 'removeOpen' returns 'False' and the caller
--   adds the file to @bitcask.pending@ for the next open. That path is slow
--   (@removeFile@ retries sharing violations for about two seconds), so it
--   shouldn't be the common case.
--
-- * __No directory sync.__ @FlushFileBuffers@ on the file is enough, so
--   'syncDir' is a no-op.
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
-- Raw Win32 @HANDLE@s, not a GHC 'Handle'. GHC has its own per-file
-- single-writer/multi-reader lock, so opening the active file for reading
-- fails ("resource busy") while it's open for append, and 'get' on a freshly
-- written key would fail.
--
-- Windows serialises I\/O on a synchronous handle, so threads sharing one
-- handle take turns. With one handle per file, more reader threads made reads
-- slower. So each file gets one handle per capability. A capability runs one
-- Haskell thread at a time and the reads are @unsafe@ calls, so reads on the
-- same handle never overlap.
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
    -- Close the ones already opened if a later one fails.
    onException' acc act = act `onException` mapM_ closeHandle acc

-- | Max read handles per file.
maxReadHandles :: Int
maxReadHandles = 16

-- | The handle for the current capability.
pickHandle :: SmallArray HANDLE -> IO HANDLE
pickHandle hs = do
  (cap, _) <- threadCapability =<< myThreadId
  pure $! indexSmallArray hs (cap `rem` sizeofSmallArray hs)
{-# INLINE pickHandle #-}

-- | Read @n@ bytes at an offset. Returns fewer at end of file.
--
-- The offset goes in the @OVERLAPPED@, so the handle's file pointer isn't used
-- and concurrent reads don't interfere.
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
        -- A read at or past EOF fails with ERROR_HANDLE_EOF instead of
        -- returning zero bytes.
        if err == eRROR_HANDLE_EOF then pure 0 else failWith "ReadFile" err
  where
    eRROR_HANDLE_EOF = 38

-- | Max bytes per @ReadFile@ or @WriteFile@. The length is a 'DWORD'; callers
-- loop.
maxChunk :: Int
maxChunk = 0x40000000

-- | An @OVERLAPPED@ with a file offset, plus a 'DWORD' for the byte count, in
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

-- These are @unsafe@ calls. A @safe@ call releases the capability, which costs
-- a few hundred ns. Reads and appends almost always hit the page cache and take
-- a microsecond or two, so that overhead would dominate. The POSIX layer does
-- the same for @pread@.
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
-- A raw @HANDLE@ for the same reason as 'ReadHandle'. Also, writing through a
-- GHC 'Handle' goes through its lock and the I\/O manager, which costs several
-- times the @WriteFile@ itself. No buffering: 'preadAt' uses separate handles
-- and wouldn't see buffered bytes, so a 'get' right after a 'put' would miss.
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
-- An @OVERLAPPED@ offset of all ones means end of file. Same as opening with
-- @FILE_APPEND_DATA@, except the handle can still @FlushFileBuffers@.
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

-- | Flush everything written through this handle to disk.
syncFile :: AppendHandle -> IO ()
syncFile (AppendHandle h) = flushFileBuffers h

closeAppend :: AppendHandle -> IO ()
closeAppend (AppendHandle h) = closeHandle h

-- | Truncate to @n@ bytes, to undo a partial append.
--
-- @SetEndOfFile@ truncates at the file pointer, so move it first. Nothing else
-- uses the pointer; 'appendBytes' always writes at the end.
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

-- | Take the store lock, or report that someone else has it.
--
-- The open is the lock: an exclusive lock shares nothing, and a shared lock
-- only shares reading with other shared locks. Windows releases it when the
-- process exits, like @fcntl@ locks on POSIX. Windows doesn't say who holds
-- it, hence the 'Nothing'.
takeLock :: FilePath -> LockMode -> IO (Either (Maybe Word32) LockHandle)
takeLock p mode = do
  r <-
    try $
      createFile
        p
        access
        share
        Nothing
        oPEN_ALWAYS
        fILE_ATTRIBUTE_NORMAL
        Nothing
  pure $ case r of
    Right h -> Right (LockHandle h)
    Left (_ :: IOException) -> Left Nothing
  where
    (access, share) = case mode of
      LockExclusive -> (gENERIC_READ .|. gENERIC_WRITE, 0)
      LockShared -> (gENERIC_READ, fILE_SHARE_READ)

dropLock :: LockHandle -> IO ()
dropLock (LockHandle h) = closeHandle h

-- | Remove a file that readers may still have open. Returns 'False' instead of
-- throwing if it fails; the caller falls back to @bitcask.pending@.
removeOpen :: FilePath -> IO Bool
removeOpen p = do
  r <- try (removeFile p)
  pure $ case r of
    Right () -> True
    Left (_ :: IOException) -> False
