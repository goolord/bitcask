-- | Data file naming, directory layout, and scanning files into keydir refs.
module Database.Bitcask.Internal.File
  ( -- * Paths
    dataPath
  , hintPath
  , lockPath
  , metaPath
  , pendingPath
  , parseDataName
  , listDataFiles

    -- * Scanning
  , foldDataFile
  , refFromHint
  , readHintRefs

    -- * The meta file
  , formatVersion
  , readMeta
  , writeMeta

    -- * The pending-delete manifest
  , readPending
  , writePending
  ) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import Text.Read (readMaybe)

import Database.Bitcask.Internal.Bytes (indexBE16, indexBE32)
import Database.Bitcask.Internal.Hint (HintEntry (..), decodeHintFile)
import Database.Bitcask.Internal.Keydir (Ref (..))
import Database.Bitcask.Internal.Platform (ReadHandle, preadAt)
import Database.Bitcask.Internal.Record
  ( Record (..)
  , decodeHeader
  , decodeRecord
  , headerSize
  , recordSize
  )
import Database.Bitcask.Types

-- | Data files are named @\<base\>-\<sub\>.data@, zero-padded to ten digits so
-- names sort in id order.
dataPath :: FilePath -> FileId -> FilePath
dataPath dir fid = dir </> fileStem fid <> ".data"

hintPath :: FilePath -> FileId -> FilePath
hintPath dir fid = dir </> fileStem fid <> ".hint"

fileStem :: FileId -> String
fileStem fid = printf "%010u-%010u" (fileBase fid) (fileSub fid)

lockPath :: FilePath -> FilePath
lockPath dir = dir </> "bitcask.lock"

metaPath :: FilePath -> FilePath
metaPath dir = dir </> "bitcask.meta"

pendingPath :: FilePath -> FilePath
pendingPath dir = dir </> "bitcask.pending"

parseDataName :: FilePath -> Maybe FileId
parseDataName name = do
  stem <- stripSuffix ".data" name
  case break (== '-') stem of
    (b, '-' : s) -> mkFileId <$> readMaybe b <*> readMaybe s
    _ -> Nothing
  where
    stripSuffix suf xs =
      let n = length xs - length suf
       in if n >= 0 && drop n xs == suf then Just (take n xs) else Nothing

-- | Every data file in the directory, in replay order.
listDataFiles :: FilePath -> IO [FileId]
listDataFiles dir = sort . mapMaybe parseDataName <$> listDirectory dir

-- | Scan read size.
chunkSize :: Int
chunkSize = 1024 * 1024

-- | Fold a data file into keydir refs, in write order.
--
-- Returns the accumulator and, if the file ends in a torn write, the offset to
-- truncate to. A bad record in the middle of a file throws 'CorruptRecord'.
-- Only a bad record that runs to the end of the file counts as a torn tail.
foldDataFile
  :: Bool
  -- ^ verify checksums
  -> FilePath
  -- ^ path, for error reporting
  -> FileId
  -> ReadHandle
  -> Word64
  -- ^ file size
  -> (a -> Ref -> a)
  -> a
  -> IO (a, Maybe Word64)
foldDataFile verify path fid rh size step = go 0 BS.empty
  where
    go off buf acc
      | off >= size = pure (acc, Nothing)
      | otherwise = case decodeHeader buf of
          -- Only fails for want of bytes.
          Left _ -> refill off buf acc headerSize
          Right h
            | BS.length buf < recordSize h -> refill off buf acc (recordSize h)
            | otherwise -> case decodeRecord verify buf of
                Left err -> tornOrCorrupt off err acc h
                Right r ->
                  let n = recordSize h
                      ref =
                        Ref
                          { refKey = recKey r
                          , refLoc = Loc fid off (fromIntegral n) (recTstamp r)
                          , refTombstone = isNothing (recValue r)
                          }
                      acc' = step acc ref
                   in acc' `seq` go (off + fromIntegral n) (BS.drop n buf) acc'

    -- Read onto the end of the buffer so it holds the @need@ bytes of the
    -- record at @off@, and at least a chunk. One read even for a big record,
    -- and never past EOF, whatever size a corrupt header claims.
    refill off buf acc need = do
      let have = off + fromIntegral (BS.length buf)
      if have >= size
        then
          -- EOF and no whole record left in the buffer.
          pure (acc, if BS.null buf then Nothing else Just off)
        else do
          let want = max chunkSize (need - BS.length buf)
          more <- preadAt rh have (fromIntegral (min (size - have) (fromIntegral want)))
          if BS.null more
            then pure (acc, if BS.null buf then Nothing else Just off)
            else go off (buf <> more) acc

    -- Only a torn tail if the record would run to EOF. Anywhere else it's
    -- corruption, and skipping it would bring back an older value.
    tornOrCorrupt off err acc h
      | off + fromIntegral (recordSize h) >= size = pure (acc, Just off)
      | otherwise = throwIO (CorruptRecord path off err)

-- | Turn a hint entry into a keydir ref.
refFromHint :: FileId -> HintEntry -> Ref
refFromHint fid e =
  Ref
    { refKey = hintKey e
    , refLoc = Loc fid (hintPos e) (hintRecSize e) (hintTstamp e)
    , refTombstone = hintTombstone e
    }

-- | The refs from a complete hint file, in write order. 'Nothing' means scan
-- the data file instead.
--
-- The file is validated up front; the list is lazy.
readHintRefs :: FilePath -> FileId -> IO (Maybe [Ref])
readHintRefs dir fid = do
  r <- try (BS.readFile (hintPath dir fid))
  pure $! case r of
    -- Includes there being no hint file.
    Left (_ :: IOException) -> Nothing
    Right bs -> case decodeHintFile bs of
      Left _ -> Nothing
      Right es -> Just (map (refFromHint fid) es)

-- | Bump on incompatible format changes.
formatVersion :: Word32
formatVersion = 1

metaMagic :: ByteString
metaMagic = BC.pack "BITCASK\0"

-- | Read @bitcask.meta@. 'Nothing' for a new store.
readMeta :: FilePath -> IO (Maybe (Word32, Maybe Text))
readMeta dir = do
  let p = metaPath dir
  exists <- doesFileExist p
  if not exists
    then pure Nothing
    else do
      bs <- BS.readFile p
      unless (metaMagic `BS.isPrefixOf` bs && BS.length bs >= 14) $
        throwIO (NotAStore dir)
      let ver = indexBE32 bs 8
          tagLen = fromIntegral (indexBE16 bs 12)
          tagBytes = BS.take tagLen (BS.drop 14 bs)
      pure . Just $
        ( ver
        , if tagLen == 0 then Nothing else either (const Nothing) Just (TE.decodeUtf8' tagBytes)
        )

writeMeta :: FilePath -> Maybe Text -> IO ()
writeMeta dir tag =
  BS.writeFile (metaPath dir) . BL.toStrict . BB.toLazyByteString $
    BB.byteString metaMagic
      <> BB.word32BE formatVersion
      <> BB.word16BE (fromIntegral (BS.length tagBytes))
      <> BB.byteString tagBytes
  where
    tagBytes = maybe BS.empty (TE.encodeUtf8) tag

-- | File ids merge couldn't delete because a reader had them open. Windows
-- only. Deleted on the next 'Database.Bitcask.open'.
readPending :: FilePath -> IO [FileId]
readPending dir = do
  let p = pendingPath dir
  exists <- doesFileExist p
  if not exists
    then pure []
    else do
      bs <- BS.readFile p
      pure . mapMaybe (parseDataName . (<> ".data") . BC.unpack) . filter (not . BS.null) $
        BC.lines bs

writePending :: FilePath -> [FileId] -> IO ()
writePending dir fids = BS.writeFile (pendingPath dir) (BC.unlines (map (BC.pack . fileStem) fids))
