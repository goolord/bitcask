-- | The in-memory index, and the pure fold that rebuilds it.
--
-- Recovery is a pure fold so it can be property-tested without touching the
-- filesystem. The IO layer turns files into @('ByteString', 'Loc',
-- isTombstone)@ refs and 'replay' builds the keydir.
--
-- Refs must be applied in @('FileId', offset)@ order, which is write order.
-- Then the newest ref for a key just overwrites the older ones, with no
-- timestamp comparison, so recovery doesn't depend on the clock.
module Database.Bitcask.Internal.Keydir
  ( Keydir
  , Ref (..)
  , empty
  , applyRef
  , replay
  , lookup
  , member
  , delete
  , insert
  , replace
  , replaceIf
  , keys
  , toList
  , foldlLocs'
  , size
  , liveBytes
  ) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Word (Word64)

import Database.Bitcask.Types (Loc (..))

-- | Encoded key to the location of its live record.
--
-- Keys are copied into 'ShortByteString's on insert. The incoming 'ByteString'
-- is often a slice of something much bigger (a scan buffer, a hint file, the
-- caller's buffer) and would keep all of it alive. Scanning on open used to
-- keep every data file in memory this way. 'ShortByteString' is unpinned and
-- costs about 16 bytes more per key than a slice.
newtype Keydir = Keydir (HashMap ShortByteString Loc)

-- | Index info for one record, from a data file or hint file.
data Ref = Ref
  { refKey :: !ByteString
  , refLoc :: !Loc
  , refTombstone :: !Bool
  }
  deriving stock (Eq, Show)

empty :: Keydir
empty = Keydir HM.empty

-- | Apply one ref. Tombstones delete, anything else replaces.
applyRef :: Ref -> Keydir -> Keydir
applyRef r kd
  | refTombstone r = delete (refKey r) kd
  | otherwise = insert (refKey r) (refLoc r) kd
{-# INLINE applyRef #-}

-- | Rebuild a keydir from refs in write order.
replay :: [Ref] -> Keydir
replay = L.foldl' (flip applyRef) empty

lookup :: ByteString -> Keydir -> Maybe Loc
lookup k (Keydir m) = HM.lookup (SBS.toShort k) m

member :: ByteString -> Keydir -> Bool
member k (Keydir m) = HM.member (SBS.toShort k) m

delete :: ByteString -> Keydir -> Keydir
delete k (Keydir m) = Keydir (HM.delete (SBS.toShort k) m)

insert :: ByteString -> Loc -> Keydir -> Keydir
insert k l (Keydir m) = Keydir (HM.insert (SBS.toShort k) l m)

-- | Insert or delete and return the old location, in one traversal. Used by
-- the write path.
replace :: ByteString -> Maybe Loc -> Keydir -> (Keydir, Maybe Loc)
replace k new (Keydir m) =
  let (old, m') = HM.alterF (\o -> (o, new)) (SBS.toShort k) m
   in (Keydir m', old)

-- | Set a key's location only if it's currently @expected@. Returns whether it
-- was. Used by merge.
replaceIf :: ByteString -> Loc -> Loc -> Keydir -> (Keydir, Bool)
replaceIf k expected new (Keydir m) =
  case HM.alterF claim (SBS.toShort k) m of
    (True, m') -> (Keydir m', True)
    (False, _) -> (Keydir m, False)
  where
    claim (Just cur) | cur == expected = (True, Just new)
    claim cur = (False, cur)

keys :: Keydir -> [ByteString]
keys (Keydir m) = map SBS.fromShort (HM.keys m)

toList :: Keydir -> [(ByteString, Loc)]
toList (Keydir m) = [(SBS.fromShort k, l) | (k, l) <- HM.toList m]

-- | Strict left fold over every location, without building any keys.
foldlLocs' :: (a -> Loc -> a) -> a -> Keydir -> a
foldlLocs' f z (Keydir m) = HM.foldl' f z m

size :: Keydir -> Int
size (Keydir m) = HM.size m

-- | Total on-disk bytes reachable from the keydir.
liveBytes :: Keydir -> Word64
liveBytes = foldlLocs' (\acc l -> acc + fromIntegral (locSize l)) 0
