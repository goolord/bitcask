-- | The in-memory index, and the pure state machine that rebuilds it.
--
-- Recovery is the part of a log-structured store that is hardest to get right
-- and scariest to get wrong, so it lives here as a pure fold: the IO layer turns
-- files into a stream of @('ByteString', 'Loc', isTombstone)@ and 'replay'
-- builds the keydir. No filesystem, no platform layer, and therefore trivially
-- property-testable.
--
-- The one invariant callers must honour: refs are applied in ascending
-- @('FileId', offset)@ order, which is the order writes actually happened. Given
-- that, the newest ref for a key always wins by simply overwriting the older,
-- and no timestamp comparison is needed — which is the point, because a
-- timestamp comparison would make recovery depend on the system clock.
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
  , foldlWithKey'
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

-- | Encoded key bytes to the location of that key's live record.
--
-- Keys are held as 'ShortByteString', copied in on insert, rather than as the
-- 'ByteString' they arrive in. A 'ByteString' key is very often a slice of
-- something much bigger — a one-megabyte scan buffer on open, a whole hint file,
-- a caller's own buffer — and the keydir would keep the whole of that alive for
-- as long as the key is live: opening a store by scanning its data files used to
-- keep every data file in memory. A 'ShortByteString' is exactly its own bytes
-- and is not pinned, so the GC can move and compact it. It costs about 16 bytes
-- a key more than a 'ByteString' slice would, not counting whatever that slice
-- would have kept alive.
newtype Keydir = Keydir (HashMap ShortByteString Loc)

-- | One record's worth of index information, as recovered from a data file or a
-- hint file.
data Ref = Ref
  { refKey :: !ByteString
  , refLoc :: !Loc
  , refTombstone :: !Bool
  }
  deriving stock (Eq, Show)

empty :: Keydir
empty = Keydir HM.empty

-- | Fold one ref into the keydir. A tombstone removes the key; anything else
-- replaces it.
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

-- | Insert or delete, and hand back what the key used to map to, in a single
-- traversal. This is the write path's one keydir update.
replace :: ByteString -> Maybe Loc -> Keydir -> (Keydir, Maybe Loc)
replace k new (Keydir m) =
  let (old, m') = HM.alterF (\o -> (o, new)) (SBS.toShort k) m
   in (Keydir m', old)

-- | Set a key's location, but only if it is currently @expected@. Returns
-- whether it was. This is merge's compare-on-location claim.
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

-- | Strict left fold over every key and location.
foldlWithKey' :: (a -> ByteString -> Loc -> a) -> a -> Keydir -> a
foldlWithKey' f z (Keydir m) = HM.foldlWithKey' (\a k l -> f a (SBS.fromShort k) l) z m

-- | Strict left fold over every location, without materialising any key.
foldlLocs' :: (a -> Loc -> a) -> a -> Keydir -> a
foldlLocs' f z (Keydir m) = HM.foldl' f z m

size :: Keydir -> Int
size (Keydir m) = HM.size m

-- | Total on-disk bytes reachable from the keydir.
liveBytes :: Keydir -> Word64
liveBytes = foldlLocs' (\acc l -> acc + fromIntegral (locSize l)) 0
