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
  , keys
  , toList
  , size
  , liveBytes
  ) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Word (Word64)

import Database.Bitcask.Types (Loc (..))

-- | Encoded key bytes to the location of that key's live record.
type Keydir = HashMap ByteString Loc

-- | One record's worth of index information, as recovered from a data file or a
-- hint file.
data Ref = Ref
  { refKey :: !ByteString
  , refLoc :: !Loc
  , refTombstone :: !Bool
  }
  deriving stock (Eq, Show)

empty :: Keydir
empty = HM.empty

-- | Fold one ref into the keydir. A tombstone removes the key; anything else
-- replaces it.
applyRef :: Ref -> Keydir -> Keydir
applyRef r kd
  | refTombstone r = HM.delete (refKey r) kd
  | otherwise = HM.insert (refKey r) (refLoc r) kd
{-# INLINE applyRef #-}

-- | Rebuild a keydir from refs in write order.
replay :: [Ref] -> Keydir
replay = foldl' (flip applyRef) empty

lookup :: ByteString -> Keydir -> Maybe Loc
lookup = HM.lookup

member :: ByteString -> Keydir -> Bool
member = HM.member

delete :: ByteString -> Keydir -> Keydir
delete = HM.delete

insert :: ByteString -> Loc -> Keydir -> Keydir
insert = HM.insert

keys :: Keydir -> [ByteString]
keys = HM.keys

toList :: Keydir -> [(ByteString, Loc)]
toList = HM.toList

size :: Keydir -> Int
size = HM.size

-- | Total on-disk bytes reachable from the keydir.
liveBytes :: Keydir -> Word64
liveBytes = HM.foldl' (\acc l -> acc + fromIntegral (locSize l)) 0
