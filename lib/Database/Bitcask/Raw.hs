-- | The same store at raw bytes, for when you have 'ByteString's already and do
-- not want the 'Codec' constraints in your signatures.
--
-- @'Raw'@ is not a different store: @'Codec' 'ByteString'@ is the identity, so a
-- 'Raw' handle and a typed one over the same directory see exactly the same
-- bytes.
module Database.Bitcask.Raw
  ( Raw
  , module Database.Bitcask
  ) where

import Data.ByteString (ByteString)

import Database.Bitcask

-- | A store whose keys and values are already bytes.
type Raw = Bitcask ByteString ByteString
