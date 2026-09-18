-- | The store over plain 'ByteString's, without 'Codec' constraints.
--
-- Same format as the typed API: @'Codec' 'ByteString'@ is the identity.
module Database.Bitcask.Raw
  ( Raw
  , module Database.Bitcask
  ) where

import Data.ByteString (ByteString)

import Database.Bitcask

-- | A store whose keys and values are already bytes.
type Raw = Bitcask ByteString ByteString
