module Main (main) where

import Test.Tasty

import qualified Test.Bitcask.Codec
import qualified Test.Bitcask.Concurrency
import qualified Test.Bitcask.Crash
import qualified Test.Bitcask.Keydir
import qualified Test.Bitcask.Model
import qualified Test.Bitcask.Record

main :: IO ()
main =
  defaultMain $
    testGroup
      "bitcask"
      [ testGroup
          "pure"
          [ Test.Bitcask.Codec.tests
          , Test.Bitcask.Record.tests
          , Test.Bitcask.Keydir.tests
          ]
      , testGroup
          "store"
          [ Test.Bitcask.Model.tests
          , Test.Bitcask.Crash.tests
          , Test.Bitcask.Concurrency.tests
          ]
      ]
