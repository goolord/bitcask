# bitcask

A Haskell implementation of the storage model in Sheehy & Smith,
[*Bitcask: A Log-Structured Hash Table for Fast Key/Value Data*][paper].

A store is a directory of append-only data files with an in-memory index, so a
read costs one hash lookup and one positional read, and a write costs one append.

**Every key in the store lives in RAM, permanently.** That is the trade Bitcask
makes, and it is the first thing to check against your data: a store with a
hundred million small keys is not a Bitcask.

```haskell
{-# LANGUAGE DerivingVia, DeriveAnyClass, DeriveGeneric, TypeApplications #-}

import Database.Bitcask
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User { userName :: Text, userAge :: Int }
  deriving stock (Show, Generic)
  deriving anyclass (Binary)
  deriving Codec via (AsBinary User)

main :: IO ()
main = withBitcask @Text @User "users" defaultOptions $ \bc -> do
  put bc "nano" (User "nano" 33)
  print =<< get bc "nano"
```

Keys and values are any types with a `Codec` instance. Derive one from a
serialisation library you already use — `AsBinary` is built in because `binary`
ships with GHC — or write the two methods yourself. `Database.Bitcask.Raw` is the
same store at plain `ByteString`s.

## What it does

* `get`, `put`, `delete`, `member`, `keys`, `fold`, `sync`, `merge`, `stats`.
* Reads take no lock; writes are serialised; `merge` blocks neither.
* One process may hold a store open for writing, enforced by a lock on the
  directory that the OS releases if the process dies.
* A torn write at the end of the newest data file is repaired on open. Corruption
  anywhere else is reported rather than skipped.
* Hint files make reopening a large store fast; they are pure cache and can be
  deleted.

## What it does not do

* No transactions and no atomic batches. Bitcask has no commit record.
* No ordered iteration. The index is a hash table.
* Keys are capped at 64 KiB and values at 4 GiB.

## Durability

The default `SyncPolicy` is `SyncNever`, which matches the paper. Records reach
the operating system as they are written, but survive a machine crash only once
`sync` has run; `withBitcask` and `close` sync on the way out. Set `syncPolicy`
if you need more.

## Build

```
$ cabal build
$ cabal test
$ cabal haddock --open
```

## Design

[`DESIGN.md`](DESIGN.md) is the record of why the format, the error handling and
the concurrency model look the way they do, including where the implementation
had to correct the design.

[paper]: https://riak.com/assets/bitcask-intro.pdf
# bitcask
