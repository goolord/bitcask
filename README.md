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
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User { userName :: String, userAge :: Int }
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)
  deriving Codec via (AsSerialize User)

main :: IO ()
main = withBitcask @Text @User "users" defaultOptions $ \bc -> do
  put bc "nano" (User "nano" 33)
  print =<< get bc "nano"
```

Keys and values are any types with a `Codec` instance. Derive one from a
serialisation library you already use — `AsSerialize` is built in for `cereal`
— or write the two methods yourself. `Database.Bitcask.Raw` is the
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

## Benchmarks

`bitcask-bench` times the pure codecs (CRC, record and hint encoding) and the
store operations (`put`, `get`, `fold`, `open`, `merge`) against a real store in
a temporary directory. Nothing in it syncs, so it measures the library and the
OS page cache, not the disk.

```
$ cabal bench bitcask-bench
```

To check a change for regressions, save a baseline first and compare against it;
`--fail-if-slower` turns a slowdown into a failing exit code:

```
$ cabal bench bitcask-bench --benchmark-options='--csv before.csv'
$ # ... make the change ...
$ cabal bench bitcask-bench --benchmark-options='--baseline before.csv --fail-if-slower 10'
```

`-p` selects benchmarks by pattern, e.g. `--benchmark-options='-p /get/'`.

## Profiling

`bitcask-workload` runs one realistic session end to end: a bulk load, random
reads from one and then several threads, overwrites, a fold, a merge, and two
reopens (from hint files, then by scanning the data files). It prints the wall
time and throughput of each phase, and the heap each reopened keydir holds.

```
$ cabal run bitcask-workload -- 300000 100 4    # keys, value bytes, reader threads
```

Profiling builds go in their own build directory so they do not invalidate the
normal one. For a time and allocation profile by cost centre, written to
`bitcask-workload.prof`:

```
$ cabal run bitcask-workload --builddir=dist-prof --enable-profiling --profiling-detail=late -- 300000 +RTS -p -RTS
```

`late` cost centres are inserted after optimisation, so the profile describes the
code that actually runs rather than a de-optimised copy of it.

A heap profile by closure type needs no profiling build at all:

```
$ cabal run bitcask-workload -- 300000 +RTS -hT -i0.05 -RTS
$ hp2ps -c bitcask-workload.hp
```

For GC statistics add `+RTS -s -RTS`, and for a timeline of threads and GC that
can be opened in `ghc-events-analyze` or `eventlog2html`, `+RTS -l -RTS`.

Two things worth knowing when reading the numbers. A `get` of a small value is
one positional read, and the system call is almost all of its cost, so it tracks
the OS more than this library. And the workload runs with `-N`: with many idle
capabilities the parallel GC can cost more than it saves, which `+RTS -qg` or
`-qn4` will show.

## Design

[`DESIGN.md`](DESIGN.md) is the record of why the format, the error handling and
the concurrency model look the way they do, including where the implementation
had to correct the design.

[paper]: https://riak.com/assets/bitcask-intro.pdf
# bitcask
