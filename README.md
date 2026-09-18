# bitcask

Haskell implementation of Sheehy & Smith,
[*Bitcask: A Log-Structured Hash Table for Fast Key/Value Data*][paper].

A store is a directory of append-only data files with an in-memory index. A
read is one hash lookup and one positional read. A write is one append.

**Every key is kept in RAM.** If you have a hundred million small keys, this
is the wrong tool.

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

Keys and values can be any type with a `Codec` instance. Derive one from a
serialisation library (`AsSerialize` is included for `cereal`) or write the two
methods yourself. `Database.Bitcask.Raw` is the same store over plain
`ByteString`s.

## Features

* `get`, `put`, `delete`, `member`, `keys`, `fold`, `sync`, `merge`, `stats`.
* Reads take no lock; writes are serialised; `merge` blocks neither.
* One writer process at a time, enforced by a directory lock that the OS
  releases if the process dies.
* A torn write at the end of the newest data file is repaired on open. Other
  corruption throws.
* Hint files make reopening fast. They're just a cache and can be deleted.

## Limitations

* No transactions or atomic batches.
* No ordered iteration; the index is a hash table.
* Keys max out at 64 KiB, values at 4 GiB.

## Durability

The default `SyncPolicy` is `SyncNever`, same as the paper. Writes reach the OS
immediately but only survive a machine crash after `sync`. `withBitcask` and
`close` sync on exit. Set `syncPolicy` if you need more.

If a write returns, it happened. If it throws, it didn't. If it's interrupted
by `timeout` or `killThread`, it either happened or didn't. The exception is a
failed `fsync` or a failed append that can't be undone. Then the store is
marked broken and writes throw `StoreBroken` until you close and reopen it,
which repairs the file. Reads keep working. See the `Database.Bitcask` haddock
for details.

## Build

```
$ cabal build
$ cabal test
$ cabal haddock --open
```

## Benchmarks

`bitcask-bench` times the codecs (CRC, record and hint encoding) and the store
operations (`put`, `get`, `fold`, `open`, `merge`) on a real store in a temp
directory. Nothing syncs, so it measures the library and page cache, not the
disk.

```
$ cabal bench bitcask-bench
```

To check for regressions, save a baseline and compare. `--fail-if-slower` exits
non-zero on a slowdown:

```
$ cabal bench bitcask-bench --benchmark-options='--csv before.csv'
$ # ... make the change ...
$ cabal bench bitcask-bench --benchmark-options='--baseline before.csv --fail-if-slower 10'
```

`-p` selects benchmarks by pattern, e.g. `--benchmark-options='-p /get/'`.

## Profiling

`bitcask-workload` runs a full session: bulk load, random reads from one then
several threads, overwrites, a fold, a merge, and two reopens (from hint files,
then by scanning). It prints wall time and throughput per phase, and the heap
used by each reopened keydir.

```
$ cabal run bitcask-workload -- 300000 100 4    # keys, value bytes, reader threads
```

Use a separate build directory for profiling so it doesn't invalidate the
normal build. Time and allocation profile, written to `bitcask-workload.prof`:

```
$ cabal run bitcask-workload --builddir=dist-prof --enable-profiling --profiling-detail=late -- 300000 +RTS -p -RTS
```

`late` inserts cost centres after optimisation, so you profile the optimised
code.

Heap profile by closure type, no profiling build needed:

```
$ cabal run bitcask-workload -- 300000 +RTS -hT -i0.05 -RTS
$ hp2ps -c bitcask-workload.hp
```

`+RTS -s -RTS` for GC stats, `+RTS -l -RTS` for an eventlog (`ghc-events-analyze`
or `eventlog2html`).

Notes: a small `get` is one pread and the syscall is most of the cost, so it
mostly measures the OS. The workload runs with `-N`, and with many idle
capabilities the parallel GC can cost more than it saves; try `+RTS -qg` or
`-qn4`.

## Design

[`DESIGN.md`](DESIGN.md) explains the format, error handling and concurrency
model, and where the implementation diverged from the original design.

[paper]: https://riak.com/assets/bitcask-intro.pdf
# bitcask
