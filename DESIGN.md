# Design: an idiomatic Haskell Bitcask

How this library implements Sheehy & Smith, *Bitcask: A Log-Structured Hash Table
for Fast Key/Value Data*, and why it makes the choices it does. It began as a
proposal and is now the record: revisions 1 and 2 were agreed before any code
existed, and revision 3 is what building it changed.

**Revision 2.** Decisions taken since the first draft:

| # | question | answer |
|---|----------|--------|
| 1 | Windows? | **Yes, supported.** Reworked — see §2 and §6. |
| 2 | Typed or `ByteString`? | **Typed in v1.** See §5. |
| 3 | Module root | `Database.Bitcask`; cabal `category:` changed to `Database`. |
| 4 | Transactions / batch atomicity | Out of v1, revisit later. |
| 5 | Merge | Concurrent, as in §7. |

The two answered *yes* both push against the first draft's simplest path, so most
of what changed is in §2 (portability) and §5 (the typed layer).

**Revision 3.** The library is now implemented and tested against this document.
Seven things changed while building it; each is written into the section it
belongs to, and collected here because a design doc that quietly drifts from the
code is worse than no design doc.

1. **File ids are a `(base, sub)` pair, not a flat counter** (§3.1). Revision 2
   was simply wrong. Merge output holds records that are *older* than anything
   written while the merge ran, so it has to replay first — and a flat counter
   has no id to give it between the newest input and the active file. The store
   would have come back from a restart with stale values. The pair leaves room to
   allocate below the active file.
2. **CRC-32 is implemented in-library** rather than taken from `digest` (§3.2).
   `digest` binds to zlib, and requiring a C library to be present is exactly the
   friction that makes a package painful to install on the platform we just
   committed to supporting.
3. **Windows positional reads use the handle pool**, which §2.1 named as the
   fallback rather than the first choice. Overlapped I/O remains the right answer
   and is the first follow-up; it was not written blind (see point 7).
4. **A process-local registry sits in front of the OS lock** (`Internal.Lock`).
   POSIX advisory locks belong to the *process*, so two handles on one directory
   inside one program both took the lock, both believed they were the writer, and
   both allocated the same file ids. A test caught it.
5. **Hint entries are written as records are appended**, not reconstructed when
   the active file rolls. Buffering them in memory would have cost hundreds of
   megabytes on a large file, and rescanning the file on every roll would have
   put a full read in the middle of a `put`.
6. **Every open starts a fresh active file** instead of appending to the previous
   one. It costs one small file per open, which merge folds away, and buys the
   invariant that a file which is not the active file is never appended to again
   — so a torn tail can only ever exist in one place.
7. **The Windows platform module has never been compiled.** There is no Windows
   and no `Win32` package on the machine this was built on, so `lib-windows` is
   careful, conventional code that no compiler has checked. Everything else here
   is built and tested. This is the first thing to fix.

Also, `base` is bounded `>=4.17 && <5` rather than `^>=4.22`, so the package
builds on the GHCs a library can expect to meet.

---

## 1. What the paper requires, and what it leaves open

**Required.** A directory of append-only data files, exactly one of which is
"active" (being appended to); the rest immutable. An in-memory `keydir` mapping
every key to the file, offset and size of its most recent record. `get` is one
hash lookup plus one positional read. `delete` appends a tombstone. Merge
rewrites the immutable files keeping only live records and emits *hint files*
alongside them, so startup can rebuild the keydir without reading values. Only
one OS process may hold a store open for writing.

**Left open**, and therefore ours to decide: the record layout (the paper shows a
diagram but specifies no field widths), the checksum, how file ids are assigned,
how tombstones are encoded, the merge trigger, and the concurrency story inside a
single process.

**The standing caveat**, which belongs in the README in bold: every key lives in
RAM, forever. Bitcask trades memory for a one-seek read. A store with a hundred
million small keys is not a Bitcask.

---

## 2. Portability: the platform layer

Windows support is the single biggest constraint on this design, because the
three primitives Bitcask leans on are exactly the three that differ most between
POSIX and Win32: positional reads, file locking, and deleting a file that is
still open. Rather than sprinkle `#ifdef` through the library, there is **one
internal module with two implementations**, chosen by cabal:

```haskell
-- Database.Bitcask.Internal.Platform
data ReadHandle
data AppendHandle
data LockHandle

openRead    :: FilePath -> IO ReadHandle
preadAt     :: ReadHandle -> Word64 -> Int -> IO ByteString   -- thread-safe
closeRead   :: ReadHandle -> IO ()

openAppend  :: FilePath -> IO AppendHandle
appendBytes :: AppendHandle -> ByteString -> IO Word64        -- returns the offset written at
syncFile    :: AppendHandle -> IO ()
syncDir     :: FilePath -> IO ()
truncateAt  :: FilePath -> Word64 -> IO ()

takeLock    :: FilePath -> LockMode -> IO (Either LockHolder LockHandle)
dropLock    :: LockHandle -> IO ()

removeOpen  :: FilePath -> IO Bool    -- delete a file that readers may still hold
```

```
library
  hs-source-dirs: lib
  other-modules:  Database.Bitcask.Internal.Platform
  if os(windows)
    hs-source-dirs: lib-windows
    build-depends:  Win32 >=2.13
  else
    hs-source-dirs: lib-posix
    build-depends:  unix >=2.8
```

Each of `lib-posix/` and `lib-windows/` holds its own
`Database/Bitcask/Internal/Platform.hs`. No CPP anywhere else in the library, and
the pure core (§4) never sees either one.

### 2.1 Positional reads

POSIX is easy: `fdPread` from `unix >= 2.8` takes no lock, moves no file pointer,
and lets N threads share one `Fd` per data file.

Windows has no `pread`. `ReadFile` accepts an offset through an `OVERLAPPED`
structure, but on a handle opened *without* `FILE_FLAG_OVERLAPPED` it also moves
the shared file pointer, so concurrent positional reads on one handle race. The
correct Windows implementation opens data files with `FILE_FLAG_OVERLAPPED` and
passes a fresh `OVERLAPPED` (with its own event handle) per call, which is
genuinely thread-safe.

**This is the main technical risk in the whole design.** If the overlapped path
turns out to be fiddly in practice, the fallback is a small pool of handles per
data file guarded by an `MVar` — fully portable, no Win32 subtleties, and it
costs some contention under a high reader count. I'd like to attempt overlapped
first and keep the pool in my back pocket; it's one module either way.

### 2.2 Locking

POSIX: an advisory `fcntl` lock (`System.Posix.IO.setLock`) on `bitcask.lock`,
not an `O_EXCL` create. The kernel releases an advisory lock when the process
dies, so a crashed writer doesn't leave a store that needs manual unwedging.

Windows gets the same self-healing property for free by opening `bitcask.lock`
with `createFile` and a share mode of zero: the exclusive open *is* the lock, and
it's released when the process exits. A shared (read-only) open uses
`FILE_SHARE_READ`.

### 2.3 Deleting merged-away files

On POSIX, merge can unlink an input file while readers still hold descriptors —
the inode survives until the last one closes. Windows refuses to delete an open
file *unless* every handle to it was opened with `FILE_SHARE_DELETE`, in which
case `DeleteFile` marks it for deletion and it disappears when the last handle
closes, which is the POSIX behaviour. So: **every Windows read handle is opened
with `FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE`** from the start.
File ids are never reused, so the lingering name is harmless.

Belt and braces, because getting this wrong leaks disk: if `removeOpen` returns
`False`, the file id is appended to a `bitcask.pending` manifest and swept at the
next `open`, when nothing else holds the store.

### 2.4 Durability

POSIX: `fileSynchronise` on the data file, plus an `fsync` of the *directory*
after creating a new data file — a newly created file is not durable until its
directory entry is.

Windows: `FlushFileBuffers` on the file handle. There is no directory sync, and
none is needed; `syncDir` is a documented no-op there.

---

## 3. On-disk format

### 3.1 Directory layout

```
mydb/
  bitcask.meta        magic, format version, optional store tag (§5.3)
  bitcask.lock        write lock; holds the pid
  0000000001.data     immutable
  0000000001.hint     immutable, optional
  0000000002.data     immutable
  0000000002.hint
  0000000003.data     active — appends go here
```

File ids are a monotonically increasing `Word32` starting at 1, zero-padded to
ten digits so lexicographic order equals numeric order. The paper's
implementation names files by timestamp; a counter is better, because it makes
`(fileId, offset)` a **total order on writes that does not depend on the clock**.
That ordering resolves "which record for this key wins" during recovery, so we
never care whether the system clock went backwards, and Windows and Linux agree
on it. Timestamps are still recorded, purely as data for the user.

### 3.2 Record layout

All integers big-endian. Fixed 19-byte header:

| offset | size | field    | notes                                           |
|-------:|-----:|----------|-------------------------------------------------|
|      0 |    4 | `crc`    | CRC-32 over bytes 4..end (header tail + k + v)  |
|      4 |    8 | `tstamp` | Word64, nanoseconds since the Unix epoch        |
|     12 |    1 | `flags`  | bit 0 = tombstone; bits 1-7 reserved, must be 0 |
|     13 |    2 | `ksz`    | Word16                                          |
|     15 |    4 | `vsz`    | Word32                                          |
|     19 | ksz  | `key`    | the *encoded* key (§5)                          |
| 19+ksz | vsz  | `value`  | the *encoded* value; `vsz = 0` for a tombstone  |

Two deliberate divergences from the paper:

- **Tombstones get a flag bit, not a sentinel value.** The paper says deletion is
  "a write of a special tombstone value", which makes some byte string magically
  unstorable. A flag bit keeps the value space total — you can store the empty
  string, and with a typed API you don't have to explain to users which of their
  values is secretly reserved.
- **Fixed-width sizes, not varints.** `ksz` caps encoded keys at 64 KiB and `vsz`
  caps encoded values at 4 GiB; `put` throws above either. In exchange the header
  is a constant 19 bytes, so a reader can read a header without parsing anything
  first, and hint entries have a fixed stride.

### 3.3 Hint records

Same shape minus the value, plus the record's position:

```
tstamp:8 | flags:1 | ksz:2 | vsz:4 | recordPos:8 | key:ksz
```

A hint file ends with a 12-byte trailer (`recordCount:8 | crc:4`) so a complete
hint file is distinguishable from one truncated by a crash. Incomplete → ignore
it and scan the `.data` file instead. Hint files are pure cache: deleting every
one of them costs startup time and nothing else.

---

## 4. The keydir, and what stays pure

```haskell
data Loc = Loc
  { locFileId :: {-# UNPACK #-} !FileId   -- Word32
  , locPos    :: {-# UNPACK #-} !Word64   -- offset of the RECORD, not the value
  , locSize   :: {-# UNPACK #-} !Word32   -- total record size on disk
  , locTstamp :: {-# UNPACK #-} !Word64
  }

type Keydir = HashMap ByteString Loc      -- always encoded-key bytes
```

Storing the **record** offset rather than the value offset costs `19 + ksz` extra
bytes per read and buys read-time checksum verification: `get` pulls the whole
record in one positional read and checks the CRC before decoding. Configurable
(`verifyChecksums`, default on).

`unordered-containers`' `HashMap` over strict `ByteString`. `Data.Map` would give
ordered iteration, which the paper never promises and which the typed layer
couldn't honour anyway (encoded-byte order is not the user's `Ord`). One type
alias to revisit if we ever want ordered folds over a `Codec`-respecting
encoding.

**Pure core, IO shell.** These are pure, total, and where most of the tests live:

```haskell
-- Database.Bitcask.Internal.Record
encodeRecord :: Word64 -> ByteString -> Maybe ByteString -> Builder  -- Nothing = tombstone
decodeRecord :: ByteString -> Either RecordError Record
decodeHeader :: ByteString -> Either RecordError Header

-- Database.Bitcask.Internal.Keydir
applyRef :: ByteString -> Loc -> Bool -> Keydir -> Keydir   -- Bool = is tombstone
replay   :: [(ByteString, Loc, Bool)] -> Keydir            -- strict left fold
```

Recovery is a *pure fold over a decoded stream*: the IO layer turns files into
`[(ByteString, Loc, Bool)]` and `replay` builds the keydir. The logic that is
hardest to get right and scariest to get wrong is property-testable with no
filesystem and no platform layer involved — which matters a lot more now that
there are two platform layers to keep honest.

Merge planning is pure too: `planMerge :: MergePolicy -> [FileStats] -> [FileId]`.

Everything else — positional reads, appends, sync, locking, unlinking — is honest
`IO`. No `BitcaskT`, no mtl class, no free monad. A storage library that makes
you thread a custom monad through your application is one people route around.

---

## 5. The typed API

The store is parameterised over its key and value types, and the encoded bytes
are a boundary concern: **the internals below §4 never change**. Typing costs one
encode on write and one decode on read, and nothing else.

### 5.1 The codec class

```haskell
-- Database.Bitcask.Codec
class Codec a where
  toBytes   :: a -> Builder
  fromBytes :: ByteString -> Either String a

-- Law:  fromBytes (toStrict (toBytes x)) == Right x
```

Method names deliberately avoid `encode`/`decode`, which would collide with
`binary`, `cereal`, `aeson` and `serialise` at every use site.

That single roundtrip law is doing more work than it looks: it implies `toBytes`
is **injective**, which is exactly the property keys need. Two distinct keys that
encode to the same bytes would silently become one key, and there'd be no way to
notice. So the law isn't decoration — it's the correctness condition for the key
side, and it's a QuickCheck property we can hold every instance to.

Instances in core for `ByteString` (both strict and lazy, `fromBytes = Right`),
`Text`, `Int`, `Int64`, `Word64`, `Double`, `Bool` and `()`.

### 5.2 Choosing a representation with `DerivingVia`

Rather than inventing another serialisation format, adapt whatever the user
already has:

```haskell
newtype AsBinary a = AsBinary a
instance Binary a => Codec (AsBinary a)
```

`binary` is a GHC boot library, so this is free and lives in core. `cereal`,
`store` and `serialise` adapters are three lines each and belong in the user's
project rather than as cabal flags on ours — flag-conditional instances are a
solver hazard, and the pattern is trivial to copy. The haddocks will show it.

In use:

```haskell
data User = User { userName :: Text, userAge :: Int }
  deriving stock (Show, Generic)
  deriving anyclass (Binary)
  deriving Codec via (AsBinary User)

main :: IO ()
main = withBitcask @Text @User "users" defaultOptions $ \bc -> do
  put bc "nano" (User "nano" 33)
  print =<< get bc "nano"          -- Just (User "nano" 33)
```

Two caveats the haddocks must state plainly, because they're the ways this bites:
a `Codec` for a key must be **deterministic** (the same value must encode to the
same bytes every time — anything hashing-order-dependent, like a `HashMap` key,
is disqualified), and a key encoding that changes between releases of your
program silently orphans every key already written.

### 5.3 Catching "opened with the wrong types"

Typed handles create a failure the untyped design didn't have: `Bitcask Int Text`
opened against a store written as `Bitcask Text Text` reads garbage and reports
it as corruption. So `bitcask.meta` carries a magic, a format version, and an
optional user-supplied tag:

```haskell
storeTag :: Maybe Text    -- in OpenOptions, default Nothing
```

Set it (`Just "users/v1"`) and a mismatch fails fast at `open` with
`SchemaMismatch` instead of failing confusingly at the first `get`. Left as
`Nothing`, nothing is checked and nothing is enforced. Deriving the tag
automatically from `Typeable` is tempting and I've deliberately not done it:
renaming a module would then invalidate a perfectly good store.

The format version is always written and always checked, tag or no tag.

### 5.4 Signatures

```haskell
module Database.Bitcask
  ( Bitcask, Codec(..), AsBinary(..)
  , OpenOptions(..), defaultOptions, SyncPolicy(..), MergePolicy(..)
  , BitcaskError(..), Loc(..)
  , open, withBitcask, close
  , get, put, delete, member
  , keys, fold, foldRefs
  , sync, merge, stats
  ) where

open        :: FilePath -> OpenOptions -> IO (Bitcask k v)
withBitcask :: FilePath -> OpenOptions -> (Bitcask k v -> IO a) -> IO a
close       :: Bitcask k v -> IO ()

get      :: (Codec k, Codec v) => Bitcask k v -> k -> IO (Maybe v)
put      :: (Codec k, Codec v) => Bitcask k v -> k -> v -> IO ()
delete   :: Codec k => Bitcask k v -> k -> IO ()
member   :: Codec k => Bitcask k v -> k -> IO Bool

keys     :: Codec k => Bitcask k v -> IO [k]
fold     :: (Codec k, Codec v) => Bitcask k v -> (a -> k -> v -> IO a) -> a -> IO a
foldRefs :: Codec k =>             Bitcask k v -> (a -> k -> Loc -> IO a) -> a -> IO a

sync     :: Bitcask k v -> IO ()
merge    :: Bitcask k v -> IO MergeStats
stats    :: Bitcask k v -> IO Stats
```

The constraints sit on the operations, not on `open`, so `open` needs no
dictionary and `Bitcask k v` is a plain phantom-parameterised handle. `open`
takes its types by application: `open @Text @User dir opts`.

Mapping to the paper: `list_keys` → `keys`, `fold` → `fold`. `foldRefs` is an
addition — folding over keys and locations without reading or decoding any values
is cheap, and it's what you want for "how much space is this using" or for
building an external index.

`Database.Bitcask.Raw` re-exports the same operations at
`Bitcask ByteString ByteString`, for when you have bytes already and don't want
the constraint noise.

### 5.5 Options

```haskell
data OpenOptions = OpenOptions
  { maxFileSize     :: !Word64       -- roll the active file past this; default 2 GiB
  , syncPolicy      :: !SyncPolicy   -- default SyncNever
  , readOnly        :: !Bool         -- default False; takes a shared lock
  , verifyChecksums :: !Bool         -- default True
  , repairTruncated :: !Bool         -- default True; see §6
  , mergePolicy     :: !MergePolicy  -- default MergeManual
  , storeTag        :: !(Maybe Text) -- default Nothing; see §5.3
  }

data SyncPolicy = SyncNever | SyncOnPut | SyncEvery !Int | SyncEveryMicros !Int
```

`SyncNever` matches the paper (its `o_sync` is off by default) and is the honest
default: Bitcask's durability story is "call `sync`, or accept losing the tail".
`withBitcask` syncs on the way out. `MergeManual` by default — a library that
silently starts rewriting gigabytes on a timer is rude.

---

## 6. Failure, errors, recovery

**Errors are exceptions, not `Either`:**

```haskell
data BitcaskError
  = LockHeld         !FilePath !(Maybe Word32)   -- pid, when the platform can tell us
  | CorruptRecord    !FilePath !Word64 !RecordError
  | DecodeFailure    !Field !String              -- Field = KeyField | ValueField
  | SchemaMismatch   !FilePath !Text !Text       -- expected, found
  | UnsupportedFormat !FilePath !Word32
  | KeyTooLarge      !Int
  | ValueTooLarge    !Int
  | NotAStore        !FilePath
  | WriteToReadOnly
  deriving stock Show
  deriving anyclass Exception
```

The rule: **absence is not an error.** `get` returns `Maybe v` and never throws
for a missing key. Everything else — a held lock, a corrupt record, a value that
won't decode — is genuinely exceptional and throws, because threading `Either`
through code already in `IO`, which can already throw `IOException` from every
syscall, buys noise and nothing else. The constructors carry enough structure to
`catch` and act on.

`DecodeFailure` is worth separating from `CorruptRecord`: a record whose CRC
verifies but whose bytes won't decode is almost always a `Codec` mismatch (§5.3),
not a damaged disk, and the error should say so.

**The torn tail.** A process killed mid-append leaves a partial record at the end
of the active file. On open we scan forward; a record at the *end* that is short
or fails its CRC is a torn write, and with `repairTruncated` on (the default) we
truncate back to the last good offset and carry on. A bad record in the *middle*
of a file is real corruption and throws `CorruptRecord` with file and offset — it
should never happen, and quietly skipping it would silently resurrect an old
value for that key.

**Durability details that are easy to skip and shouldn't be:** flushing a buffer
is not syncing a file; see §2.4 for what each platform needs, including the
directory sync POSIX requires and Windows doesn't. Merge output and its hint file
are both synced before any input file is removed.

---

## 7. Concurrency and merge

The paper's constraint is one *process*; inside our process the structure falls
out naturally.

- **Reads are lock-free.** The keydir is an `IORef Keydir`; `get` is `readIORef`
  then one `preadAt`. No lock, no per-thread handle (subject to §2.1 on Windows).
- **Writes are serialised** by an `MVar ActiveFile` holding the append handle,
  the current offset and the file id. One take per `put`; the append is a single
  write of one record.
- **Merge runs concurrently with both.** It never blocks readers, and it doesn't
  block writers: when it copies a live record it updates the keydir with
  `atomicModifyIORef'`, installing the new location **only if the entry still
  points at the old one**. A `put` that raced the merge wins, and the copied
  record is simply dead on arrival. That compare-on-location is the whole trick.

Merge itself: roll the active file first so every input is immutable, then walk
each input in order, asking the keydir whether each record is still the live one
for its key. Live records are copied to a new data file and a hint file beside
it; dead ones are dropped. v1 merges *all* immutable files at once, which is what
makes tombstone removal trivially correct — a tombstone can only be dropped once
no older file can still hold a value for that key. Incremental merge over a
subset is a later refinement and needs tombstone retention rules.

`MergeAuto` spawns a background merge thread that triggers on a dead-bytes ratio,
which is the closest thing to the paper's "periodic merge process" that still
lives inside the handle. The paper's out-of-process `merge(DirName)` also stays
available as `mergeDirectory :: FilePath -> IO ()` for compacting a store while
nothing has it open.

---

## 8. Module layout

```
Database.Bitcask                    -- public API, the only stable module
Database.Bitcask.Raw                -- the same at Bitcask ByteString ByteString
Database.Bitcask.Codec              -- the Codec class, instances, AsBinary
Database.Bitcask.Types              -- FileId, Loc, errors, options
Database.Bitcask.Internal.Record    -- pure: record codec + CRC
Database.Bitcask.Internal.Hint      -- pure: hint codec
Database.Bitcask.Internal.Keydir    -- pure: keydir + replay
Database.Bitcask.Internal.File      -- IO: naming, scanning, rolling
Database.Bitcask.Internal.Lock      -- IO: the store lock
Database.Bitcask.Internal.Merge     -- IO: merge
Database.Bitcask.Internal.Platform  -- IO: per-OS, two implementations (§2)
```

`Internal.*` modules are *exposed*, with a haddock warning that they're outside
the version policy. Hiding internals doesn't protect anyone; it means the one
person who needs a hook has to fork you. `Internal.Platform` is the exception —
it stays `other-modules`, because its type is genuinely different per OS and
nothing portable can be promised about it.

---

## 9. Dependencies

| package                 | why                                      |
|-------------------------|------------------------------------------|
| `bytestring`            | encoded keys and values, `Builder`       |
| `text`                  | `Codec Text`, store tag                  |
| `unordered-containers`  | the keydir                               |
| `hashable`              | comes with it                            |
| `filepath`, `directory` | paths, listing, rename                   |
| `binary`                | `AsBinary` (GHC boot library, free)      |
| `digest`                | CRC-32 (thin zlib binding)               |
| `unix >=2.8`            | **POSIX only** — `fdPread`, `setLock`    |
| `Win32 >=2.13`          | **Windows only** — overlapped reads, locking |

Test-only: `tasty`, `tasty-quickcheck`, `tasty-hunit`, `temporary`, and
`tasty-bench` for benchmarks.

CI must run the full suite on Linux *and* Windows from the first commit that
touches `Internal.Platform`. macOS too, since it exercises the POSIX path with a
different filesystem underneath. A platform layer that only one maintainer can
test is a platform layer that rots.

---

## 10. Testing

Because the codec, the keydir and merge planning are pure, most of this is
property tests with no filesystem and no platform layer:

- `fromBytes . toBytes ≡ Right` for every `Codec` instance, plus an injectivity
  property on a small generated key domain — the law from §5.1, enforced.
- `decodeRecord . encodeRecord ≡ id` over arbitrary keys and values, including
  empty values and tombstones; and flipping any single byte of a record makes
  `decodeRecord` fail.
- **Model test:** run a random program of `put`/`delete`/`get`/`merge` against
  both a real store and a `Data.Map` model and assert they agree. Reopen the
  store partway through and assert it *still* agrees — that's the recovery test,
  and it's the one that has to pass on Windows.
- **Crash test:** truncate a data file at every offset from 0 to its length,
  reopen, and assert the store recovers to some prefix of the write history and
  that no key is *wrong*, as opposed to absent.
- **Concurrency:** N reader threads against a writer and a merge running
  together, asserting every `get` returns the old or the new value and never a
  torn one. This is the test that will find the §2.1 Windows bug if there is one.
- A `SchemaMismatch` test: write with one tag, open with another, expect the
  throw at `open` rather than at the first `get`.

---

## 11. Where I'd start

1. `Internal.Record`, `Internal.Keydir` and `Codec` with their property tests —
   pure, no IO, no platform layer, and it locks the on-disk format down.
2. `Internal.Platform` for POSIX and Windows behind the §2 interface, with a
   direct test suite for the interface itself and CI on both from day one.
3. `open` / `put` / `get` / `close`, no merge and no hint files.
4. Recovery: startup scan, the torn tail, the model test.
5. Hint files, then merge, then the concurrency test.
6. Benchmarks, README, haddocks.

Steps 1 and 2 are independent, which is convenient, because step 2 carries all
the risk and step 1 carries all the format decisions.
