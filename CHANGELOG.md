# Revision history for bitcask

## 0.1.0.0 -- unreleased

* First version: the Bitcask storage model from Sheehy & Smith, with a typed
  `Codec`-based API over `Database.Bitcask`.
* Append-only data files with an in-memory keydir, hint files, tombstone
  deletion, and a merge that runs concurrently with reads and writes.
* Torn-write repair on open; corruption anywhere else is reported.
* POSIX and Windows platform layers. The Windows layer has not yet been
  compiled — see `DESIGN.md`, revision 3, point 7.
* Performance work, measured with the new `bitcask-bench` and
  `bitcask-workload` benchmarks:
  * The record and hint codecs poke fields straight into one exactly-sized
    buffer instead of going through a `Builder`, and CRC-32 uses slicing-by-8.
  * `encodeStrict` starts with a small untrimmed buffer instead of a 4 KiB one
    it then trims.
  * Hint entries are buffered and written in 64 KiB blocks, halving the system
    calls per `put`. `sync` no longer fsyncs the unfinished hint file.
  * The reader-handle cache is read without a lock, so concurrent `get`s no
    longer serialise on an `MVar`.
  * `fold` and `merge` read files in 1 MiB windows instead of one positional
    read per record; `merge` also writes its output in blocks and claims each
    block in the keydir with a single update.
  * The keydir stores keys as `ShortByteString`, so it no longer keeps scan
    buffers, hint files or callers' buffers alive. Opening a store by scanning
    its data files used to keep every data file in memory.
  * Timestamps come from `getSystemTime` rather than a `Rational` round trip.
  * Windows: appends go through a raw `HANDLE` rather than a GHC `Handle`, and
    each data file is read through a small pool of handles, one per
    capability, because Windows serialises I/O on a synchronous handle.
  * The library is built with `-O2`.
* Write safety. A write that failed or was interrupted part-way used to leave
  the store's idea of where the active file ends out of step with the file, so
  that every later write recorded the wrong location and read back as missing.
  Now:
  * The write path runs with asynchronous exceptions masked, so a `timeout` or
    `killThread` leaves a write either done or not done.
  * A failed append is undone by truncating the file back.
  * A failed `fsync`, or an append that cannot be undone, marks the store
    broken: later writes, `sync` and `merge` throw the new `StoreBroken` until
    the store is reopened. Reads are unaffected.
  * A failed hint write costs that file its hint, never the write.
  * Rolling to a new active file opens it before closing the old one, so
    failing to open it leaves the store usable.
  * `close` releases every handle and the lock even when finishing the active
    file fails, and throws the failure afterwards.
  * A merge that fails closes its output and never removes its inputs unless
    the output was synced.
  * Internal fault hooks (`injectFault`) let the test suite exercise each of
    these paths.
