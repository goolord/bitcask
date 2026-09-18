# Revision history for bitcask

## 0.1.0.0 -- unreleased

* First version. Bitcask from Sheehy & Smith, with a typed `Codec` API in
  `Database.Bitcask`.
* Append-only data files with an in-memory keydir, hint files, tombstone
  deletion, and a merge that runs concurrently with reads and writes.
* Torn-write repair on open; other corruption throws.
* POSIX and Windows platform layers. The Windows layer hasn't been compiled
  yet.
* Performance, measured with the new `bitcask-bench` and `bitcask-workload`:
  * Record and hint codecs poke fields into one exact-size buffer instead of
    using a `Builder`. CRC-32 uses slicing-by-8.
  * `encodeStrict` starts with a small untrimmed buffer instead of trimming a
    4 KiB one.
  * Hint entries are written in 64 KiB blocks, halving syscalls per `put`.
    `sync` no longer fsyncs the unfinished hint file.
  * The reader-handle cache is lock-free for lookups, so concurrent `get`s
    don't contend on an `MVar`.
  * `fold` and `merge` read in 1 MiB windows instead of one pread per record.
    `merge` also writes in blocks and updates the keydir once per block.
  * Keydir keys are `ShortByteString`, so they don't keep scan buffers, hint
    files or caller buffers alive. Scanning on open used to keep every data
    file in memory.
  * Timestamps use `getSystemTime` instead of going through `Rational`.
  * Windows: appends use a raw `HANDLE` instead of a GHC `Handle`, and reads
    use one handle per capability since Windows serialises I/O on a
    synchronous handle.
  * Built with `-O2`.
* Write safety. A failed or interrupted write could leave the store's offset
  out of sync with the file, so later writes recorded the wrong location and
  read back as missing. Now:
  * Async exceptions are masked in the write path, so `timeout` or
    `killThread` leaves a write either done or not.
  * A failed append is truncated back.
  * A failed `fsync`, or an append that can't be undone, marks the store
    broken. Writes, `sync` and `merge` then throw `StoreBroken` until reopen.
    Reads still work.
  * A failed hint write loses the hint, not the write.
  * Rolling opens the new active file before closing the old one, so a failed
    open leaves the store usable.
  * `close` releases all handles and the lock even if finishing the active
    file fails, then throws.
  * A failed merge closes its output and only removes inputs once the output
    is synced.
  * Fault hooks (`injectFault`) for testing these paths.
