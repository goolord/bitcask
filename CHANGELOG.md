# Revision history for bitcask

## 0.1.0.0 -- unreleased

* First version: the Bitcask storage model from Sheehy & Smith, with a typed
  `Codec`-based API over `Database.Bitcask`.
* Append-only data files with an in-memory keydir, hint files, tombstone
  deletion, and a merge that runs concurrently with reads and writes.
* Torn-write repair on open; corruption anywhere else is reported.
* POSIX and Windows platform layers. The Windows layer has not yet been
  compiled — see `DESIGN.md`, revision 3, point 7.
