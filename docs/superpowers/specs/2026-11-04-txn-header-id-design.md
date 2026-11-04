# The transaction file's header carries its id (#422)

**Status:** design, awaiting Kevin's approval (2026-11-04).
**Issue:** #422. **Branch:** `tx-header-id-422` off `experiment`.

## Intent

A `.txn` file in a store's `tx/` directory is the write-ahead record
recovery replays after a crash, and a memory graph's whole durable
journal. Today its header holds a placeholder `0` where the
transaction-id belongs, and the only copy of the id is the filename
(`~16,'0X`.txn). That was the price of relieving the commit-lock
convoy: the id is assigned under the transaction-manager lock, the
bulk write happens before the lock, and the one piece of file I/O left
inside the lock is a rename to the id-keyed name
(`docs/concurrency-scaling-investigation.md`).

Kevin's objection, 2026-11-04: a filename is far easier to change than
a file's contents. A `.txn` file renamed by a person or a tool today
replays under the renamed id and nothing notices.

The design: write the real id into the file **after** the lock is
released, in the same step that already patches it into the in-memory
bytes for the replication feed, and have recovery **cross-check** a
nonzero header id against the filename. The rename stays the commit
point; nothing new happens under the lock.

## Goals

- A committed `.txn` file's header id equals its filename's id, for
  every graph kind that writes one (on-disk, memory, master, peer).
- Recovery refuses a file whose nonzero header id differs from its
  filename, with an error that names the file and both ids.
- A header of `0` is still accepted and the filename trusted: it is
  the window between the rename (the commit point) and the patch, and
  a crash can land there.
- No file I/O is added inside `with-transaction-manager-lock`.

## Non-goals

- Changing the header layout or the replication wire. The 8 bytes at
  `+tx-header-id-offset+` already exist; only their value in a `.txn`
  file changes from `0` to the id. Peers and slaves never read `.txn`
  files (they read the feed's patched bytes), so
  `*peer-protocol-version*` does not move.
- Making the filename non-authoritative for ordering. Recovery still
  sorts and replays by filename; the header is the check, not the
  key.
- An fsync of the patch. `.txn` files are not fsynced today (prepare
  relies on the flush at close); the patch inherits that and this
  design does not change the durability contract.
- Assigning ids before the lock. Ids are commit order; prepare order
  is not.

## 1. The patch

`%commit` (transactions.lisp) gains one step between the
`with-transaction-manager-lock` form returning and
`(setf (state tx) :committed)`:

```lisp
(when renamed (patch-tx-file-header-id tx))
```

`patch-tx-file-header-id (transaction)`: open
`(transaction-pathname transaction)` for byte I/O with
`:if-exists :overwrite`, set the position to
`+tx-header-id-offset+`, write the 8-byte `serialize-uint64` of
`(transaction-id transaction)`, close. Nothing else in the file moves.
Skipped when the graph is WAL-suppressed (no file was written; the
same slot test `finalize-tx-persistence` makes). Runs on the
committing thread, so CCL's thread-private streams are not a concern.

Ordering: the patch runs before `cleanup-transaction` deletes or
renames the file to `.committed` (on-disk graphs), and before anything
can read a memory graph's journal entry back, since both happen after
`%commit` returns.

A failure in the patch (disk full, permissions) is signalled as the
commit's error after the commit point. The transaction is durable and
applied; the caller learns the file could not be patched. **Call I
made:** not swallowing it. A silently unpatched file is exactly the
state the cross-check exists to make visible, and an I/O error on the
store's own directory right after a successful rename is not a
condition to paper over.

## 2. The cross-check

`load-recovery-transaction (file)` reads the header it already parses
(`load-tx-file` returns a `tx-header` with `transaction-id`). Rule:

| header id | filename id | result |
|-----------|-------------|--------|
| 0 | n | replay as n (today's behaviour; the crash window) |
| n | n | replay as n |
| m ≠ 0 | n ≠ m | signal `transaction-file-id-mismatch` |

`transaction-file-id-mismatch` is a new condition in `conditions.lisp`,
a subclass of `transaction-error`, with slots `file`, `header-id`,
`filename-id`, and a report naming all three. It propagates out of
`recover-transactions`, so `open-graph` fails loudly on a tampered
`tx/` directory instead of replaying the wrong id. Nothing is
replayed from that file; the files before it in filename order have
already been applied, which is the same partial state any error in
recovery leaves today.

The restore path (`transaction-restore.lisp`) does not go through
`load-recovery-transaction`; it is unchanged.

## 3. What this leaves

- Masters and peers keep patching the id into `(bytes transaction)` for
  the feed exactly as now; the file patch is a second write of the same
  8 bytes to a different place.
- `*delete-committed-transaction-files*` nil keeps `.committed` files;
  these are patched too (the patch precedes `mark-as-committed`).
- The crash window is observable: the acid durability test
  `crash-after-partial-apply` already crashes inside the lock after the
  rename and leaves a `.txn` with header `0`; it stays green and gains
  an assertion that the header is `0`, pinning what the window looks
  like.

## 4. Docs

- The TODO block above `finalize-tx-persistence` goes; a two-line
  comment points here and at #422.
- Docstrings of `prepare-tx-persistence`, `finalize-tx-persistence`,
  the `+tx-header-id-offset+` comment, and the comment in
  `load-recovery-transaction` say the new contract.
- `docs/concurrency-scaling-investigation.md` (the placeholder
  paragraph), the manual's memory-graph journal paragraph and the
  recovery description, CHANGELOG `[Unreleased]` Changed.

## Acceptance

1. After a commit on an on-disk graph with
   `*delete-committed-transaction-files*` bound to nil, the
   `.committed` file's header id, read with `load-tx-file`, equals
   the id parsed from its name.
2. The same on a memory graph's retained journal file.
3. A `.txn` file whose header id is nonzero and differs from its
   filename makes `open-graph` signal `transaction-file-id-mismatch`
   naming the file and both ids.
4. `crash-after-partial-apply` stays green, and asserts the leftover
   file's header id is `0`.
5. No new form inside `with-transaction-manager-lock` in `%commit`
   (reviewed, not measured: the convoy bench lives in the perf suite
   and is not gating).
6. Replication, peer and restore suites unchanged and green.

## Calls I made

- Header `0` stays accepted rather than becoming an error after a
  grace period: the window is real and a crash in it must recover.
- The mismatch is an error, not a warning that trusts the filename:
  trusting either id silently is the defect Kevin named.
- The patch error propagates (see §1).

## Left to the builder

- Whether `patch-tx-file-header-id` reuses an 8-byte scratch vector or
  allocates one per commit; either is fine at this size.
- The exact wording of the condition's report.
