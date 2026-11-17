# Transaction file header id Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A committed `.txn` file's header carries its transaction-id, written after the commit lock, and recovery refuses a file whose nonzero header id disagrees with its filename.

**Architecture:** One new function on the commit path, called once after `with-transaction-manager-lock` returns in `%commit`; one new condition; one comparison added to `load-recovery-transaction`. No layout or wire change.

**Tech Stack:** Common Lisp (SBCL primary; CCL and ECL arms untouched), FiveAM, the acid test system `graph-db/acid-test`.

**Spec:** `docs/superpowers/specs/2026-11-04-txn-header-id-design.md`

## Global Constraints

- Lisp lines at most 80 columns, spaces only.
- No file I/O inside `with-transaction-manager-lock` in `%commit` (spec, Goals).
- Header `0` is accepted by recovery (spec §2 table).
- The replication wire and `*peer-protocol-version*` do not change.
- Tests in the acid durability suite run with `tests/run-suite.sh` from the acid system: `CL_SOURCE_REGISTRY="$PWD//:" sbcl --dynamic-space-size 12288 --non-interactive --eval '(ql:quickload :graph-db/acid-test :silent t)' --eval '(asdf:test-system :graph-db/acid-test)'` (the acid suite is a separate system; `tests/run-suite.sh` serves the main suite only). The builder may instead run one suite by loading `:graph-db/acid-test` and `(fiveam:run! 'graph-db/acid-test::acid-durability-suite)` under the bindings `tests/acid/suite.lisp`'s runner makes; read that file first.
- One SBCL at a time per checkout.

## Review Focus

1. A memory graph's retained journal: a file patched after commit must still be read back by the image-plus-journal open (memory-graph.lisp around lines 340 and 1034 select journal entries by filename id); the patch changes bytes 10..17 only. Pinned by Task 2's memory-graph test.
2. The crash window: a crash between the rename and the patch leaves header `0`; recovery must replay it. Pinned by the assertion added to `crash-after-partial-apply` in Task 1.
3. WAL-suppressed graphs (`wal-suppressed-p`): no file exists, the patch must not try to open one. Pinned in Task 2 by a shadow-graph `:fast-load t` commit.
4. `*delete-committed-transaction-files*` nil: the patch precedes the rename to `.committed`. Pinned by Task 2's on-disk test, which reads the `.committed` file.
5. A patch I/O error after the commit point propagates to the caller and the transaction stays committed and applied (spec §1). Pinned in Task 2 by a test that makes the file unwritable before commit on SBCL (`sb-posix:chmod` on `tx/`), skipped on other implementations.

---

### Task 1: The condition and the recovery cross-check

**Files:**
- Modify: `conditions.lisp` (after `transaction-error`)
- Modify: `transactions.lisp` `load-recovery-transaction` (around line 3809)
- Modify: `package.lisp` (export `transaction-file-id-mismatch` beside `transaction-error`)
- Test: `tests/acid/durability-tests.lisp`

**Interfaces:**
- Produces: condition `transaction-file-id-mismatch` (`:file :header-id :filename-id`), signalled by `load-recovery-transaction` when the header id is nonzero and differs from the filename id.

- [ ] **Step 1: Write the failing test**

In `tests/acid/durability-tests.lisp`, in `acid-durability-suite`:

```lisp
(test recovery-refuses-a-renamed-transaction-file
  "A .txn file whose nonzero header id differs from its filename is
tampered with or misplaced; recovery refuses it by name rather than
replaying it under the filename's id (#422)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (close-graph g :snapshot-p nil))
      ;; A hand-made file: header id 7, filename says 9.
      (let* ((tx-dir (merge-pathnames "tx/" path))
             (file (make-pathname :name (format nil "~16,'0X" 9)
                                  :type "txn" :defaults tx-dir))
             (header (graph-db::make-tx-header-vector)))
        (graph-db::serialize-uint64 header graph-db::+tx-header-size+ 0)
        (setf (aref header 9) graph-db::+tx-header-type-code+)
        (graph-db::serialize-uint64 header 7 graph-db::+tx-header-id-offset+)
        ;; zero writes, zero bytes of writes: the rest of the header stays 0
        (with-open-file (s file :direction :output
                                :element-type '(unsigned-byte 8))
          (write-sequence header s))
        (signals graph-db:transaction-file-id-mismatch
          (open-graph *acid-graph-name* path))
        (handler-case (open-graph *acid-graph-name* path)
          (graph-db:transaction-file-id-mismatch (c)
            (is (= 7 (graph-db::header-id c)))
            (is (= 9 (graph-db::filename-id c)))
            (is (search "0000000000000009" (princ-to-string c)))))))))
```

Check the header's first bytes against `serialize-tx-header` before relying on this hand-made layout: size at 0, flags at 8, type code at 9, id at 10, write count at 18, write size at 26. If `open-graph` after a refused open needs the `.dirty` sentinel handled, say so in the ledger and open once inside `handler-case` only.

- [ ] **Step 2: Run it, expect red**

Run the acid durability suite. Expected: FAIL, `transaction-file-id-mismatch` is not a condition (reader error on the symbol, or the open succeeds).

- [ ] **Step 3: Define the condition**

In `conditions.lisp` after `transaction-error`:

```lisp
(define-condition transaction-file-id-mismatch (transaction-error)
  ((file :initarg :file :reader mismatch-file)
   (header-id :initarg :header-id :reader header-id)
   (filename-id :initarg :filename-id :reader filename-id))
  (:report ...))
```

Report: the file's namestring, the header id and the filename id in hex, and the sentence "the file was renamed or does not belong to this store (#422)". Export the condition name from `package.lisp`.

- [ ] **Step 4: The cross-check in `load-recovery-transaction`**

Replace the four-line comment with two lines pointing at the spec and #422. Bind `header-id` to `(transaction-id tx-header)` and `filename-id` to the parsed name; when `header-id` is neither `0` nor `filename-id`, signal the condition with all three; otherwise construct the `recovery-transaction` with `filename-id` as now.

- [ ] **Step 5: Run it, expect green; the whole acid durability suite green**

- [ ] **Step 6: Pin the crash window**

In `crash-after-partial-apply`, in Phase 1 after the `handler-case`, before the close: find the one `.txn` file under `tx/` and assert `(= 0 (graph-db::transaction-id (graph-db::load-tx-file file)))` with the message "a crash inside the lock leaves the header at 0; recovery trusts the filename (#422)". Run the suite: green (the patch does not exist yet, so this is a characterisation of today, and Task 2 keeps it true because its crash fires before the patch).

- [ ] **Step 7: Commit**

```bash
git add conditions.lisp package.lisp transactions.lisp tests/acid/durability-tests.lisp
git commit -m "recovery: a .txn file whose header id disagrees with its filename is refused (#422)"
```

### Task 2: The post-lock patch

**Files:**
- Modify: `transactions.lisp` `%commit` (the form after `with-transaction-manager-lock`, around line 3756) and a new `patch-tx-file-header-id` beside `finalize-tx-persistence`
- Test: `tests/acid/durability-tests.lisp`

**Interfaces:**
- Consumes: `transaction-pathname`, `+tx-header-id-offset+`, `serialize-uint64`, `wal-suppressed-p`, `load-tx-file` (Task 1 unchanged).
- Produces: `patch-tx-file-header-id (transaction)`, returns the id written, or NIL when WAL-suppressed.

- [ ] **Step 1: Write the failing tests**

```lisp
(test a-committed-transaction-file-carries-its-id
  "After commit the .txn file's header id equals its filename's id,
written after the manager lock is released (#422)."
  (with-temp-directory (dir)
    (let ((graph-db::*delete-committed-transaction-files* nil)
          (path (namestring dir)))
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (define-acid-views)
               (with-transaction ()
                 (make-ac-item :value 1 :label "patched"))
               (let ((files (directory (merge-pathnames "tx/*.committed"
                                                        path))))
                 (is (= 1 (length files)))
                 (let* ((file (first files))
                        (header (graph-db::load-tx-file file)))
                   (is (= (parse-integer (pathname-name file) :radix 16)
                          (graph-db::transaction-id header))
                       "header id must equal the filename id"))))
          (close-graph g :snapshot-p nil))))))

(test a-memory-graph-journal-entry-carries-its-id
  "The retained journal of a memory graph is patched too (#422)."
  ...)   ; same shape with make-memory-graph; the journal file is tx/<id>.txn
         ; and stays after commit (retain-committed-transaction-p)

(test a-wal-suppressed-commit-patches-nothing
  "No file, no patch: a :fast-load shadow graph commits without
touching tx/ (#422)."
  ...)   ; open-shadow-graph :fast-load t, one commit, (directory "tx/*") empty,
         ; and no error

(test a-patch-failure-after-the-commit-point-propagates
  "The transaction is committed and applied; the caller still learns the
file could not be patched (#422)."
  #-sbcl (skip "chmod via sb-posix")
  #+sbcl ...)  ; chmod tx/ to #o500 after make-graph; commit inside
               ; handler-case; expect a file-error; then lookup-vertex finds
               ; the vertex; restore #o700 in unwind-protect before close
```

Read `tests/memory-graph-tests.lisp`'s `with-test-memory-graph` and the shadow-graph tests (`rg -n 'open-shadow-graph' tests/`) for the constructors and arguments before writing the second and third tests. If the chmod test cannot be made to fail at the patch (because `%posix-rename` into a read-only directory fails first, under the lock), move the chmod to a `*after-apply-tx-writes-hook*` lambda so it fires after the rename and before the patch, and ledger the change.

- [ ] **Step 2: Run them, expect red**

Expected: the first two fail with header id 0; the third fails because `patch-tx-file-header-id` is undefined only if the test calls it (it does not), so it may pass: that is acceptable for a guard test, note it. The fourth fails because no error is signalled.

- [ ] **Step 3: Implement `patch-tx-file-header-id`**

```lisp
(defun patch-tx-file-header-id (transaction)
  "Write TRANSACTION's id over the header placeholder in its .txn file,
after the manager lock: the rename in FINALIZE-TX-PERSISTENCE is the
commit point, this makes the file self-describing (#422, spec §1).
Returns the id, or NIL when the graph is WAL-suppressed.")
```

Body: `with-open-file` on `(transaction-pathname transaction)`, `:direction :io :if-exists :overwrite :element-type '(unsigned-byte 8)`, `(file-position stream +tx-header-id-offset+)`, `write-sequence` of an 8-byte vector filled by `serialize-uint64`. Call it in `%commit` as `(when renamed (patch-tx-file-header-id tx))` immediately after the `with-transaction-manager-lock` form, before `(setf (state tx) :committed)`. Check `:direction :io` with `:if-exists :overwrite` compiles and behaves on SBCL; it is the portable spelling.

- [ ] **Step 4: Run the tests, expect green; the whole acid suite green**

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/acid/durability-tests.lisp
git commit -m "txn: the .txn header carries its id, written after the commit lock (#422)"
```

### Task 3: Docs

**Files:**
- Modify: `transactions.lisp` (the TODO block above `finalize-tx-persistence`, the `+tx-header-id-offset+` comment, the docstrings of `prepare-tx-persistence` and `finalize-tx-persistence`)
- Modify: `docs/concurrency-scaling-investigation.md` (the placeholder paragraph near line 45)
- Modify: `docs/vivace-graph-v3-doc.org` (the memory-graph journal paragraph near line 3733; the transactions chapter's recovery description, find it with `rg -n 'recover' docs/vivace-graph-v3-doc.org`)
- Modify: `CHANGELOG.md` `[Unreleased]` `### Changed`

- [ ] **Step 1: Source comments**

Delete the TODO block. Replace with:

```lisp
;;; The .txn header id is written after the manager lock
;;; (PATCH-TX-FILE-HEADER-ID); recovery cross-checks it against the
;;; filename (#422, docs/superpowers/specs/2026-11-04-txn-header-id-design.md).
```

Rewrite the `+tx-header-id-offset+` comment and the two docstrings so neither says "placeholder" without saying "until the post-lock patch"; keep each under 80 columns.

- [ ] **Step 2: Docs**

`concurrency-scaling-investigation.md`: the paragraph gains one sentence: the id is patched into the file after the lock and recovery cross-checks it (#422). Manual: the journal paragraph says each entry carries its id and recovery refuses a mismatched file. CHANGELOG Changed entry: what changes for an operator (a renamed `.txn` file now fails `open-graph` with `transaction-file-id-mismatch`; files written by earlier versions have header 0 and are accepted).

- [ ] **Step 3: Verify**

`rg -n 'placeholder' transactions.lisp docs/concurrency-scaling-investigation.md` shows only lines that also mention the patch. `awk 'length > 80' transactions.lisp conditions.lisp` prints nothing new (compare against `git diff --stat` lines only).

- [ ] **Step 4: Commit**

```bash
git add transactions.lisp docs/concurrency-scaling-investigation.md docs/vivace-graph-v3-doc.org CHANGELOG.md
git commit -m "docs: the .txn header id contract, the placeholder TODO retired (#422)"
```

### Completion

- Acid test system green (`asdf:test-system :graph-db/acid-test`), and the main suite's durability-touching suites (`tests/run-suite.sh memory-graph-suite`, `tests/run-suite.sh backup-suite`) green locally. The replication and peer suites are left to CI (push).
- `git diff experiment...tx-header-id-422` is the review command.
