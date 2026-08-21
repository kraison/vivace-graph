# Image-Level Clock Implementation Plan (#168)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give an image one snapshot clock, so a query spanning stores resolves at one
instant, without changing the behaviour of any existing consumer.

**Architecture:** A new `SYSTEM-CLOCK` object owns a durable 64-bit epoch counter and an
append-only lifecycle journal, both in one directory. `*SYSTEM-CLOCK*` is NIL by default,
in which case every store keeps its own counter and behaviour is byte-for-byte what it is
today. When a clock is bound, a store attaching to it raises the clock above that store's
own history (this *is* the watermark, computed incrementally rather than as a migration
step) and all epoch allocation routes through the clock.

**Tech Stack:** SBCL 2.6.6, ASDF, FiveAM. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-08-20-namespaces-design.md` §6 and §9.1.
Decision log: `docs/namespace-design-decisions-2026-08-20.md` D9.

## Global Constraints

- **Lisp style: spaces only, never tabs. Hard limit 80 columns** — code, comments,
  docstrings and strings alike. A 96-column line is a defect.
- **Comments are terse and point elsewhere.** State the non-obvious fact in a line or
  two and reference the spec section or issue. Do not narrate reasoning in source.
- **`*SYSTEM-CLOCK*` NIL must preserve today's behaviour exactly.** This is the
  backward-compatibility hinge: every existing consumer and all 3684 core checks run with
  it unbound. Any task that changes behaviour in the NIL case is wrong.
- **Epochs are 64-bit and never reused.** An id issued must never be issued again, across
  crashes.
- **Baseline to preserve:** `graph-db/test` 3684 checks (3674 pass, 10 skip, 0 fail),
  `graph-db/spacetime-test` 342, `graph-db/geos-test` 185. Verified green on this branch
  at `6a7623a`.
- **Run tests from this worktree, not the main checkout.** `graph-db.asd` is symlinked
  into `~/quicklisp/local-projects` from the main checkout, so a naive load silently tests
  the wrong tree. Use the runner in `docs/superpowers/plans/` §Testing below, which
  asserts the resolved source directory.

---

## File Structure

| File | Responsibility |
|---|---|
| `system-clock.lisp` (new) | The clock: durable counter, block reservation, leases, journal. No graph dependency. |
| `graph-db.asd` | Register `system-clock` before `transactions`; register the new test file. |
| `transactions.lisp` | Route epoch allocation through the clock when one is bound. |
| `transaction-restore.lisp` | Stop minting ids from a per-store scalar (the audit finding). |
| `graph.lisp` | `:system-clock` keyword on `make-graph` / `open-graph`; attach on open. |
| `tests/system-clock-tests.lisp` (new) | The clock in isolation, then wired to graphs. |

`system-clock.lisp` depends only on `utilities` (locks), `serialize` (uint64 codecs) and
`posix`. It must **not** depend on `graph`, so it can be tested without one.

---

### Task 1: The clock object — durable, monotonic, crash-safe

**Files:**
- Create: `system-clock.lisp`
- Modify: `graph-db.asd` (add component after `"serialize"`, before `"graph-class"`)
- Test: `tests/system-clock-tests.lisp`

**Interfaces:**
- Produces:
  - `(open-system-clock location &key (block-size 4096)) => system-clock`
  - `(close-system-clock clock) => clock`
  - `(clock-next-epoch clock) => (unsigned-byte 64)` — allocates and returns a fresh epoch
  - `(clock-current-epoch clock) => (unsigned-byte 64)` — next id that would be issued
  - `(clock-observe-epoch clock epoch) => (unsigned-byte 64)` — Lamport max, idempotent
  - `(clock-lease-epochs clock n) => (values start end)` — reserves `[start, end)`
  - `*system-clock*` — special, initform NIL

- [ ] **Step 1: Write the failing tests**

```lisp
;;;; The image-level epoch clock (GH #168).  See
;;;; docs/superpowers/specs/2026-08-20-namespaces-design.md §6.
(in-package #:graph-db/test)

(def-suite system-clock-suite :in graph-db-suite
  :description "The image-level epoch clock and its journal.")
(in-suite system-clock-suite)

(test clock-issues-monotonic-epochs
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((a (clock-next-epoch c))
                 (b (clock-next-epoch c))
                 (d (clock-next-epoch c)))
             (is (< a b d)))
        (close-system-clock c)))))

(test clock-current-epoch-is-the-next-id
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((seen (clock-current-epoch c)))
             (is (= seen (clock-next-epoch c)))
             (is (= (1+ seen) (clock-current-epoch c))))
        (close-system-clock c)))))

(test clock-observe-epoch-is-a-max
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (clock-observe-epoch c 5000)
             (is (> (clock-next-epoch c) 5000))
             ;; A lower observation must not move it backwards.
             (let ((before (clock-current-epoch c)))
               (clock-observe-epoch c 10)
               (is (= before (clock-current-epoch c)))))
        (close-system-clock c)))))

(test clock-survives-clean-reopen-without-reissuing
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir)))
           (last (progn (dotimes (i 10) (clock-next-epoch c))
                        (clock-next-epoch c))))
      (close-system-clock c)
      (let ((c2 (open-system-clock (namestring dir))))
        (unwind-protect
             (is (> (clock-next-epoch c2) last))
          (close-system-clock c2))))))

(test clock-survives-crash-without-reissuing
  ;; No CLOSE-SYSTEM-CLOCK: simulates a crash after ids were handed out.  The
  ;; block reservation on disk must already dominate every issued id.
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir) :block-size 8))
           (issued (loop repeat 5 collect (clock-next-epoch c)))
           (highest (reduce #'max issued)))
      (let ((c2 (open-system-clock (namestring dir) :block-size 8)))
        (unwind-protect
             (is (> (clock-next-epoch c2) highest))
          (close-system-clock c2))))))

(test clock-lease-is-disjoint-and-advances-the-clock
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (multiple-value-bind (start end) (clock-lease-epochs c 1000)
             (is (= 1000 (- end start)))
             ;; The clock has skipped the whole lease: nothing it issues now
             ;; can collide with an id the lease holder allocates.
             (is (>= (clock-next-epoch c) end)))
        (close-system-clock c)))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: see §Testing. Expected: FAIL — `OPEN-SYSTEM-CLOCK` is undefined.

- [ ] **Step 3: Implement the clock**

```lisp
(in-package :graph-db)

;;; The image-level epoch clock (GH #168).  One clock per *system* -- a
;;; directory of stores -- rather than one per store, so a query spanning
;;; stores resolves at one instant.  See
;;; docs/superpowers/specs/2026-08-20-namespaces-design.md §6.

(defvar *system-clock* nil
  "The image's SYSTEM-CLOCK, or NIL.  NIL means every store keeps its own
transaction-id counter, which is the pre-#168 behaviour and the default.")

(defstruct (system-clock (:constructor %make-system-clock))
  (location nil)
  ;; Next epoch to hand out.  In memory; CEILING is what disk guarantees.
  (counter 0 :type (unsigned-byte 64))
  ;; Disk holds this value; every issued id is strictly below it.  A crash
  ;; therefore cannot reissue: reopen resumes at the persisted ceiling.
  (ceiling 0 :type (unsigned-byte 64))
  (block-size 4096 :type (unsigned-byte 32))
  (lock (make-recursive-lock "system clock"))
  (journal nil))

(defun %clock-counter-file (location)
  (make-pathname :name "system-clock" :type "dat" :defaults location))

(defun %write-clock-ceiling (clock value)
  "Persist VALUE as the durable ceiling.  Every id issued is < VALUE."
  (let ((buf (make-byte-vector 8)))
    (serialize-uint64 buf value 0)
    (with-open-file (s (%clock-counter-file (system-clock-location clock))
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (write-sequence buf s)
      (finish-output s)))
  (setf (system-clock-ceiling clock) value))

(defun %read-clock-ceiling (location)
  (let ((file (%clock-counter-file location))
        (buf (make-byte-vector 8)))
    (if (probe-file file)
        (with-open-file (s file :direction :input
                                :element-type '(unsigned-byte 8))
          (unless (= 8 (read-sequence buf s))
            (error "Short read from ~A" file))
          (deserialize-uint64 buf 0))
        0)))

(defun open-system-clock (location &key (block-size 4096))
  "Open or create the system clock in directory LOCATION.  Ids resume above
the persisted ceiling, so a crash never reissues one."
  (ensure-directories-exist location)
  (let* ((ceiling (%read-clock-ceiling location))
         (clock (%make-system-clock :location location
                                    :counter ceiling
                                    :ceiling ceiling
                                    :block-size block-size)))
    (%write-clock-ceiling clock (+ ceiling block-size))
    (setf (system-clock-counter clock) ceiling)
    clock))

(defun close-system-clock (clock)
  "Persist the exact counter so a clean reopen wastes no ids."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%write-clock-ceiling clock (system-clock-counter clock))
    (when (system-clock-journal clock)
      (close (system-clock-journal clock))
      (setf (system-clock-journal clock) nil)))
  clock)

(defun %clock-reserve (clock needed)
  "Raise the durable ceiling so COUNTER + NEEDED stays below it.  Caller
holds the lock."
  (let ((target (+ (system-clock-counter clock) needed)))
    (when (>= target (system-clock-ceiling clock))
      (%write-clock-ceiling
       clock (+ target (system-clock-block-size clock))))))

(defun clock-next-epoch (clock)
  "Allocate and return a fresh epoch."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%clock-reserve clock 1)
    (prog1 (system-clock-counter clock)
      (incf (system-clock-counter clock)))))

(defun clock-current-epoch (clock)
  "The next epoch CLOCK-NEXT-EPOCH would return."
  (with-recursive-lock-held ((system-clock-lock clock))
    (system-clock-counter clock)))

(defun clock-observe-epoch (clock epoch)
  "Raise CLOCK so it strictly exceeds EPOCH.  Monotonic and idempotent; a
lower EPOCH is a no-op.  Foreign epochs reach here from peer sync, so the
clock is not purely local -- see spec §6."
  (with-recursive-lock-held ((system-clock-lock clock))
    (when (and epoch (>= epoch (system-clock-counter clock)))
      (setf (system-clock-counter clock) (1+ epoch))
      (%clock-reserve clock 0))
    (system-clock-counter clock)))

(defun clock-lease-epochs (clock n)
  "Reserve N epochs for a detached store and skip the clock past them.
Returns (values START END); the holder allocates in [START, END)."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%clock-reserve clock n)
    (let* ((start (system-clock-counter clock))
           (end (+ start n)))
      (setf (system-clock-counter clock) end)
      (%clock-reserve clock 0)
      (values start end))))
```

Add to `graph-db.asd`, in `graph-db/core` components, after `"serialize"`:

```lisp
               ;; The image-level epoch clock (GH #168).  No graph dependency,
               ;; so it loads early and tests without one.
               (:file "system-clock" :depends-on ("serialize" "utilities"))
```

**⚠ `graph-db/test` does NOT `:use` `#:graph-db`.** It uses only `#:cl #:fiveam` plus an
explicit `:import-from` list, so a new symbol is invisible in test code until it is added
there. Export from `package.lisp` **and** import into `tests/package.lisp`.

Export from `package.lisp`:

```lisp
           #:*system-clock*
           #:system-clock
           #:open-system-clock
           #:close-system-clock
           #:clock-next-epoch
           #:clock-current-epoch
           #:clock-observe-epoch
           #:clock-lease-epochs
```

Add the same names to the `:import-from #:graph-db` list in `tests/package.lisp`, in a
block commented `;; image-level epoch clock (GH #168)`.

Register the test file in `graph-db.asd` under `graph-db/test`, after `"mvcc-tests"`:

```lisp
               (:file "system-clock-tests")       ; GH #168
```

- [ ] **Step 4: Run the tests to verify they pass**

Expected: 6 new checks pass; the three suites stay at their baseline counts plus these.

- [ ] **Step 5: Commit**

```bash
git add system-clock.lisp tests/system-clock-tests.lisp graph-db.asd package.lisp
git commit -m "feat(clock): durable image-level epoch clock with leases (#168)"
```

---

### Task 2: The lifecycle journal

**Files:**
- Modify: `system-clock.lisp`
- Test: `tests/system-clock-tests.lisp`

**Interfaces:**
- Consumes: `system-clock` from Task 1.
- Produces:
  - `(journal-append clock kind &rest plist) => plist` — appends one record
  - `(journal-records clock) => list of plists`, oldest first

Records are read with `*READ-EVAL*` NIL. Units 5 and 6 (#170, #171) consume this; it
lands here because §6 requires the object regardless.

- [ ] **Step 1: Write the failing tests**

```lisp
(test journal-appends-and-reads-back
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (journal-append c :detach :store :alpha :lease-start 10 :lease-end 20)
             (journal-append c :attach :store :alpha)
             (let ((rs (journal-records c)))
               (is (= 2 (length rs)))
               (is (eq :detach (getf (first rs) :kind)))
               (is (eq :alpha (getf (first rs) :store)))
               (is (eq :attach (getf (second rs) :kind)))))
        (close-system-clock c)))))

(test journal-survives-reopen
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :beta)
      (close-system-clock c))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (is (equal '(:create) (mapcar (lambda (r) (getf r :kind))
                                         (journal-records c2))))
        (close-system-clock c2)))))

(test journal-refuses-to-evaluate-on-read
  ;; A journal is data.  Reading it must never evaluate.
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "(:kind :bogus :value #.(error \"evaluated\"))~%"))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (signals error (journal-records c2))
        (close-system-clock c2)))))
```

- [ ] **Step 2: Run to verify they fail**

Expected: FAIL — `JOURNAL-APPEND` is undefined.

- [ ] **Step 3: Implement**

```lisp
(defun %clock-journal-file (location)
  (make-pathname :name "system-journal" :type "log" :defaults location))

(defun journal-append (clock kind &rest plist)
  "Append one lifecycle record.  KIND is :CREATE :DETACH :SWAP :ATTACH or
:RETIRE.  Consumed by #170 and #171."
  (let ((record (list* :kind kind :epoch (clock-current-epoch clock) plist)))
    (with-recursive-lock-held ((system-clock-lock clock))
      (unless (system-clock-journal clock)
        (setf (system-clock-journal clock)
              (open (%clock-journal-file (system-clock-location clock))
                    :direction :output
                    :if-exists :append
                    :if-does-not-exist :create)))
      (let ((s (system-clock-journal clock)))
        (let ((*print-readably* nil) (*print-pretty* nil))
          (format s "~S~%" record))
        (finish-output s)))
    record))

(defun journal-records (clock)
  "Every lifecycle record, oldest first.  Read with evaluation disabled --
the journal is data and must never execute."
  (let ((file (%clock-journal-file (system-clock-location clock))))
    (when (system-clock-journal clock)
      (finish-output (system-clock-journal clock)))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (let ((*read-eval* nil))
          (loop for r = (read s nil :eof)
                until (eq r :eof)
                collect r))))))
```

Export `#:journal-append` and `#:journal-records` from `package.lisp`, and add both to
the `:import-from` list in `tests/package.lisp`.

- [ ] **Step 4: Run to verify they pass**

- [ ] **Step 5: Commit**

```bash
git add system-clock.lisp tests/system-clock-tests.lisp package.lisp
git commit -m "feat(clock): append-only lifecycle journal, read with *read-eval* nil (#168)"
```

---

### Task 3: Route epoch allocation through the clock

**Files:**
- Modify: `transactions.lisp` (the counter reads and the commit-time assignment)
- Modify: `graph.lisp` (`make-graph` / `open-graph` gain `:system-clock`)
- Test: `tests/system-clock-tests.lisp`

**Interfaces:**
- Consumes: Task 1's clock API **and Task 2's `JOURNAL-APPEND`** (used by
  `ATTACH-TO-SYSTEM-CLOCK` below).
- Produces:
  - `(tm-next-epoch transaction-manager) => (unsigned-byte 64)`
  - `(tm-current-epoch transaction-manager) => (unsigned-byte 64)`
  - `(attach-to-system-clock graph clock)` — raises the clock above this store's
    persisted highest id. This *is* the watermark of spec §6, applied per store at
    attach rather than as a separate migration pass.
  - `graph` gains a `system-clock` slot, initform NIL.

**⚠ The NIL case must not change.** With no clock, `TM-NEXT-EPOCH` must be exactly the
old `(prog1 counter (incf counter))`.

**Note for the reviewer:** `start-tx-id` and `finish-tx-id` widen numerically under a
shared clock, because other stores consume epochs inside the window. That is expected and
harmless: `OVERLAPPING-TRANSACTIONS` filters *this store's* committed set by comparison,
so a wider numeric window covers the same transactions. It is not a correctness change.

- [ ] **Step 1: Write the failing tests**

```lisp
(test two-stores-on-one-clock-get-disjoint-ordered-epochs
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let ((ga (make-graph :sc-alpha (namestring da)
                                     :buffer-pool-size 1000
                                     :system-clock clock))
                     (gb (make-graph :sc-beta (namestring db)
                                     :buffer-pool-size 1000
                                     :system-clock clock)))
                 (unwind-protect
                      (let ((ids '()))
                        (dotimes (i 3)
                          (push (transaction-id
                                 (with-transaction (:graph ga)
                                   *transaction*))
                                ids)
                          (push (transaction-id
                                 (with-transaction (:graph gb)
                                   *transaction*))
                                ids))
                        (let ((sorted (sort (copy-list ids) #'<)))
                          ;; No two transactions anywhere share an epoch.
                          (is (= (length sorted)
                                 (length (remove-duplicates sorted))))))
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock)))))

(test no-clock-means-per-store-counters-unchanged
  ;; The backward-compatibility hinge: with *SYSTEM-CLOCK* nil and no
  ;; :SYSTEM-CLOCK argument, two graphs allocate independently, exactly as
  ;; before #168 -- so both start low and their ids DO collide.
  (with-temp-directory (da)
    (with-temp-directory (db)
      (let ((ga (make-graph :sc-gamma (namestring da) :buffer-pool-size 1000))
            (gb (make-graph :sc-delta (namestring db) :buffer-pool-size 1000)))
        (unwind-protect
             (let ((ia (transaction-id (with-transaction (:graph ga)
                                         *transaction*)))
                   (ib (transaction-id (with-transaction (:graph gb)
                                         *transaction*))))
               (is (= ia ib)))
          (close-graph ga)
          (close-graph gb))))))

(test attaching-a-store-raises-the-clock-above-its-history
  ;; The watermark: a store with existing history must not hand the clock a
  ;; reason to reissue an epoch that store already used.
  (with-temp-directory (cdir)
    (with-temp-directory (gdir)
      (let ((g (make-graph :sc-eps (namestring gdir) :buffer-pool-size 1000)))
        (dotimes (i 5) (with-transaction (:graph g) t))
        (let ((highest (load-highest-transaction-id g)))
          (close-graph g)
          (let ((clock (open-system-clock (namestring cdir))))
            (unwind-protect
                 (let ((g2 (open-graph :sc-eps (namestring gdir)
                                       :buffer-pool-size 1000
                                       :system-clock clock)))
                   (unwind-protect
                        (is (> (clock-current-epoch clock) highest))
                     (close-graph g2)))
              (close-system-clock clock))))))))
```

- [ ] **Step 2: Run to verify they fail**

Expected: FAIL — `MAKE-GRAPH` does not accept `:SYSTEM-CLOCK`.

- [ ] **Step 3: Implement**

In `graph-class.lisp`, add to `defclass graph`:

```lisp
   ;; The image-level epoch clock (GH #168), or NIL for this store's own
   ;; counter.  NIL is the pre-#168 behaviour and the default.
   (system-clock :accessor graph-system-clock :initarg :system-clock
                 :initform nil)
```

In `transactions.lisp`, add the two helpers:

```lisp
(defun tm-next-epoch (transaction-manager)
  "Allocate a fresh epoch: from the image clock when the graph has one,
otherwise from this manager's own counter (pre-#168 behaviour)."
  (let ((clock (and (slot-boundp transaction-manager 'graph)
                    (graph-system-clock (graph transaction-manager)))))
    (if clock
        (clock-next-epoch clock)
        (prog1 (tx-id-counter transaction-manager)
          (incf (tx-id-counter transaction-manager))))))

(defun tm-current-epoch (transaction-manager)
  "The next epoch TM-NEXT-EPOCH would return."
  (let ((clock (and (slot-boundp transaction-manager 'graph)
                    (graph-system-clock (graph transaction-manager)))))
    (if clock
        (clock-current-epoch clock)
        (tx-id-counter transaction-manager))))
```

**Do NOT touch `assign-transaction-id`.** It has zero callers — the live assignment is
`transactions.lisp:3075`. Editing a dead generic adds diff noise and implies it is the
live path; a separate issue tracks removing it.

Replace the three counter reads with `TM-CURRENT-EPOCH`:

- `transactions.lisp:644` — `(let ((epoch (tx-id-counter transaction-manager)))`
- `transactions.lisp:2944` — `:start-tx-id (tx-id-counter transaction-manager)`
- `transactions.lisp:3049` — `(setf (finish-tx-id tx) (tx-id-counter tm))`

And the commit-time assignment at `transactions.lisp:3075-3076`:

```lisp
               (setf (transaction-id tx) (tm-next-epoch tm))
```

Add the attach helper:

```lisp
(defun attach-to-system-clock (graph clock)
  "Raise CLOCK above GRAPH's persisted highest id and record the attach.
This is the spec §6 watermark, applied per store at attach rather than as a
separate migration pass."
  (setf (graph-system-clock graph) clock)
  (clock-observe-epoch clock (load-highest-transaction-id graph))
  (journal-append clock :attach :store (graph-name graph))
  graph)
```

In `graph.lisp`, add `system-clock` to both lambda lists (defaulting to `*system-clock*`)
and call `attach-to-system-clock` after the transaction manager exists in each.

- [ ] **Step 4: Run to verify they pass**

Also run the full three suites: the NIL path must leave 3684 / 342 / 185 unchanged.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp graph.lisp graph-class.lisp tests/system-clock-tests.lisp
git commit -m "feat(clock): route epoch allocation through the image clock (#168)"
```

---

### Task 4: Stop `recreate-graph` minting its own ids

**Files:**
- Modify: `transaction-restore.lisp:133-152`
- Test: `tests/system-clock-tests.lisp`

**Interfaces:** Consumes `TM-NEXT-EPOCH` from Task 3.

**This is the audit finding, and it has no test today** — which is how it survived. Write
the failing test first.

`recreate-graph` (`transaction-restore.lisp:116`) mints ids with `(incf tx-id)` seeded from
`(load-highest-transaction-id graph)`, a *per-store* scalar, bypassing the transaction
manager entirely. Under a shared clock it allocates **below** the global counter, so a
restore mints epochs another store already committed at.

It is reached by two routes — `replay` (`txn-log.lisp:47`) and the restore-from-backup path
(`backup.lisp:290`) — so fixing it once covers both. The test drives `SNAPSHOT` + `REPLAY`,
the real path, rather than calling the internal directly.

- [ ] **Step 1: Write the failing test**

```lisp
(test recreate-graph-allocates-from-the-image-clock
  ;; The audit finding (GH #168): RECREATE-GRAPH minted ids from a per-store
  ;; scalar, so under a shared clock it reissued epochs another store had
  ;; already used.  Drives the real path: SNAPSHOT then REPLAY.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (sdir)
             (with-temp-directory (odir)
               ;; Source store: a little history, then a snapshot on disk.
               (let ((src (make-graph :sc-src (namestring sdir)
                                      :buffer-pool-size 1000
                                      :system-clock clock)))
                 (dotimes (i 3) (with-transaction (:graph src) t))
                 (graph-db::snapshot src)
                 (close-graph src))
               ;; A second store burns epochs, pushing the clock far past
               ;; anything the restore target's own scalar knows about.
               (let ((other (make-graph :sc-other (namestring odir)
                                        :buffer-pool-size 1000
                                        :system-clock clock)))
                 (dotimes (i 50) (with-transaction (:graph other) t))
                 (let ((floor-epoch (clock-current-epoch clock)))
                   (with-temp-directory (rdir)
                     (let ((dst (make-graph :sc-dst (namestring rdir)
                                            :buffer-pool-size 1000
                                            :system-clock clock)))
                       (graph-db::replay
                        dst
                        (graph-db::persistent-transaction-directory
                         (lookup-graph :sc-src))
                        "GRAPH-DB/TEST")
                       ;; Every id the replay issued sits above the clock's
                       ;; position when it started.
                       (is (>= (graph-db::load-highest-transaction-id dst)
                               floor-epoch))
                       (close-graph dst))))
                 (close-graph other))))
        (close-system-clock clock)))))
```

**Note for the implementer:** `SNAPSHOT`, `REPLAY`, `LOAD-HIGHEST-TRANSACTION-ID` and
`PERSISTENT-TRANSACTION-DIRECTORY` are internals — reach them with `graph-db::`, matching
`tests/backup-tests.lisp`. If threading the source graph's txn-log directory to the
replay proves awkward, copying the `snap-*` file into the destination's txn-log directory
and replaying from there is an acceptable equivalent; what the test must pin is that the
ids issued during replay are `>= floor-epoch`.

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — the restored ids start near 0, well below `floor-epoch`.

- [ ] **Step 3: Implement**

In `transaction-restore.lisp`, inside `RECREATE-GRAPH`, replace the local `tx-id`
allocator:

```lisp
        (tx-id nil)
```

and inside `do-snapshot-sexps`:

```lisp
      ;; Allocate through the transaction manager, not a per-store scalar:
      ;; under an image clock (GH #168) a local counter reissues epochs
      ;; another store already used.
      (let* ((tm (transaction-manager graph))
             (*transaction* (make-instance 'restore-transaction
                                           :transaction-id
                                           (setf tx-id (tm-next-epoch tm)))))
```

and the trailing persist:

```lisp
    (when tx-id
      (persist-highest-transaction-id tx-id graph))
```

- [ ] **Step 4: Run to verify it passes**

Then the full suites, including the NIL path — `recreate-graph` with no clock must still
produce ascending ids from the store's own counter. `backup-tests` and any replay test are
the existing coverage of that branch.

- [ ] **Step 5: Commit**

```bash
git add transaction-restore.lisp tests/system-clock-tests.lisp
git commit -m "fix(clock): recreate-graph allocates from the manager (#168)"
```

---

### Task 5: `peer-observe-epoch` observes the image clock

**Files:**
- Modify: `transactions.lisp:2413-2429`
- Test: `tests/system-clock-tests.lisp`

**Interfaces:** Consumes Task 1's `CLOCK-OBSERVE-EPOCH`.

A pulled node carries a *foreign* image's epoch. With a clock bound, the observation must
raise the clock, not this store's dead counter — otherwise a later local edit opens below
the pulled node's epoch and MVCC hides it, which is the exact bug the function exists to
prevent.

- [ ] **Step 1: Write the failing test**

```lisp
(test peer-observe-epoch-raises-the-image-clock
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (gdir)
             (let ((g (make-graph :sc-peer (namestring gdir)
                                  :buffer-pool-size 1000
                                  :peer-role :device
                                  :system-clock clock)))
               (unwind-protect
                    (progn
                      (peer-observe-epoch g 999999)
                      (is (> (clock-current-epoch clock) 999999)))
                 (close-graph g))))
        (close-system-clock clock)))))
```

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — the clock is untouched; only `tx-id-counter` moved.

- [ ] **Step 3: Implement**

```lisp
  (when (and epoch (> epoch 0) (typep graph 'peer-graph))
    (let ((clock (graph-system-clock graph)))
      (if clock
          ;; A pulled node carries the HUB's epoch -- a foreign image's clock.
          ;; So the image clock is not purely local; see spec §6.
          (clock-observe-epoch clock epoch)
          (let ((tm (transaction-manager graph)))
            (with-recursive-lock-held ((lock tm))
              (when (>= epoch (tx-id-counter tm))
                (setf (tx-id-counter tm) (1+ epoch))))))))
  graph)
```

- [ ] **Step 4: Run to verify it passes**

The peer suites (`peer-lamport-tests`, `peer-conflict-tests`, `peer-rehome-tests`) must
stay green — they run with no clock, so they exercise the fallback branch.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/system-clock-tests.lisp
git commit -m "feat(clock): peer-observe-epoch raises the image clock when one is bound (#168)"
```

---

### Task 6: Cross-store read snapshots pin every participating store

**Files:**
- Modify: `transactions.lisp` (`call-with-read-snapshot`, ~2971-3010)
- Test: `tests/system-clock-tests.lisp`

**Interfaces:** Consumes Task 3.

`WITH-READ-SNAPSHOT` composes across graphs today, each internally consistent, with
deliberately no common instant. With a clock there *is* a common instant — but the
reaper in store B must not free a version store A's snapshot could still dereference. So
a cross-store snapshot registers a read pin with every participating store.

**Named cost (spec §6):** a long cross-store query delays reaping in every store it
touched. That is the intended trade, not a regression.

- [ ] **Step 1: Write the failing test**

```lisp
(test cross-store-snapshot-pins-every-store
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let ((ga (make-graph :sc-pin-a (namestring da)
                                     :buffer-pool-size 1000
                                     :system-clock clock))
                     (gb (make-graph :sc-pin-b (namestring db)
                                     :buffer-pool-size 1000
                                     :system-clock clock)))
                 (unwind-protect
                      (with-read-snapshot (ga)
                        (with-read-snapshot (gb)
                          ;; Both managers hold a pin for this reader.
                          (is (plusp (hash-table-count
                                      (read-pins
                                       (transaction-manager ga)))))
                          (is (plusp (hash-table-count
                                      (read-pins
                                       (transaction-manager gb)))))))
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock)))))
```

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — only the innermost graph is pinned.

- [ ] **Step 3: Implement**

In `call-with-read-snapshot`, take a read pin on the graph being snapshotted for the
dynamic extent of the snapshot, releasing it in the same `unwind-protect` that removes
the `*read-snapshots*` entry. Keep the existing inheritance short-circuits: an enclosing
read-write transaction on that graph, or an existing snapshot of it, still returns
without adding a second pin.

- [ ] **Step 4: Run to verify it passes**

Watch `mvcc-tests` and `multi-graph-tests` particularly — they exercise reaping and
composed snapshots.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/system-clock-tests.lisp
git commit -m "feat(clock): a cross-store snapshot pins every participating store (#168)"
```

---

### Task 7: Documentation

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` (new section after "Multiple Graphs in One Image")
- Modify: `CHANGELOG.md`
- Modify: `docs/superpowers/specs/2026-08-20-namespaces-design.md` (§6 status)

Docs travel with the code; this task is not optional.

- [ ] **Step 1: Manual section**

Cover: what a system is; `*SYSTEM-CLOCK*` NIL is the default and preserves pre-#168
behaviour; how to open a clock and attach stores; the watermark and its one stated
limitation (you cannot snapshot into the pre-migration past across stores); leases; and
that **the clock is not purely local** because `peer-observe-epoch` admits foreign epochs.

- [ ] **Step 2: CHANGELOG entry**

Note the new exports, the NIL default, and the `recreate-graph` fix as a **behaviour
change for anyone who was relying on restore's ids being dense from zero**.

- [ ] **Step 3: Mark §6 of the spec implemented**, leaving the epoch-density audit result
in place as the record of why no density work was needed.

- [ ] **Step 4: Commit**

```bash
git add docs/ CHANGELOG.md
git commit -m "docs(clock): document the image-level clock, leases and the watermark (#168)"
```

---

## Testing

Every run must assert it loaded the worktree, not the main checkout — `graph-db.asd` is
symlinked into `~/quicklisp/local-projects` from the main checkout, and a naive load
silently tests the wrong tree.

```lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(asdf:initialize-source-registry
 '(:source-registry (:directory "<worktree>/") :inherit-configuration))
(dolist (s '(:graph-db :graph-db/test)) (asdf:clear-system s))
(let ((dir (namestring (asdf:system-source-directory (asdf:find-system :graph-db)))))
  (unless (search "168-image-level-clock" dir)
    (format *error-output* "~&FATAL: ~a is not the worktree~%" dir)
    (uiop:quit 2)))
(asdf:test-system :graph-db/test)
```

Full suite is ~15 minutes and exceeds the default Bash timeout — run it in the
background and poll the log. **Never run two SBCL builds at once**; they contend on the
FASL cache.

Single suite while iterating:

```lisp
(fiveam:run! 'graph-db/test::system-clock-suite)
```

## Self-Review

**Spec coverage.** §6's five bullets map to Tasks 1 (clock, leases), 2 (journal), 3
(image-level not store-level, watermark), 5 (foreign epochs), 6 (cross-store pins). §9.1's
journal is Task 2. The audit finding is Task 4. §6's density audit needs no task — it
came back clean.

**Not in scope, deliberately:** the shadow swap (#170) and restore retention/manifest
(#171) consume the journal but are separate units. Two-phase commit (#93) stays deferred.

**Type consistency.** `CLOCK-NEXT-EPOCH` / `CLOCK-CURRENT-EPOCH` / `CLOCK-OBSERVE-EPOCH` /
`CLOCK-LEASE-EPOCHS` are used with those names in Tasks 3, 4 and 5. `TM-NEXT-EPOCH` and
`TM-CURRENT-EPOCH` are defined in Task 3 and consumed in Task 4. `GRAPH-SYSTEM-CLOCK` is
defined in Task 3 and consumed in Task 5.

**Risk to watch.** Task 3 touches the hottest path in the engine. The NIL branch must be
identical to today's code, and the three suites at their exact baseline counts are the
evidence. If any count moves, stop rather than adjusting the baseline.
