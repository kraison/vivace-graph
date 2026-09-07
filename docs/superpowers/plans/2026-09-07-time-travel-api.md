# Node-Local Time Travel (MVCC C-3) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A caller names an epoch and reads the graph as it was then — values, membership, adjacency, `select` — with reaping reported, never absorbed (GH #115, Phase C-3 of #117).

**Architecture:** An as-of read is today's read-only snapshot transaction started at E+1 (`create-transaction :start-epoch`), registered in `*read-snapshots*` as an `as-of-tx`, so every existing snapshot-resolving read path answers at E unchanged. Membership comes from walking index entries including tombstones and resolving each id at E. `resolve-version-at-epoch` reports the oldest retained version so a lookup can tell "did not exist" (revision 0) from "reaped" (revision above 0). No storage format change.

**Tech Stack:** SBCL, FiveAM, `graph-db` core; `graph-db/spacetime` for the composition tests.

**Spec:** `docs/superpowers/specs/2026-09-07-time-travel-api-design.md`. Two rulings made while planning, folded into the spec in Task 9: (1) the reaper always leaves ONE lagging archived version (a committing transaction's own start bounds the floor), so `:keep-revisions 0` answers the latest epoch and at most one behind — the spec's "only the latest" is stated as that in §3.2; (2) a graph with no transaction manager yet refuses `:as-of` with reason `:no-version-history`, alongside the memory graph.

## Global Constraints

- Lisp: spaces only, hard 80 columns, terse comments naming GH #115 or a spec section; docstrings state what/returns/trap.
- Branch `feat/mvcc-c3` from `experiment` (1973fa6), worktree
  a dedicated worktree of a clean clone of `experiment` (`<worktree>` below; the clone directory is `<clone>`). Never build in a shared checkout.
- Never run `pkill`, `pgrep -f`, or `kill`. One SBCL build at a time in this worktree. Never the full 15-minute suite by hand; the four suites below only. CI runs the full suite on push.
- No storage format change (spec R5). The untyped scan and the memory graph refuse `:as-of` (R6, R7).
- Existing tests keep passing; the baseline check counts recorded in Task 1 never drop.
- Every new `graph-db` symbol a test uses unqualified is added to the `:import-from #:graph-db` list in `tests/package.lisp` (see `tests/README.md`: the test package does not `:use` graph-db, and a forgotten import interns a fresh symbol that fails only at load time).
- Docs travel with the code (Task 9); every commit message names GH #115.
- Commit trailers on every commit:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
  ```

## Running the suites

Write once to `<clone>/vg-115-suites.lisp` (outside the worktree, git-ignored by being outside it):

```lisp
;; The four suites this unit touches, CI-style, in a fresh image.
;; Mirrors RUN-TESTS (tests/suite.lisp): FiveAM's RUN bypasses the
;; test system's :perform, so the system directory is bound here.
(ql:quickload '(:graph-db/test :graph-db/spacetime-test) :silent t)
(in-package :graph-db/test)
(log:config :error)
(let* ((system-dir (make-temp-directory))
       (graph-db::*system-directory* (namestring system-dir))
       (graph-db::*type-registry* nil)
       (ok t))
  (unwind-protect
       (progn
         (dolist (s '(mvcc-suite multi-graph-suite memory-graph-suite
                      query-suite))
           (let ((r (fiveam:run s)))
             (fiveam:explain! r)
             (format t "~&== ~a ~a~%" s
                     (if (fiveam:results-status r) "PASS" "FAIL"))
             (unless (fiveam:results-status r) (setf ok nil))))
         (unless (graph-db/spacetime-test::run-spacetime-tests)
           (setf ok nil)))
    (graph-db-test-scratch:cleanup-scratch-run))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root:

```bash
cd <worktree>
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load <clone>/vg-115-suites.lisp \
  > <clone>/vg-115-suites.log 2>&1; echo "exit=$?"
grep -E "Did [0-9]+ checks|Fail:|^== " <clone>/vg-115-suites.log
```

A single test while iterating, in the same fresh-image style:

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(in-package :graph-db/test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

## Code facts every task relies on

- Epochs: a commit takes `tm-next-epoch`; `tm-current-epoch` is the next one to be handed out. A transaction's `start-tx-id` is `tm-current-epoch` at creation; `resolve-version-at-epoch live graph epoch` returns the newest version with `commit-epoch < epoch`. `graph-db::transaction-id` of a committed transaction is its epoch (read after the commit, as `%tx` in `tests/spacetime/epoch-tests.lisp` does).
- The reaper (`reap-node-chain`, `transactions.lisp`) frees the oldest suffix whose stop-epoch is below `reap-safe-floor` = min(active starts, read pins), keeping `keep-revisions` archived versions. The committing transaction's own start always retains one lagging version.
- `call-with-read-snapshot thunk &optional graph` (`transactions.lisp` ~3337): registers a read-only `tx` via `create-transaction tm :allow-read-only t`, pins, stores it in `*read-snapshots*` under the graph; an existing snapshot of the same graph, or a read-write `*transaction*` covering it, is inherited. `lookup-object` with a NIL transaction consults `*read-snapshots*` first and recurses with the snapshot transaction.
- `delete-node` writes a new version with `deleted-p` set; the type/ve/vev index entry stays. `map-index-list ... :include-deleted-p t` also visits entries `mark-pcons-deleted` flagged (compaction). `map-vertices` typed scans and every `map-edges` typed/adjacency branch resolve each id through `lookup-vertex`/`lookup-edge`, then filter `deleted-p` / `active-edge-p` on the resolved version.
- `vertex-history graph id &key limit` (`transactions.lisp` ~671) walks the live head's chain under a read pin: `(version . commit-epoch)` newest first; archived heads come from `deserialize-vertex-head (heap graph) p`; edges use `deserialize-edge-head`.
- Test fixtures: `with-test-graph (g)` (`tests/suite.lisp`) binds `*graph*`; `g-person` (`name`, `age`), `g-knows` edge (`since`) from `tests/graph-tests.lisp`; `bump-age id n` and `version-chain-length node graph` from `tests/mvcc-tests.lisp`; `mark-deleted` is imported. `with-mem-graph (g)` in `tests/multi-graph-tests.lisp`. `with-clocked-stores (a b)`, `%tx`, `%unary`, `%one`, `make-ea-claim-unary`, `make-eb-claim-unary`, `retract-claim` in `tests/spacetime/epoch-tests.lisp`.

---

### Task 1: Baseline

**Files:**
- Create: `<clone>/vg-115-suites.lisp` (the runner above)

- [ ] **Step 1: Write the runner and run it on the unchanged branch**

Run the "Running the suites" block. Expected: `exit=0`, five PASS lines / a passing spacetime run.

- [ ] **Step 2: Record the counts**

Copy every `Did N checks` line into the SDD ledger as the baseline. No later run may report fewer checks for a suite except where a task names a removed assertion (none does).

---

### Task 2: The as-of snapshot

**Files:**
- Modify: `transactions.lisp` — conditions after `copying-uncommitted-node` (~233); `as-of-tx` after `defclass tx` (~459); `create-transaction` defgeneric (246) and method (~3279); `call-with-read-snapshot` and `with-read-snapshot` (~3337–3386); new `latest-epoch`, `%as-of-snapshot`, `as-of-skipped-count`, `with-as-of` beside them
- Modify: `package.lisp` exports after `#:call-with-read-snapshot` (283)
- Modify: `tests/package.lisp` `:import-from #:graph-db` after `#:vertex-history` (252)
- Test: `tests/mvcc-tests.lisp` (append)

**Interfaces:**
- Produces: `(create-transaction tm &key allow-read-only start-epoch (class 'tx) initargs)`; class `as-of-tx` with readers `as-of-epoch`, `as-of-if-reaped`, accessor `as-of-skipped`; `(call-with-read-snapshot thunk &optional graph &key as-of (if-reaped :error))`; `(with-as-of ((graph) epoch &key (if-reaped :error)) body...)`; `(latest-epoch graph)`; `(%as-of-snapshot graph)` → the `as-of-tx` or NIL; `(as-of-skipped-count graph)`; condition `as-of-refused` with readers `as-of-refused-graph`, `as-of-refused-epoch`, `as-of-refused-reason`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/mvcc-tests.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; GH #115: node-local time travel (spec 2026-09-07)
;;; ---------------------------------------------------------------------------

(defun %epoch-of (thunk)
  "Run THUNK in a transaction on *GRAPH*; the committed epoch."
  (graph-db::transaction-id
   (with-transaction () (funcall thunk) graph-db:*transaction*)))

(defmacro with-kept-graph ((g keep) &body body)
  "A fresh integration graph with :KEEP-REVISIONS KEEP, *GRAPH* bound."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *integration-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000 :keep-revisions ,keep)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (close-graph ,g :snapshot-p nil)
           (collect-garbage))))))

(test as-of-answers-the-version-live-at-each-epoch
  "Spec §2, R2: an as-of read is inclusive -- the version whose commit
epoch is the newest at or below E -- and NIL before the node existed."
  (with-kept-graph (g 3)
    (let (id e0 e1 e2 e3)
      (setq e0 (%epoch-of (lambda () (make-g-person :name "seed" :age 0))))
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 1) (setq e2 (latest-epoch g))
      (bump-age id 2) (setq e3 (latest-epoch g))
      (is (= e1 (1+ e0)) "control: consecutive commits, no clock")
      (is (= e3 (latest-epoch g)) "LATEST-EPOCH names the newest commit")
      (with-as-of ((g) e0)
        (is (null (lookup-vertex id)) "before creation: absent"))
      (with-as-of ((g) e1)
        (is (= 0 (slot-value (lookup-vertex id) 'age))
            "inclusive at the creating epoch"))
      (with-as-of ((g) e2)
        (is (= 1 (slot-value (lookup-vertex id) 'age))))
      (with-as-of ((g) e3)
        (is (= 2 (slot-value (lookup-vertex id) 'age))))
      (is (= 2 (slot-value (lookup-vertex id) 'age))
          "outside the extent the live version answers"))))

(test as-of-reads-are-repeatable-across-a-concurrent-commit
  "Spec §3.1: reads inside one extent resolve at one epoch even when a
transaction commits an update meanwhile."
  (with-kept-graph (g 3)
    (let (id e)
      (setq e (%epoch-of
               (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (with-as-of ((g) e)
        (is (= 0 (slot-value (lookup-vertex id) 'age)))
        (bump-age id 7)
        (is (= 0 (slot-value (lookup-vertex id) 'age))
            "the concurrent update is invisible at E")))))

(test as-of-refuses-what-it-cannot-answer
  "Spec §2.2: the refusals, each by reason; the same epoch inherits and a
plain snapshot inside an as-of extent inherits it."
  (with-test-graph (g)
    (let (id e)
      (setq e (%epoch-of
               (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (flet ((reason (thunk)
               (handler-case (progn (funcall thunk) nil)
                 (as-of-refused (c) (as-of-refused-reason c)))))
        (is (eq :future-epoch
                (reason (lambda () (with-as-of ((g) (1+ e)) nil)))))
        (is (eq :read-write-transaction
                (reason (lambda ()
                          (with-transaction () (with-as-of ((g) e) nil))))))
        (is (eq :snapshot-active
                (reason (lambda ()
                          (with-as-of ((g) e)
                            (with-as-of ((g) (1- e)) nil))))))
        (is (eq :snapshot-active
                (reason (lambda ()
                          (graph-db:with-read-snapshot (g)
                            (with-as-of ((g) e) nil))))))
        (is (null (reason (lambda ()
                            (with-as-of ((g) e) (with-as-of ((g) e) nil)))))
            "the same epoch inherits")
        (is (null (reason (lambda ()
                            (with-as-of ((g) e)
                              (graph-db:with-read-snapshot (g) nil)))))
            "a plain snapshot inside an as-of extent inherits it")))))

(test as-of-snapshot-holds-the-reaper-floor
  "Spec §2.3: an open as-of extent retains the versions live at E, as a
held read pin does (READ-PIN-RETAINS-VERSIONS-UNTIL-RELEASED); after the
extent the chain returns to steady state."
  (with-test-graph (g)
    (let (id e)
      (setq e (%epoch-of
               (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (flet ((live () (graph-db::lookup-node (graph-db::vertex-table g) id g)))
        (with-as-of ((g) e)
          (bump-age id 1) (bump-age id 2) (bump-age id 3)
          (is (>= (version-chain-length (live) g) 2)
              "an open as-of extent keeps prior versions from being reaped"))
        (bump-age id 4) (bump-age id 5)
        (is (= 1 (version-chain-length (live) g))
            "after the extent the chain returns to steady-state size")))))
```

Add to `tests/package.lisp` after `#:vertex-history`:

```lisp
                #:with-as-of                 ; GH #115
                #:latest-epoch
                #:as-of-refused
                #:as-of-refused-reason
                #:as-of-skipped-count
```

- [ ] **Step 2: Run to verify they fail**

Run the single-test command with `as-of-answers-the-version-live-at-each-epoch`. Expected: a load failure on `with-as-of` / `latest-epoch` (the export does not exist yet). That is the failure this step wants; the tests cannot compile before the API exists.

- [ ] **Step 3: Conditions, the class, the transaction**

In `transactions.lisp`, after `copying-uncommitted-node` (~233):

```lisp
(define-condition as-of-refused (error)
  ((graph :initarg :graph :reader as-of-refused-graph)
   (epoch :initarg :epoch :reader as-of-refused-epoch)
   (reason :initarg :reason :reader as-of-refused-reason))
  (:documentation "An as-of read the store cannot answer (GH #115, spec
§2.2).  REASON: :FUTURE-EPOCH, :READ-WRITE-TRANSACTION, :SNAPSHOT-ACTIVE,
:UNTYPED-SCAN or :NO-VERSION-HISTORY.")
  (:report (lambda (c s)
             (format s "as-of ~A refused on ~A: ~A"
                     (as-of-refused-epoch c)
                     (graph-name (as-of-refused-graph c))
                     (as-of-refused-reason c)))))
```

After `defclass tx`:

```lisp
(defclass as-of-tx (tx)
  ((as-of-epoch :initarg :as-of-epoch :reader as-of-epoch)
   (if-reaped :initarg :if-reaped :reader as-of-if-reaped)
   (skipped :initform 0 :accessor as-of-skipped))
  (:documentation "A read-only snapshot at a named epoch: START-TX-ID is
AS-OF-EPOCH + 1 (GH #115, spec §2).  IF-REAPED is :ERROR or :SKIP."))
```

Change the defgeneric (246) and the method (~3279):

```lisp
(defgeneric create-transaction (transaction-manager
                                &key allow-read-only start-epoch class
                                initargs))
```

```lisp
(defmethod create-transaction (transaction-manager
                               &key allow-read-only start-epoch (class 'tx)
                               initargs)
  ;; ALLOW-READ-ONLY is CALL-WITH-READ-SNAPSHOT's escape hatch: its
  ;; bookkeeping TX is never committed, so it follows the read-pin
  ;; rule (admitted under :READ-ONLY) rather than the write rule
  ;; (refused under any non-T state) -- see PIN-READ-EPOCH (GH #170).
  ;; START-EPOCH: an as-of snapshot starts in the past (GH #115); it may
  ;; never start in the future, or the floor would rise above live data.
  (with-recursive-lock-held ((lock transaction-manager))
    (let ((state (accepting-p transaction-manager)))
      (unless (or (eq state t)
                  (and allow-read-only (eq state :read-only)))
        (error 'store-not-accepting-error
               :name (graph-name (graph transaction-manager))
               :reason (if (eq state :read-only) :shadow-load state))))
    (let* ((sequence-number (next-sequence-number transaction-manager))
           (graph (graph transaction-manager))
           (cache (cache graph))
           (current (tm-current-epoch transaction-manager))
           (start-tx-id (or start-epoch current))
           (tx (apply #'make-instance class
                      :sequence-number sequence-number
                      :start-tx-id start-tx-id
                      :finish-tx-id nil
                      :tx-id nil
                      :transaction-manager transaction-manager
                      :graph graph
                      :graph-cache cache
                      initargs)))
      (when (> start-tx-id current)
        (error "start-epoch ~A is past the current epoch ~A"
               start-tx-id current))
      (add-transaction tx transaction-manager)
      (setf (state tx) :active)
      tx)))
```

(The `(when (> ...))` check runs before `add-transaction`, as shown, so a refused start registers nothing.)

- [ ] **Step 4: The snapshot entry points**

Replace `call-with-read-snapshot`'s lambda list and dispatch:

```lisp
(defun call-with-read-snapshot (thunk &optional (graph *graph*)
                                &key as-of (if-reaped :error))
  "...existing docstring, plus:

:AS-OF EPOCH (GH #115, spec §2) opens the snapshot at EPOCH instead of now:
a read-only transaction started at EPOCH+1, so every read resolves to the
version whose commit epoch is the newest at or below EPOCH.  Refused with
AS-OF-REFUSED for an epoch not yet committed, inside a read-write
transaction on GRAPH, inside a snapshot of GRAPH at another epoch (the
same epoch inherits), on a memory graph, or before GRAPH has a manager.
:IF-REAPED (:ERROR, or :SKIP) says what a read of a version reaped past
:KEEP-REVISIONS does -- see VERSION-REAPED-ERROR."
  (let ((tm (and graph
                 (slot-boundp graph 'transaction-manager)
                 (transaction-manager graph))))
    (flet ((refuse (reason)
             (error 'as-of-refused :graph graph :epoch as-of :reason reason)))
      (when as-of
        (let ((mem (find-class 'memory-graph-mixin nil)))
          (when (or (null tm) (and mem (typep graph mem)))
            (refuse :no-version-history)))
        (when (>= as-of (tm-current-epoch tm)) (refuse :future-epoch))
        (when (and *transaction* (%transaction-covers-graph-p *transaction* graph))
          (refuse :read-write-transaction))
        (let ((open (and *read-snapshots* (gethash graph *read-snapshots*))))
          (when open
            (if (and (typep open 'as-of-tx) (= (as-of-epoch open) as-of))
                (return-from call-with-read-snapshot (funcall thunk))
                (refuse :snapshot-active)))))
      (cond
        ((null tm) (funcall thunk))
        ((and *transaction* (%transaction-covers-graph-p *transaction* graph))
         (funcall thunk))
        ((and *read-snapshots* (gethash graph *read-snapshots*)) (funcall thunk))
        (t
         ...existing body, with the one line
             (setq txn (create-transaction tm :allow-read-only t))
         becoming
             (setq txn (if as-of
                           (create-transaction
                            tm :allow-read-only t :start-epoch (1+ as-of)
                            :class 'as-of-tx
                            :initargs (list :as-of-epoch as-of
                                            :if-reaped if-reaped))
                           (create-transaction tm :allow-read-only t)))
         ...)))))
```

`memory-graph-mixin` is defined in `memory-graph.lisp`, loaded after this file, hence `find-class` at run time. Keep the 80-column limit: break the `(when (and *transaction* ...))` line.

After `with-read-snapshot`:

```lisp
(defmacro with-as-of (((&optional (graph '*graph*)) epoch
                       &key (if-reaped :error))
                      &body body)
  "Evaluate BODY with reads of GRAPH resolving as of EPOCH (GH #115).
See CALL-WITH-READ-SNAPSHOT :AS-OF."
  `(call-with-read-snapshot (lambda () ,@body) ,graph
                            :as-of ,epoch :if-reaped ,if-reaped))

(defun latest-epoch (graph)
  "The newest epoch an as-of read of GRAPH may name: one below the
manager's next epoch (GH #115).  Under a system clock this is the newest
epoch handed out to ANY store on it."
  (1- (tm-current-epoch (transaction-manager graph))))

(defun %as-of-snapshot (graph)
  "GRAPH's open as-of snapshot transaction, or NIL (GH #115)."
  (let ((s (and *read-snapshots* (gethash graph *read-snapshots*))))
    (and (typep s 'as-of-tx) s)))

(defun as-of-skipped-count (graph)
  "Reads skipped as reaped inside GRAPH's open as-of extent under
:IF-REAPED :SKIP; NIL when no as-of snapshot of GRAPH is open (GH #115)."
  (let ((s (%as-of-snapshot graph)))
    (and s (as-of-skipped s))))
```

Exports in `package.lisp` after `#:call-with-read-snapshot`:

```lisp
           ;; GH #115: node-local time travel
           #:with-as-of
           #:latest-epoch
           #:as-of-skipped-count
           #:as-of-refused
           #:as-of-refused-graph
           #:as-of-refused-epoch
           #:as-of-refused-reason
```

- [ ] **Step 5: Run the four new tests**

Run each with the single-test command. Expected: all PASS. If `as-of-refuses-what-it-cannot-answer` reports `:read-write-transaction` as NIL, the check order in Step 4 is wrong (the read-write check must precede the `cond`).

- [ ] **Step 6: Column check and commit**

```bash
awk 'length > 80 {print FILENAME":"FNR}' transactions.lisp package.lisp tests/mvcc-tests.lisp tests/package.lisp
git add transactions.lisp package.lisp tests/mvcc-tests.lisp tests/package.lisp
git commit -m "feat(mvcc): as-of read snapshots (#115)

create-transaction :start-epoch; call-with-read-snapshot :as-of opens a
read-only as-of-tx started at E+1; with-as-of, latest-epoch, the
as-of-refused reasons (spec 2026-09-07 §2).

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU"
```

---

### Task 3: Reaped versus absent

**Files:**
- Modify: `transactions.lisp` — `resolve-version-at-epoch` (~645), the transactional `lookup-object` method where it calls it (~343), a new condition and `%as-of-unresolved` beside `resolve-version-at-epoch`
- Modify: `package.lisp`, `tests/package.lisp`
- Test: `tests/mvcc-tests.lisp` (append)

**Interfaces:**
- Consumes: `as-of-tx`, `as-of-epoch`, `as-of-if-reaped`, `as-of-skipped` (Task 2).
- Produces: `(resolve-version-at-epoch live graph epoch)` → `(values version-or-nil oldest-retained)`; condition `version-reaped-error` with readers `version-reaped-id`, `version-reaped-epoch`, `version-reaped-oldest-epoch`, `version-reaped-oldest-revision`.

- [ ] **Step 1: Write the failing tests**

```lisp
(test as-of-reports-a-reaped-version-instead-of-lying
  "Spec §3.2, R4: with :KEEP-REVISIONS 1 and three updates the chain holds
the live version and one archived; an epoch older than that signals
VERSION-REAPED-ERROR naming the oldest retained epoch, :IF-REAPED :SKIP
answers NIL and counts, and the retained epoch still answers."
  (with-kept-graph (g 1)
    (let (id e1 e2 e3 e4)
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 1) (setq e2 (latest-epoch g))
      (bump-age id 2) (setq e3 (latest-epoch g))
      (bump-age id 3) (setq e4 (latest-epoch g))
      (let ((c (handler-case (with-as-of ((g) e1) (lookup-vertex id) nil)
                 (version-reaped-error (c) c))))
        (is (typep c 'version-reaped-error) "as-of E1 is reaped")
        (when (typep c 'version-reaped-error)
          (is (= e3 (version-reaped-oldest-epoch c))
              "the oldest retained version is the one committed at E3")
          (is (= 2 (version-reaped-oldest-revision c)))
          (is (= e1 (version-reaped-epoch c)))))
      (signals version-reaped-error (with-as-of ((g) e2) (lookup-vertex id)))
      (with-as-of ((g) e1 :if-reaped :skip)
        (is (null (lookup-vertex id)) ":skip answers NIL")
        (is (= 1 (as-of-skipped-count g)) "and counts the skip"))
      (is (null (as-of-skipped-count g)) "no count outside an extent")
      (with-as-of ((g) e3)
        (is (= 2 (slot-value (lookup-vertex id) 'age)) "E3 is retained"))
      (with-as-of ((g) e4)
        (is (= 3 (slot-value (lookup-vertex id) 'age)))))))

(test keep-revisions-zero-is-no-time-travel
  "Spec §3.2 (as ruled in the plan): the default :KEEP-REVISIONS 0 keeps
the live version and the one lagging version the committing transaction's
own floor retains; two updates later the creation epoch is reaped, while
an epoch before the node existed still reads as absent (revision 0)."
  (with-test-graph (g)
    (let (id e0 e1)
      (setq e0 (%epoch-of (lambda () (make-g-person :name "seed" :age 0))))
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 1) (bump-age id 2)
      (signals version-reaped-error (with-as-of ((g) e1) (lookup-vertex id)))
      ;; With revision 0 gone, "created after E0" is unknowable: the
      ;; read reports reaped, the documented limit (spec §3.2).  The
      ;; "before creation is NIL" case lives in
      ;; AS-OF-ANSWERS-THE-VERSION-LIVE-AT-EACH-EPOCH, whose chain is intact.
      (signals version-reaped-error
        (with-as-of ((g) e0) (lookup-vertex id))))))
```

Add `#:version-reaped-error #:version-reaped-oldest-epoch #:version-reaped-oldest-revision #:version-reaped-epoch` to `tests/package.lisp`.

- [ ] **Step 2: Run to verify they fail**

Expected: load failure on the unexported condition name.

- [ ] **Step 3: Implement**

```lisp
(define-condition version-reaped-error (error)
  ((id :initarg :id :reader version-reaped-id)
   (epoch :initarg :epoch :reader version-reaped-epoch)
   (oldest-epoch :initarg :oldest-epoch :reader version-reaped-oldest-epoch)
   (oldest-revision :initarg :oldest-revision
                    :reader version-reaped-oldest-revision))
  (:documentation "The version of node ID live at EPOCH existed but is
reaped past :KEEP-REVISIONS; the oldest the store still holds committed at
OLDEST-EPOCH with revision OLDEST-REVISION (GH #115, spec §3.2).")
  (:report (lambda (c s)
             (format s "version of ~A at epoch ~A is reaped; oldest ~
retained is epoch ~A (revision ~A)"
                     (string-id (version-reaped-id c))
                     (version-reaped-epoch c)
                     (version-reaped-oldest-epoch c)
                     (version-reaped-oldest-revision c)))))

(defun resolve-version-at-epoch (live-node graph epoch)
  "The version of LIVE-NODE visible to a reader whose snapshot is EPOCH
(the newest with commit-epoch < EPOCH), or NIL.  Second value, when the
first is NIL: the oldest retained version head (the live head itself when
it has no chain) -- its REVISION tells absent (0) from reaped (GH #115)."
  (if (< (commit-epoch live-node) epoch)
      live-node
      (let ((id (id live-node))
            (edge-p (typep live-node 'edge))
            (oldest live-node)
            (p (prev-pointer live-node)))
        (loop
          (when (zerop p) (return (values nil oldest)))
          (let ((ver (if edge-p
                         (deserialize-edge-head (heap graph) p)
                         (deserialize-vertex-head (heap graph) p))))
            (setf (id ver) id)
            (if (< (commit-epoch ver) epoch)
                (progn (ensure-node-bytes ver graph) (return ver))
                (setf oldest ver
                      p (prev-pointer ver))))))))

(defun %as-of-unresolved (transaction oldest)
  "NIL for a node absent at TRANSACTION's epoch; for one whose version
then is reaped (OLDEST's revision above 0) apply the as-of policy: signal
VERSION-REAPED-ERROR, or count and skip (GH #115, spec §3.2)."
  (when (and (typep transaction 'as-of-tx)
             oldest
             (plusp (revision oldest)))
    (ecase (as-of-if-reaped transaction)
      (:skip (incf (as-of-skipped transaction)) nil)
      (:error (error 'version-reaped-error
                     :id (id oldest)
                     :epoch (as-of-epoch transaction)
                     :oldest-epoch (commit-epoch oldest)
                     :oldest-revision (revision oldest))))))
```

In the transactional `lookup-object` method replace

```lisp
                (when *snapshot-reads-p*
                  (setq value (resolve-version-at-epoch
                               value (graph transaction)
                               (start-tx-id transaction))))
```

with

```lisp
                (when *snapshot-reads-p*
                  (multiple-value-bind (v oldest)
                      (resolve-version-at-epoch value (graph transaction)
                                                (start-tx-id transaction))
                    (setq value (or v (%as-of-unresolved transaction
                                                         oldest)))))
```

`revision` and `commit-epoch` are node-head accessors already used in this file. Export the condition and its four readers in `package.lisp` beside the Task 2 block.

- [ ] **Step 4: Run the two tests, then the whole `mvcc-suite`**

Expected: PASS, and the suite's check count ≥ baseline + the new checks.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp package.lisp tests/mvcc-tests.lisp tests/package.lisp
git commit -m "feat(mvcc): as-of reads report reaped versions (#115)

resolve-version-at-epoch returns the oldest retained version; an as-of
lookup tells absent (revision 0) from reaped and signals
version-reaped-error or skips per :if-reaped (spec §3.2)."
```

(with the two trailers.)

---

### Task 4: Membership at E

**Files:**
- Modify: `vertex.lisp` `map-vertices` (~186–290)
- Modify: `edge.lisp` `edge-exists-p` (~370) and `map-edges` (~391–500)
- Test: `tests/mvcc-tests.lisp` (append), `tests/multi-graph-tests.lisp` (append)

**Interfaces:**
- Consumes: `%as-of-snapshot graph`, `as-of-epoch`, `as-of-refused` (Task 2).
- Produces: nothing new; the scans answer at E.

- [ ] **Step 1: Write the failing tests**

Append to `tests/mvcc-tests.lisp`:

```lisp
(defun %names-at (g e)
  (with-as-of ((g) e)
    (sort (map-vertices (lambda (v) (slot-value v 'name)) g
                        :collect-p t :vertex-type 'g-person)
          #'string<)))

(test as-of-typed-scan-reconstructs-membership-both-ways
  "Spec §3.3: at E a typed scan excludes a vertex created after E and
includes one deleted after E; at the deletion epoch it is gone.  This is
the case SNAPSHOT-HIDES-NODES-CREATED-AFTER-START never covered."
  (with-kept-graph (g 3)
    (let (b e-mid e-del)
      (with-transaction () (make-g-person :name "a" :age 1))
      (setq e-mid (%epoch-of
                   (lambda () (setq b (id (make-g-person :name "b" :age 2))))))
      (with-transaction () (make-g-person :name "c" :age 3))
      (setq e-del (%epoch-of (lambda () (mark-deleted (lookup-vertex b)))))
      (is (equal '("a" "b") (%names-at g e-mid))
          "c not yet created, b not yet deleted")
      (is (equal '("a" "c") (%names-at g e-del))
          "at the deletion epoch b is gone (inclusive)")
      (is (equal '("a" "c") (%names-at g (latest-epoch g))))
      (with-as-of ((g) e-mid)
        (is (= 1 (select-count (?p) (is-a ?p g-person)
                               (node-slot-value ?p name "b")))
            "is-a/2 enumerates through the same scan")))))

(test as-of-adjacency-reconstructs-edges-and-endpoints
  "Spec §3.3: OUTGOING-EDGES at E excludes an edge created after E,
includes one deleted after E, and an edge whose endpoint was deleted
after E is active at E."
  (with-kept-graph (g 3)
    (let (a b c e-mid e-del)
      (with-transaction ()
        (let ((va (make-g-person :name "a" :age 1))
              (vb (make-g-person :name "b" :age 2))
              (vc (make-g-person :name "c" :age 3)))
          (setq a (id va) b (id vb) c (id vc))
          (make-g-knows :from va :to vb :since 1)))
      (setq e-mid (latest-epoch g))
      (with-transaction ()
        (make-g-knows :from (lookup-vertex a) :to (lookup-vertex c) :since 2))
      (setq e-del (%epoch-of
                   (lambda ()
                     (mark-deleted
                      (find 1 (outgoing-edges (lookup-vertex a))
                            :key (lambda (ed) (slot-value ed 'since))))
                     (mark-deleted (lookup-vertex c)))))
      (flet ((sinces-at (e)
               (with-as-of ((g) e)
                 (sort (mapcar (lambda (ed) (slot-value ed 'since))
                               (outgoing-edges (lookup-vertex a)))
                       #'<))))
        (is (equal '(1) (sinces-at e-mid)) "the second edge is not yet born")
        (is (equal '(1 2) (sinces-at (1- e-del)))
            "both born, neither deleted")
        (is (equal '() (sinces-at e-del))
            "at E-DEL the first edge is deleted and the second's endpoint
c is deleted, so neither is active")
        (is (equal '() (sinces-at (latest-epoch g))))))))
```

```lisp
(test as-of-refuses-the-untyped-scan
  "Spec R6: the raw lhash walk reads live versions; under a named epoch it
is refused rather than answering live."
  (with-test-graph (g)
    (with-transaction () (make-g-person :name "a" :age 1))
    (let ((e (latest-epoch g)))
      (is (eq :untyped-scan
              (handler-case
                  (with-as-of ((g) e) (map-vertices #'identity g) nil)
                (as-of-refused (c) (as-of-refused-reason c)))))
      (is (eq :untyped-scan
              (handler-case
                  (with-as-of ((g) e) (map-edges #'identity g) nil)
                (as-of-refused (c) (as-of-refused-reason c))))))))
```

Append to `tests/multi-graph-tests.lisp`:

```lisp
(test as-of-is-refused-on-a-memory-graph
  "Spec R7: the in-memory backend keeps no version chains, so it has no
history to travel; refused by reason, before the epoch is even checked."
  (with-mem-graph (g)
    (is (eq :no-version-history
            (handler-case (graph-db:with-as-of ((g) 0) nil)
              (graph-db:as-of-refused (c)
                (graph-db:as-of-refused-reason c)))))))
```

- [ ] **Step 2: Run to verify they fail**

Expected: `as-of-refuses-the-untyped-scan` fails (no refusal yet). The two membership tests may already pass: typed scans resolve each id through the snapshot since Task 2, and a soft delete leaves its index entry in place. Record which assertions were red in the ledger. The tombstone walk (`:include-deleted-p t` at the index-list level) covers entries compaction flagged; no test here compacts, so that part is verified by reading the diff. The memory-graph test passes from Task 2 already.

- [ ] **Step 3: Implement**

`vertex.lisp` `map-vertices`: inside `with-read-pin`, bind `as-of` and thread it:

```lisp
    (with-read-pin (graph)        ; retain whatever versions this scan observes
      (let ((as-of (%as-of-snapshot graph)))  ; GH #115 spec §3.3
        (flet ((scan-type-id (type-id)
                 (let ((index-list (get-type-index-list (vertex-index graph)
                                                        type-id)))
                   (when index-list
                     (map-index-list
                      (lambda (id) ...unchanged...)
                      index-list
                      ;; Under as-of, tombstoned entries too: each id
                      ;; resolves at E, and the deleted filter above runs
                      ;; on THAT version.
                      :include-deleted-p (not (null as-of)))))))
          (let ((requested ...))
            (if requested
                ...unchanged...
                (progn
                  (when as-of
                    (error 'as-of-refused :graph graph
                                          :epoch (as-of-epoch as-of)
                                          :reason :untyped-scan))
                  (map-lhash ...unchanged...)))))))
```

`%as-of-snapshot` and `as-of-epoch` live in `transactions.lisp`, loaded later; calls resolve at run time exactly as `pin-read-epoch` does from `graph-class.lisp`.

`edge.lisp` `map-edges`: bind `(as-of (%as-of-snapshot graph))` inside `with-read-pin`; add `:include-deleted-p (not (null as-of))` to the three `map-index-list` calls (vev, ve, type-index branches); in the `t` (untyped) branch signal `as-of-refused :reason :untyped-scan` first. `edge-exists-p`: same `:include-deleted-p (not (null (%as-of-snapshot graph)))` on its `map-index-list`.

Add to both docstrings' NOTE paragraphs one sentence: "Under an as-of snapshot (WITH-AS-OF, GH #115) the untyped scan is refused."

- [ ] **Step 4: Run the new tests and the `mvcc-suite` + `multi-graph-suite`**

Expected: PASS; counts ≥ baseline.

- [ ] **Step 5: Commit**

```bash
git add vertex.lisp edge.lisp tests/mvcc-tests.lisp tests/multi-graph-tests.lisp
git commit -m "feat(mvcc): membership at an epoch by tombstone walk (#115)

Typed scans and adjacency under an as-of snapshot visit tombstoned index
entries and resolve each id at E; the untyped scan is refused (spec §3.3,
R6)."
```

---

### Task 5: Per-call `:as-of`

**Files:**
- Modify: `vertex.lisp` `lookup-vertex` (~107), `map-vertices` lambda list
- Modify: `edge.lisp` `lookup-edge` (~141), `map-edges` lambda list
- Test: `tests/mvcc-tests.lisp` (append)

**Interfaces:**
- Produces: `:as-of`, `:if-reaped` keywords on `lookup-vertex`, `lookup-edge`, `map-vertices`, `map-edges`.

- [ ] **Step 1: Write the failing test**

```lisp
(test per-call-as-of-opens-a-snapshot-for-the-call
  "Spec §3.4: :AS-OF on a lookup or scan answers at E for that call, the
result outlives the call, and inside an as-of extent at the same epoch it
inherits."
  (with-kept-graph (g 3)
    (let (id e1)
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 5)
      (let ((old (lookup-vertex id :as-of e1)))
        (is (= 0 (slot-value old 'age)) "the version at E1, materialised")
        (is (null graph-db:*read-snapshots*) "the snapshot closed"))
      (is (= 5 (slot-value (lookup-vertex id) 'age)))
      (is (equal '(0) (map-vertices (lambda (v) (slot-value v 'age)) g
                                    :collect-p t :vertex-type 'g-person
                                    :as-of e1)))
      (with-as-of ((g) e1)
        (is (= 0 (slot-value (lookup-vertex id :as-of e1) 'age))
            "same epoch inherits"))
      (signals as-of-refused (lookup-vertex id :as-of (1+ (latest-epoch g))))
      (signals as-of-refused
        (with-as-of ((g) e1) (lookup-vertex id :as-of (latest-epoch g)))))))
```

- [ ] **Step 2: Run to verify it fails**

Expected: unknown keyword `:as-of` on `lookup-vertex`.

- [ ] **Step 3: Implement**

`vertex.lisp`:

```lisp
(defmethod lookup-vertex ((id string) &key (graph *graph*) as-of
                                        (if-reaped :error))
  (lookup-vertex (read-id-array-from-string id)
                 :graph graph :as-of as-of :if-reaped if-reaped))

(defmethod lookup-vertex ((id array) &key (graph *graph*) as-of
                                       (if-reaped :error))
  "...existing docstring, plus:  :AS-OF EPOCH answers at that epoch under
a per-call snapshot (GH #115; see WITH-AS-OF for the refusals); the
result is materialised so it is safe to use after the call."
  (if as-of
      (call-with-read-snapshot
       (lambda ()
         (let ((v (lookup-object id (vertex-table graph) *transaction*
                                 graph)))
           (when v (ensure-node-bytes v graph))
           v))
       graph :as-of as-of :if-reaped if-reaped)
      (lookup-object id (vertex-table graph) *transaction* graph)))
```

`lookup-edge` in `edge.lisp` identically with `edge-table`. `map-vertices` and `map-edges`: add `as-of (if-reaped :error)` to the lambda list and, as the first form of the body:

```lisp
  (when as-of                           ; GH #115 spec §3.4
    (return-from map-vertices
      (call-with-read-snapshot
       (lambda ()
         (map-vertices fn graph :collect-p collect-p :vertex-type vertex-type
                       :include-vertex-types include-vertex-types
                       :exclude-vertex-types exclude-vertex-types
                       :include-deleted-p include-deleted-p
                       :include-subclasses-p include-subclasses-p
                       :record-reads record-reads))
       graph :as-of as-of :if-reaped if-reaped)))
```

(`map-edges` passes every one of its own keywords the same way.)

- [ ] **Step 4: Run the test and the `mvcc-suite`**

- [ ] **Step 5: Commit** — `feat(mvcc): :as-of on lookups and scans (#115)` with trailers.

---

### Task 6: `select :as-of`

**Files:**
- Modify: `prologc.lisp` `select` docstring (~1047–1070) and the `:snapshot` expansion (~1133)
- Test: `tests/mvcc-tests.lisp` (append)

- [ ] **Step 1: Write the failing tests**

```lisp
(test select-as-of-runs-the-query-at-an-epoch
  "Spec §3.5: SELECT :AS-OF E parallels :SNAPSHOT T and equals the same
query run at E; both together is a macroexpansion-time error."
  (with-kept-graph (g 3)
    (let (e1)
      (setq e1 (%epoch-of (lambda () (make-g-person :name "a" :age 1))))
      (with-transaction () (make-g-person :name "b" :age 2))
      (is (= 1 (length (select (:as-of e1) (?p) (is-a ?p g-person)))))
      (is (= 2 (select-count (?p) (is-a ?p g-person))))
      (is (equal '("a")
                 (select (:as-of e1 :flat t) (?n)
                   (is-a ?p g-person) (node-slot-value ?p name ?n))))
      (signals error
        (macroexpand-1 '(select (:snapshot t :as-of 1) (?p)
                          (is-a ?p g-person)))))))
```

- [ ] **Step 2: Run to verify it fails** — the `:as-of` option is ignored today, so the count reads 2.

- [ ] **Step 3: Implement**

Replace the `,(if (cdr (assoc :snapshot options)) ...)` form:

```lisp
              ,(let ((as-of (cdr (assoc :as-of options)))
                     (snapshot (cdr (assoc :snapshot options))))
                 (when (and as-of snapshot)
                   (error ":SNAPSHOT and :AS-OF are exclusive on SELECT ~
(GH #115)"))
                 (cond
                   (as-of
                    ;; GH #115: the query at a named epoch.
                    `(call-with-read-snapshot
                      (lambda () (funcall func #'prolog-ignore)) *graph*
                      :as-of ,as-of
                      :if-reaped ,(or (cdr (assoc :if-reaped options))
                                      :error)))
                   (snapshot
                    ...the existing :snapshot form...)
                   (t `(funcall func #'prolog-ignore))))
```

Docstring, after the `:SNAPSHOT t` sentence: ":AS-OF EPOCH runs the query as of that epoch (GH #115, WITH-AS-OF) -- exclusive with :SNAPSHOT; :IF-REAPED :SKIP skips reaped versions instead of signalling."

- [ ] **Step 4: Run the test, the `mvcc-suite` and the `query-suite`**

- [ ] **Step 5: Commit** — `feat(query): select :as-of (#115)` with trailers.

---

### Task 7: History for edges and nodes

**Files:**
- Modify: `transactions.lisp` `vertex-history` (~671) → `%node-history`, `edge-history`, `node-history`
- Modify: `package.lisp`, `tests/package.lisp`
- Test: `tests/mvcc-tests.lisp` (append)

**Interfaces:**
- Produces: `(edge-history graph id &key limit)`, `(node-history node &key limit)`.

- [ ] **Step 1: Write the failing test**

```lisp
(test edge-and-node-history-walk-the-chain-newest-first
  "Spec §4: EDGE-HISTORY is VERTEX-HISTORY's edge twin; NODE-HISTORY
dispatches on the node's class; entries are (VERSION . COMMIT-EPOCH)
newest first."
  (with-kept-graph (g 3)
    (let (aid eid e1 e2)
      (setq e1 (%epoch-of
                (lambda ()
                  (let ((a (make-g-person :name "a" :age 1))
                        (b (make-g-person :name "b" :age 2)))
                    (setq aid (id a))
                    (setq eid (id (make-g-knows :from a :to b :since 1)))))))
      (setq e2 (%epoch-of
                (lambda ()
                  (let ((c (copy (lookup-edge eid))))
                    (setf (slot-value c 'since) 2)
                    (save c)))))
      (let ((h (edge-history g eid)))
        (is (= 2 (length h)))
        (is (equal (list e2 e1) (mapcar #'cdr h)) "newest first")
        (is (equal '(2 1) (mapcar (lambda (p) (slot-value (car p) 'since)) h)))
        (is (equal (mapcar #'cdr h)
                   (mapcar #'cdr (node-history (lookup-edge eid))))))
      (is (equal (mapcar #'cdr (vertex-history g aid))
                 (mapcar #'cdr (node-history (lookup-vertex aid)))))
      (is (= 1 (length (edge-history g eid :limit 1)))))))
```

Add `#:edge-history #:node-history` to `tests/package.lisp`.

- [ ] **Step 2: Run to verify it fails** — unexported `edge-history`.

- [ ] **Step 3: Implement**

Turn `vertex-history`'s body into `%node-history`:

```lisp
(defun %node-history (graph id table deserializer &key limit)
  "VERTEX-HISTORY / EDGE-HISTORY: the retained versions of ID in TABLE,
newest first, archived heads read with DESERIALIZER (GH #115 spec §4)."
  (when (and limit (<= limit 0))
    (return-from %node-history nil))
  (let ((*graph* graph)
        (key (if (stringp id) (read-id-array-from-string id) id)))
    (with-read-pin (graph)
      (let ((live (lookup-node table key graph)))
        (when (node-p live)
          (ensure-node-bytes live graph)
          (maybe-init-node-data live :graph graph)
          (let ((history (list (cons live (commit-epoch live))))
                (count 1)
                (p (prev-pointer live)))
            (loop
              (when (or (zerop p) (and limit (>= count limit)))
                (return))
              (let ((version (funcall deserializer (heap graph) p)))
                (setf (id version) key)
                (ensure-node-bytes version graph)
                (maybe-init-node-data version :graph graph)
                (push (cons version (commit-epoch version)) history)
                (incf count)
                (setf p (prev-pointer version))))
            (nreverse history)))))))

(defun vertex-history (graph id &key limit)
  "...the existing docstring unchanged..."
  (%node-history graph id (vertex-table graph) #'deserialize-vertex-head
                 :limit limit))

(defun edge-history (graph id &key limit)
  "VERTEX-HISTORY for an edge: the retained versions of edge ID in GRAPH,
\(VERSION . COMMIT-EPOCH) newest first, live first; NIL if none.  Same
bounds and traps as VERTEX-HISTORY -- depth is :KEEP-REVISIONS, and a
short history does not mean few edits (GH #115)."
  (%node-history graph id (edge-table graph) #'deserialize-edge-head
                 :limit limit))

(defgeneric node-history (node &key limit)
  (:documentation "VERTEX-HISTORY or EDGE-HISTORY of NODE in its home
graph (or *GRAPH* when unstamped), by NODE's class (GH #115).")
  (:method ((node vertex) &key limit)
    (vertex-history (or (node-graph node) *graph*) (id node) :limit limit))
  (:method ((node edge) &key limit)
    (edge-history (or (node-graph node) *graph*) (id node) :limit limit)))
```

Export `edge-history`, `node-history` beside `vertex-history` in `package.lisp`.

- [ ] **Step 4: Run the test and the `mvcc-suite`** (the existing `vertex-history-walks-the-mvcc-chain` in `multi-graph-suite` too).

- [ ] **Step 5: Commit** — `feat(mvcc): edge-history and node-history (#115)` with trailers.

---

### Task 8: Composition and the claim layer

**Files:**
- Test: `tests/spacetime/epoch-tests.lisp` (append)

**Interfaces:**
- Consumes: `graph-db:with-as-of`, `graph-db:*read-snapshots*`, `claims-touching`, `claim-commit-epoch`, `claim-current-p`.

- [ ] **Step 1: Write the tests**

```lisp
(test as-of-snapshots-compose-across-clocked-stores
  "Spec §6 composition: WITH-AS-OF on two stores under one clock holds one
entry per store, each answering at its own epoch; both are gone after."
  (with-clocked-stores (a b)
    (let* ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
           (eb (%tx b (lambda () (%unary b #'make-eb-claim-unary "x"))))
           (e2 (%tx a (lambda () (retract-claim (%one a 'ea-claim "r1"))))))
      (is (< e1 eb e2) "control")
      (graph-db:with-as-of ((a) e1)
        (graph-db:with-as-of ((b) (1- eb))
          (is (= 2 (hash-table-count graph-db:*read-snapshots*)))
          (is (claim-current-p (%one a 'ea-claim "r1"))
              "A at E1: not yet retracted")
          (is (null (claims-touching b 'eb-claim :region "x" :role :subject))
              "B one epoch before its claim: absent")))
      (is (null graph-db:*read-snapshots*) "both entries released")
      (is (not (claim-current-p (%one a 'ea-claim "r1"))) "live: retracted"))))

(test claim-reads-under-as-of-agree-with-the-epoch-axis
  "Spec §3.6: under WITH-AS-OF a claim created after E drops out, one
retracted after E reads as its version at E, and :AS-OF-EPOCH on the same
store picks the same version (CLAIM-COMMIT-EPOCH agrees)."
  (with-clocked-stores (a b)
    (let* ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
           (eb (%tx b (lambda () (%unary b #'make-eb-claim-unary "x"))))
           (e2 (%tx a (lambda () (retract-claim (%one a 'ea-claim "r1"))))))
      (declare (ignorable eb))
      (graph-db:with-as-of ((a) (1- e1))
        (is (null (claims-touching a 'ea-claim :region "r1" :role :subject))
            "created after E: dropped"))
      (graph-db:with-as-of ((a) e1)
        (let ((snap (%one a 'ea-claim "r1"))
              (axis (first (claims-touching a 'ea-claim :region "r1"
                                            :role :subject
                                            :as-of-epoch e1))))
          (is (claim-current-p snap) "the version at E1 is unretracted")
          (is (= e1 (claim-commit-epoch snap)))
          (is (= (claim-commit-epoch snap) (claim-commit-epoch axis))
              "the snapshot and the epoch axis choose the same version")))
      (is (= e2 (claim-commit-epoch (%one a 'ea-claim "r1"))) "live: E2"))))
```

- [ ] **Step 2: Run the spacetime suite via the runner**

Expected: PASS. If `claims-touching` under the snapshot returns the retracted live version at E1, its candidate read bypasses `lookup-vertex` — find the read (`index-lookup` → `%node-by-id`, `spatial-query.lisp`) and report; the spec requires the lookup path.

- [ ] **Step 3: Commit** — `test(spacetime): as-of composition and claim reads (#115)` with trailers.

---

### Task 9: Documentation

**Files:**
- Create: `docs/time-travel.md`
- Modify: `docs/mvcc-phase-c-plan.md` (Status, §C-3), `docs/transaction-time-design.md` ("The two axes"), `docs/vivace-graph-v3-doc.org` (after "Snapshot-isolation reads", ~2905), `README.md` (line 6 MVCC sentence), `CHANGELOG.md` (Unreleased/Added, first entry), the spec (§2.2 table, §3.2)

- [ ] **Step 1: `docs/time-travel.md`**

```markdown
# Time travel: reading the graph as of an epoch

GH #115 (Phase C-3 of #117). Spec: `superpowers/specs/2026-09-07-time-travel-api-design.md`.

## The epoch model

Every commit takes the next epoch from the store's transaction manager
(or from the `system-clock` the store is attached to). A node's
`commit-epoch` is the epoch of the transaction that wrote that version;
`:keep-revisions` says how many prior versions the reaper retains. An
epoch names a point in **this node's own history** (#116): it is not
portable to a replica, and it compares across stores only while they
share one system clock.

## Reading as of an epoch

    (with-as-of ((graph) epoch) ...)          ; an extent
    (lookup-vertex id :as-of epoch)           ; one call
    (map-vertices fn graph :vertex-type 'user :as-of epoch)
    (select (:as-of epoch) (?u) (is-a ?u user))
    (latest-epoch graph)                      ; the newest epoch you may name

Inside the extent every read of GRAPH resolves to the version whose
commit epoch is the newest **at or below** EPOCH (inclusive, as
`claims-touching :as-of-epoch` reads it). A node created after EPOCH
is absent; one deleted after EPOCH is present; typed scans, adjacency
(`outgoing-edges`, `traverse`), the generated lookup functions and the
Prolog functors all answer at EPOCH. Snapshots on several graphs
compose by nesting, one epoch per graph.

Refused (`as-of-refused`, with a `reason`): an epoch above
`latest-epoch`; an extent inside a read-write transaction on the graph;
a second epoch inside an open snapshot of the graph (the same epoch
inherits); the untyped `map-vertices`/`map-edges` scan, which reads live
versions; and the in-memory backend, which keeps no history.

## Depth: keep-revisions is the depth of time travel

The default `:keep-revisions 0` keeps the live version and one lagging
version; an as-of read older than that on an updated node cannot be
answered. The engine says so rather than substituting a newer version:
`version-reaped-error` names the id, the epoch asked for, and the oldest
retained epoch. `:if-reaped :skip` on the extent skips such nodes and
counts them (`as-of-skipped-count`). Absence is told from reaping by the
oldest retained version's `revision`: 0 means the node was created
after the epoch. Once that creation version is itself reaped the two
cannot be told apart, and the read reports reaped.

An open as-of extent holds the reaper's floor at its epoch, so versions
live then are retained on any node updated meanwhile; versions reaped
before the extent opened are gone.

## History

`vertex-history`, `edge-history` and `node-history` return the retained
versions newest first as `(version . commit-epoch)`. A history never
signals; an oldest entry with `revision` above 0 means the chain was cut.

## Bounds

- Value indexes (slot indexes, the spacetime endpoint and producer
  indexes) keep live membership (#345): a node hard-removed from one
  after the epoch is not enumerated from it.
- Cost: membership at an epoch walks every entry ever indexed under a
  type, including tombstones, and resolves each id. Phase C-1 (#113)
  replaces that walk with epoch-stamped index entries; the API above
  does not change when it lands.
```

- [ ] **Step 2: The other documents**

- `docs/mvcc-phase-c-plan.md` Status: append "2026-09-07: C-3 shipped ahead of C-0–C-2 on the existing chains and tombstones (`docs/time-travel.md`, spec 2026-09-07); C-0–C-2 are the performance track behind an unchanged API." §C-3: replace items 1–4 with "Shipped; see `docs/time-travel.md`."
- `docs/transaction-time-design.md` "The two axes": one sentence — "The engine-level read of a whole graph as of an epoch is `with-as-of` (`docs/time-travel.md`, GH #115)."
- `docs/vivace-graph-v3-doc.org`: after "Snapshot-isolation reads", a `*** Reading as of an epoch` subsection of ~15 lines summarising `docs/time-travel.md` with one `#+BEGIN_SRC lisp` example (`with-as-of`, `latest-epoch`, `version-reaped-error`).
- `README.md` line 6: after "snapshot-isolation reads (see Chapter 12 of the manual)" add ", and a node-local as-of read of the graph at any retained epoch (`with-as-of`, `docs/time-travel.md`)".
- `CHANGELOG.md`, first entry under Unreleased/Added:

```markdown
- **Node-local time travel** (#115, Phase C-3): `with-as-of`, `:as-of`
  on `lookup-vertex`/`lookup-edge`/`map-vertices`/`map-edges` and on
  `select`, `latest-epoch`, `edge-history` and `node-history`. An as-of
  read is a read-only snapshot started at E+1, so it answers the version
  whose commit epoch is the newest at or below E, and membership --
  typed scans, adjacency, the generated functors -- is reconstructed by
  walking index tombstones. A version reaped past `:keep-revisions` is
  reported as `version-reaped-error` (or skipped and counted under
  `:if-reaped :skip`), never substituted; absent is told from reaped by
  the oldest retained revision. Refused with `as-of-refused` for a
  future epoch, inside a read-write transaction, inside a snapshot at
  another epoch, for the untyped scan and on a memory graph. Epochs are
  node-local (#116). No storage format change. `docs/time-travel.md`.
```

- Spec amendments: §2.2 table gains a row "`:no-version-history` | also a graph with no transaction manager yet"; §3.2 second paragraph becomes "A `:keep-revisions` of 0 (the default) keeps the live version and the one lagging version the committing transaction's own floor retains, so it answers the latest epoch and at most one behind."

- [ ] **Step 3: Commit** — `docs: node-local time travel (#115)` with trailers.

---

### Task 10: Whole-branch verification

- [ ] **Step 1: Column check on every touched Lisp file**

```bash
awk 'length > 80 {print FILENAME":"FNR}' transactions.lisp vertex.lisp edge.lisp prologc.lisp package.lisp tests/mvcc-tests.lisp tests/multi-graph-tests.lisp tests/spacetime/epoch-tests.lisp tests/package.lisp
```

- [ ] **Step 2: The four suites via the runner** — `exit=0`, every count ≥ baseline.

- [ ] **Step 3: Diffstat sanity** — `git diff --stat 1973fa6..HEAD`; no test file shrinks (`git diff 1973fa6..HEAD -- tests | grep -c '^-[^-]'` explains every removed line).

- [ ] **Step 4: Final whole-branch review** (SDD: most capable model), then hand the branch to the maintainer for push authorisation. Not pushed by this plan.
