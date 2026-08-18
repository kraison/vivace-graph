# Transaction time on the claim record — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every claim records when it was recorded, on a second temporal
extent, without disturbing what `claim-extent` means today.

**Architecture:** One new persistent slot on `+claim-shared-slots+` holding
the same versioned extent sexp the validity axis uses, stamped in the
`MAKE-<class>` wrapper `def-claim-classes` already emits. The value is an
interval open at the top, so later retention closes it rather than reshaping
it. Open-versus-unknown rides on the extent's own standing.

**Tech Stack:** Common Lisp (SBCL 2.6.6), FiveAM, `local-time`, ASDF.

**Spec:** `docs/superpowers/specs/2026-08-18-transaction-time-design.md`
(committed `20136c4`). Executors read both; the plan argues from the spec.

## Global Constraints

- **80 columns, hard**, counted in codepoints not bytes — these files
  contain `⚠` and other multibyte characters. A 96-column line is a defect.
- **Spaces only, never tabs.** Tab-width 8 if converting.
- **Comments are terse and point elsewhere** — a doc, a GH issue, a SHA.
  Do not narrate reasoning in source; that belongs in `docs/` or an issue.
- **Branch `experiment`.** Base for Task 1 is `20136c4`. Do not push; do not
  bump the version.
- **Suite baselines to preserve:** spacetime `1133` checks (1133 pass, 0
  skip, 0 fail); main suite `3667` checks (3657 pass, 10 skip, 0 fail).
- **Never run two SBCLs at once.** They share one FASL cache and concurrent
  builds have already cost this programme real time. Run suites detached to
  a log file and poll it — `timeout N sbcl ... | tail` against a
  fifteen-minute suite produces a killed process and nothing to read.
- **Run the task's own tests by name during the RED/GREEN cycle** with
  `fiveam:run!` on individual test symbols. Run the full suite once, at the
  end of the task.
- **A test whose whole value is that it would fail must be shown to fail.**
  Two tests in the last unit read as correct while pinning nothing.

---

### Task 1: Measure what a persistent class gaining a slot actually costs

**This is a gate, not a feature.** The spec's absence handling assumes an
old node simply lacks the key and reads `NIL`. That assumption is
load-bearing for the entire migration story, and #144 is evidence this area
has sharp edges. Measure it before writing any production code. **Nothing
from this task is committed as production code.**

**Files:**
- Create (throwaway): `/tmp/tt-schema-probe.lisp`

**Interfaces:**
- Consumes: nothing.
- Produces: a recorded go/no-go answer for Tasks 2-5, written into the
  progress ledger.

- [ ] **Step 1: Write the probe**

The experiment adds a slot to a claim family *between sessions* using the
same mechanism `+claim-shared-slots+` would — `def-claim-classes` re-emitting
a `def-vertex` with one more slot — against a store written before it
existed. It touches no production file.

```lisp
;;;; Throwaway probe for #148 Task 1.  Not committed.
(ql:register-local-projects)
(ql:quickload :graph-db/spacetime-test)
(in-package :graph-db/spacetime-test)

(defparameter *probe-dir* "/tmp/tt-probe-store/")
(defparameter *probe-graph* :tt-probe-graph)

(setf (gethash *probe-graph* graph-db::*schema-node-metadata*) nil)

;; PHASE A: a family WITHOUT the extra slot, written to disk.
(def-claim-classes tp-claim :tt-probe-graph)

(ensure-directories-exist *probe-dir*)
(let ((g (make-graph *probe-graph* *probe-dir* :buffer-pool-size 1000)))
  (unwind-protect
       (let ((graph-db:*graph* g))
         (with-transaction ()
           (make-tp-claim-unary :subject-namespace :ns :subject-key "s1"
                                :relation :r :producer :p
                                :standing :inferred)))
    (close-graph g)))
(format t "~&PHASE-A-WROTE-STORE~%")

;; PHASE B: the SAME family gains a slot, then the old store is reopened.
(def-claim-classes tp-claim :tt-probe-graph
  :extra-slots ((recorded-probe :initarg :recorded-probe
                                :accessor tp-recorded-probe
                                :initform nil)))

(handler-case
    (let ((g2 (open-graph *probe-graph* *probe-dir*)))
      (unwind-protect
           (let ((graph-db:*graph* g2))
             (let ((c (first (claims-touching g2 'tp-claim :ns "s1"))))
               (format t "~&PROBE-OPENED: t~%")
               (format t "PROBE-CLAIM-FOUND: ~a~%" (and c t))
               (format t "PROBE-OLD-SLOT: ~s~%" (and c (claim-standing c)))
               (format t "PROBE-NEW-SLOT: ~s~%"
                       (and c (tp-recorded-probe c)))
               (format t "PROBE-NEW-SLOT-WRITABLE: ~s~%"
                       (handler-case
                           (progn (with-transaction ()
                                    (let ((copy (graph-db::copy c)))
                                      (setf (tp-recorded-probe copy) :x)
                                      (graph-db::save copy)))
                                  :ok)
                         (error (e) (list :signalled (type-of e)))))))
        (ignore-errors (close-graph g2))))
  (error (e)
    (format t "~&PROBE-OPEN-FAILED: ~a: ~a~%" (type-of e) e)))
(format t "~&PROBE-DONE~%")
```

- [ ] **Step 2: Run it from a clean store directory**

```bash
rm -rf /tmp/tt-probe-store
cd /home/raison/work/vivace-graph-v3
nohup sbcl --dynamic-space-size 16384 --non-interactive \
  --load /tmp/tt-schema-probe.lisp > /tmp/tt-probe.log 2>&1 &
echo "pid $!"
```

Poll `/tmp/tt-probe.log` until `PROBE-DONE` or the process exits.

- [ ] **Step 3: Record the answer, and STOP if it is unfriendly**

Expected benign outcome:

```
PROBE-OPENED: t
PROBE-CLAIM-FOUND: T
PROBE-OLD-SLOT: :INFERRED
PROBE-NEW-SLOT: NIL
PROBE-NEW-SLOT-WRITABLE: :OK
```

**If `PROBE-OPEN-FAILED` appears, or the old slot reads wrong, or the new
slot is not writable on an old node: STOP and report.** The design's
migration story is then false and the spec needs revisiting before any code
is written. Do not attempt a workaround inside this plan.

- [ ] **Step 4: Record the finding in the ledger and clean up**

Write the measured output verbatim into the progress ledger. Then:

```bash
rm -rf /tmp/tt-probe-store /tmp/tt-schema-probe.lisp
```

No commit — this task produces knowledge, not code.

---

### Task 2: The slot, its accessors, and its exports

**Files:**
- Modify: `spacetime/claim.lisp` (`+claim-shared-slots+`, around line 44)
- Modify: `spacetime/claim-query.lisp` (accessors, after `claim-extent`)
- Modify: `spacetime/package.lisp` (exports, the claim block)
- Create: `tests/spacetime/claim-transaction-tests.lisp`
- Modify: `graph-db.asd` (`graph-db/spacetime-test` components)

**Interfaces:**
- Consumes: `make-interval`, `exact-bound`, `unknown-bound`,
  `extent->sexp`, `sexp->extent`, `extent-start`, `extent-standing`,
  `bound-earliest` — all already exported from `graph-db.spacetime`.
- Produces, relied on by Tasks 3-5:
  - `claim-transaction-extent-sexp` (accessor, raw slot)
  - `(claim-transaction-extent claim)` → `temporal-extent` or `NIL`
  - `((setf claim-transaction-extent) extent claim)` → `extent`
  - `(claim-recorded-at claim)` → `(values timestamp-or-nil standing)`

- [ ] **Step 1: Write the failing tests**

Create `tests/spacetime/claim-transaction-tests.lisp`:

```lisp
;;;; The transaction-time axis on the claim record (GH #148).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test a-transaction-extent-round-trips-through-the-slot
  "The second axis reuses the validity axis's codec, so a bug fixed in one
is fixed in both (design, The record)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((e (make-interval (exact-bound (local-time:now))
                            (unknown-bound)
                            :semantics :transaction
                            :standing :asserted)))
      (with-transaction ()
        (make-u :subject "s1"))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent copy) e)
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (is (eq :interval (extent-kind (claim-transaction-extent c2))))
          (is (eq :transaction
                  (extent-semantics (claim-transaction-extent c2))))
          (is (eq :asserted
                  (extent-standing (claim-transaction-extent c2)))))))))

(test the-two-axes-are-independent
  "⚠ Both slots hold the same sexp shape, so an accessor that read the
wrong one would decode perfectly and be invisible.  This pins which slot
each accessor touches."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((validity (make-interval (exact-bound (local-time:now))
                                   (unknown-bound)
                                   :semantics :validity))
          (txn (make-interval (exact-bound (local-time:now))
                              (unknown-bound)
                              :semantics :transaction
                              :standing :asserted)))
      (with-transaction () (make-u :subject "s1"))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-extent copy) validity)
            (setf (claim-transaction-extent copy) txn)
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (is (eq :validity (extent-semantics (claim-extent c2))))
          (is (eq :transaction
                  (extent-semantics (claim-transaction-extent c2)))))))))

(test claim-recorded-at-reports-the-timestamp-and-the-standing
  "The common case is a point; without this every consumer reaches through
EXTENT-START into BOUND-EARLIEST."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((then (local-time:now)))
      (with-transaction () (make-u :subject "s1"))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent copy)
                  (make-interval (exact-bound then) (unknown-bound)
                                 :semantics :transaction
                                 :standing :asserted))
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (multiple-value-bind (ts standing) (claim-recorded-at c2)
            (is (local-time:timestamp= then ts))
            (is (eq :asserted standing))))))))

(test a-transaction-extent-survives-a-close-and-reopen
  "⚠ The in-session read is not the test.  The node cache has made two
tests in this programme vacuous by serving the right answer from memory."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (then (local-time:now)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () (make-u :subject "s1"))
               (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
                 (with-transaction ()
                   (let ((copy (graph-db::copy c)))
                     (setf (claim-transaction-extent copy)
                           (make-interval (exact-bound then) (unknown-bound)
                                          :semantics :transaction
                                          :standing :asserted))
                     (graph-db::save copy)))))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (let ((c (first (claims-touching g2 'ct-claim :ns "s1"))))
                 (is (local-time:timestamp= then (claim-recorded-at c)))))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))
```

- [ ] **Step 2: Run them and confirm they fail for the right reason**

```bash
cd /home/raison/work/vivace-graph-v3
nohup sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  > /tmp/tt-task2-red.log 2>&1 &
```

Expected: the file does not compile — `CLAIM-TRANSACTION-EXTENT` is
undefined. That is the correct RED. **Record what you actually observed**,
not what you expected.

- [ ] **Step 3: Add the slot**

In `spacetime/claim.lisp`, inside `+claim-shared-slots+`, immediately after
the `extent-sexp` entry:

```lisp
    ;; The transaction axis (GH #148).  Same codec as EXTENT-SEXP; the two
    ;; never share a name so neither is mistaken for the other.
    (transaction-extent-sexp :initarg :transaction-extent-sexp
                             :accessor claim-transaction-extent-sexp
                             :initform nil)
```

- [ ] **Step 4: Add the accessors**

In `spacetime/claim-query.lisp`, after `(setf claim-extent)`:

```lisp
(defun claim-transaction-extent (claim)
  "CLAIM's transaction-time TEMPORAL-EXTENT, decoded, or NIL when the claim
predates the axis (GH #148).  NIL is INDETERMINATE, never the epoch."
  (let ((s (claim-transaction-extent-sexp claim)))
    (when s (sexp->extent s))))

(defun (setf claim-transaction-extent) (extent claim)
  "Store EXTENT as CLAIM's transaction extent.  See CLAIM-EXTENT's SETF for
the slot-mutation contract; the immutability rule is added in Task 4."
  (setf (claim-transaction-extent-sexp claim)
        (and extent (extent->sexp extent)))
  extent)

(defun claim-recorded-at (claim)
  "Two values: when CLAIM was recorded, and that extent's STANDING.  A claim
predating the axis returns (VALUES NIL :INDETERMINATE) -- we do not know
when it was recorded, and that is not the same as the epoch (GH #148)."
  (let ((e (claim-transaction-extent claim)))
    (if (null e)
        (values nil :indeterminate)
        (values (bound-earliest (extent-start e)) (extent-standing e)))))
```

- [ ] **Step 5: Export the three names**

In `spacetime/package.lisp`, in the claim block after `#:claim-extent`:

```lisp
   #:claim-transaction-extent-sexp                     ; GH #148
   #:claim-transaction-extent #:claim-recorded-at
```

- [ ] **Step 6: Register the test file**

In `graph-db.asd`, in the `graph-db/spacetime-test` components list, append
after the last existing spacetime test file:

```lisp
                 (:file "claim-transaction-tests")
```

- [ ] **Step 7: Run the four tests by name and confirm GREEN**

```lisp
(in-package :graph-db/spacetime-test)
(fiveam:run! 'a-transaction-extent-round-trips-through-the-slot)
(fiveam:run! 'the-two-axes-are-independent)
(fiveam:run! 'claim-recorded-at-reports-the-timestamp-and-the-standing)
(fiveam:run! 'a-transaction-extent-survives-a-close-and-reopen)
```

- [ ] **Step 8: Prove `the-two-axes-are-independent` is not vacuous**

Temporarily make `claim-transaction-extent` read `claim-extent-sexp`.
Re-run that test: it MUST go red. Restore, confirm green, and confirm
`git status` is clean before continuing. **Report the observed red**, and if
it does not go red, say so — a crossed accessor is exactly the defect this
test claims to catch.

- [ ] **Step 9: Commit**

```bash
git add spacetime/claim.lisp spacetime/claim-query.lisp \
        spacetime/package.lisp graph-db.asd \
        tests/spacetime/claim-transaction-tests.lisp
git commit -m "feat(spacetime): a transaction-time extent on the claim (#148)

The second temporal axis, reusing the validity axis's codec so a codec bug
is fixed in both.  CLAIM-EXTENT keeps meaning validity; nothing that reads
a claim today changes.

Stamping and immutability follow.  [skip-docs]"
```

- [ ] **Step 10: Run the full spacetime suite once, detached**

Expected: `1133 + <checks you added>`. State the decomposition per test.
Zero failures. If the count does not move by exactly what you added, a file
loaded without entering the suite — check `(in-suite ...)`.

---

### Task 3: Stamp at construction

**Files:**
- Modify: `spacetime/claim.lisp` (new encoder + the `MAKE-<class>` wrapper)
- Modify: `tests/spacetime/claim-transaction-tests.lisp` (append)

**Interfaces:**
- Consumes: `claim-transaction-extent` and friends from Task 2;
  `%plist-key-p`, `%plist-remove`, `%claim-encode-extent-arg` (all already
  in `claim.lisp`).
- Produces: `MAKE-<class>` accepting `:recorded-at <timestamp>` and
  `:transaction-extent <temporal-extent>`; every claim stamped by default.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/claim-transaction-tests.lisp`:

```lisp
(test every-new-claim-is-stamped-without-the-tenant-asking
  "Nothing a tenant does leaves a new claim unstamped (design, Stamping)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (multiple-value-bind (ts standing) (claim-recorded-at c)
        (is (typep ts 'local-time:timestamp))
        (is (eq :asserted standing)))
      (let ((e (claim-transaction-extent c)))
        (is (eq :interval (extent-kind e)))
        (is (eq :transaction (extent-semantics e)))
        (is (eq :unbounded (bound-latest (extent-end e))))))))

(test recorded-at-overrides-the-default-stamp
  "⚠ The timestamp is deliberately far from now: a stamp that ignored the
argument and used the clock would still be a valid timestamp and would
pass a weaker assertion."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((then (local-time:parse-timestring "1999-12-31T23:59:58Z")))
      (with-transaction () (make-u-at :subject "s1" :recorded-at then))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (local-time:timestamp= then (claim-recorded-at c)))))))

(test an-explicit-transaction-extent-is-stored-as-given
  "An ingest path may know a CLOSED period, or a standing other than
:ASSERTED."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((e (make-interval
              (exact-bound (local-time:parse-timestring "2001-01-01T00:00:00Z"))
              (exact-bound (local-time:parse-timestring "2002-01-01T00:00:00Z"))
              :semantics :transaction :standing :indeterminate)))
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation :r :producer :p
                             :standing :inferred
                             :transaction-extent e))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (eq :indeterminate
                (extent-standing (claim-transaction-extent c))))
        (is (not (eq :unbounded
                     (bound-latest
                      (extent-end (claim-transaction-extent c))))))))))

(test conflicting-transaction-initargs-signal
  "Picking one silently is how a caller ends up with a stamp they did not
ask for; parity with :EXTENT versus :EXTENT-SEXP (claim.lisp)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals error
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation :r :producer :p
                             :standing :inferred
                             :recorded-at (local-time:now)
                             :transaction-extent
                             (make-interval (exact-bound (local-time:now))
                                            (unknown-bound)
                                            :semantics :transaction
                                            :standing :asserted))))))
```

Add this helper beside `make-u` in
`tests/spacetime/claim-identity-tests.lisp`:

```lisp
(defun make-u-at (&key (producer :rule-a) (subject "s1") (relation :r)
                       recorded-at)
  "MAKE-U with an explicit transaction stamp (GH #148)."
  (make-ct-claim-unary :subject-namespace :ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred :recorded-at recorded-at))
```

- [ ] **Step 2: Run them and confirm RED**

Expected: the default-stamp test fails because `claim-recorded-at` returns
`NIL`; the initarg tests fail because `:recorded-at` is not a known initarg.
Record what you actually saw.

- [ ] **Step 3: Add the encoder**

In `spacetime/claim.lisp`, after `%claim-encode-extent-arg`:

```lisp
(defun %open-transaction-extent (timestamp)
  "The transaction period [TIMESTAMP, open).  :ASSERTED means still
believed; an end genuinely unknown is :INDETERMINATE (GH #148)."
  (make-interval (exact-bound timestamp) (unknown-bound)
                 :semantics :transaction :standing :asserted))

(defun %claim-encode-transaction-arg (args)
  "Rewrite a claim constructor's ARGS so the transaction axis is always
populated: :TRANSACTION-EXTENT and :RECORDED-AT are encoded to
:TRANSACTION-EXTENT-SEXP, and an unstamped claim is stamped now.  Signals
if more than one form is given, rather than picking one (GH #148)."
  (let ((n (count-if (lambda (k) (%plist-key-p args k))
                     '(:transaction-extent :recorded-at
                       :transaction-extent-sexp))))
    (when (> n 1)
      (error "Pass only one of :TRANSACTION-EXTENT, :RECORDED-AT or ~
:TRANSACTION-EXTENT-SEXP."))
    (cond
      ((%plist-key-p args :transaction-extent)
       (let ((e (getf args :transaction-extent)))
         (list* :transaction-extent-sexp (and e (extent->sexp e))
                (%plist-remove args :transaction-extent))))
      ((%plist-key-p args :recorded-at)
       (let ((ts (getf args :recorded-at)))
         (list* :transaction-extent-sexp
                (extent->sexp (%open-transaction-extent ts))
                (%plist-remove args :recorded-at))))
      ((%plist-key-p args :transaction-extent-sexp) args)
      (t (list* :transaction-extent-sexp
                (extent->sexp (%open-transaction-extent (local-time:now)))
                args)))))
```

- [ ] **Step 4: Wire it into the wrapper**

In `def-claim-classes`, change the constructor body so both encoders run:

```lisp
                         (let ((c (apply %raw
                                        (%claim-encode-transaction-arg
                                         (%claim-encode-extent-arg args)))))
```

- [ ] **Step 5: Run the four tests by name and confirm GREEN**

- [ ] **Step 6: Prove the override test is not vacuous**

Temporarily make the `:recorded-at` branch ignore its argument and stamp
`(local-time:now)`. `recorded-at-overrides-the-default-stamp` MUST go red.
Restore, re-confirm green, confirm `git status` clean.

- [ ] **Step 7: Commit**

```bash
git add spacetime/claim.lisp tests/spacetime/claim-transaction-tests.lisp \
        tests/spacetime/claim-identity-tests.lisp
git commit -m "feat(spacetime): stamp transaction time at construction (#148)

The substrate stamps so nothing is ever unstamped; :RECORDED-AT and
:TRANSACTION-EXTENT let an ingest path record a source system's own time
rather than a falsehood.  [skip-docs]"
```

- [ ] **Step 8: Full spacetime suite, detached, count decomposed**

---

### Task 4: Refuse to overwrite a stamp

**Files:**
- Modify: `spacetime/claim-query.lisp` (`(setf claim-transaction-extent)`)
- Modify: `spacetime/conditions.lisp` (new condition)
- Modify: `spacetime/package.lisp` (export the condition)
- Modify: `tests/spacetime/claim-transaction-tests.lisp` (append)

**Interfaces:**
- Produces: condition `transaction-extent-immutable`, signalled by
  `(setf claim-transaction-extent)` on a claim that already has one.

- [ ] **Step 1: Write the failing tests**

```lisp
(test overwriting-a-stamp-is-refused
  "Transaction time is an audit field.  ⚠ Accessor-level only -- writing
CLAIM-TRANSACTION-EXTENT-SEXP directly still bypasses this, and that limit
is recorded in the design and in #148."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals transaction-extent-immutable
        (setf (claim-transaction-extent c)
              (make-interval (exact-bound (local-time:now))
                             (unknown-bound)
                             :semantics :transaction
                             :standing :asserted))))))

(test a-refused-overwrite-leaves-the-original-stamp
  "⚠ The refusal is only half of it; the store must still hold the
original.  Reopened, not read from the node cache."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (then (local-time:parse-timestring "1999-12-31T23:59:58Z")))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () (make-u-at :subject "s1"
                                               :recorded-at then))
               (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
                 (handler-case
                     (with-transaction ()
                       (let ((copy (graph-db::copy c)))
                         (setf (claim-transaction-extent copy)
                               (%open-transaction-extent (local-time:now)))
                         (graph-db::save copy)))
                   (transaction-extent-immutable () nil))))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (let ((c (first (claims-touching g2 'ct-claim :ns "s1"))))
                 (is (local-time:timestamp= then (claim-recorded-at c)))))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))
```

Note the test calls `graph-db.spacetime::%open-transaction-extent`; add
that internal symbol to the test file's usage with an explicit
double-colon, or export it. **Prefer the double-colon** — it is internal on
purpose.

- [ ] **Step 2: Run and confirm RED** (condition undefined)

- [ ] **Step 3: Define the condition**

In `spacetime/conditions.lisp`, following the file's existing style:

```lisp
(define-condition transaction-extent-immutable (spacetime-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "A claim's transaction extent is written once (GH #148).")))
  (:documentation
   "Signalled when a claim that already carries a transaction extent is
given another.  Accessor-level only; see the design's immutability note."))
```

Export `#:transaction-extent-immutable` in `spacetime/package.lisp` beside
the other conditions.

- [ ] **Step 4: Enforce it in the setter**

Replace `(setf claim-transaction-extent)` from Task 2 with:

```lisp
(defun (setf claim-transaction-extent) (extent claim)
  "Store EXTENT as CLAIM's transaction extent, once.  Signals
TRANSACTION-EXTENT-IMMUTABLE if CLAIM already has one -- an audit field is
written at creation and not revised (GH #148).  Writing
CLAIM-TRANSACTION-EXTENT-SEXP bypasses this; engine-level enforcement waits
on a constraint family (#109)."
  (when (claim-transaction-extent-sexp claim)
    (error 'transaction-extent-immutable))
  (setf (claim-transaction-extent-sexp claim)
        (and extent (extent->sexp extent)))
  extent)
```

⚠ **Tasks 2 and 3's tests set the extent on an already-stamped claim.**
Task 3 made every claim stamped at construction, so those tests now signal.
Rewrite each to construct the claim with `:transaction-extent` or
`:recorded-at` rather than setting it afterwards, and say in the report
which tests you changed and why. This is expected, not a regression.

- [ ] **Step 5: Run the whole transaction test file and confirm GREEN**

- [ ] **Step 6: Prove the refusal is load-bearing**

Temporarily remove the `when` guard. `overwriting-a-stamp-is-refused` and
`a-refused-overwrite-leaves-the-original-stamp` MUST both go red. Restore,
confirm green, confirm `git status` clean.

- [ ] **Step 7: Commit**

```bash
git add spacetime/claim-query.lisp spacetime/conditions.lisp \
        spacetime/package.lisp tests/spacetime/claim-transaction-tests.lisp
git commit -m "feat(spacetime): a transaction stamp is written once (#148)

Accessor-level refusal.  The raw sexp slot still bypasses it; engine-level
enforcement needs a may-not-change-after-creation constraint family and is
recorded against #109.  [skip-docs]"
```

- [ ] **Step 8: Full spacetime suite, detached, count decomposed**

---

### Task 5: A claim predating the axis reports indeterminate

**Files:**
- Modify: `tests/spacetime/claim-transaction-tests.lisp` (append)

**Interfaces:**
- Consumes: everything from Tasks 2-4. No production change is expected —
  if one is needed, that is a finding, so report it.

- [ ] **Step 1: Write the test**

A claim with no transaction slot cannot be made through the constructor any
more, so write the raw slot to `NIL` directly — the same state an old node
on disk presents.

```lisp
(test a-claim-predating-the-axis-reports-indeterminate-not-the-epoch
  "⚠ The whole migration story rests on this.  NIL must never read as the
epoch: a fabricated audit time is worse than an admitted unknown (#148)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (with-transaction ()
        (let ((copy (graph-db::copy c)))
          ;; The raw slot, which is exactly what an old on-disk node has.
          (setf (claim-transaction-extent-sexp copy) nil)
          (graph-db::save copy)))
      (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (null (claim-transaction-extent c2)))
        (multiple-value-bind (ts standing) (claim-recorded-at c2)
          (is (null ts))
          (is (eq :indeterminate standing))
          (is (not (eq :observed standing))))))))
```

- [ ] **Step 2: Run it**

If it passes immediately, that is expected — Task 2 built the behaviour.
**Then prove it is not vacuous:** temporarily make `claim-recorded-at`
return `(values (local-time:unix-to-timestamp 0) :observed)` for the NIL
case and confirm the test goes red. Restore and re-confirm.

- [ ] **Step 3: Commit**

```bash
git add tests/spacetime/claim-transaction-tests.lisp
git commit -m "test(spacetime): an unstamped claim is indeterminate (#148)

Not the epoch.  Pins the assumption the whole migration story rests on.
[skip-docs]"
```

- [ ] **Step 4: Both suites, detached, one at a time**

Spacetime and main. Report both counts as they print.

---

### Task 6: Documentation and issue closure

**Files:**
- Create: `docs/transaction-time-design.md`
- Modify: `docs/spatiotemporal-substrate-programme.md`

**This is the commit that unblocks pushing** — the docs hook rejects a push
whose source changed without documentation.

- [ ] **Step 1: Write `docs/transaction-time-design.md`**

Model it on `docs/value-constraint-design.md` — read that first and match
its structure and register. Cover, with no placeholders:

- The two axes, and that `claim-extent` still means validity.
- Why the transaction value is an interval and not an instant, and that
  `:unbounded` makes the open period free today.
- Open versus unknown, and that the extent's standing carries it.
- Stamping: substrate by default, `:recorded-at` / `:transaction-extent`
  for ingest, and why an override exists at all.
- The immutability limit, stated plainly: accessor-level only, the raw slot
  bypasses it, engine-level enforcement is #109's next unit.
- That a claim predating the axis reports indeterminate and that **no
  migration rewrites any store**, with the contrast against #149 where
  absence over a `:required` slot was damage.
- The measured answer from Task 1 about a persistent class gaining a slot.
- What this does not do: no retention, no `:as-of`, no index on
  transaction time, no change to the Allen algebra.

- [ ] **Step 2: Note it in the programme doc**

Add a line beside the existing S4 note recording that S1 gained the
transaction axis (#148). Read the file first; match its form.

- [ ] **Step 3: Both suites one last time, detached, one at a time**

Docs-only, so both counts must be UNCHANGED from Task 5's. If either moves,
stop and report rather than rationalising.

- [ ] **Step 4: Commit**

```bash
git add docs/transaction-time-design.md \
        docs/spatiotemporal-substrate-programme.md
git commit -m "docs: transaction time on the claim record (#148)"
```

- [ ] **Step 5: Do not push. Report and ask.**

Pushing is outward-facing and needs Kevin's approval every time. Report both
suite counts and the Task 1 measurement, then stop. Issue closure happens
after approval, not before.

---

## Self-Review

**Spec coverage:**

| Spec section | Task |
|---|---|
| The record — slot, sexp reuse | 2 |
| Accessors, `claim-recorded-at` | 2 |
| `claim-extent` unchanged | 2 (independence test) |
| Interval not instant, open at top | 2, 3 |
| Open versus unknown via standing | 3 (default `:asserted`), 5 |
| Stamping: where, initargs, default | 3 |
| Conflicting initargs signal | 3 |
| Immutability + its honest limit | 4 |
| Absence, legacy claims, no migration | 5 |
| Identity unchanged | none — the existing unique tests pin it |
| MVCC epochs not the value | none — a rejected alternative |
| API surface / exports | 2, 4 |
| Schema-change cost measured | 1 |
| Testing items 1-10 | 1-5; item 10 (`in-suite`) is each task's final full run |
| What it does not do | 6 |

**Placeholder scan:** none. Every code step carries the actual code; the
docs step enumerates required content rather than saying "write docs".

**Type consistency:** `claim-transaction-extent-sexp` (raw),
`claim-transaction-extent` (decoded), `claim-recorded-at` (two values),
`%open-transaction-extent`, `%claim-encode-transaction-arg`,
`transaction-extent-immutable` — spelled identically in Tasks 2-6.

**One deliberate ordering hazard, called out where it bites:** Task 4's
immutability guard breaks tests written in Tasks 2 and 3, because Task 3
makes every claim stamped at construction. Task 4 Step 4 says so and says
what to do. A task that specifies tests a later task breaks is the defect
this programme's pre-flight scan caught last time; here it is unavoidable
(the guard is the deliverable) so it is flagged rather than hidden.
