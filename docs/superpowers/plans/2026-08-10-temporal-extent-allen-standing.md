# Temporal extents, the Allen algebra, and standing — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `graph-db/spacetime`'s temporal half — `standing`, `temporal-extent`, and an Allen algebra that returns the *set* of relations consistent with imprecise endpoints.

**Architecture:** Pure value types and total functions in a new opt-in ASDF subsystem with its own package. An endpoint is a range, not a timestamp, so imprecision, open-endedness and total ignorance are one mechanism. Two extents with exact endpoints yield a singleton relation set; uncertain endpoints yield a larger set that may never omit the truth.

**Tech Stack:** Common Lisp (SBCL), `local-time`, FiveAM. No changes to `graph-db/core`.

**Spec:** `docs/superpowers/specs/2026-08-10-temporal-extent-allen-standing-design.md`
**Issue:** #130. **Parent:** #108.

## Global Constraints

- **80-column hard limit** in all Lisp source: code, comments, docstrings and strings alike. Spaces only, never tabs.
- **Comments are terse and point elsewhere.** State the non-obvious fact and cite `(design §N)` or `(GH #130)`. No essays in source.
- **No change to any file in `graph-db/core`.** The only existing file this plan modifies is `graph-db.asd`, which gains two new `defsystem` forms.
- **Every timestamp construction passes `:timezone local-time:+utc-zone+`** explicitly (design §3.5). An unpinned constructor makes granule boundaries host-dependent.
- **Intervals are closed `[start, end]`** (design §3.2).
- **No ordering operator over `standing` may be defined** (design §4.4). `asserted` and `inferred` cannot be ranked.
- **SBCL only.** ECL is demoted to periodic; say explicitly when it was skipped.
- Work on a branch off `experiment`. Show the full diff under `## 📋 DIFF FOR REVIEW` before each commit. Do not push.
- Verify the tree under test with `(asdf:system-source-directory :graph-db)` — `~/quicklisp/local-projects/graph-db.asd` is a symlink to this checkout.

### Three spec corrections this plan carries

1. **§5's component list gains `conditions.lisp`** (six files, not five). Conditions belong with neither `standing` nor `bound`, and both need them.
2. **Added during execution: `make-interval` must reject a value-degenerate interval.** Both bounds exact and equal is a point in time, which `make-instant` already expresses; the collapsed `:interval` spelling matches no signature row and yields the empty set. Task 6's soundness property found it on its first run — before it reached its own non-vacuity check. The guard and its test were folded into Task 6 rather than re-opening Task 3, and §3.2 of the spec gained the rule.
3. **§3.1's "all thirteen" is exact only for interval-vs-interval.** An *instant* of wholly unknown position against an interval yields the five relations §3.3.1 marks reachable, not thirteen. Fewer, and correct — the instant coupling constrains the answer even when the position does not. Task 5 asserts this.

---

## File Structure

| File | Responsibility |
|---|---|
| `spacetime/package.lisp` | `graph-db.spacetime` package and its exports |
| `spacetime/conditions.lisp` | `spacetime-error` and its three subtypes |
| `spacetime/standing.lisp` | the standing vocabulary and its predicates |
| `spacetime/bound.lisp` | the range type and four-valued comparison |
| `spacetime/extent.lisp` | `temporal-extent`, constructors, granules, sexp codec |
| `spacetime/allen.lisp` | signature table, instant dispatch, relation surface |
| `tests/spacetime/*.lisp` | one test file per source file, plus property + conformance |
| `graph-db.asd` | two new `defsystem` forms |

A dedicated package rather than adding to `graph-db` (which is what `geos/` does): exporting a public API from `graph-db` would mean editing core's `package.lisp`, and `graph-db.projection` is the existing precedent for an add-on owning its own package.

---

### Task 1: Subsystem skeleton, conditions, and `standing`

**Files:**
- Create: `spacetime/package.lisp`, `spacetime/conditions.lisp`, `spacetime/standing.lisp`
- Create: `tests/spacetime/package.lisp`, `tests/spacetime/suite.lisp`, `tests/spacetime/standing-tests.lisp`
- Modify: `graph-db.asd` (append two `defsystem` forms after `graph-db/geos-test`)

**Interfaces:**
- Produces: `+standings+`, `+absence-standings+`, type `standing`, `standingp`, `standing-absence-p`, `standing-present-p`, `check-standing`; conditions `spacetime-error`, `invalid-standing`, `invalid-bound`, `invalid-extent`; test entry point `run-spacetime-tests`.

- [ ] **Step 1: Create the package**

`spacetime/package.lisp`. Export every symbol the whole subsystem will use — later tasks add their definitions, not new export forms.

```lisp
;;;; Package for graph-db/spacetime -- the temporal substrate (GH #130).
;;;;
;;;; Its own package rather than GRAPH-DB: a public API exported from
;;;; GRAPH-DB would mean editing core's package.lisp, and core gains nothing
;;;; from this programme (design §1.2).

(in-package #:cl-user)

(defpackage #:graph-db.spacetime
  (:use #:cl)
  (:export
   ;; conditions
   #:spacetime-error #:invalid-standing #:invalid-bound #:invalid-extent
   ;; standing
   #:standing #:standingp #:standing-absence-p #:standing-present-p
   #:check-standing #:+standings+ #:+absence-standings+
   ;; bound
   #:bound #:bound-p #:make-bound #:exact-bound #:unknown-bound
   #:bound-earliest #:bound-latest #:bound-exact-p #:bound-unknown-p
   #:bound-compare
   ;; extent
   #:temporal-extent #:temporal-extent-p #:+precisions+
   #:make-interval #:make-instant
   #:make-granule-interval #:make-granule-instant #:granule-bounds
   #:extent-kind #:extent-start #:extent-end #:extent-precision
   #:extent-semantics #:extent-standing #:extent-instant-p
   #:extent->sexp #:sexp->extent
   ;; allen
   #:temporal-relation #:temporal-relation-p
   #:temporal-relation-relations #:temporal-relation-standings
   #:temporal-relation-semantics
   #:+allen-relations+ #:+allen-inverses+ #:allen-inverse
   #:allen-relations #:allen-relation #:allen-definite-p
   #:extent-before-p #:extent-meets-p #:extent-overlaps-p
   #:extent-finished-by-p #:extent-contains-p #:extent-starts-p
   #:extent-equals-p #:extent-started-by-p #:extent-during-p
   #:extent-finishes-p #:extent-overlapped-by-p #:extent-met-by-p
   #:extent-after-p))
```

- [ ] **Step 2: Create the conditions**

`spacetime/conditions.lisp`. No forward references to `+standings+` — the report prints the offending value only.

```lisp
;;;; Conditions for graph-db/spacetime (GH #130).

(in-package #:graph-db.spacetime)

(define-condition spacetime-error (error) ()
  (:documentation "Root of every error this subsystem signals."))

(define-condition invalid-standing (spacetime-error)
  ((value :initarg :value :reader invalid-standing-value))
  (:report (lambda (c s)
             (format s "~S is not a standing." (invalid-standing-value c)))))

(define-condition invalid-bound (spacetime-error)
  ((earliest :initarg :earliest :reader invalid-bound-earliest)
   (latest :initarg :latest :reader invalid-bound-latest)
   (reason :initarg :reason :reader invalid-bound-reason))
  (:report (lambda (c s)
             (format s "Bad bound [~S, ~S]: ~A."
                     (invalid-bound-earliest c) (invalid-bound-latest c)
                     (invalid-bound-reason c)))))

(define-condition invalid-extent (spacetime-error)
  ((reason :initarg :reason :reader invalid-extent-reason))
  (:report (lambda (c s)
             (format s "Bad extent: ~A." (invalid-extent-reason c)))))
```

- [ ] **Step 3: Write the failing tests for `standing`**

`tests/spacetime/standing-tests.lisp`:

```lisp
;;;; The standing vocabulary (GH #130, design §3.4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test standing-vocabulary-is-closed
  "Six standings, and nothing else is one."
  (is (= 6 (length +standings+)))
  (is-true (every #'standingp +standings+))
  (is-false (standingp :observedd))
  (is-false (standingp nil))
  (is-false (standingp 0)))

(test absence-is-distinguishable-from-a-value
  "The defect class this type exists to prevent: a never-measured state must
never be confusable with a measured one (design §3.4)."
  (dolist (s '(:searched-empty :uncovered :indeterminate))
    (is-true (standing-absence-p s))
    (is-false (standing-present-p s)))
  (dolist (s '(:observed :inferred :asserted))
    (is-false (standing-absence-p s))
    (is-true (standing-present-p s))))

(test the-three-absence-cases-stay-distinct
  "A source looked and found nothing, no source covers this, and we could not
find out are three different facts.  Collapsing them is the bug."
  (is (= 3 (length (remove-duplicates +absence-standings+))))
  (is (null (set-difference +absence-standings+ +standings+))))

(test standing-has-no-ordering-operator
  "Design §4.4: no defensible total order exists over standings, so this
subsystem must not define one.  An edit that adds one fails here."
  (is-false (find-symbol "STANDING<" :graph-db.spacetime))
  (is-false (find-symbol "STANDING-WEAKEST" :graph-db.spacetime))
  (is-false (find-symbol "STANDING-WEAKER" :graph-db.spacetime)))

(test check-standing-signals-on-a-non-standing
  (signals invalid-standing (check-standing :nope))
  (is (eq :observed (check-standing :observed))))
```

- [ ] **Step 4: Create the test harness**

`tests/spacetime/package.lisp`:

```lisp
;;;; Test package for graph-db/spacetime.
;;;;
;;;; Safe to :USE GRAPH-DB.SPACETIME -- unlike GRAPH-DB, whose test package
;;;; curates an explicit import list, this package is small and ours.

(in-package #:cl-user)

(defpackage #:graph-db/spacetime-test
  (:use #:cl #:fiveam #:graph-db.spacetime)
  (:import-from #:local-time
                #:encode-timestamp #:timestamp< #:timestamp= #:timestamp+
                #:timestamp- #:+utc-zone+ #:*default-timezone*)
  (:export #:run-spacetime-tests #:spacetime-suite))
```

`tests/spacetime/suite.lisp`:

```lisp
;;;; Master suite + runner for the spacetime tests.

(in-package #:graph-db/spacetime-test)

(def-suite spacetime-suite
  :description "Temporal extents, the Allen algebra, and standing (#130).")

(defun run-spacetime-tests ()
  "Run the spacetime suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/spacetime)."
  (log:config :error)
  (let ((results (run 'spacetime-suite)))
    (explain! results)
    (results-status results)))

(defun ts (year month day &optional (hour 0) (minute 0) (sec 0) (nsec 0))
  "A UTC timestamp.  Every test builds times through this, so none of them
can accidentally depend on the host timezone (design §3.5)."
  (encode-timestamp nsec sec minute hour day month year
                    :timezone +utc-zone+))
```

- [ ] **Step 5: Add the ASDF systems**

Append to `graph-db.asd`, after the `graph-db/geos-test` form. List **only** the files that exist; later tasks add their own components.

```lisp
;; OPTIONAL spacetime add-on: temporal extents, the Allen interval algebra,
;; and standing.  Pure value types and total functions -- core graph-db does
;; NOT depend on this, and this reserves no serialize type byte (GH #130).
(defsystem graph-db/spacetime
  :name "VivaceGraph spacetime (temporal substrate)"
  :description "Temporal extents, the Allen interval algebra, and standing."
  :depends-on (:graph-db/core :local-time)
  :pathname "spacetime/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "standing")))

(defsystem graph-db/spacetime-test
  :name "VivaceGraph spacetime test suite"
  :description "FiveAM tests for graph-db/spacetime."
  :depends-on (:graph-db/spacetime :fiveam)
  :pathname "tests/spacetime/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "standing-tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/spacetime-test
                                              :run-spacetime-tests)
                      (error "graph-db spacetime tests failed."))))
```

- [ ] **Step 6: Run the tests to verify they fail**

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp"))' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(graph-db/spacetime-test:run-spacetime-tests)'
```

Expected: the build fails to load `standing.lisp` — the file does not exist yet.

- [ ] **Step 7: Implement `standing`**

`spacetime/standing.lisp`:

```lisp
;;;; The standing vocabulary: how we came to know a thing, including the
;;;; three distinct ways of not knowing it (GH #130, design §3.4).

(in-package #:graph-db.spacetime)

(defparameter +standings+
  '(:observed :inferred :asserted :searched-empty :uncovered :indeterminate)
  "The closed standing vocabulary.  Deliberately UNORDERED: ASSERTED and
INFERRED cannot be ranked, so no comparison operator over standings exists
in this subsystem (design §4.4).")

(defparameter +absence-standings+
  '(:searched-empty :uncovered :indeterminate)
  "The three standings meaning THERE IS NO VALUE, each for a different
reason.  Keeping them apart is the whole point of the type.")

(deftype standing ()
  '(member :observed :inferred :asserted
    :searched-empty :uncovered :indeterminate))

(defun standingp (x)
  "True when X belongs to the standing vocabulary."
  (and (member x +standings+) t))

(defun standing-absence-p (s)
  "True when S records an absence.  An absence is not a weaker value; it is
the state in which there is no interval at all."
  (and (member s +absence-standings+) t))

(defun standing-present-p (s)
  "True when S records a value we hold: OBSERVED, INFERRED or ASSERTED."
  (and (standingp s) (not (standing-absence-p s))))

(defun check-standing (x)
  "Return X when it is a standing; signal INVALID-STANDING otherwise."
  (unless (standingp x)
    (error 'invalid-standing :value x))
  x)
```

- [ ] **Step 8: Run the tests to verify they pass**

Same command as Step 6. Expected: 5 tests, all passing, 0 failures.

- [ ] **Step 9: Commit**

```bash
git add spacetime/ tests/spacetime/ graph-db.asd
git commit -m "feat(spacetime): subsystem skeleton and the standing type (#130)"
```

---

### Task 2: `bound` — ranges and four-valued comparison

**Files:**
- Create: `spacetime/bound.lisp`, `tests/spacetime/bound-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "bound")` and `(:file "bound-tests")`)

**Interfaces:**
- Consumes: `invalid-bound` from Task 1.
- Produces: `make-bound (earliest latest)`, `exact-bound (timestamp)`, `unknown-bound ()`, `bound-earliest`, `bound-latest`, `bound-p`, `bound-exact-p`, `bound-unknown-p`, `bound-compare (a b)` → one of `:<`, `:>`, `:=`, `:ambiguous`.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/bound-tests.lisp`:

```lisp
;;;; Bounds: a range within which one timestamp lies (design §3.2).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test bound-rejects-a-reversed-range
  (signals invalid-bound (make-bound (ts 2026 5 1) (ts 2026 1 1)))
  (signals invalid-bound (make-bound 17 (ts 2026 1 1))))

(test bound-exactness
  (is-true (bound-exact-p (exact-bound (ts 2026 1 1))))
  (is-false (bound-exact-p (make-bound (ts 2026 1 1) (ts 2026 1 2))))
  (is-false (bound-exact-p (unknown-bound)))
  (is-true (bound-unknown-p (unknown-bound)))
  (is-false (bound-unknown-p (exact-bound (ts 2026 1 1)))))

(test bound-compare-is-definite-when-ranges-are-disjoint
  (let ((a (make-bound (ts 2026 1 1) (ts 2026 1 31)))
        (b (make-bound (ts 2026 3 1) (ts 2026 3 31))))
    (is (eq :< (bound-compare a b)))
    (is (eq :> (bound-compare b a)))))

(test bound-compare-is-equal-only-when-both-are-exact
  (let ((a (exact-bound (ts 2026 1 1)))
        (b (exact-bound (ts 2026 1 1)))
        (wide (make-bound (ts 2026 1 1) (ts 2026 1 31))))
    (is (eq := (bound-compare a b)))
    ;; Two ranges that merely COINCIDE are not equal: the timestamps they
    ;; stand for may differ anywhere inside.
    (is (eq :ambiguous (bound-compare wide wide)))))

(test bound-compare-is-ambiguous-when-ranges-overlap
  (let ((a (make-bound (ts 2026 1 1) (ts 2026 2 15)))
        (b (make-bound (ts 2026 2 1) (ts 2026 3 1))))
    (is (eq :ambiguous (bound-compare a b)))
    (is (eq :ambiguous (bound-compare b a)))))

(test unbounded-never-produces-a-verdict-but-does-not-prevent-one
  "Design §3.2: :UNBOUNDED cannot satisfy a strict inequality, but the OTHER
endpoint pair can still settle the comparison."
  (let ((late  (make-bound (ts 2030 1 1) :unbounded))
        (early (make-bound :unbounded (ts 2020 1 1)))
        (any   (unknown-bound)))
    (is (eq :> (bound-compare late early)))
    (is (eq :< (bound-compare early late)))
    (is (eq :ambiguous (bound-compare any late)))
    (is (eq :ambiguous (bound-compare any any)))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp"))' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(graph-db/spacetime-test:run-spacetime-tests)'
```

Expected: load failure — `bound.lisp` does not exist.

- [ ] **Step 3: Implement `bound`**

`spacetime/bound.lisp`:

```lisp
;;;; A BOUND is the range within which one timestamp lies.  Making the
;;;; endpoint a range rather than a value is what lets imprecision, open-
;;;; endedness and total ignorance share one mechanism (design §3.1).

(in-package #:graph-db.spacetime)

(defstruct (bound (:constructor %make-bound (earliest latest))
                  (:copier nil))
  "EARLIEST and LATEST are each a LOCAL-TIME:TIMESTAMP or :UNBOUNDED, which
denotes negative infinity in EARLIEST and positive infinity in LATEST."
  (earliest nil :read-only t)
  (latest nil :read-only t))

(defun %endpoint-ok-p (x)
  (or (eq x :unbounded) (typep x 'local-time:timestamp)))

(defun make-bound (earliest latest)
  "The range [EARLIEST, LATEST], each a TIMESTAMP or :UNBOUNDED.  Signals
INVALID-BOUND on a non-endpoint or a reversed range."
  (unless (and (%endpoint-ok-p earliest) (%endpoint-ok-p latest))
    (error 'invalid-bound :earliest earliest :latest latest
           :reason "endpoints must be a TIMESTAMP or :UNBOUNDED"))
  (when (and (not (eq earliest :unbounded))
             (not (eq latest :unbounded))
             (local-time:timestamp< latest earliest))
    (error 'invalid-bound :earliest earliest :latest latest
           :reason "EARLIEST is after LATEST"))
  (%make-bound earliest latest))

(defun exact-bound (timestamp)
  "A bound pinning exactly one timestamp."
  (make-bound timestamp timestamp))

(defun unknown-bound ()
  "A bound spanning all of time -- \"we have no idea when\"."
  (%make-bound :unbounded :unbounded))

(defun bound-exact-p (b)
  "True when B pins a single timestamp."
  (and (not (eq (bound-earliest b) :unbounded))
       (not (eq (bound-latest b) :unbounded))
       (local-time:timestamp= (bound-earliest b) (bound-latest b))))

(defun bound-unknown-p (b)
  "True when B constrains nothing."
  (and (eq (bound-earliest b) :unbounded)
       (eq (bound-latest b) :unbounded)))

(defun %strictly-before (latest earliest)
  "LATEST < EARLIEST.  :UNBOUNDED is +inf in a LATEST and -inf in an
EARLIEST, so either one makes this false -- it can never PRODUCE a verdict."
  (and (not (eq latest :unbounded))
       (not (eq earliest :unbounded))
       (local-time:timestamp< latest earliest)))

(defun bound-compare (a b)
  "Compare the timestamps A and B stand for: :< :> := or :AMBIGUOUS.
Definite only when no choice within either range could give another answer,
so two overlapping ranges are :AMBIGUOUS even if they coincide exactly."
  (cond ((%strictly-before (bound-latest a) (bound-earliest b)) :<)
        ((%strictly-before (bound-latest b) (bound-earliest a)) :>)
        ((and (bound-exact-p a) (bound-exact-p b)
              (local-time:timestamp= (bound-earliest a) (bound-earliest b)))
         :=)
        (t :ambiguous)))
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: 11 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add spacetime/bound.lisp tests/spacetime/bound-tests.lisp graph-db.asd
git commit -m "feat(spacetime): bounds and four-valued comparison (#130)"
```

---

### Task 3: `temporal-extent`, granules, and the sexp codec

**Files:**
- Create: `spacetime/extent.lisp`, `tests/spacetime/extent-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "extent")` and `(:file "extent-tests")`)

**Interfaces:**
- Consumes: `make-bound`, `exact-bound`, `bound-earliest`, `bound-latest` (Task 2); `check-standing` (Task 1).
- Produces: `make-interval (start end &key precision semantics standing)`, `make-instant (bound &key ...)`, `granule-bounds (timestamp precision)` → two values, `make-granule-interval (timestamp precision &key ...)`, `make-granule-instant (timestamp precision &key ...)`, accessors `extent-kind` / `-start` / `-end` / `-precision` / `-semantics` / `-standing`, `extent-instant-p`, `extent->sexp`, `sexp->extent`, `+precisions+`.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/extent-tests.lisp`:

```lisp
;;;; Temporal extents: construction, granules, and the sexp codec.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test granule-bounds-cover-the-whole-granule
  (multiple-value-bind (start end) (granule-bounds (ts 2026 1 15 14 30) :month)
    (is-true (timestamp= start (ts 2026 1 1)))
    (is-true (timestamp= end (timestamp- (ts 2026 2 1) 1 :nsec))))
  (multiple-value-bind (start end) (granule-bounds (ts 2026 2 9) :year)
    (is-true (timestamp= start (ts 2026 1 1)))
    (is-true (timestamp= end (timestamp- (ts 2027 1 1) 1 :nsec)))))

(test granule-bounds-handle-february-without-a-table
  "Leap-year correctness comes from LOCAL-TIME's month arithmetic, not from
a days-per-month table (design §3.5)."
  (multiple-value-bind (start end) (granule-bounds (ts 2024 2 10) :month)
    (declare (ignore start))
    (is-true (timestamp= end (timestamp- (ts 2024 3 1) 1 :nsec)))))

(test granules-land-on-absolute-utc-instants
  "Design §3.5.  Asserted against hard-coded Unix seconds, which no timezone
setting can move: 2026-01-01T00:00:00Z is 1767225600 and 2026-02-01T00:00:00Z
is 1769904000.  An unpinned constructor lands 7200 seconds early on a host in
EET, and this is what catches that.

LOCAL-TIME:FIND-TIMEZONE-BY-LOCATION-NAME is deliberately NOT used here: the
timezone repository is not loaded by default and it returns NIL, which would
have made this test vacuous."
  (multiple-value-bind (start end) (granule-bounds (ts 2026 1 15 14 30) :month)
    (is (= 1767225600 (local-time:timestamp-to-unix start)))
    (is (= 1769903999 (local-time:timestamp-to-unix end)))
    (is (= 999999999 (local-time:nsec-of end)))))

(test granule-construction-ignores-the-ambient-timezone
  "The other half: rebinding *DEFAULT-TIMEZONE* must not move a granule.
Strong on a non-UTC host, trivially true on a UTC one -- which is why the
absolute-instant test above carries the real weight."
  (let ((ambient (multiple-value-list (granule-bounds (ts 2026 1 15) :month))))
    (let ((*default-timezone* +utc-zone+))
      (let ((pinned (multiple-value-list
                     (granule-bounds (ts 2026 1 15) :month))))
        (is-true (timestamp= (first ambient) (first pinned)))
        (is-true (timestamp= (second ambient) (second pinned)))))))

(test a-granule-interval-is-not-a-granule-instant
  "The §3.3 distinction: \"January 2026\" has exact endpoints; \"sometime in
January 2026\" is one uncertain timestamp."
  (let ((month (make-granule-interval (ts 2026 1 15) :month))
        (point (make-granule-instant (ts 2026 1 15) :month)))
    (is (eq :interval (extent-kind month)))
    (is (eq :instant (extent-kind point)))
    (is-true (bound-exact-p (extent-start month)))
    (is-false (bound-exact-p (extent-start point)))
    (is-true (extent-instant-p point))
    (is-false (extent-instant-p month))))

(test an-instant-couples-its-two-endpoints
  "START and END must be the SAME bound, which is what makes the endpoints
move together (design §3.3)."
  (let ((point (make-granule-instant (ts 2026 1 15) :month)))
    (is (eq (extent-start point) (extent-end point)))))

(test extent-round-trips-through-the-sexp-codec
  (let ((e (make-granule-interval (ts 2026 1 15) :month
                                  :semantics :validity :standing :inferred)))
    (let ((back (sexp->extent (extent->sexp e))))
      (is (eq (extent-kind e) (extent-kind back)))
      (is (eq (extent-precision e) (extent-precision back)))
      (is (eq (extent-semantics e) (extent-semantics back)))
      (is (eq (extent-standing e) (extent-standing back)))
      (is-true (timestamp= (bound-earliest (extent-start e))
                           (bound-earliest (extent-start back))))
      (is-true (timestamp= (bound-latest (extent-end e))
                           (bound-latest (extent-end back)))))))

(test the-codec-preserves-instant-coupling
  "A round-tripped instant must come back coupled, or the algebra would
silently start over-reporting uncertainty for it."
  (let ((back (sexp->extent (extent->sexp
                             (make-granule-instant (ts 2026 1 15) :month)))))
    (is (eq :instant (extent-kind back)))
    (is (eq (extent-start back) (extent-end back)))))

(test the-codec-emits-only-values-core-can-serialize
  "Design §6: no serialize type byte is reserved, so every leaf must already
be a keyword, an integer, or a LOCAL-TIME:TIMESTAMP."
  (labels ((ok (x)
             (or (keywordp x) (integerp x) (null x)
                 (typep x 'local-time:timestamp)
                 (and (listp x) (every #'ok x)))))
    (is-true (ok (extent->sexp (make-granule-instant (ts 2026 1 15) :month))))
    (is-true (ok (extent->sexp
                  (make-interval (unknown-bound) (unknown-bound)
                                 :standing :indeterminate))))))

(test an-extent-rejects-a-bad-standing-or-precision
  (signals invalid-standing
    (make-granule-interval (ts 2026 1 15) :month :standing :probably))
  (signals invalid-extent (make-granule-interval (ts 2026 1 15) :fortnight)))
```

- [ ] **Step 2: Run the tests to verify they fail**

Same command as Task 2 Step 2. Expected: load failure — `extent.lisp` does not exist.

- [ ] **Step 3: Implement the extent**

`spacetime/extent.lisp`:

```lisp
;;;; TEMPORAL-EXTENT: an interval or an instant, each endpoint a range, with
;;;; the precision that produced those ranges and the standing that says how
;;;; we came to know them (GH #130, design §3).

(in-package #:graph-db.spacetime)

(defparameter +precisions+
  '(:year :month :day :hour :minute :second :nsec)
  "Granularities a record may be stated at.  PRECISION never enters
comparison -- the bound width already encodes it (design §3.2).")

(defparameter +precision-units+
  '((:year . :year) (:month . :month) (:day . :day) (:hour . :hour)
    (:minute . :minute) (:second . :sec) (:nsec . :nsec))
  "Our precision names mapped to LOCAL-TIME's arithmetic unit names.")

(defstruct (temporal-extent
            (:conc-name extent-)
            (:constructor %make-extent
                (kind start end precision semantics standing))
            (:copier nil))
  "KIND is :INSTANT or :INTERVAL.  For an :INSTANT, START and END are the
SAME bound object -- that identity is the endpoint coupling (design §3.3)."
  (kind nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t)
  (precision nil :read-only t)
  (semantics nil :read-only t)
  (standing nil :read-only t))

(defun extent-instant-p (e)
  "True when E is a degenerate extent -- one timestamp, not a span."
  (eq (extent-kind e) :instant))

(defun %check-precision (p)
  (unless (member p +precisions+)
    (error 'invalid-extent
           :reason (format nil "~S is not a precision" p)))
  p)

(defun make-interval (start end &key (precision :nsec) (semantics :event)
                                     (standing :observed))
  "An extent spanning [START, END], both BOUNDs, whose endpoints move
independently.  Intervals are closed (design §3.2)."
  (%make-extent :interval start end
                (%check-precision precision) semantics
                (check-standing standing)))

(defun make-instant (bound &key (precision :nsec) (semantics :event)
                                (standing :observed))
  "A degenerate extent: one timestamp, positioned somewhere in BOUND.  START
and END share the bound, so the two endpoints cannot move apart."
  (%make-extent :instant bound bound
                (%check-precision precision) semantics
                (check-standing standing)))

(defun granule-bounds (timestamp precision)
  "The first and last instants of the PRECISION granule containing
TIMESTAMP, as two values, computed in UTC (design §3.5)."
  (%check-precision precision)
  (let ((z local-time:+utc-zone+))
    (multiple-value-bind (nsec sec minute hour day month year)
        (local-time:decode-timestamp timestamp :timezone z)
      (let ((start (ecase precision
                     (:year (local-time:encode-timestamp
                             0 0 0 0 1 1 year :timezone z))
                     (:month (local-time:encode-timestamp
                              0 0 0 0 1 month year :timezone z))
                     (:day (local-time:encode-timestamp
                            0 0 0 0 day month year :timezone z))
                     (:hour (local-time:encode-timestamp
                             0 0 0 hour day month year :timezone z))
                     (:minute (local-time:encode-timestamp
                               0 0 minute hour day month year :timezone z))
                     (:second (local-time:encode-timestamp
                               0 sec minute hour day month year :timezone z))
                     (:nsec (local-time:encode-timestamp
                             nsec sec minute hour day month year
                             :timezone z)))))
        (values start
                (if (eq precision :nsec)
                    start
                    (local-time:timestamp-
                     (local-time:timestamp+
                      start 1 (cdr (assoc precision +precision-units+)))
                     1 :nsec)))))))

(defun make-granule-interval (timestamp precision &rest args)
  "The granule itself, as an interval with EXACT endpoints -- \"January
2026\".  Contrast MAKE-GRANULE-INSTANT (design §3.3)."
  (%check-precision precision)
  (multiple-value-bind (start end) (granule-bounds timestamp precision)
    (apply #'make-interval (exact-bound start) (exact-bound end)
           :precision precision args)))

(defun make-granule-instant (timestamp precision &rest args)
  "One timestamp known only to PRECISION -- \"sometime in January 2026\"."
  (%check-precision precision)
  (multiple-value-bind (start end) (granule-bounds timestamp precision)
    (apply #'make-instant (make-bound start end)
           :precision precision args)))

(defun %bound->sexp (b)
  (list (bound-earliest b) (bound-latest b)))

(defun %sexp->bound (s)
  (make-bound (first s) (second s)))

(defun extent->sexp (e)
  "A tree of values GRAPH-DB:SERIALIZE already handles -- keywords,
integers and LOCAL-TIME:TIMESTAMPs.  No core type byte is reserved
(design §6).  An :INSTANT writes ONE bound, so the codec cannot lose the
endpoint coupling."
  (list :temporal-extent 1
        (extent-kind e)
        (%bound->sexp (extent-start e))
        (if (extent-instant-p e) nil (%bound->sexp (extent-end e)))
        (extent-precision e)
        (extent-semantics e)
        (extent-standing e)))

(defun sexp->extent (s)
  "Inverse of EXTENT->SEXP.  Signals INVALID-EXTENT on an unknown tag or
version."
  (destructuring-bind (tag version kind start end precision semantics
                       standing)
      s
    (unless (and (eq tag :temporal-extent) (eql version 1))
      (error 'invalid-extent
             :reason (format nil "not a version-1 extent sexp: ~S ~S"
                             tag version)))
    (ecase kind
      (:instant (make-instant (%sexp->bound start) :precision precision
                              :semantics semantics :standing standing))
      (:interval (make-interval (%sexp->bound start) (%sexp->bound end)
                                :precision precision :semantics semantics
                                :standing standing)))))
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: 21 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add spacetime/extent.lisp tests/spacetime/extent-tests.lisp graph-db.asd
git commit -m "feat(spacetime): temporal extents, UTC granules, sexp codec (#130)"
```

---

### Task 4: The Allen core and surface, for intervals

**Files:**
- Create: `spacetime/allen.lisp`, `tests/spacetime/allen-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "allen")` and `(:file "allen-tests")`)

**Scope:** interval-vs-interval only. Task 5 adds instants. Do **not** write instant tests here.

**Interfaces:**
- Consumes: `bound-compare` (Task 2); `extent-start`, `extent-end`, `extent-standing`, `extent-semantics`, `extent-instant-p` (Task 3).
- Produces: `+allen-relations+`, `+allen-inverses+`, `allen-inverse (r)`, struct `temporal-relation` with readers `temporal-relation-relations` / `-standings` / `-semantics`, `allen-relations (a b)`, `allen-relation (a b)`, `allen-definite-p (a b)`, and the thirteen `extent-<rel>-p` predicates.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/allen-tests.lisp`:

```lisp
;;;; The Allen algebra over interval extents (design §4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun exact-interval (s e)
  "An interval extent with exact endpoints, for the exactness tests."
  (make-interval (exact-bound s) (exact-bound e)))

(test the-vocabulary-is-thirteen-and-inversion-is-an-involution
  (is (= 13 (length +allen-relations+)))
  (is (= 13 (length (remove-duplicates +allen-relations+))))
  (is (eq :equals (allen-inverse :equals)))
  (dolist (r +allen-relations+)
    (is (eq r (allen-inverse (allen-inverse r)))
        "~S must invert back to itself" r)))

(test exact-intervals-give-a-singleton-matching-classical-allen
  (let ((cases
          ;; a-start a-end b-start b-end  expected
          '((1 2 3 4 :before)   (1 2 2 3 :meets)
            (1 3 2 4 :overlaps) (1 4 2 4 :finished-by)
            (1 5 2 4 :contains) (1 2 1 3 :starts)
            (1 2 1 2 :equals)   (1 3 1 2 :started-by)
            (2 3 1 4 :during)   (2 4 1 4 :finishes)
            (2 4 1 3 :overlapped-by)
            (2 3 1 2 :met-by)   (3 4 1 2 :after))))
    (dolist (c cases)
      (destructuring-bind (as ae bs be expected) c
        (let ((a (exact-interval (ts 2026 1 as) (ts 2026 1 ae)))
              (b (exact-interval (ts 2026 1 bs) (ts 2026 1 be))))
          (is (eq expected (allen-relation a b))
              "[~D,~D] vs [~D,~D] should be ~S, got ~S"
              as ae bs be expected (allen-relation a b))
          (is-true (allen-definite-p a b)))))))

(test every-relation-is-reachable-and-they-are-disjoint
  "Jointly exhaustive and pairwise disjoint for exact intervals (§7.3)."
  (let ((seen '()))
    (loop for as from 1 to 4 do
      (loop for ae from (1+ as) to 5 do
        (loop for bs from 1 to 4 do
          (loop for be from (1+ bs) to 5 do
            (let ((r (allen-relation (exact-interval (ts 2026 1 as)
                                                     (ts 2026 1 ae))
                                     (exact-interval (ts 2026 1 bs)
                                                     (ts 2026 1 be)))))
              (is-true r "exact intervals must give a singleton")
              (pushnew r seen))))))
    (is (null (set-difference +allen-relations+ seen))
        "unreached relations: ~S" (set-difference +allen-relations+ seen))))

(test inversion-holds-for-exact-intervals
  (loop for as from 1 to 3 do
    (loop for ae from (1+ as) to 4 do
      (loop for bs from 1 to 3 do
        (loop for be from (1+ bs) to 4 do
          (let ((a (exact-interval (ts 2026 1 as) (ts 2026 1 ae)))
                (b (exact-interval (ts 2026 1 bs) (ts 2026 1 be))))
            (is (eq (allen-relation a b)
                    (allen-inverse (allen-relation b a))))))))))

(test an-imprecise-interval-yields-a-set-not-a-wrong-answer
  "Two extents recorded as \"January 2026\" as INTERVALS are genuinely
EQUALS -- their endpoints are exact.  The uncertainty case is the instant,
which Task 5 covers."
  (let ((jan (make-granule-interval (ts 2026 1 15) :month)))
    (is (eq :equals (allen-relation jan jan)))))

(test a-wholly-unknown-interval-relates-to-everything
  "Design §3.1: total ignorance comes back as all thirteen, in the
algebra's own terms, not as NIL."
  (let ((unknown (make-interval (unknown-bound) (unknown-bound)
                                :standing :indeterminate))
        (known (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (is (= 13 (length (temporal-relation-relations
                       (allen-relations unknown known)))))
    (is-false (allen-relation unknown known))
    (is-false (allen-definite-p unknown known))))

(test the-relation-set-is-never-empty
  (let ((a (exact-interval (ts 2026 1 1) (ts 2026 1 2)))
        (b (exact-interval (ts 2026 6 1) (ts 2026 6 2))))
    (is-true (temporal-relation-relations (allen-relations a b)))))

(test a-relation-carries-both-standings-and-both-semantics
  "Design §4.4: the set, not a collapsed weakest value."
  (let* ((a (exact-interval (ts 2026 1 1) (ts 2026 1 2)))
         (b (make-interval (exact-bound (ts 2026 6 1))
                           (exact-bound (ts 2026 6 2))
                           :standing :inferred :semantics :validity))
         (r (allen-relations a b)))
    (is (null (set-difference '(:observed :inferred)
                              (temporal-relation-standings r))))
    (is (null (set-difference '(:event :validity)
                              (temporal-relation-semantics r))))))

(test predicates-are-set-membership
  (let ((a (exact-interval (ts 2026 1 1) (ts 2026 1 2)))
        (b (exact-interval (ts 2026 1 3) (ts 2026 1 4))))
    (is-true (extent-before-p a b))
    (is-false (extent-after-p a b))
    (is-true (extent-after-p b a))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Same command. Expected: load failure — `allen.lisp` does not exist.

- [ ] **Step 3: Implement the Allen core**

`spacetime/allen.lisp`:

```lisp
;;;; The Allen interval algebra over extents whose endpoints are ranges.
;;;;
;;;; The thirteen relations are determined by the signs of four endpoint
;;;; comparisons; an :AMBIGUOUS comparison is a wildcard, so an imprecise
;;;; extent yields a SET (GH #130, design §4.1).

(in-package #:graph-db.spacetime)

(defparameter +allen-relations+
  '(:before :meets :overlaps :finished-by :contains :starts :equals
    :started-by :during :finishes :overlapped-by :met-by :after)
  "The closed relation vocabulary.  Thirteen, not fourteen: :EQUALS is its
own inverse.")

(defparameter +allen-inverses+
  '((:before . :after) (:meets . :met-by) (:overlaps . :overlapped-by)
    (:finished-by . :finishes) (:contains . :during) (:starts . :started-by)
    (:equals . :equals) (:started-by . :starts) (:during . :contains)
    (:finishes . :finished-by) (:overlapped-by . :overlaps)
    (:met-by . :meets) (:after . :before)))

(defun allen-inverse (relation)
  "The relation R such that (R b a) holds exactly when (RELATION a b) does."
  (or (cdr (assoc relation +allen-inverses+))
      (error 'spacetime-error)))

(defparameter +allen-signatures+
  ;; (relation s1?s2 s1?e2 e1?s2 e1?e2), read off canonical NON-degenerate
  ;; examples.  Degenerate extents do not obey this table -- see the instant
  ;; path (design §3.3.1).
  '((:before        :< :< :< :<)
    (:meets         :< :< := :<)
    (:overlaps      :< :< :> :<)
    (:finished-by   :< :< :> :=)
    (:contains      :< :< :> :>)
    (:starts        := :< :> :<)
    (:equals        := :< :> :=)
    (:started-by    := :< :> :>)
    (:during        :> :< :> :<)
    (:finishes      :> :< :> :=)
    (:overlapped-by :> :< :> :>)
    (:met-by        :> := :> :>)
    (:after         :> :> :> :>)))

(defstruct (temporal-relation (:copier nil))
  "RELATIONS is never empty: two extents always stand in at least one Allen
relation, and total ignorance is all thirteen rather than none.  STANDINGS
and SEMANTICS carry both endpoints' values -- not a collapse (design §4.4)."
  (relations nil :read-only t)
  (standings nil :read-only t)
  (semantics nil :read-only t))

(defun %compatible-p (computed expected)
  "An :AMBIGUOUS comparison constrains nothing, so it matches any sign."
  (or (eq computed :ambiguous) (eq computed expected)))

(defun %interval-relations (a b)
  "The relations consistent with A and B's four endpoint comparisons.
Correct only when NEITHER extent is an instant."
  (let ((c1 (bound-compare (extent-start a) (extent-start b)))
        (c2 (bound-compare (extent-start a) (extent-end b)))
        (c3 (bound-compare (extent-end a) (extent-start b)))
        (c4 (bound-compare (extent-end a) (extent-end b))))
    (loop for (rel s1 s2 s3 s4) in +allen-signatures+
          when (and (%compatible-p c1 s1) (%compatible-p c2 s2)
                    (%compatible-p c3 s3) (%compatible-p c4 s4))
            collect rel)))

(defun %relations-between (a b)
  "Dispatch on degeneracy.  Task 5 replaces the instant arms."
  (%interval-relations a b))

(defun allen-relations (a b)
  "The TEMPORAL-RELATION between extents A and B: every Allen relation
consistent with their endpoint ranges, plus both standings and semantics."
  (let ((rels (%relations-between a b)))
    (assert rels ()
            "empty relation set for ~S vs ~S -- a signature table bug" a b)
    (make-temporal-relation
     :relations rels
     :standings (remove-duplicates
                 (list (extent-standing a) (extent-standing b)))
     :semantics (remove-duplicates
                 (list (extent-semantics a) (extent-semantics b))))))

(defun allen-relation (a b)
  "The single relation between A and B when the answer is definite, else
NIL.  NIL means \"more than one relation is possible\", never \"unrelated\"."
  (let ((rels (temporal-relation-relations (allen-relations a b))))
    (when (null (cdr rels))
      (car rels))))

(defun allen-definite-p (a b)
  "True when exactly one relation is possible between A and B."
  (null (cdr (temporal-relation-relations (allen-relations a b)))))

(defmacro %define-relation-predicate (name relation)
  `(defun ,name (a b)
     ,(format nil "True when ~S is possible between extents A and B."
              relation)
     (and (member ,relation (temporal-relation-relations
                             (allen-relations a b)))
          t)))

(%define-relation-predicate extent-before-p :before)
(%define-relation-predicate extent-meets-p :meets)
(%define-relation-predicate extent-overlaps-p :overlaps)
(%define-relation-predicate extent-finished-by-p :finished-by)
(%define-relation-predicate extent-contains-p :contains)
(%define-relation-predicate extent-starts-p :starts)
(%define-relation-predicate extent-equals-p :equals)
(%define-relation-predicate extent-started-by-p :started-by)
(%define-relation-predicate extent-during-p :during)
(%define-relation-predicate extent-finishes-p :finishes)
(%define-relation-predicate extent-overlapped-by-p :overlapped-by)
(%define-relation-predicate extent-met-by-p :met-by)
(%define-relation-predicate extent-after-p :after)
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: 30 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add spacetime/allen.lisp tests/spacetime/allen-tests.lisp graph-db.asd
git commit -m "feat(spacetime): the Allen algebra over interval extents (#130)"
```

---

### Task 5: Instants — the degenerate path

**Files:**
- Modify: `spacetime/allen.lisp` (replace `%relations-between`, add two functions)
- Create: `tests/spacetime/instant-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "instant-tests")`)

**Why this is its own task.** The signature table of Task 4 is read off non-degenerate examples and **does not describe instants**. An instant at an interval's start has `e1?s2` of `:=`, which no table row admits, so the naive path returns the empty set and trips the assertion. Pruning the table's output is *not* sufficient — the instant needs its own computation.

**Interfaces:**
- Consumes: `bound-compare`, `extent-instant-p`, `extent-start`, `extent-end`, `allen-inverse`.
- Produces: replaces `%relations-between`; adds `%instant-vs-instant`, `%instant-vs-interval`.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/instant-tests.lisp`:

```lisp
;;;; Degenerate extents against the thirteen (design §3.3.1).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun exact-instant (timestamp)
  (make-instant (exact-bound timestamp)))

(test an-instant-at-an-interval-start-is-starts-not-meets
  "The collision §3.3.1 resolves: E1 = S2 satisfies :MEETS and S1 = S2 with
E1 < E2 satisfies :STARTS.  Under closed intervals the point is INSIDE, so
:STARTS states strictly more and wins."
  (let ((p (exact-instant (ts 2026 1 2)))
        (i (exact-interval (ts 2026 1 2) (ts 2026 1 3))))
    (is (eq :starts (allen-relation p i)))
    (is-false (extent-meets-p p i))))

(test an-instant-relates-to-an-interval-by-the-five-reachable-relations
  (let ((i (exact-interval (ts 2026 1 10) (ts 2026 1 20))))
    (is (eq :before (allen-relation (exact-instant (ts 2026 1 5)) i)))
    (is (eq :starts (allen-relation (exact-instant (ts 2026 1 10)) i)))
    (is (eq :during (allen-relation (exact-instant (ts 2026 1 15)) i)))
    (is (eq :finishes (allen-relation (exact-instant (ts 2026 1 20)) i)))
    (is (eq :after (allen-relation (exact-instant (ts 2026 1 25)) i)))))

(test the-unreachable-relations-never-appear-for-an-instant
  "Design §7.3: a reintroduced collision shows up here first."
  (let ((i (exact-interval (ts 2026 1 10) (ts 2026 1 20)))
        (forbidden '(:meets :overlaps :contains :finished-by :started-by
                     :equals :met-by :overlapped-by)))
    (loop for day from 5 to 25 do
      (let ((rels (temporal-relation-relations
                   (allen-relations (exact-instant (ts 2026 1 day)) i))))
        (is (null (intersection forbidden rels))
            "day ~D produced forbidden ~S" day
            (intersection forbidden rels))))))

(test an-interval-versus-an-instant-is-the-inverse
  (let ((i (exact-interval (ts 2026 1 10) (ts 2026 1 20))))
    (dolist (day '(5 10 15 20 25))
      (let ((p (exact-instant (ts 2026 1 day))))
        (is (eq (allen-relation i p)
                (allen-inverse (allen-relation p i)))
            "day ~D" day)))))

(test two-instants-relate-only-three-ways
  (let ((a (exact-instant (ts 2026 1 10))))
    (is (eq :before (allen-relation a (exact-instant (ts 2026 1 11)))))
    (is (eq :equals (allen-relation a (exact-instant (ts 2026 1 10)))))
    (is (eq :after (allen-relation a (exact-instant (ts 2026 1 9)))))
    (let ((rels (temporal-relation-relations
                 (allen-relations (make-instant (unknown-bound))
                                  (make-instant (unknown-bound))))))
      (is (null (set-difference rels '(:before :equals :after)))))))

(test an-uncertain-instant-is-constrained-by-its-coupling
  "Plan correction 2: a wholly unknown INSTANT against an interval yields the
five reachable relations, not thirteen.  The coupling constrains the answer
even when the position does not."
  (let ((rels (temporal-relation-relations
               (allen-relations (make-instant (unknown-bound))
                                (exact-interval (ts 2026 1 10)
                                                (ts 2026 1 20))))))
    (is (= 5 (length rels)))
    (is (null (set-difference
               rels '(:before :starts :during :finishes :after))))))

(test two-granule-instants-in-one-month-are-not-equal
  "The §3.3 payoff: \"sometime in January\" twice is genuinely uncertain,
where two January INTERVALS are exactly EQUALS."
  (let ((a (make-granule-instant (ts 2026 1 4) :month))
        (b (make-granule-instant (ts 2026 1 27) :month)))
    (is-false (allen-definite-p a b))
    (is (null (set-difference (temporal-relation-relations
                               (allen-relations a b))
                              '(:before :equals :after))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Same command. Expected: failures in `an-instant-at-an-interval-start-is-starts-not-meets` and others, several as the `empty relation set` assertion from Task 4 firing — which is the diagnosis, not a surprise.

- [ ] **Step 3: Implement the instant path**

In `spacetime/allen.lisp`, add before `%relations-between` and replace that function:

```lisp
(defun %instant-vs-instant (a b)
  "Two points relate only three ways.  :AMBIGUOUS admits all three."
  (let ((c (bound-compare (extent-start a) (extent-start b))))
    (ecase c
      (:< '(:before))
      (:= '(:equals))
      (:> '(:after))
      (:ambiguous '(:before :equals :after)))))

(defun %instant-vs-interval (p i)
  "Point P against interval I, per the design §3.3.1 table.  :MEETS and the
other eight are unreachable: under closed intervals a point at I's start is
INSIDE I, so :STARTS states strictly more than :MEETS."
  (let ((cs (bound-compare (extent-start p) (extent-start i)))
        (ce (bound-compare (extent-start p) (extent-end i)))
        (rels '()))
    (flet ((maybe (comparison &rest admissible)
             (member comparison admissible)))
      (when (maybe cs :< :ambiguous) (push :before rels))
      (when (maybe cs := :ambiguous) (push :starts rels))
      (when (and (maybe cs :> :ambiguous) (maybe ce :< :ambiguous))
        (push :during rels))
      (when (maybe ce := :ambiguous) (push :finishes rels))
      (when (maybe ce :> :ambiguous) (push :after rels)))
    (nreverse rels)))

(defun %relations-between (a b)
  "Dispatch on degeneracy: the signature table is read off non-degenerate
examples and does not describe instants (design §3.3.1)."
  (let ((ai (extent-instant-p a))
        (bi (extent-instant-p b)))
    (cond ((and ai bi) (%instant-vs-instant a b))
          (ai (%instant-vs-interval a b))
          (bi (mapcar #'allen-inverse (%instant-vs-interval b a)))
          (t (%interval-relations a b)))))
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: 37 tests, 0 failures. Confirm no Task 4 test regressed.

- [ ] **Step 5: Commit**

```bash
git add spacetime/allen.lisp tests/spacetime/instant-tests.lisp graph-db.asd
git commit -m "feat(spacetime): the degenerate path -- instants vs the thirteen (#130)"
```

---

### Task 6: The soundness property test

**Files:**
- Create: `tests/spacetime/property-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "property-tests")`)

**The oracle, and why it is sound.** Concretising an extent yields an extent with *exact* bounds, so `allen-relations` on the concretised pair must return a singleton — and that path is verified independently in Task 4 (against classical Allen, by table) and Task 5 (against §3.3.1's table). So the exact path is the oracle and the uncertain path is what is under test. Layered, not circular.

**Interfaces:**
- Consumes: the whole public surface.
- Produces: no production code — a test-only task.

- [ ] **Step 1: Write the property test**

`tests/spacetime/property-tests.lisp`:

```lisp
;;;; Soundness: the relation set may never omit the truth (design §7.1).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *property-trials* 400)

(defparameter *property-seed* 20260810
  "Fixed so a failure is reproducible.  A flaky property test nobody can
re-run is worse than no property test.")

(defun random-day (state) (1+ (random 28 state)))

(defun random-bound (state)
  "A bound over January 2026, sometimes exact, sometimes open-ended."
  (case (random 6 state)
    (0 (unknown-bound))
    (1 (make-bound (ts 2026 1 (random-day state)) :unbounded))
    (2 (make-bound :unbounded (ts 2026 1 (random-day state))))
    (t (let* ((d1 (random-day state))
              (d2 (max d1 (random-day state))))
         (if (= d1 d2)
             (exact-bound (ts 2026 1 d1))
             (make-bound (ts 2026 1 d1) (ts 2026 1 d2)))))))

(defun random-extent (state)
  (if (zerop (random 2 state))
      (make-instant (random-bound state))
      (let* ((s (random-day state))
             (e (min 28 (+ s 1 (random 10 state)))))
        (make-interval (exact-bound (ts 2026 1 s))
                       (exact-bound (ts 2026 1 e))))))

(defun concretise (e state)
  "Pick one admissible timestamp inside each of E's bounds and return an
extent with EXACT bounds.  :UNBOUNDED is drawn from a window well outside
the January range the generators use, so it stays outside every interval."
  (labels ((pick (b)
             (let ((lo (bound-earliest b))
                   (hi (bound-latest b)))
               (cond ((and (eq lo :unbounded) (eq hi :unbounded))
                      (ts 2026 1 (random-day state)))
                     ((eq lo :unbounded) hi)
                     ((eq hi :unbounded) lo)
                     ((timestamp= lo hi) lo)
                     (t (if (zerop (random 2 state)) lo hi))))))
    (if (extent-instant-p e)
        (make-instant (exact-bound (pick (extent-start e)))
                      :standing (extent-standing e))
        (make-interval (exact-bound (pick (extent-start e)))
                       (exact-bound (pick (extent-end e)))
                       :standing (extent-standing e)))))

(test the-relation-set-never-omits-the-truth
  "Design §7.1.  For any two extents, every concrete instantiation of their
endpoints must produce a relation the uncertain answer already contains.  If
this can fail, the algebra emits confidently-wrong answers."
  (let ((state (sb-ext:seed-random-state *property-seed*))
        (checked 0))
    (dotimes (i *property-trials*)
      (let* ((a (random-extent state))
             (b (random-extent state))
             (set (temporal-relation-relations (allen-relations a b)))
             (ca (concretise a state))
             (cb (concretise b state))
             (truth (allen-relation ca cb)))
        (when truth
          (incf checked)
          (is-true (member truth set)
                   "trial ~D: concrete truth ~S missing from ~S" i truth
                   set))))
    (is (> checked (floor *property-trials* 2))
        "only ~D of ~D trials concretised to a definite relation -- the ~
         generators are not exercising the exact path"
        checked *property-trials*)))

(test concretising-an-extent-always-gives-a-definite-answer
  "Guards the oracle itself: if an exact pair ever went indefinite, the
soundness test above would silently stop checking anything."
  (let ((state (sb-ext:seed-random-state *property-seed*)))
    (dotimes (i 200)
      (let ((ca (concretise (random-extent state) state))
            (cb (concretise (random-extent state) state)))
        (is-true (allen-definite-p ca cb)
                 "trial ~D: exact endpoints gave ~S" i
                 (temporal-relation-relations (allen-relations ca cb)))))))
```

- [ ] **Step 2: Run the tests to verify they pass**

Same command. Expected: 39 tests, 0 failures.

If `the-relation-set-never-omits-the-truth` fails, **do not weaken the test**. It has found a real soundness bug in `%interval-relations` or the instant path. Report it and stop.

- [ ] **Step 3: Prove the test is non-vacuous**

Temporarily break soundness — in `%interval-relations`, change the `%compatible-p` wildcard arm to `(eq computed expected)` so `:ambiguous` matches nothing:

```lisp
(defun %compatible-p (computed expected)
  (eq computed expected))
```

Re-run. Expected: `the-relation-set-never-omits-the-truth` FAILS, and the empty-set assertion in `allen-relations` fires. **Restore the wildcard arm** and re-run to confirm green.

- [ ] **Step 4: Commit**

```bash
git add tests/spacetime/property-tests.lisp graph-db.asd
git commit -m "test(spacetime): soundness property -- the set never omits the truth (#130)"
```

---

### Task 7: Absence-vs-value conformance and documentation

**Files:**
- Create: `tests/spacetime/conformance-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "conformance-tests")`)
- Modify: `docs/vivace-graph-v3-doc.org` (new top-level section)

**Interfaces:**
- Consumes: the whole public surface. Produces: no production code.

- [ ] **Step 1: Write the conformance tests**

`tests/spacetime/conformance-tests.lisp`:

```lisp
;;;; Absence-vs-value conformance (design §7.4).  A never-measured state must
;;;; never be confusable with a measured one -- the defect class with seven
;;;; confirmed instances that this subsystem exists to make unrepresentable.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test no-interval-is-not-a-zero-length-interval-at-the-epoch
  (let ((unknown (make-instant (unknown-bound) :standing :indeterminate))
        (epoch (make-instant (exact-bound (ts 1970 1 1))
                             :standing :observed)))
    (is-false (bound-exact-p (extent-start unknown)))
    (is-true (bound-exact-p (extent-start epoch)))
    (is-true (standing-absence-p (extent-standing unknown)))
    (is-false (standing-absence-p (extent-standing epoch)))
    (is-false (allen-definite-p unknown epoch))))

(test the-three-absence-reasons-survive-into-a-relation
  "A relation resting on SEARCHED-EMPTY and one resting on UNCOVERED must
not read the same downstream."
  (let ((known (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (dolist (s '(:searched-empty :uncovered :indeterminate))
      (let ((r (allen-relations
                (make-interval (unknown-bound) (unknown-bound) :standing s)
                known)))
        (is-true (member s (temporal-relation-standings r))
                 "~S was lost from the relation" s)))))

(test an-unknown-extent-yields-relations-not-an-error-and-not-nil
  (let ((unknown (make-interval (unknown-bound) (unknown-bound)
                                :standing :uncovered))
        (known (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (finishes (allen-relations unknown known))
    (is-true (temporal-relation-relations (allen-relations unknown known)))))

(test no-accessor-returns-a-value-mistakable-for-a-measurement
  "PRECISION and SEMANTICS are metadata; STANDING says whether there is a
measurement at all.  None of them may default to something that reads as a
real observation."
  (let ((e (make-interval (unknown-bound) (unknown-bound)
                          :standing :uncovered)))
    (is (eq :uncovered (extent-standing e)))
    (is-false (standing-present-p (extent-standing e)))
    (is (eq :unbounded (bound-earliest (extent-start e))))
    (is (eq :unbounded (bound-latest (extent-end e))))))

(test standing-is-required-and-validated-at-construction
  "There is no way to build an extent with a standing that is not one of the
six -- the collapse is unrepresentable, not merely discouraged."
  (signals invalid-standing
    (make-interval (unknown-bound) (unknown-bound) :standing nil))
  (signals invalid-standing
    (make-interval (unknown-bound) (unknown-bound) :standing 0))
  (signals invalid-standing
    (make-instant (unknown-bound) :standing "observed")))
```

- [ ] **Step 2: Run the tests to verify they pass**

Same command. Expected: 44 tests, 0 failures.

- [ ] **Step 3: Document the subsystem**

Add a top-level section to `docs/vivace-graph-v3-doc.org`, after the vector-index section. Cover, in org-mode prose matching the file's register:

- what `graph-db/spacetime` is, that it is opt-in, and that core does not depend on it;
- the endpoint-as-range idea and the three kinds of not-knowing it unifies;
- the `January 2026` versus `sometime in January 2026` distinction, with both constructors shown;
- that the algebra returns a *set*, with `allen-relations` / `allen-relation` / the predicates;
- that `standing` is unordered and a relation carries both endpoints' standings;
- that intervals are closed and granules are UTC;
- a worked example ending in a definite relation and one ending in a set.

- [ ] **Step 4: Run the full graph-db suite to confirm nothing regressed**

The spacetime system is not loaded by `graph-db/test`, so the core suite must be unchanged:

```bash
sbcl --dynamic-space-size 12288 --non-interactive --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp") (ql:quickload :graph-db/test) (fiveam:run! (quote graph-db/test::graph-db-suite)))' > /tmp/full-130.log 2>&1
```

Expected: **3526 checks, 3516 pass, 10 skip, 0 fail** — identical to the pre-branch baseline. Any change means this plan touched core, which it must not.

- [ ] **Step 5: Commit**

```bash
git add tests/spacetime/conformance-tests.lisp graph-db.asd docs/vivace-graph-v3-doc.org
git commit -m "test(spacetime): absence-vs-value conformance, and document the subsystem (#130)"
```

---

## Self-Review

**Spec coverage.** §2 API shape → Task 4 surface. §3.1 three kinds of not-knowing → Tasks 2, 3, and the unknown-extent tests. §3.2 records and `:unbounded` polarity → Task 2. §3.3 `kind` → Task 3 coupling tests. §3.3.1 degenerate table → Task 5. §3.4 standing orthogonal → Task 1. §3.5 UTC granules → Task 3. §4.1 signature table → Task 4. §4.2 the thirteen → Task 4. §4.3 surface → Task 4. §4.4 standings as a set → Task 4 and Task 7. §5 packaging → Task 1. §6 storage, no core change → Task 3 codec and Task 7 Step 4. §7.1 soundness → Task 6. §7.2 completeness → Task 4's reachability test and Task 5's forbidden-relation test. §7.3 exactness → Task 4. §7.4 conformance → Task 7. §10 version floor → no core change, verified in Task 7 Step 4.

**Type consistency.** `bound-compare` returns `:<` / `:>` / `:=` / `:ambiguous` in Tasks 2, 4 and 5. `extent-start` / `extent-end` are used consistently. `temporal-relation-relations` is the reader in every task that touches the result. `%relations-between` is defined in Task 4 and replaced in Task 5 with the same signature.

**Known gap, deliberately left.** §7.2's completeness property — *every* relation in a set is achieved by *some* instantiation — is tested by enumeration for the cases that matter (Task 4's reachability sweep, Task 5's forbidden-relation sweep) rather than as a general property. A general completeness property would need an admissible-instantiation search per pair; the enumerated cases catch the decoupling bug that motivated the requirement, which is what §7.2 was written for.
