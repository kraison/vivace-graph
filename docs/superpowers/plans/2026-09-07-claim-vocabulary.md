# Claim-Family Vocabulary (#350) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A caller asks a claim family which namespaces, relations and keys it holds, with optional counts, in distinct-count times log-n instead of a walk of every claim (GH #350).

**Architecture:** Two domain-neutral engine primitives in `index.lisp` — `map-index-prefixes`, a seek-and-skip walk of distinct leading prefixes over an ordered index, and `index-count`, a bounded range count — plus a fifth `(relation)` index declared by `def-claim-classes`. Three spacetime functions (`claim-namespaces`, `claim-relations`, `claim-keys`) walk the family's subject, object and relation indexes, confirm each name by resolving one live node, resolve the range for `:current`, and overlay an open transaction's writes through the commit view `claims-touching` already uses. No storage format change.

**Tech Stack:** SBCL, FiveAM, `graph-db` core (`index.lisp`), `graph-db/spacetime`.

**Spec:** `docs/superpowers/specs/2026-09-07-claim-vocabulary-design.md` (amended 630c399). **Facts:** `docs/superpowers/notes/2026-09-07-claim-vocabulary-engine-facts.md` — every `file:line` below was verified there at 5bcece8; match on the quoted forms, not the numbers. Read its §X and §E before any task.

## Global Constraints

- Lisp: spaces only, hard 80 columns on every line, terse comments naming GH #350 or a spec section; docstrings state what/returns/trap.
- Branch `feat/claim-vocabulary` from `experiment` (8e65e0f), worktree `<worktree>` = `/home/raison/work/vg-c3/.worktrees/claim-vocab` (clone `/home/raison/work/vg-c3`). Never build in a shared checkout.
- Never run `pkill`, `pgrep -f`, or `kill`. One SBCL build at a time in this worktree. Never the full 15-minute suite by hand; the two suites below only. CI runs the full suite on push.
- No storage format change (spec R1, R3). The walk takes no lock of its own (spec §2.1, facts X2/E3). Never `ix-map`'s open-ended path (facts E2).
- The object index is declared on the family's BINARY class, the other four on the PARENT (facts X3/E6).
- Every `%index-bounds` call on a short tuple passes `prefix` true (facts E1). Arity comes from `(length (slot-index-slot-names six))`, never from a cursor key (facts E9).
- Every new `graph-db` symbol a `graph-db/test` test uses unqualified is added to the `:import-from #:graph-db` list in `tests/package.lisp` (facts E12); spacetime tests `:use` the spacetime package and write `graph-db:` for engine symbols.
- Existing tests keep passing; the baseline check counts recorded in Task 1 never drop.
- Docs travel with the code (Task 7); every commit message names GH #350.
- Commit trailers on every commit:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_013BfdWuKGebCvLBKsbU753o
  ```

## Running the suites

Write once to `/home/raison/work/vg-c3-notes/vg-350-suites.lisp` (outside every checkout):

```lisp
;; The two suites this unit touches, CI-style, in a fresh image.
;; Mirrors RUN-TESTS (tests/suite.lisp): FiveAM's RUN bypasses the
;; test system's :perform, so the system directory is bound here.
(ql:quickload '(:graph-db/test :graph-db/spacetime-test) :silent t)
(format t "~&== graph-db loaded from ~a~%"
        (asdf:system-source-directory :graph-db))
(in-package :graph-db/test)
(log:config :error)
(let* ((system-dir (make-temp-directory))
       (graph-db::*system-directory* (namestring system-dir))
       (graph-db::*type-registry* nil)
       (ok t))
  (unwind-protect
       (progn
         (let ((r (fiveam:run 'index-suite)))
           (fiveam:explain! r)
           (format t "~&== index-suite ~a~%"
                   (if (fiveam:results-status r) "PASS" "FAIL"))
           (unless (fiveam:results-status r) (setf ok nil)))
         (unless (graph-db/spacetime-test::run-spacetime-tests)
           (setf ok nil)))
    (graph-db-test-scratch:cleanup-scratch-run))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root (foreground, timeout 600000 ms — a background run can be killed by the harness's memory heuristic):

```bash
cd /home/raison/work/vg-c3/.worktrees/claim-vocab
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3-notes/vg-350-suites.lisp \
  > /home/raison/work/vg-c3-notes/vg-350-suites.log 2>&1; echo "exit=$?"
grep -E "loaded from|Did [0-9]+ checks|Fail:|^== " /home/raison/work/vg-c3-notes/vg-350-suites.log
```

A single test while iterating, same fresh-image style (for a spacetime test, quickload `:graph-db/spacetime-test` and `in-package :graph-db/spacetime-test`; the `let*` binding stays because `run-spacetime-tests` is not used here):

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(in-package :graph-db/test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

Always read back `Did N checks`: a suite whose tests never registered reports green with zero checks. Run suites in a fresh image after any export change (facts E15).

## Code facts every task relies on

- `%require-index graph class-name slot-name` returns the `slot-index`, NIL for a declared-but-empty index, or signals `query-precondition-error` (`index.lisp:973-988`). Arity is `(length (slot-index-slot-names six))`.
- `%index-key six value` canonicalises a query value (NIL → `+null-component+`; at arity 1 it wraps a scalar); returns NIL for a full-arity all-null tuple (`index.lisp:405-426`). `%index-bounds six value prefix` returns `(values lo hi)`: full arity `[vals+null-key, vals+max-key]`; a shorter tuple with `prefix` true `[vals, vals+max-sentinels+max-key]`; otherwise signals (`index.lisp:456-486`). `%index-head-key n` / `%index-tail-key n` are the sentinels (`index.lisp:256-262`).
- `make-range-cursor sl lo hi` seeks on all three backends; `(cursor-next cur :eoc)` yields a skip-node or `:eoc`; `(%sn-key node)` is `(v1 ... vn id)` (`index.lisp:488-498` is the template). No lock of the caller's own.
- Collation per component is the generic `less-than` (`utilities.lisp` ~300): NIL and `+null-component+` first, symbols by `symbol-name` `string<`, strings by `string<`, a string after every symbol. Verify the generic's name at that line before using it.
- `def-index` builds now on an open graph and at open otherwise (`index.lisp:882-904`, `graph.lisp:1050-1055`); every declaration `def-claim-classes` emits is `:name`d (`spacetime/claim.lisp:412-421`).
- `claim-family parent` → struct with `claim-family-parent/-unary/-binary`, signals `unknown-claim-family` (`spacetime/claim.lisp:14-32`). `claims-touching` reads the subject index on the parent and the object index on the binary (`spacetime/claim-query.lisp:349-362`); `%paginate list limit offset` returns the cut and a "more" flag (`:246-254`); `%overlay-transaction` (`:256-279`) is the commit-view model; `claim-current-p` (`:469-474`); `retract-claim claim &key at` joins an ambient transaction (`:476-502`).
- `graph-db:make-commit-view graph tx`, `view-node view id` (NIL for a write that deletes), `view-writes view` (TX-WRITE objects, `(graph-db:id w)` works), `view-old-node view node` (NIL only for a create) — `value-constraint.lisp:143-189`, exported at `package.lisp:450`.
- Test fixtures: `with-ix-graph (g)` and the three-slot `ix-claim (ns key rel)` index in `tests/index-tests.lisp:30-37,99-107`; `with-claim-graph (g)`, family `ct-claim`/`-unary`/`-binary`, `make-u`/`make-b` (namespace fixed to `:ns`) in `tests/spacetime/claim-tests.lisp:7-24` and `claim-identity-tests.lisp:7-18`; the close/reopen shape at `tests/spacetime/claim-query-tests.lisp:64-85`. New spacetime test files register in `graph-db.asd:598-614` after `(:file "epoch-tests")`.

---

### Task 1: Baseline

**Files:**
- Create: `/home/raison/work/vg-c3-notes/vg-350-suites.lisp` (the runner above)

- [ ] **Step 1: Write the runner and run it on the unchanged branch**

Run the "Running the suites" block. Expected: `exit=0`, `graph-db loaded from` the worktree, `== index-suite PASS`, and a passing spacetime run.

- [ ] **Step 2: Record the counts**

Copy every `Did N checks` line into the SDD ledger as the baseline. No later run may report fewer checks for a suite.

---

### Task 2: The distinct-prefix walk and `index-count`

**Files:**
- Modify: `index.lisp` — after `index-range` (~1039), before the Prolog generator section
- Modify: `package.lisp:466` (`#:index-lookup #:index-range #:map-index`)
- Modify: `tests/package.lisp` after `#:map-index` (~279)
- Test: `tests/index-tests.lisp` (append)

**Interfaces:**
- Produces: `(map-index-prefixes fn graph class-name slot-name &key (arity 1) start)` calling FN with a component list (NIL for a null component), in index order, once per distinct prefix; `(index-count graph class-name slot-name value &key prefix)` → integer. Both exported from `graph-db`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/index-tests.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; GH #350: distinct-prefix walk and counts (spec 2026-09-07 §2)
;;; ---------------------------------------------------------------------------

(defvar *ix-seeks* nil
  "Seek counter for %COUNT-SEEKS; NIL when not counting.")

(defun %count-seeks (thunk)
  "Run THUNK counting MAKE-RANGE-CURSOR calls -- the one seek each hop
of the prefix walk makes -- through an :AROUND method removed afterwards.
Returns the count.  Callers prove the probe live with a control."
  (let* ((gf #'make-range-cursor)
         (method (eval '(defmethod make-range-cursor :around
                            ((index t) start end &key &allow-other-keys)
                          (declare (ignore start end))
                          (when *ix-seeks* (incf *ix-seeks*))
                          (call-next-method)))))
    (unwind-protect
         (let ((*ix-seeks* 0))
           (funcall thunk)
           *ix-seeks*)
      (remove-method gf method))))

(test map-index-prefixes-reports-each-distinct-prefix-once-in-order
  "Spec §2: each distinct leading prefix once, in index order, at arity
1 and 2; :START begins at the first prefix at or after it."
  (with-ix-graph (g)
    (with-transaction ()
      (dolist (row '(("ops" "e1" "at") ("ops" "e1" "by") ("ops" "e2" "at")
                     ("hr" "p1" "at") ("hr" "p1" "at") ("zz" "q" "at")))
        (make-ix-claim :ns (first row) :key (second row)
                       :rel (third row))))
    (flet ((walk (arity &optional start)
             (let ((out '()))
               (map-index-prefixes (lambda (p) (push p out)) g 'ix-claim
                                   '(ns key rel) :arity arity :start start)
               (nreverse out))))
      (is (equal '(("hr") ("ops") ("zz")) (walk 1)))
      (is (equal '(("hr" "p1") ("ops" "e1") ("ops" "e2") ("zz" "q"))
                 (walk 2))
          "two claims share (hr p1): one prefix")
      (is (equal '(("ops") ("zz")) (walk 1 '("ops"))) ":start inclusive")
      (is (equal '(("ops" "e1") ("ops" "e2") ("zz" "q"))
                 (walk 2 '("ops")))
          "a :start shorter than the arity starts at its first prefix")
      (is (null (walk 1 '("zzz"))) "past the last prefix: nothing")
      (signals query-precondition-error (walk 4))
      (signals query-precondition-error (walk 1 '("a" "b"))))))

(test map-index-prefixes-seeks-once-per-prefix
  "Spec §2.1: seek-and-skip -- K distinct prefixes cost K+1 seeks
whatever N is.  Control: one prefix INDEX-LOOKUP is one seek, so the
probe fires.  Ablation, recorded in the task report: a linear hop makes
the walk cost N+1 seeks and turns the second check red."
  (with-ix-graph (g)
    (with-transaction ()
      (dotimes (i 30)
        (make-ix-claim :ns (nth (mod i 3) '("a" "b" "c"))
                       :key (format nil "k~D" i) :rel "r")))
    (is (= 1 (%count-seeks
              (lambda ()
                (index-lookup g 'ix-claim '(ns key rel) '("a")
                              :prefix t))))
        "control: one range lookup is one seek")
    (is (= 4 (%count-seeks
              (lambda ()
                (map-index-prefixes #'identity g 'ix-claim
                                    '(ns key rel)))))
        "3 prefixes over 30 entries: 3 hops plus the terminating seek")))

(test map-index-prefixes-reports-a-null-component-as-nil
  "Spec §2.2: a stored null (+NULL-COMPONENT+) reads back as NIL and
sorts first; a NIL in :START maps the other way."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-claim :ns "ops" :key nil :rel "at")
      (make-ix-claim :ns "ops" :key "e1" :rel "at"))
    (flet ((walk (&optional start)
             (let ((out '()))
               (map-index-prefixes (lambda (p) (push p out)) g 'ix-claim
                                   '(ns key rel) :arity 2 :start start)
               (nreverse out))))
      (is (equal '(("ops" nil) ("ops" "e1")) (walk)))
      (is (equal '(("ops" nil) ("ops" "e1")) (walk '("ops" nil)))))))

(test index-count-is-the-range-size
  "Spec §2: INDEX-COUNT counts entries under a full tuple or, with
:PREFIX T, under a prefix; 0 for an absent prefix and for a declared
index with no entries; a short tuple without :PREFIX signals as
INDEX-LOOKUP does."
  (with-ix-graph (g)
    (is (= 0 (index-count g 'ix-claim '(ns key rel) '("ops") :prefix t))
        "declared, empty")
    (with-transaction ()
      (make-ix-claim :ns "ops" :key "e1" :rel "at")
      (make-ix-claim :ns "ops" :key "e1" :rel "by")
      (make-ix-claim :ns "ops" :key "e2" :rel "at")
      (make-ix-claim :ns "hr" :key "p1" :rel "at"))
    (is (= 3 (index-count g 'ix-claim '(ns key rel) '("ops") :prefix t)))
    (is (= 2 (index-count g 'ix-claim '(ns key rel) '("ops" "e1")
                          :prefix t)))
    (is (= 1 (index-count g 'ix-claim '(ns key rel) '("ops" "e1" "at"))))
    (is (= 0 (index-count g 'ix-claim '(ns key rel) '("none")
                          :prefix t)))
    (signals query-precondition-error
      (index-count g 'ix-claim '(ns key rel) '("ops")))))
```

Add to `tests/package.lisp` after `#:map-index`:

```lisp
                #:map-index-prefixes         ; GH #350
                #:index-count
```

`make-range-cursor` and `query-precondition-error` are already imported there (facts C17; verify with grep, add if not).

- [ ] **Step 2: Run to verify they fail**

Run the single-test command with `map-index-prefixes-reports-each-distinct-prefix-once-in-order`. Expected: a load failure on the unexported symbol (the test package cannot import what `graph-db` does not export). That failure is the RED this step wants.

- [ ] **Step 3: Implement**

In `index.lisp`, after `index-range`:

```lisp
;;; ---------------------------------------------------------------------------
;;; Distinct-prefix walk and counts (GH #350).
;;;
;;; MAP-INDEX-PREFIXES reports each distinct leading prefix once by
;;; seek-and-skip: one range cursor per prefix, opened at the previous
;;; prefix's high bound (%INDEX-BOUNDS with PREFIX T), which sorts past
;;; every tuple sharing it.  Never IX-MAP's open-ended path (a full
;;; scan).  No lock of its own -- MAKE-RANGE-CURSOR / CURSOR-NEXT own
;;; locking per backend and nesting deadlocks on ECL (skip-list.lisp) --
;;; so a walk is not an atomic snapshot.  Spec: docs/superpowers/specs/
;;; 2026-09-07-claim-vocabulary-design.md §2.

(defun %ix-first (six lo hi)
  "The first entry of SIX with LO <= key <= HI as (COMPONENTS . ID), or
NIL.  One seek and one step."
  (let* ((cur (make-range-cursor (slot-index-skip-list six) lo hi))
         (node (and cur (cursor-next cur :eoc))))
    (unless (or (null node) (eql node :eoc))
      (let ((key (%sn-key node)))
        (cons (butlast key) (car (last key)))))))

(defun %ix-prefix-out (components arity)
  "The first ARITY of COMPONENTS, +NULL-COMPONENT+ read back as NIL."
  (loop for v in components
        repeat arity
        collect (if (eq v +null-component+) nil v)))

(defun %ix-start-key (six start arity)
  "START as the canonical key a walk at ARITY begins from: NIL maps to
+NULL-COMPONENT+ and canonicalizers apply (%INDEX-KEY); the head key
when START is NIL.  Signals on more than ARITY components."
  (let ((n (length (slot-index-slot-names six))))
    (if (null start)
        (%index-head-key n)
        (let ((vals (if (listp start) start (list start))))
          (when (> (length vals) arity)
            (error 'query-precondition-error
                   :reason (format nil "A :START of ~D component(s) for ~
a prefix walk at arity ~D" (length vals) arity)))
          (or (%index-key six (if (= n 1) (first vals) vals))
              ;; Full arity, every component null: still a real prefix
              ;; here, unlike an equality lookup (GH #107).
              (make-list (length vals)
                         :initial-element +null-component+))))))

(defun map-index-prefixes (fn graph class-name slot-name
                           &key (arity 1) start)
  "Call FN with each distinct leading prefix of ARITY components held by
the index on CLASS-NAME.SLOT-NAME -- a list, NIL for a null component --
in index order, once each.  START (a value, or a tuple of at most ARITY
components) begins at the first prefix at or after it.  One seek per
distinct prefix, never a scan (GH #350).  Trap: membership is live and
a walk is not an atomic snapshot; resolve a prefix's nodes when a
snapshot or a deletion matters."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                   ; NIL => declared but empty => no prefixes
      (let ((n (length (slot-index-slot-names six))))
        (unless (<= 1 arity n)
          (error 'query-precondition-error
                 :reason (format nil "Index on ~S has arity ~D; cannot ~
walk prefixes of ~D" (slot-index-slot-names six) n arity)))
        (let ((hi (%index-tail-key n))
              (lo (%ix-start-key six start arity)))
          (loop
            (let ((entry (%ix-first six lo hi)))
              (when (null entry) (return))
              (let ((prefix (subseq (car entry) 0 arity)))
                (funcall fn (%ix-prefix-out prefix arity))
                ;; Hop: PREFIX's high bound sorts past every tuple
                ;; sharing it, so the next seek lands on the next prefix.
                (setf lo (nth-value 1 (%index-bounds six prefix t)))))))))))

(defun index-count (graph class-name slot-name value &key prefix)
  "Number of entries in the index on CLASS-NAME.SLOT-NAME whose tuple
equals VALUE, or with PREFIX T starts with it; 0 when none.  Same VALUE
and PREFIX rules as INDEX-LOOKUP.  Entries, not live nodes: inside an
open transaction this is the committed membership (GH #350)."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (if (null six)
        0
        (let ((key (%index-key six value)))
          (if (null key)
              0                 ; an all-null full tuple matches nothing
              (multiple-value-bind (lo hi) (%index-bounds six key prefix)
                (let ((cur (make-range-cursor (slot-index-skip-list six)
                                              lo hi))
                      (n 0))
                  (loop for node = (cursor-next cur :eoc)
                        until (eql node :eoc)
                        do (incf n))
                  n)))))))
```

`package.lisp:466` becomes:

```lisp
           #:index-lookup #:index-range #:map-index
           #:map-index-prefixes #:index-count            ; GH #350
```

- [ ] **Step 4: Run the four tests, then the `index-suite`**

Expected: all PASS. If `map-index-prefixes-seeks-once-per-prefix`'s control is not 1, report the observed count and what else seeks inside `index-lookup`; do not loosen the assertion. Then the ablation: temporarily replace the hop line `(setf lo (nth-value 1 (%index-bounds six prefix t)))` with `(setf lo (nth-value 1 (%index-bounds six (car entry) nil)))` — the full-arity high bound of the entry just read, which advances one entry per seek — run the seek test and record the red count (31 seeks expected); restore the line, run it green, and record both runs in the report.

- [ ] **Step 5: Column check and commit**

```bash
awk 'length > 80 {print FILENAME":"FNR}' index.lisp package.lisp tests/index-tests.lisp tests/package.lisp
git add index.lisp package.lisp tests/index-tests.lisp tests/package.lisp
git commit -m "feat(index): distinct-prefix walk and range count (#350)

map-index-prefixes seeks once per distinct leading prefix; index-count
is a bounded range count (spec 2026-09-07 §2)."
```

(with the two trailers).

---

### Task 3: The relation index

**Files:**
- Modify: `spacetime/claim.lisp:435-450` (the `def-index` block inside `def-claim-classes`)
- Test: `tests/spacetime/claim-query-tests.lisp` (append); `tests/index-tests.lisp` (append, one test, only if none like it exists)

**Interfaces:**
- Produces: every claim family has an index named `claim-relation` on `(relation)` declared on the PARENT class; `(graph-db:index-lookup g parent '(relation) "r")` answers.

- [ ] **Step 1: Write the failing test**

Append to `tests/spacetime/claim-query-tests.lisp`:

```lisp
(test the-relation-index-answers-and-survives-reopen
  "GH #350 spec §3: DEF-CLAIM-CLASSES declares a (RELATION) index named
CLAIM-RELATION on the parent; it answers INDEX-LOOKUP on a fresh family
and again after close and reopen (the sidecar round trip)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (make-u :subject "a" :relation "likes")
                 (make-b :subject "a" :object "b" :relation "knows")
                 (make-u :subject "c" :relation "likes"))
               (is (= 2 (length (graph-db:index-lookup
                                 g 'ct-claim '(relation) "likes"))))
               (is (= 1 (length (graph-db:index-lookup
                                 g 'ct-claim '(relation) "knows")))))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (is (= 2 (length (graph-db:index-lookup
                                 g2 'ct-claim '(relation) "likes")))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
```

`with-temp-directory`, `collect-garbage`, `open-graph`, `close-graph`, `make-graph` are used by the existing reopen test in this file (`:64-85`); confirm they resolve unqualified.

Then grep `tests/index-tests.lisp` for a test where `def-index` is evaluated on an OPEN graph that already holds instances (search `def-index` inside a `test` body). If none exists, append:

```lisp
(test def-index-declared-on-an-open-graph-indexes-existing-instances
  "GH #350 spec §3: a DEF-INDEX evaluated while the graph is open builds
over the instances already stored -- the relation index's no-migration
contract."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "late"))
    (def-index ix-claim (rel) :graph-db-index-test :name ix-late-rel)
    (unwind-protect
         (is (= 1 (length (index-lookup g 'ix-claim '(rel) "late"))))
      (undef-index ix-claim (rel) :graph-db-index-test))))
```

(Check `undef-index`'s lambda list at its definition before using it; `ix-claim` is shared by the whole suite, so the index must be retracted afterwards.)

- [ ] **Step 2: Run to verify they fail**

Expected: the spacetime test signals `query-precondition-error` ("No secondary index on CT-CLAIM.(RELATION)"). The engine test, if added, is expected to pass already (it pins existing behaviour); record that.

- [ ] **Step 3: Implement**

In `spacetime/claim.lisp`, after the `claim-subject-relation` declaration (`:448-450`), inside the same `progn`:

```lisp
       ;; The vocabulary listing skips to distinct relations (GH #350).
       (graph-db:def-index ,parent (relation) ,graph-name
                           :name claim-relation)
```

- [ ] **Step 4: Run the new test(s), then both suites via the runner**

Expected: PASS; counts ≥ baseline.

- [ ] **Step 5: Commit** — `feat(spacetime): a (relation) index on every claim family (#350)` with trailers. Body: "Declared by def-claim-classes as claim-relation; existing families build it on next open (spec §3)."

---

### Task 4: `claim-namespaces` and `claim-relations`

**Files:**
- Modify: `spacetime/claim-query.lisp` (append after `claims-by-producer`, ~561)
- Modify: `spacetime/package.lisp:53` (after `#:claim-extent #:claims-touching`)
- Modify: `graph-db.asd:614` (add `(:file "vocabulary-tests")` after `(:file "epoch-tests")`)
- Create: `tests/spacetime/vocabulary-tests.lisp`

**Interfaces:**
- Consumes: `graph-db:map-index-prefixes`, `graph-db:index-count` (Task 2); the `claim-relation` index (Task 3).
- Produces: `(claim-namespaces graph claim-class &key (role :either) current counts as-of as-of-epoch)`; `(claim-relations graph claim-class &key current counts as-of as-of-epoch)`; internal `%refuse-vocabulary-axis`, `%vocabulary-sources`, `%vocabulary-key`, `%name-admitted-p`, `%name-count`, `%walk-names`, `%name-lessp`, `%merge-names` — Task 5 and Task 6 build on these exact names and lambda lists.

- [ ] **Step 1: Write the failing tests**

Create `tests/spacetime/vocabulary-tests.lisp`:

```lisp
;;;; tests/spacetime/vocabulary-tests.lisp -- what a family names
;;;; (GH #350, spec 2026-09-07 §4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun %ns-u (ns subject &key (relation "r") (producer "rule-a"))
  "A unary CT-CLAIM with an explicit namespace (MAKE-U fixes :NS)."
  (make-ct-claim-unary :subject-namespace ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred))

(defun %ns-b (ns subject ons object
              &key (relation "r") (producer "rule-a"))
  "A binary CT-CLAIM with explicit namespaces on both ends."
  (make-ct-claim-binary :subject-namespace ns :subject-key subject
                        :relation relation
                        :object-namespace ons :object-key object
                        :producer producer :standing :inferred))

(test claim-namespaces-lists-by-role-in-index-order
  "Spec §4.1-4.2: subject names from the parent's index, object names
from the binary's, :EITHER merged and de-duplicated, in index order
(keywords by SYMBOL-NAME); :COUNTS sums over roles."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-b :ns "b" :other "x")
      (%ns-b :region "c" :ns "d"))
    (is (equal '(:ns :region) (claim-namespaces g 'ct-claim :role :subject)))
    (is (equal '(:ns :other) (claim-namespaces g 'ct-claim :role :object)))
    (is (equal '(:ns :other :region) (claim-namespaces g 'ct-claim)))
    (is (equal '((:ns . 2) (:region . 1))
               (claim-namespaces g 'ct-claim :role :subject :counts t)))
    (is (equal '((:ns . 3) (:other . 1) (:region . 1))
               (claim-namespaces g 'ct-claim :counts t))
        ":either sums the two roles")))

(test claim-relations-lists-distinct-relations-with-counts
  "Spec §4.1: relations from the family's CLAIM-RELATION index, once
each, in index order, with counts."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a" :relation "r")
      (%ns-u :ns "b" :relation "r")
      (%ns-b :ns "c" :ns "d" :relation "knows"))
    (is (equal '("knows" "r") (claim-relations g 'ct-claim)))
    (is (equal '(("knows" . 1) ("r" . 2))
               (claim-relations g 'ct-claim :counts t)))))

(test vocabulary-current-drops-retracted-only-names
  "Spec §4.3, R4: the default lists a name whose claims are all
retracted (the record of what was believed); :CURRENT T drops it and
counts only current claims."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :other "b")
      (%ns-u :other "c"))
    (retract-claim (first (claims-touching g 'ct-claim :other "b")))
    (is (equal '(:ns :other) (claim-namespaces g 'ct-claim)))
    (is (equal '((:ns . 1) (:other . 2))
               (claim-namespaces g 'ct-claim :counts t)))
    (is (equal '((:ns . 1) (:other . 1))
               (claim-namespaces g 'ct-claim :counts t :current t))
        "the retracted claim is not counted")
    (retract-claim (first (claims-touching g 'ct-claim :other "c")))
    (is (equal '(:ns) (claim-namespaces g 'ct-claim :current t))
        "every claim under :other is retracted")
    (is (equal '(:ns :other) (claim-namespaces g 'ct-claim)))))

(test vocabulary-drops-a-name-whose-claims-were-deleted
  "Spec §2.3, §5: a deleted claim's index entries go at commit, so its
name disappears without :CURRENT."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :gone "z"))
    (is (equal '(:gone :ns) (claim-namespaces g 'ct-claim)))
    (with-transaction ()
      (graph-db:mark-deleted (first (claims-touching g 'ct-claim :gone "z"))))
    (is (equal '(:ns) (claim-namespaces g 'ct-claim)))
    (is (equal '((:ns . 1)) (claim-namespaces g 'ct-claim :counts t)))))

(test vocabulary-refuses-the-epoch-axes-and-an-unknown-family
  "Spec §4.5, R6: :AS-OF and :AS-OF-EPOCH signal a typed refusal;
an unknown family signals as CLAIMS-TOUCHING does."
  (with-claim-graph (g)
    (with-transaction () (%ns-u :ns "a"))
    (signals graph-db:query-precondition-error
      (claim-namespaces g 'ct-claim :as-of 1))
    (signals graph-db:query-precondition-error
      (claim-relations g 'ct-claim :as-of-epoch 1))
    (signals unknown-claim-family (claim-namespaces g 'no-such-family))
    (is (equal '(:ns) (claim-namespaces g 'ct-claim))
        "control: the same call without the axis answers")))
```

Register the file in `graph-db.asd` after `(:file "epoch-tests")` in the `graph-db/spacetime-test` components. If `unknown-claim-family` or `retract-claim` is not visible in the test package, add the import next to the existing ones rather than qualifying.

- [ ] **Step 2: Run to verify they fail**

Expected: load failure on `claim-namespaces` (unexported/undefined).

- [ ] **Step 3: Implement**

Append to `spacetime/claim-query.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; Vocabulary: what a family names (GH #350, spec 2026-09-07 §4).
;;;
;;; Names come from the family's ordered indexes by MAP-INDEX-PREFIXES;
;;; every name is confirmed by resolving one live node under it (R7),
;;; counts are index-range sizes (R2), and :CURRENT resolves the range
;;; (R4).  Membership is live: an open WITH-AS-OF extent changes only
;;; what a name's nodes resolve to (R6).
;;; ---------------------------------------------------------------------------

(defun %refuse-vocabulary-axis (as-of as-of-epoch)
  "The listing answers live membership only (GH #350 R6)."
  (when (or as-of as-of-epoch)
    (error 'graph-db:query-precondition-error
           :reason "The vocabulary listing has no :AS-OF / :AS-OF-EPOCH ~
axis: index membership is live (GH #350, docs/time-travel.md Bounds).")))

(defun %vocabulary-sources (family role)
  "The (CLASS SLOTS) pairs the walk reads for ROLE: the subject index on
the parent, the object index on the binary class -- declared on
different classes, and the parent signals for the object slots."
  (ecase role
    (:subject (list (list (claim-family-parent family)
                          '(subject-namespace subject-key))))
    (:object (list (list (claim-family-binary family)
                         '(object-namespace object-key))))
    (:either (append (%vocabulary-sources family :subject)
                     (%vocabulary-sources family :object)))))

(defun %vocabulary-key (slots prefix)
  "PREFIX as MAP-INDEX and INDEX-COUNT take it: a scalar on a
single-slot index, the tuple otherwise."
  (if (= 1 (length slots)) (first prefix) prefix))

(defun %name-admitted-p (graph class slots prefix current)
  "T when a live node -- a current claim, with CURRENT -- sits under
PREFIX (R7, R4); stops at the first."
  (let ((key (%vocabulary-key slots prefix)))
    (block found
      (graph-db:map-index
       (lambda (node)
         (when (or (not current) (claim-current-p node))
           (return-from found t)))
       graph class slots :start key :end key)
      nil)))

(defun %name-count (graph class slots prefix current)
  "Claims under PREFIX: the index range's size, or with CURRENT the
current claims in it, each resolved (R2, R4)."
  (let ((key (%vocabulary-key slots prefix)))
    (if current
        (let ((n 0))
          (graph-db:map-index
           (lambda (node) (when (claim-current-p node) (incf n)))
           graph class slots :start key :end key)
          n)
        (graph-db:index-count graph class slots key :prefix t))))

(defun %walk-names (graph class slots arity start position current counts)
  "The admitted names under (CLASS SLOTS) at ARITY from START, in index
order: the component at POSITION of each prefix, or (NAME . COUNT) with
COUNTS.  With START the walk stops at the first prefix whose leading
component leaves START's."
  (let ((names '()))
    (block walk
      (graph-db:map-index-prefixes
       (lambda (prefix)
         (when (and start (not (equal (first prefix) (first start))))
           (return-from walk))
         (when (%name-admitted-p graph class slots prefix current)
           (let ((name (nth position prefix)))
             (push (if counts
                       (cons name (%name-count graph class slots prefix
                                               current))
                       name)
                   names))))
       graph class slots :arity arity :start start))
    (nreverse names)))

(defun %name-lessp (a b)
  "Index order for two names: the engine's per-component collation, NIL
first."
  (graph-db::less-than a b))

(defun %merge-names (lists counts)
  "LISTS, each in index order, as one list in index order without
duplicates; with COUNTS the entries are (NAME . COUNT) and a name in
several lists sums its counts."
  (let ((all (stable-sort (apply #'append lists) #'%name-lessp
                          :key (if counts #'car #'identity)))
        (out '()))
    (dolist (e all (nreverse out))
      (let ((name (if counts (car e) e)))
        (if (and out (equal name (if counts (car (first out)) (first out))))
            (when counts (incf (cdr (first out)) (cdr e)))
            (push (if counts (cons name (cdr e)) name) out))))))

(defun claim-namespaces (graph claim-class
                         &key (role :either) current counts
                              as-of as-of-epoch)
  "The namespaces CLAIM-CLASS's family names as subject, object or
either, in index order, one entry per name; with COUNTS each is
\(NAME . COUNT), the claims under it in ROLE, summed under :EITHER.
The default lists every name the indexes hold, retracted claims
included; :CURRENT keeps a name only if a claim under it is current
and counts only those.  Trap: membership is live -- :AS-OF and
:AS-OF-EPOCH are refused, and an open WITH-AS-OF extent changes only
what a name's claims resolve to (GH #350)."
  (check-type role (member :subject :object :either))
  (%refuse-vocabulary-axis as-of as-of-epoch)
  (let ((family (claim-family claim-class)))
    (%merge-names
     (loop for (class slots) in (%vocabulary-sources family role)
           collect (%walk-names graph class slots 1 nil 0 current counts))
     counts)))

(defun claim-relations (graph claim-class
                        &key current counts as-of as-of-epoch)
  "The relations CLAIM-CLASS's family uses, in index order, from its
CLAIM-RELATION index; with COUNTS, (NAME . COUNT).  :CURRENT and the
refusals as CLAIM-NAMESPACES (GH #350)."
  (%refuse-vocabulary-axis as-of as-of-epoch)
  (let ((family (claim-family claim-class)))
    (%walk-names graph (claim-family-parent family) '(relation)
                 1 nil 0 current counts)))
```

Before using `graph-db::less-than`, confirm the generic's name at `utilities.lisp` ~300 (the collation `%index-value-lessp`'s docstring names); if it differs, use the real name.

Exports in `spacetime/package.lisp` after line 53:

```lisp
   #:claim-namespaces #:claim-relations         ; GH #350
```

- [ ] **Step 4: Run the five tests, then both suites via the runner**

Expected: PASS; counts ≥ baseline + new. If a namespace order assertion fails, print the observed order and check facts E5 before touching the expected list.

- [ ] **Step 5: Column check and commit** — `feat(spacetime): claim-namespaces and claim-relations (#350)` with trailers.

---

### Task 5: `claim-keys`

**Files:**
- Modify: `spacetime/claim-query.lisp` (after `claim-relations`), `spacetime/package.lisp` (same export line)
- Test: `tests/spacetime/vocabulary-tests.lisp` (append)

**Interfaces:**
- Consumes: `%vocabulary-sources`, `%walk-names`, `%merge-names`, `%refuse-vocabulary-axis`, `%paginate` (Task 4 / existing).
- Produces: `(claim-keys graph claim-class namespace &key (role :either) current counts limit offset as-of as-of-epoch)` → `(values keys more-p)`.

- [ ] **Step 1: Write the failing test**

```lisp
(test claim-keys-lists-keys-under-a-namespace-merged-and-paged
  "Spec §4.1-4.2: keys under NAMESPACE from both roles, merged and
de-duplicated in index order, counted per role or summed; :LIMIT /
:OFFSET page the merged list and the second value says whether more
existed; nothing filed there answers NIL."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-b :ns "c" :ns "b")
      (%ns-b :other "x" :ns "c")
      (%ns-u :other "y"))
    (is (equal '("a" "b" "c") (claim-keys g 'ct-claim :ns)))
    (is (equal '("a" "c") (claim-keys g 'ct-claim :ns :role :subject)))
    (is (equal '("b" "c") (claim-keys g 'ct-claim :ns :role :object)))
    (is (equal '(("a" . 1) ("b" . 1) ("c" . 2))
               (claim-keys g 'ct-claim :ns :counts t))
        "c is a subject once and an object once")
    (is (equal '("x" "y") (claim-keys g 'ct-claim :other)))
    (is (null (claim-keys g 'ct-claim :nowhere)))
    (multiple-value-bind (page more) (claim-keys g 'ct-claim :ns :limit 2)
      (is (equal '("a" "b") page))
      (is (eq t more)))
    (multiple-value-bind (page more)
        (claim-keys g 'ct-claim :ns :limit 2 :offset 2)
      (is (equal '("c") page))
      (is (null more)))
    (signals graph-db:query-precondition-error
      (claim-keys g 'ct-claim :ns :as-of 1))))
```

- [ ] **Step 2: Run to verify it fails** — undefined `claim-keys`.

- [ ] **Step 3: Implement**

```lisp
(defun claim-keys (graph claim-class namespace
                   &key (role :either) current counts limit offset
                        as-of as-of-epoch)
  "The keys filed under NAMESPACE by CLAIM-CLASS's family in ROLE, in
index order, one entry per key, with COUNTS as (KEY . COUNT); NIL when
nothing is filed there.  :LIMIT / :OFFSET page the merged list; the
second value is T when entries existed past the cut.  :CURRENT and the
refusals as CLAIM-NAMESPACES (GH #350)."
  (check-type role (member :subject :object :either))
  (%refuse-vocabulary-axis as-of as-of-epoch)
  (let ((family (claim-family claim-class)))
    (%paginate
     (%merge-names
      (loop for (class slots) in (%vocabulary-sources family role)
            collect (%walk-names graph class slots 2 (list namespace) 1
                                 current counts))
      counts)
     limit offset)))
```

Export `#:claim-keys` on the Task 4 export line.

- [ ] **Step 4: Run the test and the spacetime suite via the runner.**

- [ ] **Step 5: Commit** — `feat(spacetime): claim-keys (#350)` with trailers.

---

### Task 6: Inside a transaction

**Files:**
- Modify: `spacetime/claim-query.lisp` — `%name-admitted-p`, `%name-count`, `%walk-names`, plus new `%vocabulary-view`, `%view-resolve`, `%claim-tuple`, `%created-under`
- Test: `tests/spacetime/vocabulary-tests.lisp` (append)

**Interfaces:**
- Consumes: `graph-db:make-commit-view`, `view-node`, `view-writes`, `view-old-node`; `claim-subject-namespace`, `claim-subject-key`, `claim-object-namespace`, `claim-object-key`, `claim-relation` accessors.
- Produces: nothing new; the three functions answer what the open transaction will commit (spec §4.4, R5).

- [ ] **Step 1: Write the failing tests**

```lisp
(test vocabulary-inside-a-transaction-is-what-it-will-commit
  "Spec §4.4, R5 (the GH #324 rule): a name asserted in the open
transaction is listed and counted; one whose only claim the
transaction deleted is gone; a retraction in the transaction moves the
:CURRENT answer; nothing of it is visible to the index until commit."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :other "b"))
    (with-transaction ()
      (%ns-u :fresh "n")
      (is (equal '(:fresh :ns :other) (claim-namespaces g 'ct-claim))
          "a created claim's namespace is listed before commit")
      (is (equal '((:fresh . 1) (:ns . 1) (:other . 1))
                 (claim-namespaces g 'ct-claim :counts t)))
      (is (equal '("n") (claim-keys g 'ct-claim :fresh)))
      (graph-db:mark-deleted (first (claims-touching g 'ct-claim :ns "a")))
      (is (equal '(:fresh :other) (claim-namespaces g 'ct-claim))
          "a name whose only claim the transaction deleted drops")
      (retract-claim (first (claims-touching g 'ct-claim :other "b")))
      (is (equal '(:fresh :other) (claim-namespaces g 'ct-claim))
          "retracted, still believed once: listed by default")
      (is (equal '(:fresh) (claim-namespaces g 'ct-claim :current t))
          "the retraction is seen by :current before commit")
      (is (equal '((:fresh . 1) (:other . 1))
                 (claim-namespaces g 'ct-claim :counts t))))
    (is (equal '(:fresh :other) (claim-namespaces g 'ct-claim))
        "after commit the index agrees")
    (is (equal '(:fresh) (claim-namespaces g 'ct-claim :current t)))))

(test vocabulary-in-a-transaction-counts-created-and-retracted-claims
  "Spec §4.4: inside a transaction a count is the committed count
adjusted by the transaction's own writes, under the default and under
:CURRENT."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :ns "b"))
    (with-transaction ()
      (%ns-u :ns "c")
      (retract-claim (first (claims-touching g 'ct-claim :ns "a")))
      (is (equal '((:ns . 3)) (claim-namespaces g 'ct-claim :counts t)))
      (is (equal '((:ns . 2))
                 (claim-namespaces g 'ct-claim :counts t :current t)))
      (is (equal '(("b" . 1) ("c" . 1))
                 (claim-keys g 'ct-claim :ns :counts t :current t)))
      (is (equal '(("r" . 3)) (claim-relations g 'ct-claim :counts t))
          "the created claim's relation counts before commit"))))
```

- [ ] **Step 2: Run to verify they fail** — the created claim's name is missing (the index does not hold it yet).

- [ ] **Step 3: Implement**

Add before `%name-admitted-p`:

```lisp
(defun %vocabulary-view (graph)
  "The commit view of the open transaction on GRAPH, or NIL outside one
\(the GH #324 rule, R5)."
  (let ((tx graph-db::*transaction*))
    (and tx (graph-db:make-commit-view graph tx))))

(defun %view-resolve (view node)
  "NODE as the transaction will commit it: NODE itself outside a
transaction, its written version inside one, NIL if that write deletes
it."
  (if view (graph-db:view-node view (graph-db:id node)) node))

(defun %claim-tuple (claim slots)
  "CLAIM's values for the index SLOTS, in order."
  (loop for slot in slots
        collect (ecase slot
                  (subject-namespace (claim-subject-namespace claim))
                  (subject-key (claim-subject-key claim))
                  (object-namespace (claim-object-namespace claim))
                  (object-key (claim-object-key claim))
                  (relation (claim-relation claim)))))

(defun %created-under (view class slots prefix current)
  "The claims of CLASS the open transaction created whose SLOTS tuple
starts with PREFIX (NIL for any prefix) -- current ones with CURRENT.
NIL outside a transaction."
  (when view
    (let ((out '()))
      (dolist (w (graph-db:view-writes view) (nreverse out))
        (let ((n (graph-db:view-node view (graph-db:id w))))
          (when (and n
                     (typep n class)
                     (null (graph-db:view-old-node view n))
                     (or (null prefix)
                         (every #'equal prefix
                                (subseq (%claim-tuple n slots)
                                        0 (length prefix))))
                     (or (not current) (claim-current-p n)))
            (push n out)))))))
```

Replace `%name-admitted-p`, `%name-count` and `%walk-names` with:

```lisp
(defun %name-admitted-p (graph class slots prefix current view)
  "T when a claim under PREFIX resolves live through VIEW -- current,
with CURRENT (R7, R4); stops at the first."
  (let ((key (%vocabulary-key slots prefix)))
    (block found
      (graph-db:map-index
       (lambda (node)
         (let ((n (%view-resolve view node)))
           (when (and n (or (not current) (claim-current-p n)))
             (return-from found t))))
       graph class slots :start key :end key)
      nil)))

(defun %name-count (graph class slots prefix current view)
  "Claims under PREFIX as the transaction will commit them: outside a
transaction and without CURRENT the index range's size; otherwise each
committed entry resolved through VIEW, plus the claims the transaction
created under PREFIX (R2, R4, §4.4)."
  (let ((key (%vocabulary-key slots prefix)))
    (if (and (null view) (not current))
        (graph-db:index-count graph class slots key :prefix t)
        (let ((n (length (%created-under view class slots prefix
                                         current))))
          (graph-db:map-index
           (lambda (node)
             (let ((c (%view-resolve view node)))
               (when (and c (or (not current) (claim-current-p c)))
                 (incf n))))
           graph class slots :start key :end key)
          n))))

(defun %walk-names (graph class slots arity start position current counts)
  "The admitted names under (CLASS SLOTS) at ARITY from START, in index
order, plus the names the open transaction's created claims introduce:
the component at POSITION of each prefix, or (NAME . COUNT) with
COUNTS.  With START the walk stops at the first prefix whose leading
component leaves START's.  The result is sorted by %MERGE-NAMES."
  (let* ((view (%vocabulary-view graph))
         (seen '())
         (names '()))
    (flet ((note (prefix)
             (push prefix seen)
             (let ((name (nth position prefix)))
               (push (if counts
                         (cons name (%name-count graph class slots prefix
                                                 current view))
                         name)
                     names))))
      (block walk
        (graph-db:map-index-prefixes
         (lambda (prefix)
           (when (and start (not (equal (first prefix) (first start))))
             (return-from walk))
           (when (%name-admitted-p graph class slots prefix current view)
             (note prefix)))
         graph class slots :arity arity :start start))
      ;; Names only the transaction's own creates hold (GH #324).
      (dolist (c (%created-under view class slots start current))
        (let ((prefix (subseq (%claim-tuple c slots) 0 arity)))
          (unless (member prefix seen :test #'equal)
            (note prefix)))))
    (nreverse names)))
```

`%merge-names` (Task 4) sorts, so a created name lands in index order. `%created-under view class slots start ...` with `start` NIL means every created claim of the class; with `(namespace)` only those under it.

- [ ] **Step 4: Run the two tests, then the whole `vocabulary-tests` set and the spacetime suite via the runner**

Expected: PASS; every Task 4 and 5 test still green (outside a transaction `view` is NIL and the fast paths are unchanged).

- [ ] **Step 5: Commit** — `feat(spacetime): the vocabulary sees the open transaction's writes (#350)` with trailers.

---

### Task 7: Documentation

**Files:**
- Modify: `docs/general-index-design.md` (append `### 6a.` after line 127, inside §6)
- Modify: `docs/vivace-graph-v3-doc.org` (new `*** Vocabulary: what a family names` between lines 5907 and 5908; the index sentence at 5794-5803 gains the fifth index)
- Modify: `CHANGELOG.md` (first bullet under Unreleased/Added, line 16)
- Modify: the spec's Status line

- [ ] **Step 1: `docs/general-index-design.md`**, after the last line of §6:

```markdown
### 6a. Distinct-prefix walk and counts (GH #350)

`map-index-prefixes fn graph class slot &key arity start` calls FN once
per distinct leading prefix of ARITY components, in index order, NIL
standing for a null component. It seeks: one range cursor per prefix,
opened at the previous prefix's high bound (`%index-bounds` with
`prefix` true), which sorts past every tuple sharing it. Cost is the
number of distinct prefixes times log n; it never takes the open-ended
`ix-map` path. It holds no lock of its own (the cursors do, per
backend), so a walk is not an atomic snapshot: entries can come and go
between hops. Membership is live — under a read snapshot or `with-as-of`
a prefix's nodes may resolve to deleted or absent versions; callers that
care resolve them (`graph-db/spacetime`'s vocabulary does).

`index-count graph class slot value &key prefix` is the size of a
tuple's or prefix's range, counted entries, no node resolution; 0 for an
absent prefix or a declared-but-empty index.
```

- [ ] **Step 2: the org manual.** Insert at the same `***` level after the "Finding claims, and reading their extent" subsection ends (before "The source onboarding contract"):

```org
*** Vocabulary: what a family names (GH #350)

Before an exact read a caller often needs to know what is there:
which namespaces a family's claims use, which relations, which keys
sit under a namespace. Three functions answer from the family's own
indexes, in index order, without walking its claims:

#+BEGIN_SRC lisp
  (claim-namespaces graph 'site-claim)                 ; (:device :sym ...)
  (claim-namespaces graph 'site-claim :role :object :counts t)
  (claim-relations graph 'site-claim :counts t)        ; (("wifi-drop" . 12) ...)
  (claim-keys graph 'site-claim :device :limit 50)     ; ("d1" "d42" ...), more-p
#+END_SRC

~:counts t~ makes each entry ~(name . count)~; under ~:role :either~
the count sums the subject and object roles. The default lists every
name the indexes hold, retracted claims included -- they are the
record of what was believed; ~:current t~ keeps a name only if a claim
under it is current, and counts only those. Inside an open
transaction the answer is what that transaction will commit, as
~claims-touching~ does. Names come from index membership, which is
live: ~:as-of~ and ~:as-of-epoch~ are refused, and an open
~with-as-of~ extent changes only what a name's claims resolve to
(chapter 12, "Reading as of an epoch"). Distinct relations ride a fifth
index every family declares, ~claim-relation~; a family created before
it gets it on its next open.
```

In the sentence at 5794-5803 that lists the family's indexes, add `claim-relation` on `(relation)`.

- [ ] **Step 3: CHANGELOG**, first bullet under Unreleased/Added:

```markdown
- **Claim-family vocabulary** (#350): `claim-namespaces`,
  `claim-relations` and `claim-keys` list what a family names, in
  index order, with opt-in counts, `:current`, paging on keys, and the
  open transaction's own writes overlaid; refused on the epoch axes
  (membership is live). Built on two new engine primitives,
  `map-index-prefixes` (a seek-and-skip walk of distinct leading
  prefixes) and `index-count`, and a fifth index every family declares,
  `claim-relation`. No storage format change; existing families build
  the new index on next open. `docs/general-index-design.md` §6a, the
  manual's spacetime chapter.
```

- [ ] **Step 4: Spec Status** — append "; implemented on `feat/claim-vocabulary`" to the Status line.

- [ ] **Step 5: Commit** — `docs: claim-family vocabulary (#350)` with trailers.

---

### Task 8: Whole-branch verification

- [ ] **Step 1: Column check on every touched Lisp file**

```bash
awk 'length > 80 {print FILENAME":"FNR}' index.lisp package.lisp spacetime/claim.lisp spacetime/claim-query.lisp spacetime/package.lisp tests/index-tests.lisp tests/package.lisp tests/spacetime/claim-query-tests.lisp tests/spacetime/vocabulary-tests.lisp
```

(compare against the base commit: no ADDED line over 80).

- [ ] **Step 2: Both suites via the runner** — `exit=0`, every count ≥ baseline.

- [ ] **Step 3: Diffstat sanity** — `git diff --stat 8e65e0f..HEAD`; `git diff 8e65e0f..HEAD -- tests | grep -c '^-[^-]'` explains every removed test line (none expected).

- [ ] **Step 4: Final whole-branch review** (SDD: most capable model), then hand the branch to the maintainer for push authorisation. Not pushed by this plan.
