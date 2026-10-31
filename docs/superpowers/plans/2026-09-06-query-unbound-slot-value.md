# node-slot-value enumeration and string/keyword unification — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `node-slot-value/3` enumerates when its node or slot is unbound, and a string unifies with a keyword, so a guarded query can find a vertex by slot value without `is-a`, list a vertex's slots, and filter a keyword-valued slot (GH #351).

**Architecture:** Three small changes in the Prolog core. `prolog-equal` (`prologc.lisp`) gains two methods. `node-slot-value/3` (`prolog-functors.lisp`) branches on which of its arguments are bound: bound node + bound slot is today's read; bound node + unbound slot walks `data-slots`; unbound node enumerates vertices the way `is-a/2` does with both arguments unbound, then recurses into the bound-node cases. Tests in the existing `query-suite` and `prolog-functor-suite`, plus one guarded-runner test.

**Tech Stack:** SBCL, FiveAM, graph-db's `select`/`select-flat` macros, `graph-db.query:run-guarded-prolog`.

**Spec:** kraison/cl-llm `docs/superpowers/specs/2026-09-06-memory-taxonomy-design.md` §5, and GH #351's body.

## Global Constraints

- Lisp: spaces only, hard 80 columns, terse comments that point at GH #351.
- Branch `fix/query-unbound-slot-value` from `experiment` (cde8a23), worktree
  `/tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-experiment/.worktrees/query-351`.
  Never touch `/home/raison/work/vivace-graph-v3`.
- Never run `pkill`, `pgrep -f`, or `kill`; other agents' SBCL images share the host.
- One SBCL build at a time in this worktree. Do not run the full suite (15 min); run the three suites below.
- Snapshot safety: any enumeration of vertices goes through `map-vertices` per type, as `is-a/2`'s both-unbound arm does, never a raw lhash scan.
- Docs travel with the code: `docs/guarded-query.md` changes in Task 4; every commit message names GH #351.
- Commit trailer on every commit:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
  ```

## Running the suites

Write this once to `/tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-351-suites.lisp`:

```lisp
;; Three suites, CI-style, in a fresh image.  The binding replicates
;; RUN-TESTS (tests/suite.lisp): every store needs the system registry.
(ql:quickload '(:graph-db/test :graph-db/query-test) :silent t)
(in-package :graph-db/test)
(log:config :error)
(let* ((system-dir (make-temp-directory))
       (graph-db::*system-directory* (namestring system-dir))
       (graph-db::*type-registry* nil)
       (ok t))
  (unwind-protect
       (dolist (suite '(prolog-functor-suite query-suite))
         (let ((r (fiveam:run suite)))
           (fiveam:explain! r)
           (unless (fiveam:results-status r) (setf ok nil))))
    (graph-db-test-scratch:cleanup-scratch-run))
  (unless (graph-db/query-test::run-query-tests) (setf ok nil))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root:

```bash
cd /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-experiment/.worktrees/query-351
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-351-suites.lisp \
  > /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-351-suites.log 2>&1; echo "exit=$?"
grep -E "Did [0-9]+ checks|Fail:" /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-351-suites.log
```

Expected before any change: exit 0, three `Did N checks` lines, `Fail: 0` each. Record the counts; every later run must not drop below them.

---

### Task 1: A string unifies with a keyword

**Files:**
- Modify: `prologc.lisp:116-129` (`prolog-equal`)
- Test: `tests/prolog-functor-tests.lisp` (append), `tests/query/guard-tests.lisp` (append)

**Interfaces:**
- Produces: `prolog-equal` succeeds for `(string keyword)` and `(keyword string)` when `string-equal` on the symbol name holds. Nothing else changes; `(string string)` stays `string=` (case-sensitive).

- [ ] **Step 1: Write the failing functor test**

Append to `tests/prolog-functor-tests.lisp`:

```lisp
(test a-string-unifies-with-a-keyword-case-insensitively
  "GH #351: a keyword-valued slot can only be filtered by a string --
the guard admits no keyword spelling -- so \"foo\" and :FOO unify, in
both orders, case-insensitively.  Two strings stay case-sensitive."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(:foo) (select-flat (?x) (= ?x :foo) (= ?x "foo"))))
    (is (equal '(:foo) (select-flat (?x) (= ?x :foo) (= ?x "FOO"))))
    (is (equal '("foo") (select-flat (?x) (= ?x "foo") (= ?x :foo))))
    (is (null (select-flat (?x) (= ?x :foo) (= ?x "bar"))))
    (is (null (select-flat (?x) (= ?x "foo") (= ?x "FOO")))
        "control: two strings stay case-sensitive")))
```

- [ ] **Step 2: Write the failing guarded-runner test**

Append to `tests/query/guard-tests.lisp`:

```lisp
(test a-keyword-slot-is-filtered-by-a-string
  "GH #351: RANK is untyped, so an item can hold a keyword there; the
guard refuses every keyword spelling, and a string now unifies."
  (with-query-graph (g)
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db/query-test.schema::make-qt-item :graph g :label "k"
                                                 :rank :high)
      (graph-db/query-test.schema::make-qt-item :graph g :label "n"
                                                 :rank 1))
    (let ((rows (nth-value 1 (q g "(is-a ?i qt-item)
                                   (node-slot-value ?i rank \"high\")
                                   (node-slot-value ?i label ?l)"))))
      (is (= 1 (length rows)))
      (is (string= "k" (second (first rows)))))
    (let ((rows (nth-value 1 (q g "(is-a ?i qt-item)
                                   (node-slot-value ?i rank ?r)
                                   (= ?r \"HIGH\")"))))
      (is (= 1 (length rows)) "case-insensitive through =/2 too"))))
```

- [ ] **Step 3: Run the suites; expect the two new tests to fail**

Run the suites (see "Running the suites"). Expected: `Fail: 1` in prolog-functor-suite (the `(= ?x "foo")` checks) and one failure in the query-test run; everything else green.

- [ ] **Step 4: Add the methods**

In `prologc.lisp`, after the `(:method ((x string) (y string)) (string= x y))` line of `prolog-equal`, add:

```lisp
  ;; A keyword-valued slot has no keyword spelling in a guarded query
  ;; (the screen refuses every colon), so a string stands in for one,
  ;; case-insensitively; two strings stay STRING= (GH #351).
  (:method ((x string) (y keyword)) (string-equal x (symbol-name y)))
  (:method ((x keyword) (y string)) (string-equal (symbol-name x) y))
```

- [ ] **Step 5: Run the suites; expect green**

Expected: exit 0, all three suites `Fail: 0`, functor-suite count up by 5, query-test count up by 3.

- [ ] **Step 6: Commit**

```bash
git add prologc.lisp tests/prolog-functor-tests.lisp tests/query/guard-tests.lisp
git commit -F - <<'EOF'
fix(prolog): a string unifies with a keyword, case-insensitively (#351)

A keyword-valued slot could not be filtered from a guarded query: the
screen refuses every keyword spelling and a string never unified with
the keyword.  PROLOG-EQUAL now matches a string against a keyword by
name, both orders; two strings stay case-sensitive.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 2: An unbound slot enumerates the vertex's slots

**Files:**
- Modify: `prolog-functors.lisp:852-866` (`node-slot-value/3`)
- Test: `tests/query-tests.lisp` (append)

**Interfaces:**
- Consumes: `data-slots` (`node-class.lisp:195`), a method on a node instance returning its managed slot names as symbols, inherited ones included; `node-slot-value node key` (`primitive-node.lisp:442`) accepts a symbol or keyword key.
- Produces: with the node bound and the slot unbound, one solution per slot: the slot variable bound to the slot name as a **keyword**, the value variable to `(node-slot-value node slot)` (NIL when unset). Bound node + bound slot unchanged.

- [ ] **Step 1: Write the failing test**

Append to `tests/query-tests.lisp`:

```lisp
(test node-slot-value-with-an-unbound-slot-lists-the-slots
  "GH #351: (node-slot-value ?p ?s ?v) yields one row per data slot of
?p's type, ?s a keyword, ?v the value or NIL; an inherited slot counts."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-employee :name "E" :title "boss"))
    (let ((rows (select (:flat nil) (?s ?v)
                        (is-a ?p g-employee) (node-slot-value ?p ?s ?v))))
      (is (equal '((:age nil) (:name "E") (:title "boss"))
                 (sort (copy-list rows) #'string< :key #'first))))))
```

- [ ] **Step 2: Run the suites; expect the new test to fail**

Expected: query-suite `Fail: 1` (today an unbound slot is a non-match, so `rows` is NIL).

- [ ] **Step 3: Restructure the functor**

Replace the whole `node-slot-value/3` definition in `prolog-functors.lisp` with:

```lisp
(defun %slot-value-guarded (node slot)
  "NODE's SLOT, or :FAIL on a read error.  Guards ONLY the read: the
continuation is the rest of the query and must keep its own errors."
  (handler-case (node-slot-value node slot)
    (error (c)
      (log:error "Problem reading (node-slot-value ~A ~A): ~A"
                 node slot c)
      :fail)))

(defun %node-has-slot-p (node slot)
  "SLOT (a symbol from any package) names a data slot of NODE's type."
  (member (symbol-name slot) (data-slots node)
          :key #'symbol-name :test #'string=))

(defun %unify-slot (node slot var cont)
  "The bound-node cases of NODE-SLOT-VALUE/3 (GH #351): SLOT bound
reads it; SLOT unbound yields every data slot, keyword-named."
  (cond ((var-p slot)
         (dolist (s (data-slots node))
           (let ((old-trail (fill-pointer *trail*))
                 (value (%slot-value-guarded node s)))
             (unless (eq value :fail)
               (when (and (unify slot (intern (symbol-name s) :keyword))
                          (unify var value))
                 (funcall cont)))
             (undo-bindings old-trail))))
        (t
         (let ((value (%slot-value-guarded node slot)))
           (unless (eq value :fail)
             (when (unify var value)
               (funcall cont)))))))

(def-global-prolog-functor node-slot-value/3 (node slot var cont)
  (setq node (var-deref node)
        slot (var-deref slot)
        var (var-deref var))
  (%unify-slot node slot var cont))
```

Keep the `old-trail`/`undo-bindings` shape exactly: each slot solution must unwind its bindings before the next, as `is-a/2` does.

- [ ] **Step 4: Run the suites; expect green**

Expected: all three `Fail: 0`; query-suite count up by 1. The existing `select-callback-streams-each-row`, `select-count-*` and the guarded `data-rows-are-json-shaped` tests cover the bound-slot path and must still pass.

- [ ] **Step 5: Commit**

```bash
git add prolog-functors.lisp tests/query-tests.lisp
git commit -F - <<'EOF'
feat(prolog): node-slot-value with an unbound slot lists the slots (#351)

(node-slot-value ?p ?s ?v) was a silent non-match; it now yields one
solution per data slot of ?p's type, ?s keyword-named, ?v the value or
NIL.  The bound-slot read is unchanged and still guards only the read.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 3: An unbound node enumerates the vertices

**Files:**
- Modify: `prolog-functors.lisp` (`node-slot-value/3` from Task 2)
- Test: `tests/query-tests.lisp` (append), `tests/query/guard-tests.lisp` (append)

**Interfaces:**
- Consumes: Task 2's `%unify-slot`, `%node-has-slot-p`; `is-a/2`'s both-unbound arm (`prolog-functors.lisp:893-935`) as the enumeration pattern: `(list-vertex-types *graph*)`, `(lookup-node-type-by-id type-id :vertex)`, `(map-vertices fn *graph* :vertex-type type-id :include-subclasses-p nil)`.
- Produces: with the node unbound, every vertex whose type declares the (bound) slot is a candidate; a vertex lacking the slot is skipped, never a NIL row. Node and slot both unbound: every vertex, then every slot.

- [ ] **Step 1: Write the failing tests**

Append to `tests/query-tests.lisp`:

```lisp
(test node-slot-value-with-an-unbound-node-finds-vertices-by-slot
  "GH #351: no IS-A needed to look a vertex up by a slot value, and a
vertex whose type lacks the slot is skipped rather than read as NIL."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-employee :name "C" :title "boss"))
    (let ((found (select-flat (?p) (node-slot-value ?p name "B"))))
      (is (= 1 (length found)))
      (is (string= "B" (slot-value (first found) 'name))))
    ;; TITLE is declared on g-employee only: two persons are skipped.
    (is (equal '("boss")
               (select-flat (?t) (node-slot-value ?p title ?t))))
    ;; Both unbound: every vertex, every slot.  3 vertices x 2 slots
    ;; (name, age) + 1 x title = 7 rows.
    (is (= 7 (select-count () (node-slot-value ?p ?s ?v))))))
```

Append to `tests/query/guard-tests.lisp`:

```lisp
(test node-slot-value-needs-no-is-a-through-the-guard
  "GH #351: the guarded runner reaches the enumeration; the ?c column
holds the node id as a string, as any node cell does."
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (q g "(node-slot-value ?i label \"b\") (node-slot-value ?i rank ?r)")
      (is (equal '("i" "r") columns))
      (is (= 1 (length rows)))
      (is (= 2 (second (first rows)))))))
```

- [ ] **Step 2: Run the suites; expect the two new tests to fail**

Expected: query-suite `Fail: 1`, query-test one failure.

- [ ] **Step 3: Add the unbound-node arm**

Replace the `node-slot-value/3` definition (from Task 2) with:

```lisp
(def-global-prolog-functor node-slot-value/3 (node slot var cont)
  (setq node (var-deref node)
        slot (var-deref slot)
        var (var-deref var))
  (if (var-p node)
      ;; Enumerate as IS-A/2's both-unbound arm does: per-type scans,
      ;; each through LOOKUP-VERTEX, so a snapshot reader sees its own
      ;; epoch.  A vertex whose type lacks a bound SLOT is skipped, not
      ;; read as NIL (GH #351).
      (dolist (type-id (list-vertex-types *graph*))
        (when (lookup-node-type-by-id type-id :vertex)
          (map-vertices
           (lambda (vertex)
             (when (or (var-p slot) (%node-has-slot-p vertex slot))
               (let ((old-trail (fill-pointer *trail*)))
                 (when (unify node vertex)
                   (%unify-slot vertex slot var cont))
                 (undo-bindings old-trail))))
           *graph* :vertex-type type-id :include-subclasses-p nil)))
      (%unify-slot node slot var cont)))
```

- [ ] **Step 4: Run the suites; expect green**

Expected: all three `Fail: 0`; query-suite count up by 4, query-test up by 3.

- [ ] **Step 5: Commit**

```bash
git add prolog-functors.lisp tests/query-tests.lisp tests/query/guard-tests.lisp
git commit -F - <<'EOF'
feat(prolog): node-slot-value with an unbound node enumerates vertices (#351)

(node-slot-value ?c subject-key "x") alone was a silent non-match, so a
guarded query had to bind ?c with is-a first.  An unbound node now
enumerates vertices per type through map-vertices, as is-a/2 does; a
vertex whose type lacks the slot is skipped, never a NIL row.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 4: Docs

**Files:**
- Modify: `docs/guarded-query.md` ("What the guard admits", after the two bullets that begin "Two things a new caller reliably gets wrong first")

- [ ] **Step 1: Add the paragraph**

After the second bullet (the one ending "from client text."), add:

```markdown

Three things `node-slot-value/3` does that the bare functor list does
not say (GH #351):

- **A keyword-valued slot is filtered by a string.** The screen admits
  no keyword spelling, so `(node-slot-value ?i kind "widget")` matches
  a slot holding `:widget`, case-insensitively; so does `(= ?k
  "WIDGET")` after binding. Two strings stay case-sensitive.
- **An unbound node enumerates.** `(node-slot-value ?i label "b")`
  alone finds every vertex whose type declares `label` with that
  value; no `is-a` needed. A vertex whose type lacks the slot is
  skipped, never read as null.
- **An unbound slot lists the slots.** `(node-slot-value ?i ?s ?v)`
  yields one row per data slot of the vertex's type, `?s` the slot
  name as a keyword and `?v` its value or null.
```

- [ ] **Step 2: Check columns and commit**

```bash
awk 'length > 80 {print FILENAME":"FNR}' prologc.lisp prolog-functors.lisp tests/query-tests.lisp tests/prolog-functor-tests.lisp tests/query/guard-tests.lisp
git add docs/guarded-query.md
git commit -F - <<'EOF'
docs(guarded-query): node-slot-value enumeration and string/keyword unification (#351)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

Expected: no over-80 lines in the Lisp files; commit succeeds.
