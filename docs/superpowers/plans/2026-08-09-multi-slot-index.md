# Multi-slot Indexes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the general ordered index from one slot to a tuple of slots, with
leading-prefix lookup, and add a class-level multi-slot unique constraint.

**Architecture:** The index key stays a **flat list** `(v1 … vn id)`, so today's
single-slot `(value id)` is literally the n=1 case and no existing index rebuilds. That
property depends on a generalised comparator being order-identical at n=2, so building
and proving that comparator is Task 1 and everything else sits on it. A null component
is stored under a dedicated sentinel in ordinary indexes (keeping the row findable by
prefix) and exempts the tuple entirely in unique indexes (SQL semantics).

**Tech Stack:** Common Lisp, SBCL, FiveAM, ASDF. Engine files are flat in the repo root;
tests live in `tests/`.

**Spec:** `docs/superpowers/specs/2026-08-09-multi-slot-index-design.md`. **Issue:**
#107.

## Global Constraints

- **Branch:** work on a new branch off `experiment`. Do not commit to `experiment`.
- **Verify on SBCL only.** ECL is demoted to periodic; say explicitly when it was
  skipped.
- **80-column limit in Lisp source.** Code, comments, docstrings and strings alike.
- **Spaces only, never tabs**, in any Lisp file.
- **Terse comments**: state the invariant and why it matters, then cite `(GH #107)`.
  Detail goes in the issue or the spec, never inline.
- **No rebuild, no storage-version change.** If either becomes necessary, stop and
  re-open the flat-key decision (spec §3.1) rather than working around it.
- Run the whole unit suite with `(asdf:test-system :graph-db)`; run one suite with
  `(fiveam:run! 'graph-db/test::index-suite)`.
- Full diff shown for review before every commit (repo standing directive).

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `globals.lisp` | constants | add `+null-component+` |
| `utilities.lisp` | generic ordering | `less-than` methods for the new sentinel |
| `index.lisp` | the ordered index | comparator, arity, tuple keys, decl, query, sidecar |
| `unique-constraint.lisp` | enforcement | `def-unique`, multi-slot descriptors |
| `memory-graph.lisp` | memory backend | arity-aware head/tail keys |
| `package.lisp` | exports | `def-unique` |
| `tests/index-tests.lisp` | index suite | multi-slot cases |
| `tests/unique-constraint-tests.lisp` | unique suite | multi-slot unique cases |
| `docs/vivace-graph-v3-doc.org` | manual | multi-slot section |
| `CHANGELOG.md` | release notes | entry |

---

### Task 1: The generalised comparator (the hard gate)

Everything else rests on this being order-identical at n=2. Build and prove it first.

**Files:**
- Modify: `index.lisp` (add after `%open-secondary-skip-list`, ~line 135)
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Consumes: `less-than` and `key-vector<` from `utilities.lisp`.
- Produces: `%index-comp-lessp (key1 key2) -> boolean` and
  `%index-equal (key1 key2) -> boolean`. Both take flat lists `(v1 … vn id)` of any
  length ≥ 2, and tolerate keys of *different* lengths (a shorter key sorts first),
  which is what makes prefix range bounds work.

- [ ] **Step 1: Write the failing test**

In `tests/index-tests.lisp`, after the `ix-names` helper:

```lisp
;;; --- the generalised composite comparator (GH #107) -------------------------

(test index-comparator-matches-reduce-comp-at-arity-2
  "%INDEX-COMP-LESSP must order 2-element keys exactly as REDUCE-COMP-LESSP, or
every existing single-slot index would need a rebuild."
  (let ((a (list "alice" graph-db::+null-key+))
        (b (list "bob"   graph-db::+null-key+))
        (c (list "alice" graph-db::+max-key+)))
    (is (eq (graph-db::reduce-comp-lessp a b)
            (graph-db::%index-comp-lessp a b)))
    (is (eq (graph-db::reduce-comp-lessp a c)
            (graph-db::%index-comp-lessp a c)))
    (is (eq (graph-db::reduce-comp-lessp b a)
            (graph-db::%index-comp-lessp b a)))
    (is (eq (graph-db::reduce-equal a a) (graph-db::%index-equal a a)))
    (is (eq (graph-db::reduce-equal a b) (graph-db::%index-equal a b)))))

(test index-comparator-orders-tuples-and-prefix-bounds
  "Longer keys order component-wise; a short bound key sorts before any
longer key sharing its prefix, so a prefix range scan terminates correctly."
  (let ((k  (list "a" "b" "c" graph-db::+null-key+))
        (lo (list "a" "b"))
        (hi (list "a" "b" graph-db::+max-sentinel+ graph-db::+max-key+)))
    (is-true  (graph-db::%index-comp-lessp lo k))
    (is-false (graph-db::%index-comp-lessp hi k))
    (is-true  (graph-db::%index-comp-lessp k hi))
    (is-false (graph-db::%index-equal lo k))))
```

- [ ] **Step 2: Run test to verify it fails**

Run in the REPL:
```lisp
(asdf:load-system :graph-db/test)
(fiveam:run! 'graph-db/test::index-suite)
```
Expected: FAIL — `%INDEX-COMP-LESSP` is undefined.

- [ ] **Step 3: Write minimal implementation**

In `index.lisp`, immediately after `%open-secondary-skip-list`:

```lisp
;;; Ordering for a flat index key (v1 ... vn id): every component but the last
;;; compares with LESS-THAN, the trailing node id with KEY-VECTOR<.  At n=2 this
;;; is exactly REDUCE-COMP-LESSP, which is what lets an existing single-slot
;;; index reopen under it without a rebuild (GH #107).  Keys of unequal length
;;; are tolerated -- the shorter sorts first -- so a prefix bound works.

(defun %index-comp-lessp (key1 key2)
  "True when KEY1 sorts before KEY2.  See the comment above for the contract."
  (let ((n1 (length key1))
        (n2 (length key2)))
    (loop for a in key1
          for b in key2
          for i from 0
          do (if (and (= i (1- n1)) (= i (1- n2)))
                 (return (key-vector< a b))
                 (cond ((less-than a b) (return t))
                       ((equal a b))
                       (t (return nil))))
          finally (return (< n1 n2)))))

(defun %index-equal (key1 key2)
  "Key equality matching %INDEX-COMP-LESSP: components by EQUAL, trailing id by
EQUALP.  At n=2 this is exactly REDUCE-EQUAL."
  (let ((n1 (length key1))
        (n2 (length key2)))
    (and (= n1 n2)
         (loop for a in key1
               for b in key2
               for i from 0
               always (if (= i (1- n1)) (equalp a b) (equal a b))))))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: PASS, and the pre-existing index tests still pass.

- [ ] **Step 5: Commit**

```bash
git add index.lisp tests/index-tests.lisp
git commit -m "feat(index): generalised composite comparator, order-identical at n=2 (#107)"
```

---

### Task 2: The null-component sentinel

**Files:**
- Modify: `globals.lisp:228-229` region; `utilities.lisp` (`less-than`, ~line 248)
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Produces: constant `+null-component+` (value `:gnull`), ordered strictly above
  `+min-sentinel+` and strictly below every real value and above `NIL`.

- [ ] **Step 1: Write the failing test**

```lisp
(test null-component-orders-below-real-values
  "+NULL-COMPONENT+ sits above +MIN-SENTINEL+ and below every real value, so a
null-bearing tuple falls inside a prefix scan of its populated parts (#107)."
  (is-true  (less-than graph-db::+min-sentinel+ graph-db::+null-component+))
  (is-false (less-than graph-db::+null-component+ graph-db::+min-sentinel+))
  (is-true  (less-than graph-db::+null-component+ graph-db::+max-sentinel+))
  (is-true  (less-than graph-db::+null-component+ 0))
  (is-true  (less-than graph-db::+null-component+ "a"))
  (is-true  (less-than graph-db::+null-component+ 'zzz))
  (is-false (less-than 0 graph-db::+null-component+))
  (is-false (less-than "a" graph-db::+null-component+))
  (is-false (less-than graph-db::+null-component+ graph-db::+null-component+)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: FAIL — `+NULL-COMPONENT+` is undefined.

- [ ] **Step 3: Write minimal implementation**

In `globals.lisp`, directly after `+max-sentinel+`:

```lisp
;; A stored "this component has no value" marker for a multi-slot index key.
;; Distinct from +MIN-SENTINEL+ (which stays a pure range bound, so an exact
;; match on a null component stays expressible) and from NIL (which %INDEX-KEY
;; already uses to mean "not indexable").  (GH #107)
(alexandria:define-constant +null-component+ :gnull)
```

In `utilities.lisp`, inside the `less-than` generic, immediately after the
`+max-sentinel+` methods and **before** the null/list catch-alls:

```lisp
  ;; +NULL-COMPONENT+: above +MIN-SENTINEL+, below every real value (GH #107).
  (:method ((x (eql +null-component+)) (y (eql +null-component+))) nil)
  (:method ((x (eql +min-sentinel+))   (y (eql +null-component+))) t)
  (:method ((x (eql +null-component+)) (y (eql +min-sentinel+)))   nil)
  (:method ((x (eql +null-component+)) (y (eql +max-sentinel+)))   t)
  (:method ((x (eql +max-sentinel+))   (y (eql +null-component+))) nil)
  (:method ((x (eql +null-component+)) (y number))  t)
  (:method ((x (eql +null-component+)) (y string))  t)
  (:method ((x (eql +null-component+)) (y symbol))  t)
  (:method ((x (eql +null-component+)) (y (eql t))) t)
  (:method ((x (eql +null-component+)) (y cons))    t)
  (:method ((x (eql +null-component+)) (y null))    nil)
  (:method ((x number)  (y (eql +null-component+))) nil)
  (:method ((x string)  (y (eql +null-component+))) nil)
  (:method ((x symbol)  (y (eql +null-component+))) nil)
  (:method ((x (eql t)) (y (eql +null-component+))) nil)
  (:method ((x cons)    (y (eql +null-component+))) nil)
  (:method ((x null)    (y (eql +null-component+))) t)
```

- [ ] **Step 4: Run test to verify it passes**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: PASS. Then run the full suite — `less-than` is used everywhere, so a
regression here is broad: `(asdf:test-system :graph-db)`.

- [ ] **Step 5: Commit**

```bash
git add globals.lisp utilities.lisp tests/index-tests.lisp
git commit -m "feat(index): +null-component+ sentinel and its ordering (#107)"
```

---

### Task 3: Slot-list normalisation — refactor behind characterisation tests

**Files:**
- Modify: `index.lisp:68` (`index-spec`), `:117` (`slot-index`), `:76`, `:89`, `:100`,
  `:198`, `:222`, `:233`, `:273`, `:446`, `:461`, `:470`, `:479`
- Test: `tests/index-tests.lisp` (characterisation tests, added FIRST)

**Interfaces:**
- Produces: `%normalize-slots (slot-or-list) -> list`. `index-spec-slot-names` and
  `slot-index-slot-names` replace the singular accessors. Registry keys become
  `(owner . slot-list)`.
- The public API (`def-index`, `index-lookup`, `map-index`, `index-range`) is
  **unchanged** in this task and still takes a bare symbol.

- [ ] **Step 1: Write characterisation tests and watch them PASS before refactoring**

These pin the *current* single-slot behaviour so the refactor is provably invisible.
Unusually for this plan they pass before the change — that is the point: they fail only
if the refactor alters observable behaviour. Do not assert that the refactor happened;
assert what a caller sees.

```lisp
;;; --- characterisation: single-slot behaviour must survive Task 3 (GH #107) --

(test characterise-single-slot-equality-and-range
  "Pins the caller-visible single-slot contract across the slot-list refactor."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)
      (make-ix-person :name "b" :age 40)
      (make-ix-person :name "c" :age 50))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'name "a"))))
    (is (equal '("a" "b") (ix-names (index-range g 'ix-person 'age
                                                 :start 30 :end 40))))
    (is (null (index-lookup g 'ix-person 'name "nope")))))

(test characterise-single-slot-canonicalizer-and-unindexed
  "Pins canonicalized lookup and the error on a genuinely unindexed slot."
  (with-ix-graph (g)
    (with-transaction () (make-ix-person :name "d" :email "D@X.COM"))
    (is (equal '("d") (ix-names (index-lookup g 'ix-person 'email "d@x.com"))))
    (signals error (index-lookup g 'ix-person 'title "x"))))
```

- [ ] **Step 2: Run them to confirm they PASS on the unmodified code**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: PASS. A failure here means the tests describe behaviour the code does not
have — fix the tests, not the code, before touching `index.lisp`.

- [ ] **Step 3: Add the normaliser and rename the struct slots**

In `index.lisp`, before `register-index-spec`:

```lisp
(defun %normalize-slots (slot-or-list)
  "A slot designator as a list.  A bare symbol becomes a 1-list, so single-slot
and multi-slot share one code path (GH #107)."
  (if (listp slot-or-list) slot-or-list (list slot-or-list)))
```

Change `(defstruct (index-spec …) owner-name slot-name graph-name canonicalize)` to use
`slot-names`, and `(defstruct (slot-index …) owner-name slot-name canonicalizer
skip-list)` to use `slot-names canonicalizers` (plural; a list of functions, NIL entries
meaning identity).

- [ ] **Step 4: Update every reader to the plural accessor**

Mechanically update the thirteen sites listed under **Files**. Registry keys change from
`(cons owner-name slot-name)` to `(cons owner-name slot-list)`; the hash tables are
already `:test 'equal`, so a list key needs no other change.

In `def-index` (`index.lisp:434`), wrap the slot at macroexpansion:
`:slot-names (%normalize-slots ',slot)`.

In the query helpers (`%secondary-index-lookup`, `%slot-index-declared-p`,
`%def-index-declared-p`, `%require-index`), normalise the incoming `slot-name` argument
with `%normalize-slots` on entry, so a symbol caller still resolves.

- [ ] **Step 5: Run the characterisation tests and the whole suite**

Run: `(fiveam:run! 'graph-db/test::index-suite)` then `(asdf:test-system :graph-db)`
Expected: PASS, with the characterisation tests still green and the total count higher
than before this task by exactly the two tests added in Step 1.

- [ ] **Step 6: Verify a pre-existing on-disk graph still reopens**

Run: `(fiveam:run! 'graph-db/test::index-suite)` — the suite's durability test reopens a
graph and re-queries. Confirm it passes rather than rebuilding.

- [ ] **Step 7: Commit**

```bash
git add index.lisp tests/index-tests.lisp
git commit -m "refactor(index): normalise slot designators to lists, no behaviour change (#107)"
```

---

### Task 4: Arity-aware skip-list construction

**Files:**
- Modify: `index.lisp:124` (`make-secondary-skip-list`), `:130`
  (`%open-secondary-skip-list`); `memory-graph.lisp:1257`
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Consumes: `%index-comp-lessp`, `%index-equal` (Task 1).
- Produces: `make-secondary-skip-list (graph arity)` and
  `%open-secondary-skip-list (graph address arity &optional backend)`. Head key is
`(min-sentinel × (1- arity+1)) + +null-key+`; tail is the `+max-sentinel+` / `+max-key+`
equivalent. Arity here means the **number of value components**, so a key has `arity +
1` elements.

- [ ] **Step 1: Write the failing test**

```lisp
(test secondary-skip-list-head-tail-match-arity
  "Head/tail sentinel keys must have arity+1 elements, or a multi-slot index's
bounds sort wrongly against real keys (#107)."
  (with-ix-graph (g)
    (let ((sl (graph-db::make-secondary-skip-list g 3)))
      (is (= 4 (length (graph-db::%sl-head-key sl))))
      (is (= 4 (length (graph-db::%sl-tail-key sl)))))))
```

*Note:* if `%sl-head-key` is not the accessor name on this backend, read
`skip-list.lisp:400` for the constructor's parameter names and use the matching reader.

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: FAIL — `MAKE-SECONDARY-SKIP-LIST` takes one argument.

- [ ] **Step 3: Write minimal implementation**

```lisp
(defun %index-head-key (arity)
  "Lower sentinel key for an index of ARITY value components (GH #107)."
  (append (make-list arity :initial-element +min-sentinel+) (list +null-key+)))

(defun %index-tail-key (arity)
  "Upper sentinel key for an index of ARITY value components (GH #107)."
  (append (make-list arity :initial-element +max-sentinel+) (list +max-key+)))

(defun make-secondary-skip-list (graph arity)
  "The ordered map backing a secondary index of ARITY value components."
  (make-heap-index (graph-index-backend graph) (indexes graph)
                   '%index-comp-lessp
                   :head-key (%index-head-key arity)
                   :tail-key (%index-tail-key arity)))
```

*Note:* `make-heap-index`'s current signature takes `(backend heap comparison)`. Read
`views.lisp:596` and the `make-heap-index` definition, and extend it with the two
keyword arguments (defaulting to the arity-1 keys) rather than changing its positional
shape.

Mirror the same arity derivation in the memory-graph method at `memory-graph.lisp:1257`,
replacing the hardcoded `(list +min-sentinel+ +null-key+)` / `(list +max-sentinel+
+max-key+)` and switching `:key-equal` to `'%index-equal` and `:key-comparison` to
`'%index-comp-lessp`.

- [ ] **Step 4: Run tests**

Run: `(asdf:test-system :graph-db)`
Expected: PASS. Existing single-slot indexes now run on `%index-comp-lessp` with
arity-1 head/tail keys — identical ordering to before.

- [ ] **Step 5: Commit**

```bash
git add index.lisp memory-graph.lisp tests/index-tests.lisp
git commit -m "feat(index): arity-aware head/tail keys and the generalised comparator (#107)"
```

---

### Task 5: Tuple key building and maintenance

**Files:**
- Modify: `index.lisp:136` (`%index-key`), `:154` (`ix-lookup`), `:164` (`ix-put`),
  `:170` (`ix-remove`), `:174` (`ix-map`), `:222` (`%ix-claim`), `:233` (`%ix-release`),
  `:273` (`rebuild-secondary-indexes`)
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Consumes: `+null-component+` (Task 2), `%normalize-slots` (Task 3).
- Produces: `%index-tuple-key (six node) -> list-or-nil` — the value components for
  NODE, canonicalizer applied per position, `+null-component+` substituted for a null
  component; NIL when **every** component is null (nothing to index).

- [ ] **Step 1: Write the failing test**

Add a two-slot test class near the top of `tests/index-tests.lisp`, beside `ix-person`:

```lisp
;; Multi-slot: the endpoint-identity shape (namespace, external-key) (GH #107).
(def-vertex ix-claim ()
  ((ns  :initarg :ns  :accessor ix-ns)
   (key :initarg :key :accessor ix-key)
   (rel :initarg :rel :accessor ix-rel))
  :graph-db-index-test)

(def-index ix-claim (ns key rel) :graph-db-index-test)
```

And the test:

```lisp
(test multi-slot-index-finds-exact-tuple
  "A three-component index resolves an exact tuple (#107)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-claim :ns "ops" :key "e1" :rel "at")
      (make-ix-claim :ns "ops" :key "e2" :rel "at"))
    (let ((hits (index-lookup g 'ix-claim '(ns key rel)
                             (list "ops" "e1" "at"))))
      (is (= 1 (length hits)))
      (is (string= "e1" (ix-key (first hits)))))))

(test multi-slot-index-stores-null-component
  "A tuple with a null component is still indexed, so it stays findable (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key nil :rel "at"))
    (is (= 1 (length (index-lookup g 'ix-claim '(ns key rel)
                                   (list "ops") :prefix t))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: FAIL — the multi-slot declaration is not yet honoured.

- [ ] **Step 3: Write minimal implementation**

```lisp
(defun %index-tuple-key (six node)
  "The value components of NODE's key for SIX, per-position canonicalizer
applied and +NULL-COMPONENT+ substituted for a null.  NIL when every
component is null -- there is nothing to index (GH #107)."
  (let* ((slots (slot-index-slot-names six))
         (cans  (slot-index-canonicalizers six))
         (any nil)
         (key (loop for s in slots
                    for i from 0
                    for v = (slot-value node s)
                    collect (cond ((null v) +null-component+)
                                  (t (setf any t)
                                     (let ((c (nth i cans)))
                                       (if c (funcall c v) v)))))))
    (when any key)))
```

Update `ix-put` / `ix-remove` to append the id to the tuple:
`(add-to-skip-list (slot-index-skip-list six) (append key (list id)) nil)`, and read the
id back with `(car (last (%sn-key node)))` rather than `(second …)`.

Update `%ix-claim` / `%ix-release` / `rebuild-secondary-indexes` to call
`%index-tuple-key` once per descriptor instead of reading a single slot. Keep the
`%indexable-value-p` geometry gate, applied per component.

- [ ] **Step 4: Run tests**

Run: `(asdf:test-system :graph-db)`
Expected: PASS, including every pre-existing single-slot test.

- [ ] **Step 5: Commit**

```bash
git add index.lisp tests/index-tests.lisp
git commit -m "feat(index): tuple key building and maintenance (#107)"
```

---

### Task 6: Declaration surface

**Files:**
- Modify: `index.lisp:421` (`def-index`), `:25` (`%resolve-index-canonicalizer`)
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Produces: `def-index` accepting a list in the slot position and `:canonicalize`
  accepting a positional list (NIL entry = identity). A bare symbol and a single
  function keep working verbatim.

- [ ] **Step 1: Write the failing test**

```lisp
(def-index ix-claim (ns key) :graph-db-index-test
  :canonicalize (string-downcase nil))

(test multi-slot-canonicalizer-is-positional
  "A per-position canonicalizer applies to its own component only (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "OPS" :key "E1" :rel "at"))
    (is (= 1 (length (index-lookup g 'ix-claim '(ns key) (list "ops" "E1")))))
    (is (= 0 (length (index-lookup g 'ix-claim '(ns key) (list "ops" "e1")))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: FAIL — `:canonicalize` is resolved as a single function.

- [ ] **Step 3: Write minimal implementation**

```lisp
(defun %resolve-index-canonicalizers (spec arity)
  "SPEC -> a list of ARITY canonicalizers (NIL = identity).  A single spec
applies to component 0 (the single-slot form); a list is positional (#107)."
  (if (and (consp spec) (not (member (car spec) '(function lambda))))
      (loop for i from 0 below arity
            collect (%resolve-index-canonicalizer (nth i spec)))
      (cons (%resolve-index-canonicalizer spec)
            (make-list (max 0 (1- arity)) :initial-element nil))))
```

Wire it into `%slot-index-for` in place of the singular resolver, and pass the
already-normalised slot list through `def-index`.

- [ ] **Step 4: Run tests**

Run: `(asdf:test-system :graph-db)` — Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add index.lisp tests/index-tests.lisp
git commit -m "feat(index): def-index accepts a slot list and positional canonicalizers (#107)"
```

---

### Task 7: Query API — tuple lookup and `:prefix`

**Files:**
- Modify: `index.lisp:491` (`index-lookup`), `:507` (`map-index`), `:524`
  (`index-range`)
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Produces: `index-lookup (graph class-name slots value &key prefix collect-p)`. A
  `value` list shorter than the index arity **signals** unless `:prefix t`.
  `map-index` / `index-range` take tuple `:start` / `:end`.

- [ ] **Step 1: Write the failing test**

```lisp
(test short-value-without-prefix-signals
  "A value list shorter than the arity must signal, never silently return a
superset -- silent-wrong-answer is this project's dominant defect shape (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "at"))
    (signals error (index-lookup g 'ix-claim '(ns key rel) (list "ops")))
    (is (= 1 (length (index-lookup g 'ix-claim '(ns key rel)
                                   (list "ops") :prefix t))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: FAIL — no arity check exists.

- [ ] **Step 3: Write minimal implementation**

```lisp
(defun %index-bounds (six value prefix)
  "Range bounds for VALUE against SIX.  Signals when VALUE is shorter than the
index arity and PREFIX is not requested (GH #107)."
  (let* ((vals  (if (listp value) value (list value)))
         (arity (length (slot-index-slot-names six))))
    (cond ((= (length vals) arity)
           (values (append vals (list +null-key+))
                   (append vals (list +max-key+))))
          ((and prefix (< (length vals) arity))
           (values vals
                   (append vals
                           (make-list (- arity (length vals))
                                      :initial-element +max-sentinel+)
                           (list +max-key+))))
          (t (error "Index on ~S has arity ~D; got ~D value(s)~
~:[~; -- pass :PREFIX T~]"
                    (slot-index-slot-names six) arity (length vals)
                    (< (length vals) arity))))))
```

Have `index-lookup` build its cursor from `%index-bounds` instead of `ix-lookup`'s
hardcoded pair, and thread `:prefix` through. `map-index` / `index-range` accept tuple
`:start` / `:end` and pad short bounds the same way.

- [ ] **Step 4: Run tests**

Run: `(asdf:test-system :graph-db)` — Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add index.lisp tests/index-tests.lisp
git commit -m "feat(index): tuple lookup with an explicit :prefix (#107)"
```

---

### Task 8: Sidecar reader shim

**Files:**
- Modify: `index.lisp:299` (`save-secondary-index-roots`), `:333`
  (`restore-secondary-index-roots`), `:318` (`%owner-slot-canonicalizer`)
- Test: `tests/index-tests.lisp`

**Interfaces:**
- Produces: the sidecar record's slot field is now a **list**; the reader accepts a bare
  symbol and normalises it, so sidecars written before this work restore untouched.

- [ ] **Step 1: Write the failing test**

```lisp
(test legacy-single-slot-sidecar-restores
  "A sidecar record carrying a bare symbol (written before #107) must
restore as a 1-list, with no rebuild."
  (is (equal '(name)
             (graph-db::%normalize-slots 'name)))
  (is (equal '(ns key)
             (graph-db::%normalize-slots '(ns key)))))
```

Then, in the existing durability test, write a graph, close it, reopen it, and assert a
single-slot `index-lookup` still resolves — the real regression gate.

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::index-suite)`
Expected: FAIL if the reader does not normalise; PASS on the pure-normaliser assertions.

- [ ] **Step 3: Write minimal implementation**

In `restore-secondary-index-roots`, wrap the slot field read:
`(let ((slots (%normalize-slots stored-slot))) …)`, and use `(length slots)` as the
arity passed to `%open-secondary-skip-list`. In `save-secondary-index-roots`, write
`(slot-index-slot-names six)` instead of the singular name.

- [ ] **Step 4: Run tests**

Run: `(asdf:test-system :graph-db)` — Expected: PASS, and the reopen path does **not**
fall back to `rebuild-secondary-indexes`.

- [ ] **Step 5: Commit**

```bash
git add index.lisp tests/index-tests.lisp
git commit -m "feat(index): sidecar carries the slot list, reader accepts a symbol (#107)"
```

---

### Task 9: `def-unique` — multi-slot uniqueness

**Files:**
- Modify: `unique-constraint.lisp` (add `def-unique` and a registry mirroring
  `*schema-index-metadata*`), `:74` (`class-unique-slots`), `:179` (`%unique-key`),
  `:189` (`%unique-index-for`), `:266` (`validate-unique-constraints`);
  `package.lisp:178` region
- Test: `tests/unique-constraint-tests.lisp`

**Interfaces:**
- Consumes: `%normalize-slots`, `%index-comp-lessp`, `%index-equal`.
- Produces: `def-unique (owner-class slots graph-name &key canonicalize scope)`;
  the `unique-index` struct gains `unique-index-slot-names` (a list) and
  `unique-index-canonicalizers` (a positional list, NIL = identity), mirroring
  `slot-index` from Task 3; and `%unique-tuple-key (uix node graph) -> list-or-nil`.
  A tuple containing **any** null component is exempt from the constraint.

- [ ] **Step 1: Write the failing test**

```lisp
(def-vertex uq-claim ()
  ((ns :initarg :ns :accessor uqc-ns)
   (ky :initarg :ky :accessor uqc-ky))
  :graph-db-unique-test)

(def-unique uq-claim (ns ky) :graph-db-unique-test)

(test multi-slot-unique-rejects-duplicate-tuple
  "The same (ns, ky) pair twice must signal at the commit boundary (#107)."
  (with-uq-graph (g)
    (with-transaction () (make-uq-claim :ns "ops" :ky "e1"))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-uq-claim :ns "ops" :ky "e1")))))

(test multi-slot-unique-exempts-null-component
  "Two tuples sharing their populated component but both null elsewhere do NOT
collide -- SQL semantics, and the unary-claim case (#107)."
  (with-uq-graph (g)
    (with-transaction () (make-uq-claim :ns "ops" :ky nil))
    (finishes (with-transaction () (make-uq-claim :ns "ops" :ky nil)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `(fiveam:run! 'graph-db/test::unique-constraint-suite)`
Expected: FAIL — `DEF-UNIQUE` is undefined.

- [ ] **Step 3: Write minimal implementation**

Add a `*schema-unique-metadata*` registry and a `unique-spec` struct mirroring
`index-spec` (Task 3), a `def-unique` macro mirroring `def-index` (Task 6) but also
carrying `:scope`, and extend `%unique-key` to build a tuple:

```lisp
(defun %unique-tuple-key (uix node graph)
  "The unique key for NODE in UIX, or NIL when ANY component is null -- a tuple
containing an unknown is exempt, SQL-style (GH #107)."
  (let ((vals (loop for s in (unique-index-slot-names uix)
                    for v = (slot-value node s)
                    when (null v) do (return-from %unique-tuple-key nil)
                    collect v)))
    (let ((k (loop for v in vals
                   for c in (unique-index-canonicalizers uix)
                   collect (if c (funcall c v) v))))
      (if (eq (unique-index-scope uix) :origin)
          (cons (%origin-token (%node-origin node graph)) k)
          k))))
```

Route `validate-unique-constraints` and the claim/release path through it. Export
`def-unique` from `package.lisp`.

- [ ] **Step 4: Run tests**

Run: `(asdf:test-system :graph-db)` — Expected: PASS.

Then the concurrency gate, mirroring the existing single-slot case: 8 threads racing the
same tuple, exactly 1 commit and 7 `unique-constraint-violation`.

- [ ] **Step 5: Commit**

```bash
git add unique-constraint.lisp package.lisp tests/unique-constraint-tests.lisp
git commit -m "feat(unique): def-unique with multi-slot keys, null-exempt (#107)"
```

---

### Task 10: Peer path, manual and CHANGELOG

**Files:**
- Test: `tests/peer-index-tests.lisp`, `tests/peer-unique-tests.lisp`
- Modify: `docs/vivace-graph-v3-doc.org`, `CHANGELOG.md`

- [ ] **Step 1: Add the peer-path regression test**

`apply-tx-writes-to-secondary-indexes` runs from three sites —
`transactions.lisp:1714`, `peer-streaming.lisp:781` and `:818`. All three call the same
function, so tuple support should be automatic; this test proves it rather than
assuming.

In `tests/peer-index-tests.lisp`, mirror the existing single-slot pull test with a
multi-slot index and assert a pulled node resolves through `index-lookup`.

- [ ] **Step 2: Run the peer suites**

Run: `(fiveam:run! 'graph-db/test::peer-index-suite)` and
`(fiveam:run! 'graph-db/test::peer-unique-suite)` — Expected: PASS.

- [ ] **Step 3: Run the two-process replication harness**

It is a shell script outside the FiveAM matrix and must be run deliberately:
`tests/replication/run-replication-test.sh`. Expected: PASS.

- [ ] **Step 4: Write the manual section and CHANGELOG entry**

Add a multi-slot subsection to the index chapter of `docs/vivace-graph-v3-doc.org`
covering: the flat key, `def-index` / `def-unique` with a slot list, positional
canonicalizers, `:prefix`, and — stated explicitly, because it will surprise people —
that a null component is **stored** in an ordinary index but **exempts** a unique tuple.

Add a CHANGELOG entry under Unreleased.

- [ ] **Step 5: Commit**

```bash
git add tests/peer-index-tests.lisp tests/peer-unique-tests.lisp \
        docs/vivace-graph-v3-doc.org CHANGELOG.md
git commit -m "docs(index): multi-slot indexes in the manual, plus peer-path coverage (#107)"
```

---

## Final gate

- [ ] Full SBCL suite green: `(asdf:test-system :graph-db)`.
- [ ] An existing single-slot graph reopens with **no rebuild** and returns identical
      results in identical order (the Task 1 / Task 8 property, re-checked end to end).
- [ ] The two-process replication harness passes.
- [ ] ECL skipped — say so explicitly in the PR.
- [ ] Every performance claim, if any is made, names its host and is the third run.
