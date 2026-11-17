# Clause Indexing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `<-` compiles one clause, and a call with a bound first
argument reaches only the clauses that can match (GH #121, unit 2 of
the Prolog Engine 5.0 scope).

**Architecture:** Each clause compiles into its own closure returning
true when its body executed a cut; a per-functor dispatcher built once
walks the candidate clauses in source order, undoing bindings between
them, and stops at a cut. The closures, a first-argument index and the
variable-headed positions live in one immutable `clause-set` the
functor publishes in a single slot write per assert. A compile-time
special names the clause's cut flag; the barrier macros clear it, as
they already rebind the block name.

**Tech Stack:** Common Lisp (SBCL primary; the code is plain CL, no new
per-implementation arm beyond what the spec names), FiveAM.

**Spec:** `docs/superpowers/specs/2026-11-09-clause-indexing-design.md`
(D1 to D7 binding).

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments pointing at the
  spec section or GH #121 (`~/.claude/CLAUDE.md`).
- Engine core: written inline in this session, not by a worker
  (`inline-when-kevin-says-write-it-yourself`).
- A published `clause-set` is never mutated: every assert builds a new
  one. That is what makes lock-free reads safe on every implementation,
  ECL included (its tables are unsafe only under concurrent writes), so
  no `#+ecl` lock is needed in the dispatcher; spec D5's ECL sentence
  is tightened to say so in Task 2.
- Behaviour of the top-level query builders (`?-`, `select`,
  `map-query`, `do-query`) is unchanged: they call `compile-clause`
  without `:cut-flag`, and their cut keeps returning from their block.
- Suites, one SBCL at a time in this checkout, never the full suite:
  - the new file's suite and its neighbours:
    `tests/run-suite.sh clause-indexing-suite`,
    `tests/run-suite.sh prolog-functor-suite`,
    `tests/run-suite.sh prolog-stress-suite`
  - `tests/run-system-tests.sh graph-db/concurrency-test` (the
    functor tests are in it)
  - `tests/run-system-tests.sh graph-db/rules-test` once, at the end
    (rules compile bodies through the same compiler; baseline 1129)
- FiveAM names are symbols in `graph-db/test`: every new test, helper
  and functor name in this plan is prefixed `ci-`; grep `tests/` for
  `ci-` before adding another.
- Asserted functors are image-global: every test drops what it
  asserts, before and after, as `defining-reach` does
  (`tests/prolog-stress-tests.lisp:53`).
- Docs travel with code: CHANGELOG `## [Unreleased]` in Tasks 2 and 3,
  the manual and `docs/codebase-notes.md` in Task 4.
- Public repo: domain-neutral text.

## Review Focus

1. A cut in a clause reached through a *dynamic* meta-call
   (`(call ?g)` with `?g` bound to a `ci-f` goal): the clause's own cut
   must still commit the clause (the closure's flag), while the
   meta-call itself stays a barrier. Test in Task 2.
2. A clause whose first head argument is `?` (anonymous): it must land
   in the variable positions, not under a key named `?`. Test in Task 3.
3. A call whose first argument derefs to a *bound* variable chain
   (`(= ?k a) (ci-k ?k ?r)`): the key must come from the dereferenced
   value. Test in Task 3.
4. `reset-functor` while another thread calls the functor: the reader
   holds the old set or the new empty one, never a torn pair. Covered
   by the immutability rule; the concurrency suite's mixed test is the
   regression net.
5. A `make-functor :clauses` list with one clause of another arity:
   `prolog-error`, and the functor is left registered with no clauses
   rather than half-built. Test in Task 2.

---

### Task 1: The cut flag, behaviour-neutral

**Files:**
- Modify: `globals.lisp:416` (after `*functor*`)
- Modify: `prologc.lisp:532-540` (`%commit`), `:542-568` (`not`),
  `:570-583` (`once`), `:585-619` (`if`), `:672-685` (`compile-clause`),
  `:765-777` (`compile-body`'s cut arm)

**Interfaces:**
- Produces: `*clause-cut-flag*` (nil by default); `compile-clause
  (parms clause cont &key cut-flag)`, which binds `*clause-cut-flag*`
  to `cut-flag` around its `compile-body`; `(%commit BARRIER OUTER
  OUTER-FLAG)` as the goal the `once` and `if` macros build.

- [ ] **Step 1:** In `globals.lisp` after `*functor*`:

```lisp
(defvar *clause-cut-flag* nil
  "While a clause body compiles into its own closure: the symbol of
that closure's CUT variable, set by a cut before it returns from the
clause block.  NIL at top level and under a barrier (spec D3, #121).")
```

- [ ] **Step 2:** `compile-body`'s cut arm becomes

```lisp
`(progn ,(compile-body (rest body) cont bindings)
        ,@(when *clause-cut-flag* `((setf ,*clause-cut-flag* t)))
        (return-from ,(or *functor* :prolog-select) nil))
```

- [ ] **Step 3:** The barriers. In `not`, bind `*clause-cut-flag*` nil
  in the same `let` that binds `*functor*` to `barrier` for `g-code`.
  In `once` and `if`, capture `(outer-flag *clause-cut-flag*)` beside
  `outer`, bind `*clause-cut-flag*` nil where `*functor*` is bound to
  `barrier`, and build the goal as `(list '%commit barrier outer
  outer-flag)`. In `%commit`, destructure `(barrier outer outer-flag)`
  and compile the rest under both `(let ((*functor* outer)
  (*clause-cut-flag* outer-flag)) ...)`.

- [ ] **Step 4:** `compile-clause` gains `&key cut-flag` and wraps its
  `compile-body` call in `(let ((*clause-cut-flag* cut-flag)) ...)`.
  No caller passes it yet.

- [ ] **Step 5: Verify neutrality.** Run `prolog-functor-suite` and
  `prolog-stress-suite`: both at their baseline counts (record them),
  all pass.

- [ ] **Step 6: Commit.**

```bash
git add globals.lisp prologc.lisp
git commit -m "prolog: a compile-time cut flag the barriers clear, unused yet (#121)"
```

### Task 2: One closure per clause, and the dispatcher

**Files:**
- Modify: `functor.lisp` (struct, `make-functor`, `add-functor-clause`,
  `reset-functor`)
- Modify: `prologc.lisp:168-187` (`prolog-compile-help`,
  `prolog-compile` method, `clauses-with-arity`: replaced), `:744-763`
  (`prolog-compile-null`, `compile-functor`: replaced), new functions
  beside them
- Modify: `globals.lisp` (`*clause-compile-count*`)
- Create: `tests/clause-indexing-tests.lisp`
- Modify: `graph-db.asd:722` (add `(:file "clause-indexing-tests")`
  after `prolog-functor-tests`)
- Modify: `docs/superpowers/specs/2026-11-09-clause-indexing-design.md`
  D5 (the ECL sentence), `CHANGELOG.md`

**Interfaces:**
- Produces, in `graph-db`:
  - `(defstruct (clause-set (:constructor %make-clause-set)) fns index vars)`:
    `fns` a simple-vector of clause closures `(p1 ... pN cont) =>
    cut-p`; `index` a hash table (`:test 'equal`) key => simple-vector
    of fixnum positions, ascending; `vars` a simple-vector of fixnum
    positions, ascending. Task 2 fills `index` empty and `vars` with
    every position; Task 3 fills them properly.
  - `functor` struct slots added: `arity` (nil until the first clause),
    `dispatcher` (nil until built), `clause-set` (a `clause-set`, an
    empty one at creation).
  - `(%compile-clause-fn functor clause) => closure`: `compile-clause`
    on `(make-parameters arity)` with `:cut-flag` a fresh symbol and
    `*functor*` bound to a fresh block symbol, wrapped as spec D1,
    `eval`ed; increments `*clause-compile-count*`.
  - `(%build-clause-set functor clauses fns) => clause-set`: Task 2's
    version puts every position in `vars`.
  - `(%make-dispatcher functor arity) => function` of
    `(p1 ... pN cont)`: spec D2, iterating
    `(%map-candidates fn set first-arg)` where Task 2's
    `%map-candidates` walks `vars` only (every position).
  - `*clause-compile-count*` (globals.lisp, a fixnum, incremented per
    clause compile; read by tests as `graph-db::`).
- `prolog-compile` (the generic) keeps its name: on a functor with no
  clauses installs the null function; with clauses (the
  `make-functor :clauses` path) compiles each, builds the set and the
  dispatcher. `add-functor-clause` compiles the one clause, builds a
  new set from the old plus it, stores it, and installs the dispatcher
  when `(functor-fn f)` is not `eq` to `(functor-dispatcher f)`.
  `reset-functor` stores an empty set, `arity` nil, and installs the
  null function. `functor-clauses` stays the ordered list.

- [ ] **Step 1: Write the failing tests** in the new file, header and
  suite as `tests/prolog-functor-tests.lisp` has them, suite
  `clause-indexing-suite :in graph-db-suite`, a helper
  `(ci-drop name arity)` that is `drop-rule`'s body under its own name,
  and these tests (each wrapped in `with-test-graph` and dropping its
  functors before and after; follow `defining-reach` for the `<-` then
  `select` idiom):

```lisp
(test ci-an-assert-compiles-one-clause
  (ci-drop 'ci-fact 2)
  (let ((before graph-db::*clause-compile-count*))
    (<- (ci-fact a 1)) (<- (ci-fact b 2)) (<- (ci-fact c 3))
    (is (= 3 (- graph-db::*clause-compile-count* before))))
  (is (equal '(((ci-fact a 1)) ((ci-fact b 2)) ((ci-fact c 3)))
             (functor-clauses (lookup-functor (make-functor-symbol 'ci-fact 2)))))
  (ci-drop 'ci-fact 2))

(test ci-a-cut-in-an-asserted-clause-commits-the-clause
  ;; ci-g: three facts.  ci-h: no cut, four solutions.  ci-f: cut,
  ;; one.  ci-o: cut under once, opaque, so clause two is reached.
  ;; ci-i: cut in an if's then branch cuts the clause.  ci-i2: cut
  ;; inside the if's test is under the barrier, clause two reached.
  (<- (ci-g 1)) (<- (ci-g 2)) (<- (ci-g 3))
  (<- (ci-h ?x) (ci-g ?x))               (<- (ci-h z))
  (<- (ci-f ?x) (ci-g ?x) !)             (<- (ci-f z))
  (<- (ci-o ?x) (once (and (ci-g ?x) !))) (<- (ci-o z))
  (<- (ci-i ?x) (if (ci-g ?x) (and (= ?x ?x) !) (= ?x none))) (<- (ci-i z))
  (<- (ci-i2 ?x) (if (and (ci-g ?x) !) (= ?x ?x)))           (<- (ci-i2 z))
  (is (equal '(1 2 3 z) (select-flat (?x) (ci-h ?x))))
  (is (equal '(1)       (select-flat (?x) (ci-f ?x))))
  (is (equal '(1 z)     (select-flat (?x) (ci-o ?x))))
  (is (equal '(1)       (select-flat (?x) (ci-i ?x))))
  (is (equal '(1 z)     (select-flat (?x) (ci-i2 ?x))))
  ;; Review focus 1: through a dynamic meta-call the clause's cut
  ;; still commits the clause.
  (is (equal '(1) (select-flat (?x) (= ?g (ci-f ?x)) (call ?g)))))

(test ci-an-assert-after-set-functor-fn-wins
  (<- (ci-s a))
  (set-functor-fn (make-functor-symbol 'ci-s 1)
                  (lambda (x cont) (declare (ignore x cont)) nil))
  (is (null (select-flat (?x) (ci-s ?x))))
  (<- (ci-s b))
  (is (equal '(a b) (select-flat (?x) (ci-s ?x)))))

(test ci-reset-functor-empties-and-an-arity-mismatch-signals
  (<- (ci-r a)) (<- (ci-r b))
  (let ((f (lookup-functor (make-functor-symbol 'ci-r 1))))
    (signals prolog-error (add-functor-clause f '((ci-r a b))))
    (is (= 2 (length (functor-clauses f))))
    (reset-functor f)
    (is (null (functor-clauses f)))
    (is (null (select-flat (?x) (ci-r ?x))))
    (<- (ci-r c))
    (is (equal '(c) (select-flat (?x) (ci-r ?x)))))
  ;; Review focus 5: a mixed :CLAUSES list signals and leaves no
  ;; half-built functor.
  (signals prolog-error
    (make-functor :name (make-functor-symbol 'ci-m 1 :define t)
                  :clauses '(((ci-m a)) ((ci-m a b)))))
  (is (null (functor-clauses (lookup-functor (make-functor-symbol 'ci-m 1))))))
```

  Add the file to `graph-db.asd` after `prolog-functor-tests`.

- [ ] **Step 2:** `tests/run-suite.sh clause-indexing-suite`. Expected:
  the compile-count test fails (`*CLAUSE-COMPILE-COUNT*` unbound), the
  rest may pass or fail; record.

- [ ] **Step 3: `functor.lisp`.** The `clause-set` struct, the new
  `functor` slots (default `clause-set` `(%make-clause-set :fns #()
  :index (make-hash-table :test 'equal) :vars #())`), and:
  `make-functor` unchanged in shape (it still calls `prolog-compile`
  under the lock); `add-functor-clause` appends the clause under the
  lock, then `(%add-compiled-clause functor clause)` (prologc.lisp,
  below), returning `functor-clauses`; `reset-functor` sets clauses
  nil, `arity` nil, an empty set, and the null function via
  `prolog-compile`.

- [ ] **Step 4: `prologc.lisp`.** Delete `prolog-compile-help`,
  `clauses-with-arity` and `compile-functor`. New, in their place:

```lisp
(defun %clause-arity (functor clause) ...)   ; sets/validates ARITY, signals PROLOG-ERROR
(defun %compile-clause-fn (functor clause) ...)
(defun %build-clause-set (functor clauses fns) ...)
(defun %make-dispatcher (functor arity) ...)
(defun %add-compiled-clause (functor clause) ...)  ; compile, new set, install dispatcher if needed
(defmethod prolog-compile ((functor functor)) ...)  ; nil clauses: null fn; else every clause through the above
```

  `%make-dispatcher`'s template, the arity-generic form (the spec
  leaves a family-of-arities variant to the builder; start generic,
  measure in Task 4):

```lisp
(funcall (eval `(lambda (functor)
                  (lambda (,@params cont)
                    (let ((set (functor-clause-set functor))
                          (old-trail (fill-pointer *trail*))
                          (first t))
                      (%map-candidates
                       (lambda (fn)
                         (if first (setf first nil) (undo-bindings old-trail))
                         (funcall fn ,@params cont))
                       set ,(if (plusp arity) (first params) nil))))))
         functor)
```

  `(%map-candidates visit set first-arg)` in Task 2 calls `visit` on
  `(svref fns pos)` for each position of `vars` in order and returns as
  soon as `visit` returns true. `prolog-compile-null`'s null function
  stays for the clause-less case. `%compile-clause-fn` binds
  `*functor*` to a fresh block symbol (a `prolog-gensym "CLAUSE"`) and
  passes `:cut-flag` a fresh symbol; the wrapper is spec D1's lambda.

- [ ] **Step 5:** `tests/run-suite.sh clause-indexing-suite`: all pass.
  Then `prolog-functor-suite`, `prolog-stress-suite` at their Task 1
  counts, and `graph-db/concurrency-test` green.

- [ ] **Step 6: Mutation check.** Remove the `(setf ,*clause-cut-flag*
  t)` emission from `compile-body`, run `clause-indexing-suite`:
  `ci-a-cut-in-an-asserted-clause-commits-the-clause` must fail on the
  `ci-f` and `ci-i` assertions. Restore it, rerun green. Delete the
  stale FASL if the check count moves (CLAUDE.md, "One FASL cache").

- [ ] **Step 7: Docs.** Spec D5: replace the ECL sentence with "a
  published set is never mutated, so no implementation needs a read
  lock". CHANGELOG `### Changed`: **`<-` compiles the clause it adds,
  not the functor** (#121): one closure per clause, a dispatcher built
  once; the cut, the barriers and the top-level queries behave as
  before; `*clause-compile-count*` counts clause compiles.

- [ ] **Step 8: Commit.**

```bash
git add functor.lisp prologc.lisp globals.lisp tests/clause-indexing-tests.lisp graph-db.asd docs/superpowers/specs/2026-11-09-clause-indexing-design.md CHANGELOG.md
git commit -m "prolog: one closure per asserted clause, a dispatcher built once (#121)"
```

### Task 3: The first-argument index

**Files:**
- Modify: `prologc.lisp` (`%build-clause-set`, `%map-candidates`, new
  `%clause-index-key`, `%head-index-key`)
- Modify: `globals.lisp` (`*clause-indexing*`), `package.lisp:547`
  (export `#:*clause-indexing*`)
- Modify: `tests/clause-indexing-tests.lisp`, `CHANGELOG.md`

**Interfaces:**
- Produces: `(%clause-index-key x) => (values key all-p)`: spec D4's
  table over `(var-deref x)`; `all-p` true for an unbound variable, a
  non-string array, and a float `rational` refuses (`handler-case`).
  `(%head-index-key arg) => (values key var-p)`: for a clause head's
  first argument; `var-p` for a `variable-p` symbol, `?`, an array, or
  a `:opaque` key. `%build-clause-set` fills `index` and `vars` from
  the heads; `%map-candidates` merges `index[key]` and `vars` by
  position, `vars` alone for `:opaque`, every position when `all-p` or
  `*clause-indexing*` is nil or arity is 0. `*clause-indexing*`
  (`defvar`, `t`, exported).

- [ ] **Step 1: Write the failing tests** in the same file:

```lisp
(defparameter *ci-key* nil)

(defun ci-with-key (key)
  "Solutions of ci-k for first argument KEY, through a (lisp ...) goal
so one compiled query serves every key."
  (let ((*ci-key* key))
    (select-flat (?r) (lisp ?k *ci-key*) (ci-k ?k ?r))))

(test ci-the-index-is-a-superset-of-prolog-equal
  (<- (ci-k a 1)) (<- (ci-k b 2)) (<- (ci-k "s" 3)) (<- (ci-k :s 4))
  (<- (ci-k 1 5)) (<- (ci-k 1.0 6)) (<- (ci-k #\c 7)) (<- (ci-k (x . y) 8))
  (<- (ci-k nil 9)) (<- (ci-k ?v 10)) (<- (ci-k a 11)) (<- (ci-k ? 12))
  (with-test-graph (g)
    (let ((node (with-transaction () (make-g-person :name "n"))))
      (loop for (key expected)
              in `((a (1 10 11 12)) (b (2 10 12)) ("s" (3 4 10 12))
                   ("S" (4 10 12)) (:s (3 4 10 12)) (1 (5 6 10 12))
                   (1.0 (5 6 10 12)) (#\c (7 10 12)) ((x . y) (8 10 12))
                   ((p q) (10 12)) (nil (9 10 12)) (,node (10 12))
                   (#(#\s) (3 10 12))
                   #+sbcl (,sb-ext:double-float-positive-infinity (10 12)))
            do (is (equal expected (ci-with-key key)) "key ~S" key)
               (is (equal expected
                          (let ((*clause-indexing* nil)) (ci-with-key key)))
                   "key ~S, index off" key)))))

(test ci-a-bound-chain-keys-by-its-value
  ;; Review focus 3.
  (<- (ci-k2 a 1)) (<- (ci-k2 b 2))
  (is (equal '(1) (select-flat (?r) (= ?k ?j) (= ?j a) (ci-k2 ?k ?r)))))

(test ci-solutions-keep-source-order-under-a-key
  (<- (ci-k3 a 1)) (<- (ci-k3 ?v 2)) (<- (ci-k3 a 3)) (<- (ci-k3 ?w 4))
  (is (equal '(1 2 3 4) (select-flat (?r) (ci-k3 a ?r))))
  (is (equal '(2 4) (select-flat (?r) (ci-k3 zz ?r)))))
```

  (`make-g-person` and `with-test-graph` are the main suite's fixtures,
  `tests/graph-tests.lisp`; check the constructor's name there.)

- [ ] **Step 2:** `tests/run-suite.sh clause-indexing-suite`. Expected:
  the three new tests fail (`*CLAUSE-INDEXING*` unbound).

- [ ] **Step 3: Implement** the two key functions and the real
  `%build-clause-set` and `%map-candidates`. Keys: a non-keyword symbol
  is itself; string or keyword `(cons :text (string-upcase name))`;
  number `(cons :number (rational x))`; character itself; cons `:cons`;
  else `:opaque`. The merge: two indices over the two ascending
  vectors, emit the smaller position each step. Bucket and `vars`
  vectors are built sorted because positions are assigned in assert
  order and each new set appends.

- [ ] **Step 4:** `clause-indexing-suite` all pass; `prolog-functor-suite`,
  `prolog-stress-suite` at their counts; `graph-db/concurrency-test`
  green.

- [ ] **Step 5: Docs.** CHANGELOG `### Added`: **First-argument clause
  indexing** (#121): a call with a bound first argument tries only the
  clauses whose first head argument can unify with it, keyed as a
  superset of `prolog-equal` (1 and 1.0 share a key, a string and a
  keyword share one); `*clause-indexing*` nil turns it off, for a
  `prolog-equal` specialisation that widens equality among symbols,
  strings, numbers and characters.

- [ ] **Step 6: Commit.**

```bash
git add prologc.lisp globals.lisp package.lisp tests/clause-indexing-tests.lisp CHANGELOG.md
git commit -m "prolog: first-argument indexing of asserted clauses (#121)"
```

### Task 4: Measurement and the manual

**Files:**
- Modify: `tests/perf/prolog-bench.lisp` (`bench-clause-assert-10k`,
  added to `run-prolog-bench`'s list and `run-perf`'s), `tests/perf/run-bench.sh`
  (the new name in its case list)
- Modify: `docs/vivace-graph-v3-doc.org` (Chapter 6, a subsection
  "Asserted clauses: one closure each, first-argument indexing"),
  `docs/codebase-notes.md` (one line in the access-cost list)

- [ ] **Step 1:** `bench-clause-assert-10k`: `(scale 10000)` asserts of
  `ci`-free facts (`pbp-fact10k`), then `(scale 20000)` bound lookups,
  as `bench-clause-assert` does; reset the functor first the same way.
- [ ] **Step 2:** `tests/perf/run-bench.sh run-prolog-bench` green at
  small scale; then three runs at `:normal` on odm with nothing else
  running (the `~/.cache/vg-runs/prolog-before.sh` recipe), the third
  run's `clause-assert`, `clause-bound-first-arg` and
  `clause-assert-10k` rows on #121 beside the before rows, and the
  report committed as `tests/perf/results/prolog-after-u2-odm-<commit>.report`.
  If `clause-assert-10k` exceeds a minute at `:normal`, drop it from
  `run-prolog-bench`'s default list, keep it runnable by name, and say
  so on #121.
- [ ] **Step 3:** The dispatcher's call overhead: compare
  `clause-bound-first-arg` and `k-hop-3-select` after against before;
  if the generic `%map-candidates` closure costs more than 10% on
  `clause-bound-first-arg`, inline the loop into the eval'd template
  per arity and remeasure (spec, "Left to the builder").
- [ ] **Step 4: Docs.** Manual Chapter 6: how `<-` compiles now, the
  index's keys in one table, the `*clause-indexing*` switch, the cut's
  scope rules unchanged. `docs/codebase-notes.md`: asserted-clause call
  cost is the candidate count, not the clause count.
- [ ] **Step 5:** `graph-db/rules-test` once (baseline 1129).
- [ ] **Step 6: Commit.**

```bash
git add tests/perf/prolog-bench.lisp tests/perf/run-bench.sh tests/perf/results/prolog-after-u2-odm-*.report docs/vivace-graph-v3-doc.org docs/codebase-notes.md
git commit -m "prolog: clause indexing measured; manual and notes (#121)"
```

### Landing

- Whole-branch review, PR into `experiment`, post-merge run read, then
  #121 closed with the landing SHA and the before/after table.
