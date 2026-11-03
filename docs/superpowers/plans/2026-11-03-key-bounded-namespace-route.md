# Key-Bounded Namespace Route Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A key-bounded page on `claim/7`'s namespace route costs its own
keys' claims at any depth, with the goal text the docs already recommend.

**Architecture:** A generic clause-body rewriter hook in the Prolog
compiler (core). The rules system registers one rewriter that fuses an
adjacent `(> ?k B)` into a new functor `claim-after/9`, whose namespace
route skips keys the comparison would reject before reading their
claims, using `>/2`'s own predicate. The comparison stays in the body.

**Tech Stack:** Common Lisp (SBCL verified, ECL/CCL arms untouched),
FiveAM, the `graph-db/rules` and `graph-db/query` systems, the perf
suite.

**Spec:** `docs/superpowers/specs/2026-11-03-key-bounded-namespace-route-design.md`

**Issue:** kraison/vivace-graph#451. Branch `key-bounded-route-451` off
`experiment`. One commit per task.

## Global Constraints

- 80 columns, spaces only, terse comments pointing at the spec or #451.
- Public repository: domain-neutral text in code, docs, commits, issue.
- `graph-db/core` knows nothing of claims: the hook is in core, the
  rewriter and the functor in `graph-db/rules`.
- "A functor's skip predicate is `>/2`'s own": `graph-db::%ordered-p`
  with `#'>` and `#'string>`. Never the index collation.
- Only `>` fuses. Only adjacent, top-level goals fuse. The comparison
  goal is kept.
- The GUI functor inventory does not list rules functors; add nothing
  there.
- Test names are symbols in a shared package per suite: grep `tests/`
  for a name before using it.
- Run suites with `--dynamic-space-size 12288`; one SBCL build of this
  checkout at a time (shared FASL cache).
- Do not run the full suite by hand; CI runs it on push.

Commands used below:

```bash
# main-suite suites, one at a time (~75 s each)
tests/run-suite.sh prolog-functor-suite
tests/run-suite.sh query-suite
# the rules suite (its own system; CI's command)
CL_SOURCE_REGISTRY="$PWD//:" sbcl --dynamic-space-size 12288 \
  --non-interactive \
  --eval '(ql:quickload :graph-db/rules-test :silent t)' \
  --eval '(asdf:test-system :graph-db/rules-test)'
```

The rules suite cannot run one test in isolation (`run-rules-tests`
binds the system directory); run the whole suite, 576 checks today.

## Review Focus

1. A bound given through a variable that is unbound at run time: no
   skip, the same rows as `claim/7` (Task 2 `an-unbound-bound-is-no-bound`,
   Task 3 end-to-end with `?b` unbound).
2. The key variable already bound at run time by an earlier goal: the
   bound-key route, the bound a filter only (Task 2
   `claim-after-filters-on-the-routes-it-cannot-skip`, Task 3
   `a-key-bound-at-run-time-takes-its-own-route`).
3. Both namespaces bound and neither key, with the bound on the object
   key: the subject route runs, the object bound filters (Task 2, same
   test as 2).
4. A rule body naming `claim-after` directly, including under `not`:
   refused, so the reads and stratification analysis keep seeing
   `claim/7` (Task 2 `claim-after-is-not-a-rule-premise`).
5. A cut, or any goal, between the claim goal and the comparison is not
   fused; a free-text query through the guard is (Task 3 rewriter unit
   tests and `the-guard-path-is-fused`).

---

### Task 1: The clause-body rewriter hook

**Files:**
- Modify: `prologc.lisp` (`compile-clause`, around line 661)
- Modify: `package.lisp` (the `graph-db` export list, near `*prolog-trace*`
  at line 577)
- Test: `tests/prolog-functor-tests.lisp`

**Interfaces:**
- Produces: `graph-db:*clause-body-rewriters*`, a list of function
  designators `(goals) -> goals`, applied in list order to
  `(clause-body clause)` inside `compile-clause`, before the head's `=`
  goals are prepended and before `compile-body`. Core registers none.

- [ ] **Step 1: Write the failing test**

In `tests/prolog-functor-tests.lisp`, in `prolog-functor-suite`:

```lisp
(test a-clause-body-rewriter-runs-before-the-body-compiles
  "GH #451: a registered rewriter sees the clause body as a goal list
and its result is what compiles.  Registered for the test only."
  (with-test-graph (g)
    (flet ((rewrite (goals)
             (substitute-if '(= ?x 42)
                            (lambda (goal)
                              (and (consp goal)
                                   (string= (symbol-name (first goal))
                                            "RW-MARKER")))
                            goals)))
      ;; SELECT compiles its clause at macroexpansion time, so the
      ;; query is EVALed under the binding, not written inline.
      (let ((graph-db:*clause-body-rewriters* (list #'rewrite)))
        (is (equal '(42) (eval '(select-flat (?x) (rw-marker ?x)))))))
    ;; Without it the marker is an unknown functor.
    (signals error (eval '(select-flat (?x) (rw-marker ?x))))))
```

Use the file's existing graph fixture macro name if it is not
`with-test-graph`; read the file's first test.

- [ ] **Step 2: Run it to see it fail**

Run: `tests/run-suite.sh prolog-functor-suite`
Expected: FAIL, `*CLAUSE-BODY-REWRITERS*` unbound or the first `is`
failing with an unknown-functor error.

- [ ] **Step 3: Implement the hook**

In `prologc.lisp`, immediately above `compile-clause`:

```lisp
(defvar *clause-body-rewriters* '()
  "Functions (GOALS) -> GOALS applied in order to a clause body before
it compiles.  A rewriter returns an equivalent body: same solutions,
same order.  Subsystems register (rules/facts.lisp, #451); core
registers none.")
```

In `compile-clause`, replace `(clause-body clause)` with the body after
`(reduce (lambda (goals f) (funcall f goals)) *clause-body-rewriters*
:initial-value (clause-body clause))`. Export the symbol from
`package.lisp` beside `*prolog-trace*`.

- [ ] **Step 4: Run the suite**

Run: `tests/run-suite.sh prolog-functor-suite`
Expected: all checks pass, count up by 2.

- [ ] **Step 5: Commit**

```bash
git add prologc.lisp package.lisp tests/prolog-functor-tests.lisp
git commit -m "prolog: *clause-body-rewriters*, a hook a subsystem rewrites clause bodies through (#451, spec §3)"
```

---

### Task 2: `claim-after/9` and the bounded namespace route

**Files:**
- Modify: `rules/facts.lisp` (`%namespace-route` at line 263, `claim/7`
  at line 288)
- Modify: `rules/compile.lisp` (`compile-rule`, after `%guard` at
  line 455)
- Test: `tests/rules/facts-tests.lisp`, `tests/rules/compile-tests.lisp`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces:
  - Prolog functor `claim-after/9`
    `(?c ?family ?sns ?skey ?rel ?ons ?okey ?skey-after ?okey-after cont)`,
    defined with `def-global-prolog-functor` in `rules/facts.lisp`, the
    symbol `CLAIM-AFTER/9` in the package of `graph-db:claim/7`.
  - `(defun %claim-goal (?c ?family ?sns ?skey ?rel ?ons ?okey
    skey-after okey-after cont))`: `claim/7`'s present body with the
    two bounds threaded through. `claim/7` calls it with `nil nil`;
    `claim-after/9` with its last two arguments.
  - `(defun %key-after-p (key bound))` → generalized boolean: `t` when
    `bound` is `nil`, else `(graph-db::%ordered-p #'> #'string> key
    bound)`. The one predicate for skip and filter.
  - `(defun %namespace-route (family role namespace rel emit &key
    after))`: as now, but a key with `(%key-after-p key after)` false is
    skipped before its lookup. `after` is already through
    `%prolog-index-bound` by the caller.
  - In `%claim-goal`'s `emit`, every claim is dropped unless
    `(%key-after-p (claim-subject-key claim) skey-after)` and
    `(%key-after-p (claim-object-key-or-nil claim) okey-after)` hold,
    on every route, before `%unify-claim`.
  - `compile-rule` refuses a body that mentions `claim-after` at any
    nesting with a reason containing `claim-after/9`.

- [ ] **Step 1: Write the failing tests in `tests/rules/facts-tests.lisp`**

Add after `an-unresolvable-namespace-without-a-key-is-still-the-empty-path`
(line ~516). All `select`s take `(:max-inferences 1000)` unless stated.

```lisp
(test claim-after-with-no-bounds-answers-claim-rows
  (with-rules-graph (g)
    (seed g)
    (is (equal '("h1" "h1" "h2" "h2")
               (select-flat (?k)
                 (claim-after ?c rt-claim "host" ?k ?r ?ons ?o nil nil))))))

(test claim-after-skips-subject-keys-at-or-below-the-bound
  (with-rules-graph (g)
    (seed g)
    (is (equal '("h2" "h2")
               (select-flat (?k)
                 (claim-after ?c rt-claim "host" ?k ?r ?ons ?o "h1" nil))))
    (is (null (select-flat (?k)
                (claim-after ?c rt-claim "host" ?k ?r ?ons ?o "h2" nil))))
    (is (= 4 (length (select-flat (?k)
                       (claim-after ?c rt-claim "host" ?k ?r ?ons ?o
                                    "" nil)))))
    ;; >/2 fails on a string against a number: nothing qualifies.
    (is (null (select-flat (?k)
                (claim-after ?c rt-claim "host" ?k ?r ?ons ?o 5 nil))))))

(test claim-after-bounds-the-object-key-on-the-object-route
  (with-rules-graph (g)
    (seed g)
    (is (equal '("web" "web")
               (select-flat (?o)
                 (claim-after ?c rt-claim ?sns ?s ?r "app" ?o nil "db"))))))

(test claim-after-filters-on-the-routes-it-cannot-skip
  "Both namespaces bound: the subject route, the object bound a filter.
Subject key bound: the bound-key route, same."
  (with-rules-graph (g)
    (seed g)
    (is (equal '(("h1" "web") ("h2" "web"))
               (sort (select (?s ?o)
                       (claim-after ?c rt-claim "host" ?s "runs" "app" ?o
                                    nil "db"))
                     #'string< :key #'first)))
    (is (equal '("web")
               (select-flat (?o)
                 (claim-after ?c rt-claim "host" "h1" "runs" "app" ?o
                              nil "db"))))
    ;; An object bound excludes a unary claim: > on NIL fails.
    (is (equal '("runs" "runs" "runs")
               (select-flat (?r)
                 (claim-after ?c rt-claim "host" ?k ?r ?ons ?o nil ""))))))

(test an-unbound-bound-is-no-bound
  (with-rules-graph (g)
    (seed g)
    (is (= 4 (length (select-flat (?k)
                       (claim-after ?c rt-claim "host" ?k ?r ?ons ?o
                                    ?after nil)))))))
```

The cost test. `seed-keys` is a new helper in `tests/rules/suite.lisp`
beside `seed`: `(defun seed-keys (g n &key (relation "up"))` makes `n`
`rt-claim` unary claims in one transaction, subject namespace `:host`,
keys `"k00"` to `"k59"` for `n` 60 (two digits, zero padded, so string
order is numeric order), producer `"p"`, standing `:observed`.

```lisp
(test a-bounded-namespace-route-costs-its-own-keys
  "60 keys, bound at k55: the route reads four keys.  Budget 30 covers
four keys' goals and not sixty.  The same bound as a separate goal
after a TRUE still walks every key and blows the budget."
  (with-rules-graph (g)
    (seed-keys g 60)
    (is (equal '("k56" "k57" "k58" "k59")
               (select (:flat t :max-inferences 30) (?k)
                 (claim-after ?c rt-claim "host" ?k "up" ?ons ?o
                              "k55" nil))))
    (signals graph-db::prolog-resource-error
      (select (:flat t :max-inferences 30) (?k)
        (claim ?c rt-claim "host" ?k "up" ?ons ?o)
        (true)
        (> ?k "k55")))))
```

- [ ] **Step 2: Write the failing test in `tests/rules/compile-tests.lisp`**

After `the-body-goes-through-the-guard`:

```lisp
(defparameter *body-claim-after*
  "(claim-after ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h nil nil)")
(defparameter *body-claim-after-under-not*
  (concatenate 'string *web-hosts-body*
               " (not (claim-after ?q rt-claim \"app\" \"web\" \"z\" \"host\" ?h nil nil))"))

(test claim-after-is-not-a-rule-premise
  "GH #451: the reads and strata analyses recognise claim/7 by name; a
rule writes claim/7 and the comparison, and the rewrite fuses them."
  (refuses "claim-after/9" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body *body-claim-after*)
  (refuses "claim-after/9" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body *body-claim-after-under-not*))
```

- [ ] **Step 3: Run the rules suite to see them fail**

Run: the rules suite command.
Expected: the six new facts tests fail with an unknown functor
`CLAIM-AFTER/9` (or a guard refusal); `claim-after-is-not-a-rule-premise`
fails because the compile is refused for another reason or not at all.

- [ ] **Step 4: Implement**

In `rules/facts.lisp`: `%key-after-p`; `%namespace-route`'s `&key after`
with the skip at the top of the `dolist`; extract `claim/7`'s body into
`%claim-goal` with the filter in `emit`; redefine `claim/7` as the call
with `nil nil`; define `claim-after/9` with a docstring stating the
contract from spec §1 and that `compile-rule` refuses it in a body;
add to `claim/7`'s docstring one sentence: "A `>` on the key right
after this goal compiles to `claim-after/9`, which starts the namespace
route at the bound (#451)." The object key of a unary claim is `nil`;
read it as `%unify-claim` does (line ~210).

In `rules/compile.lisp`: `(defun %mentions-claim-after-p (goals))`, a
cons walk testing `(%engine-goal-p goal "CLAIM-AFTER" 9)` at every
level; in `compile-rule`, right after the `%guard` binding, refuse with
`"claim-after/9 is not a rule premise: write claim/7 and the comparison, the compiler fuses them (#451)"`.

- [ ] **Step 5: Run the rules suite**

Run: the rules suite command.
Expected: all pass; count 576 plus the new checks.

- [ ] **Step 6: Mutation**

Comment out the skip in `%namespace-route` (keep the filter). Run the
rules suite. Expected: `a-bounded-namespace-route-costs-its-own-keys`
fails on its first `is` with `prolog-resource-error`; everything else
passes. Restore the skip, rerun, all green. This proves the cost test
sees the skip and not the filter.

- [ ] **Step 7: Commit**

```bash
git add rules/facts.lisp rules/compile.lisp tests/rules/facts-tests.lisp tests/rules/compile-tests.lisp tests/rules/suite.lisp
git commit -m "rules: claim-after/9, a claim/7 whose namespace route starts after a key bound (#451, spec §1)"
```

---

### Task 3: The rewrite

**Files:**
- Modify: `rules/facts.lisp` (after `claim-after/9`)
- Test: `tests/rules/facts-tests.lisp`

**Interfaces:**
- Consumes: `graph-db:*clause-body-rewriters*` (Task 1);
  `claim-after/9` (Task 2).
- Produces: `(defun %fuse-key-bounds (goals))` → goals, exported from
  nothing (tests reach it as `graph-db.rules::%fuse-key-bounds`);
  `(defun %engine-functor-goal-p (goal name arity))`, the recognition
  rule of spec §2: a symbol head with `(string= (symbol-name head) name)`
  and `arity` arguments, whose package is `(symbol-package
  'graph-db:claim/7)` or holds no symbol `NAME/ARITY` registered in
  `graph-db::*prolog-global-functors*` or `graph-db::*user-functors*`.
  Use `find-symbol`, never `make-functor-symbol` (it interns).

- [ ] **Step 1: Write the failing unit tests**

In `tests/rules/facts-tests.lisp`. `claim` and `>` here are whatever
symbols the test package reads them as; that is the point.

```lisp
(defun fused (goals) (graph-db.rules::%fuse-key-bounds goals))

(test the-rewrite-fuses-an-adjacent-key-bound
  (let* ((body '((claim ?c rt-claim "host" ?k ?r ?ons ?o) (> ?k "h1")))
         (out (fused body)))
    (is (= 2 (length out)))
    (is (string= "CLAIM-AFTER" (symbol-name (first (first out)))))
    (is (equal '(?c rt-claim "host" ?k ?r ?ons ?o "h1" nil)
               (rest (first out))))
    (is (equal '(> ?k "h1") (second out))))
  ;; The object key, and a bound through a variable.
  (is (equal '(?c rt-claim ?sns ?s ?r "app" ?o nil ?b)
             (rest (first (fused '((claim ?c rt-claim ?sns ?s ?r "app" ?o)
                                   (> ?o ?b)))))))
  ;; Both keys, two comparisons in a row.
  (is (equal '(?c rt-claim "host" ?s ?r "app" ?o "h1" "db")
             (rest (first (fused '((claim ?c rt-claim "host" ?s ?r "app" ?o)
                                   (> ?s "h1") (> ?o "db"))))))))

(test the-rewrite-leaves-every-other-shape-alone
  (flet ((same (body) (is (eq body (fused body)))))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o) (true) (> ?k "h1")))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o) ! (> ?k "h1")))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o) (>= ?k "h1")))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o) (< ?k "h1")))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o) (> ?k 5)))
    (same '((claim ?c rt-claim "host" "h1" ?r ?ons ?o) (> ?o "db")))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o) (> ?z "h1")))
    (same '((claim ?c rt-claim "host" ?k ?r ?ons ?o)))
    (same '((claim-current ?c) (> ?k "h1")))
    (same '())))
```

- [ ] **Step 2: Write the failing end-to-end tests**

```lisp
(test the-recommended-paging-shape-costs-its-own-keys
  "The goal text docs/guarded-query.md recommends, under the budget
Task 2's cost test fixed: it answers only if the rewrite fused it."
  (with-rules-graph (g)
    (seed-keys g 60)
    (is (equal '("k56" "k57" "k58" "k59")
               (select (:flat t :max-inferences 30) (?k)
                 (claim ?c rt-claim "host" ?k "up" ?ons ?o)
                 (> ?k "k55"))))))

(test the-guard-path-is-fused
  (with-rules-graph (g)
    (seed-keys g 60)
    (let ((graph-db::*query-default-max-inferences* 30))
      (multiple-value-bind (columns rows)
          (graph-db.query:run-guarded-prolog
           "(claim ?c rt-claim \"host\" ?k \"up\" ?ons ?o) (> ?k \"k55\")"
           g)
        (declare (ignore columns))
        (is (= 4 (length rows)))))))

(test a-key-bound-at-run-time-takes-its-own-route
  "Syntactically a variable, so it fuses; bound by = before the goal,
so the bound-key route runs and the bound only filters."
  (with-rules-graph (g)
    (seed-keys g 60)
    (is (equal '("k57")
               (select (:flat t :max-inferences 1000) (?k)
                 (= ?k "k57")
                 (claim ?c rt-claim "host" ?k "up" ?ons ?o)
                 (> ?k "k55"))))
    (is (null (select (:flat t :max-inferences 1000) (?k)
                (= ?k "k50")
                (claim ?c rt-claim "host" ?k "up" ?ons ?o)
                (> ?k "k55"))))))

(test an-unbound-bound-variable-answers-as-before
  (with-rules-graph (g)
    (seed g)
    (is (null (select (:flat t :max-inferences 1000) (?k)
                (claim ?c rt-claim "host" ?k ?r ?ons ?o)
                (> ?k ?b))))))
```

- [ ] **Step 3: Run the rules suite to see them fail**

Expected: the unit tests fail with `%FUSE-KEY-BOUNDS` undefined;
`the-recommended-paging-shape-costs-its-own-keys` and
`the-guard-path-is-fused` fail with `prolog-resource-error`; the other
two pass (they pin behaviour that must not change; note that in the
ledger).

- [ ] **Step 4: Implement**

In `rules/facts.lisp`: `%engine-functor-goal-p`, `%fuse-key-bounds`
(one left-to-right pass; when a `claim/7` goal is followed by one or two
qualifying comparisons, emit the fused goal then the comparisons
unchanged; return the input list `eq` when nothing fused), and at top
level:

```lisp
(pushnew '%fuse-key-bounds graph-db:*clause-body-rewriters*)
```

The fused head symbol is `(intern "CLAIM-AFTER" (symbol-package
'graph-db:claim/7))`. A qualifying comparison: `(%engine-functor-goal-p
g ">" 2)`, its first argument `eq` the key variable, its second a
string or `(graph-db::variable-p …)`.

- [ ] **Step 5: Run the rules suite, then the query suite**

Run: the rules suite command. Expected: all pass.
Run: `tests/run-suite.sh query-suite`. Expected: all pass (the guard's
own tests do not load rules; this checks the hook under the guard with
an empty rewriter list).

- [ ] **Step 6: Commit**

```bash
git add rules/facts.lisp tests/rules/facts-tests.lisp
git commit -m "rules: a > on the key right after claim/7 compiles to claim-after/9 (#451, spec §2-3)"
```

---

### Task 4: The paging bench

**Files:**
- Create: `tests/perf/rules-bench.lisp`
- Modify: `graph-db.asd` (`graph-db/perf-test`, line 417: add
  `:graph-db/rules` to `:depends-on`, and `(:file "rules-bench")` after
  `"spacetime-bench"`)
- Modify: `tests/perf/benchmarks.lisp` (`run-perf`, line ~1018: call
  `(bench-claim-key-paging)` after `(bench-claim-linking)`)
- Modify: `tests/perf/README.md` ("What lives here")

**Interfaces:**
- Consumes: `claim/7` and the rewrite (Tasks 2–3) through
  `graph-db.query:run-guarded-prolog`.
- Produces: `(defun bench-claim-key-paging ())` recording
  `key-page-first-500`, `key-page-last-500` (seconds each, via
  `timed-seconds`) and `key-page-depth-ratio` (`:ratio`, last over
  first, `float-3`).

- [ ] **Step 1: Write the bench**

Own graph name `:graph-db-perf-paging`, own source and claim family
(`def-source pp-thing`, `def-claim-classes pp-claim`), as
`spacetime-bench.lisp` does, so no existing bench's work changes (no
generation bump). N = `(scale 20000)` unary claims, subject namespace
`:pp-things`, keys zero padded to five digits, one relation `"rel"`,
inserted in batches of 500. Text for a page, with `BOUND` the last key
of the previous page, or no comparison for the first:

```
(claim ?c pp-claim "pp-things" ?k "rel" ?ons ?o) (> ?k "BOUND")
```

Run each timed page through `run-guarded-prolog … :limit 500` with
`*query-default-max-inferences*` and `*query-default-timeout*` bound
high enough (1,000,000 and 60). The last page's bound is the key at
index N − 500 − 1. Time the first page and the last page once each
after one untimed warm-up page.

- [ ] **Step 2: Load and run it at small scale**

```lisp
(ql:quickload :graph-db/perf-test)
(let ((graph-db/perf-test::*perf-scale* :small))
  (graph-db/perf-test::bench-claim-key-paging))
```

Expected: loads without warnings; three records; the ratio under 3.
Record the three numbers for the issue comment.

- [ ] **Step 3: The checker's own tests still pass**

Run: `tests/run-suite.sh perf-check-suite` (find the suite's name in
`tests/perf/check-tests.lisp`; it runs in the main suite).
Expected: pass.

- [ ] **Step 4: Commit**

```bash
git add tests/perf/rules-bench.lisp graph-db.asd tests/perf/benchmarks.lisp tests/perf/README.md
git commit -m "perf: key-bounded paging through the guard, first page against last (#451)"
```

---

### Task 5: Documentation

**Files:**
- Modify: `docs/rules.md` ("Solution order", line 284)
- Modify: `docs/guarded-query.md` ("Paging", line 100, the "page by key"
  bullet)
- Modify: `CHANGELOG.md` (`[Unreleased]`)

- [ ] **Step 1: `docs/rules.md`**

Replace the sentence from "A caller who wants a boundary a write cannot
move" to "discarding them." with: the key bound still restarts after
the key; when the `>` goal is the very next goal after the `claim/7`
goal it compiles to `claim-after/9`, whose namespace route skips the
earlier keys before reading their claims, so a page costs one
comparison per earlier key plus its own keys' claims (#451). Then one
sentence each: the shapes that are not fused (`>=`, `<`, a numeric
bound, any goal or cut between, a comparison inside `or`/`not`/`if`),
`claim-after/9`'s full contract and that it may be written by hand, and
that a rule body writes `claim/7` and the comparison. One more: a
`select` is rewritten when its file compiles, so a file compiled before
`graph-db/rules` loaded is not fused until recompiled; a guarded query
is compiled per request and always is.

- [ ] **Step 2: `docs/guarded-query.md`**

In the "page by key" bullet replace "whose cost is the discarded keys'
claims rather than every earlier solution" with the new cost and its
condition (the comparison immediately after the indexed goal), pointing
at `docs/rules.md` "Solution order" for the detail.

- [ ] **Step 3: `CHANGELOG.md`**

Under `[Unreleased]`, add `### Added` (or extend it): `claim-after/9`
and the rewrite, one paragraph, with the condition and the non-fused
shapes; `*clause-body-rewriters*` as the engine hook. Under `### Changed`
one line: the cost of paging by key on a namespace route (#451).

- [ ] **Step 4: Check**

`grep -n 'claim-after' docs/rules.md docs/guarded-query.md CHANGELOG.md`
shows all three. Reread each changed paragraph once for a claim the
code does not make.

- [ ] **Step 5: Commit**

```bash
git add docs/rules.md docs/guarded-query.md CHANGELOG.md
git commit -m "docs: paging by key starts the namespace route at the bound; claim-after/9 (#451)"
```

---

## After the last task

Comment on #451 with the per-task SHAs, the bench's three numbers, and
`Ready for review:` with `git diff experiment...key-bounded-route-451`.
The issue closes by hand with the landing SHA once the branch is on
`experiment` with CI green.
