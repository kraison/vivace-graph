# Tracer and Explain Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A 4-port trace hook that costs an untraced call nothing new,
`trace-prolog` over it, and `explain-goals` / `profile-goals` with
guarded text entries (GH #124, unit 3 of the Prolog Engine 5.0 scope).

**Architecture:** `compile-call` and `%solve` route a goal call through
`%ported-call` when `*prolog-port-hook*` is set, which fires call, exit,
redo and fail with the depth rebound around the continuation and an
`unwind-protect` that fires the fails a non-local exit abandons; the
unit 2 dispatcher fires a clause event. `trace-prolog` is a printing
hook with a name filter. `explain-goals` walks a goal list with a
static binding analysis and asks a registry of access-path functions,
one per functor family, what each goal will do; the rules layer
registers `claim/7` by factoring its route choice into `%claim-route`.
`profile-goals` runs the goals under a counting hook. The text entries
live in `graph-db.query` over `guard-query-text`.

**Tech Stack:** Common Lisp (SBCL primary, plain CL throughout),
FiveAM.

**Spec:** `docs/superpowers/specs/2026-11-09-tracer-explain-design.md`
(D1 to D6 binding). **Base:** `experiment` after PR #483 (unit 2) has
merged; the branch is `tracer-explain`.

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments pointing at the
  spec section or GH #124 (`~/.claude/CLAUDE.md`).
- Engine core (Tasks 1, 3): inline in the session. Tasks 2, 4, 5 and 6
  are checkable and may go to local-agent with this plan as the brief.
- The untraced call path changes shape only as D1 says: one special
  read and a branch where `(when *prolog-trace* ...)` sits today; the
  k-hop and bound-lookup bench rows must not move (Task 6).
- No new reader conditional; `get-internal-real-time` for timing.
- Suites, one SBCL at a time in the checkout, never the full suite:
  `tests/run-suite.sh tracer-explain-suite`, `prolog-functor-suite`,
  `prolog-stress-suite`, `clause-indexing-suite`;
  `tests/run-system-tests.sh graph-db/rules-test` (Task 4),
  `graph-db/query-test` (Task 5), `graph-db/concurrency-test` (Task 1).
- Test names are symbols in `graph-db/test`; every new name here is
  prefixed `te-`; the rules and query suites' names `ter-` and `teq-`.
- Asserted functors are image-global: drop before and after, with the
  `ci-with-functors` macro of `tests/clause-indexing-tests.lisp` (same
  package; reuse, do not redefine).
- Docs travel with code: CHANGELOG `## [Unreleased]` in the task that
  adds the behaviour; the manual, `docs/rules.md` (routes) and
  `docs/codebase-notes.md` in Task 6.
- Public repo: domain-neutral text.

## Review Focus

1. A traced call whose callee signals a Lisp error (not a cut, not a
   budget): the `unwind-protect` must fire `:fail` and re-signal, not
   swallow. Test in Task 1.
2. The hook itself signalling or returning non-locally: the engine
   must not be left with a wrong `*prolog-port-depth*`; binding, not
   `setf`, makes this hold. Test in Task 1 (a hook that throws once).
3. `explain-goals` on a goal list naming a functor the image does not
   have: a row with `:kind :unknown`, no error, and the remaining rows
   still produced. Test in Task 3.
4. `trace-prolog` with a name that resolves in a schema package, not
   `graph-db` (an edge functor): the filter must match the functor
   symbol `make-functor-symbol` resolves, so `(trace-prolog 'g-knows)`
   traces `g-knows/2`. Test in Task 2.
5. `profile-goals` under `:max-inferences` that trips: rows returned,
   the error as the third value, counts balanced (calls = fails per
   functor). Test in Task 5.

---

### Task 1: The port hook

**Files:**
- Modify: `globals.lisp` (after `*clause-indexing*`): `*prolog-port-hook*`,
  `*prolog-port-depth*`
- Modify: `prologc.lisp`: `compile-call` (:245), new `%ported-call`,
  `call-with-prolog-ports`, `with-prolog-ports`; `%make-dispatcher`'s
  template (the `:clause` event)
- Modify: `prolog-functors.lisp`: `%solve`'s apply site (:278)
- Modify: `package.lisp`: export `*prolog-port-hook*`,
  `*prolog-port-depth*`, `call-with-prolog-ports`, `with-prolog-ports`
- Create: `tests/tracer-explain-tests.lisp`; add to `graph-db.asd`
  after `clause-indexing-tests`
- Modify: `CHANGELOG.md`

**Interfaces:**
- Produces: `*prolog-port-hook*` (nil or `(lambda (port functor args
  depth))`), `*prolog-port-depth*` (fixnum, 0 at top level);
  `(%ported-call hook functor func args cont)` where `args` is a list;
  `(call-with-prolog-ports hook thunk)`; `(with-prolog-ports ((port
  functor args depth) &body body) &body)` binding the hook to a lambda
  over those four names around the second body (signature:
  `(with-prolog-ports (lambda-list &body hook-body) form)`; use
  `(with-prolog-ports ((port functor args depth) (push ... log)) (select ...))`).

- [ ] **Step 1: Failing tests** (header and suite as
  `clause-indexing-tests.lisp`, suite `tracer-explain-suite :in
  graph-db-suite`):

```lisp
(defun te-ports (thunk &key (functor nil))
  "(PORT FUNCTOR-NAME DEPTH) for every event THUNK's run fires, newest
last; FUNCTOR, a string, keeps only that functor's."
  (let ((log '()))
    (with-prolog-ports ((port f args depth)
                        (declare (ignore args))
                        (when (or (null functor)
                                  (string= functor (symbol-name f)))
                          (push (list port (symbol-name f) depth) log)))
      (funcall thunk))
    (nreverse log)))

(test te-four-ports-of-a-fact-table
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1)) (<- (te-g 2))
      (is (equal '((:call "TE-G/1" 0) (:clause "TE-G/1" 1)
                   (:exit "TE-G/1" 0) (:redo "TE-G/1" 0)
                   (:clause "TE-G/1" 1)
                   (:exit "TE-G/1" 0) (:redo "TE-G/1" 0) (:fail "TE-G/1" 0))
                 (te-ports (lambda () (select-flat (?x) (te-g ?x)))))))))

(test te-depth-nests-callees-not-siblings
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1) (te-h 1))
      (<- (te-g 1))
      (<- (te-h ?x) (te-g ?x))
      ;; te-g inside te-h is depth 1; the = after te-h is not a ported
      ;; goal, so the next ported sibling (te-g again) is depth 0.
      (is (equal '((:call "TE-H/1" 0) (:clause "TE-H/1" 1)
                   (:call "TE-G/1" 1) (:clause "TE-G/1" 1) (:exit "TE-G/1" 1)
                   (:exit "TE-H/1" 0)
                   (:call "TE-G/1" 0) (:clause "TE-G/1" 1) (:exit "TE-G/1" 0)
                   (:redo "TE-G/1" 0) (:fail "TE-G/1" 0)
                   (:redo "TE-H/1" 0)
                   (:redo "TE-G/1" 1) (:fail "TE-G/1" 1)
                   (:fail "TE-H/1" 0))
                 (te-ports (lambda ()
                             (select-flat (?x) (te-h ?x) (te-g ?x)))))))))

(test te-dynamic-meta-call-has-ports
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (is (equal '(:call :exit :redo :fail)
                 (mapcar #'first
                         (te-ports (lambda ()
                                     (select-flat (?x) (= ?goal (te-g ?x))
                                                  (call ?goal)))
                                   :functor "TE-G/1")))))))

(test te-a-cut-fires-the-fails-it-abandons
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1) (te-c 1))
      (<- (te-g 1)) (<- (te-g 2))
      (<- (te-c ?x) (te-g ?x) !)
      (let ((ports (te-ports (lambda () (select-flat (?x) (te-c ?x))))))
        (is (= (count :call ports :key #'first)
               (count :fail ports :key #'first)))
        (is (equal '(1) (select-flat (?x) (te-c ?x))))))))

(test te-a-budget-error-fires-the-fails-and-resignals
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1)) (<- (te-g 2)) (<- (te-g 3))
      (let (ports)
        (signals graph-db:prolog-resource-error
          (setf ports (te-ports
                       (lambda ()
                         (select (:max-inferences 2) (?x ?y ?z)
                           (te-g ?x) (te-g ?y) (te-g ?z))))))
        ;; TE-PORTS did not return; re-run collecting through the error.
        (let ((log '()))
          (ignore-errors
           (with-prolog-ports ((port f args depth)
                               (declare (ignore args depth))
                               (push (list port f) log))
             (select (:max-inferences 2) (?x ?y ?z)
               (te-g ?x) (te-g ?y) (te-g ?z))))
          (is (= (count :call log :key #'first)
                 (count :fail log :key #'first))))))))

(test te-a-hook-that-throws-leaves-the-depth-at-zero
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (ignore-errors
       (with-prolog-ports ((port f args depth)
                           (declare (ignore f args depth))
                           (when (eq port :exit) (error "hook")))
         (select-flat (?x) (te-g ?x))))
      (is (= 0 graph-db:*prolog-port-depth*))
      (is (null graph-db:*prolog-port-hook*)))))

(test te-a-lisp-error-in-a-callee-resignals-after-the-fail
  (with-test-graph (g) (declare (ignore g))
    (let ((log '()))
      (signals error
        (with-prolog-ports ((port f args depth)
                            (declare (ignore args depth))
                            (push (list port f) log))
          (select-flat (?x) (lisp ?x (error "callee")))))
      (is (member :fail log :key #'first)))))
```

  Add `(:file "tracer-explain-tests")` after `clause-indexing-tests`
  in `graph-db.asd`.

- [ ] **Step 2:** `tests/run-suite.sh tracer-explain-suite`: fails on
  `WITH-PROLOG-PORTS` undefined (a read or compile error is the
  expected RED here).

- [ ] **Step 3: globals.lisp.**

```lisp
(defvar *prolog-port-hook* nil
  "NIL, or a function of (PORT FUNCTOR ARGS DEPTH) called at :CALL,
:EXIT, :REDO and :FAIL of every goal call and at :CLAUSE when a
dispatcher tries a candidate (tracer design D1, GH #124).  Bind it;
TRACE-PROLOG sets it.")
(defvar *prolog-port-depth* 0
  "Goal calls open above the current one while the hook runs.")
```

- [ ] **Step 4: prologc.lisp.** `%ported-call` per spec D1:

```lisp
(defun %ported-call (hook functor func args cont)
  "FUNC applied to ARGS and a continuation, with HOOK's ports (D1)."
  (let ((d *prolog-port-depth*) (failed nil))
    (funcall hook :call functor args d)
    (unwind-protect
         (let ((*prolog-port-depth* (1+ d)))
           (apply func
                  (append args
                          (list (lambda ()
                                  (let ((*prolog-port-depth* d))
                                    (funcall hook :exit functor args d)
                                    (funcall cont)
                                    (funcall hook :redo functor args d))))))
           (setf failed t)
           (funcall hook :fail functor args d))
      (unless failed (funcall hook :fail functor args d)))))
```

  `compile-call`'s generated code: replace the `*prolog-trace*` print
  and the `(funcall func ,@args ,cont)` with

```lisp
(let ((hook *prolog-port-hook*))
  (if hook
      (%ported-call hook ',functor func (list ,@args) ,cont)
      (funcall func ,@args ,cont)))
```

  keeping `(%tick)` and the existence error as they are. In
  `%make-dispatcher`'s template, before each `(funcall (svref fns ...)`
  (both the single-candidate and the loop forms), emit
  `(let ((hook *prolog-port-hook*)) (when hook (funcall hook :clause
  (functor-name functor) (list pos n) *prolog-port-depth*)))` with `pos`
  the position about to be tried (bind it in the template; the `n` is
  the candidate count already in scope). `call-with-prolog-ports` binds
  `*prolog-port-hook*` to `hook` and `*prolog-port-depth*` to 0 around
  `(funcall thunk)`; `with-prolog-ports` expands to it with a lambda.

- [ ] **Step 5: `%solve`** (prolog-functors.lisp:278): the `apply` arm
  becomes the same `(let ((hook *prolog-port-hook*)) (if hook
  (%ported-call hook functor fn gargs cont) (apply fn (append gargs
  (list cont)))))`.

- [ ] **Step 6:** `tracer-explain-suite` all pass; then
  `clause-indexing-suite` (51), `prolog-functor-suite` (60),
  `prolog-stress-suite` (93), `graph-db/concurrency-test` (1680).

- [ ] **Step 7: Mutation.** Remove the `:redo` firing: the first two
  tests fail. Restore, rerun green.

- [ ] **Step 8: Docs and commit.** CHANGELOG `### Added`: **A 4-port
  trace hook** (#124): `*prolog-port-hook*` at call, exit, redo, fail
  and the dispatcher's clause event; an untraced call costs what it
  did; `with-prolog-ports` binds it for one extent.

```bash
git add globals.lisp prologc.lisp prolog-functors.lisp package.lisp tests/tracer-explain-tests.lisp graph-db.asd CHANGELOG.md
git commit -m "prolog: the 4-port trace hook (#124)"
```

### Task 2: `trace-prolog`

**Files:**
- Modify: `prologc.lisp:47-48` (`trace-prolog`, `untrace-prolog`)
- Modify: `tests/tracer-explain-tests.lisp`, `CHANGELOG.md`

**Interfaces:**
- Produces: `(trace-prolog &rest names)`: installs `%print-port` as the
  global hook with a filter; `names` are symbols (`'te-g`, every arity;
  `'te-g/1`, one) resolved by `make-functor-symbol` the lookup way for
  bare names (try arities 0 to 8 and keep the registered ones) and by
  string for `NAME/N`; no names: every functor. `(untrace-prolog)` sets
  the hook nil. `%print-port` writes to `*trace-output*` one line
  `  [D] PORT functor (args)` indented two spaces per depth, args
  through `deref-exp` under `*print-length*` 8 and `*print-level*` 3;
  `:clause` prints `CLAUSE functor POS/N` (1-based).

- [ ] **Step 1: Failing tests:**

```lisp
(defun te-trace-text (thunk &rest names)
  (let ((*trace-output* (make-string-output-stream)))
    (unwind-protect
         (progn (apply #'trace-prolog names) (funcall thunk)
                (get-output-stream-string *trace-output*))
      (untrace-prolog))))

(test te-trace-prolog-prints-ports-indented
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-g 1) (te-h 1))
      (<- (te-g 1)) (<- (te-h ?x) (te-g ?x))
      (let ((text (te-trace-text (lambda () (select-flat (?x) (te-h ?x))))))
        (is (search "[0] CALL TE-H/1 (" text))
        (is (search "[1]   CALL TE-G/1 (" text))
        (is (search "[1]   EXIT TE-G/1 (1)" text))
        (is (search "[0] EXIT TE-H/1 (1)" text))
        (is (search "CLAUSE TE-G/1 1/1" text))
        (is (search "[0] FAIL TE-H/1" text))))))

(test te-trace-prolog-filters-by-name-across-packages
  (with-test-graph (g)
    (with-transaction () (make-g-person :name "te-a"))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (let ((text (te-trace-text
                   (lambda () (select-flat (?x) (is-a ?p g-person) (te-g ?x)))
                   'te-g)))
        (is (search "TE-G/1" text))
        (is (null (search "IS-A/2" text))))
      ;; Review focus 4: a schema-package edge functor by bare name.
      (let ((text (te-trace-text
                   (lambda () (select-flat (?a) (g-knows ?a ?b)))
                   'g-knows)))
        (is (search "G-KNOWS/2" text))))))

(test te-untrace-prolog-clears-the-hook
  (trace-prolog) (untrace-prolog)
  (is (null graph-db:*prolog-port-hook*)))
```

- [ ] **Step 2:** RED on `trace-prolog` with arguments (today it takes
  none) or on the text shape.
- [ ] **Step 3:** Implement per Interfaces; `trace-prolog` no longer
  touches `*prolog-trace*`.
- [ ] **Step 4:** Suite green; `prolog-functor-suite` green.
- [ ] **Step 5:** CHANGELOG `### Changed`: **`trace-prolog` prints the
  four ports** (#124), optionally for named functors, and no longer
  sets `*prolog-trace*`, which stays the compiler's own chatter.

```bash
git add prologc.lisp tests/tracer-explain-tests.lisp CHANGELOG.md
git commit -m "prolog: trace-prolog prints the four ports, by name (#124)"
```

### Task 3: `explain-goals` and the access-path registry

**Files:**
- Modify: `prologc.lisp`: `*access-paths*` (an `equal`? no: `eq` table
  keyed by functor symbol, created like `*cost-unbounded-functors*`),
  `register-access-path`, `functor-access-path`
- Create: `prolog-explain.lisp` after `index.lisp` in `graph-db/core`
  (`:depends-on ("index" "prolog-functors")`): the binding walk,
  `explain-goals`, the printer, and the core registrations for
  `is-a/2`, `node-slot-value/3`, the asserted-functor rule
- Modify: `prolog-functors.lisp` (`install` at :1207): register the
  edge functors' paths; `index.lisp` and `spatial-query.lisp`: register
  the `find-*` paths beside their definitions
- Modify: `package.lisp` (exports), `tests/tracer-explain-tests.lisp`,
  `CHANGELOG.md`

**Interfaces:**
- Produces: `(register-access-path functor-symbol fn)`, `fn` of
  `(bound args graph) => string` where `bound` is a list of booleans
  and `args` the goal's arguments with `:bound` in place of a variable
  bound by an earlier goal and the variable itself when unbound;
  `(explain-goals vars goals &key (graph *graph*) stream) => rows`,
  each `(:goal form :functor sym-or-nil :kind kind :bound list :path
  string [:candidates (k . n)])`. The walk: `%explain-walk (goals
  bound-vars)` recursing into `and`, `or`, `not`, `if`, `once`,
  `forall`, static `call`; `not` and `forall` bodies' new bindings do
  not leak. Spec D3's table is the registration list.

- [ ] **Step 1: Failing tests** (schema `g-person`/`g-knows` from the
  main suite; `find-by-slot` needs an index: use the one
  `tests/index-prolog-tests.lisp` declares, or declare `te-` ones in
  this file's own graph):

```lisp
(defun te-paths (vars goals)
  (mapcar (lambda (row) (getf row :path)) (explain-goals vars goals)))

(test te-explain-type-slot-and-edge-paths
  (with-test-graph (g) (declare (ignore g))
    (is (equal '("type index: G-PERSON" "slot read" "ve-out index of ?P")
               (te-paths '(?n) '((is-a ?p g-person) (node-slot-value ?p name ?n)
                                 (g-knows ?p ?q)))))
    (is (equal "type check" (first (te-paths '(?t) '((is-a ?p ?t) (is-a ?p g-person))))))
    (is (search "every vertex" (first (te-paths '(?p) '((is-a ?p ?t))))))
    (is (search "not a type" (first (te-paths '(?p) '((is-a ?p no-such-type))))))
    (is (equal "vev index: one lookup"
               (third (te-paths '(?a) '((is-a ?a g-person) (is-a ?b g-person) (g-knows ?a ?b))))))
    (is (equal "ve-in index of ?B" (second (te-paths '(?b) '((is-a ?b g-person) (g-knows ?a ?b))))))
    (is (equal "every G-KNOWS edge" (first (te-paths '(?a) '((g-knows ?a ?b))))))))

(test te-explain-asserted-functor-candidates
  (with-test-graph (g) (declare (ignore g))
    (ci-with-functors ((te-k 2))
      (<- (te-k a 1)) (<- (te-k b 2)) (<- (te-k ?v 3))
      (let ((rows (explain-goals '(?r) '((te-k a ?r)))))
        (is (equal '(2 . 3) (getf (first rows) :candidates)))
        (is (equal "first-argument index: 2 of 3 clauses" (getf (first rows) :path))))
      (is (equal "every clause: 3" (first (te-paths '(?k ?r) '((te-k ?k ?r))))))
      (is (search "by value at run time, 3 clauses"
                  (second (te-paths '(?r) '((= ?k a) (te-k ?k ?r)))))))))

(test te-explain-unknown-functor-and-binding-through-not
  (with-test-graph (g) (declare (ignore g))
    (let ((rows (explain-goals '(?x) '((no-such-pred ?x) (is-a ?x g-person)))))
      (is (eq :unknown (getf (first rows) :kind)))
      (is (= 2 (length rows))))
    ;; ?Q is bound inside not, unbound after it: the edge goal after
    ;; the not sees ?Q unbound, so the path is ve-out of ?P, not vev.
    (is (equal "ve-out index of ?P"
               (third (te-paths '(?p) '((is-a ?p g-person)
                                        (not (g-knows ?p ?q))
                                        (g-knows ?p ?q))))))
    (is (search "meta-call" (first (te-paths '(?g) '((call ?g))))))))

(test te-explain-index-paths
  ;; Declare a te- index on this graph's schema; the test owns it.
  (with-test-graph (g)
    (is (search "ordered index on" (first (te-paths '(?n) '((find-by-slot ?n g-person name "x"))))))
    (is (search "the call signals" (first (te-paths '(?n) '((find-by-slot ?n g-person no-slot "x"))))))))
```

  (`find-by-slot` on `g-person.name`: check whether the main suite
  declares an index on it; if not, `def-index` one in this file as
  `te-name-index` and withdraw it after.)

- [ ] **Step 2:** RED on `explain-goals` undefined.
- [ ] **Step 3:** Implement: the registry in prologc.lisp; the file
  `prolog-explain.lisp` with the walk (bound set threaded as a list of
  variable symbols; `vars` start unbound), functor resolution by
  `make-functor-symbol` without `:define` plus a registered-functor
  check (`%registered-functor-p`), kinds `:user` (in `*user-functors*`
  with clauses), `:global`, `:control` (compiler macro or variable
  head), `:unknown`; the asserted-functor path from
  `functor-clause-set` and `%clause-index-key` (`%candidates`'s own
  logic, on the literal); the registrations in the files named, the
  edge ones inside `install` with the type name in the string; a
  functor with no entry gets "no path declared", with
  ", cost-unbounded" appended when `functor-cost-unbounded-p`.
- [ ] **Step 4:** Suite green; `clause-indexing-suite`,
  `prolog-functor-suite`, `index-prolog-suite` (its name in
  `tests/index-prolog-tests.lisp`) green.
- [ ] **Step 5:** CHANGELOG `### Added`: **`explain-goals`** (#124):
  per goal the functor, the bound arguments and the access path, from
  a registry (`register-access-path`) the type, slot, edge, ordered
  index and spatial predicates fill; an asserted functor reports the
  clauses its first-argument index leaves.

```bash
git add prologc.lisp prolog-explain.lisp prolog-functors.lisp index.lisp spatial-query.lisp graph-db.asd package.lisp tests/tracer-explain-tests.lisp CHANGELOG.md
git commit -m "prolog: explain-goals and the access-path registry (#124)"
```

### Task 4: `claim/7`'s route, factored and registered

**Files:**
- Modify: `rules/facts.lisp` (`%claim-goal` :300; new `%claim-route`;
  registrations for `claim/7`, `claim-after/9`)
- Modify: `tests/rules/facts-tests.lisp` (or a new
  `tests/rules/explain-tests.lisp` added to the rules test system),
  `docs/rules.md` ("Routes, and what refuses": one sentence naming
  `explain-goals`), `CHANGELOG.md`

**Interfaces:**
- Produces: `(%claim-route c sns-arg sns ons-arg ons skey okey rel) =>
  keyword` in `%claim-goal`'s `cond` order: `:bound-claim`,
  `:subject-relation-index`, `:subject-index`, `:object-index`,
  `:empty-namespace`, `:subject-vocabulary`, `:object-vocabulary`,
  `:family-walk`; `%claim-goal` switches on it (`case`) with the same
  bodies; the path function maps the keyword to docs/rules.md's words,
  "family walk; refused under a resource bound" for the last, and
  resolves literal namespaces through `%namespace-keyword` so a string
  naming no keyword reports `:empty-namespace`; a namespace bound by
  an earlier goal reports "subject index, by value at run time".

- [ ] **Step 1: Failing tests** in the rules suite: for each of the
  eight patterns, `explain-goals` on a `(claim ?c fam "ns" ...)` goal
  reports the expected route words, and a run of the same goal under
  `with-prolog-ports` counting `:call`s of the index readers (or, more
  simply, a `trace-call`-free check: `%claim-route` on the dereferenced
  arguments equals the keyword `%claim-goal` is about to switch on,
  exposed for the test as a special `*last-claim-route*` set under
  `(when *prolog-port-hook* ...)` only).
- [ ] **Step 2:** RED. **Step 3:** implement. **Step 4:**
  `graph-db/rules-test` green (baseline 1129 plus the new checks).
- [ ] **Step 5:** docs, commit:

```bash
git add rules/facts.lisp tests/rules/ docs/rules.md CHANGELOG.md
git commit -m "rules: claim/7's route in one place, and explained (#124)"
```

### Task 5: `profile-goals` and the text entries

**Files:**
- Modify: `prolog-explain.lisp` (`profile-goals`), `query/guard.lisp`
  (`explain-text`, `profile-text`), `query/package.lisp`,
  `package.lisp`, `tests/tracer-explain-tests.lisp`,
  `tests/query/guard-tests.lisp`, `docs/guarded-query.md`, `CHANGELOG.md`

**Interfaces:**
- Produces: `(profile-goals vars goals &key (graph *graph*) stream limit
  max-inferences timeout) => (values solution-count rows error)`; rows
  `(:functor sym :calls n :exits n :redos n :fails n :clauses n
  :seconds float)` sorted by seconds descending; runs through
  `run-query-goals` with `:format :raw` and a counting callback under
  `call-with-prolog-ports`; a `prolog-resource-error` is caught and
  returned third. `graph-db.query:explain-text (text graph &key stream)`
  and `profile-text (text graph &key stream limit max-inferences
  timeout)` over `guard-query-text`.

- [ ] **Step 1: Failing tests:** `te-profile-counts-match-the-ports`
  (counts for `te-g/1` equal the Task 1 sequence's: 1 call, 2 exits, 2
  redos, 1 fail, 2 clauses; seconds `>= 0`; solutions 2);
  `te-profile-under-a-budget-returns-rows-and-the-error` (review focus
  5: calls = fails per functor, third value a
  `prolog-resource-error`). In the query suite:
  `teq-explain-text-matches-explain-goals` and
  `teq-profile-text-refuses-what-the-guard-refuses`.
- [ ] **Step 2:** RED. **Step 3:** implement. **Step 4:**
  `tracer-explain-suite`, `graph-db/query-test` green.
- [ ] **Step 5:** `docs/guarded-query.md`: a section for the two text
  entries. CHANGELOG `### Added`: **`profile-goals`, `explain-text`,
  `profile-text`** (#124).

```bash
git add prolog-explain.lisp query/guard.lisp query/package.lisp package.lisp tests/tracer-explain-tests.lisp tests/query/guard-tests.lisp docs/guarded-query.md CHANGELOG.md
git commit -m "prolog: profile-goals, and the guarded text entries (#124)"
```

### Task 6: Measurement and the manual

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` (Chapter 6, a section after
  "Asserted clauses with ~<-~": the ports, `trace-prolog` with the
  `ancestor/2` trace from the spec's acceptance, `explain-goals` with a
  printed table, `profile-goals`), `docs/codebase-notes.md` (one line:
  `explain-goals` before guessing a goal's path),
  `tests/perf/prolog-bench.lisp` (a `traced-k-hop` row: the k-hop case
  under `trace-prolog` to a null stream), `docs/perf-baselines.md`

- [ ] **Step 1:** Bench, three normal-scale runs, third recorded:
  `k-hop-3-select`, `clause-bound-first-arg`, `guarded-shape-repeat`
  against the unit 2 after rows (expected unchanged within noise), and
  the new `traced-k-hop` row for the record. Report committed as
  `tests/perf/results/prolog-after-u3-odm-<commit>.report`; the rows on
  #124.
- [ ] **Step 2:** The manual section, with a real trace pasted from a
  run of the `ancestor/2` example.
- [ ] **Step 3:** `graph-db/rules-test` and `graph-db/query-test` once
  more; commit.

```bash
git add docs/vivace-graph-v3-doc.org docs/codebase-notes.md docs/perf-baselines.md tests/perf/prolog-bench.lisp tests/perf/results/prolog-after-u3-odm-*.report
git commit -m "prolog: tracer and explain measured; manual and notes (#124)"
```

### Landing

- Whole-branch review, PR into `experiment`, post-merge run read, #124
  closed with the landing SHA, the acceptance's three items named.
