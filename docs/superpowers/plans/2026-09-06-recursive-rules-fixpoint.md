# Recursive Rules Under Fixpoint Iteration — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A rule whose body reads its own head relation (directly or through other rules) compiles, and `run-rules` derives its fixpoint by semi-naive iteration, terminating on a cyclic claim graph (GH #333, #304's last acceptance criterion; #122 unit 1).

**Architecture:** The compiler replaces the cycle refusal with strata (SCCs of the relation dependency graph) and refuses only unstratified negation and `:any` reads. `run-rules` orders strata and runs a recursive stratum in rounds: round 0 in full, later rounds as body variants in which one recursive `claim/7` goal is fed by an internal delta generator over the previous round's new claims; the sweep happens once at the fixpoint. Single-store strata run in one transaction (the open transaction sees its own writes); cross-store strata commit per round.

**Tech Stack:** SBCL, FiveAM, `graph-db/rules` over `graph-db/spacetime` and `graph-db/query`.

**Spec:** `docs/superpowers/specs/2026-09-06-recursive-rules-fixpoint-design.md`. One ruling made while planning, to be folded into spec §3 in Task 6: **single-store strata run in ONE transaction** (a refusal leaves the previous derivation standing exactly); cross-store strata commit per round, since a foreign read inside a transaction is refused (GH #53) and a later round must see the earlier rounds' claims.

## Global Constraints

- Lisp: spaces only, hard 80 columns, terse comments pointing at GH #333 or the spec section; docstrings state what/returns/trap.
- Branch `feat/rules-s4` from `experiment` (8ad65b0), worktree
  `/tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-experiment/.worktrees/rules-s4`. Never touch `/home/raison/work/vivace-graph-v3`.
- Never run `pkill`, `pgrep -f`, or `kill`. One SBCL build at a time in this worktree. Never the full 15-minute suite; the rules suite and the guarded-query suite only (below). CI runs the full suite on push.
- The delta generator is never admissible from free text: it is in `*prolog-excluded-predicates*`, and `compile-rule`'s guard therefore refuses a body naming it.
- `select` and the guarded surface are unchanged.
- Existing rules tests keep passing except the three cycle tests Task 1 amends; every amendment is named in that task.
- Docs travel with the code (Task 6); every commit message names GH #333.
- Commit trailers on every commit:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
  ```

## Running the suites

Write once to `/tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-333-suites.lisp`:

```lisp
;; The rules suite and the guarded-query suite, CI-style, fresh image.
(ql:quickload '(:graph-db/rules-test :graph-db/query-test) :silent t)
(let ((ok t))
  (unless (graph-db/rules-test::run-rules-tests) (setf ok nil))
  (unless (graph-db/query-test::run-query-tests) (setf ok nil))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root:

```bash
cd /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-experiment/.worktrees/rules-s4
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-333-suites.lisp \
  > /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-333-suites.log 2>&1; echo "exit=$?"
grep -E "Did [0-9]+ checks|Fail:" /tmp/claude-1000/-home-raison-work-cl-llm/8235f26d-9ab1-4573-b1ef-c3408365e71c/scratchpad/vg-333-suites.log
```

Task 1 records the baseline counts before any change; no later run may drop below them except where a task names a removed assertion.

## Code facts every task relies on

- A guarded body is a list of goal forms. A `claim/7` goal is `(graph-db:claim ?c family sns skey rel ons okey)`; `%engine-goal-p goal "CLAIM" 7` recognises it and `(sixth goal)` is the relation, a string when bound. A negation is `(not GOAL)` with `not` the CL symbol; the guard admits only a full goal as its argument.
- `claim/7` with `?c` bound to a node is a filter: it unifies family and endpoints against that one claim (`rules/facts.lisp`, the `(node-p c)` clause).
- `%solutions compiled graph report` runs `(compiled-rule-goals compiled)` through `run-query-goals` under the rails; `%desired` turns rows into `(values TABLE ORDER)`, TABLE a dedupe key → `(ARGS . PREMISES)`; `%reconcile-claims` keeps/sweeps/constructs against `claims-by-producer` and returns the standing alist; `%reconcile-provenance` rewrites `derived-from` records from `desired` and `standing`.
- Inside an open transaction on the own store, index reads (`claims-touching`, `index-lookup`) see the transaction's uncommitted writes (GH #324); a read of another store inside one is `cross-graph-transaction-error` (GH #53). `%under-snapshots graphs thunk` composes read snapshots for the cross-store path.
- `rules-in-scope graph` is every enabled stored rule plus every enabled `def-rule`; `%edges spec graph` gives `(head-relation . reads)` for a spec, NIL when its text does not guard.

---

### Task 1: Strata at compile time

**Files:**
- Modify: `rules/compile.lisp` (`compiled-rule` struct, `%body-reads`, new `%negative-reads`, `%strata` replacing `%check-cycle`, `compile-rule`), `rules/package.lisp` (exports)
- Test: `tests/rules/compile-tests.lisp` (amend three, add three)

**Interfaces:**
- Produces: `compiled-rule` gains `stratum` (list of rule-name strings, sorted `string<`, always containing the rule's own name) and `stratum-relations` (list of relation strings the stratum's rules derive, sorted). Exported accessors `compiled-rule-stratum`, `compiled-rule-stratum-relations`. `(%recursive-p compiled)` is `(rest (compiled-rule-stratum c))` or a body read of the rule's own relation — i.e. `(intersection (compiled-rule-reads c) (compiled-rule-stratum-relations c))` non-empty.
- Refusals: unstratified negation → `rule-compile-error` with reason containing "negation over the rule's own stratum"; `:any` reads as today.

- [ ] **Step 1: Baseline the suites**

Write the runner script from "Running the suites" and run it on the untouched tree. Record the two `Did N checks` lines in the ledger. Expected `Fail: 0` both.

- [ ] **Step 2: Amend the three cycle tests and add three**

In `tests/rules/compile-tests.lisp`, replace `a-rule-that-reads-its-own-relation-is-refused` with:

```lisp
(test a-rule-that-reads-its-own-relation-compiles-as-its-own-stratum
  "GH #333: recursion is a stratum, not a refusal; an unbound relation
still reads everything and is refused (P6)."
  (with-rules-graph (g)
    (let ((c (graph-db.rules:compile-rule
              g (graph-db.rules::%make-rule-spec
                 :name "r" :version "1" :family "rt-claim"
                 :head *head-runs* :body *web-hosts-body*
                 :extent-policy :premises :enabled t :source :stored))))
      (is (equal '("r") (graph-db.rules:compiled-rule-stratum c)))
      (is (equal '("runs")
                 (graph-db.rules:compiled-rule-stratum-relations c))))
    (refuses "bind the relation" :name "r" :version "1" :family "rt-claim"
             :head *web-hosts-head* :body *body-relation-var*)))
```

Replace `a-cycle-across-two-rules-is-refused-and-named` with:

```lisp
(test a-cycle-across-two-rules-is-one-stratum
  "GH #333: two rules deriving each other's reads share a stratum,
whichever is written first; a third reading elsewhere is its own."
  (with-rules-graph (g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head *head-x* :body *body-y*)
    (write-rule g :name "b" :version "1" :family "rt-claim"
                :head *head-y* :body *body-x*)
    (write-rule g :name "c" :version "1" :family "rt-claim"
                :head *head-y* :body *body-z*)
    (let ((a (graph-db.rules:compile-rule g "a"))
          (c (graph-db.rules:compile-rule g "c")))
      (is (equal '("a" "b" "c") (graph-db.rules:compiled-rule-stratum a)))
      (is (equal '("x" "y")
                 (graph-db.rules:compiled-rule-stratum-relations a)))
      ;; c derives y too, so it is in the stratum by relation, not by
      ;; its own reads.
      (is (equal '("a" "b" "c") (graph-db.rules:compiled-rule-stratum c))))))
```

In `a-def-rule-joins-the-cycle-graph-and-collides-by-name`, delete the `(signals graph-db.rules:rule-compile-error (write-rule g :name "a" ...))` form and its comment (the cycle is no longer a refusal); keep the name-collision `signals`, the `undef-rule`, and the final `finishes`. Rename the test `a-def-rule-collides-by-name`.

Append:

```lisp
(defparameter *body-not-y*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a)
   (not (claim ?q rt-claim \"app\" ?a \"y\" \"host\" ?h))")
(defparameter *body-not-z*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a)
   (not (claim ?q rt-claim \"app\" ?a \"z\" \"host\" ?h))")

(test negation-over-the-rules-own-stratum-is-refused
  "GH #333: a NOT whose goal reads a relation the rule's stratum derives
has no fixpoint; refused naming the relation."
  (with-rules-graph (g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head *head-x* :body *body-y*)
    (let ((c (handler-case
                 (progn (write-rule g :name "b" :version "1"
                                    :family "rt-claim"
                                    :head *head-y* :body *body-not-x*)
                        nil)
               (graph-db.rules:rule-compile-error (c) c))))
      (is-true c)
      (when c
        (is (search "negation over the rule's own stratum"
                    (graph-db.rules:rule-compile-error-reason c)))
        (is (search "x" (graph-db.rules:rule-compile-error-reason c)))))))

(test negation-over-an-earlier-stratum-compiles
  (with-rules-graph (g)
    (write-rule g :name "z-maker" :version "1" :family "rt-claim"
                :head *head-z* :body *web-hosts-body*)
    (finishes
      (write-rule g :name "b" :version "1" :family "rt-claim"
                  :head *head-y* :body *body-not-z*))
    (is (equal '("b") (graph-db.rules:compiled-rule-stratum
                       (graph-db.rules:compile-rule g "b"))))))
```

Define `*body-not-x*` beside the two above as the `*body-not-y*` text with `"y"` replaced by `"x"`, and check that `*head-z*` exists in the file (it does if `*body-z*` does; otherwise define it as `(claim ?c rt-claim \"app\" \"web\" \"z\" \"host\" ?h)`).

- [ ] **Step 3: Run the suites; expect the amended and new tests to fail**

Expected: the rules suite fails on the two stratum accessors being undefined and on the negation refusal not happening; everything else green.

- [ ] **Step 4: Implement strata**

In `rules/compile.lisp`, extend the struct:

```lisp
(defstruct (compiled-rule (:constructor %make-compiled-rule))
  "A rule ready to run: the guarded goals and what the head derives.
HEAD-* are the head's argument terms -- a keyword (namespace), a string
(key or relation), NIL, or a body variable.  VARS is SELECT's variable
list, PREMISE-VARS the ?c of every body CLAIM/7 goal, READS the
relations the body reads -- never :ANY, which COMPILE-RULE refuses
before this struct is built.  STRATUM is the sorted names of the rules
whose head relations are mutually reachable with this one's (its own
name always), STRATUM-RELATIONS the sorted relations they derive; a
rule is recursive when READS meets STRATUM-RELATIONS (GH #333)."
  spec family relation
  head-c head-sns head-skey head-ons head-okey unary-p
  vars premise-vars goals reads stratum stratum-relations)
```

Add after `%body-reads`:

```lisp
(defun %not-goal-p (goal)
  "(not G): the guard admits only a full goal as G."
  (and (consp goal) (symbolp (first goal))
       (string= (symbol-name (first goal)) "NOT")
       (= 2 (length goal))))

(defun %negative-reads (goals)
  "The relations read under a NOT, at any nesting; :ANY when one is
unbound there (GH #333)."
  (let ((reads '()))
    (labels ((walk (goal negated)
               (cond ((%not-goal-p goal) (walk (second goal) t))
                     ((and negated (%engine-goal-p goal "CLAIM" 7))
                      (let ((rel (sixth goal)))
                        (if (stringp rel)
                            (pushnew rel reads :test #'string=)
                            (return-from %negative-reads :any))))
                     ((and (consp goal) (rest goal))
                      (dolist (sub (rest goal))
                        (when (consp sub) (walk sub negated)))))))
      (dolist (goal goals) (walk goal nil)))
    (nreverse reads)))
```

Replace `%check-cycle` with:

```lisp
(defun %strata (relation reads graph others)
  "The stratum RELATION belongs to over READS and OTHERS' edges (spec
SS2): (values RULE-NAMES RELATIONS), both sorted.  The strongly
connected component of the relation graph, by a DFS from RELATION
forward and one over the reversed edges; the stratum's rules are every
rule in OTHERS deriving a component relation, plus this one."
  (let ((edges (list (cons relation reads)))
        (derivers (list (cons relation nil))))
    (dolist (o others)
      (let ((e (%edges o graph)))
        (when e
          (push e edges)
          (push (cons (car e) (rule-spec-name o)) derivers))))
    (labels ((succ (rel)
               (loop for e in edges when (string= (car e) rel)
                     append (if (eq (cdr e) :any) '() (cdr e))))
             (pred (rel)
               (loop for e in edges
                     when (and (listp (cdr e))
                               (member rel (cdr e) :test #'string=))
                       collect (car e)))
             (reach (start next)
               (let ((seen (list start)) (stack (list start)))
                 (loop while stack do
                   (dolist (n (funcall next (pop stack)))
                     (unless (member n seen :test #'string=)
                       (push n seen) (push n stack))))
                 seen)))
      (let* ((component (intersection (reach relation #'succ)
                                      (reach relation #'pred)
                                      :test #'string=))
             (names (loop for (rel . name) in derivers
                          when (and name
                                    (member rel component :test #'string=))
                            collect name)))
        (values (sort (copy-list names) #'string<)
                (sort (copy-list component) #'string<))))))
```

In `compile-rule`, replace the `(%check-cycle ...)` call with:

```lisp
          (let ((negative (%negative-reads body)))
            (when (eq negative :any)
              (%refuse spec "a negated claim/7 goal leaves its relation ~
unbound: bind the relation"))
            (multiple-value-bind (stratum relations)
                (%strata (getf parsed :relation) reads graph others)
              (let ((bad (intersection negative relations
                                       :test #'string=)))
                (when bad
                  (%refuse spec "negation over the rule's own stratum ~
~{~A~^, ~}: a NOT may read only an earlier stratum" bad)))
              (apply #'%make-compiled-rule
                     :spec spec
                     :vars (remove-duplicates
                            (append head-vars premise-vars))
                     :premise-vars premise-vars
                     :goals body :reads reads
                     :stratum (sort (cons name (copy-list stratum))
                                    #'string<)
                     :stratum-relations relations
                     parsed)))
```

(The `apply` replaces the existing one; `name` is bound at the top of `compile-rule`.) Keep the `:any` refusal for positive reads where it is today (`%check-cycle` raised it; move that `%refuse` into `compile-rule` before `%strata`). Delete `%check-cycle`.

Export `#:compiled-rule-stratum #:compiled-rule-stratum-relations` from `rules/package.lisp` under `;; compiling`.

- [ ] **Step 5: Run the suites; expect green**

Expected: rules suite green with the amended tests; count recorded. The guarded suite unchanged.

- [ ] **Step 6: Commit**

```bash
git add rules/compile.lisp rules/package.lisp tests/rules/compile-tests.lisp
git commit -F - <<'EOF'
feat(rules): strata replace the cycle refusal; unstratified negation refused (#333)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 2: Strata ordering in `run-rules`

**Files:**
- Modify: `rules/run.lisp` (`%dependency-order`, `run-rules`)
- Test: `tests/rules/run-tests.lisp` (append)

**Interfaces:**
- Consumes: `compiled-rule-stratum`, `compiled-rule-stratum-relations`, `compiled-rule-reads`.
- Produces: `(%strata-order compiled-rules)` → a list of strata, each a list of compiled rules in input order; strata ordered so one runs after every stratum deriving a relation it reads outside itself. `run-rules` iterates strata; for now each stratum's rules still go through `run-rule` one by one (Task 4 replaces that for recursive strata).

- [ ] **Step 1: Write the failing test**

Append to `tests/rules/run-tests.lisp`:

```lisp
(test run-rules-orders-a-recursive-stratum-after-its-base-producer
  "GH #333: a and b derive each other's reads (one stratum) and both
read z; z-maker must run first, and the stratum's two rules keep input
order.  Task 4 makes the stratum derive; here only the order is under
test, so bodies that find nothing are fine."
  (with-rules-graph (g)
    (seed g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head *head-x* :body *body-y-and-z*)
    (write-rule g :name "b" :version "1" :family "rt-claim"
                :head *head-y* :body *body-x*)
    (write-rule g :name "z-maker" :version "1" :family "rt-claim"
                :head *head-z* :body *web-hosts-body*)
    (let ((names (mapcar #'graph-db.rules:rule-report-rule-name
                         (graph-db.rules:run-rules g))))
      ;; Stored rules come back in index order, not write order, so
      ;; only the stratum boundary is asserted.
      (is (string= "z-maker" (first names)))
      (is (equal '("a" "b") (sort (copy-list (rest names)) #'string<))))))
```

Define `*body-y-and-z*` as `"(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h) (claim ?q rt-claim \"app\" \"web\" \"z\" \"host\" ?h)"` beside `*body-y*`.

- [ ] **Step 2: Run the rules suite; expect the new test to fail**

Expected: `run-rules` signals "no runnable rule ... a cycle the compiler should have refused" (the old order sees a cycle), or orders a and b before z-maker.

- [ ] **Step 3: Implement the strata order**

Replace `%dependency-order` with:

```lisp
(defun %strata-of (compiled-rules)
  "COMPILED-RULES grouped by stratum, input order kept inside and
between groups (first appearance)."
  (let ((groups '()))
    (dolist (c compiled-rules (nreverse (mapcar #'nreverse groups)))
      (let ((key (compiled-rule-stratum c)))
        (let ((g (find key groups :key (lambda (grp)
                                         (compiled-rule-stratum
                                          (first grp)))
                                  :test #'equal)))
          (if g
              (push c (cdr (last g)))
              (push (list c) groups)))))))

(defun %stratum-reads (stratum)
  "The relations STRATUM's rules read outside the stratum."
  (let ((own (compiled-rule-stratum-relations (first stratum))))
    (remove-duplicates
     (loop for c in stratum
           append (set-difference (compiled-rule-reads c) own
                                  :test #'string=))
     :test #'string=)))

(defun %strata-order (compiled-rules)
  "COMPILED-RULES as strata, sorted so a stratum runs after EVERY
stratum deriving a relation it reads outside itself (spec SS2); ties
keep input order.  Strata are the compiler's SCCs, so a pending set
always holds a ready one."
  (let ((pending (%strata-of compiled-rules))
        (done '()))
    (loop while pending do
      (let ((ready
              (find-if
               (lambda (s)
                 (every (lambda (r)
                          (notany
                           (lambda (o)
                             (and (not (eq o s))
                                  (member r (compiled-rule-stratum-relations
                                             (first o))
                                          :test #'string=)))
                           pending))
                        (%stratum-reads s)))
               pending)))
        (unless ready
          (error "RUN-RULES: no runnable stratum among ~S -- the ~
compiler's strata disagree with the reads."
                 (mapcar (lambda (s) (compiled-rule-stratum (first s)))
                         pending)))
        (setf pending (remove ready pending))
        (push ready done)))
    (nreverse done)))
```

`%strata-of`'s `(push c (cdr (last g)))` appends; write it as `(setf (cdr (last g)) (list c))` — the intent is append-in-place, keep input order. In `run-rules`, replace the `%dependency-order` loop with:

```lisp
    (dolist (stratum (%strata-order (nreverse compiled)))
      (dolist (c stratum)
        (push (run-rule graph (compiled-rule-spec c) :scope scope)
              reports)))
```

Delete `%dependency-order` and its `:any` error (the compiler refuses `:any` before a compiled rule exists).

- [ ] **Step 4: Run the rules suite; expect green**

Expected: the new test and `run-rules-runs-in-dependency-order-and-skips-the-disabled`, `run-rules-runs-a-reader-after-every-producer-of-its-relation` all pass; count recorded.

- [ ] **Step 5: Commit**

```bash
git add rules/run.lisp tests/rules/run-tests.lisp
git commit -F - <<'EOF'
feat(rules): run-rules orders strata, not rules (#333)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 3: The delta generator

**Files:**
- Modify: `rules/facts.lisp` (new `*rule-delta*`, `rule-delta/2`), `query/guard.lisp` (`*prolog-excluded-predicates*`), `package.lisp` (export `#:*rule-delta*` from `graph-db` next to `#:claim/7`)
- Test: `tests/rules/facts-tests.lisp` (append), `tests/query/guard-tests.lisp` (append; and the inventory tripwire if it pins the excluded list)

**Interfaces:**
- Produces: `graph-db:*rule-delta*` — NIL, or an `equal` hash table relation-string → list of claim nodes; `(rule-delta ?c "rel")` generates `?c` over that relation's list (bound `?c`: succeeds iff it is in the list). Never admitted from free text.

- [ ] **Step 1: Write the failing tests**

Append to `tests/rules/facts-tests.lisp`:

```lisp
(test rule-delta-generates-the-bound-delta-and-nothing-else
  "GH #333: the fixpoint's internal generator.  Unbound *RULE-DELTA*
answers nothing; bound, it yields exactly the listed claims of the
named relation, and filters a bound ?c by membership."
  (with-rules-graph (g)
    (seed g)
    (let* ((runs (claims-touching g 'rt-claim :host "h1" :role :subject
                                  :relation "runs"))
           (web (find "web" runs :key #'claim-object-key :test #'string=))
           (table (make-hash-table :test 'equal)))
      (is (= 2 (length runs)))
      (setf (gethash "runs" table) (list web))
      (is (null (select-flat (?c) (graph-db::rule-delta ?c "runs"))))
      (let ((graph-db:*rule-delta* table))
        (is (equal (list web)
                   (select-flat (?c) (graph-db::rule-delta ?c "runs"))))
        (is (null (select-flat (?c) (graph-db::rule-delta ?c "other"))))
        ;; A bound ?c is a membership test: web is in the delta, db not.
        (is (= 1 (select-count ()
                   (claim ?c rt-claim "host" "h1" "runs" "app" "web")
                   (graph-db::rule-delta ?c "runs"))))
        (is (= 0 (select-count ()
                   (claim ?c rt-claim "host" "h1" "runs" "app" "db")
                   (graph-db::rule-delta ?c "runs"))))
        ;; Composes with claim/7 as a filter: the delta claim's endpoints.
        (is (equal '("web")
                   (select-flat (?a)
                     (graph-db::rule-delta ?c "runs")
                     (claim ?c rt-claim "host" "h1" "runs" "app" ?a))))))))
```

Append to `tests/query/guard-tests.lisp`:

```lisp
(test rule-delta-is-withheld-from-free-text
  "GH #333: the fixpoint's generator is not a query surface."
  (with-query-graph (g)
    (signals graph-db.query:prolog-guard-error
      (q g "(rule-delta ?c \"x\")"))))
```

Check `q`'s error contract: if the guard's refusal is not `prolog-guard-error` at that call site, read `docs/guarded-query.md` "The condition contract" and use the type it names.

If a test in `tests/query/guard-tests.lisp` named like `prolog-functor-inventory-is-pinned` enumerates `*prolog-excluded-predicates*`, add `"RULE-DELTA"` to its pinned list.

- [ ] **Step 2: Run the suites; expect the new tests to fail**

- [ ] **Step 3: Implement**

In `rules/facts.lisp`, after `*claim-scope*`:

```lisp
(defvar *rule-delta* nil
  "NIL, or an EQUAL hash table relation -> list of claim nodes: the
claims a fixpoint round derived new, which RULE-DELTA/2 generates for
the next round (GH #333).  Bound by RUN-RULES around a recursive
stratum; never by a query.")

(def-global-prolog-functor rule-delta/2 (?c ?rel cont)
  "?C over *RULE-DELTA*'s claims of relation ?REL (a string); a bound ?C
succeeds only when it is one of them.  Nothing without a delta bound.
Withheld from free text (*PROLOG-EXCLUDED-PREDICATES*): a rule body
cannot name it, the fixpoint loop injects it (GH #333)."
  (let ((rel (var-deref ?rel))
        (c (var-deref ?c)))
    (when (and *rule-delta* (stringp rel))
      (let ((claims (gethash rel *rule-delta*)))
        (if (var-p c)
            (dolist (claim claims)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify ?c claim) (funcall cont))
                (undo-bindings old-trail)))
            (when (member c claims) (funcall cont)))))))
```

In `query/guard.lisp`, add `"RULE-DELTA"` to `*prolog-excluded-predicates*` with one comment line: `;; GH #333: the fixpoint's internal generator, injected by RUN-RULES.` Export `#:*rule-delta*` from `graph-db` in `package.lisp` beside `#:claim/7`.

- [ ] **Step 4: Run the suites; expect green**

Both suites green; counts recorded.

- [ ] **Step 5: Commit**

```bash
git add rules/facts.lisp query/guard.lisp package.lisp tests/rules/facts-tests.lisp tests/query/guard-tests.lisp
git commit -F - <<'EOF'
feat(rules): rule-delta/2, the fixpoint's internal generator (#333)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 4: The fixpoint loop

**Files:**
- Modify: `rules/run.lisp` (`rule-report` struct, `*rules-max-rounds*`, `%solutions` takes goals, `%desired` takes goals, new `%variants`, `%round`, `%run-stratum`, `run-rule`, `run-rules`), `rules/package.lisp` (exports `#:rule-report-rounds #:rule-report-stratum #:*rules-max-rounds*`)
- Test: `tests/rules/fixpoint-tests.lisp` (new), `graph-db.asd` (`graph-db/rules-test` components: `(:file "fixpoint-tests")` after `"run-tests"`)

**Interfaces:**
- Consumes: Task 1's `compiled-rule-stratum`/`-stratum-relations`; Task 2's `%strata-order`; Task 3's `graph-db:*rule-delta*` and `rule-delta/2`.
- Produces: `rule-report` gains `(rounds 1)` and `(stratum '())`; `*rules-max-rounds*` default 1000; `run-rules` runs a recursive stratum through `%run-stratum`; `run-rule` on a rule of a recursive stratum runs the stratum and returns that rule's report.

- [ ] **Step 1: Write the failing tests**

Create `tests/rules/fixpoint-tests.lisp`:

```lisp
;;;; tests/rules/fixpoint-tests.lisp -- recursive rules under fixpoint
;;;; iteration (GH #333, spec SS3).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defun link (g from to)
  "A \"next\" rt-claim from node FROM to node TO, producer \"seed\"."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :node
                          :subject-key from :relation "next"
                          :object-namespace :node :object-key to
                          :producer "seed" :standing :observed)))

(defparameter *tc-base-head*
  "(claim ?c rt-claim \"node\" ?a \"reaches\" \"node\" ?b)")
(defparameter *tc-base-body*
  "(claim ?p rt-claim \"node\" ?a \"next\" \"node\" ?b)")
(defparameter *tc-step-body*
  "(claim ?p rt-claim \"node\" ?a \"next\" \"node\" ?m)
   (claim ?q rt-claim \"node\" ?m \"reaches\" \"node\" ?b)")

(defun write-closure (g)
  "The two-rule transitive closure of \"next\" into \"reaches\"."
  (write-rule g :name "tc-base" :version "1" :family "rt-claim"
              :head *tc-base-head* :body *tc-base-body*)
  (write-rule g :name "tc-step" :version "1" :family "rt-claim"
              :head *tc-base-head* :body *tc-step-body*))

(defun reaches (g)
  "The current \"reaches\" pairs, sorted."
  (sort (mapcar (lambda (c) (cons (claim-subject-key c)
                                  (claim-object-key c)))
                (remove-if-not
                 #'claim-current-p
                 (claims-touching g 'rt-claim :node "a" :role :subject
                                  :relation "reaches")))
        #'string< :key #'cdr))

(test a-transitive-closure-over-a-cycle-terminates-with-the-fixpoint
  "a -> b -> c -> a, and c -> d: 12 reaches pairs.  A top-down rule
would loop; the fixpoint terminates."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "a") (link g "c" "d")
    (write-closure g)
    (let* ((reports (graph-db.rules:run-rules g))
           (base (report-named "tc-base" reports))
           (step (report-named "tc-step" reports)))
      (is (every (lambda (r) (eq :derived (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (equal '("tc-base" "tc-step")
                 (graph-db.rules:rule-report-stratum base)))
      (is (= (graph-db.rules:rule-report-rounds base)
             (graph-db.rules:rule-report-rounds step)))
      (is (<= 3 (graph-db.rules:rule-report-rounds step) 5))
      ;; From each of a b c: a b c d (the cycle plus d); d reaches
      ;; nothing.  12 pairs.
      (is (= 12 (+ (graph-db.rules:rule-report-derived base)
                   (graph-db.rules:rule-report-derived step))))
      (is (equal '(("a" . "a") ("a" . "b") ("a" . "c") ("a" . "d"))
                 (reaches g))))))

(test the-semi-naive-delta-derives-what-naive-re-evaluation-derives
  "The delta variants are an optimisation, never a different answer:
the identity set equals what a full re-evaluation each round gives."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d") (link g "d" "b")
    (write-closure g)
    (graph-db.rules:run-rules g)
    (let ((semi (reaches g)))
      (let ((graph-db.rules::*rules-naive-rounds* t))
        (graph-db.rules:run-rules g))
      (is (equal semi (reaches g)))
      (is (= 4 (length semi))))))

(test rounds-count-the-chain-and-a-round-0-claim-survives
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-closure g)
    (let ((reports (graph-db.rules:run-rules g)))
      ;; round 0: a-b b-c c-d (base) and a-c b-d (step over base);
      ;; round 1: a-d; round 2: nothing.  3 rounds.
      (is (= 3 (graph-db.rules:rule-report-rounds
                (report-named "tc-step" reports))))
      (is (equal '(("a" . "b") ("a" . "c") ("a" . "d")) (reaches g))))))

(test a-premise-retracted-between-runs-sweeps-exactly-the-stale-closure
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-closure g)
    (graph-db.rules:run-rules g)
    (is (= 3 (length (reaches g))))
    (with-transaction ((graph-db::transaction-manager g))
      (retract-claim (first (claims-touching g 'rt-claim :node "c"
                                             :role :subject
                                             :relation "next"))))
    (let ((reports (graph-db.rules:run-rules g)))
      (is (equal '(("a" . "b") ("a" . "c")) (reaches g)))
      (is (= 1 (+ (graph-db.rules:rule-report-swept
                   (report-named "tc-base" reports))
                  (graph-db.rules:rule-report-swept
                   (report-named "tc-step" reports))))))))

(test run-rule-on-one-rule-of-a-stratum-runs-the-stratum
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (write-closure g)
    (let ((report (graph-db.rules:run-rule g "tc-step")))
      (is (string= "tc-step" (graph-db.rules:rule-report-rule-name report)))
      (is (equal '("tc-base" "tc-step")
                 (graph-db.rules:rule-report-stratum report)))
      (is (equal '(("a" . "b") ("a" . "c")) (reaches g))))))

(test the-rounds-cap-refuses-naming-the-count
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-closure g)
    (let* ((graph-db.rules:*rules-max-rounds* 1)
           (reports (graph-db.rules:run-rules g)))
      (is (every (lambda (r) (eq :refused (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (search "1" (refusal-text (report-named "tc-step" reports))))
      ;; Single-store: one transaction, so nothing landed.
      (is (null (reaches g))))))

(test a-budget-refusal-in-a-later-round-leaves-the-previous-derivation
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (write-closure g)
    (graph-db.rules:run-rules g)
    (is (= 2 (length (reaches g))))
    (link g "c" "d")
    (let* ((graph-db.rules:*rules-max-inferences* 1)
           (reports (graph-db.rules:run-rules g)))
      (is (eq :refused (graph-db.rules:rule-report-outcome
                        (report-named "tc-step" reports))))
      (is (eq :budget (refusal-tag (report-named "tc-step" reports))))
      (is (= 2 (length (reaches g)))))))
```

`retract-claim`: use whatever `graph-db.spacetime` exports to close a claim's transaction period (grep `tests/rules/run-tests.lisp` for the retract helper the existing `a-retracted-derived-claim-stays-retracted-on-rerun` test uses, and call that). Fix the first test's derived count and pairs by reasoning from the seed before running: from `a`, `reaches` is `b c a d` — write the expected sorted pairs accordingly (`("a" . "a") ("a" . "b") ("a" . "c") ("a" . "d")`) and total pairs 12.

Add `(:file "fixpoint-tests")` after `(:file "run-tests")` in `graph-db/rules-test`.

- [ ] **Step 2: Run the rules suite; expect the new tests to fail**

Expected: `rule-report-rounds` undefined and the closure not derived.

- [ ] **Step 3: Implement**

In `rules/run.lisp`:

```lisp
(defvar *rules-max-rounds* 1000
  "Rounds a recursive stratum may run before it is refused naming the
count -- a guard on the delta logic, not a budget (GH #333).")

(defvar *rules-naive-rounds* nil
  "True to re-evaluate every rule in full each round instead of the
delta variants: the test reference for the semi-naive answer.")
```

Extend `rule-report`: add `(rounds 1) (stratum '())` slots and one docstring sentence: "ROUNDS is the fixpoint rounds a recursive stratum ran (1 otherwise), STRATUM the names it ran with (GH #333)."

Make `%solutions` and `%desired` take the goals to run: `(defun %solutions (compiled graph report &optional (goals (compiled-rule-goals compiled)))` passing `goals` to `run-query-goals`; `(defun %desired (compiled graph report &optional (goals (compiled-rule-goals compiled)))` passing them to `%solutions`. Nothing else in them changes.

Add:

```lisp
(defun %variants (compiled)
  "One goal list per recursive CLAIM/7 goal of COMPILED: the goals with
a RULE-DELTA/2 generator for that goal's ?c inserted before it, so the
goal filters the previous round's new claims (spec SS3).  NIL when the
rule has no recursive goal."
  (let ((own (compiled-rule-stratum-relations compiled))
        (goals (compiled-rule-goals compiled))
        (variants '()))
    (loop for goal in goals
          for i from 0
          when (and (%engine-goal-p goal "CLAIM" 7)
                    (stringp (sixth goal))
                    (member (sixth goal) own :test #'string=)
                    (%variable-p (second goal)))
            do (push (append (subseq goals 0 i)
                             (list (list 'graph-db::rule-delta
                                         (second goal) (sixth goal)))
                             (subseq goals i))
                     variants))
    (nreverse variants)))

(defun %merge-desired (into into-order table order)
  "TABLE/ORDER merged into INTO/INTO-ORDER by dedupe key, premises
merged as within one solution set.  => (values INTO INTO-ORDER)."
  (dolist (key order)
    (let ((entry (gethash key table))
          (have (gethash key into)))
      (if have
          (setf (cdr have) (%merge-premise-refs (cdr have) (cdr entry)))
          (progn (setf (gethash key into) entry)
                 (setf into-order (nconc into-order (list key)))))))
  (values into into-order))

(defun %construct-new (compiled graph report desired order seen)
  "Construct the claims of DESIRED/ORDER whose key is not in SEEN,
marking SEEN; => the new claims.  Round writes: keep and construct
only, the sweep waits for the fixpoint (spec SS3)."
  (let ((ctor (%constructor (compiled-rule-family compiled)
                            (compiled-rule-unary-p compiled)))
        (producer (rule-producer (rule-spec-name
                                  (compiled-rule-spec compiled))))
        (version (rule-spec-version (compiled-rule-spec compiled)))
        (new '()))
    (dolist (key order (nreverse new))
      (unless (gethash key seen)
        (setf (gethash key seen) t)
        (let ((claim (apply ctor :graph graph
                            :relation (compiled-rule-relation compiled)
                            :producer producer :rule-version version
                            :standing :inferred
                            (car (gethash key desired)))))
          (incf (rule-report-derived report))
          (push claim new))))))
```

`SEEN` for a rule starts as the keys of the producer's committed claims (so a claim already standing is not constructed): build it from `claims-by-producer` with `%existing-key`, exactly as `%reconcile-claims` does, once per rule before round 0.

The stratum runner:

```lisp
(defun %run-stratum (graph stratum scope)
  "STRATUM (compiled rules sharing a stratum) to its fixpoint (spec
SS3): rounds of evaluation, round 0 in full and later rounds over the
delta variants, claims constructed per round so the next round reads
them, the sweep and provenance once at the end.  Single-store: one
transaction, so a refusal leaves the previous derivation standing.
Cross-store: evaluation under snapshots and one transaction per round,
since a foreign read inside a transaction is refused (GH #53); a
refusal then leaves earlier rounds committed, which the report says.
=> one RULE-REPORT per rule, input order."
  (let* ((foreign (rest scope))
         (reports (mapcar (lambda (c)
                            (%make-rule-report
                             :rule-name (rule-spec-name
                                         (compiled-rule-spec c))
                             :version (rule-spec-version
                                       (compiled-rule-spec c))
                             :stratum (compiled-rule-stratum c)))
                          stratum))
         (start (get-internal-real-time))
         (desired (mapcar (lambda (c) (declare (ignore c))
                            (make-hash-table :test 'equal))
                          stratum))
         (orders (mapcar (lambda (c) (declare (ignore c)) '()) stratum))
         (seen (mapcar (lambda (c)
                         (let ((h (make-hash-table :test 'equal)))
                           (dolist (x (graph-db.spacetime:claims-by-producer
                                       graph
                                       (graph-db.spacetime:claim-family-parent
                                        (compiled-rule-family c))
                                       (rule-producer
                                        (rule-spec-name
                                         (compiled-rule-spec c)))))
                             (setf (gethash (%existing-key
                                             x (compiled-rule-family c))
                                            h)
                                   t))
                           h))
                       stratum))
         (delta nil)
         (round 0))
    (labels ((evaluate (c report goals)
               (let ((graph-db::*claim-scope* scope)
                     (graph-db:*rule-delta* delta))
                 (%desired c graph report goals)))
             (one-round ()
               ;; => the new claims of this round, keyed by relation.
               (let ((next (make-hash-table :test 'equal)) (any nil))
                 (loop for c in stratum
                       for report in reports
                       for d in desired
                       for i from 0
                       do (let ((goal-lists
                                  (cond ((zerop round)
                                         (list (compiled-rule-goals c)))
                                        (*rules-naive-rounds*
                                         (list (compiled-rule-goals c)))
                                        (t (%variants c)))))
                            (dolist (goals goal-lists)
                              (multiple-value-bind (table order)
                                  (if foreign
                                      (%under-snapshots
                                       scope (lambda ()
                                               (evaluate c report goals)))
                                      (evaluate c report goals))
                                (multiple-value-bind (d2 o2)
                                    (%merge-desired d (nth i orders)
                                                    table order)
                                  (declare (ignore d2))
                                  (setf (nth i orders) o2))))
                            (let ((new (%construct-new
                                        c graph report d (nth i orders)
                                        (nth i seen))))
                              (when new
                                (setf any t)
                                (setf (gethash (compiled-rule-relation c)
                                               next)
                                      (append (gethash
                                               (compiled-rule-relation c)
                                               next)
                                              new))))))
                 (values next any)))
             (finish ()
               (loop for c in stratum
                     for report in reports
                     for d in desired
                     for order in orders
                     do (let ((standing (%reconcile-claims
                                         c graph report d order)))
                          (%reconcile-provenance c graph d standing))))
             (run ()
               (loop
                 (when (>= round *rules-max-rounds*)
                   (error 'rule-run-refusal :tag :rounds
                          :text (format nil "no fixpoint after ~D rounds"
                                        *rules-max-rounds*)))
                 (multiple-value-bind (next any)
                     (if foreign
                         (graph-db:with-transaction (:graph graph)
                           (one-round))
                         (one-round))
                   (setf delta next)
                   (incf round)
                   (unless any (return))))
               (if foreign
                   (graph-db:with-transaction (:graph graph) (finish))
                   (finish))))
      (handler-case
          (if foreign
              (run)
              (graph-db:with-transaction (:graph graph) (run)))
        (error (c)
          (let ((tag (typecase c
                       (rule-run-refusal (rule-run-refusal-tag c))
                       (graph-db:prolog-permission-error :rule)
                       (graph-db:prolog-error :budget)
                       (graph-db:constraint-violation
                        (%violation-family c))
                       (t :rule)))
                (text (if (typep c 'rule-run-refusal)
                          (rule-run-refusal-text c)
                          (princ-to-string c))))
            (unless (or (typep c 'rule-run-refusal)
                        (typep c 'graph-db:prolog-error)
                        (typep c 'graph-db:constraint-violation)
                        (typep c 'graph-db:query-precondition-error)
                        (typep c 'graph-db.spacetime:missing-claim-identity-component))
              (error c))
            (dolist (r reports)
              (setf (rule-report-outcome r) :refused
                    (rule-report-derived r) 0
                    (rule-report-kept r) 0
                    (rule-report-swept r) 0
                    (rule-report-refusals r)
                    (list (cons tag text)))))))
      (dolist (r reports reports)
        (setf (rule-report-rounds r) round
              (rule-report-elapsed r)
              (/ (- (get-internal-real-time) start)
                 (float internal-time-units-per-second 1.0d0)))))))
```

Correctness notes the implementer must keep: (a) `%reconcile-claims` in `finish` reads `claims-by-producer`, which inside the single-store transaction overlays this run's constructions (GH #324; on the cross-store path they are committed), so every claim constructed this run is found standing and counted as `kept`, and none is constructed twice. `kept` must keep today's meaning, previous-derivation claims re-derived: after `%reconcile-claims` returns in `finish`, do `(decf (rule-report-kept report) (rule-report-derived report))`. Add to `rounds-count-the-chain-and-a-round-0-claim-survives`: `(is (= 3 (+ derived-base derived-step)))`, `(is (= 0 (+ kept-base kept-step)))`, `(is (= 0 (+ swept-base swept-step)))` using `rule-report-derived`/`-kept`/`-swept` of the two reports. (b) On the cross-store path each round's `one-round` is its own transaction, and `%desired`'s premise refs are read under the snapshots as today. (c) The refusal `handler-case` re-signals anything that is not one of the refusal classes `run-rule` already catches.

`run-rules`: for each stratum from `%strata-order`, if `(rest stratum)` or the single rule is recursive (`(intersection (compiled-rule-reads c) (compiled-rule-stratum-relations c) :test #'string=)`), push `(%run-stratum graph stratum scope)`'s reports; else `run-rule` as today. `run-rule`: after compiling, if the compiled rule is recursive, compile every other rule in its stratum (`rules-in-scope`, filtered by `(member name (compiled-rule-stratum compiled))`), call `%run-stratum` on them in `rules-in-scope` order, and return the report named for this rule.

Export `#:rule-report-rounds #:rule-report-stratum #:*rules-max-rounds*` from `graph-db.rules`.

- [ ] **Step 4: Run the suites; expect green**

Both suites green; every earlier run test still passes (non-recursive strata take the old path). Record counts. Where a fixpoint test's expected count disagrees with the observed derivation, print the observed `reaches` pairs, reason from the seed, and fix the test only if the observed set is the true closure; otherwise the code is wrong.

- [ ] **Step 5: Commit**

```bash
git add rules/run.lisp rules/package.lisp tests/rules/fixpoint-tests.lisp graph-db.asd
git commit -F - <<'EOF'
feat(rules): recursive strata run to a semi-naive fixpoint (#333)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 5: Cross-store fixpoint

**Files:**
- Test: `tests/rules/scope-tests.lisp` (append)

**Interfaces:**
- Consumes: Task 4's `%run-stratum` cross-store path; the suite's `with-two-stores (a b)` and `seed-b`.

- [ ] **Step 1: Write the test**

Append to `tests/rules/scope-tests.lisp`:

```lisp
(test a-closure-whose-base-lives-in-a-foreign-store-reaches-the-fixpoint
  "GH #333, spec SS3: the base relation is in B, the closure is derived
into A; each round evaluates under snapshots and commits on A."
  (with-two-stores (a b)
    (with-transaction ((graph-db::transaction-manager b))
      (dolist (pair '(("a" . "b") ("b" . "c") ("c" . "d")))
        (make-rt-claim-binary :graph b :subject-namespace :node
                              :subject-key (car pair) :relation "next"
                              :object-namespace :node
                              :object-key (cdr pair)
                              :producer "seed" :standing :observed)))
    (write-closure a)
    (let ((reports (graph-db.rules:run-rules a :scope (list a b))))
      (is (every (lambda (r) (eq :derived (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (= 3 (graph-db.rules:rule-report-rounds
                (report-named "tc-step" reports))))
      (is (equal '(("a" . "b") ("a" . "c") ("a" . "d")) (reaches a)))
      ;; Provenance names the foreign premises by store.
      (let ((p (graph-db.rules:premises-of
                a (first (claims-touching a 'rt-claim :node "a"
                                          :role :subject
                                          :relation "reaches"))
                :scope (list a b))))
        (is (plusp (length p)))))))
```

`write-closure` and `reaches` are defined in `fixpoint-tests.lisp`, which loads before `scope-tests.lisp` in the asd order only if you move `(:file "fixpoint-tests")` before `(:file "scope-tests")`; do that.

- [ ] **Step 2: Run the rules suite; expect green (or the one failure to fix)**

If the cross-store path fails, the likely cause is `%under-snapshots` composing with `graph-db:*rule-delta*` bound outside it (fine) or the round transaction not seeing the previous round's commit (a snapshot acquired before it); fix in `%run-stratum` so each round's evaluation snapshots are acquired after the previous round's commit (they are, if `one-round` acquires per evaluation).

- [ ] **Step 3: Commit**

```bash
git add tests/rules/scope-tests.lisp graph-db.asd
git commit -F - <<'EOF'
test(rules): a cross-store closure reaches its fixpoint (#333)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

---

### Task 6: Docs

**Files:**
- Modify: `docs/rules.md` (new "## Recursive rules" section before "## `run-rules`", and the `run-rules` section's cycle sentence), `docs/superpowers/specs/2026-09-04-rules-as-producers-design.md` (§6 and §12 pointers), `docs/superpowers/specs/2026-09-06-recursive-rules-fixpoint-design.md` (§3 transaction ruling), `CHANGELOG.md`

- [ ] **Step 1: docs/rules.md**

Insert before `## \`run-rules\``:

```markdown
## Recursive rules

A rule may read its own head relation, directly or through other
rules (GH #333). The compiler no longer refuses the cycle: it computes
**strata**, the strongly connected components of the relation
dependency graph, and `compiled-rule-stratum` /
`compiled-rule-stratum-relations` say which rules and relations a rule
runs with. Two things are still refused at compile, naming the rule
and the relation: a `not` whose goal reads a relation in the rule's
own stratum (unstratified negation has no fixpoint; a `not` over an
earlier stratum is fine), and a `claim/7` goal that leaves its
relation unbound (it would read every relation, its own included).

`run-rules` runs strata in dependency order. A recursive stratum runs
in **rounds**: round 0 evaluates every rule in full and constructs the
claims it derives; each later round evaluates, per rule and per
recursive `claim/7` goal, a variant of the body in which that goal
reads only the claims derived new in the previous round (an internal
generator a rule body cannot name), and constructs what is new by
identity; the stratum stops at a round that derives nothing new. That
is semi-naive evaluation: the answer is the same as re-evaluating
everything each round, at a fraction of the cost. The sweep of claims
no longer derived happens once, at the fixpoint, so a claim derived in
an early round is never swept by a later one.

- **Single store**: the whole stratum is one transaction, so a refusal
  in any round -- the budget, a commit constraint, the rounds cap
  `*rules-max-rounds*` (default 1000) -- leaves the previous derivation
  standing, as for any refused rule.
- **Cross-store scope**: each round evaluates under snapshots and
  commits on the own store, because a foreign read inside a
  transaction is refused (GH #53) and a later round must see the
  earlier ones; a refusal in round *n* leaves rounds before it
  committed, and the report says so.
- `rule-report-rounds` is the rounds run (1 for a non-recursive rule),
  `rule-report-stratum` the rules it ran with. A refusal in a stratum
  is reported on the rule it happened in; the others report `:refused`
  naming it.
- `run-rule` on one rule of a recursive stratum runs the whole stratum
  and returns that rule's report.

`select` and the guarded query surface are unchanged: a recursive
`<-` predicate there still runs top-down under the resource bounds
(tabling for that is kraison/vivace-graph#122).
```

In the `run-rules` section, replace "Cycles were refused at compile, so the order always exists" with "Strata are the compiler's components, so the order always exists".

- [ ] **Step 2: Spec pointers and the ruling**

In `2026-09-04-rules-as-producers-design.md` §6 replace the "**Recursion is refused** until #122" bullet's first sentence with "**Recursion is a stratum** since GH #333: see `2026-09-06-recursive-rules-fixpoint-design.md`; what is refused is unstratified negation and an unbound relation." In §12 replace "Tabling and recursive rules (#122; slice 4 follows it)" with "Tabling for `select` (#122); recursive rules are slice 4 (#333)".

In `2026-09-06-recursive-rules-fixpoint-design.md` §3, replace the "**Budget and refusals**" paragraph's cross-store sentence with the single-store/cross-store transaction ruling from this plan's header, verbatim in substance: single-store strata are one transaction; cross-store strata commit per round and a refusal leaves earlier rounds committed.

- [ ] **Step 3: CHANGELOG**

Under `## [Unreleased]` / `### Added`, in the entry style already there:

```markdown
- **Recursive rules** (#333, Phase 3 unit 1 of #122): a rule may read
  its own head relation. `compile-rule` computes strata (SCCs of the
  relation dependency graph) instead of refusing a cycle, and refuses
  only unstratified negation and an unbound relation; `run-rules`
  runs a recursive stratum to a semi-naive fixpoint -- rounds over the
  previous round's new claims through the internal `rule-delta/2`
  generator, withheld from free text -- and sweeps once at the
  fixpoint. `rule-report` gains `rounds` and `stratum`;
  `*rules-max-rounds*` caps a stratum. Single-store strata are one
  transaction; cross-store strata commit per round.
```

- [ ] **Step 4: Column check and commit**

```bash
awk 'length > 80 {print FILENAME":"FNR}' rules/compile.lisp rules/run.lisp rules/facts.lisp tests/rules/fixpoint-tests.lisp tests/rules/compile-tests.lisp tests/rules/run-tests.lisp tests/rules/scope-tests.lisp tests/rules/facts-tests.lisp
git add docs/rules.md docs/superpowers/specs/2026-09-04-rules-as-producers-design.md docs/superpowers/specs/2026-09-06-recursive-rules-fixpoint-design.md CHANGELOG.md
git commit -F - <<'EOF'
docs(rules): recursive rules, strata, rounds and the transaction ruling (#333)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
EOF
```

Expected: no over-80 line among the lines this branch added.

---

### Task 7: Measurement on ma-dev (after the engine PR merges; needs host access)

**Gate:** the engine PR merged to `experiment`; ma-dev's engine checkout at that merge; a mine-action session on odm with the knowledge store and the spine open read-write. This task is a runbook, executed by Kevin or an agent with host access, and its output is a comment on #333.

- [ ] **Step 1: Two closure rules** in the private tenant (mine-action), written as stored rules on the respective stores. Domain-neutral shape, filled with the tenant's family and relation names:
  - Knowledge store: family the tenant's document-claim family; base relation the supersession relation; head relation `"supersedes-transitively"` — the two-rule closure of Task 4's `write-closure`, endpoints the tenant's document namespace.
  - Spine: family the spine claim family; base relation the containment relation; head relation `"contains-transitively"`.
- [ ] **Step 2: Three runs each** of `run-rules` on the store, `*rules-max-inferences*` and `*rules-timeout*` at the tenant's usual values, recording per run: `rounds`, `derived`, `kept`, `swept`, `elapsed`, and whether the knowledge store's closure hit a cycle (a self-pair derived).
- [ ] **Step 3: Record** the third run's numbers in the tenant's runbook, and a domain-neutral summary (host, claim counts per store, rounds, elapsed) as the closing comment on #333 with the merge SHA. If a delta round costs more than the naive round for the same store (`*rules-naive-rounds*` t, one extra run), record it as a finding on #333.
