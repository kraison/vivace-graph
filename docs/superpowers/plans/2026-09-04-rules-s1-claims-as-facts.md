# rules S1: claims as Prolog facts — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A new subsystem `graph-db/rules` whose first content is a set of global Prolog functors that make claims queryable as facts, index-backed, admitted by the guard, so `run-guarded-prolog` and later the rule compiler can read claims by subject, object or producer.

**Architecture:** `rules/facts.lisp` registers `claim/7` and six `claim-*` filters with `def-global-prolog-functor`, generating from the family's `claim-subject`, `claim-object` and `claim-producer` indexes by the same trail discipline as `find-by-slot/4`. Registration is enough for the guard's whitelist, which enumerates `*prolog-global-functors*`. Namespaces cross the functor boundary as strings, resolved with `find-symbol` in `KEYWORD` so a query cannot grow that package.

**Tech Stack:** SBCL 2.6.6, ASDF, FiveAM, `graph-db/spacetime`, `graph-db/query`, `local-time`, `cl-temporal-extent`.

**Spec:** `docs/superpowers/specs/2026-09-04-rules-as-producers-design.md` §3, §4, §11 (#304, sub-issue #330).

## Global Constraints

- Lisp: spaces only; hard 80-column limit; terse comments pointing at the spec section or #330.
- `graph-db/rules` depends on `graph-db/spacetime` and `graph-db/query` only; no web package.
- **No interning of query-derived text.** A namespace string resolves with `find-symbol` in `KEYWORD`; a name no claim was ever recorded under yields no solutions and interns nothing. Family names are symbols the guard already canonicalised (or, from Lisp, the parent symbol).
- Every generator follows `find-by-slot/4`'s discipline (`index.lisp:1078`): `var-deref` the arguments, `(let ((old-trail (fill-pointer *trail*))) (when (unify …) (funcall cont)) (undo-bindings old-trail))` per candidate.
- Retracted claims are generated unless `claim-current/1` filters them, matching `claims-touching`'s default.
- Suites run only in a subprocess from the worktree with the worktree first on `asdf:*central-registry*`: `sbcl --dynamic-space-size 4096 --non-interactive --eval '(push #p"<worktree>/" asdf:*central-registry*)' --eval '(ql:quickload :graph-db/rules-test :silent t)' --eval '(asdf:test-system :graph-db/rules-test)'`. Always target the `-test` system; read `Did N checks`. Run every suite in the foreground; never background a run.
- Commit trailers: `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` and `Claude-Session: https://claude.ai/code/session_016XhUGNmKWzsBV8PftSVfVo`. No push until the branch is complete (the spec commit 4053d9a is already on `feat/rules`).

**Spec amendment recorded here (§4):** endpoint namespaces are **strings** on the wire (`"repo"`), not keywords: the guard's screen refuses every colon, and a string is data to it. `claim/7` binds an unbound namespace argument to the keyword's downcased name as a string, so results are symmetric with inputs.

---

## File structure

| file | responsibility |
|---|---|
| `rules/package.lisp` | package `graph-db.rules`; exports nothing in S1 (the functors are global Prolog names) |
| `rules/facts.lisp` | `claim/7` and the `claim-*` filters |
| `graph-db.asd` | systems `graph-db/rules`, `graph-db/rules-test` |
| `tests/rules/{package,suite,facts-tests}.lisp` | the suite |
| `docs/rules.md` | S1 section: the functor table, namespace convention, examples |
| `CHANGELOG.md`, `docs/ci.md`, `.github/workflows/test.yml` | entry, suite list, lane |

---

### Task 1: Scaffold — systems, package, test fixture, first generator

**Files:**
- Create: `rules/package.lisp`, `rules/facts.lisp`, `tests/rules/package.lisp`, `tests/rules/suite.lisp`, `tests/rules/facts-tests.lisp`
- Modify: `graph-db.asd`

**Interfaces:**
- Produces: system `graph-db/rules`; test system `graph-db/rules-test` with `run-rules-tests`; fixture macro `with-rules-graph ((g) &body)`; families `rt-claim` (non-temporal) and `rtt-claim` (`:temporal t`) on graph `:graph-db-rules-test`; helper `(seed g)` writing the claims listed below; internal helpers in `rules/facts.lisp`: `%namespace-keyword string → keyword-or-nil`, `%namespace-string keyword → string`, `%yield node-var node cont`.

- [ ] **Step 1: Systems**

Append to `graph-db.asd` after `graph-db/query-test`:

```lisp
;; Rules as versioned producers (GH #304): S1 makes claims Prolog facts.
(defsystem graph-db/rules
  :name "VivaceGraph rules"
  :description "Claims as Prolog facts; later, rules as versioned
producers.  docs/rules.md; GH #304."
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "4.0.1"
  :depends-on (:graph-db/spacetime :graph-db/query)
  :pathname "rules/"
  :serial t
  :components ((:file "package") (:file "facts"))
  :in-order-to ((test-op (test-op :graph-db/rules-test))))

(defsystem graph-db/rules-test
  :name "VivaceGraph rules test suite"
  :depends-on (:graph-db/rules :graph-db/test-scratch :fiveam)
  :pathname "tests/rules/"
  :serial t
  :components ((:file "package") (:file "suite") (:file "facts-tests"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :graph-db/rules-test :run-rules-tests)
               (error "graph-db rules tests failed."))))
```

- [ ] **Step 2: Packages**

```lisp
;;;; rules/package.lisp -- rules as versioned producers (GH #304).
(defpackage #:graph-db.rules
  (:use #:cl)
  (:import-from #:graph-db.spacetime
                #:claim-family #:claim-family-parent #:claim-family-binary
                #:claim-family-temporal-p #:claim-subject-namespace
                #:claim-subject-key #:claim-object-namespace
                #:claim-object-key #:claim-relation #:claim-producer
                #:claim-standing #:claim-rule-version #:claim-current-p
                #:claim-extent #:unknown-claim-family)
  (:export))
```

```lisp
;;;; tests/rules/package.lisp
(defpackage #:graph-db/rules-test
  (:use #:cl #:fiveam #:graph-db.spacetime)
  (:import-from #:graph-db #:make-graph #:close-graph #:with-transaction
                #:select #:select-flat #:select-count)
  (:export #:run-rules-tests #:rules-suite))
```

- [ ] **Step 3: Suite and fixture**

```lisp
;;;; tests/rules/suite.lisp
(in-package #:graph-db/rules-test)

(def-suite rules-suite :description "graph-db/rules (GH #304).")

(defun run-rules-tests ()
  (log:config :error)
  (let* ((system-dir (graph-db-test-scratch:make-scratch-directory
                      "graph-db-rules-sys"))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'rules-suite)))
           (explain! results)
           (results-status results))
      (graph-db-test-scratch:cleanup-scratch-run))))

(defparameter *graph-name* :graph-db-rules-test)

(def-claim-classes rt-claim :graph-db-rules-test)
(def-claim-classes rtt-claim :graph-db-rules-test :temporal t)

(defmacro with-rules-graph ((g) &body body)
  `(let* ((dir (graph-db-test-scratch:make-scratch-directory "graph-db-rules"))
          (,g (make-graph *graph-name* (namestring dir)
                          :buffer-pool-size 1000)))
     (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g)))))

(defun ts (y m d)
  (local-time:encode-timestamp 0 0 0 0 d m y
                               :timezone local-time:+utc-zone+))

(defun interval (from to)
  (make-interval (exact-bound from) (exact-bound to)
                 :semantics :validity :standing :asserted))

(defun seed (g)
  "Four rt-claims and two rtt-claims.  Returns nothing; tests query."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h1"
                          :relation "runs" :object-namespace :app
                          :object-key "web" :producer "scan-a"
                          :standing :observed)
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h1"
                          :relation "runs" :object-namespace :app
                          :object-key "db" :producer "scan-a"
                          :standing :observed)
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h2"
                          :relation "runs" :object-namespace :app
                          :object-key "web" :producer "scan-b"
                          :standing :observed)
    (make-rt-claim-unary :graph g :subject-namespace :host :subject-key "h2"
                         :relation "reachable" :producer "scan-b"
                         :standing :inferred)
    (make-rtt-claim-binary :graph g :subject-namespace :app :subject-key "web"
                           :relation "version" :object-namespace :ver
                           :object-key "1" :producer "scan-a"
                           :standing :observed
                           :extent (interval (ts 2026 1 1) (ts 2026 3 31)))
    (make-rtt-claim-binary :graph g :subject-namespace :app :subject-key "web"
                           :relation "version" :object-namespace :ver
                           :object-key "2" :producer "scan-a"
                           :standing :observed
                           :extent (interval (ts 2026 4 1) (ts 2026 12 31)))))
```

Check the constructor keyword names and `make-interval`'s signature against `tests/spacetime/claim-identity-tests.lisp` and `tests/spacetime/temporal-tests.lisp` before relying on them, and copy those files' spelling.

- [ ] **Step 4: The first failing test**

```lisp
;;;; tests/rules/facts-tests.lisp
(in-package #:graph-db/rules-test)
(in-suite rules-suite)

(test claim-generates-from-the-subject-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (?o) (claim ?c rt-claim "host" "h1" "runs" "app" ?o))))
      (is (equal '("db" "web") (sort (mapcar #'first rows) #'string<))))))
```

`select` here is the raw macro from Lisp in a package that uses `graph-db`... it does not: `graph-db/rules-test` imports `select` but its heads (`claim`) must resolve — after #329 a head resolves in its own package first, then `graph-db`; `claim` read in `graph-db/rules-test` is that package's symbol, so the lookup falls to `graph-db`, where `claim/7` is registered as a global functor (`def-global-prolog-functor` interns and exports in `graph-db`). Confirm by running; if the head does not resolve, `(:import-from #:graph-db #:claim)` in the test package fixes it, and note that in the report.

- [ ] **Step 5: Run to verify failure**

Expected: `unknown Prolog functor CLAIM/7`.

- [ ] **Step 6: Implement the subject-bound generator**

```lisp
;;;; rules/facts.lisp -- claims as Prolog facts (spec SS4, GH #330).
;;;; Global functors, so the guard admits them by name; generators
;;;; follow FIND-BY-SLOT/4's trail discipline (index.lisp).

(in-package #:graph-db.rules)

(defun %namespace-keyword (x)
  "The keyword a namespace argument names, or NIL: a string resolves
with FIND-SYMBOL so a query cannot grow KEYWORD; a keyword passes; an
unbound variable or anything else is NIL (SS4)."
  (let ((v (graph-db::var-deref x)))
    (cond ((keywordp v) v)
          ((stringp v) (find-symbol (string-upcase v) :keyword))
          (t nil))))

(defun %namespace-string (keyword)
  (string-downcase (symbol-name keyword)))

(defun %bound (x)
  "X's value when bound to a non-variable, else NIL."
  (let ((v (graph-db::var-deref x)))
    (if (graph-db::var-p v) nil v)))

(defun %family-or-ill-typed (x)
  "The CLAIM-FAMILY the FAMILY argument names.  UNKNOWN-CLAIM-FAMILY
is what the runner reports as ill-typed client input, so it passes."
  (let ((v (%bound x)))
    (unless (symbolp v)
      (error 'unknown-claim-family :parent v))
    (claim-family v)))

(defmacro %yield ((var value) &body body)
  "Unify VAR with VALUE, run BODY (the continuation), undo."
  `(let ((old-trail (fill-pointer graph-db::*trail*)))
     (when (graph-db::unify ,var ,value) ,@body)
     (graph-db::undo-bindings old-trail)))

(defun %unify-claim (claim ?c ?sns ?skey ?rel ?ons ?okey family cont)
  "Bind every argument to CLAIM's fields and continue; unary claims
bind the object pair to NIL."
  (let ((binary (typep claim (claim-family-binary family))))
    (%yield (?c claim)
      (%yield (?sns (%namespace-string (claim-subject-namespace claim)))
        (%yield (?skey (claim-subject-key claim))
          (%yield (?rel (claim-relation claim))
            (%yield (?ons (and binary
                               (%namespace-string
                                (claim-object-namespace claim))))
              (%yield (?okey (and binary (claim-object-key claim)))
                (funcall cont)))))))))

(graph-db::def-global-prolog-functor claim/7
    (?c ?family ?sns ?skey ?rel ?ons ?okey cont)
  "Claims of ?FAMILY (a parent class) as facts: subject namespace and
key, relation, object namespace and key -- namespaces as strings, NIL
object pair for a unary claim.  Generates from the subject index when
the subject is bound, the object index when the object is, the producer
index through CLAIM-PRODUCER/2 in the same body; with nothing bound
under a resource bound it is refused as cost-unbounded (GH #285), and
without a bound it walks the family (SS4)."
  (let* ((family (%family-or-ill-typed ?family))
         (g graph-db:*graph*)
         (sns (%namespace-keyword ?sns)) (skey (%bound ?skey))
         (ons (%namespace-keyword ?ons)) (okey (%bound ?okey))
         (rel (%bound ?rel))
         (candidates
           (cond ((graph-db::node-p (%bound ?c)) (list (%bound ?c)))
                 ((and sns skey rel)
                  (graph-db:index-lookup
                   g (claim-family-parent family)
                   '(subject-namespace subject-key relation)
                   (list sns skey rel)))
                 ((and sns skey)
                  (graph-db:index-lookup
                   g (claim-family-parent family)
                   '(subject-namespace subject-key) (list sns skey)))
                 ((and ons okey)
                  (graph-db:index-lookup
                   g (claim-family-binary family)
                   '(object-namespace object-key) (list ons okey)))
                 ((and (stringp (%bound ?sns)) (null sns)) '())  ; unknown ns
                 ((and (stringp (%bound ?ons)) (null ons)) '())
                 (t (%unbound-claim-scan family)))))
    (dolist (claim candidates)
      (%unify-claim claim ?c ?sns ?skey ?rel ?ons ?okey family cont))))
```

For Task 1 make `%unbound-claim-scan` signal `prolog-cost-unbounded-error` unconditionally; Task 2 completes it. Check `graph-db::var-deref`, `unify`, `undo-bindings`, `*trail*`, `node-p` and `index-lookup`'s multi-slot value form against `index.lisp:1078` and `spacetime/claim-query.lisp`'s `claims-touching` before running.

- [ ] **Step 7: Run to verify pass**

Expected: `Did 1 checks. Pass: 1`.

- [ ] **Step 8: Commit**

```bash
git add graph-db.asd rules tests/rules
git commit -m "feat(rules): graph-db/rules with claim/7 over the subject index (#330)"
```

---

### Task 2: `claim/7` — the object and producer routes, unary claims, the unbound case

**Files:**
- Modify: `rules/facts.lisp`, `tests/rules/facts-tests.lisp`

**Interfaces:**
- Produces: `%unbound-claim-scan family` → every claim of the family via the type index (`graph-db::map-vertices` or `is-a`'s route; read `prolog-functors.lisp`'s `is-a/2` for the call) when no resource bound is in effect, else `prolog-cost-unbounded-error :functor 'claim/7`. `claim-producer/2` (Task 3) supplies the producer route; in Task 2 the producer route is reached by binding `?c` from `claim-producer/2` first, which needs no code here.

- [ ] **Step 1: Failing tests**

```lisp
(test claim-generates-from-the-object-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (?s) (claim ?c rt-claim "host" ?s "runs" "app" "web"))))
      (is (equal '("h1" "h2") (sort (mapcar #'first rows) #'string<))))))

(test a-unary-claim-binds-a-nil-object-pair
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (?r ?ons ?okey)
                  (claim ?c rt-claim "host" "h2" ?r ?ons ?okey))))
      (is (member '("reachable" nil nil) rows :test #'equal))
      (is (member '("runs" "app" "web") rows :test #'equal)))))

(test an-unknown-namespace-yields-nothing-and-interns-nothing
  (with-rules-graph (g)
    (seed g)
    (is (null (select (?o) (claim ?c rt-claim "never-recorded" "x" ?r ?ons ?o))))
    (is (null (find-symbol "NEVER-RECORDED" :keyword)))))

(test an-unregistered-family-is-ill-typed
  (with-rules-graph (g)
    (signals unknown-claim-family
      (select (?o) (claim ?c no-such-family "host" "h1" ?r ?ons ?o)))))

(test nothing-bound-is-refused-under-a-bound-and-walks-without-one
  (with-rules-graph (g)
    (seed g)
    (signals graph-db::prolog-cost-unbounded-error
      (select (:max-inferences 1000) (?c) (claim ?c rt-claim ?a ?b ?r ?d ?e)))
    (is (= 4 (select-count (?c) (claim ?c rt-claim ?a ?b ?r ?d ?e))))))
```

Check `select`'s option syntax for `:max-inferences` in `prologc.lisp` (`(select (:limit …) vars goals)` is the shape the GUI tests use).

- [ ] **Step 2: Run to verify failure** — the object route and the unbound walk fail.

- [ ] **Step 3: Implement**

```lisp
(defun %unbound-claim-scan (family)
  "Every claim of FAMILY when no resource bound is in effect; refused
as cost-unbounded otherwise (GH #285): %TICK cannot preempt inside a
family walk."
  (when (or graph-db::*inference-budget* graph-db::*query-deadline*)
    (error 'graph-db::prolog-cost-unbounded-error :functor 'claim/7))
  (let ((out '()))
    (graph-db::map-vertices (lambda (v) (push v out)) graph-db:*graph*
                            :vertex-type (claim-family-parent family))
    (nreverse out)))
```

Check `map-vertices`' keyword for the type filter and whether subclasses are included (the parent covers unary and binary); if not, walk both `claim-family-parent`'s subclasses explicitly. Check the names `*inference-budget*` and `*query-deadline*` in `prologc.lisp` (`%refuse-cost-unbounded` reads them).

- [ ] **Step 4: Run to verify pass** — expected all Task 1–2 tests green.

- [ ] **Step 5: Commit**

```bash
git add rules/facts.lisp tests/rules/facts-tests.lisp
git commit -m "feat(rules): claim/7 object route, unary claims, cost-unbounded rule (#330)"
```

---

### Task 3: The filters and the producer generator

**Files:**
- Modify: `rules/facts.lisp`, `tests/rules/facts-tests.lisp`

**Interfaces:**
- Produces: `claim-current/1`, `claim-valid-at/2`, `claim-producer/2` (generator when `?c` is unbound and the producer bound; filter otherwise), `claim-standing/2`, `claim-relation/2`, `claim-rule-version/2`. All take `?c` a claim node; a non-node `?c` fails (no solution), never signals.

- [ ] **Step 1: Failing tests**

```lisp
(test claim-current-filters-a-retracted-claim
  (with-rules-graph (g)
    (seed g)
    (let ((c (first (claims-touching g 'rt-claim :host "h2" :role :subject
                                     :relation "reachable"))))
      (retract-claim c))
    (is (= 2 (select-count (?c) (claim ?c rt-claim "host" "h2" ?r ?a ?b))))
    (is (= 1 (select-count (?c) (claim ?c rt-claim "host" "h2" ?r ?a ?b)
                                (claim-current ?c))))))

(test claim-valid-at-uses-the-validity-extent
  (with-rules-graph (g)
    (seed g)
    (is (equal '("1")
               (select-flat (?v) (claim ?c rtt-claim "app" "web" "version" "ver" ?v)
                                 (claim-valid-at ?c "2026-02-15T00:00:00Z"))))
    (is (equal '("2")
               (select-flat (?v) (claim ?c rtt-claim "app" "web" "version" "ver" ?v)
                                 (claim-valid-at ?c "2026-06-15T00:00:00Z"))))
    ;; A claim with no extent never matches.
    (is (null (select-flat (?c) (claim ?c rt-claim "host" "h1" ?r ?a ?b)
                                (claim-valid-at ?c "2026-02-15T00:00:00Z"))))))

(test claim-producer-generates-from-the-producer-index
  (with-rules-graph (g)
    (seed g)
    (is (= 2 (select-count (?c) (claim-producer ?c "scan-b"))))
    (is (= 3 (select-count (?c) (claim-producer ?c "scan-a")
                                (claim ?c rt-claim ?s ?k ?r ?a ?b))))
    (is (equal '("scan-a")
               (select-flat (?p) (claim ?c rt-claim "host" "h1" "runs" "app" "db")
                                 (claim-producer ?c ?p))))))

(test the-slot-filters
  (with-rules-graph (g)
    (seed g)
    (is (equal '("inferred")
               (select-flat (?s) (claim ?c rt-claim "host" "h2" "reachable" ?a ?b)
                                 (claim-standing ?c ?s))))
    (is (equal '("runs")
               (select-flat (?r) (claim ?c rt-claim "host" "h1" "runs" "app" "db")
                                 (claim-relation ?c ?r))))
    (is (equal '(nil)
               (select-flat (?v) (claim ?c rt-claim "host" "h1" "runs" "app" "db")
                                 (claim-rule-version ?c ?v))))))
```

`claim-producer/2` with `?c` unbound generates over BOTH families' producer indexes? No: the producer index is per family (`claim-producer` index on each parent). Decide: `claim-producer/2` with `?c` unbound generates across every registered family in the current graph (iterate `*claim-families*`, skip families whose index this graph lacks by catching the missing-index condition `%require-index` signals — check its name in `index.lisp`). State this in the docstring. The count in the test (`scan-b` → 2) covers rt-claim only because rtt-claim's producer is `scan-a`; the `scan-a` case with `claim/7` filtering to rt-claim gives 3.

- [ ] **Step 2: Run to verify failure** — the six functors undefined.

- [ ] **Step 3: Implement**

```lisp
(defun %claim-arg (x)
  "X as a claim node, or NIL when X is unbound or not a node."
  (let ((v (%bound x)))
    (and v (graph-db::node-p v) v)))

(graph-db::def-global-prolog-functor claim-current/1 (?c cont)
  "True while ?C's transaction period is open (CLAIM-CURRENT-P)."
  (let ((c (%claim-arg ?c)))
    (when (and c (claim-current-p c)) (funcall cont))))

(defun %instant-arg (x)
  (let ((v (%bound x)))
    (cond ((typep v 'local-time:timestamp) v)
          ((stringp v) (ignore-errors (local-time:parse-timestring v)))
          (t nil))))

(graph-db::def-global-prolog-functor claim-valid-at/2 (?c ?at cont)
  "True when ?C's validity extent possibly contains ?AT (an ISO-8601
string or a timestamp); a claim with no extent never matches."
  (let ((c (%claim-arg ?c)) (at (%instant-arg ?at)))
    (when (and c at)
      (let ((e (claim-extent c)))
        (when (and e (not (extents-disjoint-p
                           e (make-instant (exact-bound at)))))
          (funcall cont))))))

(defun %slot-filter (?c value cont)
  (%yield (?c-value value) (funcall cont)))
```

`%slot-filter` is wrong as written; write each filter explicitly:

```lisp
(graph-db::def-global-prolog-functor claim-standing/2 (?c ?s cont)
  (let ((c (%claim-arg ?c)))
    (when c
      (%yield (?s (%namespace-string (claim-standing c))) (funcall cont)))))

(graph-db::def-global-prolog-functor claim-relation/2 (?c ?r cont)
  (let ((c (%claim-arg ?c)))
    (when c (%yield (?r (claim-relation c)) (funcall cont)))))

(graph-db::def-global-prolog-functor claim-rule-version/2 (?c ?v cont)
  (let ((c (%claim-arg ?c)))
    (when c (%yield (?v (claim-rule-version c)) (funcall cont)))))

(defun %producer-candidates (producer)
  "Every claim PRODUCER wrote in the current graph, across every family
registered for it; a family this graph does not index is skipped."
  (let ((out '()))
    (maphash (lambda (parent family)
               (declare (ignore parent))
               (handler-case
                   (dolist (c (graph-db:index-lookup
                               graph-db:*graph* (claim-family-parent family)
                               '(producer) producer))
                     (push c out))
                 (graph-db::query-precondition-error () nil)))
             graph-db.spacetime::*claim-families*)
    (nreverse out)))

(graph-db::def-global-prolog-functor claim-producer/2 (?c ?p cont)
  "?C's producer; with ?C unbound and ?P bound, every claim ?P wrote,
from the producer index of every family in this graph."
  (let ((c (%claim-arg ?c)) (p (%bound ?p)))
    (cond (c (%yield (?p (claim-producer c)) (funcall cont)))
          ((stringp p)
           (dolist (claim (%producer-candidates p))
             (%yield (?c claim) (funcall cont))))
          (t nil))))
```

Standing comes back as a lowercase string (`"inferred"`), matching the namespace convention. Check the condition `index-lookup` signals for a missing index (`%require-index` in `index.lisp`) and catch exactly that.

- [ ] **Step 4: Run to verify pass.**

- [ ] **Step 5: Commit**

```bash
git add rules/facts.lisp tests/rules/facts-tests.lisp
git commit -m "feat(rules): claim-current, claim-valid-at, claim-producer and the slot filters (#330)"
```

---

### Task 4: Through the guard, CI lane, docs

**Files:**
- Modify: `tests/rules/facts-tests.lisp`, `.github/workflows/test.yml`, `docs/ci.md`, `CHANGELOG.md`
- Create: `docs/rules.md`

- [ ] **Step 1: Failing test — the guarded runner admits the functors**

```lisp
(test the-guard-admits-the-claim-functors
  (with-rules-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (graph-db.query:run-guarded-prolog
         "(claim ?c rt-claim \"host\" \"h1\" \"runs\" \"app\" ?o) (claim-producer ?c ?p)"
         g)
      (is (equal '("c" "o" "p") columns))
      (is (= 2 (length rows)))
      (is (every (lambda (row) (string= "scan-a" (third row))) rows)))
    ;; A keyword namespace is a reader refusal, not a match (SS4 amendment).
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:run-guarded-prolog
       "(claim ?c rt-claim :host \"h1\" ?r ?a ?b)" g))
    ;; An unregistered family through the guard is ill-typed, not a fault.
    (signals graph-db.query:prolog-ill-typed-error
      (graph-db.query:run-guarded-prolog
       "(claim ?c host \"host\" \"h1\" ?r ?a ?b)" g))))
```

The third assertion passes a schema *vertex type name* that is not a claim family: the guard admits it as a schema name, `claim-family` signals `unknown-claim-family`, and `%ill-typed-condition-p` must classify that as client-shaped. Read `%ill-typed-condition-p` in `query/guard.lisp`; if it does not admit `unknown-claim-family`, add it there (it is `graph-db/query` code; `graph-db/rules` depends on it, so the classifier may name the condition by `find-symbol` at load or the spacetime package may be made a dependency — prefer the `find-symbol` route, as `*no-applicable-method-type*` does, so `graph-db/query` stays independent of spacetime). Record which you did.

- [ ] **Step 2: Run to verify failure, then fix, then pass.** Also run `graph-db/query-test` (33 / 0 expected) if `query/guard.lisp` changed.

- [ ] **Step 3: CI lane and docs**

Copy the `query suite` step in `.github/workflows/test.yml` as `rules suite` after it with `:graph-db/rules-test`; add `rules` to `docs/ci.md`'s suite list. Create `docs/rules.md` with: what `graph-db/rules` is (S1 now, rules later, pointing at the spec); the functor table from spec §4 with the string-namespace convention and the NIL object pair; one guarded example and one raw `select` example; the cost-unbounded rule; what an unknown family or namespace does. CHANGELOG `[Unreleased]` Added: one entry (#304, #330).

- [ ] **Step 4: Final local run**

`graph-db/rules-test`, `graph-db/query-test`, `graph-db/spacetime-test` (650 / 0), all foreground. Record counts.

- [ ] **Step 5: Commit**

```bash
git add tests/rules query/guard.lisp .github/workflows/test.yml docs/ci.md docs/rules.md CHANGELOG.md
git commit -m "feat(rules): the claim functors through the guard; rules suite lane; docs (#330)"
```

Do not push; the branch is pushed once, with the spec, when S1 is complete.

---

## Self-review notes

- **Spec coverage.** §3 home → Task 1. §4 functors: `claim/7` all routes → Tasks 1–2; filters and `claim-producer/2` → Task 3; guard admission → Task 4; cost-unbounded rule → Task 2; unregistered family ill-typed → Tasks 2 and 4. §11 S1 bullets → the tests above. §13 findings → each task's report.
- **Deviation recorded.** Namespaces as strings (amendment at the top). `claim-producer/2`'s generator spans every family the graph indexes — spec §4 said "the producer index"; plural is the honest reading since the index is per family.
- **Placeholders.** None; the "check before relying" notes name the file and symbol to read.
- **Type consistency.** `%yield`, `%bound`, `%namespace-keyword`, `%namespace-string`, `%claim-arg` used identically across Tasks 1–3; `run-guarded-prolog text graph` per #322's signature in Task 4.
