# rules S3: cross-store scope — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A rule declared in store A reads every store in a scope the operator hands `run-rule` and writes only A; a premise from another store is named in its `derived-from` record's `method` slot; the reads of a cross-store run resolve in one comparable epoch space under the shared clock (equal epochs in a quiescent image; recon C2 — the engine provides no single instant across stores).

**Architecture:** One special, `graph-db::*claim-scope*`, bound by `run-rule` for the body's evaluation and read by S1's `claim/7` and `claim-producer/2`, which iterate every store in it per route (a store whose schema lacks the family contributes nothing). Because a read-write transaction on A refuses every read of B (the GH #53 contract, `transactions.lisp`'s transactional `lookup-object` method), a run whose scope holds a foreign store evaluates its body **before** the write transaction, under composed `call-with-read-snapshot`s of every store in scope — one comparable epoch space when the stores share a system clock, equal epochs in a quiescent image (recon C2); per-store consistency and no comparability otherwise — and then reconciles inside A's transaction exactly as S2 does; a single-store scope keeps S2's inside-the-transaction evaluation unchanged. Premises leave the evaluation as identity keys plus store names, never as nodes, so the reconcile touches no foreign store. `premises-of` resolves a premise in the store its record names when that store is in the scope the caller passes.

**Tech Stack:** SBCL 2.6.6, ASDF, FiveAM, `graph-db/rules` (S1+S2), `graph-db/spacetime`, `graph-db/query`, the system clock (`system-clock.lisp`, #168).

**Spec:** `docs/superpowers/specs/2026-09-04-rules-as-producers-design.md` §10 and §11's S3 bullet (epic #304, sub-issue #332). S2's record: `docs/superpowers/decisions/2026-09-05-rules-s2-rulings.md`, `docs/superpowers/notes/2026-09-05-rules-s2-engine-api-facts.md`, `docs/rules.md`, handoff `docs/superpowers/handoffs/2026-09-05-rules-s2.md`. Consumer: kraison/cl-llm#24, whose evidence convention is `memory/trace.lisp` (`store-name`, `%resolve-in`, a cite resolves in the store it names when that store is in scope, else `:absent`).

## Global Constraints

- Lisp: spaces only; hard 80-column limit; terse comments pointing at spec §10, a ruling, or #332. Docstrings: what, returns, the one trap.
- `graph-db.rules` `(:use #:cl)`; nothing imported; every spacetime/graph-db/temporal-extent symbol qualified (S2's `rules/package.lisp` header; ruling R4). `rules/facts.lisp` is `(in-package #:graph-db)` (S1 ruling R1) and stays there.
- Core untouched. The scope special lives in `rules/facts.lisp` beside the functors it serves.
- Every S1 and S2 test keeps passing unchanged: with `*claim-scope*` NIL every functor behaves exactly as before, and a single-store `run-rule` takes exactly S2's path.
- Substring assertions use `:test #'char-equal`; refusal messages print symbols downcased; rule text contains no colon; every constructor call passes `:graph` (a second `def-claim-classes` for a family under another store name rebinds the constructors' default store — S2 recon C2).
- Worktree: `/home/raison/work/vivace-graph-v3/.worktrees/rules-s3` (branch `feat/rules-s3` from `origin/experiment` `a75cf96`). The main checkout `/home/raison/work/vivace-graph-v3` is shared with other sessions: never build, edit or commit there. cl-temporal-extent 0.3.0 is on master, so no second worktree this time.
- Suites, foreground, one at a time, worktree first on the registry:

  ```
  cd /home/raison/work/vivace-graph-v3/.worktrees/rules-s3
  sbcl --dynamic-space-size 4096 --non-interactive \
    --eval '(push #p"/home/raison/work/vivace-graph-v3/.worktrees/rules-s3/" asdf:*central-registry*)' \
    --eval '(ql:quickload :graph-db/rules-test :silent t)' \
    --eval '(asdf:test-system :graph-db/rules-test)'
  ```

  Always the `-test` system; read `Did N checks.`; never background a run; never two suites at once from one worktree. Baselines at `a75cf96`: `graph-db/rules-test` 314/0, `graph-db/spacetime-test` 653/0, `graph-db/query-test` 40/0, `graph-db/gui-test` 635/0, full `graph-db` 5370/0 with ten GEOS skips. Before the branch is called done: rules, spacetime, query, gui green with counts recorded; the full `graph-db` once, detached by the controller.
- After every scripted edit to a test file, `git diff <base> -- <file> | grep -c '^-[^-]'` and expect 0 for an append.
- Every negative test names its mechanism and has a control.
- No push without Kevin. Trailers: `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` and `Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU`. Docs travel with code in the same commit.
- Issues: #332 gets the decisions as a comment when the branch is complete; close by hand with the merge SHA.

## Rulings taken while planning (record them in the S3 decision file)

Order of authority while executing: spec > these > the recon note > the task text.

- **S3-P1 — scope is a run-time argument, and the special is `graph-db::*claim-scope*`.** `run-rule graph rule &key scope` and `run-rules graph &key scope`; `scope` is a list of open graphs, `graph` is put first if absent, and the rule writes only `graph` (spec §10). During evaluation `run-rule` binds `*claim-scope*` to that list; `claim/7` and `claim-producer/2` iterate it per route, own store first, and with it NIL behave exactly as S1 shipped them. A Lisp caller may bind it around a raw `select`. Cost if wrong: a scope that should have been a record slot must be threaded by every caller; reversible by adding a slot later.
- **S3-P2 — a cross-store run evaluates before its transaction, under composed snapshots.** The engine's read-resolution rule is exhaustive: inside a read-write transaction on A, any read of B signals `cross-graph-transaction-error`, snapshot or no snapshot (`transactions.lisp`, `lookup-object`'s transactional method; `tests/multi-graph-tests.lisp` `read-write-transaction-blocks-a-foreign-read-even-under-a-snapshot`). So when the scope holds a store other than `graph`, `run-rule` evaluates the body inside nested `call-with-read-snapshot`s of every store in scope (own store included) and only then opens the write transaction for the reconcile. Under a shared clock those snapshots take epochs from one counter — equal in a quiescent image, comparable always (#168); the engine deliberately provides no single instant across stores (`call-with-read-snapshot`'s docstring, #53; recon C2), so no doc or test claims one. Without a clock each store is internally consistent and the epochs are not even comparable. A single-store scope keeps S2's path unchanged. **Cost if wrong:** a cross-store run is not serialised against concurrent premise writes — a premise committed after the snapshots is seen by the next run, not this one, with no conflict raised. Single-store runs keep S2's serialisation.
- **S3-P3 — a premise leaves evaluation as `(identity-key . store-name)`, never as a node.** `%desired` records for each solution the premises' `claim-identity-key` and the name of the store they came from (`node-graph`, set by the engine on every node a lookup returns; `resolve-node-graph` is the fallback), so the reconcile — which runs inside A's transaction — reads no foreign node. The binding reason is the read pin, not the cross-graph error (recon C4): a node `index-lookup` returns under a snapshot skips `ensure-node-bytes`, so reading its slots after the snapshot's extent reads B's heap unpinned. Everything the reconcile needs from a premise (identity key, store name; the extent is consumed by `%premise-extent` inside the same extent) is computed inside the snapshot; nodes never leave it — a rule for every later change too. `store-name` is cl-llm's convention: `(string-downcase (symbol-name (graph-name g)))`. The `derived-from` record's `method` slot is that name when the premise's store is not the rule's store, NIL otherwise (spec §10). Two stores holding one identity key contribute one record (the family's `def-unique` tuple excludes `method`), the rule's own store preferred, else the first in scope order. Cost: one store name lost in that corner; recorded.
- **S3-P4 — `premises-of graph claim &key (scope (list graph))` resolves in the named store when it is in scope, else drops the premise.** A record naming no store resolves in `graph`. Mirrors cl-llm's `%resolve-in` (`:absent` for a store out of scope). `dependents-of` is unchanged: the records live in the rule's store, and a premise from any store is looked up by its identity key. Cost: a caller who forgets the scope sees fewer premises, never wrong ones.
- **S3-P5 — compile stays single-store.** The guard validates a rule's text against its own store's schema and the cycle graph is over the rule's own store (spec §6); a family read from a foreign store must be declared under the rule's store's name too (`def-claim-classes fam :store-a` and `:store-b` — the registry is per family symbol, the indexes per store, S2 recon A4). A foreign store in scope whose schema lacks a family a goal names contributes nothing: Task 1's `%scope-lookup` swallows the `query-precondition-error` `%require-index` signals, per foreign store and per route, as `%producer-candidates` and `%claim-by-identity-key` already do (recon C1 — `claim/7`'s bare `index-lookup` calls signal today). The rule's OWN store keeps S1's behaviour and still signals (ruling S3-R1), so a single-store goal on a family the store does not index stays the ill-typed refusal S1 documented. Cost: consumers declare shared families under both names; a cross-store cycle (A reads what B's rule derives and vice versa) is not detected — recorded as a known limit for #333.
- **S3-P6 — the unrouted walk under scope walks every store, and the refusal is unchanged.** `%unbound-claim-scan` maps every store in scope when no bound is in effect; under a bound it refuses exactly as S1 does. No new behaviour on the guarded surface.

---

## File structure

| file | responsibility |
|---|---|
| `rules/facts.lisp` | `*claim-scope*`, `%scope-graphs`, the routes and generators iterating it (Task 1) |
| `rules/run.lisp` | scope on `run-rule`/`run-rules`; evaluation under composed snapshots; premises as key + store; `method` on `derived-from`; `premises-of` with scope (Tasks 2–3) |
| `rules/package.lisp` | no new exports needed (`*claim-scope*` is `graph-db`-homed; the keyword arguments are on existing exports) |
| `tests/rules/suite.lisp` | store B (`:graph-db-rules-b`) with `rt-claim`/`rtt-claim` declared for it too; `with-two-stores`; `seed-b`; the clock fixture (Task 1) |
| `tests/rules/scope-tests.lisp` | the S3 suite (Tasks 1–3) |
| `docs/rules.md` | "Cross-store scope" section; edits to "Running a rule", "Provenance", "What the functors do not see" (Task 3) |
| `CHANGELOG.md`, `docs/superpowers/decisions/2026-09-05-rules-s3-rulings.md`, `docs/superpowers/handoffs/2026-09-05-rules-s2.md` (a closing status line) | record (Task 3) |

---

### Task 0: Recon — verify before any code

Nine assumptions, each to be confirmed or refuted from source in the `rules-s3` worktree, quoting the form. Write `docs/superpowers/notes/2026-09-05-rules-s3-engine-api-facts.md` (pinned to the worktree head; §C corrections first with a finding → correction map), commit it, and amend the tasks below before Task 1 starts.

- [ ] **B1** `index-lookup graph …` on a graph other than the open transaction's signals `cross-graph-transaction-error` from `lookup-object`'s transactional method (`transactions.lisp` ~:319-323), reached through `%node-by-id` (`spatial-query.lisp:37`, `lookup-vertex :graph graph`). Confirm the path and that no route in `index-lookup` bypasses it.
- [ ] **B2** Outside any transaction, nested `call-with-read-snapshot` on graphs A and B register both in `*read-snapshots*` (`transactions.lisp` ~:3355-3372), and `index-lookup` on either resolves through its own snapshot (`lookup-object`'s null-transaction method ~:294). `run-query-goals`' `select :snapshot t` inherits an enclosing snapshot of the same graph rather than opening a second (`call-with-read-snapshot`'s docstring: "An enclosing snapshot of the SAME graph is inherited"). Confirm from the code, not the docstring.
- [ ] **B3** Every node `index-lookup` returns has `node-graph` set to the graph it was looked up in (`finalize-node` `primitive-node.lisp:208-212`, `ensure-node-bytes` `:216-225`, `lookup-node`'s path). Name any path that returns a node with `node-graph` NIL.
- [ ] **B4** Reading a claim's slots (the `claim-*` accessors, `claim-identity-key`, `claim-extent`) on a node whose `node-graph` is B, while a read-write transaction on A is open, performs no `lookup-object` and so cannot signal the cross-graph error — check `slot-value-using-class :around` (`primitive-node.lisp:506-516`; `clos.lisp` is in no system and never loads, recon C3) and `maybe-init-node-data` (`primitive-node.lisp:300-322`, reads the heap of `node-home-graph`). If any lazy slot read can call `lookup-object`, S3-P3 must also materialise the premise's extent sexp during evaluation; say which.
- [ ] **B5** `graph-name graph` is the keyword `make-graph` was given; `lookup-graph` keys `*graphs*` on it (`graph-class.lisp:3-12`, `:511`); `resolve-node-graph` (`interface.lisp:7`) returns `(values graph status …)`. Confirm the keyword shape and the downcased-string convention cl-llm's `store-name` uses (`~/work/cl-llm/memory/schema.lisp:49-51`).
- [ ] **B6** A second `(def-claim-classes rt-claim :graph-db-rules-b)` after the S1 declaration for `:graph-db-rules-test`: the `claim-family` struct is re-registered under the same parent symbol with the same class names; `def-index`/`def-unique`/`def-value-constraint` are registered under the new graph name without unregistering the old (S2 recon A4); the constructors' default store rebinds to B (S2 recon C2). Confirm nothing in `def-claim-classes` clears the first store's registrations (`spacetime/claim.lisp:376-484`).
- [ ] **B7** The clock in a test: `open-system-clock (namestring dir)` then `make-graph name dir :system-clock clock` for each store, `close-graph` both, `close-system-clock` last (`tests/system-clock-tests.lisp:395-420`). Confirm `make-graph`'s `:system-clock` keyword and that `*system-clock*` need not be bound for the composed snapshots to share an instant (what `call-with-read-snapshot` reads: the graph's attached clock or the special?). Quote the site.
- [ ] **B8** `map-vertices` on a foreign store outside any transaction is legal and snapshot-consistent under that store's registered snapshot (`vertex.lisp:185-233`); inside a read-write transaction on another store it signals (`with-read-pin`/`lookup` path). Needed for S3-P6.
- [ ] **B9** `%overlay-transaction` (`spacetime/claim-query.lisp:196`) is reached from `claims-by-producer` and `claims-touching` only; `claim/7`'s routes call `index-lookup` directly and never overlay, so a foreign store read during evaluation sees committed state only (S2 recon A6 extended to scope).

---

### Task 1: `*claim-scope*` — the functors read every store in scope

**Files:**
- Modify: `rules/facts.lisp`, `tests/rules/suite.lisp`
- Create: `tests/rules/scope-tests.lisp`
- Modify: `graph-db.asd` (`graph-db/rules-test` gains `scope-tests`)

**Interfaces:**
- Produces: `graph-db::*claim-scope*` (a list of graphs or NIL), `graph-db::%scope-graphs () => list`; unchanged functor signatures. Fixture: store name `:graph-db-rules-b`, `with-two-stores ((a b) &body)`, `seed-b (g)`, `with-clocked-stores ((a b) &body)`.

- [ ] **Step 1: Fixture** — in `tests/rules/suite.lisp`, after the `rtu-claim` declaration:

```lisp
;; S3: a second store the scope tests read from (spec §10).  The same
;; families, declared under B's name too -- the registry is per family
;; symbol, the indexes per store (S2 recon A4) -- and every constructor
;; call passes :GRAPH, because this second declaration rebinds the
;; constructors' default store to B (S2 recon C2).
(eval-when (:load-toplevel :execute)
  (setf (gethash :graph-db-rules-b graph-db::*schema-node-metadata*) nil))

(def-claim-classes rt-claim :graph-db-rules-b)
(def-claim-classes rtt-claim :graph-db-rules-b :temporal t)

(defmacro with-two-stores ((a b) &body body)
  "A fresh store A (*GRAPH-NAME*) and a fresh store B (:GRAPH-DB-RULES-B),
*GRAPH* bound to A."
  (let ((da (gensym "DIR-A")) (db (gensym "DIR-B")))
    `(let* ((,da (graph-db-test-scratch:make-scratch-directory
                  "graph-db-rules-a"))
            (,db (graph-db-test-scratch:make-scratch-directory
                  "graph-db-rules-b"))
            (,a (make-graph *graph-name* (namestring ,da)
                            :buffer-pool-size 1000))
            (,b (make-graph :graph-db-rules-b (namestring ,db)
                            :buffer-pool-size 1000)))
       (unwind-protect (let ((graph-db:*graph* ,a)) ,@body)
         (ignore-errors (close-graph ,b))
         (ignore-errors (close-graph ,a))))))

(defun seed-b (g)
  "Store B's claims: h3 runs web (scan-c), h1 runs cache (scan-c), and
web version 3 valid Jul 1 - Sep 30 (scan-c).  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h3"
                          :relation "runs" :object-namespace :app
                          :object-key "web" :producer "scan-c"
                          :standing :observed)
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h1"
                          :relation "runs" :object-namespace :app
                          :object-key "cache" :producer "scan-c"
                          :standing :observed)
    (make-rtt-claim-binary :graph g :subject-namespace :app :subject-key "web"
                           :relation "version" :object-namespace :ver
                           :object-key "3" :producer "scan-c"
                           :standing :observed
                           :extent (interval (ts 2026 7 1) (ts 2026 9 30)))))

(defmacro with-clocked-stores ((a b) &body body)
  "WITH-TWO-STORES under one system clock opened in a scratch directory,
so the two stores' snapshot epochs come from one counter (#168) -- equal
in a quiescent image, never guaranteed one instant (recon C2)."
  (let ((cdir (gensym "CLOCK")) (clock (gensym "CLOCK"))
        (da (gensym "DIR-A")) (db (gensym "DIR-B")))
    `(let* ((,cdir (graph-db-test-scratch:make-scratch-directory
                    "graph-db-rules-clock"))
            (,clock (graph-db:open-system-clock (namestring ,cdir)))
            (,da (graph-db-test-scratch:make-scratch-directory
                  "graph-db-rules-a"))
            (,db (graph-db-test-scratch:make-scratch-directory
                  "graph-db-rules-b"))
            (,a nil) (,b nil))
       (unwind-protect
            (progn
              (setf ,a (make-graph *graph-name* (namestring ,da)
                                   :buffer-pool-size 1000
                                   :system-clock ,clock)
                    ,b (make-graph :graph-db-rules-b (namestring ,db)
                                   :buffer-pool-size 1000
                                   :system-clock ,clock))
              (let ((graph-db:*graph* ,a)) ,@body))
         (when ,b (ignore-errors (close-graph ,b)))
         (when ,a (ignore-errors (close-graph ,a)))
         (graph-db:close-system-clock ,clock)))))
```

Recon B7 settles `open-system-clock`'s and `make-graph`'s exact keywords and whether `open-system-clock`/`close-system-clock` are exported (write `graph-db::` if not). `tests/rules/package.lisp` needs nothing new unless B7 names it.

- [ ] **Step 2: Failing tests** — `tests/rules/scope-tests.lisp`:

```lisp
;;;; tests/rules/scope-tests.lisp -- cross-store scope (spec §10, GH #332).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(test claim-reads-every-store-in-scope
  "With *CLAIM-SCOPE* bound, CLAIM/7 generates from each store's index in
scope order; with it NIL, from *GRAPH* alone (S3-P1)."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    ;; Control: NIL scope is S1 exactly -- A's two hosts of web.
    (is (equal '("h1" "h2")
               (sort (select-flat (?h) (claim ?c rt-claim "host" ?h "runs"
                                              "app" "web"))
                     #'string<)))
    (let ((graph-db::*claim-scope* (list a b)))
      (is (equal '("h1" "h2" "h3")
                 (sort (select-flat (?h) (claim ?c rt-claim "host" ?h "runs"
                                                "app" "web"))
                       #'string<)))
      ;; The subject route across stores: h1 runs web and db in A, cache
      ;; in B.
      (is (equal '("cache" "db" "web")
                 (sort (select-flat (?a) (claim ?c rt-claim "host" "h1"
                                                "runs" "app" ?a))
                       #'string<)))
      ;; A node from B is a node: the filters work on it.
      (is (equal '("scan-c")
                 (select-flat (?p) (claim ?c rt-claim "host" "h3" "runs"
                                          "app" "web")
                                   (claim-producer ?c ?p)))))))

(test claim-producer-generates-across-scope
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((graph-db::*claim-scope* (list a b)))
      ;; scan-c wrote 2 rt-claims and 1 rtt-claim, all in B.
      (is (= 2 (select (:count t :max-inferences 1000) (?c)
                 (claim-producer ?c "scan-c")
                 (claim ?c rt-claim ?s ?k ?r ?o ?ok))))
      ;; scan-a is in A only; the scope adds nothing to it.
      (is (= 2 (select (:count t :max-inferences 1000) (?c)
                 (claim-producer ?c "scan-a")
                 (claim ?c rt-claim ?s ?k ?r ?o ?ok)))))))

(test a-store-lacking-the-family-contributes-nothing
  "rtu-claim is declared for A only; B in scope adds nothing and refuses
nothing (S3-P5)."
  (with-two-stores (a b)
    (seed a)
    (with-transaction ((graph-db::transaction-manager a))
      (make-rtu-claim-binary :graph a :subject-namespace :app
                             :subject-key "web" :relation "owned-by"
                             :object-namespace :team :object-key "t1"
                             :producer "scan-a" :standing :observed))
    (let ((graph-db::*claim-scope* (list a b)))
      (is (equal '("t1")
                 (select-flat (?t) (claim ?c rtu-claim "app" "web"
                                          "owned-by" "team" ?t)))))))

(test the-walk-covers-every-store-without-a-bound-and-refuses-under-one
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((graph-db::*claim-scope* (list a b)))
      ;; 4 rt-claims in A + 2 in B.
      (is (= 6 (select-count (?c) (claim ?c rt-claim ?a ?b ?r ?d ?e))))
      (signals graph-db::prolog-cost-unbounded-error
        (select (:max-inferences 1000) (?c)
          (claim ?c rt-claim ?a ?b ?r ?d ?e))))))

(test a-foreign-read-inside-a-transaction-is-the-engines-refusal
  "The GH #53 contract, not ours: a read-write transaction on A refuses
every read of B.  RUN-RULE evaluates before its transaction for that
reason (S3-P2); a Lisp caller binding the scope inside one gets the
engine's error."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((graph-db::*claim-scope* (list a b)))
      (signals graph-db:cross-graph-transaction-error
        (with-transaction ((graph-db::transaction-manager a))
          (select-flat (?h) (claim ?c rt-claim "host" ?h "runs"
                                   "app" "web")))))))
```

- [ ] **Step 3: Run to verify they fail** — `*claim-scope*` unbound; record the count (baseline 314).

- [ ] **Step 4: `rules/facts.lisp`** — after the `+claim-*-index-slots+` parameters:

```lisp
(defvar *claim-scope* nil
  "The stores CLAIM/7 and CLAIM-PRODUCER/2 read, own store first, or NIL
for *GRAPH* alone (spec §10, GH #332).  RUN-RULE binds it for a body's
evaluation; a Lisp caller may bind it around a SELECT.  Trap: inside a
read-write transaction every read of another store is the engine's
CROSS-GRAPH-TRANSACTION-ERROR (GH #53) -- bind it outside one.")

(defun %scope-graphs ()
  (or *claim-scope* (list *graph*)))

(defun %scope-lookup (class-name slots value)
  "INDEX-LOOKUP over every store in scope, in scope order.  A FOREIGN
store whose schema does not carry CLASS-NAME contributes nothing --
QUERY-PRECONDITION-ERROR read as %PRODUCER-CANDIDATES reads it -- while
the own store (first) still signals, so a single-store goal keeps S1's
ill-typed refusal (ruling S3-R1, recon C1)."
  (let ((graphs (%scope-graphs)))
    (append (index-lookup (first graphs) class-name slots value)
            (loop for g in (rest graphs)
                  append (handler-case
                             (index-lookup g class-name slots value)
                           (query-precondition-error () '()))))))
```

Then in `claim/7` replace the four `index-lookup` calls with `%scope-lookup` (drop the `g` argument), and make `%unbound-claim-scan` take no graph, walking `(%scope-graphs)` after its refusal check:

```lisp
  (let ((parent (graph-db.spacetime:claim-family-parent family)))
    (loop for g in (%scope-graphs)
          append (map-vertices #'identity g :vertex-type parent
                                           :collect-p t))))
```

and `%producer-candidates` likewise iterate `(%scope-graphs)` inside its family loop (the existing `handler-case` per lookup stays). Update the docstrings of `claim/7` and `claim-producer/2` with one sentence each on the scope. The `(g *graph*)` binding in `claim/7` goes.

- [ ] **Step 5: Run** — rules suite green; every S1/S2 test unchanged; record the count. Then the spacetime and query suites once (facts.lisp is loaded by neither, but the assertion is cheap).

- [ ] **Step 6: Docs** — `docs/rules.md` "What the functors do not see" gains: with `*claim-scope*` bound the routes read every store in it, committed state only, own store first; and the transaction trap in one sentence. Full section comes in Task 3.

- [ ] **Step 7: Commit** — `feat(rules): the claim functors read every store in *claim-scope* (#332)`.

---

### Task 2: `run-rule` with a scope — evaluate under composed snapshots, name the premise's store

**Files:**
- Modify: `rules/run.lisp`, `tests/rules/scope-tests.lisp` (append), `docs/rules.md` ("Running a rule", "Provenance")

**Interfaces:**
- Consumes: `graph-db::*claim-scope*`, `graph-db:call-with-read-snapshot` (check export; `graph-db::` if not), `graph-db::node-graph`, `graph-db::resolve-node-graph`, `graph-db:graph-name`.
- Produces: `run-rule (graph rule &key scope)`, `run-rules (graph &key scope)`; `%store-name (graph) => string`; `%desired` returning premises as `(key . store-name)`; `%reconcile-provenance` writing `:method`; `%normalize-scope (graph scope) => list`.

- [ ] **Step 1: Failing tests** — append to `tests/rules/scope-tests.lisp`:

```lisp
(defparameter *web-hosts-body-any*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")

(test a-rule-in-a-derives-from-premises-in-a-and-b-and-writes-only-a
  "Spec §11's S3 bullet."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let* ((r (write-rule a :name "web-hosts" :version "1"
                          :family "rt-claim"
                          :head *web-hosts-head* :body *web-hosts-body-any*))
           (report (graph-db.rules:run-rule a r :scope (list a b))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 3 (graph-db.rules:rule-report-derived report)))
      (let ((claims (derived a 'rt-claim "web-hosts")))
        (is (equal '("h1" "h2" "h3")
                   (sort (mapcar #'claim-object-key claims) #'string<))))
      ;; Nothing was written to B: no derived claim, no derivation record.
      (is (null (claims-by-producer b 'rt-claim "rule/web-hosts")))
      (is (null (claims-by-producer b 'graph-db.rules:derivation
                                    "rule/web-hosts")))
      ;; Provenance names B for h3's premise and nothing for h1's.
      (let* ((h3 (find "h3" (derived a 'rt-claim "web-hosts")
                       :key #'claim-object-key :test #'string=))
             (h1 (find "h1" (derived a 'rt-claim "web-hosts")
                       :key #'claim-object-key :test #'string=))
             (rec-h3 (first (claims-touching a 'graph-db.rules:derivation
                                             :claim (claim-identity-key h3)
                                             :role :subject)))
             (rec-h1 (first (claims-touching a 'graph-db.rules:derivation
                                             :claim (claim-identity-key h1)
                                             :role :subject))))
        (is (string= "graph-db-rules-b" (claim-method rec-h3)))
        (is (null (claim-method rec-h1)))))))

(test a-single-store-scope-is-s2-unchanged
  "Control for S3-P2: RUN-RULE with no scope, or a scope of the store
alone, derives inside its transaction exactly as before."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body-any*)))
      (is (= 2 (graph-db.rules:rule-report-derived
                (graph-db.rules:run-rule a r))))
      (is (= 2 (graph-db.rules:rule-report-kept
                (graph-db.rules:run-rule a r :scope (list a)))))
      ;; The scope's own store is put first whatever the caller wrote.
      (is (= 1 (graph-db.rules:rule-report-derived
                (graph-db.rules:run-rule a r :scope (list b a))))))))

(test cross-store-validity-intersects-across-stores
  "rtt premises: web version 3 (B, Jul 1 - Sep 30) against h1's
deployments (A, Feb 1 - Jun 30 and Aug 1 - Sep 30) and h2's (A, May)."
  (with-two-stores (a b)
    (seed a)
    (seed-temporal a)
    (seed-b b)
    (let ((report (graph-db.rules:run-rule
                   a (write-rule a :name "host-version" :version "1"
                                 :family "rtt-claim"
                                 :head *host-version-head*
                                 :body *host-version-body*)
                   :scope (list a b))))
      ;; A alone gave 4 derived, 2 disjoint (S2).  Version 3 adds
      ;; (h1 Aug-Sep, v3) = [Aug 1, Sep 30] and two more disjoint pairs
      ;; ((h1 Feb-Jun, v3), (h2 May, v3)): 5 derived, 4 disjoint.
      (is (= 5 (graph-db.rules:rule-report-derived report)))
      (is (= 4 (graph-db.rules:rule-report-disjoint-premises report)))
      (let ((v3 (find "3" (derived a 'rtt-claim "host-version")
                      :key #'claim-object-key :test #'string=)))
        (is-true v3)
        (when v3
          (is (= 2 (length (graph-db.rules:premises-of
                            a v3 :scope (list a b))))))))))

(test a-cross-store-rerun-reconciles-and-a-b-change-is-seen-next-run
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body-any*)))
      (graph-db.rules:run-rule a r :scope (list a b))
      (with-transaction ((graph-db::transaction-manager b))
        (mark-deleted (first (claims-touching b 'rt-claim :host "h3"
                                              :role :subject))))
      (let ((report (graph-db.rules:run-rule a r :scope (list a b))))
        (is (= 2 (graph-db.rules:rule-report-kept report)))
        (is (= 1 (graph-db.rules:rule-report-swept report)))
        (is (= 0 (graph-db.rules:rule-report-derived report)))))))

(test run-rules-takes-the-scope
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                :head *web-hosts-head* :body *web-hosts-body-any*)
    (let ((reports (graph-db.rules:run-rules a :scope (list a b))))
      (is (= 1 (length reports)))
      (is (= 3 (graph-db.rules:rule-report-derived (first reports)))))))

(test a-cross-store-run-under-one-clock-derives
  "Under a shared clock the composed snapshots take epochs from one
counter (#168).  Observable here: the run works and derives from both
stores.  Nothing asserts one instant -- the engine provides none across
stores (recon C2), and a test of it would pass vacuously in a quiescent
suite."
  (with-clocked-stores (a b)
    (seed a)
    (seed-b b)
    (let ((report (graph-db.rules:run-rule
                   a (write-rule a :name "web-hosts" :version "1"
                                 :family "rt-claim"
                                 :head *web-hosts-head*
                                 :body *web-hosts-body-any*)
                   :scope (list a b))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 3 (graph-db.rules:rule-report-derived report))))))

(test a-scope-must-be-open-graphs
  (with-two-stores (a b)
    (declare (ignorable b))
    (signals error (graph-db.rules:run-rule a "web-hosts" :scope '(:not-a-graph)))))
```

Re-derive the temporal arithmetic from `seed`, `seed-temporal` and `seed-b` before trusting it (the plan's author has been wrong on such counts before); the docstring of the test carries the derivation.

- [ ] **Step 2: RED**, record the count.

- [ ] **Step 3: `rules/run.lisp`**

```lisp
(defun %store-name (graph)
  "GRAPH's name as the string a DERIVED-FROM record's METHOD carries --
cl-llm's STORE-NAME convention: the downcased graph name (spec §10)."
  (string-downcase (symbol-name (graph-db:graph-name graph))))

(defun %normalize-scope (graph scope)
  "SCOPE as RUN-RULE reads it: open graphs, GRAPH first, no duplicates.
Signals on anything that is not a graph."
  (dolist (g scope)
    (unless (typep g 'graph-db:graph)
      (error "RUN-RULE :SCOPE holds ~S, which is not an open graph." g)))
  (cons graph (remove graph (remove-duplicates scope) :test #'eq)))

(defun %premise-ref (node graph)
  "A premise as the reconcile carries it: (IDENTITY-KEY . STORE-NAME),
STORE-NAME NIL for the rule's own GRAPH (S3-P3).  Read during
evaluation, so the reconcile touches no foreign store."
  (let ((home (or (graph-db::node-graph node)
                  (graph-db::resolve-node-graph (graph-db:id node)))))
    (cons (graph-db.spacetime:claim-identity-key node)
          (and home (not (eq home graph)) (%store-name home)))))
```

In `%desired`: premises become `(mapcar (lambda (n) (%premise-ref n graph)) …)` and the union in the collapse branch dedupes on `car` with `string=`; `%premise-extent` must therefore run on the NODES before they are turned into refs (compute the extent first, then the refs). In `%reconcile-provenance`: the `wanted` key stays `(derived-key . premise-key)`, its value the store name (or NIL); a pair seen twice keeps the first value with NIL preferred (own store); the constructor call adds `:method (or store-name nil)`; an existing record is kept only when its `claim-method` `equal`s the wanted store name too (else swept and rewritten).

`run-rule`:

```lisp
(defun run-rule (graph rule &key scope)
  "...  SCOPE (spec §10): the open graphs the body reads, GRAPH put first;
NIL or (GRAPH) is S2 exactly.  With a foreign store in SCOPE the body is
evaluated BEFORE the write transaction, under one read snapshot per
store -- epochs from one counter when the stores share a system clock, else
each store consistent on its own and incomparable (GH #53, recon C2) -- because a read-write transaction
refuses every read of another store (S3-P2).  ..."
  ...
  (let* ((scope (%normalize-scope graph scope))
         (foreign (rest scope)))
    ...
    (flet ((evaluate ()
             (let ((graph-db::*claim-scope* scope))
               (%desired compiled graph report))))
      (if foreign
          (multiple-value-bind (desired order)
              (%under-snapshots scope #'evaluate)
            (graph-db:with-transaction (:graph graph)
              (%derive compiled graph report desired order)))
          (graph-db:with-transaction (:graph graph)
            (multiple-value-bind (desired order) (evaluate)
              (%derive compiled graph report desired order)))))))

(defun %under-snapshots (graphs thunk)
  "THUNK under a composed read snapshot of every graph in GRAPHS."
  (if (null graphs)
      (funcall thunk)
      (graph-db:call-with-read-snapshot
       (lambda () (%under-snapshots (rest graphs) thunk))
       (first graphs))))
```

`%derive` takes `desired`/`order` (the counter reset stays first; the evaluation moved out); the retry-safety note updates: on a retry only the reconcile re-runs against a fresh transaction, the evaluation's result is reused — say so in the docstring, and that a cross-store run's evaluation is not repeated on conflict. `run-rules graph &key scope` passes it through. Every `rule-run-refusal`/`prolog-error` raised during an out-of-transaction evaluation still lands in `run-rule`'s handlers (they wrap both branches).

- [ ] **Step 4: GREEN**, record the count; spacetime and query suites once.

- [ ] **Step 5: Docs** — "Running a rule": the `:scope` keyword, S3-P2's two paths and their consistency (one comparable epoch space under the clock, equal in a quiescent image, never a guaranteed instant; per-store and incomparable without; not serialised against concurrent premise writes in the foreign case), the retry note; recon O1 in "What the functors do not see": secondary-index membership is not snapshot-versioned, so a claim deleted after a snapshot is invisible to it too (pre-existing, single-store as well); recon B9's corollary: during a cross-store evaluation `*transaction*` is NIL, so a body reaching for `claims-touching` sees no transaction overlay; recon B5: a store in a scope must be keyword-named (`%store-name` takes `symbol-name`). "Provenance": `method` names the premise's store, NIL for the rule's own; the collision corner.

- [ ] **Step 6: Commit** — `feat(rules): run-rule reads a scope of stores under composed snapshots and writes its own (#332)`.

---

### Task 3: `premises-of` with scope, the docs section, the record, the runs

**Files:**
- Modify: `rules/run.lisp` (`premises-of`), `tests/rules/scope-tests.lisp` (append), `docs/rules.md` (new "Cross-store scope" section + read-through), `CHANGELOG.md`, `docs/superpowers/handoffs/2026-09-05-rules-s2.md` (one closing line: S3 landed on `feat/rules-s3`)
- Create: `docs/superpowers/decisions/2026-09-05-rules-s3-rulings.md`

- [ ] **Step 1: Failing tests**

```lisp
(test premises-of-resolves-in-the-store-the-record-names
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (graph-db.rules:run-rule
     a (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                   :head *web-hosts-head* :body *web-hosts-body-any*)
     :scope (list a b))
    (let ((h3 (find "h3" (derived a 'rt-claim "web-hosts")
                    :key #'claim-object-key :test #'string=)))
      ;; In scope: the premise, from B.
      (let ((ps (graph-db.rules:premises-of a h3 :scope (list a b))))
        (is (= 1 (length ps)))
        (is (string= "h3" (claim-subject-key (first ps))))
        (is (eq b (graph-db::node-graph (first ps)))))
      ;; Out of scope: dropped, not resolved in A by mistake (S3-P4).
      (is (null (graph-db.rules:premises-of a h3)))
      ;; And a B premise's dependents are findable from A.
      (let ((premise (first (claims-touching b 'rt-claim :host "h3"
                                             :role :subject))))
        (is (= 1 (length (graph-db.rules:dependents-of a premise))))))))
```

- [ ] **Step 2: RED**; **Step 3:** `premises-of graph claim &key (scope (list graph))`: for each `derived-from` record, `(claim-method r)` NIL → resolve in `graph`; a name → `(find name scope :key #'%store-name :test #'string=)` → resolve there, else drop. `%claim-by-identity-key` already takes the graph. `dependents-of`: no change beyond the docstring. **Step 4: GREEN**, count.

- [ ] **Step 5: `docs/rules.md`** — new section "Cross-store scope (GH #332)" after "Provenance": what a scope is, own store first, writes only the own store, the schema rule (S3-P5, families declared under both names), the two evaluation paths and the clock as C2 has it (S3-P2), `method` and `premises-of :scope` (S3-P3/P4), the walk (S3-P6), the transaction trap for Lisp callers, the known limit (no cross-store cycle detection, #333). Read the whole file once against the shipped code. `CHANGELOG.md` entry under Added. The decision record with S3-P1..P6 and every execution ruling from the ledger, in the S2 file's shape, stating they were taken without Kevin and which deviate from the spec (none deviate; S3-P2 refines §7's "one transaction" for the cross-store case, forced by GH #53 — say so). One closing line in the S2 handoff pointing at this branch.

- [ ] **Step 6: Runs** — rules, query, spacetime, gui in the foreground (each under 10 minutes); the full `graph-db` is the controller's, detached. Record every `Did N checks.`.

- [ ] **Step 7: Commit** — `docs(rules): cross-store scope contract and record; premises-of takes a scope (#332)`.

- [ ] **Step 8:** whole-branch review by a fresh reviewer, one fix wave, one scoped re-review; then the #332 comment.

---

## Self-review against the spec

- §10 "reads stores in a scope, A first, writes only A": Task 2 (`%normalize-scope`, the nothing-written-to-B assertions).
- §10 "`claim/7` generates over every store in scope": Task 1.
- §10 "a premise from B is recorded with its store name in `method`": Task 2 (S3-P3), the cl-llm convention.
- §10 "reads resolve at one instant under the shared clock": Task 2 (S3-P2, composed snapshots; the clocked fixture).
- §11 S3 bullet: Task 2's first test.
- §9 reads under scope: Task 3 (S3-P4).
- Type consistency: `%premise-ref` produces `(key . store-name)`; `%reconcile-provenance` consumes `car`/`cdr` of it; `%store-name` is the one place the string is minted and the one place `premises-of` compares it.
