# Edges under claims, U1 (#369, tracker #367) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A claim gains one real edge to each endpoint that resolves in its own store — `subject-of` to its subject's node, `object-of` to its object's — linked when the claim is written, so `traverse`, `node-claims` and the Prolog goals `related/3` / `claimed/4` walk claim topology through the engine's adjacency instead of an index probe per hop.

**Architecture:** Two shipped edge classes in `graph-db.spacetime` with no default store, always placed in the claim's store. `def-claim-classes`' constructor wrapper links each endpoint after the claim node is built: a same-store source found by `index-lookup` plus the open transaction's own creates (the commit-view overlay), or a node the caller resolved beforehand and passes as `:subject-node` / `:object-node`, verified. Edges are derived and never identity; the `(namespace, key)` slots stay the truth and `claims-touching` stays the complete read. Reads over the edges filter by currency; retraction keeps them; a deleted endpoint hides them through `active-edge-p`.

**Tech Stack:** SBCL, FiveAM, `graph-db/core`, `graph-db/spacetime`, `graph-db/query` (tests only, for the guarded runner).

**Spec:** `docs/superpowers/specs/2026-09-12-edges-under-claims-design.md` at `6dde0ee` on `feat/edges-under-claims` — §3, §4.1–4.3, §6.1–6.3, §7, §11 (in-store bullets), §13 U1. Every `file:line` below was verified against `experiment` b787516 during planning (2026-09-12); match on the quoted forms, not the numbers. U2 (the sweep, §5, §9) and #368 (cross-store continuation, §8) are NOT this plan.

## Global Constraints

- Lisp: spaces only, hard 80 columns on every line, terse comments naming `GH #369` or a spec section; docstrings state what / returns / the one trap.
- Branch `feat/edges-under-claims`, worktree `/home/raison/work/vg-367` (the clone is `/home/raison/work/vg-c3`, on `experiment`). Never build in the clone.
- Never run `pkill`, `pgrep -f` or `kill`. One SBCL build at a time in this worktree. Never the full suite by hand; the suites below only. CI runs the full suite on push (`docs/ci.md`).
- No store format change. No edge appears in any `def-unique` or `def-index` declaration (spec §3 "Not identity"). The edge classes have no slots.
- **Linking never fails a claim write** (spec §4.1). The only write-time `error` is `endpoint-mismatch` for a caller-supplied node (§4.2); everything else leaves the claim exactly as the first implementation wrote it.
- The functors use `graph-db:lookup-vertex` on the claim's graph, never `lookup-vertex-anywhere`: a caller-resolved cross-store endpoint is linked but does not unify (spec §8 as amended). `claim-endpoints`, the Lisp read, DOES use `lookup-vertex-anywhere`.
- Every new `graph-db.spacetime` symbol a test uses unqualified goes on `spacetime/package.lisp`'s export list; engine symbols are written `graph-db:` in spacetime tests (house rule, `tests/spacetime/package.lisp` header).
- Existing tests keep passing; the baseline counts recorded in Task 1 never drop.
- Docs travel with the code (Task 8). Every commit message names `#369`.
- Commit trailers on every commit:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01Dai2CTvMRE41v7RzAUnWmn
  ```

## Running the suites

Write once to `/home/raison/work/vg-c3-notes/vg-369-suites.lisp` (outside every checkout):

```lisp
;; The suites #369 touches, CI-style, in a fresh image (docs/ci.md).
(ql:quickload '(:graph-db/spacetime-test :graph-db/query-test) :silent t)
(format t "~&== graph-db loaded from ~a~%"
        (asdf:system-source-directory :graph-db))
(log:config :error)
(let ((ok t))
  (unless (graph-db/spacetime-test:run-spacetime-tests) (setf ok nil))
  (unless (graph-db/query-test:run-query-tests) (setf ok nil))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root, foreground, timeout 600000 ms:

```bash
cd /home/raison/work/vg-367
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3-notes/vg-369-suites.lisp \
  > /home/raison/work/vg-c3-notes/vg-369-suites.log 2>&1; echo "exit=$?"
grep -E "loaded from|Did [0-9]+ checks|Fail:|^== " \
  /home/raison/work/vg-c3-notes/vg-369-suites.log
```

A single spacetime test while iterating (`run-spacetime-tests` binds the system directory itself; FiveAM's `run` does not, so bind it here):

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/spacetime-test :silent t)' \
  --eval '(in-package :graph-db/spacetime-test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

Always read back `Did N checks`. Fresh image after any export or ASDF change.

## Engine facts every task relies on

- **Adjacency is indexed in the edge's store, at commit apply.** `add-to-ve-index` writes `(ve-index-in graph)` / `(ve-index-out graph)` for the edge's own graph (`edge.lisp:168-174`), called from `add-node-to-indexes` (`transactions.lisp:987-996`) during apply. So `map-edges :vertex v :direction :in` on the claim's graph finds a claim's edge to `v` even when `v` lives elsewhere (the ve-key is the id), and NO edge created in an open transaction is visible to `map-edges` / `outgoing-edges` until that transaction commits.
- **Secondary indexes are also maintained at commit apply** (`%ix-claim`, `index.lisp:589-600`, under "Maintenance (APPLY, post-durability)"). `index-lookup` (`index.lisp:994`) cannot see a node created in the open transaction. The overlay for that is `graph-db:make-commit-view graph tx` / `view-writes` / `view-node` / `view-old-node` (`value-constraint.lisp:151-190`), used by `%overlay-transaction` (`spacetime/claim-query.lisp:257-282`); `view-old-node` is NIL for a create.
- `index-lookup graph class (list slot) key` signals `graph-db:query-precondition-error` (`globals.lisp:487`, initarg `:reason`) when CLASS.SLOT has no index in GRAPH, and returns NIL when the index is declared but empty (`%require-index`, `index.lisp:977`).
- `active-edge-p` (`edge.lisp:373-385`) is what `map-edges` / `outgoing-edges` / `incoming-edges` filter through: an endpoint that is `:found` and `deleted-p`, or `:missing`, hides the edge; `:detached` / `:unknown` / `:absent-in-store` keep it. So a deleted claim's edges vanish from adjacency reads and a deleted source's do too, with no code here.
- Generated edge constructor (`%make-constructor-closure`, `schema.lisp:483-495`): `(make-<edge> &key from to weight graph id ...)`; `from`/`to` may be vertices or id arrays (`make-edge`, `edge.lisp:227-235`). Without `:graph` and with no default store it signals `default-store-not-open-error` (`schema.lisp:339-357`, the `(:DEFAULT-STORE NIL)` report branch; `%default-store-graph`, `:396`). `def-edge NAME () () NIL` registers under the NIL store key and instantiates into nothing (`%install-node-type`, `schema.lisp:758-760`, `(lookup-graph nil)` is NIL); the type is adopted into a store lazily on first write (`%ensure-type-in-store`, `:378`).
- A freshly created vertex carries its graph: `(setf (node-graph v) graph)` in `make-vertex` (`vertex.lisp:179`).
- `def-edge` installs `<name>/2` and `<name>/3` in the class's own package (`%install-edge-functors`, `prolog-functors.lisp:1152`); they scan `*graph*` and resolve endpoints with `lookup-vertex` (`%edge-functor/2`, `:1106-1150`).
- `def-global-prolog-functor name/arity (args... cont) body` (`prolog-functors.lisp:11-23`) is `defun` + `export` from `*package*` + registry entry. In BODY: `graph-db:var-deref` each argument; `graph-db::var-p` is true for an unbound variable; `graph-db:unify` binds or compares (nodes compare by id, `prolog-equal`, `prologc.lisp:136`); save `(fill-pointer graph-db:*trail*)` before and `graph-db:undo-bindings` after each solution; `(funcall cont)` once per solution.
- The guarded runner (`graph-db.query:run-guarded-prolog text graph &key limit max-inferences timeout format`, `query/guard.lisp:705`) whitelists every registered functor by `(name . arity)` from any package (`%functor-whitelist`, `:307-337`) and resolves the goal head in the functor's home package; a `graph-db:query-precondition-error` raised while solving surfaces as `graph-db.query:prolog-ill-typed-error` (`:660-670`). Query text admits no colon: type and slot names are spelled bare (`tests/query/guard-tests.lisp` header). A node binds through `(is-a ?x <type>)` and `(node-slot-value ?x <slot> "v")` (`prolog-functors.lisp:953`, `:893`).
- The GUI functor-inventory tripwire (`tests/gui/gui-tests.lisp:1497`, `*reviewed-functor-inventory*`) runs in the gui lane's own image, which loads `graph-db/gui` → `graph-db`, never `graph-db/spacetime` (`docs/ci.md` "Each lane is its own sbcl process"). The six new spacetime functors do not reach it in CI; a dev image loading both WILL trip it, which is the tripwire working (same as the rules functors).
- Spacetime: `def-claim-classes` (`spacetime/claim.lisp:333-518`); the wrapper lambda at `:474-497` (`%check-claim-identity`, `%claim-encode-extent-arg`, `%claim-encode-version-stamp`, `%claim-encode-transaction-arg`, `check-standing`); `*claim-families*` / `claim-family` struct (`parent unary binary temporal-p`, `:14-47`); `namespace-sources` signals `unknown-namespace` for an unregistered namespace (`spacetime/source.lisp:42-47`); `source-contract class` → facets, `(getf (source-facets-identity f) :key-slot)` is the identity slot symbol (`spacetime/resolve.lisp:44-45`); `claims-touching` (`claim-query.lisp:284-419`) with its filter tail at `:395-419`; `%claim-as-of` (`:185`), `%claim-as-of-epoch` (`:220`), `%claim-validity-touches-p` (`:6`), `%paginate` (`:246`), `%refuse-epoch-axis` (`:162`), `claim-current-p` (`:469`), `retract-claim` (`:476`), `delete-claims-by-producer graph parent producer` (`:547`).
- Tests: `tests/spacetime/claim-tests.lisp:7-24` (graph-name parameter, the `*schema-node-metadata*` clear idiom, `with-claim-graph`), `source-tests.lisp:7-28` (`def-source` with all seven facets), `:159-167` (`with-source-graph`), `resolve-tests.lisp` (a second class in one namespace is legal), `suite.lisp` (`make-temp-directory`, `with-temp-directory`, `collect-garbage`); `store-resolver-tests.lisp:63-76` (detach = `close-graph g :snapshot-p nil`, then `lookup-vertex-anywhere` yields an `unresolved-node`). `graph-db.asd:561-582` (spacetime components), `:584-623` (spacetime-test).

## File structure

- `spacetime/conditions.lisp` — modify: `endpoint-link-skipped` (warning), `endpoint-mismatch` (error).
- `spacetime/claim.lisp` — modify: the two `def-edge` forms (before `def-claim-classes`); the wrapper strips `:subject-node` / `:object-node` and calls `%link-claim-at-write` after `check-standing`.
- `spacetime/link.lisp` — create: write-time linking (`%same-store-candidates`, `%uncommitted-creates`, `%verify-endpoint-node`, `%link-endpoint`, `%link-claim-at-write`). U2 adds the sweep to this file.
- `spacetime/claim-query.lisp` — modify: extract `%narrow-claims` from `claims-touching`'s tail; add `claim-endpoints`, `node-claims`.
- `spacetime/functors.lisp` — create: `%claim-linked-endpoint`, `%binary-claim-p`, `%solve-claims`, `related/3`, `claimed/4`.
- `spacetime/package.lisp` — modify: exports.
- `graph-db.asd` — modify: `link` after `resolve`, `functors` after `temporal`; test system gains `:graph-db/query` and `endpoint-edge-tests`.
- `tests/spacetime/endpoint-edge-tests.lisp` — create: fixtures + every §11 in-store test.
- Docs: `docs/vivace-graph-v3-doc.org` (Chapter 18, new `***` section before "Vocabulary"), `CHANGELOG.md`, the spec's Built note, `docs/ci.md` tripwire paragraph.

---

### Task 1: Baseline

**Files:**
- Create: `/home/raison/work/vg-c3-notes/vg-369-suites.lisp` (the runner above)

- [ ] **Step 1: Run the runner on the unchanged branch (6dde0ee)**

Expected: `exit=0`, `graph-db loaded from /home/raison/work/vg-367/`, two `Did N checks` lines with no `Fail:`.

- [ ] **Step 2: Record both counts** in the SDD ledger (`/home/raison/work/vg-c3-notes/vg-369-sdd-ledger.md`, create it) as the baseline. No later run may report fewer for either suite.

---

### Task 2: The two edge classes, exported, with no default store

**Files:**
- Modify: `spacetime/claim.lisp` (insert before `(defmacro def-claim-classes`, i.e. before line 333's docstring block; after `+claim-object-slots+`)
- Modify: `spacetime/package.lisp` (export list)
- Modify: `graph-db.asd` (`graph-db/spacetime-test` `:depends-on` and components)
- Create: `tests/spacetime/endpoint-edge-tests.lisp`

**Interfaces:**
- Produces: classes `graph-db.spacetime:subject-of`, `graph-db.spacetime:object-of` (edge, no slots, no default store); generated `make-subject-of`, `make-object-of`, `subject-of-p`, `object-of-p`, `lookup-subject-of`, `lookup-object-of`, functors `subject-of/2`, `subject-of/3`, `object-of/2`, `object-of/3` — all in and exported from `graph-db.spacetime`.
- Produces: test fixtures `*ee-graph-name*`, `with-ee-graph`, source classes `ee-thing` (`:ee-things`, key `thing-id`) and `ee-twin` (`:ee-things`, key `twin-id`), family `ee-claim`, helper `ee-b`.

- [ ] **Step 1: Write the failing tests and fixtures**

Create `tests/spacetime/endpoint-edge-tests.lisp`:

```lisp
;;;; Edges under claims, U1 (GH #369, spec 2026-09-12 sec.3-4, 6, 11).
;;;;
;;;; One store holds the sources AND the claims here: that is the shape
;;;; the consumer runs (spec sec.10) and the only one U1 links in.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *ee-graph-name* :graph-db-ee-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ee-graph-name* graph-db::*schema-node-metadata*) nil))

(def-source ee-thing :graph-db-ee-test
    ((label :initarg :label :accessor ee-label)
     (thing-id :initarg :thing-id :accessor ee-thing-id))
  :identity     (:namespace :ee-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "EE fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

;; A second source class in the SAME namespace: two records under one
;; key are the ambiguity case of spec sec.4.1 step 3.
(def-source ee-twin :graph-db-ee-test
    ((twin-id :initarg :twin-id :accessor ee-twin-id))
  :identity     (:namespace :ee-things :key-slot twin-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "EE fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(def-claim-classes ee-claim :graph-db-ee-test)

(defmacro with-ee-graph ((g) &body body)
  "A fresh on-disk graph named *EE-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ee-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun ee-b (&key (subject "t-1") (object "t-2") (relation "r")
                  (producer "p") (subject-namespace :ee-things)
                  (object-namespace :ee-things) subject-node object-node)
  "A binary EE-CLAIM.  NIL for a -NODE key means 'not given'."
  (make-ee-claim-binary :subject-namespace subject-namespace
                        :subject-key subject :relation relation
                        :object-namespace object-namespace
                        :object-key object :producer producer
                        :standing :inferred
                        :subject-node subject-node
                        :object-node object-node))

(defun ee-thing (id)
  (make-ee-thing :label id :thing-id id))

(defun same-node-p (a b)
  (and a b (equalp (id a) (id b))))

(test edge-classes-exist-with-no-default-store
  "Spec sec.3: shipped, slotless, placed only by :GRAPH."
  (is-true (find-class 'subject-of nil))
  (is-true (find-class 'object-of nil))
  (is-true (subtypep 'subject-of 'graph-db:edge))
  (is-true (fboundp 'make-subject-of))
  (is-true (fboundp 'make-object-of))
  (signals graph-db:default-store-not-open-error
    (make-subject-of :from nil :to nil))
  (signals graph-db:default-store-not-open-error
    (make-object-of :from nil :to nil)))

(test edge-classes-are-placed-by-graph-in-any-store
  "The claim's store adopts the type lazily on first write (#167 R3)."
  (with-ee-graph (g)
    (let (c n)
      (with-transaction ()
        (setq n (ee-thing "t-1"))
        (setq c (ee-b :subject "nope" :object "nope-either"))
        (make-subject-of :from c :to n :graph g))
      (is (= 1 (length (graph-db:outgoing-edges
                        c :graph g :edge-type 'subject-of))))
      (is-true (graph-db:lookup-node-type-by-name 'subject-of :edge
                                                  :graph g)))))
```

- [ ] **Step 2: Wire the test file and the query dependency into ASDF**

In `graph-db.asd`, `graph-db/spacetime-test`: change

```lisp
  :depends-on (:graph-db/spacetime :graph-db/core :graph-db/geos
               :graph-db/test-scratch :fiveam)
```
to
```lisp
  ;; GRAPH-DB/QUERY: endpoint-edge-tests.lisp drives RELATED/3 and
  ;; CLAIMED/4 through RUN-GUARDED-PROLOG (GH #369, spec sec.6.3).
  :depends-on (:graph-db/spacetime :graph-db/core :graph-db/geos
               :graph-db/query :graph-db/test-scratch :fiveam)
```
and append to the components, after `(:file "vocabulary-tests")`:
```lisp
               (:file "endpoint-edge-tests"))            ; GH #369
```
(move the closing paren: `vocabulary-tests` loses its `)`).

- [ ] **Step 3: Run the two tests to verify they fail**

Run the single-test command with `edge-classes-exist-with-no-default-store`. Expected: a compile-time or run-time failure naming `subject-of` (undefined class / function).

- [ ] **Step 4: Define the classes**

In `spacetime/claim.lisp`, immediately before the `(defmacro def-claim-classes` form (after `+claim-object-slots+`), insert:

```lisp
;;; The two shipped edge classes (GH #369, spec sec.3).  Direction is
;;; claim -> endpoint.  No default store: MAKE-SUBJECT-OF / MAKE-
;;; OBJECT-OF refuse without :GRAPH, and the wrapper always passes the
;;; claim's own graph.  No slots, no identity, no index: the edge is a
;;; cache of one resolution; the (namespace, key) slots stay the truth.
(graph-db:def-edge subject-of () () nil)
(graph-db:def-edge object-of () () nil)
```

- [ ] **Step 5: Export**

In `spacetime/package.lisp`, after the `;; registration (GH #138)` line's exports (i.e. append before the closing `))`):

```lisp
   ;; edges under claims (GH #369)
   #:subject-of #:object-of
   #:make-subject-of #:make-object-of
   #:subject-of-p #:object-of-p
   #:lookup-subject-of #:lookup-object-of
   #:subject-of/2 #:subject-of/3 #:object-of/2 #:object-of/3
   #:claim-endpoints #:node-claims
   #:endpoint-link-skipped #:endpoint-link-skipped-claim
   #:endpoint-link-skipped-namespace #:endpoint-link-skipped-key
   #:endpoint-link-skipped-classes
   #:endpoint-mismatch #:endpoint-mismatch-node
   #:endpoint-mismatch-namespace #:endpoint-mismatch-key
   #:endpoint-mismatch-reason
   #:related/3 #:claimed/4
```

(Exporting names before they are defined is legal; `def-edge`'s own `export` and `def-global-prolog-functor`'s are then no-ops.)

- [ ] **Step 6: Run the two tests to verify they pass**

Fresh image (export + ASDF change). Expected: both PASS.

- [ ] **Step 7: Commit**

```bash
git add spacetime/claim.lisp spacetime/package.lisp graph-db.asd \
        tests/spacetime/endpoint-edge-tests.lisp
git commit -m "feat(spacetime): SUBJECT-OF and OBJECT-OF edge classes, no default store (#369)"
```

---

### Task 3: Conditions and the write-time linker

**Files:**
- Modify: `spacetime/conditions.lisp` (append)
- Create: `spacetime/link.lisp`
- Modify: `graph-db.asd` (`graph-db/spacetime` components: `(:file "link")` after `(:file "resolve")`)
- Modify: `spacetime/claim.lisp` (the wrapper lambda, `:474-497`)
- Test: `tests/spacetime/endpoint-edge-tests.lisp`

**Interfaces:**
- Consumes: `subject-of` / `object-of` (Task 2); `namespace-sources`, `source-contract`, `source-facets-identity` (source.lisp); `graph-db:make-commit-view`, `view-writes`, `view-node`, `view-old-node`; `graph-db:index-lookup`.
- Produces: `(%link-claim-at-write claim &key subject-node object-node) => claim`; conditions `endpoint-link-skipped` (readers `-claim -namespace -key -classes`), `endpoint-mismatch` (readers `-node -namespace -key -reason`, reason `:not-a-source` or `:key`); constructor keys `:subject-node` / `:object-node` on every `make-<family>-unary/-binary`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/endpoint-edge-tests.lisp`.  These use `claim-endpoints` (Task 4) only through `graph-db:outgoing-edges`, so they can pass before Task 4:

```lisp
(defun ee-linked-to (claim edge-type g)
  "The id the CLAIM's EDGE-TYPE edge points at, or NIL."
  (let ((e (first (graph-db:outgoing-edges claim :graph g
                                                 :edge-type edge-type))))
    (and e (graph-db:to e))))

(test auto-link-in-two-transactions
  "Spec sec.4.1: sources committed first, the claim later; both link."
  (with-ee-graph (g)
    (let (s o c)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction () (setq c (ee-b)))
      (is (equalp (id s) (ee-linked-to c 'subject-of g)))
      (is (equalp (id o) (ee-linked-to c 'object-of g))))))

(test auto-link-in-one-transaction
  "Spec sec.4.1 'Same-transaction visibility': the index cannot see the
sources yet; the commit-view overlay must."
  (with-ee-graph (g)
    (let (s o c)
      (with-transaction ()
        (setq s (ee-thing "t-1") o (ee-thing "t-2"))
        (setq c (ee-b)))
      (is (equalp (id s) (ee-linked-to c 'subject-of g)))
      (is (equalp (id o) (ee-linked-to c 'object-of g))))))

(test key-only-when-nothing-resolves
  "An unknown namespace, and a known one with no such key, both leave
the claim key-only and legal; CLAIMS-TOUCHING still finds it."
  (with-ee-graph (g)
    (let (s c1 c2)
      (with-transaction () (setq s (ee-thing "t-1")))
      (with-transaction ()
        (setq c1 (ee-b :object-namespace :ee-nowhere :object "x"))
        (setq c2 (ee-b :object "t-missing")))
      (is (equalp (id s) (ee-linked-to c1 'subject-of g)))
      (is (null (ee-linked-to c1 'object-of g)))
      (is (null (ee-linked-to c2 'object-of g)))
      (is (= 2 (length (claims-touching g 'ee-claim :ee-things "t-1"
                                        :role :subject)))))))

(test ambiguity-warns-once-and-writes-unlinked
  "Spec sec.4.1 step 3 / sec.7: two candidates -> no edge, one
ENDPOINT-LINK-SKIPPED, the write commits."
  (with-ee-graph (g)
    (let ((warned 0) c)
      (with-transaction ()
        (ee-thing "dup")
        (make-ee-twin :twin-id "dup"))
      (handler-bind ((endpoint-link-skipped
                       (lambda (w)
                         (incf warned)
                         (is (equal "dup" (endpoint-link-skipped-key w)))
                         (is (= 2 (length
                                   (endpoint-link-skipped-classes w))))
                         (muffle-warning w))))
        (with-transaction () (setq c (ee-b :subject "dup"))))
      (is (= 1 warned))
      (is (null (ee-linked-to c 'subject-of g)))
      (is (= 1 (length (claims-touching g 'ee-claim :ee-things "dup")))))))

(test caller-resolved-cross-store-endpoint-is-linked
  "Spec sec.4.2: the object lives in another store; the caller resolved
it before the transaction and hands it over; the edge carries a
foreign id."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (declare (ignorable sg))
      (let (n c)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1")))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (is-true n)
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            (setq c (ee-b :object-namespace :st-reports :object "r-1"
                          :object-node n))))
        (is (equalp (id n) (ee-linked-to c 'object-of g)))))))

(test caller-resolved-mismatch-refuses-the-write
  "Spec sec.4.2 step 1 / sec.7: a node that is not a source of the
namespace, or whose key is not the claim's, is ENDPOINT-MISMATCH and
nothing commits."
  (with-ee-graph (g)
    (let (s)
      (with-transaction () (setq s (ee-thing "t-1")))
      ;; wrong class for the namespace
      (signals endpoint-mismatch
        (with-transaction ()
          (ee-b :object-namespace :st-reports :object "r-1"
                :object-node s)))
      ;; right class, wrong key
      (signals endpoint-mismatch
        (with-transaction ()
          (ee-b :object "t-2" :object-node s)))
      (is (null (claims-touching g 'ee-claim :ee-things "t-1"))))))
```

- [ ] **Step 2: Run one to verify it fails**

Run `auto-link-in-two-transactions`. Expected: FAIL — the edge is absent (`ee-linked-to` NIL), and `:subject-node` may already be rejected by the constructor.

- [ ] **Step 3: Add the conditions**

Append to `spacetime/conditions.lisp`:

```lisp
;;; Edges under claims (GH #369, spec sec.7).

(define-condition endpoint-link-skipped (warning)
  ((claim :initarg :claim :reader endpoint-link-skipped-claim)
   (namespace :initarg :namespace :reader endpoint-link-skipped-namespace)
   (key :initarg :key :reader endpoint-link-skipped-key)
   (classes :initarg :classes :reader endpoint-link-skipped-classes))
  (:report (lambda (c s)
             (format s "Endpoint (~S ~S) has ~D candidate nodes across ~
~S; the claim was written unlinked (GH #369)."
                     (endpoint-link-skipped-namespace c)
                     (endpoint-link-skipped-key c)
                     (length (endpoint-link-skipped-classes c))
                     (endpoint-link-skipped-classes c)))))

(define-condition endpoint-mismatch (spacetime-error)
  ((node :initarg :node :reader endpoint-mismatch-node)
   (namespace :initarg :namespace :reader endpoint-mismatch-namespace)
   (key :initarg :key :reader endpoint-mismatch-key)
   ;; :NOT-A-SOURCE or :KEY
   (reason :initarg :reason :reader endpoint-mismatch-reason))
  (:report (lambda (c s)
             (format s "~A is not the endpoint (~S ~S): ~A (GH #369)."
                     (endpoint-mismatch-node c)
                     (endpoint-mismatch-namespace c)
                     (endpoint-mismatch-key c)
                     (ecase (endpoint-mismatch-reason c)
                       (:not-a-source
                        "its class is not a registered source of the namespace")
                       (:key "its identity key is not the claim's"))))))
```

- [ ] **Step 4: Write the linker**

Create `spacetime/link.lisp`:

```lisp
;;;; Linking a claim to its endpoint nodes (GH #369, spec sec.4).
;;;;
;;;; Write-time here; the idempotent sweep (U2, spec sec.5) joins this
;;;; file.  Edges are derived: the (namespace, key) slots are the truth,
;;;; and nothing in this file may fail a claim write except a caller's
;;;; own mismatch (sec.4.2).

(in-package #:graph-db.spacetime)

(defun %identity-slot (class)
  "CLASS's identity key slot symbol (SOURCE-FACETS-IDENTITY :KEY-SLOT)."
  (getf (source-facets-identity (source-contract class)) :key-slot))

(defun %uncommitted-creates (graph class slot key)
  "Nodes of CLASS this transaction creates whose SLOT is KEY -- what
INDEX-LOOKUP cannot see, because the index is written at commit apply
(spec sec.4.1 'Same-transaction visibility'; the #324 overlay)."
  (let ((tx graph-db::*transaction*))
    (when tx
      (let ((view (graph-db:make-commit-view graph tx))
            (out '()))
        (dolist (w (graph-db:view-writes view) out)
          (let ((n (graph-db:view-node view (graph-db:id w))))
            (when (and n
                       (typep n class)
                       (null (graph-db:view-old-node view n))
                       (let ((v (slot-value n slot)))
                         (and (stringp v) (string= v key))))
              (push n out))))))))

(defun %same-store-candidates (graph namespace key)
  "Distinct nodes in GRAPH that are registered sources of NAMESPACE with
identity key KEY: index hits unioned with the open transaction's own
creates.  NIL, never a signal, for an unknown namespace or a class with
no index in GRAPH (spec sec.4.1 steps 1-2)."
  (let ((hits '()))
    (dolist (class (handler-case (namespace-sources namespace)
                     (unknown-namespace () nil)))
      (let ((slot (%identity-slot class)))
        (dolist (n (handler-case
                       (graph-db:index-lookup graph class (list slot) key)
                     (graph-db:query-precondition-error () nil)))
          (push n hits))
        (dolist (n (%uncommitted-creates graph class slot key))
          (push n hits))))
    (remove-duplicates hits :key #'graph-db:id :test #'equalp)))

(defun %verify-endpoint-node (node namespace key)
  "Signal ENDPOINT-MISMATCH unless NODE is a registered source of
NAMESPACE whose identity key is KEY (spec sec.4.2 step 1).  An
UNRESOLVED-NODE marker is never a source, so it is refused too."
  (let* ((classes (handler-case (namespace-sources namespace)
                    (unknown-namespace () nil)))
         (class (find-if (lambda (c) (typep node c)) classes)))
    (unless class
      (error 'endpoint-mismatch :node node :namespace namespace :key key
                                :reason :not-a-source))
    (let ((v (slot-value node (%identity-slot class))))
      (unless (and (stringp v) (string= v key))
        (error 'endpoint-mismatch :node node :namespace namespace
                                  :key key :reason :key)))
    node))

(defun %link-endpoint (claim graph ctor namespace key given)
  "Create one CTOR edge from CLAIM to its endpoint in GRAPH: GIVEN, verified,
when the caller resolved it; else the single same-store candidate.  Zero
candidates: nothing.  Several: ENDPOINT-LINK-SKIPPED and nothing."
  (let ((target
          (cond (given (%verify-endpoint-node given namespace key))
                (t (let ((cands (%same-store-candidates graph namespace
                                                        key)))
                     (cond ((null cands) nil)
                           ((cdr cands)
                            (warn 'endpoint-link-skipped
                                  :claim claim :namespace namespace
                                  :key key
                                  :classes (remove-duplicates
                                            (mapcar (lambda (n)
                                                      (class-name
                                                       (class-of n)))
                                                    cands)))
                            nil)
                           (t (first cands))))))))
    (when target
      (funcall ctor :from claim :to (graph-db:id target) :graph graph))))

(defun %link-claim-at-write (claim &key subject-node object-node)
  "Link CLAIM's endpoints in its own store, inside the caller's open
transaction (spec sec.4.1-4.2).  Returns CLAIM.  Never signals an ERROR
for the derived edge; ENDPOINT-MISMATCH is the caller's, not the edge's."
  (let ((graph (graph-db:node-graph claim)))
    (%link-endpoint claim graph #'make-subject-of
                    (claim-subject-namespace claim)
                    (claim-subject-key claim) subject-node)
    (when (slot-exists-p claim 'object-key)
      (%link-endpoint claim graph #'make-object-of
                      (claim-object-namespace claim)
                      (claim-object-key claim) object-node))
    claim))

(defun %strip-endpoint-node-args (args)
  "ARGS without :SUBJECT-NODE / :OBJECT-NODE, which the raw constructor
must never see."
  (let ((copy (copy-list args)))
    (remf copy :subject-node)
    (remf copy :object-node)
    copy))
```

- [ ] **Step 5: Load it**

In `graph-db.asd`, `graph-db/spacetime` components, after `(:file "resolve")` add:

```lisp
               ;; Edges under claims: write-time linking (GH #369).
               ;; After RESOLVE for NAMESPACE-SOURCES / SOURCE-CONTRACT.
               (:file "link")
```

- [ ] **Step 6: Change the wrapper**

In `spacetime/claim.lisp`, the `mapcar` lambda inside `def-claim-classes` (the form beginning `(let ((%raw (fdefinition ',ctor)))`). Replace the inner lambda:

```lisp
                       (lambda (&rest args)
                         (%check-claim-identity args ',identity-keys)
                         (let ((args (%claim-encode-extent-arg args)))
                           ,@(when temporal
                               `((unless (getf args :extent-sexp)
                                   (error 'missing-claim-identity-component
                                          :slot :extent))))
                           (let ((c (apply %raw
                                          (%claim-encode-version-stamp
                                           (%claim-encode-transaction-arg
                                            args)))))
                             (check-standing (claim-standing c))
                             c)))
```
with
```lisp
                       (lambda (&rest args)
                         (%check-claim-identity args ',identity-keys)
                         ;; Endpoint nodes the caller resolved (spec
                         ;; sec.4.2); stripped before the raw ctor.
                         (let* ((subject-node (getf args :subject-node))
                                (object-node (getf args :object-node))
                                (args (%strip-endpoint-node-args
                                       (%claim-encode-extent-arg args))))
                           ,@(when temporal
                               `((unless (getf args :extent-sexp)
                                   (error 'missing-claim-identity-component
                                          :slot :extent))))
                           (let ((c (apply %raw
                                          (%claim-encode-version-stamp
                                           (%claim-encode-transaction-arg
                                            args)))))
                             (check-standing (claim-standing c))
                             ;; Derived edges, in the same transaction
                             ;; (GH #369, spec sec.4.1).
                             (%link-claim-at-write
                              c :subject-node subject-node
                                :object-node object-node))))
```

Add to the `def-claim-classes` docstring, after the `:TEMPORAL T` paragraph:

```
Every MAKE-<NAME> also accepts :SUBJECT-NODE and :OBJECT-NODE, endpoint
nodes the caller resolved BEFORE its transaction (RESOLVE-ENDPOINT);
each is verified and linked, and a same-store endpoint not given is
linked automatically (GH #369, spec sec.4).  Linking never fails the
write; ENDPOINT-MISMATCH on a wrong node does.
```

- [ ] **Step 7: Run the six tests**

Fresh image. Run each of `auto-link-in-two-transactions`, `auto-link-in-one-transaction`, `key-only-when-nothing-resolves`, `ambiguity-warns-once-and-writes-unlinked`, `caller-resolved-cross-store-endpoint-is-linked`, `caller-resolved-mismatch-refuses-the-write`. Expected: all PASS.

If `auto-link-in-one-transaction` fails with the subject unlinked: `view-writes` returns `tx-write` records; confirm `(graph-db:id w)` works on them (it does for `%overlay-transaction`), and that `(slot-value n 'thing-id)` reads the pending node's data (the `node-class` `slot-value-using-class :around`, `primitive-node.lisp:506`). If `slot-value` returns unbound on a pending node, read through the class's accessor instead: `(funcall (fdefinition accessor) n)` is not available generically, so fall back to `(cdr (assoc (intern (symbol-name slot) :keyword) (graph-db::data n)))` — node data is an alist keyed by keyword (memory: "Node data is an ALIST").

- [ ] **Step 8: Run the whole spacetime suite** (the runner). Expected: no `Fail:`, count ≥ baseline + new checks. The existing families (`ct-claim`, `kr-claim`, register's, temporal's) now run the linker on every write: none of their namespaces has a registered source in their graph, so every existing claim stays key-only — that is the "consumers keep working unchanged" property (spec §10) and this run is its test.

- [ ] **Step 9: Commit**

```bash
git add spacetime/conditions.lisp spacetime/link.lisp spacetime/claim.lisp \
        graph-db.asd tests/spacetime/endpoint-edge-tests.lisp
git commit -m "feat(spacetime): link claim endpoints at write -- same-store auto-link, caller-resolved nodes, ENDPOINT-MISMATCH (#369)"
```

---

### Task 4: `claim-endpoints` and `node-claims`

**Files:**
- Modify: `spacetime/claim-query.lisp` (extract `%narrow-claims`; add the two functions after `claims-touching`)
- Test: `tests/spacetime/endpoint-edge-tests.lisp`

**Interfaces:**
- Produces: `(claim-endpoints claim &key (graph (graph-db:node-graph claim))) => (values subject-node object-node)`; `(node-claims node &key (graph (graph-db:node-graph node)) family (role :either) current relation at during as-of as-of-epoch limit offset) => (values claims more-p)`; internal `(%narrow-claims graph claims &key current probe relation as-of as-of-epoch limit offset)`.
- Note for the spec's Built note (Task 8): `node-claims` takes `:graph`, the store holding the claims, defaulting to the node's own store. The spec's §6.1 "each claim family's graph" is not derivable — a family is registered by class, and a class is instantiable in any store (#167) — so the caller names the claim store when it differs; in the one-store deployment the default is right.

- [ ] **Step 1: Write the failing tests**

Append:

```lisp
(test claim-endpoints-answers-the-linked-nodes
  (with-ee-graph (g)
    (let (s o c u)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction ()
        (setq c (ee-b))
        (setq u (make-ee-claim-unary :subject-namespace :ee-things
                                     :subject-key "t-1" :relation "u"
                                     :producer "p" :standing :inferred)))
      (multiple-value-bind (cs co) (claim-endpoints c)
        (is-true (same-node-p s cs))
        (is-true (same-node-p o co)))
      (multiple-value-bind (us uo) (claim-endpoints u)
        (is-true (same-node-p s us))
        (is (null uo)))
      (let ((k (with-transaction () (ee-b :subject "nobody" :object "x"))))
        (is (equal '(nil nil) (multiple-value-list (claim-endpoints k))))))))

(test node-claims-is-the-adjacency-twin-of-claims-touching
  "Spec sec.6.1: same filters, same meaning; linked claims only."
  (with-ee-graph (g)
    (let (s o c1 c2 c3)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction ()
        (setq c1 (ee-b :relation "likes"))
        (setq c2 (ee-b :relation "knows"))
        ;; T-2 as the SUBJECT of a claim about a key-only object.
        (setq c3 (ee-b :subject "t-2" :object "elsewhere" :relation "r")))
      (flet ((ids (claims) (sort (mapcar (lambda (c) (graph-db:string-id
                                                        (id c)))
                                          claims)
                                 #'string<)))
        (is (equal (ids (list c1 c2)) (ids (node-claims s))))
        (is (equal (ids (list c1 c2 c3)) (ids (node-claims o))))
        (is (equal (ids (list c3)) (ids (node-claims o :role :subject))))
        (is (equal (ids (list c1 c2)) (ids (node-claims o :role :object))))
        (is (equal (ids (list c1))
                   (ids (node-claims s :relation "likes"))))
        (is (equal (ids (list c1 c2))
                   (ids (node-claims s :family 'ee-claim))))
        (is (null (node-claims s :family 'ct-claim)))
        ;; A key-only claim is absent here and present in CLAIMS-TOUCHING.
        (with-transaction () (ee-b :subject "ghost"))
        (is (null (node-claims s :relation "ghost")))
        (is (= 1 (length (claims-touching g 'ee-claim :ee-things "ghost"))))
        ;; Pagination.
        (multiple-value-bind (page more) (node-claims s :limit 1)
          (is (= 1 (length page)))
          (is-true more))))))

(test retraction-keeps-the-edges-and-current-filters
  "Spec R3 / sec.4.3: RETRACT-CLAIM touches no edge; :CURRENT hides it."
  (with-ee-graph (g)
    (let (s c)
      (with-transaction () (setq s (ee-thing "t-1")) (ee-thing "t-2"))
      (with-transaction () (setq c (ee-b)))
      (retract-claim c)
      (is (= 1 (length (node-claims s))))
      (is (null (node-claims s :current t)))
      (is-true (same-node-p s (claim-endpoints c))))))

(test regeneration-drops-the-old-edges-and-links-the-new
  "Spec sec.4.3: delete, then insert; ACTIVE-EDGE-P hides the deleted
claim's edges, the new claim links at write."
  (with-ee-graph (g)
    (let (s c2)
      (with-transaction () (setq s (ee-thing "t-1")) (ee-thing "t-2"))
      (with-transaction () (ee-b :producer "gen"))
      (is (= 1 (length (node-claims s))))
      (is (= 1 (delete-claims-by-producer g 'ee-claim "gen")))
      (is (null (node-claims s)))
      (with-transaction () (setq c2 (ee-b :producer "gen")))
      (is (= 1 (length (node-claims s))))
      (is-true (same-node-p s (claim-endpoints c2))))))
```


- [ ] **Step 2: Run one to verify it fails**

`claim-endpoints-answers-the-linked-nodes` — expected: undefined function `claim-endpoints`.

- [ ] **Step 3: Extract the filter tail of `claims-touching`**

In `spacetime/claim-query.lisp`, before `(defun claims-touching`, add:

```lisp
(defun %narrow-claims (graph claims &key current probe relation
                                         as-of as-of-epoch limit offset)
  "The shared tail of CLAIMS-TOUCHING and NODE-CLAIMS (GH #369): resolve
the transaction axis (:AS-OF / :AS-OF-EPOCH), then filter currency
(:CURRENT), validity (PROBE, an extent), and RELATION, then paginate.
A REAPED-CLAIM survives every filter -- it is the record that a version
existed, not a candidate to judge (GH #300)."
  (let ((all claims))
    (cond (as-of
           (setf all (loop for c in all
                           for v = (%claim-as-of graph c as-of)
                           when v collect v)))
          (as-of-epoch
           (setf all (loop for c in all
                           for v = (%claim-as-of-epoch graph c as-of-epoch)
                           when v collect v))))
    (when current
      (setf all (remove-if-not (lambda (c)
                                 (or (reaped-claim-p c) (claim-current-p c)))
                               all)))
    (when probe
      (setf all (remove-if-not (lambda (c)
                                 (or (reaped-claim-p c)
                                     (%claim-validity-touches-p c probe)))
                               all)))
    (when relation
      (setf all (remove-if-not (lambda (c)
                                 (or (reaped-claim-p c)
                                     (equal relation (claim-relation c))))
                               all)))
    (%paginate all limit offset)))
```

Then in `claims-touching`, replace everything from `;; The overlay is for the neither-axis arm only` through `(%paginate all limit offset))))` with:

```lisp
      ;; The overlay is for the neither-axis arm only: an uncommitted
      ;; write has no epoch (#347 recon C6).
      (unless (or as-of as-of-epoch)
        (setf all (%overlay-transaction
                   graph all family
                   (lambda (c)
                     (or (and (member role '(:subject :either))
                              (equal namespace (claim-subject-namespace c))
                              (equal key (claim-subject-key c))
                              (or (null relation)
                                  (equal relation (claim-relation c))))
                         (and (member role '(:object :either))
                              (typep c (claim-family-binary family))
                              (equal namespace (claim-object-namespace c))
                              (equal key (claim-object-key c))))))))
      ;; The subject side already rode the relation index (GH #302); only
      ;; the object side still needs the filter.
      (%narrow-claims graph all
                      :current current :probe probe
                      :relation (and (member role '(:object :either))
                                     relation)
                      :as-of as-of :as-of-epoch as-of-epoch
                      :limit limit :offset offset))))
```

Run `claim-query-tests` + `epoch-tests` + `temporal-tests` (they exercise `claims-touching`) — quickest is the whole spacetime suite via the runner. Expected: identical pass count to Task 3's run.

- [ ] **Step 4: Add the two reads**

After `claims-touching`, add:

```lisp
;;; Edges under claims: the adjacency reads (GH #369, spec sec.6.1).
;;; Both see LINKED claims only; CLAIMS-TOUCHING is the complete read.

(defun claim-endpoints (claim &key (graph (graph-db:node-graph claim)))
  "CLAIM's linked endpoint nodes: (VALUES SUBJECT-NODE OBJECT-NODE), from
its outgoing SUBJECT-OF / OBJECT-OF edges in GRAPH (its own store).  NIL
for an endpoint that is not linked -- key-only, or a unary claim's
object.  A cross-store endpoint is read through LOOKUP-VERTEX-ANYWHERE,
so it may be an UNRESOLVED-NODE marker while that store is detached.
Edges created in a still-open transaction are not visible until it
commits (adjacency is indexed at commit apply)."
  (flet ((endpoint (type)
           (let ((e (first (graph-db:outgoing-edges claim :graph graph
                                                          :edge-type type))))
             (when e
               (graph-db:lookup-vertex-anywhere (graph-db:to e))))))
    (values (endpoint 'subject-of) (endpoint 'object-of))))

(defun node-claims (node &key (graph (graph-db:node-graph node))
                              family (role :either) current relation
                              at during as-of as-of-epoch limit offset)
  "Claims linked to NODE, from its incoming SUBJECT-OF / OBJECT-OF edges
in GRAPH -- the adjacency twin of CLAIMS-TOUCHING, with the same
filters and the same meaning for each (GH #369, spec sec.6.1).  Linked
claims only: a key-only claim is not here.  FAMILY (a parent class
name) restricts to one family; default every family.

GRAPH is the store holding the CLAIMS, not necessarily NODE's own --
adjacency is indexed in the edge's store (edge.lisp, ADD-TO-VE-INDEX) --
and defaults to NODE's store, which is the answer in a one-store
deployment; pass the claim store when the two differ.  Inside an open
transaction, only committed adjacency is visible; CLAIMS-TOUCHING is the
read that sees the transaction's own writes (GH #324)."
  (check-type role (member :subject :object :either))
  (check-type at (or null local-time:timestamp))
  (check-type during (or null temporal-extent))
  (check-type as-of-epoch (or null unsigned-byte))
  (when (and at during)
    (error "Pass only one of :AT or :DURING, not both."))
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
  (when as-of-epoch (%refuse-epoch-axis graph))
  (let ((probe (cond (at (make-instant (exact-bound at)))
                     (during during)))
        (parent (and family (claim-family-parent (claim-family family))))
        (claims '()))
    (flet ((collect (type)
             (graph-db:map-edges
              (lambda (e)
                (let ((c (graph-db:lookup-vertex (graph-db:from e)
                                                 :graph graph)))
                  (when (and c
                             (or (null parent) (typep c parent))
                             (not (find (graph-db:id c) claims
                                        :key #'graph-db:id :test #'equalp)))
                    (push c claims))))
              graph :vertex node :direction :in :edge-type type)))
      (when (member role '(:subject :either)) (collect 'subject-of))
      (when (member role '(:object :either)) (collect 'object-of)))
    (%narrow-claims graph (nreverse claims)
                    :current current :probe probe :relation relation
                    :as-of as-of :as-of-epoch as-of-epoch
                    :limit limit :offset offset)))
```

- [ ] **Step 5: Run the four new tests and the suite**

Expected: all PASS; runner shows no `Fail:`.

- [ ] **Step 6: Commit**

```bash
git add spacetime/claim-query.lisp tests/spacetime/endpoint-edge-tests.lisp
git commit -m "feat(spacetime): CLAIM-ENDPOINTS and NODE-CLAIMS over the endpoint edges; %NARROW-CLAIMS shared with CLAIMS-TOUCHING (#369)"
```

---

### Task 5: `traverse` within a store, pinned

**Files:**
- Test: `tests/spacetime/endpoint-edge-tests.lisp`

No engine change; these pin what spec §6.2 and the deferred §8 build on. `traverse` collects nothing without `:edge-type` (`traverse.lisp:83`, `(typep edge edge-type)`), so every call passes one.

- [ ] **Step 1: Write the tests**

```lisp
(defun ee-ids (nodes)
  (sort (mapcar (lambda (n) (format nil "~A" (id n))) nodes) #'string<))

(test traverse-walks-source-to-source-through-a-claim
  "Spec sec.6.2: T-1 <-subject-of- C -object-of-> T-2, both directions."
  (with-ee-graph (g)
    (let (s o c)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction () (setq c (ee-b)))
      (let ((reached (graph-db:traverse
                      s :graph g :direction :both
                      :edge-type '(or subject-of object-of))))
        (is (equal (ee-ids (list c o)) (ee-ids reached))))
      (is (null (graph-db:traverse s :graph g :direction :both)))
      ;; From the claim, both endpoints are one hop out.
      (is (equal (ee-ids (list s o))
                 (ee-ids (graph-db:traverse
                          c :graph g :direction :out
                          :edge-type '(or subject-of object-of))))))))

(test traverse-lands-a-cross-store-endpoint-without-walking-past-it
  "Spec sec.11 (pinned for #368): a caller-resolved endpoint in another
store lands in the results -- the vertex while its store is open, the
UNRESOLVED-NODE marker once it is closed -- and nothing beyond it is
walked."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (let (n c)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1"))
          ;; A neighbour in the far store that a continuation WOULD reach.
          (make-st-report :headline "two" :report-id "r-2"))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            (setq c (ee-b :object-namespace :st-reports :object "r-1"
                          :object-node n))))
        (let ((open (graph-db:traverse c :graph g :direction :out
                                         :edge-type 'object-of)))
          (is (= 1 (length open)))
          (is-true (same-node-p n (first open))))
        (close-graph sg :snapshot-p nil)
        (let ((closed (graph-db:traverse c :graph g :direction :out
                                           :edge-type 'object-of)))
          (is (= 1 (length closed)))
          (is-true (graph-db:unresolved-node-p (first closed))))))))
```

`with-source-graph` closes `sg` again in its cleanup under `ignore-errors`, so the explicit close is safe.

- [ ] **Step 2: Run both**

Expected: PASS. If the second test's closed branch yields NIL instead of a marker, read `tests/store-resolver-tests.lisp:63-76` for the registry precondition (the store must be registered under the run's `*system-directory*`, which `make-graph` under `run-spacetime-tests` does) and fix the fixture, not the assertion.

- [ ] **Step 3: Commit**

```bash
git add tests/spacetime/endpoint-edge-tests.lisp
git commit -m "test(spacetime): pin TRAVERSE over claim edges in-store and at a cross-store endpoint (#369)"
```

---

### Task 6: `related/3` and `claimed/4`

**Files:**
- Create: `spacetime/functors.lisp`
- Modify: `graph-db.asd` (`graph-db/spacetime` components: `(:file "functors")` after `(:file "temporal")`)
- Test: `tests/spacetime/endpoint-edge-tests.lisp`

**Interfaces:**
- Consumes: `claim-current-p`, `claim-relation`, `*claim-families*`, the edge classes.
- Produces: functors `related/3 (subject relation object)` and `claimed/4 (claim subject relation object)` in `graph-db.spacetime`, registered in `graph-db::*prolog-global-functors*`.

- [ ] **Step 1: Write the failing tests**

```lisp
(defun ee-q (g text &rest keys)
  "RUN-GUARDED-PROLOG on G; rows as bound (:RAW)."
  (nth-value 1 (apply #'graph-db.query:run-guarded-prolog text g
                      :format :raw keys)))

(defun ee-seed (g)
  "T-1 likes T-2 and knows T-3; T-2 likes T-3; one retracted T-1 hates T-3.
G is *GRAPH* already (WITH-EE-GRAPH binds it); taken for the call site's
readability."
  (declare (ignorable g))
  (let (c)
    (with-transaction ()
      (ee-thing "t-1") (ee-thing "t-2") (ee-thing "t-3"))
    (with-transaction ()
      (ee-b :subject "t-1" :object "t-2" :relation "likes")
      (ee-b :subject "t-1" :object "t-3" :relation "knows")
      (ee-b :subject "t-2" :object "t-3" :relation "likes")
      (setq c (ee-b :subject "t-1" :object "t-3" :relation "hates")))
    (retract-claim c)
    c))

(defun ee-labels (rows col)
  (sort (mapcar (lambda (r) (ee-label (nth col r))) rows) #'string<))

(test related-solves-from-a-bound-subject-current-only
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(is-a ?s ee-thing) (node-slot-value ?s thing-id \"t-1\")
                         (related ?s ?r ?o)")))
      (is (= 2 (length rows)))
      (is (equal '("knows" "likes") (sort (mapcar #'second rows) #'string<)))
      (is (equal '("t-2" "t-3") (ee-labels rows 2))))))

(test related-solves-from-a-bound-object
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(is-a ?o ee-thing) (node-slot-value ?o thing-id \"t-3\")
                         (related ?s ?r ?o)")))
      ;; knows(t-1,t-3), likes(t-2,t-3); hates is retracted.
      (is (= 2 (length rows)))
      (is (equal '("t-1" "t-2") (ee-labels rows 0))))))

(test related-scans-by-relation-when-only-it-is-bound
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(related ?s \"likes\" ?o)")))
      (is (= 2 (length rows)))
      (is (equal '("t-1" "t-2") (ee-labels rows 0))))))

(test related-refuses-an-unbounded-scan
  (with-ee-graph (g)
    (ee-seed g)
    (signals graph-db.query:prolog-ill-typed-error
      (ee-q g "(related ?s ?r ?o)"))))

(test claimed-exposes-the-claim-and-keeps-history
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(is-a ?s ee-thing) (node-slot-value ?s thing-id \"t-1\")
                         (claimed ?c ?s ?r ?o)")))
      ;; likes, knows, and the RETRACTED hates.
      (is (= 3 (length rows)))
      (is (every (lambda (r) (typep (first r) 'ee-claim)) rows))
      (is (equal '("hates" "knows" "likes")
                 (sort (mapcar #'third rows) #'string<))))))

(test a-cross-store-endpoint-is-linked-but-does-not-unify
  "Spec sec.8 as amended: the functors resolve with LOOKUP-VERTEX on the
claim's graph, so the foreign endpoint yields no row in #367."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (let (n)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1")))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            (ee-b :object-namespace :st-reports :object "r-1"
                  :object-node n)))
        (is (null (ee-q g "(is-a ?s ee-thing) (related ?s ?r ?o)")))
        ;; The same-store half still solves.
        (is (= 1 (length (ee-q g "(is-a ?s ee-thing) (subject-of ?c ?s)"))))))))
```

- [ ] **Step 2: Run one to verify it fails**

`related-solves-from-a-bound-subject-current-only` — expected: `prolog-guard-error` "related/3 is not a registered Prolog functor" (or similar refusal).

- [ ] **Step 3: Write the functors**

Create `spacetime/functors.lisp`:

```lisp
;;;; Prolog goals over claim topology (GH #369, spec sec.6.3).
;;;;
;;;; Both resolve endpoints with LOOKUP-VERTEX on the CLAIM's graph
;;;; (*GRAPH*, which the guarded runner binds): a caller-resolved
;;;; cross-store endpoint is linked but does not unify here; #368 is
;;;; the unit that changes that (spec sec.8).  Unary claims have no
;;;; object and are not solutions of either goal; SUBJECT-OF/2 reaches
;;;; them.

(in-package #:graph-db.spacetime)

(defun %binary-claim-p (claim)
  (slot-exists-p claim 'object-key))

(defun %claim-linked-endpoint (claim type graph)
  "The vertex CLAIM's TYPE edge points at, in GRAPH only; NIL when
unlinked or when the endpoint lives in another store."
  (let ((e (first (graph-db:outgoing-edges claim :graph graph
                                                 :edge-type type))))
    (when e (graph-db:lookup-vertex (graph-db:to e) :graph graph))))

(defun %solve-claims (claim subject relation object require-current cont)
  "The engine behind RELATED/3 and CLAIMED/4.  Arguments are already
VAR-DEREFed; CLAIM is NIL for RELATED/3.  The bound endpoint drives the
scan; with only RELATION bound, every family's CLAIM-RELATION index;
with nothing bound, QUERY-PRECONDITION-ERROR (the guard renders it as a
refusal), as an unbounded generator is refused elsewhere."
  (let ((graph graph-db:*graph*))
    (flet ((try (c)
             (when (and (%binary-claim-p c)
                        (or (not require-current) (claim-current-p c)))
               (let ((s (%claim-linked-endpoint c 'subject-of graph))
                     (o (%claim-linked-endpoint c 'object-of graph)))
                 (when (and s o)
                   (let ((old-trail (fill-pointer graph-db:*trail*)))
                     (when (and (graph-db:unify subject s)
                                (graph-db:unify relation (claim-relation c))
                                (graph-db:unify object o)
                                (or (null claim) (graph-db:unify claim c)))
                       (funcall cont))
                     (graph-db:undo-bindings old-trail))))))
           (claims-into (vertex type)
             (graph-db:map-edges
              (lambda (e)
                (let ((c (graph-db:lookup-vertex (graph-db:from e)
                                                 :graph graph)))
                  (when c (try c))))
              graph :vertex vertex :direction :in :edge-type type)))
      (cond ((not (graph-db::var-p subject))
             (when (graph-db::vertex-p subject)
               (claims-into subject 'subject-of)))
            ((not (graph-db::var-p object))
             (when (graph-db::vertex-p object)
               (claims-into object 'object-of)))
            ((stringp relation)
             (maphash (lambda (parent family)
                        (declare (ignore family))
                        (dolist (c (handler-case
                                       (graph-db:index-lookup
                                        graph parent '(relation) relation)
                                     (graph-db:query-precondition-error ()
                                       nil)))
                          (try c)))
                      *claim-families*))
            (t (error 'graph-db:query-precondition-error
                      :reason "related/claimed: bind the subject, the ~
object or the relation; a scan over every claim is refused"))))))

(graph-db:def-global-prolog-functor related/3 (subject relation object cont)
  "(related ?subject ?relation ?object): SUBJECT and OBJECT are the
endpoint nodes of a CURRENT binary claim, RELATION its canonical string.
Any argument may be unbound; the bound endpoint drives the scan, a bound
relation alone rides the CLAIM-RELATION index, nothing bound is refused.
Linked claims only; endpoints are looked up in the claim's graph, so a
cross-store endpoint does not unify (GH #369, spec sec.6.3, sec.8)."
  (%solve-claims nil (graph-db:var-deref subject)
                 (graph-db:var-deref relation)
                 (graph-db:var-deref object) t cont))

(graph-db:def-global-prolog-functor claimed/4 (claim subject relation object
                                                cont)
  "(claimed ?claim ?subject ?relation ?object): as RELATED/3 with the
claim node exposed and NO currency filter -- retracted claims answer,
for history and for reading provenance off the claim in the same query.
Same scan rules and the same cross-store caveat (GH #369, spec sec.6.3)."
  (%solve-claims (graph-db:var-deref claim) (graph-db:var-deref subject)
                 (graph-db:var-deref relation)
                 (graph-db:var-deref object) nil cont))
```


- [ ] **Step 4: Load it**

In `graph-db.asd`, `graph-db/spacetime` components, after `(:file "temporal")` (make `temporal` lose its closing paren):

```lisp
               (:file "temporal")
               ;; RELATED/3 and CLAIMED/4 over the endpoint edges (GH
               ;; #369); needs claim-query's CLAIM-CURRENT-P.
               (:file "functors")))
```

- [ ] **Step 5: Run the six functor tests**

Fresh image. Expected: all PASS. If `related-scans-by-relation-when-only-it-is-bound` returns 0 rows, confirm the `claim-relation` index name resolves through `'(relation)` the way `claims-touching` resolves `'(subject-namespace subject-key relation)` (slot lists, not names); if the guard refuses `related` as "not a registered Prolog functor", check `(gethash 'graph-db.spacetime::related/3 graph-db::*prolog-global-functors*)` in the image and that `spacetime/functors.lisp` loaded.

- [ ] **Step 6: Run the runner** (both suites). Expected: no `Fail:`; query suite unchanged in count.

- [ ] **Step 7: Commit**

```bash
git add spacetime/functors.lisp graph-db.asd tests/spacetime/endpoint-edge-tests.lisp
git commit -m "feat(spacetime): RELATED/3 and CLAIMED/4 over the endpoint edges (#369)"
```

---

### Task 7: Existing consumers unchanged

**Files:** none new.

- [ ] **Step 1: Run the runner** one more time from a fresh image and compare `Did N checks` against Task 1: spacetime ≥ baseline + every new check; query = baseline.
- [ ] **Step 2: Load `examples/example-spacetime.lisp` in a fresh image** (it is smoke-tested by CI): `sbcl --dynamic-space-size 4096 --non-interactive --eval '(push #p"./" asdf:*central-registry*)' --eval '(ql:quickload :graph-db/spacetime :silent t)' --load examples/example-spacetime.lisp` — expected: exits 0 with no `ENDPOINT-` condition in the output.
- [ ] **Step 3: Record both in the ledger.** No commit.

---

### Task 8: Docs travel with the code

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` (Chapter 18: amend the paragraph at `:5433-5441` beginning "The endpoints a claim names"; insert a new `***` section immediately before `*** Vocabulary: what a family names (GH #350)`, currently line 5910)
- Modify: `CHANGELOG.md` (`## [Unreleased]` → `### Added`, a new first bullet)
- Modify: `docs/superpowers/specs/2026-09-12-edges-under-claims-design.md` (Built note)
- Modify: `docs/ci.md` (the tripwire paragraph)

- [ ] **Step 1: Amend the manual's claims paragraph**

Replace the sentence `Resolving ~(namespace, key)~ back into an actual node is deliberately left to a later add-on; see below.` with:

```
Resolving ~(namespace, key)~ back into an actual node is the source
onboarding contract's ~resolve-endpoint~ (below); since 4.1 a claim also
carries a *derived* edge to each endpoint that resolves in its own
store — see "Edges under claims".
```

- [ ] **Step 2: Add the new section** before `*** Vocabulary: what a family names (GH #350)`:

```org
*** Edges under claims: derived adjacency to the endpoints (GH #369)

The ~(namespace, key)~ slots stay the truth and ~claims-touching~ stays the
complete read. Beside them, a claim written into a store that also holds
its endpoint gains one edge per endpoint — ~subject-of~ to its subject's
node, ~object-of~ to its object's — so the engine's adjacency, ~traverse~
and Prolog can walk claim topology without an index probe per hop. Both
edge classes are shipped by ~graph-db/spacetime~, have no slots and no
default store, and are always placed in the claim's own store.

Linking happens in the constructor, inside your transaction:

- An endpoint whose namespace has a registered source (~def-source~) in
  the *same* store is looked up by its identity key — through the index,
  and through the transaction's own uncommitted creates, so a source and
  a claim about it written together still link. Exactly one node: an
  edge. None: the claim is key-only, as before. Several: no edge and an
  ~endpoint-link-skipped~ warning.
- An endpoint anywhere else is the caller's to resolve *before* the
  transaction (~resolve-endpoint~ refuses to run inside one); pass it as
  ~:subject-node~ / ~:object-node~ and the engine verifies it is a source
  of that namespace with that key — ~endpoint-mismatch~, an error, if not.

Linking never fails a claim write. Retraction keeps the edges (the
record of what was believed); reads filter by currency. Deleting a
claim or a source hides the edges through ~active-edge-p~, and
~compact-edges~ reclaims them.

Reads over the edges — ~claim-endpoints~, ~node-claims~ (the adjacency twin
of ~claims-touching~, same filters), ~(traverse node :edge-type '(or
subject-of object-of) ...)~, and the goals ~(related ?s ?r ?o)~ (current
claims) and ~(claimed ?c ?s ?r ?o)~ (with history) — see *linked claims
only*. Two facts to keep in mind: adjacency is indexed in the *edge's*
store, so ~node-claims~ takes ~:graph~, the claim store, defaulting to the
node's own; and the Prolog goals resolve endpoints in the claim's graph,
so a caller-resolved endpoint in another store is linked but does not
unify until cross-store continuation lands (GH #368). Backfilling a store
written before 4.1 is the sweep, ~link-claim-endpoints~ (GH #367 U2).
```

- [ ] **Step 3: CHANGELOG**

Under `## [Unreleased]` / `### Added`, as the first bullet:

```markdown
- **Edges under claims** (#367, U1 #369): a claim gains a derived
  `subject-of` / `object-of` edge to each endpoint that resolves in its
  own store, linked in the constructor (same-store sources through the
  index plus the transaction's own creates; anywhere else through the
  caller-resolved `:subject-node` / `:object-node`, verified). New reads
  `claim-endpoints`, `node-claims`, and the Prolog goals `related/3`
  (current) and `claimed/4` (history); conditions `endpoint-link-skipped`
  (warning) and `endpoint-mismatch`. The key slots remain the identity
  and `claims-touching` the complete read; no store format change.
  Cross-store traversal continuation is deferred (#368).
  `docs/superpowers/specs/2026-09-12-edges-under-claims-design.md`.
```

- [ ] **Step 4: The spec's Built note**

Append to the spec, before `## 14. Traceability`:

```markdown
## Built — U1 (#369, <SHA of Task 6's commit>)

§3, §4.1–4.3, §6.1–6.3, §7 and §11's in-store bullets, on
`feat/edges-under-claims`. Two deviations from §13's file list, both
recorded here: the write-time linker lives in `spacetime/link.lisp`
(U2's sweep joins it) because it needs `namespace-sources`, which loads
after `claim.lisp`; and `node-claims` takes `:graph`, the store holding
the claims, defaulting to the node's own — §6.1's "each claim family's
graph" is not derivable, since a family is registered by class and a
class is instantiable in any store (#167). Unary claims are not
solutions of `related/3` / `claimed/4` (no object); `subject-of/2`
reaches them. `claims-touching`'s filter tail is now `%narrow-claims`,
shared with `node-claims`.
```

- [ ] **Step 5: `docs/ci.md`**

In the "Each lane is its own sbcl process" bullet, after the sentence ending `(docs/rules.md, GH #330).`, add:

```
  The same holds for `graph-db/spacetime`'s six (`related/3`,
  `claimed/4`, `subject-of/2`, `subject-of/3`, `object-of/2`,
  `object-of/3`; GH #369): absent from the gui lane, present in any
  image that loads spacetime beside the gui.
```

- [ ] **Step 6: Check line lengths in the Lisp files touched**

```bash
cd /home/raison/work/vg-367
awk 'length > 80 {print FILENAME": "FNR": "length}' spacetime/link.lisp \
  spacetime/functors.lisp spacetime/conditions.lisp spacetime/claim.lisp \
  spacetime/claim-query.lisp tests/spacetime/endpoint-edge-tests.lisp
grep -nP '\t' spacetime/link.lisp spacetime/functors.lisp \
  tests/spacetime/endpoint-edge-tests.lisp
```
Expected: no output from either. Fix any hit before committing.

- [ ] **Step 7: Commit**

```bash
git add docs/vivace-graph-v3-doc.org CHANGELOG.md docs/ci.md \
        docs/superpowers/specs/2026-09-12-edges-under-claims-design.md
git commit -m "docs: edges under claims U1 -- manual ch.18, changelog, spec Built note, ci tripwire note (#369)"
```

---

### Task 9: Hand-off

- [ ] **Step 1: Final runner** from a fresh image; both suites green; counts in the ledger.
- [ ] **Step 2: Update #369** with a comment: the commits, the two deviations from §13 (link.lisp; `node-claims :graph`), the counts, and "ready for PR". Do NOT push; pushing is Kevin's call (memory: pushes are explicit-only, batched).
- [ ] **Step 3: Tell Kevin** the branch is ready to push and that CI will run the full matrix; U2 (the sweep) is the next plan.

## Self-review against the spec

- §3 classes, no default store, placement, not identity, weight default, keep-revisions default, version floor → Task 2 (floor is a consequence of adoption; documented in Task 8).
- §4.1 steps 1–3, "linking never fails", same-transaction visibility, class filter → Task 3.
- §4.2 verify + link, precedence over §4.1 → Task 3 (`%link-endpoint` checks GIVEN first).
- §4.3 retraction / update / regeneration → Task 4 tests; no code (engine behaviour).
- §6.1 `claim-endpoints`, `node-claims` → Task 4. §6.2 traverse → Task 5. §6.3 functors → Task 6.
- §7 conditions → Task 3.
- §11 in-store bullets: classes ✓ T2; auto-link one/two txns ✓ T3; key-only ✓ T3 + T4; ambiguity ✓ T3; caller-resolved + mismatch ✓ T3; retraction ✓ T4 + T6 (`related` no, `claimed` yes); regeneration ✓ T4; traversal in-store + pinned cross-store landing ✓ T5; functors in-store + cross-store non-unification ✓ T6; existing suites ✓ T7. "an as-of read before the retraction answers" — covered by `%narrow-claims` reuse; add an `:as-of` assertion to `retraction-keeps-the-edges-and-current-filters` if time allows (`(node-claims s :as-of <timestamp before retraction> :current t)` → 1).
- §10 consumers unchanged → Task 7.
- §13 files: `traverse.lisp` and `prolog-functors.lisp` untouched (U3 deferred) ✓; deviations recorded in Task 8.
- Names consistent across tasks: `%link-claim-at-write`, `%strip-endpoint-node-args`, `%narrow-claims`, `claim-endpoints`, `node-claims`, `%solve-claims`, `%claim-linked-endpoint`, `ee-b`, `ee-thing`, `same-node-p`, `ee-linked-to`, `ee-q`, `ee-seed`, `ee-labels`, `ee-ids`.
