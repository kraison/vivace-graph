# Counting Index (#361) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A generic counting index maintained at commit apply, so a claim family's vocabulary — namespaces, keys, relations, with counts of all and of current claims — is a lookup instead of a walk (GH #361).

**Architecture:** `def-count-index` declares, on the `def-unique` pattern (own registry, own graph slot, own sidecar), an ordered map keyed `(depth v1 … vk)` for every leading prefix of a node's canonical tuple, whose value is the counter pair `(all . current)`. A fourth maintenance pass beside the secondary-index pass adjusts the counters on create, update (predicate flip or tuple move) and delete, in the commit apply, the two replication applies and the peer purge; a re-apply (crash-recovery replay, device re-pull) marks the maps stale and a scan rebuilds them. The three spacetime vocabulary functions read the maps outside an as-of extent and keep #350's walk inside one.

**Tech Stack:** SBCL, FiveAM, `graph-db` core (`count-index.lisp`, new), `graph-db/spacetime`.

**Spec:** `docs/superpowers/specs/2026-09-08-count-index-design.md` (as amended at af1cf01). **Facts:** `docs/superpowers/notes/2026-09-08-count-index-engine-facts.md` — every `file:line` below was verified there at 4a2e645; match on the quoted forms, not the numbers. Read its §X and §G before any task.

## Global Constraints

- Lisp: spaces only, hard 80 columns on every line, terse comments naming GH #361 or a spec section; docstrings state what/returns/trap.
- Branch `feat/count-index` from `experiment` (837b460), worktree `<worktree>` = `/home/raison/work/vg-c3/.worktrees/count-index` (clone `/home/raison/work/vg-c3`). Never build in a shared checkout.
- Never run `pkill`, `pgrep -f`, or `kill`. One SBCL build at a time in this worktree. Never the full 15-minute suite by hand; the suites below only. CI runs the full suite on push.
- No node-format change; the secondary sidecar `secondary-indexes.dat` is untouched (spec §2.4). The count index has its own registry, graph slot, sidecar, comparator and key codec (R2, facts X1/X4/X5).
- The count map's keys have NO trailing id: comparator `%index-value-lessp`, key-equal `equal`, one-element sentinels `(:gmin)`/`(:gmax)`, key codec `serialize`/`deserialize` (facts X4/X5). Never `%index-comp-lessp`, `%index-equal`, `%index-head-key`, `%index-tail-key` or `%index-key-serialize` on a count key.
- Every counter step is read-then-branch: `find-in-skip-list`, then `add-to-skip-list` when absent, else `update-in-skip-list` with the OLD value as its fourth argument (facts X2, B8). No lock of the pass's own (R9, facts X3).
- Under `*add-to-indexes-unless-present-p*` the pass counts nothing and sets the stale flag (R8, facts X7). `tx-delete` is a `tx-update` subclass: all three methods, and the delete subtracts once (facts C12).
- Every new `graph-db` symbol a `graph-db/test` test uses unqualified is added to `tests/package.lisp`'s `:import-from #:graph-db` list (facts G16); spacetime tests write `graph-db:` for engine symbols.
- Existing tests keep passing; the baseline check counts recorded in Task 1 never drop; every #350 vocabulary test passes unchanged (R5).
- Docs travel with the code (Task 7); every commit message names GH #361.
- Commit trailers on every commit:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_013BfdWuKGebCvLBKsbU753o
  ```

## Running the suites

Write once to `/home/raison/work/vg-c3-notes/vg-361-suites.lisp` (outside every checkout). Until Task 2 creates `count-index-suite`, leave it out of the list (an unknown suite name signals).

```lisp
;; The suites #361 touches, CI-style, in a fresh image.
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
         (dolist (s '(index-suite count-index-suite peer-index-suite))
           (let ((r (fiveam:run s)))
             (fiveam:explain! r)
             (format t "~&== ~a ~a~%" s
                     (if (fiveam:results-status r) "PASS" "FAIL"))
             (unless (fiveam:results-status r) (setf ok nil))))
         (unless (graph-db/spacetime-test::run-spacetime-tests)
           (setf ok nil)))
    (graph-db-test-scratch:cleanup-scratch-run))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root, foreground, timeout 600000 ms (a background run is killed by the harness's memory heuristic):

```bash
cd /home/raison/work/vg-c3/.worktrees/count-index
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3-notes/vg-361-suites.lisp \
  > /home/raison/work/vg-c3-notes/vg-361-suites.log 2>&1; echo "exit=$?"
grep -E "loaded from|Did [0-9]+ checks|Fail:|^== " /home/raison/work/vg-c3-notes/vg-361-suites.log
```

A single test while iterating (for a spacetime test, quickload both systems and `in-package :graph-db/spacetime-test`, keeping the `let*`):

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(in-package :graph-db/test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

Always read back `Did N checks`. Fresh image after any export change (facts G18).

## Code facts every task relies on

- Registry pattern to copy: `index-spec`/`*schema-index-metadata*`/`register-index-spec`/`unregister-index-spec`/`%registered-index-specs`/`%index-spec-declared-p` (`index.lisp:87-211`), `%spec-identity` (`:99-115`, one identity rule across registries), `%withdrawn-p (withdrawn kind owner graph-name name slots)` (`:132-144`), `%applicable-index-descriptors` (`:213-225`), `%slot-present-p` (`:213`), `%normalize-slots`, `%resolve-index-canonicalizers spec arity` (`:38-60`), `%tuple-indexable-p node slot-names` (`:446-454`), `%index-tuple-key` (`:428-444`, the write-side tuple builder), `%index-key` (`:405-426`, the query-side one; NIL for a full-arity all-null tuple), `%ix-prefix-out` (`:1062-1066`).
- Graph slots: `secondary-indexes` at `graph-class.lisp:203`, `unique-indexes` at `:171`; the hash-table constructor with the per-implementation `:synchronized`/`:shared` readers is in `%slot-index-for` (`index.lisp:559-583`).
- Backends: `make-heap-index backend heap comparison &key head-key tail-key key-equal key-serializer key-deserializer` (`bplus-tree.lisp:1014`), `open-heap-index backend &key address heap comparison key-equal key-serializer key-deserializer` (`:1054`), both giving `serialize`/`deserialize` as the value codec; `make-mem-skip-list &key key-comparison key-equal value-equal head-key head-value tail-key tail-value duplicates-allowed-p` (`mem-skip-list.lisp:43`); `make-secondary-skip-list` and `%open-secondary-skip-list` (`index.lisp:272-316`) are the templates. `graph-index-backend`, `(indexes graph)` is the heap. Generics: `find-in-skip-list (sl key &optional preds succs)` → node or NIL, `add-to-skip-list (sl key value)`, `update-in-skip-list (sl key value &optional old-value)`, `remove-from-skip-list (sl key &optional value)`; `make-range-cursor`/`cursor-next`/`%sn-key`/`%sn-value` as in #350. `view-index-p`, `view-index-address`, `view-index-backend-tag`, `delete-view-index` (`bplus-tree.lisp:979-997`).
- Maintenance: `apply-tx-write-to-secondary-indexes` and its three methods (`index.lisp:611-628`); call sites `transactions.lisp:2008` (inside `apply-transaction`, after unique, before `reap-old-versions`), `peer-streaming.lisp:1140` and `:1177`; `peer-purge-node`'s `%ix-release` block (`peer-streaming.lisp:1443-1445`). Write objects: `tx-create` (`node`), `tx-update` (`node`, `old-node`), `tx-delete` (subclass of `tx-update`); data slots readable at apply. `*add-to-indexes-unless-present-p*` (`transactions.lisp:38-42`).
- Persistence: `save-secondary-index-roots`/`restore-secondary-index-roots` (`index.lisp:673-816`, records `(owner slot-names address backend)`, `cl-store:restore`, `%atomic-cl-store object path` from `graph.lisp:30`), `%ensure-index-built`/`install-secondary-indexes`/`%build-index-for-spec` (`:843-880`), `regenerate-secondary-indexes` (`:818-841`); open sequence `graph.lisp:1045-1057`, `crash-recovery-p` at `:994` inside a `let` that closes before the restores, the spatial `(when crash-recovery-p (rebuild-spatial-indexes graph))` at `:1043-1044`; close `graph.lisp:1253-1254`; memory graph `memory-graph.lisp:1116-1125` (`lazy-p` skips builds).
- Spacetime: `def-claim-classes`' five `def-index` forms at `spacetime/claim.lisp:440-453`, `home`/`unary`/`binary` at `:360-363`; the vocabulary section `spacetime/claim-query.lisp:563-795` (`%refuse-vocabulary-axis`, `%vocabulary-sources`, `%vocabulary-key`, `%vocabulary-view`, `%view-resolve`, `%claim-tuple`, `%created-under`, `%name-admitted-p`, `%name-count`, `%walk-names`, `%name-lessp`, `%merge-names`, the three functions); `%paginate` (`:246-254`); `claim-current-p` (`:469-474`); `retract-claim` writes a `tx-update` whose tuple is unchanged and whose predicate flips; `graph-db::%as-of-snapshot graph` (`transactions.lisp:3544`, unexported); `graph-db::*transaction*`; `graph-db:make-commit-view`/`view-node`/`view-writes`/`view-old-node`; `graph-db::node`/`graph-db::old-node` on writes.
- Tests: `tests/index-tests.lisp` (`with-ix-graph (g &key backend)`, `with-ix-memory-graph`, class `ix-claim (ns key rel)` with two secondary indexes, `%count-seeks` probe at `:928-945`, reopen shape `:540-554`, build-at-open shape `:1035-1044`, reclaim shape `:889-921`); `tests/peer-index-tests.lisp` (`with-pi-device`, `pi-authored-create`, direct `apply-peer-authored-op`/`apply-peer-create-writes`/`apply-peer-purge` calls, suite `peer-index-suite`, graph name `*pi-graph-name*`); `tests/spacetime/vocabulary-tests.lisp` (`%ns-u`, `%ns-b`, ten #350 tests, the as-of test at `:99-117`); `graph-db.asd:721` (`index-tests`), `:731` (`peer-index-tests`), `:615` (`vocabulary-tests`); `tests/package.lisp:276-286` imports.

---

### Task 1: Baseline

**Files:**
- Create: `/home/raison/work/vg-c3-notes/vg-361-suites.lisp` (the runner above, without `count-index-suite`)

- [ ] **Step 1: Write the runner and run it on the unchanged branch**

Expected: `exit=0`, `graph-db loaded from` the worktree, `index-suite PASS`, `peer-index-suite PASS`, a passing spacetime run.

- [ ] **Step 2: Record the counts**

Copy every `Did N checks` line into the SDD ledger as the baseline. No later run may report fewer for a suite.

---

### Task 2: Declaration, map and counter primitives

**Files:**
- Create: `count-index.lisp`
- Modify: `graph-db.asd` (`graph-db/core` components: `(:file "count-index" :depends-on ("index"))` right after the `"index"` entry at ~139)
- Modify: `graph-class.lisp` (after `secondary-indexes`, ~203)
- Modify: `package.lisp` (~415 and ~465-467)
- Modify: `tests/package.lisp` (~276-286), `graph-db.asd` (`graph-db/test` components after `"index-tests"` at ~721)
- Test: `tests/count-index-tests.lisp` (new)

**Interfaces:**
- Produces: `*schema-count-metadata*`; struct `count-index-spec` (owner-name slot-names graph-name canonicalize name current-p) with `count-index-spec-identity`; `register-count-index-spec`, `unregister-count-index-spec (owner graph-name &key slot-names name)`, `%registered-count-index-specs`, `%count-index-spec-declared-p`, `%applicable-count-index-specs class graph`; macros `def-count-index`, `undef-count-index`; graph slots `count-indexes`, `count-indexes-stale-p`; struct `count-index` (owner-name slot-names canonicalizers current-p skip-list); `make-count-skip-list graph`; `%count-index-for graph spec`; `%count-tuple cix node`; `%current-p cix node` → 0/1; `%count-adjust cix components d-all d-current`; `%count-node cix node d-all d-current`; `%count-refresh graph` (calls `rebuild-count-indexes` when stale; Task 2 defines placeholder `rebuild-count-indexes` and `%ensure-count-index-built`, replaced in Task 3); `%require-count-index graph class-name slot-name`; `count-index-lookup`; `map-count-index`. Exports: `def-count-index`, `undef-count-index`, `unregister-count-index-spec`, `count-index-lookup`, `map-count-index`.

- [ ] **Step 1: Write the failing tests**

Create `tests/count-index-tests.lisp`:

```lisp
;;;; tests/count-index-tests.lisp -- the counting index (GH #361).

(in-package #:graph-db/test)

(def-suite count-index-suite
  :description "Counting index (def-count-index): per-prefix counters."
  :in graph-db-suite)

(in-suite count-index-suite)

(defun ix-live-p (node)
  "The CURRENT-P of the test count index: a claim whose REL is \"dead\"
is not current."
  (not (equal (ix-rel node) "dead")))

;; Two count indexes on IX-CLAIM, which already carries two secondary
;; indexes on (ns key rel) and (ns key): the two kinds coexist on one
;; owner and one slot list (facts X1).
(def-count-index ix-claim (ns key) :graph-db-index-test
  :name ix-count-ns-key :current-p ix-live-p)
(def-count-index ix-claim (rel) :graph-db-index-test :name ix-count-rel)

(defun %cix (g slots)
  "The built COUNT-INDEX for IX-CLAIM.SLOTS in G."
  (graph-db::%require-count-index g 'ix-claim slots))

(defun %count-of (g slots tuple)
  "(ALL CURRENT) for TUPLE, as a list."
  (multiple-value-list (count-index-lookup g 'ix-claim slots tuple)))

(defun %entries (g slots &key (depth 1) prefix)
  "MAP-COUNT-INDEX's calls as a list of (COMPONENTS ALL CURRENT)."
  (let ((out '()))
    (map-count-index (lambda (c a n) (push (list c a n) out))
                     g 'ix-claim slots :depth depth :prefix prefix)
    (nreverse out)))

(test count-node-counts-every-prefix
  "Spec §2.2-2.3: %COUNT-NODE adds a node's contribution at every
leading prefix of its tuple; a duplicate tuple is a second contribution;
CURRENT follows CURRENT-P; an index without CURRENT-P keeps ALL only."
  (with-ix-graph (g)
    (let (nodes)
      (with-transaction ()
        (setq nodes (list (make-ix-claim :ns "ops" :key "e1" :rel "at")
                          (make-ix-claim :ns "ops" :key "e1" :rel "dead")
                          (make-ix-claim :ns "ops" :key "e2" :rel "at")
                          (make-ix-claim :ns "hr" :key "p1" :rel "at"))))
      (let ((ck (%cix g '(ns key))) (cr (%cix g '(rel))))
        (dolist (n nodes)
          (graph-db::%count-node ck n 1 (graph-db::%current-p ck n))
          (graph-db::%count-node cr n 1 (graph-db::%current-p cr n))))
      (is (equal '(3 2) (%count-of g '(ns key) '("ops"))))
      (is (equal '(2 1) (%count-of g '(ns key) '("ops" "e1"))))
      (is (equal '(1 1) (%count-of g '(ns key) '("hr"))))
      (is (equal '(0 0) (%count-of g '(ns key) '("none"))) "absent: 0 0")
      (is (equal '(3 nil) (%count-of g '(rel) "at"))
          "no CURRENT-P: ALL only, CURRENT is NIL")
      (is (equal '((("hr") 1 1) (("ops") 3 2)) (%entries g '(ns key)))
          "depth 1, index order (hr < ops)")
      (is (equal '((("ops" "e1") 2 1) (("ops" "e2") 1 1))
                 (%entries g '(ns key) :depth 2 :prefix '("ops"))))
      (is (equal '((("at") 3 nil) (("dead") 1 nil)) (%entries g '(rel)))))))

(test count-adjust-removes-a-key-at-zero-and-moves-current
  "Spec R4, §2.3: subtracting a node's contribution removes the key when
ALL reaches 0 and leaves a sibling; a current-only step moves CURRENT."
  (with-ix-graph (g)
    (let (a b)
      (with-transaction ()
        (setq a (make-ix-claim :ns "ops" :key "e1" :rel "at")
              b (make-ix-claim :ns "ops" :key "e2" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck a 1 1)
        (graph-db::%count-node ck b 1 1)
        (graph-db::%count-node ck a -1 -1)
        (is (equal '(0 0) (%count-of g '(ns key) '("ops" "e1"))))
        (is (equal '(("ops" "e2")) (mapcar #'first
                                           (%entries g '(ns key) :depth 2
                                                     :prefix '("ops"))))
            "the emptied key is gone, not zero")
        (is (equal '(1 1) (%count-of g '(ns key) '("ops"))))
        (graph-db::%count-adjust ck '("ops") 0 -1)
        (is (equal '(1 0) (%count-of g '(ns key) '("ops")))
            "a predicate flip moves CURRENT and leaves ALL")))))

(test count-index-null-component-and-refusals
  "Spec §2.2, §2.5: a null component is stored and read back as NIL and
sorts first; an all-null full tuple answers 0 0; an undeclared index
signals; a depth or prefix beyond the arity signals."
  (with-ix-graph (g)
    (let (n)
      (with-transaction ()
        (setq n (make-ix-claim :ns "ops" :key nil :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck n 1 1))
      (is (equal '(1 1) (%count-of g '(ns key) '("ops" nil))))
      (is (equal '((("ops" nil) 1 1))
                 (%entries g '(ns key) :depth 2 :prefix '("ops"))))
      (is (equal '(0 0) (%count-of g '(ns key) '(nil nil))))
      (signals query-precondition-error
        (count-index-lookup g 'ix-claim '(key) "x"))
      (signals query-precondition-error
        (%entries g '(ns key) :depth 3))
      (signals query-precondition-error
        (%entries g '(ns key) :depth 1 :prefix '("a" "b"))))))

(test count-adjust-crosses-a-serialization-boundary
  "Facts G5: SERIALIZE's integer width grows at 256 and at 65536; the
counter keeps counting across the first boundary on the heap backend
(in place with OLD-VALUE, or remove+add) and reads back right."
  (with-ix-graph (g)
    (let (n)
      (with-transaction ()
        (setq n (make-ix-claim :ns "big" :key "k" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (dotimes (i 300) (graph-db::%count-node ck n 1 1)))
      (is (equal '(300 300) (%count-of g '(ns key) '("big" "k")))))))

(test count-index-on-the-memory-and-bplus-backends
  "Facts X2: the memory backend's UPDATE-IN-SKIP-LIST does not insert, so
the FIRST increment of a name must go through ADD-TO-SKIP-LIST; the B+
tree backend round-trips the counter pair too."
  (with-ix-memory-graph (g)
    (let (n)
      (with-transaction () (setq n (make-ix-claim :ns "m" :key "k" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck n 1 1)
        (graph-db::%count-node ck n 1 1))
      (is (equal '(2 2) (%count-of g '(ns key) '("m"))))))
  (with-ix-graph (g :backend :bplus-tree)
    (let (n)
      (with-transaction () (setq n (make-ix-claim :ns "b" :key "k" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck n 1 1)
        (graph-db::%count-node ck n 1 0))
      (is (equal '(2 1) (%count-of g '(ns key) '("b" "k")))))))

(test def-count-index-registers-by-name-and-undef-withdraws
  "Spec §2.1: a named declaration replaces in place; UNDEF-COUNT-INDEX
withdraws it (and warns when nothing matched, GH #152); the built map
of a withdrawn declaration is reclaimed at the next open (Task 4)."
  (let ((before (length (gethash :graph-db-index-test
                                 graph-db::*schema-count-metadata*))))
    (def-count-index ix-claim (ns) :graph-db-index-test :name ix-count-tmp)
    (def-count-index ix-claim (ns) :graph-db-index-test :name ix-count-tmp)
    (is (= (1+ before) (length (gethash :graph-db-index-test
                                        graph-db::*schema-count-metadata*)))
        "re-declaring by name replaces, not pushes")
    (is (eq t (undef-count-index ix-claim :graph-db-index-test
                                 :name ix-count-tmp)))
    (is (= before (length (gethash :graph-db-index-test
                                   graph-db::*schema-count-metadata*))))
    (signals schema-withdrawal-matched-nothing
      (undef-count-index ix-claim :graph-db-index-test :name ix-count-tmp))))
```

Register the file in `graph-db.asd`'s `graph-db/test` components after `(:file "index-tests")`. Add to `tests/package.lisp` after `#:index-count`:

```lisp
                #:def-count-index            ; GH #361
                #:undef-count-index
                #:count-index-lookup
                #:map-count-index
```

`schema-withdrawal-matched-nothing` and `query-precondition-error` are already imported (check).

- [ ] **Step 2: Run to verify they fail**

Run `count-node-counts-every-prefix`. Expected: a load failure on the unexported `def-count-index` (the file's top-level declaration cannot read). That is the RED.

- [ ] **Step 3: Implement**

`graph-class.lisp`, after the `secondary-indexes` slot:

```lisp
   ;; Counting indexes (DEF-COUNT-INDEX, GH #361): (owner . slot-names) ->
   ;; COUNT-INDEX; STALE-P is set by a re-applying apply and cleared by
   ;; REBUILD-COUNT-INDEXES (spec R8).  See count-index.lisp.
   (count-indexes :accessor count-indexes :initarg :count-indexes
                  :initform nil)
   (count-indexes-stale-p :accessor count-indexes-stale-p :initform nil)
```

`count-index.lisp`:

```lisp
;;;; count-index.lisp -- a counting index: a counter pair per leading
;;;; prefix of a node's tuple, maintained at commit apply, so a listing
;;;; with counts is a lookup (GH #361).  Spec: docs/superpowers/specs/
;;;; 2026-09-08-count-index-design.md.  Parallel to index.lisp on the
;;;; DEF-UNIQUE pattern: own registry, own graph slot, own sidecar,
;;;; id-free keys (facts note §X).

(in-package #:graph-db)

;;; ---------------------------------------------------------------------------
;;; Registry (spec §2.1)
;;; ---------------------------------------------------------------------------

(defvar *schema-count-metadata* (make-hash-table)
  "graph-name -> list of COUNT-INDEX-SPECs (newest first): the
DEF-COUNT-INDEX registry, reconciled at open by INSTALL-COUNT-INDEXES.")

(defstruct (count-index-spec (:constructor make-count-index-spec))
  owner-name slot-names graph-name canonicalize name current-p)

(defun count-index-spec-identity (spec)
  "See %SPEC-IDENTITY: one identity rule across registries (GH #140)."
  (%spec-identity (count-index-spec-owner-name spec)
                  (count-index-spec-slot-names spec)
                  (count-index-spec-name spec)))

(defun register-count-index-spec (spec)
  "Record SPEC, replacing one of the same identity in place."
  (let* ((g (count-index-spec-graph-name spec))
         (id (count-index-spec-identity spec))
         (existing (gethash g *schema-count-metadata*))
         (hit (find id existing :key #'count-index-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-count-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-count-index-spec (owner-name graph-name
                                    &key slot-names name)
  "Withdraw the declaration identified by (OWNER . NAME) or (OWNER .
SLOT-NAMES); T if one was withdrawn, NIL silently otherwise."
  (let* ((id (%spec-identity owner-name (%normalize-slots slot-names) name))
         (existing (gethash graph-name *schema-count-metadata*))
         (hit (find id existing :key #'count-index-spec-identity
                                :test #'equal)))
    (when hit
      (setf (gethash graph-name *schema-count-metadata*)
            (remove hit existing))
      t)))

(defun %registered-count-index-specs (graph)
  "Count specs registered for GRAPH, de-duped by identity, newest wins."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (spec (gethash (graph-name graph) *schema-count-metadata*))
      (let ((k (count-index-spec-identity spec)))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push spec result))))
    (nreverse result)))

(defun %count-spec-for (owner-name slot-names graph)
  "The live count spec covering (OWNER-NAME . SLOT-NAMES) for GRAPH, or
NIL.  Count indexes come only from DEF-COUNT-INDEX (no MOP arm), so
absence is positive evidence to drop a sidecar record."
  (find-if (lambda (s)
             (and (eq (count-index-spec-owner-name s) owner-name)
                  (equal (count-index-spec-slot-names s) slot-names)))
           (%registered-count-index-specs graph)))

(defun %count-index-spec-declared-p (owner-name slot-names graph)
  (and (%count-spec-for owner-name slot-names graph) t))

(defun %applicable-count-index-specs (class graph)
  "The count specs applying to CLASS: the owner is CLASS or an ancestor
and every slot exists in CLASS (as %APPLICABLE-INDEX-DESCRIPTORS)."
  (when (class-finalized-p class)
    (loop for spec in (%registered-count-index-specs graph)
          when (and (subtypep (class-name class)
                              (count-index-spec-owner-name spec))
                    (every (lambda (s) (%slot-present-p class s))
                           (count-index-spec-slot-names spec)))
          collect spec)))

(defmacro def-count-index (owner-class slots graph-name
                           &key name current-p canonicalize)
  "Declare a counting index on OWNER-CLASS.SLOTS in GRAPH-NAME: a
counter pair (ALL . CURRENT) per leading prefix of each node's tuple,
maintained at commit apply (GH #361).  CURRENT-P names a one-argument
predicate, funcalled at maintenance; NIL keeps ALL only.  Declarative
and idempotent like DEF-INDEX: registers, builds now if the graph is
open.  Trap: counts are live at commit granularity (spec R1)."
  `(let ((spec (make-count-index-spec
                :owner-name ',owner-class
                :slot-names (%normalize-slots ',slots)
                :graph-name ',graph-name
                :name ',name
                :current-p ',current-p
                :canonicalize ,(when canonicalize `',canonicalize))))
     (register-count-index-spec spec)
     (let ((g (lookup-graph ',graph-name)))
       (when g (%ensure-count-index-built g spec)))
     spec))

(defmacro undef-count-index (owner-class graph-name &key slots name)
  "Withdraw a DEF-COUNT-INDEX declaration; warns when nothing matched
(GH #152).  The built map is reclaimed at the next open."
  `(%withdrawn-p (unregister-count-index-spec ',owner-class ',graph-name
                                             :slot-names ',slots
                                             :name ',name)
                 :count-index ',owner-class ',graph-name ',name ',slots))

;;; ---------------------------------------------------------------------------
;;; The map (spec §2.2)
;;; ---------------------------------------------------------------------------

(defstruct (count-index (:constructor %make-count-index))
  owner-name slot-names canonicalizers current-p skip-list)

(defgeneric make-count-skip-list (graph)
  (:documentation "The ordered map backing a count index: keys (DEPTH
V1 ... VK) with no trailing id, under %INDEX-VALUE-LESSP / EQUAL, plain
SERIALIZE codec (GH #361, facts X4/X5).  A MEM-SKIP-LIST on a memory
graph.")
  (:method ((graph graph))
    (make-heap-index (graph-index-backend graph) (indexes graph)
                     '%index-value-lessp
                     :head-key (list +min-sentinel+)
                     :tail-key (list +max-sentinel+)
                     :key-equal 'equal
                     :key-serializer 'serialize
                     :key-deserializer 'deserialize))
  (:method ((graph memory-graph-mixin))
    (make-mem-skip-list :key-equal 'equal
                        :key-comparison '%index-value-lessp
                        :value-equal 'equal
                        :head-key (list +min-sentinel+) :head-value nil
                        :tail-key (list +max-sentinel+) :tail-value nil
                        :duplicates-allowed-p nil)))

(defun %open-count-skip-list (graph address backend)
  "Reopen a persisted count map at ADDRESS (Task 4's restore)."
  (open-heap-index backend :address address :heap (indexes graph)
                   :comparison '%index-value-lessp :key-equal 'equal
                   :key-serializer 'serialize :key-deserializer 'deserialize))

(defun %count-registry (graph)
  (or (count-indexes graph)
      (setf (count-indexes graph)
            (make-hash-table :test 'equal
                             #+sbcl :synchronized #+sbcl t
                             #+ccl :shared #+ccl t
                             #+graph-db-ecl-sync-hash :synchronized
                             #+graph-db-ecl-sync-hash t))))

(defun %count-index-for (graph spec)
  "Get-or-create the COUNT-INDEX for SPEC in GRAPH, keyed
(owner . slot-names) in the count registry (separate from the secondary
one, so both kinds may share an owner and slots -- facts X1)."
  (let* ((reg (%count-registry graph))
         (slot-names (count-index-spec-slot-names spec))
         (key (cons (count-index-spec-owner-name spec) slot-names)))
    (or (gethash key reg)
        (let ((cix (%make-count-index
                    :owner-name (count-index-spec-owner-name spec)
                    :slot-names slot-names
                    :canonicalizers (%resolve-index-canonicalizers
                                     (count-index-spec-canonicalize spec)
                                     (length slot-names))
                    :current-p (count-index-spec-current-p spec))))
          (setf (count-index-skip-list cix) (make-count-skip-list graph))
          (setf (gethash key reg) cix)))))

;;; ---------------------------------------------------------------------------
;;; Counter steps (spec §2.3)
;;; ---------------------------------------------------------------------------

(defun %count-tuple (cix node)
  "NODE's canonical components for CIX, +NULL-COMPONENT+ for a null;
NIL when every component is null or one is a geometry (nothing to
count) -- the write-side mirror of %INDEX-TUPLE-KEY."
  (let ((slots (count-index-slot-names cix)))
    (when (%tuple-indexable-p node slots)
      (let* ((cans (count-index-canonicalizers cix))
             (any nil)
             (key (loop for s in slots
                        for i from 0
                        for v = (slot-value node s)
                        collect (cond ((null v) +null-component+)
                                      (t (setf any t)
                                         (let ((c (nth i cans)))
                                           (if c (funcall c v) v)))))))
        (when any key)))))

(defun %current-p (cix node)
  "1 when CIX's CURRENT-P holds on NODE, else 0 (0 with no predicate)."
  (let ((p (count-index-current-p cix)))
    (if (and p (funcall p node)) 1 0)))

(defun %count-adjust (cix components d-all d-current)
  "Add D-ALL / D-CURRENT to the pair under COMPONENTS in CIX: create the
key, remove it when ALL reaches 0 (R4).  Read then branch --
UPDATE-IN-SKIP-LIST inserts on the on-disk backends and not on the
memory one -- and pass the old value so the update stays in place
(facts X2, B8).  No lock of its own (R9)."
  (let* ((sl (count-index-skip-list cix))
         (key (cons (length components) (copy-list components)))
         (node (find-in-skip-list sl key))
         (old (and node (%sn-value node)))
         (all (+ (if old (car old) 0) d-all))
         (current (and (count-index-current-p cix)
                       (+ (if old (or (cdr old) 0) 0) d-current))))
    (cond ((<= all 0)
           (when node (remove-from-skip-list sl key)))
          ((null node)
           (add-to-skip-list sl key (cons all current)))
          (t (update-in-skip-list sl key (cons all current) old)))))

(defun %count-node (cix node d-all d-current)
  "Apply D-ALL / D-CURRENT for NODE at every leading prefix of its
tuple; nothing for a tuple that is not countable."
  (let ((tuple (%count-tuple cix node)))
    (when tuple
      (loop for k from 1 to (length tuple)
            do (%count-adjust cix (subseq tuple 0 k) d-all d-current)))))

;;; ---------------------------------------------------------------------------
;;; Queries (spec §2.5)
;;; ---------------------------------------------------------------------------

(defun rebuild-count-indexes (graph)
  "Placeholder until Task 3: clear the stale flag."
  (setf (count-indexes-stale-p graph) nil)
  graph)

(defun %ensure-count-index-built (graph spec)
  "Placeholder until Task 3: create the (empty) map for SPEC."
  (%count-index-for graph spec))

(defun %count-refresh (graph)
  "Rebuild the count maps when a re-apply left them stale (R8)."
  (when (count-indexes-stale-p graph)
    (rebuild-count-indexes graph)))

(defun %require-count-index (graph class-name slot-name)
  "The COUNT-INDEX on CLASS-NAME.SLOT-NAME -- CLASS-NAME's own or an
ancestor's -- or NIL when declared but not built (a lazy memory graph,
a declaration with no node yet); QUERY-PRECONDITION-ERROR when none is
declared."
  (let* ((slot-names (%normalize-slots slot-name))
         (reg (count-indexes graph))
         (class (find-class class-name nil)))
    (or (and reg
             (or (gethash (cons class-name slot-names) reg)
                 (and class (class-finalized-p class)
                      (loop for c in (cdr (class-precedence-list class))
                            for hit = (gethash (cons (class-name c)
                                                     slot-names)
                                               reg)
                            when hit return hit))))
        (if (some (lambda (s)
                    (and (subtypep class-name
                                   (count-index-spec-owner-name s))
                         (equal (count-index-spec-slot-names s)
                                slot-names)))
                  (%registered-count-index-specs graph))
            nil
            (error 'query-precondition-error
                   :reason (format nil "No count index on ~S.~S in ~S"
                                   class-name slot-name
                                   (graph-name graph)))))))

(defun %count-query-key (cix value)
  "VALUE -- a value or a component list of at most CIX's arity -- as a
canonical prefix, NIL mapped to +NULL-COMPONENT+; NIL for a full-arity
all-null tuple (as %INDEX-KEY).  Signals on more than the arity."
  (let* ((arity (length (count-index-slot-names cix)))
         (cans (count-index-canonicalizers cix))
         (vals (if (listp value) value (list value)))
         (any nil)
         (key (loop for v in vals
                    for i from 0
                    collect (cond ((null v) +null-component+)
                                  (t (setf any t)
                                     (let ((c (nth i cans)))
                                       (if c (funcall c v) v)))))))
    (when (> (length vals) arity)
      (error 'query-precondition-error
             :reason (format nil "Count index on ~S has arity ~D; got ~D"
                             (count-index-slot-names cix) arity
                             (length vals))))
    (when (or any (< (length vals) arity)) key)))

(defun count-index-lookup (graph class-name slot-name tuple)
  "(VALUES ALL CURRENT) for TUPLE -- a full tuple or a leading prefix --
in the count index on CLASS-NAME.SLOT-NAME: 0 0 for an absent name, a
declared-but-empty index, or an all-null full tuple; CURRENT is NIL when
the index has no CURRENT-P.  Signals QUERY-PRECONDITION-ERROR when no
count index is declared.  Trap: live at commit granularity; a re-apply
is repaired by a rebuild first (GH #361)."
  (let ((*graph* graph))
    (%count-refresh graph)
    (let ((cix (%require-count-index graph class-name slot-name)))
      (if (null cix)
          (values 0 0)
          (let ((key (%count-query-key cix tuple)))
            (if (null key)
                (values 0 0)
                (let ((node (find-in-skip-list
                             (count-index-skip-list cix)
                             (cons (length key) key))))
                  (if (null node)
                      (values 0 0)
                      (let ((v (%sn-value node)))
                        (values (car v) (cdr v)))))))))))

(defun map-count-index (fn graph class-name slot-name
                        &key (depth 1) prefix)
  "Call FN with (COMPONENTS ALL CURRENT) for each entry at DEPTH of the
count index on CLASS-NAME.SLOT-NAME whose leading components equal
PREFIX, in index order, a null component read back as NIL.  Zero calls
for a declared-but-empty index; signals when none is declared, or on a
DEPTH or PREFIX beyond the arity.  Trap: not an atomic snapshot, live at
commit granularity (GH #361)."
  (let ((*graph* graph))
    (%count-refresh graph)
    (let ((cix (%require-count-index graph class-name slot-name)))
      (when cix
        (let ((arity (length (count-index-slot-names cix)))
              (pre (and prefix (%count-query-key cix prefix))))
          (unless (<= 1 depth arity)
            (error 'query-precondition-error
                   :reason (format nil "Count index on ~S has arity ~D; ~
cannot list depth ~D" (count-index-slot-names cix) arity depth)))
          (when (> (length pre) depth)
            (error 'query-precondition-error
                   :reason (format nil "A :PREFIX of ~D component(s) at ~
depth ~D" (length pre) depth)))
          (unless (and prefix (null pre)) ; all-null full prefix: nothing
            (let* ((lo (cons depth pre))
                   (hi (append (cons depth pre)
                               (make-list (- depth (length pre))
                                          :initial-element +max-sentinel+)))
                   (cur (make-range-cursor (count-index-skip-list cix)
                                           lo hi)))
              (loop for node = (cursor-next cur :eoc)
                    until (eql node :eoc)
                    do (let ((k (%sn-key node)) (v (%sn-value node)))
                         (funcall fn (%ix-prefix-out (rest k) depth)
                                  (car v) (cdr v)))))))))))
```

Check `(find-in-skip-list sl key)` returns NIL (not a sentinel) for an absent key on each backend before relying on `(null node)`; if the heap method returns the tail or a non-matching node, compare `(%sn-key node)` with `equal` and treat a mismatch as absent — say which in the report. If `%ix-prefix-out` is defined later in the load order than `count-index.lisp` needs it, it is not: `index.lisp` loads first (`:depends-on ("index")`).

`graph-db.asd`, `graph-db/core`: `(:file "count-index" :depends-on ("index"))` after the `"index"` component. `package.lisp`: `#:def-count-index #:undef-count-index` after `#:def-index` (~415); `#:unregister-count-index-spec` on the `#:unregister-index-spec` line (~465); `#:count-index-lookup #:map-count-index   ; GH #361` after the `#:map-index-prefixes #:index-count` line (~467).

- [ ] **Step 4: Run the six tests, then `count-index-suite`, `index-suite`**

Expected: PASS; `index-suite` unchanged. Keep every line ≤ 80 columns (awk check on `count-index.lisp graph-class.lisp package.lisp tests/count-index-tests.lisp tests/package.lisp graph-db.asd`).

- [ ] **Step 5: Commit** — `feat(index): a counting index -- declaration, map, counters, lookups (#361)` with trailers.

---

### Task 3: Maintenance at commit apply, replication and purge; rebuild and the stale flag

**Files:**
- Modify: `count-index.lisp` (replace the placeholders `rebuild-count-indexes` and `%ensure-count-index-built`; add the pass, build/install, purge release)
- Modify: `transactions.lisp` (~2008), `peer-streaming.lisp` (~1140, ~1177, ~1445)
- Test: `tests/count-index-tests.lisp` (append)

**Interfaces:**
- Consumes: Task 2.
- Produces: `apply-tx-write-to-count-indexes` (generic, three methods), `apply-tx-writes-to-count-indexes writes graph`, `%count-purge node graph`, `%build-count-index-for-spec graph spec`, `%ensure-count-index-built graph spec`, `install-count-indexes graph`, `rebuild-count-indexes graph` (real).

- [ ] **Step 1: Write the failing tests**

```lisp
(defvar *ix-resolutions* nil
  "Node resolutions counted by %COUNT-RESOLUTIONS; NIL when not counting.")

(defun %count-resolutions (thunk)
  "Run THUNK counting LOOKUP-VERTEX calls through an :AROUND method
removed afterwards (the generic; an FDEFINITION swap would miss it).
Returns the count.  Callers prove the probe live with a control."
  (let* ((gf #'lookup-vertex)
         (method (eval '(defmethod lookup-vertex :around
                            ((id t) &key &allow-other-keys)
                          (when *ix-resolutions* (incf *ix-resolutions*))
                          (call-next-method)))))
    (unwind-protect
         (let ((*ix-resolutions* 0))
           (funcall thunk)
           *ix-resolutions*)
      (remove-method gf method))))

(test commits-maintain-the-counters
  "Spec §2.3: a committed create counts at every prefix; a retraction-
shaped update (predicate flip, tuple unchanged) moves CURRENT only; an
update that changes an indexed slot moves both counters to the new
tuple and removes the emptied old key; a delete subtracts once."
  (with-ix-graph (g)
    (let (a b)
      (with-transaction ()
        (setq a (id (make-ix-claim :ns "ops" :key "e1" :rel "at"))
              b (id (make-ix-claim :ns "ops" :key "e2" :rel "at"))))
      (is (equal '(2 2) (%count-of g '(ns key) '("ops"))))
      (is (equal '(2 nil) (%count-of g '(rel) "at")))
      (with-transaction ()
        (let ((c (copy (lookup-vertex a))))
          (setf (ix-rel c) "dead")
          (save c)))
      (is (equal '(2 1) (%count-of g '(ns key) '("ops")))
          "the flip moved CURRENT and left ALL")
      (is (equal '(1 nil) (%count-of g '(rel) "at")))
      (is (equal '(1 nil) (%count-of g '(rel) "dead"))
          "on the (rel) index the same update is a tuple move")
      (with-transaction ()
        (let ((c (copy (lookup-vertex b))))
          (setf (ix-ns c) "hr")
          (save c)))
      (is (equal '(1 1) (%count-of g '(ns key) '("hr"))))
      (is (equal '(1 0) (%count-of g '(ns key) '("ops"))))
      (is (equal '(0 0) (%count-of g '(ns key) '("ops" "e2")))
          "the moved-away key is gone")
      (with-transaction () (mark-deleted (lookup-vertex a)))
      (is (equal '(0 0) (%count-of g '(ns key) '("ops")))
          "a delete subtracts once, and the key goes at zero")
      (is (equal '(0 nil) (%count-of g '(rel) "dead"))))))

(test a-re-applying-apply-marks-stale-and-a-rebuild-repairs
  "Spec R8, facts X7: under *ADD-TO-INDEXES-UNLESS-PRESENT-P* the pass
counts nothing and marks the maps stale; the next lookup rebuilds by
scan, so applying the same writes twice leaves the counters equal to one
application.  Ablation, recorded in the task report: counting anyway
under the special reads 2."
  (with-ix-graph (g)
    (let (writes)
      (with-transaction ()
        (make-ix-claim :ns "ops" :key "e1" :rel "at")
        (setq writes (copy-list (graph-db::writes *transaction*))))
      (is (equal '(1 1) (%count-of g '(ns key) '("ops"))) "control")
      (let ((graph-db::*add-to-indexes-unless-present-p* t))
        (graph-db::apply-tx-writes-to-count-indexes writes g)
        (graph-db::apply-tx-writes-to-count-indexes writes g))
      (is (eq t (graph-db::count-indexes-stale-p g)) "marked stale")
      (is (equal '(1 1) (%count-of g '(ns key) '("ops")))
          "the lookup rebuilt: still one")
      (is (null (graph-db::count-indexes-stale-p g))))))

(test a-listing-resolves-no-node
  "Spec §2.5: MAP-COUNT-INDEX and COUNT-INDEX-LOOKUP never resolve a
node.  Control: INDEX-LOOKUP resolves.  Ablation: a listing routed
through the secondary index turns the second check red."
  (with-ix-graph (g)
    (with-transaction ()
      (dotimes (i 20)
        (make-ix-claim :ns (nth (mod i 2) '("a" "b"))
                       :key (format nil "k~D" i) :rel "at")))
    (is (plusp (%count-resolutions
                (lambda () (index-lookup g 'ix-claim '(ns key rel) '("a")
                                         :prefix t))))
        "control: the probe counts the secondary lookup's resolutions")
    (is (= 0 (%count-resolutions
              (lambda ()
                (%entries g '(ns key) :depth 2 :prefix '("a"))
                (count-index-lookup g 'ix-claim '(ns key) '("b")))))
        "the count index answers from the map alone")))
```

If `(graph-db::writes *transaction*)` is not the accessor that returns the create-set plus write-set (facts C13 says `writes` at `transactions.lisp:387-391`), use the real one and say so.

- [ ] **Step 2: Run to verify they fail**

Expected: `commits-maintain-the-counters` red at the first assertion (no pass wired yet); the re-apply test red at "marked stale" (placeholder); the listing test's control green and its second check green already — record that; it becomes load-bearing once the spacetime path exists.

- [ ] **Step 3: Implement**

In `count-index.lisp`, replace the placeholder and add:

```lisp
;;; ---------------------------------------------------------------------------
;;; Maintenance (spec §2.3): a pass beside the secondary one at every
;;; apply, serialised by the manager lock / the device writer (R9).
;;; ---------------------------------------------------------------------------

(defgeneric apply-tx-write-to-count-indexes (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defun %count-specs-for (node graph)
  (%applicable-count-index-specs (class-of node) graph))

(defmethod apply-tx-write-to-count-indexes ((write tx-create) graph)
  (let ((node (node write)))
    (dolist (spec (%count-specs-for node graph))
      (let ((cix (%count-index-for graph spec)))
        (%count-node cix node 1 (%current-p cix node))))))

(defmethod apply-tx-write-to-count-indexes ((write tx-update) graph)
  ;; Tuple unchanged: CURRENT moves by the predicate's flip.  Tuple
  ;; changed: the old contribution leaves, the new one arrives.
  (let ((old (old-node write)) (new (node write)))
    (dolist (spec (%count-specs-for new graph))
      (let* ((cix (%count-index-for graph spec))
             (ot (%count-tuple cix old))
             (nt (%count-tuple cix new)))
        (if (equal ot nt)
            (let ((d (- (%current-p cix new) (%current-p cix old))))
              (unless (or (zerop d) (null nt))
                (loop for k from 1 to (length nt)
                      do (%count-adjust cix (subseq nt 0 k) 0 d))))
            (progn
              (%count-node cix old -1 (- (%current-p cix old)))
              (%count-node cix new 1 (%current-p cix new))))))))

;; TX-DELETE is a TX-UPDATE subclass: subtract the old node ONCE (the
;; secondary method releases twice; a counter cannot, facts C12).
(defmethod apply-tx-write-to-count-indexes ((write tx-delete) graph)
  (let ((old (old-node write)))
    (dolist (spec (%count-specs-for old graph))
      (let ((cix (%count-index-for graph spec)))
        (%count-node cix old -1 (- (%current-p cix old)))))))

(defun apply-tx-writes-to-count-indexes (writes graph)
  "The count pass of an apply (GH #361).  Under *ADD-TO-INDEXES-UNLESS-
PRESENT-P* -- crash-recovery replay and a device re-pull, which may
apply a write twice -- it counts nothing and marks the maps stale (R8).
Nothing at all for a graph with no count declarations."
  (when (gethash (graph-name graph) *schema-count-metadata*)
    (if *add-to-indexes-unless-present-p*
        (setf (count-indexes-stale-p graph) t)
        (dolist (write writes)
          (apply-tx-write-to-count-indexes write graph)))))

(defun %count-purge (node graph)
  "PEER-PURGE-NODE's count release: the node leaves without a write."
  (dolist (spec (%count-specs-for node graph))
    (let ((cix (%count-index-for graph spec)))
      (%count-node cix node -1 (- (%current-p cix node))))))

;;; ---------------------------------------------------------------------------
;;; Build, install, rebuild (spec §2.3-2.4)
;;; ---------------------------------------------------------------------------

(defun %build-count-index-for-spec (graph spec)
  "Build SPEC's map over the live nodes of its owner: a typed scan,
deleted nodes skipped, one bad node tolerated (as %BUILD-INDEX-FOR-SPEC)."
  (let ((cix (%count-index-for graph spec))
        (owner (count-index-spec-owner-name spec)))
    (flet ((count-node (node)
             (unless (deleted-p node)
               (ignore-errors
                (%count-node cix node 1 (%current-p cix node))))))
      (if (subtypep owner 'edge)
          (map-edges #'count-node graph :edge-type owner)
          (map-vertices #'count-node graph :vertex-type owner)))
    cix))

(defun %ensure-count-index-built (graph spec)
  "Build SPEC's map unless the registry already holds it (idempotent)."
  (let ((key (cons (count-index-spec-owner-name spec)
                   (count-index-spec-slot-names spec))))
    (unless (and (count-indexes graph)
                 (gethash key (count-indexes graph)))
      (%build-count-index-for-spec graph spec))))

(defun install-count-indexes (graph)
  "Build any declared count index missing from GRAPH's registry."
  (dolist (spec (%registered-count-index-specs graph))
    (%ensure-count-index-built graph spec)))

(defun rebuild-count-indexes (graph)
  "Drop every count map and rebuild each declared one by scan; clears
the stale flag (R8).  Authoritative and idempotent."
  (when (count-indexes graph)
    (maphash (lambda (k cix)
               (declare (ignore k))
               (let ((sl (count-index-skip-list cix)))
                 (when (and sl (view-index-p sl)) (delete-view-index sl))))
             (count-indexes graph))
    (clrhash (count-indexes graph)))
  (dolist (spec (%registered-count-index-specs graph))
    (%build-count-index-for-spec graph spec))
  (setf (count-indexes-stale-p graph) nil)
  graph)
```

Call sites, each one line after the secondary pass:

- `transactions.lisp` ~2008: `(apply-tx-writes-to-count-indexes writes graph) ; GH #361`
- `peer-streaming.lisp` ~1140 and ~1177: the same, over `writes` / `final-writes`.
- `peer-streaming.lisp` ~1445, after the `%ix-release` block:
  ```lisp
  ;; Counting indexes (GH #361): a purge is a delete without a write.
  (when (count-indexes graph)
    (%count-purge node graph))
  ```

`node`, `old-node` are the write accessors `index.lisp`'s methods use; `deleted-p`, `map-vertices`, `map-edges` as in `%build-index-for-spec`.

- [ ] **Step 4: Run the three tests, then `count-index-suite`, `index-suite`, `peer-index-suite`**

Then the ablation for the re-apply test: temporarily make `apply-tx-writes-to-count-indexes` count even under the special; run the test, record red (the lookup reads 2 and "marked stale" fails); restore, run green; record both in the report. Expected counts ≥ baseline.

- [ ] **Step 5: Commit** — `feat(index): count indexes maintained at apply, replication and purge (#361)` with trailers.

---

### Task 4: Persistence and open

**Files:**
- Modify: `count-index.lisp` (sidecar save/restore), `graph.lisp` (~1043-1057 open, ~1253 close), `memory-graph.lisp` (~1124), `index.lisp` (`regenerate-secondary-indexes`, ~838)
- Test: `tests/count-index-tests.lisp` (append)

**Interfaces:**
- Produces: `count-index-root-file location`, `save-count-index-roots graph`, `restore-count-index-roots graph` → T/NIL; the open/close wiring; `rebuild-count-indexes` called from `regenerate-secondary-indexes`.

- [ ] **Step 1: Write the failing tests**

```lisp
(test count-index-survives-close-and-reopen
  "Spec §2.4: the sidecar round trip -- close, reopen, same counters,
no scan; and a declaration the stored graph predates is built at open
(the withdrawn open and close make the sidecar unable to restore it)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at")
                 (make-ix-claim :ns "ops" :key "e2" :rel "dead")))
          (close-graph g)))
      (let ((g (open-graph *ix-graph-name* path)))
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(2 1) (%count-of g '(ns key) '("ops")))
                   "restored from the sidecar"))
          (close-graph g :snapshot-p nil)))
      ;; Build-at-open: withdraw, open and close once (the sidecar
      ;; record is reclaimed), re-declare, open: the map is built by scan.
      (undef-count-index ix-claim :graph-db-index-test :name ix-count-rel)
      (unwind-protect
           (let ((g (open-graph *ix-graph-name* path)))
             (unwind-protect
                  (let ((*graph* g))
                    (signals query-precondition-error
                      (count-index-lookup g 'ix-claim '(rel) "at")))
               (close-graph g :snapshot-p nil)))
        (def-count-index ix-claim (rel) :graph-db-index-test
          :name ix-count-rel))
      (let ((g (open-graph *ix-graph-name* path)))
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(1 nil) (%count-of g '(rel) "at"))
                   "built at open over the pre-existing nodes")
               (is (equal '(1 nil) (%count-of g '(rel) "dead"))))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test a-withdrawn-count-index-is-reclaimed-at-open
  "Spec §2.4 (GH #147): a sidecar record whose declaration is withdrawn
has its pages reclaimed at the next open -- DELETE-VIEW-INDEX is
called for it."
  (def-count-index ix-claim (ns) :graph-db-index-test :name ix-count-gone)
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at")))
          (close-graph g)))
      (undef-count-index ix-claim :graph-db-index-test :name ix-count-gone)
      (let* ((freed 0)
             (old (fdefinition 'graph-db::delete-view-index)))
        (setf (fdefinition 'graph-db::delete-view-index)
              (lambda (ix) (incf freed) (funcall old ix)))
        (unwind-protect
             (let ((g (open-graph *ix-graph-name* path)))
               (close-graph g :snapshot-p nil)
               (collect-garbage))
          (setf (fdefinition 'graph-db::delete-view-index) old))
        (is (plusp freed) "the retired count map was reclaimed")))))

(test a-stale-flag-is-repaired-before-the-first-lookup-and-saved
  "Spec R8, facts X7: an apply under *ADD-TO-INDEXES-UNLESS-PRESENT-P*
(what a crash-recovery replay or a device re-pull runs) counts nothing
and flags the maps; the first lookup rebuilds by scan and counts the
write the maps never saw, and the close saves the rebuilt maps."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at"))
               (let ((graph-db::*add-to-indexes-unless-present-p* t))
                 (with-transaction ()
                   (make-ix-claim :ns "ops" :key "e2" :rel "at")))
               (is (eq t (graph-db::count-indexes-stale-p g)) "flagged")
               (is (equal '(2 2) (%count-of g '(ns key) '("ops")))
                   "the first lookup rebuilt and counted the flagged write")
               (is (null (graph-db::count-indexes-stale-p g))))
          (close-graph g)))
      (let ((g (open-graph *ix-graph-name* path)))
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(2 2) (%count-of g '(ns key) '("ops")))
                   "the rebuilt maps were saved at close"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))
```

Then grep `tests/` for an existing crash-recovery shape (`recovery-transaction-files`, `.txn`, "crash"); if one is cheap to copy, add a sibling test that commits, leaves the `.txn` files in place without a clean close, reopens, and asserts the replayed write is counted (the `graph.lisp` flag-and-rebuild path). If none is cheap, say so in the report; the flag test above pins the same mechanism.

`delete-view-index` is a generic (facts D22's template swaps its `fdefinition` too — copy that test's exact mechanism from `tests/index-tests.lisp:889-921`).

- [ ] **Step 2: Run to verify they fail** — the reopen reads 0 (no sidecar, and no rebuild at open).

- [ ] **Step 3: Implement**

`count-index.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; Persistence (spec §2.4): an own sidecar, byte-compatible with nothing
;;; else -- the secondary one is untouched in both directions.
;;; ---------------------------------------------------------------------------

(defun count-index-root-file (location)
  (format nil "~A/count-indexes.dat" location))

(defun save-count-index-roots (graph)
  "Persist (owner slot-names address backend-tag) per on-disk count map;
no-op with no heap or no maps.  Called at CLOSE-GRAPH, unguarded like the
other index saves: a stale root is silently wrong (GH #361)."
  (when (and (indexes graph) (count-indexes graph))
    (let ((roots '()))
      (maphash (lambda (k cix)
                 (declare (ignore k))
                 (let ((sl (count-index-skip-list cix)))
                   (when (and sl (view-index-p sl))
                     (push (list (count-index-owner-name cix)
                                 (count-index-slot-names cix)
                                 (view-index-address sl)
                                 (view-index-backend-tag sl))
                           roots))))
               (count-indexes graph))
      (%atomic-cl-store roots (count-index-root-file (location graph))))))

(defun restore-count-index-roots (graph)
  "Reopen the count maps from the sidecar; T when one was present and
readable (the empty case included), NIL to fall back to
REBUILD-COUNT-INDEXES.  A record whose declaration is withdrawn is
reclaimed (GH #147); the predicate and canonicalizers come from the live
spec, never the file."
  (let ((file (count-index-root-file (location graph))))
    (when (probe-file file)
      (handler-case
          (let ((records (cl-store:restore file))
                (reg (%count-registry graph)))
            (dolist (r records t)
              (destructuring-bind (owner stored-slots address backend) r
                (let* ((slot-names (%normalize-slots stored-slots))
                       (spec (%count-spec-for owner slot-names graph)))
                  (if spec
                      (setf (gethash (cons owner slot-names) reg)
                            (%make-count-index
                             :owner-name owner :slot-names slot-names
                             :canonicalizers
                             (%resolve-index-canonicalizers
                              (count-index-spec-canonicalize spec)
                              (length slot-names))
                             :current-p (count-index-spec-current-p spec)
                             :skip-list (%open-count-skip-list
                                         graph address backend)))
                      (handler-case
                          (progn
                            (delete-view-index
                             (%open-count-skip-list graph address backend))
                            (log:info "reclaimed retired count index ~
~A.~A (GH #147)" owner slot-names))
                        (error (e)
                          (warn "could not reclaim retired count index ~
~A.~A: ~A" owner slot-names e))))))))
        (error (e)
          (warn "count-index sidecar unreadable, rebuilding: ~A" e)
          nil)))))
```

`graph.lisp`: inside the `(when crash-recovery-p ...)` block at ~1043 add `(setf (count-indexes-stale-p graph) t)` with a comment naming R8 (the replay's counter writes are discarded by the restore below); after `(install-unique-tuple-constraints graph)` at ~1057:

```lisp
        ;; Counting indexes (GH #361): reopen-or-rebuild like the others;
        ;; a crash-recovery replay left the maps stale (R8), rebuild then.
        (unless (restore-count-index-roots graph)
          (rebuild-count-indexes graph))
        (install-count-indexes graph)
        (when (count-indexes-stale-p graph)
          (rebuild-count-indexes graph))
```

Check the `let` scope of `crash-recovery-p` (facts D20: it closes before the restores) — the flag is why the rebuild can happen after. `graph.lisp` ~1254: `(save-count-index-roots graph)` after `(save-secondary-index-roots graph)`. `memory-graph.lisp` ~1124, inside the `(unless (lazy-p graph) ...)`: `(rebuild-count-indexes graph)` `(install-count-indexes graph)`. `index.lisp` `regenerate-secondary-indexes`, before `graph` is returned: `(rebuild-count-indexes graph)` `(save-count-index-roots graph)` with a one-line comment.

- [ ] **Step 4: Run the new tests, then `count-index-suite`, `index-suite`, `peer-index-suite`.**

- [ ] **Step 5: Commit** — `feat(index): count-index sidecar, build at open, crash rebuild (#361)` with trailers.

---

### Task 5: Replication

**Files:**
- Test: `tests/peer-index-tests.lisp` (append)

**Interfaces:**
- Consumes: Tasks 3–4; `pi-claim`, `with-pi-device`, `pi-authored-create`, the graph name `*pi-graph-name*`'s value (read the file).

- [ ] **Step 1: Write the tests**

```lisp
;;; --- counting index on the device (GH #361) ---------------------------------

(defun pi-dead-p (node)
  (equal (pi-rel node) "dead"))
(defun pi-live-p (node) (not (pi-dead-p node)))

(def-count-index pi-claim (ns key) :graph-db-peer-index-test
  :name pi-count :current-p pi-live-p)

(test authored-pull-counts-the-node
  "APPLY-PEER-AUTHORED-OP maintains the device's count index."
  (with-pi-device (g)
    (graph-db::apply-peer-authored-op
     g (pi-authored-create g 'pi-claim '((:ns . "ops") (:key . "e1")
                                         (:rel . "at"))
                           *pi-remote-origin*))
    (is (equal '(1 1) (multiple-value-list
                       (count-index-lookup g 'pi-claim '(ns key) '("ops")))))))

(test a-pulled-retraction-moves-current
  "A TX-UPDATE over the wire whose OLD node is live and whose NEW node
is not moves CURRENT and leaves ALL."
  (with-pi-device (g)
    (multiple-value-bind (op nid)
        (pi-authored-create g 'pi-claim '((:ns . "ops") (:key . "e1")
                                          (:rel . "at"))
                            *pi-remote-origin*)
      (graph-db::apply-peer-authored-op g op)
      (let* ((old (lookup-vertex nid :graph g))
             (new (graph-db::%copy old)))
        (setf (pi-rel new) "dead")
        (graph-db::apply-peer-authored-op
         g (graph-db::make-peer-op
            :kind :authored :op-id (graph-db::gen-op-id)
            :origin *pi-remote-origin* :lamport 6 :tx-id 9001
            :writes (list (make-instance 'graph-db::tx-update
                                         :node new :old-node old)))))
      (is (equal '(1 0) (multiple-value-list
                         (count-index-lookup g 'pi-claim '(ns key)
                                             '("ops"))))))))

(test a-state-sync-re-pull-does-not-double-count
  "Spec R8: APPLY-PEER-CREATE-WRITES binds *ADD-TO-INDEXES-UNLESS-
PRESENT-P*; applying the same create twice leaves the counter at one
(stale flag, rebuild on the next lookup)."
  (with-pi-device (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name 'pi-claim :vertex
                                                      :graph g)))
           (n (graph-db::%make-vertex :class 'pi-claim :id (gen-id)
                                      :type-id tid :revision 0)))
      (setf (graph-db::data n) '((:ns . "ops") (:key . "e1") (:rel . "at")))
      (dotimes (i 2)
        (graph-db::apply-peer-create-writes
         g 7777 (list (make-instance 'graph-db::tx-create :node n))
         *pi-remote-origin*))
      (is (equal '(1 1) (multiple-value-list
                         (count-index-lookup g 'pi-claim '(ns key)
                                             '("ops"))))))))

(test purge-releases-the-counter
  "PEER-PURGE-NODE subtracts the purged node's contribution."
  (with-pi-device (g)
    (multiple-value-bind (op nid)
        (pi-authored-create g 'pi-claim '((:ns . "ops") (:key . "e1")
                                          (:rel . "at"))
                            *pi-remote-origin*)
      (graph-db::apply-peer-authored-op g op)
      (graph-db::apply-peer-purge g (list nid))
      (is (equal '(0 0) (multiple-value-list
                         (count-index-lookup g 'pi-claim '(ns key)
                                             '("ops"))))))))
```

Read `pi-claim`'s accessors and `*pi-graph-name*`'s value first and use the real names; the authored-update shape (a `tx-update` whose `old-node` is the stored vertex) must match how `peer-merge-write` reads it — if `apply-peer-authored-op` rejects a hand-built update, look at `tests/peer-unique-tests.lisp` for the update shape and copy it, and say so.

- [ ] **Step 2: Run the four tests and `peer-index-suite`.** Expected: PASS (the wiring landed in Task 3); if a test is green before the pass existed, that is a vacuity signal — check by reading which pass each depends on and report.

- [ ] **Step 3: Commit** — `test(peer): the count index on the device (#361)` with trailers.

---

### Task 6: Spacetime on the count index

**Files:**
- Modify: `spacetime/claim.lisp` (~453, after `claim-relation`), `spacetime/claim-query.lisp` (the vocabulary section)
- Test: `tests/spacetime/vocabulary-tests.lisp` (append)

**Interfaces:**
- Consumes: `graph-db:map-count-index`, `graph-db:count-index-lookup`, `graph-db:def-count-index`; #350's helpers (all kept).
- Produces: the three functions answer from the count indexes outside an as-of extent and from #350's walk inside one (R7). New internal helpers `%names-at`, `%vocabulary-delta`, `%counted-names`.

- [ ] **Step 1: Write the failing tests**

```lisp
(defvar *vt-resolutions* nil)

(defun %vt-count-resolutions (thunk)
  "LOOKUP-VERTEX calls during THUNK, through an :AROUND removed after."
  (let* ((gf #'graph-db:lookup-vertex)
         (method (eval '(defmethod graph-db:lookup-vertex :around
                            ((id t) &key &allow-other-keys)
                          (when *vt-resolutions* (incf *vt-resolutions*))
                          (call-next-method)))))
    (unwind-protect
         (let ((*vt-resolutions* 0)) (funcall thunk) *vt-resolutions*)
      (remove-method gf method))))

(test a-current-listing-resolves-no-node-outside-an-as-of-extent
  "Spec §3.2, R7: outside an as-of extent the three functions read the
count indexes and resolve nothing, under :CURRENT and :COUNTS too;
inside one they run #350's walk and do resolve.  Control: the walk
resolves.  Ablation: routing the plain case to the walk turns the first
check red."
  (with-claim-graph (g)
    (with-transaction ()
      (dotimes (i 12) (%ns-u :ns (format nil "k~D" i)))
      (%ns-b :ns "s" :other "o" :relation "knows"))
    (retract-claim (first (claims-touching g 'ct-claim :ns "k3")))
    (let ((e (graph-db:latest-epoch g)))
      (is (= 0 (%vt-count-resolutions
                (lambda ()
                  (claim-namespaces g 'ct-claim :counts t :current t)
                  (claim-keys g 'ct-claim :ns :counts t :current t)
                  (claim-relations g 'ct-claim :counts t :current t))))
          "the count indexes answer without a resolution")
      (is (equal '((:ns . 13) (:other . 1))
                 (claim-namespaces g 'ct-claim :counts t)))
      (is (equal '((:ns . 12) (:other . 1))
                 (claim-namespaces g 'ct-claim :counts t :current t))
          "the retracted claim moved the current counter")
      (is (equal '(("knows" . 1) ("r" . 11))
                 (claim-relations g 'ct-claim :counts t :current t)))
      (is (plusp (%vt-count-resolutions
                  (lambda ()
                    (graph-db:with-as-of ((g) e)
                      (claim-namespaces g 'ct-claim :counts t :current t)))))
          "control: inside an as-of extent the walk resolves"))))

(test current-moves-on-retraction-and-a-create-then-delete
  "Spec §3.3: inside a transaction a created claim counts, a claim
created then MARK-DELETED in the same transaction counts nothing, and a
retraction moves the :CURRENT count; after commit the counters agree."
  (with-claim-graph (g)
    (with-transaction () (%ns-u :ns "a"))
    (with-transaction ()
      (let ((c (%ns-u :fresh "n")))
        (%ns-u :gone "z")
        (graph-db:mark-deleted
         (first (claims-touching g 'ct-claim :gone "z")))
        (retract-claim (first (claims-touching g 'ct-claim :ns "a")))
        (is (equal '((:fresh . 1) (:ns . 1))
                   (claim-namespaces g 'ct-claim :counts t)))
        (is (equal '((:fresh . 1))
                   (claim-namespaces g 'ct-claim :counts t :current t)))
        (is (equal '("n") (claim-keys g 'ct-claim :fresh)))
        c))
    (is (equal '((:fresh . 1) (:ns . 1))
               (claim-namespaces g 'ct-claim :counts t)))
    (is (equal '((:fresh . 1))
               (claim-namespaces g 'ct-claim :counts t :current t)))))

(test a-family-opened-over-pre-existing-claims-has-its-count-indexes
  "Spec §3.1: a stored family builds the three count indexes at open."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (%ns-u :ns "a" :relation "likes")
                 (%ns-b :ns "b" :other "c" :relation "knows")))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (is (equal '(("knows" . 1) ("likes" . 1))
                          (claim-relations g2 'ct-claim :counts t)))
               (is (equal '((:ns . 2) (:other . 1))
                          (claim-namespaces g2 'ct-claim :counts t))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
```

If a `mark-deleted` of a claim created in the same transaction is refused (facts: `delete-node` uses `%copy` to allow it, GH #135), keep the test; if it signals, replace the create-then-delete with a create-then-delete across two transactions and say so.

- [ ] **Step 2: Run to verify they fail** — the first test's zero-resolution check is red (the walk resolves under `:current`); the reopen test is red (`query-precondition-error`, no count index declared).

- [ ] **Step 3: Implement**

`spacetime/claim.lisp`, after the `claim-relation` declaration (~453), before `fmakunbound`:

```lisp
       ;; Counting indexes (GH #361): the vocabulary is a lookup.  The
       ;; object index on BINARY, like CLAIM-OBJECT above.
       (graph-db:def-count-index ,parent (subject-namespace subject-key)
           ,graph-name :name claim-subject-count
           :current-p claim-current-p)
       (graph-db:def-count-index ,binary (object-namespace object-key)
           ,graph-name :name claim-object-count
           :current-p claim-current-p)
       (graph-db:def-count-index ,parent (relation) ,graph-name
           :name claim-relation-count :current-p claim-current-p)
```

`claim-current-p` is defined in `claim-query.lisp`, which loads after; the symbol is stored and funcalled at maintenance (Task 2), so no forward-reference problem — confirm the symbol is `graph-db.spacetime::claim-current-p` in the expansion (it is, the macro is in that package).

`spacetime/claim-query.lisp`, in the vocabulary section, add after `%merge-names`:

```lisp
;;; The count path (GH #361 spec §3.2-3.3).  The walk above stays: it is
;;; the as-of mechanism (R7), the only one that answers at an epoch.

(defun %count-slots (slots)
  "The count index a source's secondary SLOTS map to: the same slots."
  slots)

(defun %names-at (graph class slots depth prefix current counts)
  "The count index's entries at DEPTH under PREFIX as (NAME ALL CURRENT)
triples, NAME the last component, in index order."
  (let ((out '()))
    (graph-db:map-count-index
     (lambda (components all cur)
       (push (list (car (last components)) all (or cur 0)) out))
     graph class (%count-slots slots) :depth depth :prefix prefix)
    (declare (ignore current counts))
    (nreverse out)))

(defun %vocabulary-delta (view class slots depth prefix)
  "Per-name (ALL . CURRENT) adjustments the open transaction will
commit for the names at DEPTH under PREFIX of (CLASS SLOTS): an EQUAL
table, NIL outside a transaction.  Bounded by the write set (§3.3)."
  (when view
    (let ((delta (make-hash-table :test 'equal)))
      (flet ((bump (node d-all d-cur)
               (when (and node (typep node class))
                 (let ((tuple (%claim-tuple node slots)))
                   (when (every #'equal prefix tuple)
                     (let* ((name (nth (1- depth) tuple))
                            (cell (or (gethash name delta)
                                      (setf (gethash name delta)
                                            (cons 0 0)))))
                       (incf (car cell) d-all)
                       (incf (cdr cell) d-cur)))))))
        (dolist (w (graph-db:view-writes view))
          (let ((new (graph-db::node w))
                (old (and (typep w 'graph-db::tx-update)
                          (graph-db::old-node w))))
            (cond ((typep w 'graph-db::tx-delete)
                   (bump old -1 (if (claim-current-p old) -1 0)))
                  ((typep w 'graph-db::tx-update)
                   (bump old -1 (if (claim-current-p old) -1 0))
                   (bump new 1 (if (claim-current-p new) 1 0)))
                  (t (bump new 1 (if (claim-current-p new) 1 0)))))))
      delta)))

(defun %counted-names (graph class slots depth prefix current counts)
  "The names of one source from its count index, the open transaction's
delta applied: names, or (NAME . COUNT); a name whose adjusted ALL is 0
is dropped; under CURRENT one whose CURRENT is 0 is dropped."
  (let* ((view (%vocabulary-view graph))
         (delta (%vocabulary-delta view class slots depth prefix))
         (out '()))
    (flet ((emit (name all cur)
             (let ((n (if current cur all)))
               (when (plusp n)
                 (push (if counts (cons name n) name) out)))))
      (dolist (e (%names-at graph class slots depth prefix current counts))
        (destructuring-bind (name all cur) e
          (let ((d (and delta (gethash name delta))))
            (when d (remhash name delta))
            (emit name (+ all (if d (car d) 0)) (+ cur (if d (cdr d) 0))))))
      (when delta
        (maphash (lambda (name d) (emit name (car d) (cdr d))) delta)))
    (nreverse out)))

(defun %vocabulary (graph family role depth prefix current counts)
  "The merged names of FAMILY for ROLE at DEPTH under PREFIX: the count
path, or #350's walk inside an as-of extent (R7)."
  (%merge-names
   (loop for (class slots) in (%vocabulary-sources family role)
         collect (if (graph-db::%as-of-snapshot graph)
                     (%walk-names graph class slots depth prefix
                                  (1- depth) current counts)
                     (%counted-names graph class slots depth prefix
                                     current counts)))
   counts))
```

Then the three functions' bodies become:

```lisp
;; claim-namespaces:
  (let ((family (claim-family claim-class)))
    (%vocabulary graph family role 1 nil current counts))
;; claim-relations:
  (let* ((family (claim-family claim-class))
         (class (claim-family-parent family)))
    (if (graph-db::%as-of-snapshot graph)
        (%walk-names graph class '(relation) 1 nil 0 current counts)
        (%counted-names graph class '(relation) 1 nil current counts)))
;; claim-keys:
  (let ((family (claim-family claim-class)))
    (%paginate (%vocabulary graph family role 2 (list namespace)
                            current counts)
               limit offset))
```

`%walk-names`' START argument: pass `prefix` (`nil` or `(list namespace)`) — it is what #350 passed. `%merge-names` on a single list is fine. Note `%names-at`'s `declare` placement: put `(declare (ignore current counts))` at the top of the body or drop the two parameters; keep the signature simple — drop them if unused (the plan's snippet keeps them only to mirror `%counted-names`; simplify and say so). The relation source is one `(class slots)` pair; `%vocabulary-sources` does not include it, hence the separate arm in `claim-relations`.

- [ ] **Step 4: Run the three new tests, every #350 vocabulary test (unchanged — R5), then the spacetime suite via the runner and the other three suites**

Then the ablation: temporarily make `%vocabulary` always take the walk arm; run the first new test, record red at the zero-resolution check; restore; run green; record both.

- [ ] **Step 5: Commit** — `feat(spacetime): the vocabulary reads the count indexes (#361)` with trailers.

---

### Task 7: Documentation

**Files:**
- Modify: `docs/general-index-design.md` (new `### 6b. Counting index (GH #361)` after §6a, before `## 7.`), `docs/vivace-graph-v3-doc.org` ("Vocabulary: what a family names"), `CHANGELOG.md` (first bullet under Unreleased/Added), the #350 spec's Status line, this spec's Status line

- [ ] **Step 1: `docs/general-index-design.md`**, after §6a:

```markdown
### 6b. Counting index (GH #361)

`def-count-index owner (slots) graph-name &key name current-p
canonicalize` declares an ordered map keyed `(depth v1 … vk)` for every
leading prefix of a node's canonical tuple, whose value is the counter
pair `(all . current)`; `current` counts the nodes on which CURRENT-P
holds and is NIL when none is declared. It has its own registry
(`*schema-count-metadata*`, the `def-unique` pattern), graph slot
(`count-indexes`) and sidecar (`count-indexes.dat`), so a class may carry
a secondary index and a count index on the same slots.

Maintenance runs at commit apply beside the secondary pass, in the two
replication applies and in the peer purge: a create adds at every prefix,
a delete subtracts once, an update moves `current` by the predicate's
flip or moves both counters when the tuple changed; a key is removed at
zero. Counter steps are read-then-branch (the memory backend's update is
not an upsert) and pass the old value so the update stays in place. The
pass takes no lock: the transaction manager's lock and the device's
single writer serialise it.

An apply that runs under `*add-to-indexes-unless-present-p*` — a
crash-recovery replay or a device re-pull, both of which may apply a
write twice — counts nothing and marks the maps stale; a stale map is
rebuilt by scan at that open or on the next query. A memory graph rebuilds
at every open (nothing on a lazy graph).

`count-index-lookup graph class slot tuple` → `(values all current)`;
`map-count-index fn graph class slot &key depth prefix` calls FN with
`(components all current)` in index order. Neither resolves a node. Counts
are live at commit granularity: a reader under a snapshot or an as-of
extent sees later commits, and a walk is not an atomic snapshot.
```

- [ ] **Step 2: the org manual.** Rewrite the "Vocabulary: what a family names" subsection's mechanism paragraph: the three functions read three counting indexes every family declares (`claim-subject-count`, `claim-object-count`, `claim-relation-count`), so a listing with counts is a lookup and resolves no node; `:current` reads the current counter; inside a transaction the transaction's own writes are overlaid; under an as-of extent the listing falls back to the walk of #350 (the only mechanism that answers at an epoch) and costs what it cost before. Keep the examples.

- [ ] **Step 3: CHANGELOG**, first bullet under Unreleased/Added:

```markdown
- **Counting index** (#361): `def-count-index` declares a per-prefix
  counter pair `(all . current)` maintained at commit apply, replication
  and purge, persisted through its own sidecar and rebuilt after a
  re-applying apply; `count-index-lookup` and `map-count-index` read it
  without resolving a node. Every claim family now declares three
  (`claim-subject-count`, `claim-object-count`, `claim-relation-count`),
  so `claim-namespaces`, `claim-keys` and `claim-relations` are lookups
  outside an as-of extent (inside one they keep #350's walk). No node
  format change; existing families build the maps on next open.
  `docs/general-index-design.md` §6b.
```

- [ ] **Step 4: Status lines** — the #350 spec: "; mechanism superseded outside as-of extents by `2026-09-08-count-index-design.md`"; this spec: "; implemented on `feat/count-index`".

- [ ] **Step 5: Commit** — `docs: counting index (#361)` with trailers.

---

### Task 8: Whole-branch verification

- [ ] **Step 1: Column check on every touched Lisp file** (compare against 837b460: no ADDED line over 80).
- [ ] **Step 2: All four suites via the runner** — `exit=0`, every count ≥ baseline.
- [ ] **Step 3: Diffstat sanity** — `git diff --stat 837b460..HEAD`; no test line removed (`git diff 837b460..HEAD -- tests | grep -c '^-[^-]'` = 0).
- [ ] **Step 4: Final whole-branch review** (SDD: most capable model), then hand the branch to the maintainer for push authorisation. Not pushed by this plan.
