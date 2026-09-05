# Epoch Axis for Claim Reads (#347) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `claims-touching` and `claims-by-producer` answer `:as-of-epoch E` (the version live at commit epoch E), and `claim-commit-epoch` reads a version's commit epoch, so a consumer holding two stores on one system clock can read both at one epoch.

**Architecture:** Read-side only, in `graph-db/spacetime`. The engine already stamps every node version with the id of the committing transaction (`commit-epoch`, a node-head slot, exported here for the first time). A resolver `%claim-as-of-epoch` walks `vertex-history` newest-first and selects the newest version whose epoch is `<=` E; `claim-current-p` on that version is the whole retraction test; when nothing is old enough, the oldest retained version's `revision` tells "created after E" (0, answer NIL) from "reaped" (> 0, answer a `reaped-claim`). A store with no system clock refuses with `epoch-axis-unavailable`, a `query-precondition-error` subtype. No persisted field, no migration, no write-side change; `split-claim-identity-key` is untouched (issue part 3 is a no-op).

**Tech Stack:** Common Lisp (SBCL), graph-db/spacetime, FiveAM, cl-temporal-extent 0.3.0.

**Spec:** The bounded design Kevin approved in chat on 2026-09-05 (reproduced under "Design" below) argued from `docs/superpowers/notes/2026-09-05-epoch-axis-engine-api-facts.md` (the recon note; read §C and §S first). Order of authority: this plan's Design section > the recon note > the issue text of kraison/vivace-graph#347.

## Global Constraints

- Worktree: `/home/raison/work/vivace-graph-v3/.worktrees/epoch-axis` (branch `feat/epoch-axis` from `origin/experiment` `3a8ca96`). The main checkout `/home/raison/work/vivace-graph-v3` is shared with other sessions: never build, edit or commit there.
- Lisp: spaces only; hard 80-column limit in every `.lisp` and `.asd` line, docstrings included; terse comments that point at the recon note (`#347 recon C2`), an issue, or the manual. Docstrings: what, returns, the one trap.
- Never `pkill`, `pgrep -f`, or `kill` anything: other agents' SBCL images share this host.
- Suites run in the foreground, one at a time, never two builds in one worktree at once (shared FASL cache). The spacetime suite:

  ```
  cd /home/raison/work/vivace-graph-v3/.worktrees/epoch-axis
  sbcl --dynamic-space-size 4096 --non-interactive \
    --eval '(push #p"/home/raison/work/vivace-graph-v3/.worktrees/epoch-axis/" asdf:*central-registry*)' \
    --eval '(ql:quickload :graph-db/spacetime-test :silent t)' \
    --eval '(asdf:test-system :graph-db/spacetime-test)'
  ```

  Always the `-test` system (`asdf:test-system :graph-db/spacetime` is a silent no-op). Read `Did N checks.` and record N in the report. Baseline at `a75cf96`: 653 checks, 0 failures. One test alone (bare `fiveam:run!` skips the runner's system-directory binding and every `make-graph` would fail):

  ```
  sbcl --dynamic-space-size 4096 --non-interactive \
    --eval '(push #p"/home/raison/work/vivace-graph-v3/.worktrees/epoch-axis/" asdf:*central-registry*)' \
    --eval '(ql:quickload :graph-db/spacetime-test :silent t)' \
    --eval '(let* ((d (graph-db-test-scratch:make-scratch-directory "ep")) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (log:config :error) (fiveam:run! (quote graph-db/spacetime-test::TEST-NAME)))'
  ```

- The test package is `graph-db/spacetime-test`; it `:use`s `graph-db.spacetime` and imports `make-graph`, `close-graph`, `with-transaction`, `open-graph`, `id`, `lookup-vertex` from `graph-db`. Everything else from `graph-db` is written qualified (`graph-db:open-system-clock`, `graph-db:*transaction*`, `graph-db::transaction-id`).
- Every constructor call passes `:graph` explicitly, even inside `(with-transaction (:graph g) ...)`.
- Every negative test names its mechanism in its docstring and has a control in the same test.
- After every scripted edit to an existing file, `git diff HEAD -- <file> | grep -c '^-[^-]'` and expect 0 for a pure append.
- Docs travel with code in the same commit (Task 4 carries the manual; the push hook refuses source-only pushes). Nothing is pushed without Kevin. Commit trailers, both lines, on every commit:

  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU
  ```

- Issue #347 gets a comment from the controller when the branch is complete; closing is by hand with the merge SHA.

## Design (approved 2026-09-05)

1. **Export.** `package.lisp` exports `#:commit-epoch` beside `#:vertex-history`. Not `#:prev-pointer` (a heap address).
2. **Part 1.** `graph-db.spacetime:claim-commit-epoch (claim) => integer or NIL`: the version's `commit-epoch` when positive; NIL for a `reaped-claim` and for a version stamped 0 (written before the store had a counter). The number is the writer's own `transaction-id`, comparable across stores only while they share one `system-clock`.
3. **Part 2, resolver.** `%claim-as-of-epoch (graph claim epoch)` beside `%claim-as-of`, on `vertex-history` (not `resolve-version-at-epoch`, which needs `*graph*` bound and takes no read pin: recon C3). Select the first `(version . e)` with `(<= e epoch)` (recon C2: `<=`, never `<`). Selected: return it if `claim-current-p`, else NIL (a retraction is a version; recon E4). None selected, history non-NIL: oldest retained version's `revision` 0 means created after E, return NIL; otherwise return `(%make-reaped-claim id)` (recon C4). History NIL: NIL.
4. **Part 2, readers.** `claims-touching` and `claims-by-producer` gain `as-of-epoch`. Both given with `as-of`: `(error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both.")`. `as-of-epoch` on a store whose `graph-system-clock` is NIL: signal `epoch-axis-unavailable`. The resolve step is a three-way `cond`; `%overlay-transaction` stays only on the neither-axis arm (recon C6: an uncommitted write has no epoch). Whether two stores share a clock is the consumer's precondition, never checked here (recon E9).
5. **Condition.** `epoch-axis-unavailable`, a subtype of `graph-db:query-precondition-error` with a `graph-name` reader; the parent's `reason` is filled at the signal site so existing handlers that print the reason keep working.
6. **Part 3.** `split-claim-identity-key` unchanged. No task touches identity.

## File Structure

| file | responsibility |
|---|---|
| `package.lisp` | export `#:commit-epoch` |
| `spacetime/package.lisp` | export `#:claim-commit-epoch`, `#:epoch-axis-unavailable`, `#:epoch-axis-unavailable-graph-name` |
| `spacetime/claim-query.lisp` | `claim-commit-epoch`, `epoch-axis-unavailable`, `%refuse-epoch-axis`, `%claim-as-of-epoch`, `:as-of-epoch` on both readers |
| `tests/spacetime/epoch-tests.lisp` (new) | the two-store one-clock fixture and every #347 test |
| `graph-db.asd` | the new test file in `graph-db/spacetime-test` |
| `docs/vivace-graph-v3-doc.org` | the `:as-of-epoch` paragraph beside `:as-of` |

---

### Task 1: The fixture, the export, and `claim-commit-epoch`

**Files:**
- Modify: `package.lisp` (the `;; MVCC: public read path ...` export block, next to `#:vertex-history`)
- Modify: `spacetime/package.lisp` (the export list, next to `#:claim-version-stamp`)
- Modify: `spacetime/claim-query.lisp` (after the `reaped-claim` defstruct, before `%claim-effective-stamp`)
- Modify: `graph-db.asd` (`graph-db/spacetime-test` components, after `temporal-tests`)
- Create: `tests/spacetime/epoch-tests.lisp`

**Interfaces:**
- Consumes: `graph-db::commit-epoch` (node-head accessor, already populated on every read path), `reaped-claim-p`, `graph-db:open-system-clock`, `graph-db:close-system-clock`, `graph-db:graph-system-clock`, `make-graph ... :system-clock`.
- Produces: `graph-db.spacetime:claim-commit-epoch (claim) => (or null integer)`; the test fixture `with-clocked-stores ((a b) &body)`, the families `ea-claim` (store `:graph-db-ep-a`), `eb-claim` (store `:graph-db-ep-b`), `ek-claim` (store `:graph-db-ep-a`, `:keep-revisions 1`), and the helpers `%tx`, `%one`, `%unary` used by Tasks 2 to 4.

- [ ] **Step 1: Write the failing tests and the fixture**

Create `tests/spacetime/epoch-tests.lisp` with exactly this content:

```lisp
;;;; The epoch axis for claim reads (GH #347): a version's commit
;;;; epoch, and :AS-OF-EPOCH on the two readers.  Facts in
;;;; docs/superpowers/notes/2026-09-05-epoch-axis-engine-api-facts.md.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *ep-a-name* :graph-db-ep-a)
(defparameter *ep-b-name* :graph-db-ep-b)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ep-a-name* graph-db::*schema-node-metadata*) nil)
  (setf (gethash *ep-b-name* graph-db::*schema-node-metadata*) nil))

;; Two stores need two families: DEF-CLAIM-CLASSES binds node types to
;; one graph name and class names are global (#347 recon F).
(def-claim-classes ea-claim :graph-db-ep-a)
(def-claim-classes eb-claim :graph-db-ep-b)
(def-claim-classes ek-claim :graph-db-ep-a :keep-revisions 1)

(defmacro with-clocked-stores ((a b) &body body)
  "Two fresh stores A and B on ONE system clock, all in scratch dirs.
The attach is asserted inside the fixture: a store that silently failed
to attach would let every epoch test pass for the wrong reason."
  (let ((cdir (gensym "CDIR")) (da (gensym "DA")) (db (gensym "DB"))
        (clock (gensym "CLOCK")))
    `(with-temp-directory (,cdir)
       (with-temp-directory (,da)
         (with-temp-directory (,db)
           (let ((,clock (graph-db:open-system-clock (namestring ,cdir))))
             (unwind-protect
                  (let ((,a (make-graph *ep-a-name* (namestring ,da)
                                        :buffer-pool-size 1000
                                        :system-clock ,clock))
                        (,b (make-graph *ep-b-name* (namestring ,db)
                                        :buffer-pool-size 1000
                                        :system-clock ,clock)))
                    (unwind-protect
                         (progn
                           (is (eq (graph-db:graph-system-clock ,a)
                                   (graph-db:graph-system-clock ,b))
                               "fixture: both stores on one clock")
                           ,@body)
                      (ignore-errors (close-graph ,a))
                      (ignore-errors (close-graph ,b))
                      (collect-garbage)))
               (graph-db:close-system-clock ,clock))))))))

(defun %tx (graph thunk)
  "Run THUNK in a transaction on GRAPH; return the committed epoch --
the transaction's id, readable after the commit (#347 recon E2)."
  (graph-db::transaction-id
   (with-transaction (:graph graph)
     (funcall thunk)
     graph-db:*transaction*)))

(defun %unary (graph maker key &key extent)
  "Make one unary claim with MAKER in GRAPH on (:region KEY)."
  (apply maker :graph graph
               :subject-namespace :region :subject-key key
               :relation "verified" :producer "audit" :standing :observed
               (and extent (list :extent extent))))

(defun %one (graph family key)
  "The live version of the one claim of FAMILY on (:region KEY)."
  (first (claims-touching graph family :region key :role :subject)))

(test epochs-form-one-sequence-across-two-stores
  "#347 part 1 (recon C1, E2): under one clock the commits A, B, A get
distinct increasing ids, and each claim's CLAIM-COMMIT-EPOCH read back
from the store is its own transaction's id."
  (with-clocked-stores (a b)
    (let ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
          (e2 (%tx b (lambda () (%unary b #'make-eb-claim-unary "r1"))))
          (e3 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r2")))))
      (is (< e1 e2 e3))
      (is (= e1 (claim-commit-epoch (%one a 'ea-claim "r1"))))
      (is (= e2 (claim-commit-epoch (%one b 'eb-claim "r1"))))
      (is (= e3 (claim-commit-epoch (%one a 'ea-claim "r2")))))))

(test claim-commit-epoch-is-nil-for-a-reaped-claim
  "Part 1's reader must survive the REAPED-CLAIM structs :AS-OF mixes
into a result list."
  (with-clocked-stores (a b)
    (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1")))
    (let ((c (%one a 'ea-claim "r1")))
      (is (integerp (claim-commit-epoch c)) "control: a live version")
      (is (null (claim-commit-epoch
                 (graph-db.spacetime::%make-reaped-claim (id c))))))))

(test claim-commit-epoch-reads-on-a-clockless-store
  "A store with no system clock still stamps versions from its own
counter, so part 1's reader answers an integer there; only the
:AS-OF-EPOCH reader refuses (recon E9).  *SYSTEM-CLOCK* is bound
explicitly so the premise does not depend on run order."
  (let ((graph-db:*system-clock* nil))
    (with-temp-directory (dir)
      (let ((g (make-graph *ep-a-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (progn
               (is (null (graph-db:graph-system-clock g))
                   "control: no clock")
               (%tx g (lambda () (%unary g #'make-ea-claim-unary "r1")))
               (is (integerp (claim-commit-epoch (%one g 'ea-claim "r1")))))
          (ignore-errors (close-graph g))
          (collect-garbage))))))
```

Register the file: in `graph-db.asd`, inside the `graph-db/spacetime-test` components list, change

```lisp
               (:file "temporal-tests"))               ; GH #296
```

to

```lisp
               (:file "temporal-tests")                ; GH #296
               (:file "epoch-tests"))                  ; GH #347
```

- [ ] **Step 2: Run the new tests to verify they fail**

Run the one-test command from Global Constraints with `TEST-NAME` = `epochs-form-one-sequence-across-two-stores`.
Expected: the fixture assertion passes and the test then FAILS with an undefined-function error naming `CLAIM-COMMIT-EPOCH` (the symbol is the test package's own until the export exists).

- [ ] **Step 3: Export the accessor and write the reader**

In `package.lisp`, change

```lisp
           ;; MVCC: public read path over the versions KEEP-REVISIONS retains
           #:vertex-history
```

to

```lisp
           ;; MVCC: public read path over the versions KEEP-REVISIONS retains
           #:vertex-history
           ;; MVCC: a version's committing epoch (GH #347)
           #:commit-epoch
```

In `spacetime/package.lisp`, after the line exporting `#:claim-version-stamp`, add

```lisp
   #:claim-commit-epoch                                ; GH #347
```

In `spacetime/claim-query.lisp`, directly after the `reaped-claim` defstruct (the form ending `id)`), add

```lisp
(defun claim-commit-epoch (claim)
  "The epoch of the transaction that committed CLAIM's version, or NIL
for a REAPED-CLAIM (a version the store no longer holds) and for a
version stamped 0, written before the store had a counter.  The number
is the writer's own TRANSACTION-ID, comparable across stores only while
they share one SYSTEM-CLOCK -- see GRAPH-DB:GRAPH-SYSTEM-CLOCK (GH
#347)."
  (unless (reaped-claim-p claim)
    (let ((e (graph-db:commit-epoch claim)))
      (and (plusp e) e))))
```

- [ ] **Step 4: Run the three new tests to verify they pass**

Run the one-test command for each of `epochs-form-one-sequence-across-two-stores`, `claim-commit-epoch-is-nil-for-a-reaped-claim`, `claim-commit-epoch-reads-on-a-clockless-store`.
Expected: each PASSES with no failures.

- [ ] **Step 5: Commit**

```bash
git add package.lisp spacetime/package.lisp spacetime/claim-query.lisp \
        graph-db.asd tests/spacetime/epoch-tests.lisp
git commit -m "feat(spacetime): claim-commit-epoch reads a version's commit epoch (#347)

Exports graph-db:commit-epoch; adds the two-store one-clock test fixture.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU"
```

---

### Task 2: `%claim-as-of-epoch` and `:as-of-epoch` on `claims-touching`

**Files:**
- Modify: `spacetime/claim-query.lisp` (`%claim-as-of` region and `claims-touching`)
- Modify: `tests/spacetime/epoch-tests.lisp` (append)

**Interfaces:**
- Consumes: from Task 1, `with-clocked-stores`, `%tx`, `%unary`, `%one`, `claim-commit-epoch`, families `ea-claim`/`eb-claim`.
- Produces: `%claim-as-of-epoch (graph claim epoch) => claim, reaped-claim or NIL`; `claims-touching ... &key ... as-of-epoch` (integer). Task 3 reuses the resolver for `claims-by-producer`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/epoch-tests.lisp`:

```lisp
(test as-of-epoch-selects-the-version-committed-at-or-before
  "#347 part 2 (recon C2): create at E1, commit in B, update the extent
at E2.  :AS-OF-EPOCH E1 -> old extent; E2 -> new; EB, B's epoch between
them, -> old.  The E2 case fails against a strict < comparison, the one
the engine's own snapshot predicate uses."
  (with-clocked-stores (a b)
    (let* ((old (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
           (new (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
           (e1 (%tx a (lambda ()
                        (%unary a #'make-ea-claim-unary "r1" :extent old))))
           (eb (%tx b (lambda () (%unary b #'make-eb-claim-unary "x"))))
           (e2 (%tx a (lambda ()
                        (let ((k (graph-db:copy (%one a 'ea-claim "r1"))))
                          (setf (claim-extent k) new)
                          (graph-db:save k))))))
      (is (< e1 eb e2) "control: B's commit sits between E1 and E2")
      (flet ((at (e)
               (claims-touching a 'ea-claim :region "r1" :role :subject
                                :as-of-epoch e)))
        (is (= 1 (length (at e1))))
        (is (extent-equals-p old (claim-extent (first (at e1)))))
        (is (extent-equals-p new (claim-extent (first (at e2)))))
        (is (extent-equals-p old (claim-extent (first (at eb)))))
        (is (null (at (1- e1))) "not yet created one epoch earlier")))))

(test as-of-epoch-drops-a-claim-retracted-at-or-before
  "#347 part 2 (recon E4): a retraction is a version whose epoch is the
retracting transaction's id.  Create at E1, retract at E2: E1 and EB
(B's epoch between them) return the claim, E2 returns NIL, and the
live version's CLAIM-COMMIT-EPOCH is E2."
  (with-clocked-stores (a b)
    (let* ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
           (eb (%tx b (lambda () (%unary b #'make-eb-claim-unary "x"))))
           (e2 (%tx a (lambda () (retract-claim (%one a 'ea-claim "r1"))))))
      (is (< e1 eb e2) "control: B's commit sits between E1 and E2")
      (flet ((at (e)
               (claims-touching a 'ea-claim :region "r1" :role :subject
                                :as-of-epoch e)))
        (is (= 1 (length (at e1))))
        (is (claim-current-p (first (at e1))))
        (is (= 1 (length (at eb))))
        (is (null (at e2)))
        (is (= e2 (claim-commit-epoch (%one a 'ea-claim "r1"))))))))

(test as-of-epoch-composes-with-current-and-at
  "The downstream filters already guard on REAPED-CLAIM-P, so :CURRENT
and :AT apply to the RESOLVED version exactly as they do under :AS-OF."
  (with-clocked-stores (a b)
    (let* ((old (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
           (new (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
           (e1 (%tx a (lambda ()
                        (%unary a #'make-ea-claim-unary "r1" :extent old))))
           (e2 (%tx a (lambda ()
                        (let ((k (graph-db:copy (%one a 'ea-claim "r1"))))
                          (setf (claim-extent k) new)
                          (graph-db:save k))))))
      (is (null (claims-touching a 'ea-claim :region "r1" :role :subject
                                 :as-of-epoch e1 :at (ts 2022 5 1)))
          "May is outside the E1 version's validity")
      (is (= 1 (length (claims-touching a 'ea-claim :region "r1"
                                        :role :subject
                                        :as-of-epoch e2 :at (ts 2022 5 1)
                                        :current t)))))))
```

- [ ] **Step 2: Run the new tests to verify they fail**

Run the one-test command for `as-of-epoch-selects-the-version-committed-at-or-before`.
Expected: FAIL with an error that `:AS-OF-EPOCH` is not a valid keyword argument to `CLAIMS-TOUCHING`.

- [ ] **Step 3: Write the resolver and thread the keyword**

In `spacetime/claim-query.lisp`, directly after `%claim-as-of` (before `%paginate`), add

```lisp
(defun %claim-as-of-epoch (graph claim epoch)
  "The version of CLAIM live at EPOCH -- committed at or before it and
not retracted at or before it -- or NIL when there is none (created
after EPOCH, or retracted by then), or a REAPED-CLAIM when versions of
that age existed but are past the family's :KEEP-REVISIONS window.
Walks VERTEX-HISTORY newest-first comparing each version's own commit
epoch with <=; RESOLVE-VERSION-AT-EPOCH is a strict snapshot-start
predicate and would drop the commit made AT EPOCH (#347 recon C2, C3).
A retraction is a version, so CLAIM-CURRENT-P on the selected version
is the whole retraction test (recon E4)."
  (let* ((history (graph-db:vertex-history graph (graph-db:id claim)))
         (resolved (loop for (version . e) in history
                         when (<= e epoch) return version)))
    (cond (resolved (and (claim-current-p resolved) resolved))
          ((null history) nil)
          ;; Nothing old enough.  Reaping severs the chain, so only the
          ;; oldest retained REVISION tells created-after-EPOCH (0: it
          ;; is the create) from reaped (> 0) -- recon C4.
          ((zerop (graph-db:revision (car (car (last history))))) nil)
          (t (%make-reaped-claim (graph-db:id claim))))))
```

In `claims-touching`, change the lambda list

```lisp
(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during
                             relation limit offset as-of)
```

to

```lisp
(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during
                             relation limit offset as-of as-of-epoch)
```

In its docstring, after the `:AS-OF` paragraph (the one ending `so replicas answer from their own applied history.`), insert

```
:AS-OF-EPOCH (an integer) answers on the same axis by commit epoch (GH
#347): each claim is the version whose committing transaction id is the
newest at or below it, dropped when that version is retracted, and a
REAPED-CLAIM when older versions existed but are past :KEEP-REVISIONS
-- told from \"created after\" by the oldest retained REVISION.  Epochs
compare across stores only while the stores share one SYSTEM-CLOCK; a
clockless store signals EPOCH-AXIS-UNAVAILABLE.  One of :AS-OF or
:AS-OF-EPOCH, not both.
```

and change the sentence `:AS-OF is the exception -- it answers committed history only, since an uncommitted change is not yet history.` to `:AS-OF and :AS-OF-EPOCH are the exceptions -- they answer committed history only; an uncommitted change is not yet history and has no epoch at all.`

After the existing `(when (and at during) (error "Pass only one of :AT or :DURING, not both."))`, add

```lisp
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
```

Replace the resolve step

```lisp
      (if as-of
          (setf all (loop for c in all
                          for v = (%claim-as-of graph c as-of)
                          when v collect v))
          (setf all (%overlay-transaction
                     graph all family
                     (lambda (c)
                       (or (and (member role '(:subject :either))
                                (equal namespace
                                       (claim-subject-namespace c))
                                (equal key (claim-subject-key c))
                                (or (null relation)
                                    (equal relation (claim-relation c))))
                           (and (member role '(:object :either))
                                (typep c (claim-family-binary family))
                                (equal namespace
                                       (claim-object-namespace c))
                                (equal key (claim-object-key c))))))))
```

with

```lisp
      ;; The overlay is for the neither-axis arm only: an uncommitted
      ;; write has no epoch (#347 recon C6).
      (cond (as-of
             (setf all (loop for c in all
                             for v = (%claim-as-of graph c as-of)
                             when v collect v)))
            (as-of-epoch
             (setf all (loop for c in all
                             for v = (%claim-as-of-epoch graph c
                                                         as-of-epoch)
                             when v collect v)))
            (t
             (setf all (%overlay-transaction
                        graph all family
                        (lambda (c)
                          (or (and (member role '(:subject :either))
                                   (equal namespace
                                          (claim-subject-namespace c))
                                   (equal key (claim-subject-key c))
                                   (or (null relation)
                                       (equal relation
                                              (claim-relation c))))
                              (and (member role '(:object :either))
                                   (typep c (claim-family-binary family))
                                   (equal namespace
                                          (claim-object-namespace c))
                                   (equal key (claim-object-key c)))))))))
```

- [ ] **Step 4: Run the new tests to verify they pass**

Run the one-test command for each of `as-of-epoch-selects-the-version-committed-at-or-before`, `as-of-epoch-drops-a-claim-retracted-at-or-before`, `as-of-epoch-composes-with-current-and-at`.
Expected: each PASSES.

- [ ] **Step 5: Commit**

```bash
git add spacetime/claim-query.lisp tests/spacetime/epoch-tests.lisp
git commit -m "feat(spacetime): claims-touching :as-of-epoch resolves by commit epoch (#347)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU"
```

---

### Task 3: The refusal, the exclusion, the reaped/absent rule, and `claims-by-producer`

**Files:**
- Modify: `spacetime/claim-query.lisp` (a condition beside `reaped-claim`; `claims-touching`'s precondition block; `claims-by-producer`)
- Modify: `spacetime/package.lisp` (exports)
- Modify: `tests/spacetime/epoch-tests.lisp` (append)

**Interfaces:**
- Consumes: from Tasks 1 and 2, the fixture and helpers, `%claim-as-of-epoch`, family `ek-claim` (`:keep-revisions 1`).
- Produces: `epoch-axis-unavailable` (subtype of `graph-db:query-precondition-error`) with reader `epoch-axis-unavailable-graph-name`; `%refuse-epoch-axis (graph)`; `claims-by-producer ... &key limit offset as-of as-of-epoch`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/epoch-tests.lisp`:

```lisp
(test as-of-epoch-tells-reaped-from-created-after
  "#347 recon C4: with :KEEP-REVISIONS 1, the E1 version of a claim
updated twice is reaped -> REAPED-CLAIM; a claim created AFTER E1 in
the same store -> NIL.  Both exhaust the chain identically; only the
oldest retained REVISION tells them apart, so the second case is the
non-vacuity control for the first."
  (with-clocked-stores (a b)
    (let ((e1 (%tx a (lambda () (%unary a #'make-ek-claim-unary "kr")))))
      (dotimes (i 2)
        (%tx a (lambda ()
                 (let ((k (graph-db:copy (%one a 'ek-claim "kr"))))
                   (setf (claim-confidence k) (* 0.1 (1+ i)))
                   (graph-db:save k)))))
      (%tx a (lambda () (%unary a #'make-ek-claim-unary "late")))
      (let ((then (claims-touching a 'ek-claim :region "kr" :role :subject
                                   :as-of-epoch e1)))
        (is (= 1 (length then)))
        (is (reaped-claim-p (first then))
            "the E1 version is past the window: reaped, not substituted")
        (is (equalp (id (%one a 'ek-claim "kr"))
                    (reaped-claim-id (first then)))))
      (is (null (claims-touching a 'ek-claim :region "late" :role :subject
                                 :as-of-epoch e1))
          "created after E1: absent, not reaped")
      (let ((by (claims-by-producer a 'ek-claim "audit" :as-of-epoch e1)))
        (is (= 1 (length by)) "by producer: the reaped one, not the late one")
        (is (reaped-claim-p (first by)))))))

(test as-of-epoch-refuses-a-clockless-store-but-as-of-still-answers
  "#347 recon E9: a store with no system clock draws epochs from its own
counter, so :AS-OF-EPOCH refuses with EPOCH-AXIS-UNAVAILABLE naming the
graph -- a QUERY-PRECONDITION-ERROR whose reason prints -- while :AS-OF
on the same store keeps answering.  *SYSTEM-CLOCK* is bound explicitly
so the premise does not depend on run order."
  (let ((graph-db:*system-clock* nil))
    (with-temp-directory (dir)
      (let ((g (make-graph *ep-a-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((t0 (graph-db.spacetime::%st-now)))
               (is (null (graph-db:graph-system-clock g))
                   "control: no clock")
               (sleep 0.01)
               (%tx g (lambda () (%unary g #'make-ea-claim-unary "r1")))
               (signals epoch-axis-unavailable
                 (claims-touching g 'ea-claim :region "r1" :role :subject
                                  :as-of-epoch 1))
               (signals epoch-axis-unavailable
                 (claims-by-producer g 'ea-claim "audit" :as-of-epoch 1))
               (handler-case
                   (claims-touching g 'ea-claim :region "r1"
                                    :role :subject :as-of-epoch 1)
                 (epoch-axis-unavailable (c)
                   (is (typep c 'graph-db:query-precondition-error))
                   (is (eq *ep-a-name*
                           (epoch-axis-unavailable-graph-name c)))
                   (is (search "no system clock"
                               (graph-db:query-precondition-error-reason
                                c)))))
               (is (null (claims-touching g 'ea-claim :region "r1"
                                          :role :subject :as-of t0))
                   "wall clock still answers: not yet created at T0")
               (is (= 1 (length (claims-touching
                                 g 'ea-claim :region "r1" :role :subject
                                 :as-of (graph-db.spacetime::%st-now))))))
          (ignore-errors (close-graph g))
          (collect-garbage))))))

(test as-of-and-as-of-epoch-are-exclusive
  "Passing both axes signals rather than silently preferring one, on
both readers."
  (with-clocked-stores (a b)
    (let ((now (graph-db.spacetime::%st-now)))
      (signals simple-error
        (claims-touching a 'ea-claim :region "r1" :role :subject
                         :as-of now :as-of-epoch 1))
      (signals simple-error
        (claims-by-producer a 'ea-claim "audit" :as-of now :as-of-epoch 1))
      (is (null (claims-touching a 'ea-claim :region "r1" :role :subject
                                 :as-of-epoch 1))
          "control: one axis alone is accepted"))))

(test claims-by-producer-as-of-epoch-unwinds-an-update
  "The producer index takes the same resolver: E1 answers the old
extent, E2 the new one."
  (with-clocked-stores (a b)
    (let* ((old (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
           (new (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
           (e1 (%tx a (lambda ()
                        (%unary a #'make-ea-claim-unary "r1" :extent old))))
           (e2 (%tx a (lambda ()
                        (let ((k (graph-db:copy (%one a 'ea-claim "r1"))))
                          (setf (claim-extent k) new)
                          (graph-db:save k))))))
      (is (extent-equals-p
           old (claim-extent
                (first (claims-by-producer a 'ea-claim "audit"
                                           :as-of-epoch e1)))))
      (is (extent-equals-p
           new (claim-extent
                (first (claims-by-producer a 'ea-claim "audit"
                                           :as-of-epoch e2))))))))
```

- [ ] **Step 2: Run the new tests to verify they fail**

Run the one-test command for `as-of-epoch-refuses-a-clockless-store-but-as-of-still-answers`.
Expected: FAIL; `EPOCH-AXIS-UNAVAILABLE` is not a defined condition, or `:AS-OF-EPOCH` is rejected by `CLAIMS-BY-PRODUCER`.

- [ ] **Step 3: Define the condition, refuse on both readers, thread `claims-by-producer`**

In `spacetime/claim-query.lisp`, directly after `claim-commit-epoch` (Task 1), add

```lisp
(define-condition epoch-axis-unavailable
    (graph-db:query-precondition-error)
  ((graph-name :initarg :graph-name
               :reader epoch-axis-unavailable-graph-name))
  (:documentation "An :AS-OF-EPOCH read of a store with no system clock.
Its epochs are a private counter, so an answer would look like the
attached case and mean something unrelated (GH #347 recon E9).  The
parent's REASON is filled at the signal site."))

(defun %refuse-epoch-axis (graph)
  "Signal EPOCH-AXIS-UNAVAILABLE unless GRAPH is attached to a clock.
Whether two stores share ONE clock is the consumer's precondition; a
single-store reader cannot see the other store."
  (unless (graph-db:graph-system-clock graph)
    (let ((name (graph-db:graph-name graph)))
      (error 'epoch-axis-unavailable
             :graph-name name
             :reason (format nil "~(~s~) has no system clock; ~
                                  :as-of-epoch needs one"
                             name)))))
```

In `claims-touching`, directly after the `:AS-OF`/`:AS-OF-EPOCH` exclusion `when` added in Task 2, add

```lisp
  (when as-of-epoch (%refuse-epoch-axis graph))
```

Change `claims-by-producer`'s lambda list

```lisp
(defun claims-by-producer (graph claim-class producer
                           &key limit offset as-of)
```

to

```lisp
(defun claims-by-producer (graph claim-class producer
                           &key limit offset as-of as-of-epoch)
```

Add to its docstring, before the final `Uses the PRODUCER index` paragraph:

```
:AS-OF and :AS-OF-EPOCH resolve each claim to the version believed at
a wall-clock instant or live at a commit epoch, exactly as
CLAIMS-TOUCHING does (GH #300, GH #347); one or the other, not both.
```

Replace its body

```lisp
  (let* ((family (claim-family claim-class))
         (all (graph-db:index-lookup graph (claim-family-parent family)
                                     '(producer) producer)))
    (if as-of
        (setf all (loop for c in all
                        for v = (%claim-as-of graph c as-of)
                        when v collect v))
        (setf all (%overlay-transaction
                   graph all family
                   (lambda (c) (equal producer (claim-producer c))))))
    (%paginate all limit offset)))
```

with

```lisp
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
  (when as-of-epoch (%refuse-epoch-axis graph))
  (let* ((family (claim-family claim-class))
         (all (graph-db:index-lookup graph (claim-family-parent family)
                                     '(producer) producer)))
    (cond (as-of
           (setf all (loop for c in all
                           for v = (%claim-as-of graph c as-of)
                           when v collect v)))
          (as-of-epoch
           (setf all (loop for c in all
                           for v = (%claim-as-of-epoch graph c as-of-epoch)
                           when v collect v)))
          (t
           (setf all (%overlay-transaction
                      graph all family
                      (lambda (c) (equal producer (claim-producer c)))))))
    (%paginate all limit offset)))
```

In `spacetime/package.lisp`, after the `#:claim-commit-epoch` export line, add

```lisp
   #:epoch-axis-unavailable                            ; GH #347
   #:epoch-axis-unavailable-graph-name
```

- [ ] **Step 4: Run the new tests to verify they pass**

Run the one-test command for each of `as-of-epoch-tells-reaped-from-created-after`, `as-of-epoch-refuses-a-clockless-store-but-as-of-still-answers`, `as-of-and-as-of-epoch-are-exclusive`, `claims-by-producer-as-of-epoch-unwinds-an-update`.
Expected: each PASSES.

- [ ] **Step 5: Commit**

```bash
git add spacetime/claim-query.lisp spacetime/package.lisp \
        tests/spacetime/epoch-tests.lisp
git commit -m "feat(spacetime): epoch-axis-unavailable, and :as-of-epoch on claims-by-producer (#347)

A clockless store refuses the epoch axis; reaped vs created-after is told
by the oldest retained revision.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU"
```

---

### Task 4: The index-membership bound, the manual, and the full spacetime suite

**Files:**
- Modify: `tests/spacetime/epoch-tests.lisp` (append)
- Modify: `docs/vivace-graph-v3-doc.org` (the claims chapter, the `~:AS-OF~ (a TIMESTAMP)` paragraph and the one after it)

**Interfaces:**
- Consumes: everything from Tasks 1 to 3.
- Produces: nothing new in code; the documented contract.

- [ ] **Step 1: Write the bound test**

Append to `tests/spacetime/epoch-tests.lisp`:

```lisp
(test as-of-epoch-does-not-resurrect-a-deleted-claim
  "The stated bound, pinned: index membership is not snapshot-versioned
(kraison/vivace-graph#345, docs/rules.md), so a claim created at E1 and
MARK-DELETED later is gone from the endpoint index and :AS-OF-EPOCH E1
does not return it.  The read before the delete is the control."
  (with-clocked-stores (a b)
    (let ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1")))))
      (is (= 1 (length (claims-touching a 'ea-claim :region "r1"
                                        :role :subject :as-of-epoch e1)))
          "control: visible before the delete")
      (%tx a (lambda () (graph-db:mark-deleted (%one a 'ea-claim "r1"))))
      (is (null (claims-touching a 'ea-claim :region "r1" :role :subject
                                 :as-of-epoch e1))))))
```

- [ ] **Step 2: Run it**

Run the one-test command for `as-of-epoch-does-not-resurrect-a-deleted-claim`.
Expected: PASS (this test documents existing behaviour; it is green on first run, and the control assertion is what makes it non-vacuous). If it FAILS because the deleted claim IS returned, stop and report: that would mean #345 does not apply to the epoch path, and the docstring and the manual paragraph below must say the opposite.

- [ ] **Step 3: Document the axis in the manual**

In `docs/vivace-graph-v3-doc.org`, in the `~:AS-OF~ (a TIMESTAMP)` paragraph, change its last sentence `No argument or result is ever an epoch.` to `For ~:as-of~, no argument or result is an epoch; the epoch axis is ~:as-of-epoch~, below.`

Then, after the paragraph `One of ~:AT~ or ~:DURING~, not both. A claim with no extent makes no validity statement and is excluded by either filter.`, insert this paragraph:

```
~:AS-OF-EPOCH~ (an integer) answers on the same transaction axis by
*commit epoch* rather than by wall clock (vivace-graph#347): each claim
comes back as the version whose committing transaction id is the newest
at or below the epoch, and drops out when that version is retracted — a
retraction is a version, so its epoch is the retracting transaction's.
When older versions existed but are past ~:keep-revisions~ the claim is
a ~reaped-claim~, told apart from "created after the epoch" by the
oldest retained version's ~revision~ (0 is the create), which is a
sharper rule than the wall-clock axis has. ~claim-commit-epoch~ reads a
version's epoch (~NIL~ for a ~reaped-claim~ or a version stamped before
the store had a counter). Epochs compare across stores only while the
stores share one system clock (~open-system-clock~ and ~make-graph
:system-clock~, Chapter 12); a store with no clock signals
~epoch-axis-unavailable~ — a ~query-precondition-error~ that names the
graph — rather than answering from its private counter, while ~:as-of~
on the same store keeps working. One of ~:as-of~ or ~:as-of-epoch~, not
both. An uncommitted write has no epoch, so ~:as-of-epoch~ answers
committed history only, as ~:as-of~ does. Index membership is not
versioned (vivace-graph#345): a claim deleted after the epoch is not
returned. ~claims-by-producer~ takes ~:as-of-epoch~ the same way.
```

- [ ] **Step 4: Run the whole spacetime suite**

Run the suite command from Global Constraints.
Expected: `Did N checks.` with N at least 653 plus the new tests' checks, `0 failures`, `0 errors`. Record N in the report.

- [ ] **Step 5: Commit**

```bash
git add tests/spacetime/epoch-tests.lisp docs/vivace-graph-v3-doc.org
git commit -m "docs(spacetime): the epoch axis in the manual; pin the index-membership bound (#347)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU"
```

---

## After the tasks (controller)

1. Final whole-branch review, then a comment on kraison/vivace-graph#347 listing: the three parts as delivered; the corrections to the issue's assumptions (recon C2 inclusive comparison, C3 build on `vertex-history`, C4 `revision` discriminator, C6 overlay bypass); that part 3 was a no-op; the check count.
2. Kevin decides the push and the PR against `experiment`. The PR body ends with the Claude Code attribution and session URL.
