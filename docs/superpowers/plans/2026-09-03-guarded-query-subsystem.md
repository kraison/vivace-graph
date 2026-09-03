# graph-db/query Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Home the JSON pattern DSL and the free-text Prolog guard in a web-free subsystem `graph-db/query`, export one runner that returns data, and make a goal's head resolve its functor in its own package.

**Architecture:** `query-dsl.lisp` moves verbatim to `query/dsl.lisp` (package `graph-db`, exports unchanged) minus one ningle line that moves to the REST route. The guard pipeline moves from `gui/prolog.lisp` to `query/guard.lisp` in a new package `graph-db.query`, which exports `run-guarded-prolog` returning `(values columns rows truncated-p)`; the GUI handler becomes a thin caller. `compile-call` derives `NAME/ARITY` in the head symbol's package. A new `graph-db/query-test` suite covers the runner directly; the GUI and core suites are the regression net.

**Tech Stack:** SBCL 2.6.6, ASDF, FiveAM, cl-json (already a core dependency), the existing `graph-db-test-scratch` fixtures.

**Spec:** `docs/superpowers/specs/2026-09-03-guarded-query-subsystem-design.md` (#322).

## Global Constraints

- Lisp: spaces only; hard 80-column limit; terse comments pointing at the spec section or issue.
- **Moves are moves.** Bodies moved in Tasks 1 and 2 are byte-identical to their originals except where a task names an edit; `git diff --color-moved` on the commit must show the bulk as moved, not rewritten.
- Nothing the guard admits or refuses changes (spec §10); the whitelist, exclusions and control words move untouched.
- No new dependency on `graph-db/query` beyond `graph-db/core`. No web package (`ningle`, `lack`, `clack`, `hunchentoot`) is referenced from `query/`.
- Suites run only in a subprocess from the worktree, with the worktree first on `asdf:*central-registry*`:
  `sbcl --dynamic-space-size 4096 --non-interactive --eval '(push #p"<worktree>/" asdf:*central-registry*)' --eval '(ql:quickload :<system> :silent t)' --eval '(asdf:test-system :<system>)'`.
  `asdf:test-system :graph-db/spacetime` and `:graph-db/query` (the non-test systems) are silent no-ops; always target the `-test` system, and read `Did N checks` from the output.
- Local runs per task: `graph-db/gui-test` (about two minutes) and `graph-db/query-test`; the full `graph-db/test` suite and the rest run once, in CI on the single PR (Kevin: batch, limit pushes). Task 4 also runs the core `select` tests by name, listed there.
- Commit trailers: `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` and `Claude-Session: https://claude.ai/code/session_016XhUGNmKWzsBV8PftSVfVo`. No push until the branch is complete.

---

## File structure

| file | after this plan |
|---|---|
| `query/dsl.lisp` | the DSL, moved from `query-dsl.lisp`; ndjson arm returns text only |
| `query/package.lisp` | `graph-db.query` package and exports |
| `query/guard.lisp` | the guard pipeline and `run-guarded-prolog`, moved from `gui/prolog.lisp`; `schema-type-names` from `gui/api.lisp` |
| `gui/prolog.lisp` | flag, request-body helpers, envelope glue, handler |
| `gui/api.lisp` | calls `graph-db.query:schema-type-names` |
| `rest.lisp` | `/query` sets the ndjson content type itself |
| `prologc.lisp` | `make-functor-symbol` homes the functor in the head's package |
| `graph-db.asd` | systems `graph-db/query`, `graph-db/query-test`; `graph-db` depends on `graph-db/query` |
| `tests/query/{package,suite,guard-tests}.lisp` | the new suite |
| `.github/workflows/test.yml`, `docs/ci.md` | the new lane |
| `docs/guarded-query.md`, `CHANGELOG.md`, two doc pointers | docs |

---

### Task 1: The `graph-db/query` system holding the DSL; the ndjson line moves to REST

**Files:**
- Create: `query/dsl.lisp` (git mv from `query-dsl.lisp`)
- Modify: `graph-db.asd`, `rest.lisp`
- Test: `tests/rest-http-tests.lisp` (existing `http-pattern-query-ndjson`, `http-pattern-query-json-body`, `http-def-query-friends`), `tests/rest-tests.lisp` (existing `def-query-returns-json-objects`)

**Interfaces:**
- Produces: system `graph-db/query` (`:depends-on (:graph-db/core)`, pathname `query/`, component `"dsl"`); `graph-db` depends on it and no longer lists `query-dsl`. `emit-query-results` with `:ndjson` returns the text and sets no header.

- [ ] **Step 1: Move the file and rewire the systems**

```bash
mkdir -p query && git mv query-dsl.lisp query/dsl.lisp
```

In `graph-db.asd`, before `(defsystem graph-db`, add:

```lisp
;; The web-free query subsystem (GH #322): the JSON pattern DSL and,
;; from Task 2 on, the free-text Prolog guard.  Depends on core only so
;; a consumer that wants a bounded query tool never loads a web stack.
(defsystem graph-db/query
  :name "VivaceGraph guarded query"
  :description "The JSON pattern DSL and the free-text Prolog guard,
web-free.  docs/guarded-query.md; GH #322."
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "4.0.1"
  :depends-on (:graph-db/core)
  :pathname "query/"
  :serial t
  :components ((:file "dsl")))
```

In `(defsystem graph-db`: add `:graph-db/query` to `:depends-on` after `:graph-db/replication`; delete the `(:file "query-dsl")` component and the comment block above it that explains why it was not in core (it is now the subsystem's header, Step 2); leave `(:file "rest")`.

- [ ] **Step 2: Edit the moved file's header and the ndjson arm**

Replace the header paragraph beginning `;;;; Why this file sits in the :GRAPH-DB system` with:

```lisp
;;;; Home: the graph-db/query subsystem (GH #322), which depends on
;;;; graph-db/core only.  The one web-bound line this file had -- the
;;;; :NDJSON arm setting a content type on NINGLE:*RESPONSE* -- moved to
;;;; rest.lisp's /query route, the only caller that asks for ndjson.
```

In `emit-query-results`, delete the `(setf (lack.response:response-headers ningle:*response*) ...)` form from the `:ndjson` arm and change the docstring's last sentence to `with :NDJSON returns each row as its own JSON line; the caller sets the content type.`

- [ ] **Step 3: Set the header in the REST route**

In `rest.lisp`, `call-rest-pattern-query`, immediately inside `(with-rest-graph (...)` and before `(handler-case (run-pattern-query dsl *graph*)`:

```lisp
      ;; The DSL renders ndjson; the wire header is HTTP's (GH #322).
      (when (string-equal "ndjson"
                          (princ-to-string (or (cdr (assoc :format dsl)) "")))
        (setf (lack.response:response-headers ningle:*response*)
              (list* :content-type "application/x-ndjson"
                     (lack.response:response-headers ningle:*response*))))
```

- [ ] **Step 4: Run the four REST tests by name**

```bash
W=$(pwd)/; sbcl --dynamic-space-size 4096 --non-interactive \
  --eval "(push #p\"$W\" asdf:*central-registry*)" \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(in-package :graph-db/test)' --eval '(log:config :error)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote (http-pattern-query-ndjson http-pattern-query-json-body http-def-query-friends def-query-returns-json-objects)))) (graph-db-test-scratch:cleanup-scratch-run)))'
```

Expected: `Did N checks. Pass: N (100%)`. If `fiveam:run` does not accept a list, run the four names in four `run` calls. Also load `graph-db/gui` in a fresh subprocess and confirm it compiles (`(ql:quickload :graph-db/gui)` exits 0).

- [ ] **Step 5: Commit**

```bash
git add -A query graph-db.asd rest.lisp
git commit -m "refactor(query): graph-db/query holds the DSL; REST sets the ndjson header (#322)"
```

`git show --color-moved --stat` must show `query-dsl.lisp => query/dsl.lisp` as a rename.

---

### Task 2: The guard moves to `query/guard.lisp` in package `graph-db.query`

**Files:**
- Create: `query/package.lisp`, `query/guard.lisp`
- Modify: `gui/prolog.lisp`, `gui/api.lisp`, `gui/package.lisp` (if it imports anything moved), `graph-db.asd`
- Test: `graph-db/gui-test` (existing; unchanged)

**Interfaces:**
- Produces: package `graph-db.query` exporting `prolog-guard-error prolog-guard-error-reason prolog-ill-typed-error prolog-server-fault *prolog-max-query-length* *prolog-max-depth* schema-type-names` and, internally for Task 3, the moved `%`-functions. `gui/prolog.lisp` keeps `*allow-prolog*`, `*prolog-internal-error-message*`, `%prolog-request-body`, `%prolog-field`, `%prolog-request-text`, `%run-guarded-prolog` (rewritten in Task 3) and `api-graph-prolog`.

- [ ] **Step 1: Write the package**

```lisp
;;;; query/package.lisp -- the web-free guarded query subsystem (GH #322).

(defpackage #:graph-db.query
  (:use #:cl)
  (:import-from #:graph-db
                ;; the runner and its bounds (query/dsl.lisp)
                #:run-query-goals #:*query-default-limit*
                #:*query-default-max-inferences* #:*query-default-timeout*
                #:query-param-error #:query-precondition-error
                #:prolog-error #:prolog-resource-error
                #:prolog-permission-error
                ;; the registries the whitelist enumerates
                #:*prolog-global-functors* #:*user-functors*
                ;; schema lookups
                #:schema #:schema-type-table #:node-type-name
                #:lookup-node-type-by-name #:node-type-slots)
  (:export
   ;; the runner (spec SS4)
   #:run-guarded-prolog
   ;; conditions (spec SS3)
   #:prolog-guard-error #:prolog-guard-error-reason
   #:prolog-ill-typed-error #:prolog-server-fault
   ;; the screen's limits
   #:*prolog-max-query-length* #:*prolog-max-depth*
   ;; schema names, shared with the GUI
   #:schema-type-names))
```

Adjust the `:import-from` list to exactly the `graph-db` symbols the moved code references unqualified; the moved code was written in `graph-db.gui`, which uses only `cl`, so every engine reference in it is already package-qualified (`graph-db::...`) and needs no import — check with `grep -c 'graph-db::' query/guard.lisp` and keep the import list minimal (possibly empty). Keep the explicit `graph-db::` qualifications in the moved code rather than importing; that is what "moved verbatim" means.

- [ ] **Step 2: Move the guard**

Create `query/guard.lisp` with `(in-package #:graph-db.query)` and, in this order, the definitions from `gui/prolog.lisp` at these lines (numbers from the file at commit 4c9bc9f): the header comment `;;;; Free-text Prolog ...` through the interning-subtlety paragraph (lines 1–34), then `*prolog-max-query-length*` (48), `*prolog-max-depth*` (51), the three conditions (66–93), `%refuse` (95), `%term-label` (99), `%token-around` (111), `%skip-delimited` (123), `%scan-query-text` (134), `*prolog-readtable*` (186), `%refuse-reader-macro` (189), `%prolog-readtable` (193), `*prolog-scratch-counter*` (203), `%make-scratch-package` (205), `%read-query-forms` (218), `%split-functor-name` (238), `*prolog-excluded-predicates*` (254), `*prolog-goal-argument-control*` (269), `*prolog-cost-unbounded-predicates*` (279), `%excluded-predicate-p` (306), `%functor-whitelist` (311), `%control-word-table` (344), `%schema-name-table` (361), the `guard-ctx` struct (379), `%guard-context` (385), `%routes-to-engine-control-p` (396), `%cut-symbol-p` (418), `%guard-symbol` (425), `%guard-goal` (449), `%guard-term` (495), `%guard-query` (505), `%prolog-functor-names` (524), `%non-finite-p` (588), `%screen-non-finite` (601), `%read-guarded-forms` (612), `*no-applicable-method-type*` (637), `%ill-typed-condition-p` (651), `%run-guarded-query` (682), `%run-guarded-prolog` (718, renamed `%run-guarded-prolog-json` here; Task 3 replaces it).

Also move `%schema-type-names` from `gui/api.lisp` (line 273) as `schema-type-names` (exported), and `%schema-package` (579) — keep it in `guard.lisp` for Task 4 to delete.

Edit the moved header: change `;;;;   1. *ALLOW-PROLOG* -- the flag, checked in the handler` to note the flag stays in the GUI handler (`gui/prolog.lisp`), and add one line: `;;;; Home: graph-db/query (GH #322); the GUI handler is a caller.`

Delete every moved definition from `gui/prolog.lisp`. What remains there: the file header (rewritten to three lines: free-text Prolog HTTP surface; the guard lives in `graph-db.query`; GH #279, #322), `*allow-prolog*`, `*prolog-internal-error-message*`, `%prolog-request-body`, `%prolog-field`, `%prolog-request-text`, `%run-guarded-prolog`, `api-graph-prolog`, and the capabilities/inventory handler if one lives here (check `def-gui-handler` occurrences: two). Qualify the references that now cross packages: `graph-db.query:prolog-guard-error`, `graph-db.query::%refuse` (used by `%prolog-request-text`), `graph-db.query:*prolog-max-query-length*`, `graph-db.query::%prolog-functor-names` (capabilities), `graph-db.query::%run-guarded-prolog-json`, `graph-db.query:prolog-ill-typed-error`, `graph-db.query:prolog-server-fault`.

In `gui/api.lisp`, replace the definition of `%schema-type-names` with nothing and its three call sites with `graph-db.query:schema-type-names`.

- [ ] **Step 3: Wire the system**

`graph-db/query` components become `((:file "package") (:file "dsl") (:file "guard"))`. `graph-db/gui` needs no dependency change (it depends on `graph-db`, which depends on `graph-db/query`).

- [ ] **Step 4: Run the GUI suite**

`asdf:test-system :graph-db/gui-test` from the worktree. Expected: the same check count as before the move (record it from a run on `4c9bc9f` first: `Did N checks`), zero failures. A compile error naming a symbol is a missed qualification; a behaviour change is a moved body that was edited.

- [ ] **Step 5: Commit**

```bash
git add -A query gui graph-db.asd
git commit -m "refactor(query): the guard pipeline moves to graph-db.query (#322)"
```

---

### Task 3: `run-guarded-prolog` returning data; the GUI calls it; the new suite

**Files:**
- Modify: `query/guard.lisp`, `gui/prolog.lisp`, `graph-db.asd`
- Create: `tests/query/package.lisp`, `tests/query/suite.lisp`, `tests/query/guard-tests.lisp`
- Test: `graph-db/query-test` (new), `graph-db/gui-test`

**Interfaces:**
- Produces: `(graph-db.query:run-guarded-prolog text graph &key limit max-inferences timeout (format :data))` → `(values columns rows truncated-p)` per spec §4. Internal `%guarded-rows` shared by both formats. `%run-guarded-prolog-json` deleted; the GUI's `%run-guarded-prolog` builds its envelope from the data.

- [ ] **Step 1: Write the failing tests**

```lisp
;;;; tests/query/package.lisp
(defpackage #:graph-db/query-test
  (:use #:cl #:fiveam)
  (:import-from #:graph-db #:def-vertex #:def-edge #:make-graph
                #:close-graph #:with-transaction #:string-id)
  (:export #:run-query-tests #:query-suite))
```

```lisp
;;;; tests/query/suite.lisp -- runner + fixture for graph-db/query.
(in-package #:graph-db/query-test)

(def-suite query-suite
  :description "The guarded query runner, called directly (GH #322).")

(defun run-query-tests ()
  "Run the suite; T when every check passed.  Invoked by
(asdf:test-system :graph-db/query-test)."
  (log:config :error)
  (let* ((system-dir (graph-db-test-scratch:make-scratch-directory
                      "graph-db-query-sys"))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'query-suite)))
           (explain! results)
           (results-status results))
      (graph-db-test-scratch:cleanup-scratch-run))))

;; A schema in a package that USES NOTHING: the case the head-resolution
;; change (spec SS6) exists for.  Domain-neutral per repo policy #197.
(defpackage #:graph-db/query-test.schema (:use))

(defparameter *graph-name* :query-test-graph)

(eval-when (:load-toplevel :execute)
  (setf (gethash *graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex graph-db/query-test.schema::qt-item ()
  ((label :type string) (rank))
  :query-test-graph)

(def-edge graph-db/query-test.schema::qt-links ()
  ()
  :query-test-graph)

(defmacro with-query-graph ((g) &body body)
  `(let* ((dir (graph-db-test-scratch:make-scratch-directory "graph-db-query"))
          (,g (make-graph *graph-name* (namestring dir)
                          :buffer-pool-size 1000)))
     (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g)))))

(defun seed (g)
  "Three items and two links; returns the items in rank order."
  (with-transaction ((graph-db::transaction-manager g))
    (let ((a (graph-db/query-test.schema::make-qt-item
              :graph g :label "a" :rank 1))
          (b (graph-db/query-test.schema::make-qt-item
              :graph g :label "b" :rank 2))
          (c (graph-db/query-test.schema::make-qt-item
              :graph g :label "c" :rank 3)))
      (graph-db/query-test.schema::make-qt-links :graph g :from a :to b)
      (graph-db/query-test.schema::make-qt-links :graph g :from b :to c)
      (list a b c))))
```

Check `def-vertex`/`def-edge`'s exact constructor names and the edge constructor's `:from`/`:to` initargs against `tests/gui/suite.lisp`'s fixture before relying on them, and copy that fixture's spelling.

```lisp
;;;; tests/query/guard-tests.lisp
(in-package #:graph-db/query-test)
(in-suite query-suite)

(defun q (g text &rest keys)
  (apply #'graph-db.query:run-guarded-prolog text g keys))

(test data-rows-are-json-shaped
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i :qt-item) (node-slot-value ?i label ?l)")
      (is (equal '("i" "l") columns))
      (is (= 3 (length rows)))
      (is (every (lambda (row) (and (stringp (first row))
                                    (stringp (second row))))
                 rows))
      (is (equal '("a" "b" "c") (sort (mapcar #'second rows) #'string<)))
      (is (null truncated)))))

(test raw-rows-carry-nodes
  (with-query-graph (g)
    (seed g)
    (let ((rows (nth-value 1 (q g "(is-a ?i :qt-item)" :format :raw))))
      (is (every (lambda (row) (graph-db::node-p (first row))) rows)))))

(test limit-clamps-and-flags-truncation
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i :qt-item)" :limit 2)
      (declare (ignore columns))
      (is (= 2 (length rows)))
      (is (eq t truncated)))
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i :qt-item)" :limit 3)
      (declare (ignore columns))
      (is (= 3 (length rows)))
      (is (null truncated)))))

(test each-screened-token-is-refused-and-its-absence-accepted
  (with-query-graph (g)
    (seed g)
    (dolist (pair '(("(is-a ?i graph-db::qt-item)" . "(is-a ?i :qt-item)")
                    ("(is-a ?i #.(quit))" . "(is-a ?i :qt-item)")
                    ("(is-a ?i `x)" . "(is-a ?i :qt-item)")
                    ("(is-a ?i ,x)" . "(is-a ?i :qt-item)")
                    ("(is-a ?i :qt-item" . "(is-a ?i :qt-item)")))
      (signals graph-db.query:prolog-guard-error (q g (car pair)))
      (finishes (q g (cdr pair))))))

(test unregistered-functor-and-string-head-refused
  (with-query-graph (g)
    (signals graph-db.query:prolog-guard-error (q g "(no-such-thing ?x)"))
    (signals graph-db.query:prolog-guard-error (q g "(\"is-a\" ?x :qt-item)"))
    (signals graph-db.query:prolog-guard-error (q g "(lisp ?x (quit))"))))

(test an-inference-budget-breach-is-a-resource-error
  (with-query-graph (g)
    (seed g)
    (signals graph-db:prolog-resource-error
      (q g "(is-a ?i :qt-item) (is-a ?j :qt-item) (is-a ?k :qt-item)"
         :max-inferences 2))))

(test the-scratch-package-is-gone-afterwards
  (with-query-graph (g)
    (seed g)
    (let ((before (length (list-all-packages))))
      (q g "(is-a ?i :qt-item)")
      (ignore-errors (q g "(is-a ?i #.(quit))"))
      (is (= before (length (list-all-packages)))))))

(test edge-and-global-functors-resolve-in-one-goal-list
  "Spec SS6: the schema package uses nothing, so before the
head-resolution change QT-LINKS/2 could not be found from GRAPH-DB's
package nor IS-A/2 from the schema's."
  (with-query-graph (g)
    (destructuring-bind (a b c) (seed g)
      (declare (ignore c))
      (multiple-value-bind (columns rows)
          (q g "(is-a ?x :qt-item) (qt-links ?x ?y)")
        (is (equal '("x" "y") columns))
        (is (= 2 (length rows)))
        (is (member (list (string-id a) (string-id b)) rows
                    :test #'equal))))))
```

The last test is expected to **fail** until Task 4; mark it `(test (edge-and-global-functors-resolve-in-one-goal-list :depends-on nil) ...)` is not needed — leave it failing and note the expected count in the ledger.

```lisp
;; graph-db.asd
(defsystem graph-db/query-test
  :name "VivaceGraph guarded-query test suite"
  :depends-on (:graph-db/query :graph-db/test-scratch :fiveam)
  :pathname "tests/query/"
  :serial t
  :components ((:file "package") (:file "suite") (:file "guard-tests"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :graph-db/query-test :run-query-tests)
               (error "graph-db query tests failed."))))
```

and on `graph-db/query`: `:in-order-to ((test-op (test-op :graph-db/query-test)))`.

- [ ] **Step 2: Run to verify failure**

`asdf:test-system :graph-db/query-test`. Expected: `run-guarded-prolog` undefined.

- [ ] **Step 3: Implement the runner**

In `query/guard.lisp`, replace `%run-guarded-query` and `%run-guarded-prolog-json` with:

```lisp
(defun %clamp-cap (limit)
  (if (and (integerp limit) (plusp limit))
      (min limit graph-db::*query-default-limit*)
      graph-db::*query-default-limit*))

(defun %probe (cap)
  "One past CAP tells truncated from exactly full; at the ceiling there
is no room, so an exactly-full page reads as truncated (GH #278)."
  (if (< cap graph-db::*query-default-limit*) (1+ cap) cap))

(defun %run-guarded-goals (vars goals graph probe)
  "The already-guarded query's rows, RAW, at most PROBE of them, with
the GUI's three-way condition contract (GH #279)."
  (handler-case
      (let ((rows '()))
        (graph-db::run-query-goals
         vars goals graph :limit probe :format :raw
         :callback (lambda (row) (push row rows)))
        (nreverse rows))
    (graph-db:prolog-error (c) (error c))
    (graph-db:query-param-error (c) (error c))
    (error (c)
      (cond ((%ill-typed-condition-p c)
             (log:error "query guard: ill-typed query (~S): ~A"
                        (type-of c) c)
             (error 'prolog-ill-typed-error))
            (t
             (log:error "query guard: UNEXPECTED SERVER FAULT (~S): ~A"
                        (type-of c) c)
             (error 'prolog-server-fault))))))

(defun run-guarded-prolog (text graph &key limit max-inferences timeout
                                            (format :data))
  "Screen, read, guard and run TEXT against GRAPH; (VALUES COLUMNS ROWS
TRUNCATED-P).  COLUMNS are the variables in first-appearance order as
downcased wire strings; ROWS one list per solution, cells JSON-shaped
under :DATA (a node is its id string; strings, numbers, T, NIL pass) or
as bound under :RAW.  LIMIT is clamped to *QUERY-DEFAULT-LIMIT*;
MAX-INFERENCES and TIMEOUT bind the DSL's budgets for this call.
Refusals signal PROLOG-GUARD-ERROR; see the header for the rest of the
condition contract (spec SS4, GH #322)."
  (check-type format (member :data :raw))
  (let* ((scratch (%make-scratch-package))
         (cap (%clamp-cap limit))
         (probe (%probe cap))
         (graph-db::*query-default-max-inferences*
           (or max-inferences graph-db::*query-default-max-inferences*))
         (graph-db::*query-default-timeout*
           (or timeout graph-db::*query-default-timeout*)))
    (unwind-protect
         (multiple-value-bind (vars goals)
             (%read-guarded-forms text scratch (%guard-context graph scratch))
           (let* ((rows (%run-guarded-goals vars goals graph probe))
                  (n (length rows))
                  (truncated (if (> probe cap) (> n cap) (>= n cap)))
                  (shown (if (> n cap) (subseq rows 0 cap) rows)))
             (values (mapcar #'graph-db::%query-var-field vars)
                     (if (eq format :data)
                         (mapcar (lambda (row)
                                   (mapcar #'graph-db::%query-value->json row))
                                 shown)
                         shown)
                     (and truncated t))))
      (delete-package scratch))))
```

`run-query-goals` today renders through `emit-query-results` and has no `:raw`/`:callback` path. Add to `query/dsl.lisp`'s `run-query-goals` a `:format :raw` arm that requires a `callback` keyword and calls it per row instead of emitting — a small extension of the existing `run` closure (`(funcall run callback)`), with the docstring noting the new arm. `%query-value->json` and `%query-var-field` are internal to `graph-db`; reference them qualified as above.

Then rewrite the GUI's `%run-guarded-prolog` in `gui/prolog.lisp`:

```lisp
(defun %run-guarded-prolog (text limit graph)
  "The workbench envelope over GRAPH-DB.QUERY:RUN-GUARDED-PROLOG (GH
#322).  The runner clamps and probes exactly as the DSL endpoint does,
so the envelope's CAP/PROBE arithmetic is unchanged."
  (let ((cap (%clamp-row-cap limit)))
    (multiple-value-bind (columns rows)
        (graph-db.query:run-guarded-prolog
         text graph :limit (%query-probe-limit cap))
      (%query-envelope
       (graph-db::query-results->json-from-fields columns rows)
       cap (%query-probe-limit cap)))))
```

`query-results->json` takes return VARS (symbols) and rows of raw values; add in `query/dsl.lisp` a sibling `query-results->json-from-fields (fields rows)` that takes field strings and already-converted cells and encodes the same array-of-objects shape (same `encode-json-alist` discipline as `query-results->json`, GH #279). Export it from `graph-db`. The envelope decodes that string as before, so the GUI's wire shape is unchanged.

Wait: `run-guarded-prolog` is asked for `(%query-probe-limit cap)` rows and itself probes one past *that*; to keep the GUI's envelope arithmetic exact, pass `:limit cap` and let the runner's own truncation flag drive the envelope instead: change `%query-envelope`'s caller to hand it `truncated` directly. Simplest correct form:

```lisp
(defun %run-guarded-prolog (text limit graph)
  (let ((cap (%clamp-row-cap limit)))
    (multiple-value-bind (columns rows truncated)
        (graph-db.query:run-guarded-prolog text graph :limit cap)
      (%query-envelope-from-rows columns rows cap truncated))))
```

and split `%query-envelope` in `gui/api.lisp` into the existing string-taking entry (used by the builder endpoint) and `%query-envelope-from-rows (columns rows cap truncated)` that both call to build the JSON response object. Keep the response keys and order identical (`columns rows rowCount limit truncated`).

- [ ] **Step 4: Run both suites**

`graph-db/query-test`: everything passes except `edge-and-global-functors-resolve-in-one-goal-list` (expected until Task 4; record the failure text). `graph-db/gui-test`: same count as Task 2, zero failures.

- [ ] **Step 5: Commit**

```bash
git add -A query gui tests/query graph-db.asd
git commit -m "feat(query): run-guarded-prolog returns data; the GUI is a caller; query-test suite (#322)"
```

---

### Task 4: Head resolution in the head's package

**Files:**
- Modify: `prologc.lisp` (`make-functor-symbol`, header comment), `query/guard.lisp` (drop `:package` and `%schema-package`)
- Test: `tests/query/guard-tests.lisp` (the failing test from Task 3), core `tests/query-tests.lisp` select tests by name, `graph-db/gui-test`

**Interfaces:**
- Produces: `(make-functor-symbol symbol arity)` interns `NAME/ARITY` in `(symbol-package symbol)` when that is non-NIL, else in `*package*`.

- [ ] **Step 1: Confirm the failing test fails for the right reason**

Run `graph-db/query-test`; the failure text must be `unknown Prolog functor QT-LINKS/2` (or `IS-A/2`), not a fixture error.

- [ ] **Step 2: Implement**

```lisp
(defun make-functor-symbol (symbol arity)
  "The NAME/ARITY functor symbol for SYMBOL, interned where SYMBOL lives
-- so a goal list whose canonical heads span the engine's package and a
schema's resolves each in its home (GH #322).  An uninterned SYMBOL, or
a string, falls back to *PACKAGE*, which is what every caller saw before."
  (let ((*package* (or (and (symbolp symbol) (symbol-package symbol))
                       *package*)))
    (new-interned-symbol symbol '/ arity)))
```

Read `new-interned-symbol` (utilities.lisp:155) first: if it takes a package argument, pass it instead of rebinding `*package*`.

In `query/guard.lisp`, `%run-guarded-goals` passes no `:package` (the default is `graph-db`, now irrelevant for guarded heads); delete `%schema-package`. Update the moved header's step 5 to say heads resolve in their own homes.

- [ ] **Step 3: Run the suites**

`graph-db/query-test`: all pass. `graph-db/gui-test`: unchanged count, zero failures. The core `select` tests by name, in a subprocess as in Task 1 step 4: `select-flat-is-a select-one-returns-single select-no-matches-is-nil select-edge-functor-pairs select-callback-streams-each-row select-count-returns-solution-count select-count-honors-limit-and-skip select-join-multiple-goals` plus the four REST tests. Expected: all pass. `graph-db/spacetime-test`: 650, unchanged (it uses `select` through the guard nowhere, but it is cheap).

- [ ] **Step 4: Commit**

```bash
git add prologc.lisp query/guard.lisp
git commit -m "fix(prolog): a goal head resolves its functor in its own package (#322)"
```

---

### Task 5: CI lane, docs, changelog

**Files:**
- Modify: `.github/workflows/test.yml`, `docs/ci.md`, `CHANGELOG.md`, `docs/superpowers/specs/2026-08-27-vg-gui-v1-design.md`, `docs/spatiotemporal-substrate-programme.md`
- Create: `docs/guarded-query.md`

- [ ] **Step 1: CI lane**

Copy the `spacetime suite` step in `.github/workflows/test.yml` as a new step `query suite` immediately after it, with `:graph-db/query-test` in both `quickload` and `test-system`. In `docs/ci.md`, add `query` to the list of suites in the first paragraph.

- [ ] **Step 2: `docs/guarded-query.md`**

Sections: what the subsystem is and why it is web-free (#322, one paragraph); loading it (`(ql:quickload :graph-db/query)`); `run-guarded-prolog` — signature, the `:data`/`:raw` shapes with one example each, budgets and truncation; the condition contract as a table (guard refusal / engine's own Prolog conditions / ill-typed / server fault, what the caller sees, what is logged); head resolution (spec §6, three sentences); what the guard admits, pointing at the whitelist rules in `guard.lisp`'s header rather than restating them; the GUI and REST as callers; consumers (cl-llm's query tool, blackboard) in one sentence.

- [ ] **Step 3: CHANGELOG and pointers**

`[Unreleased]` → Added: `graph-db/query` and `run-guarded-prolog` (two sentences, #322). Fixed: head resolution (#322, second finding). In the GUI design doc's Prolog section add one line: the guard pipeline lives in `graph-db/query` since #322. In the substrate programme doc's related issues, note #322 landed.

- [ ] **Step 4: Full local check and commit**

Run `graph-db/query-test` and `graph-db/gui-test` one last time. Commit:

```bash
git add .github docs CHANGELOG.md
git commit -m "docs(query): guarded-query guide, CI lane, changelog (#322)"
```

Do not push. The PR, when Kevin says so, carries the five commits and one CI run.

---

## Self-review notes

- **Spec coverage.** §2 → Tasks 1–2. §3 → Task 2 (package, exports). §4 → Task 3. §5 → Task 1. §6 → Task 4. §7 → Task 3 (suite) and Task 4 (the motivating test); CI → Task 5. §8 consumers → out of this repo; cl-llm re-homing is filed at landing. §9 → Task 5.
- **Deviation recorded.** Spec §4 says `%schema-package` "stays for the DSL path only"; the DSL resolves its package inside `compile-pattern-query` and never called the guard's helper, so Task 4 deletes it. Spec §4's GUI paragraph is realised by `%query-envelope-from-rows` (Task 3) rather than re-encoding to a string and decoding it; the wire shape is the same and the GUI suite proves it.
- **Type consistency.** `run-guarded-prolog text graph &key limit max-inferences timeout format` in Tasks 3–5 and the docs; `schema-type-names graph parent` in Tasks 2 and 5; `%run-guarded-goals vars goals graph probe` only inside Task 3.
- **Placeholders.** None; the two "check before relying" notes (constructor spelling, `new-interned-symbol`'s arglist) name the file and line to read.
