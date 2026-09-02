# Package-Aware Bare Type Names (#190) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Kill the schema's package-blind keyword alias: a bare (keyword) type
name resolves to the unique matching registered type or signals loudly, never
silently to whichever same-named type was defined last.

**Architecture:** `update-node-type` stops writing the keyword alias key into
the schema type-table. `lookup-node-type-by-name` gains an explicit resolution
path for keywords: scan the sub-table's real (package-qualified) symbol keys by
`symbol-name`; zero matches → NIL, one → that type, more → a new
`ambiguous-node-type-name` error naming every candidate package-qualified.
Stale alias entries persisted in old `schema.dat` files become inert (the
keyword path never consults `gethash` directly). The REST/DSL layer converts
the new condition into its existing error surfaces.

**Tech Stack:** Common Lisp (SBCL primary; code must keep the
`#+sbcl/#+ccl/#+ecl/#+lispworks` discipline where touched — nothing here is
impl-conditional), FiveAM.

**Spec:** `docs/superpowers/specs/2026-08-20-namespaces-design.md` §3.4
(records this as the known defect), plus GH #190 (the authoritative
statement) and the #201 decision comment (the same resolution policy —
suffix/bare-name match, error on ambiguity — chosen for the peer wire).

## Global Constraints

- Lisp: spaces only, never tabs; hard 80-column limit (a 96-column line is a
  defect).
- Comments: terse, state the invariant, cite `(GH #190)`; history goes in the
  issue/commit message.
- Run suites with SBCL `--dynamic-space-size 16384`; the default heap dies.
- **Reconcile suite counts**: record the full-suite pass count before and
  after; every delta must equal the checks added (baseline at merge of #203:
  3982 pass / 10 skip / 0 fail).
- **Ablate every guard**: each new test must fail against the nearest wrong
  implementation (listed per test below); actually run the ablation for the
  ambiguity guard.
- ECL is demoted: verify on SBCL only, say so explicitly.
- Host safety: a live `ma-dev-server` runs on this host. **Never
  `pkill`/`pgrep -f` on `sbcl` or a bare path** — kill only the specific PID
  you started. Never two SBCL builds at once (shared FASL cache). Never open a
  graph under `/data0`.
- Work in a worktree under `.worktrees/` on branch `190-keyword-alias`, off
  current `experiment` (eb307ed or later). PR against `experiment`. Pushing is
  explicit-only — do not push without Kevin's say-so.
- Tests write scratch databases under `/var/tmp/`; the fixtures'
  `with-temp-directory` cleans up after itself.

---

### Task 1: Ambiguity-aware bare-name resolution in the schema layer

**Files:**
- Modify: `schema.lisp:222-241` (`lookup-node-type-by-name`,
  `update-node-type`; new condition + helpers directly above
  `lookup-node-type-by-name`)
- Modify: `package.lisp:178` region (export the new condition + readers)
- Modify: `node-class.lisp:300-310` region (one docstring sentence in
  `resolve-node-type-ids`: an ambiguous bare designator now signals instead of
  being skipped)
- Create: `tests/keyword-alias-tests.lisp`
- Modify: `graph-db.asd` (add `(:file "keyword-alias-tests")` immediately
  after `(:file "type-id-seeding-tests")`, line ~526)

**Interfaces:**
- Produces: condition `ambiguous-node-type-name` with readers
  `ambiguous-type-name`, `ambiguous-type-parent`,
  `ambiguous-type-candidates` (list of package-qualified symbols, sorted by
  package name then symbol name); helper
  `%qualified-type-name-string (symbol) -> string`. Task 2 consumes all of
  these. `lookup-node-type-by-name (name parent &key graph)` keeps its exact
  signature and its behavior for non-keyword symbols.

- [ ] **Step 1: Write the failing tests**

Create `tests/keyword-alias-tests.lisp`:

```lisp
;;;; Bare (keyword) type names resolve package-aware or signal (GH #190).
(in-package #:graph-db/test)

(def-suite keyword-alias-suite :in graph-db-suite
  :description "Bare type names: unique match or a loud ambiguity error.")
(in-suite keyword-alias-suite)

;;; Two packages that legitimately share a symbol-name -- the namespaces
;;; design's motivating case (spec 3.4, GH #190).  Nothing imports them;
;;; the qualified reference is the point.
(defpackage #:graph-db-alias-pkg-a
  (:use)
  (:export #:alias-species #:alias-unique))
(defpackage #:graph-db-alias-pkg-b
  (:use)
  (:export #:alias-species))

;;; Both ALIAS-SPECIES types land in ONE store's schema -- that is the
;;; collision.  The generated MAKE-/LOOKUP-/-P helpers intern in THIS
;;; package, so the second definition redefines the first's helpers; the
;;; tests below use only LOOKUP-NODE-TYPE-BY-NAME, never those helpers.
(def-vertex graph-db-alias-pkg-a:alias-species () ((label :type string))
  :alias-two-store)
(def-vertex graph-db-alias-pkg-b:alias-species () ((label :type string))
  :alias-two-store)
(def-vertex graph-db-alias-pkg-a:alias-unique () ((label :type string))
  :alias-solo-store)

(defmacro with-alias-test-graph ((g store-name) &body body)
  "One store under its own system directory, like WITH-TWO-TEST-GRAPHS
(global-type-id-tests) but for a single graph of a chosen name."
  (let ((s (gensym)) (d (gensym)))
    `(with-temp-directory (,s)
       (with-temp-directory (,d)
         (let ((graph-db::*system-directory* (namestring ,s)))
           (let ((,g (make-graph ,store-name (namestring ,d)
                                 :buffer-pool-size 1000)))
             (unwind-protect (progn ,@body)
               (ignore-errors (close-graph ,g :snapshot-p nil))
               (collect-garbage))))))))

(defun %vertex-sub-table (graph)
  (gethash :vertex (graph-db::schema-type-table (graph-db::schema graph))))

(test bare-name-resolves-when-unique
  "A keyword designator still works when exactly one registered type
matches -- the public make-vertex/map-vertices convenience survives.
Nearest wrong implementation: keyword lookups always return NIL."
  (with-alias-test-graph (g :alias-solo-store)
    (let ((meta (lookup-node-type-by-name :alias-unique :vertex :graph g)))
      (is (graph-db::node-type-p meta))
      (is (eq 'graph-db-alias-pkg-a:alias-unique
              (graph-db::node-type-name meta))))))

(test ambiguous-bare-name-signals
  "Two same-named types in different packages: a bare name is genuinely
ambiguous and must ERROR, never resolve to whichever was defined last.
Nearest wrong implementation: return the first (or last) match."
  (with-alias-test-graph (g :alias-two-store)
    (signals graph-db:ambiguous-node-type-name
      (lookup-node-type-by-name :alias-species :vertex :graph g))))

(test qualified-names-resolve-past-the-ambiguity
  "The package-qualified symbols each reach their own type, with distinct
registry ids -- the alias collision never touched the real keys."
  (with-alias-test-graph (g :alias-two-store)
    (let ((meta-a (lookup-node-type-by-name
                   'graph-db-alias-pkg-a:alias-species :vertex :graph g))
          (meta-b (lookup-node-type-by-name
                   'graph-db-alias-pkg-b:alias-species :vertex :graph g)))
      (is (eq 'graph-db-alias-pkg-a:alias-species
              (graph-db::node-type-name meta-a)))
      (is (eq 'graph-db-alias-pkg-b:alias-species
              (graph-db::node-type-name meta-b)))
      (is (/= (graph-db::node-type-id meta-a)
              (graph-db::node-type-id meta-b))))))

(test keyword-alias-no-longer-written
  "UPDATE-NODE-TYPE stops writing the third (keyword) key.  Fails against
the pre-#190 code, which stored :ALIAS-UNIQUE -> id here."
  (with-alias-test-graph (g :alias-solo-store)
    (is (null (gethash :alias-unique (%vertex-sub-table g))))))

(test stale-persisted-alias-is-ignored
  "Old schema.dat files carry alias entries written by the old code, and
after the very collision this fixes they can point at the WRONG id.  The
keyword path must resolve by scanning the real symbol keys, never by
GETHASH on the keyword.  Nearest wrong implementation: try GETHASH first
and only scan on a miss."
  (with-alias-test-graph (g :alias-solo-store)
    (setf (gethash :alias-unique (%vertex-sub-table g)) 999999)
    (let ((meta (lookup-node-type-by-name :alias-unique :vertex :graph g)))
      (is (graph-db::node-type-p meta))
      (is (/= 999999 (graph-db::node-type-id meta))))))
```

- [ ] **Step 2: Wire the file into the test system and watch the tests fail**

In `graph-db.asd`, after `(:file "type-id-seeding-tests")    ; GH #186` add:

```lisp
               (:file "keyword-alias-tests")      ; GH #190
```

Run (from the worktree, fresh SBCL, kill only this PID afterwards):

```
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:quickload :graph-db)' \
  --eval '(asdf:load-system :graph-db/test)' \
  --eval '(fiveam:run! (quote graph-db/test::keyword-alias-suite))'
```

Expected: `ambiguous-bare-name-signals`, `keyword-alias-no-longer-written`,
and `stale-persisted-alias-is-ignored` FAIL (the first because no condition
of that name exists — a compile-time failure of the test file also counts,
fix by loading after Step 3 and confirming the *tests* fail for behavioral
reasons where possible); `bare-name-resolves-when-unique` and
`qualified-names-resolve-past-the-ambiguity` may pass already (they pin
behavior that must survive).

Note: `ambiguous-node-type-name` is referenced as `graph-db:...` so the test
file cannot even compile until the symbol is exported. To observe true
behavioral failure first, it is acceptable to add the condition + export
(Step 3's first hunk) and then run the tests before changing
`update-node-type`/`lookup-node-type-by-name`; the three tests above must
fail at that point.

- [ ] **Step 3: Implement**

In `schema.lisp`, replace lines 222–241 (`lookup-node-type-by-name` and
`update-node-type`) with:

```lisp
(define-condition ambiguous-node-type-name (error)
  ((name :initarg :name :reader ambiguous-type-name)
   (parent :initarg :parent :reader ambiguous-type-parent)
   (candidates :initarg :candidates :reader ambiguous-type-candidates))
  (:report
   (lambda (c s)
     (format s "The bare type name ~S names ~D registered ~(~A~) types: ~
~{~A~^, ~}.  A bare name resolves only when unique; use the ~
package-qualified symbol (GH #190)."
             (ambiguous-type-name c)
             (length (ambiguous-type-candidates c))
             (ambiguous-type-parent c)
             (mapcar #'%qualified-type-name-string
                     (ambiguous-type-candidates c))))))

(defun %qualified-type-name-string (symbol)
  "SYMBOL printed package-qualified regardless of the ambient *PACKAGE* --
the package is the discriminator in every message that uses this (GH #190)."
  (let ((*package* (find-package :keyword)))
    (prin1-to-string symbol)))

(defun %resolve-bare-type-name (name parent graph)
  "The unique registered PARENT-kind type whose SYMBOL-NAME matches bare
NAME, as the schema's real (package-qualified) key.  NIL when none match;
AMBIGUOUS-NODE-TYPE-NAME when more than one does -- resolving a genuinely
ambiguous name by definition order is the wrong-class read GH #190 exists
to forbid.  Scans only symbol->id entries: keyword keys may survive in a
schema.dat written before #190 and can point at a clobbered id."
  (let ((sub (gethash parent (schema-type-table (schema graph))))
        (matches nil))
    (when sub
      (maphash (lambda (key value)
                 (when (and (symbolp key) (not (keywordp key))
                            (integerp value)
                            (string= (symbol-name key) (symbol-name name)))
                   (push key matches)))
               sub))
    (cond ((null matches) nil)
          ((null (cdr matches)) (first matches))
          (t (error 'ambiguous-node-type-name
                    :name name :parent parent
                    :candidates (sort matches #'string<
                                      :key #'%qualified-type-name-string))))))

(defun lookup-node-type-by-name (name parent &key (graph *graph*))
  "The NODE-TYPE metadata NAME names among GRAPH's PARENT (:VERTEX/:EDGE)
types, or NIL.  A keyword NAME is a bare-name designator: it resolves to
the unique matching type or signals AMBIGUOUS-NODE-TYPE-NAME (GH #190).  A
non-keyword symbol is the type's identity and is looked up directly."
  (let ((key (if (keywordp name)
                 (%resolve-bare-type-name name parent graph)
                 name)))
    (when key
      (let ((id (gethash key (gethash parent
                                      (schema-type-table (schema graph))))))
        (when id
          (lookup-node-type-by-id id parent :graph graph))))))

(defmethod update-node-type ((meta node-type) (graph graph))
  ;; Two keys, not three: the keyword alias this also wrote was
  ;; package-blind -- two same-named types clobbered one entry (GH #190).
  (setf (gethash (node-type-id meta)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        meta)
  (setf (gethash (node-type-name meta)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        (node-type-id meta))
  (finalize-inheritance (find-class (node-type-name meta)))
  (save-schema (schema graph) graph))
```

In `package.lisp`, next to `#:lookup-node-type-by-name` (line ~178) add:

```lisp
           #:ambiguous-node-type-name
           #:ambiguous-type-name
           #:ambiguous-type-parent
           #:ambiguous-type-candidates
```

In `node-class.lisp` (`resolve-node-type-ids` docstring, the "Designators
that resolve to no registered type of KIND are skipped" sentence), append:

```
An AMBIGUOUS bare (keyword) designator is not skipped: it signals
AMBIGUOUS-NODE-TYPE-NAME (GH #190).
```

Also check `%store-schema-claims`'s docstring reference to "triple-keyed
(id -> meta, symbol -> id, keyword -> id; see UPDATE-NODE-TYPE)"
(`schema.lisp:509`): reword to "double-keyed (id -> meta, symbol -> id);
schemas written before GH #190 may also carry stale keyword aliases, which
the key discrimination below skips."

- [ ] **Step 4: Run the new suite, then the full suite; reconcile counts**

Run the Step 2 command again: all 5 tests PASS (they contain 9 checks).
Then run the full suite:

```
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:quickload :graph-db)' \
  --eval '(asdf:test-system :graph-db)'
```

Expected: baseline 3982 pass / 10 skip / 0 fail becomes 3991 pass / 10 skip
/ 0 fail. Any other delta must be explained check-by-check before
proceeding.

- [ ] **Step 5: Ablation — actually run it**

Temporarily change `%resolve-bare-type-name`'s ambiguity branch to
`(first matches)` (the nearest wrong implementation), re-run the
keyword-alias suite, and confirm `ambiguous-bare-name-signals` FAILS.
Temporarily restore the old `gethash`-first keyword path and confirm
`stale-persisted-alias-is-ignored` FAILS. Revert both ablations; re-run;
green.

- [ ] **Step 6: Commit**

```bash
git add schema.lisp package.lisp node-class.lisp graph-db.asd \
        tests/keyword-alias-tests.lisp
git commit -m "fix(schema): bare type names resolve package-aware or signal (#190)"
```

(Docs land in Task 3; the push hook checks pushes, not commits.)

---

### Task 2: REST/DSL layer surfaces ambiguity as a client error

**Files:**
- Modify: `rest.lisp:487-496` (`%dsl-resolve-type`), `rest.lisp:638-660`
  (`rest-post-vertex`), `rest.lisp:695-715` (`rest-post-edge`); new helper
  `%rest-resolve-post-type` next to `%dsl-resolve-type`
- Modify: `tests/rest-tests.lisp` (append two tests)

**Interfaces:**
- Consumes: `ambiguous-node-type-name`, `ambiguous-type-candidates`,
  `%qualified-type-name-string` from Task 1.
- Produces: `%rest-resolve-post-type (type-name parent) -> (values meta-or-nil
  error-string-or-nil)` — exactly one non-NIL; used by both POST handlers.

- [ ] **Step 1: Write the failing tests**

Append to `tests/rest-tests.lisp` (it is already in package `graph-db/test`,
suite `rest-suite`; the fixtures from `tests/keyword-alias-tests.lisp` load
earlier in the same system, but stay self-contained — reuse only the two
packages, which that file defines at load time):

```lisp
(test dsl-ambiguous-type-name-is-a-query-param-error
  "An ambiguous bare type name from a REST client must come back as the
DSL's own client-error surface, not a raw internal condition.  Nearest
wrong implementation: let AMBIGUOUS-NODE-TYPE-NAME propagate raw (the
test would then see the wrong condition class)."
  (with-alias-test-graph (g :alias-two-store)
    (signals graph-db::query-param-error
      (graph-db::%dsl-resolve-type "aliasSpecies" :vertex g))))

(test rest-post-type-resolution-reports-ambiguity
  "The POST vertex/edge path returns (values NIL message) for an ambiguous
name and (values meta NIL) for a unique one."
  (with-alias-test-graph (g :alias-two-store)
    (let ((graph-db::*graph* g))
      (multiple-value-bind (meta msg)
          (graph-db::%rest-resolve-post-type "aliasSpecies" :vertex)
        (is (null meta))
        (is (search "mbiguous" msg)))))
  (with-alias-test-graph (g :alias-solo-store)
    (let ((graph-db::*graph* g))
      (multiple-value-bind (meta msg)
          (graph-db::%rest-resolve-post-type "aliasUnique" :vertex)
        (is (graph-db::node-type-p meta))
        (is (null msg))))))
```

`with-alias-test-graph` is defined in `tests/keyword-alias-tests.lisp`,
which loads before `rest-tests` in the .asd — if the reviewer prefers no
cross-file fixture use, move the macro to `tests/fixtures/` instead; either
is acceptable, but the dependency direction must stay
keyword-alias-tests → rest-tests.

- [ ] **Step 2: Run to verify failure**

Run `(fiveam:run! 'graph-db/test::rest-suite)` (same SBCL invocation shape
as Task 1 Step 2). Expected: both new tests FAIL —
`%rest-resolve-post-type` undefined; `%dsl-resolve-type` signals
`ambiguous-node-type-name`, not `query-param-error`.

- [ ] **Step 3: Implement**

In `rest.lisp`, change `%dsl-resolve-type`'s lookup to translate the
condition:

```lisp
  (let ((meta (handler-case
                  (lookup-node-type-by-name (%dsl-keyword name) parent
                                            :graph graph)
                (ambiguous-node-type-name (c)
                  (error 'query-param-error
                         :reason (format nil "ambiguous ~(~A~) type '~A': ~
one of ~{~A~^, ~}"
                                         parent name
                                         (mapcar
                                          #'%qualified-type-name-string
                                          (ambiguous-type-candidates
                                           c))))))))
```

(the rest of `%dsl-resolve-type` is unchanged). Add, next to it:

```lisp
(defun %rest-resolve-post-type (type-name parent)
  "TYPE-NAME (a client camelCase string) resolved to a node-type of PARENT
in *GRAPH*.  (values META ERROR-STRING), exactly one non-NIL: the POST
handlers turn ERROR-STRING into their :error JSON (GH #190)."
  (handler-case
      (let ((meta (lookup-node-type-by-name
                   (intern (json:camel-case-to-lisp type-name) :keyword)
                   parent :graph *graph*)))
        (if meta
            (values meta nil)
            (values nil (format nil "Unknown ~(~A~) type ~A"
                                parent type-name))))
    (ambiguous-node-type-name (c)
      (values nil
              (format nil "Ambiguous ~(~A~) type ~A: one of ~{~A~^, ~}"
                      parent type-name
                      (mapcar #'%qualified-type-name-string
                              (ambiguous-type-candidates c)))))))
```

Rework `rest-post-vertex` and `rest-post-edge` to use it — replace each
`(type (lookup-node-type-by-name (intern ...) ... :graph *graph*))` binding
and the `(if type ... (json:encode-json-to-string (list (cons :error
(format nil "Unknown vertex type ~A" type-name)))))` else-branch with:

```lisp
      (multiple-value-bind (type error-string)
          (%rest-resolve-post-type type-name :vertex)   ; :edge in post-edge
        (if type
            ;; ... existing success body, unchanged ...
            (json:encode-json-to-string
             (list (cons :error error-string)))))
```

The success bodies are untouched; only the binding and else-branch change.

- [ ] **Step 4: Run rest-suite, then the full suite; reconcile counts**

`rest-suite` green including the 2 new tests (6 checks). Full suite:
expected 3997 pass / 10 skip / 0 fail (Task 1's 3991 + 6). Account for any
other delta before proceeding.

- [ ] **Step 5: Commit**

```bash
git add rest.lisp tests/rest-tests.lisp
git commit -m "fix(rest): ambiguous bare type names are client errors, not wrong-class reads (#190)"
```

---

### Task 3: Documentation and issue closure

**Files:**
- Modify: `CHANGELOG.md` (Unreleased → Fixed)
- Modify: `docs/vivace-graph-v3-doc.org` (the section documenting
  type lookup / REST type parameters — find it with a search for
  `lookup-node-type-by-name` or the REST POST vertex docs, and state the new
  bare-name rule)
- Modify: `docs/superpowers/specs/2026-08-20-namespaces-design.md` §3.4
  (mark the known defect fixed by #190's change, one sentence)

- [ ] **Step 1: CHANGELOG entry**

Under `## [Unreleased]` / `### Fixed` (create the heading if absent):

```markdown
- The schema's bare (keyword) type-name lookup was package-blind: two
  same-named types in different packages silently clobbered one alias
  entry, and REST/DSL callers got whichever class was defined last. A bare
  name now resolves only when unique and signals
  `ambiguous-node-type-name` otherwise; the alias key is no longer
  written, and stale aliases in old `schema.dat` files are ignored.
  (#190)
```

- [ ] **Step 2: Manual + spec**

In `docs/vivace-graph-v3-doc.org`, at the type-lookup / REST-API text,
document: a keyword type designator resolves against registered types by
symbol-name; unique → that type, none → NIL/unknown-type error, several →
`ambiguous-node-type-name` (REST: an `:error` response naming the
package-qualified candidates). Use the package-qualified symbol where a
name is ambiguous.

In the namespaces spec §3.4, note the defect is fixed (cite #190 and this
plan's branch).

- [ ] **Step 3: Commit**

```bash
git add CHANGELOG.md docs/vivace-graph-v3-doc.org \
        docs/superpowers/specs/2026-08-20-namespaces-design.md
git commit -m "docs: bare type names resolve package-aware or signal (#190)"
```

- [ ] **Step 4: Whole-branch review, then PR**

Run the whole-branch review (capable model) over the full diff against
`experiment` — per the standing rule, per-task reviews do not substitute.
Then open a PR against `experiment` titled
"Bare type names resolve package-aware or signal (#190)", body linking
#190 and noting: unblocks one of #167's three gates; same resolution
policy as the #201 decision. **Do not push without Kevin's explicit
go-ahead — show him the full diff first (fenced ```diff block under
`## 📋 DIFF FOR REVIEW`).**

---

## Self-Review

- Spec coverage: #190's two suggested fixes were (a) qualify the alias key
  or (b) drop the alias and resolve explicitly with ambiguity reporting;
  the issue prefers (b), this plan implements (b) at the schema layer so
  every keyword caller (REST, `make-vertex` designators,
  `resolve-node-type-ids`, Prolog's `node-type-p` path) inherits it. The
  `%check-node-class-graph-unique` remark in the issue is context only
  (that guard is #196's scope, not this plan's).
- Types: `%rest-resolve-post-type` returns `(values meta-or-nil
  string-or-nil)` and both Task 2 call sites use it that way; condition
  readers are named identically in Tasks 1 and 2.
- Placeholders: none; every step carries its code or exact command.
