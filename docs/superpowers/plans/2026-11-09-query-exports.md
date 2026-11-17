# Query Exports Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `graph-db/query` exports what an embedder of the guard needs
and keeps the row-cap rule in one place: #328, #366 and #383, Unit 1 of
the Prolog Engine 5.0 scope.

**Architecture:** Three independent tasks on one branch. The cap and
probe rule (today in `query/guard.lisp` and twice in `gui/`) becomes
two functions beside `*query-default-limit*` in `query/dsl.lisp`,
exported from `graph-db`, and every copy calls them. The guard's
screen-and-read half is exported as a combinator that owns the scratch
package's lifetime, and the length cap moves into the screen so every
entry enforces it. `run-guarded-prolog` splits into its read and its
run, and the run is exported as `run-guarded-goals` for a caller that
kept `guard-query-text`'s goals.

**Tech Stack:** Common Lisp (SBCL), FiveAM, the `graph-db/query-test`
and `graph-db/gui-test` suites.

**Spec:** `docs/superpowers/specs/2026-11-09-prolog-engine-5.0-scope.md`,
"Unit 1", and the three issues, which are the requirements.

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments pointing at the GH
  issue (`~/.claude/CLAUDE.md`).
- `graph-db.query` uses only `cl`: every `graph-db` name in
  `query/guard.lisp` is written qualified, `graph-db:` once exported.
- Behaviour of every existing caller is unchanged except the one named
  change: an overlong text is refused by `run-guarded-prolog` and
  `guard-query-text`, not only by the GUI endpoint (Task 2).
- The GUI's refusal messages are searched for by `tests/gui/gui-tests.lisp`
  (`"the limit is 4096"` at lines 1092 and 1794): the length refusal
  keeps the wording `query is ~D characters; the limit is ~D`.
- Suites, one SBCL at a time in this checkout, never the full suite:
  - query: `sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/query-test :silent t)' --eval '(asdf:test-system :graph-db/query-test)'`
    (baseline 71 checks, 15 s, recorded 2026-11-09)
  - gui: the same with `:graph-db/gui-test` (record the baseline in Task 0)
  - rules: the same with `:graph-db/rules-test` (record the baseline in Task 0)
- FiveAM test names are symbols in `graph-db/query-test`: grep
  `tests/query/` before naming one.
- Docs travel with code: `docs/guarded-query.md` and `CHANGELOG.md`'s
  `## [Unreleased]` in the task that changes the behaviour.
- Public repo: domain-neutral text everywhere.

## Review Focus

1. A GUI Prolog request over 4096 characters still answers 400
   `refused-query` with the old message, now from the screen instead of
   the endpoint: `prolog-input-caps-refuse` in the gui suite pins it
   (Task 2 runs that suite).
2. The screened reader refused mid-read (unbalanced text, a bad
   number): the scratch package must still be gone. Test in Task 2.
3. `run-guarded-goals` at the ceiling: `limit` omitted gives the same
   `truncated-p` reading `run-guarded-prolog` gives. Test in Task 3
   (the comparison runs both with no limit).
4. `run-guarded-goals :format` other than `:data` or `:raw` is a type
   error before anything runs. Test in Task 3.
5. `run-query-goals`'s own inline clamp (query/dsl.lisp:325-327) is a
   third copy of the rule: Task 1 replaces it, and the grep in Task 1
   step 6 is the check that no copy survives.

---

### Task 0: Branch and baselines

**Files:** none changed.

- [ ] **Step 1:** `git checkout -b query-exports experiment` in the main
  checkout (no other session is using it; otherwise a worktree per
  `tests/README.md`).
- [ ] **Step 2:** Run the gui suite and the rules suite once each with
  the commands above; write both check counts at the top of this
  plan's PR description. Query is 71.

### Task 1: One home for the cap and probe rule (#328)

**Files:**
- Modify: `query/dsl.lisp:19-20` (after `*query-default-limit*`), `:325-327`
- Modify: `package.lisp:281`
- Modify: `query/guard.lisp:728-736`, `:780-781`
- Modify: `gui/api.lisp:593-611`, `:690`
- Modify: `gui/prolog.lisp:81`
- Modify: `tests/query/guard-tests.lisp:44-45` (the comment names the old functions)
- Modify: `docs/guarded-query.md:88-98`, `CHANGELOG.md`
- Test: `tests/query/guard-tests.lisp`

**Interfaces:**
- Produces: `graph-db:clamp-row-cap (limit) => integer` and
  `graph-db:probe-limit (cap) => integer`, both exported from
  `graph-db`. Task 3 calls both.

- [ ] **Step 1: Write the failing test** at the end of
  `tests/query/guard-tests.lisp`:

```lisp
(test the-cap-and-probe-rule-has-one-home
  "clamp-row-cap and probe-limit are the DSL's rule about
*query-default-limit*, used by the runner, the guard and the GUI (#328)."
  (let ((ceiling graph-db:*query-default-limit*))
    (is (= ceiling (graph-db:clamp-row-cap nil)))
    (is (= ceiling (graph-db:clamp-row-cap 0)))
    (is (= ceiling (graph-db:clamp-row-cap -3)))
    (is (= ceiling (graph-db:clamp-row-cap "7")))
    (is (= 7 (graph-db:clamp-row-cap 7)))
    (is (= ceiling (graph-db:clamp-row-cap (1+ ceiling))))
    (is (= 8 (graph-db:probe-limit 7)))
    (is (= ceiling (graph-db:probe-limit ceiling))))
  (let ((graph-db:*query-default-limit* 5))
    (is (= 5 (graph-db:clamp-row-cap 9)))
    (is (= 4 (graph-db:clamp-row-cap 4)))
    (is (= 5 (graph-db:probe-limit 4)))
    (is (= 5 (graph-db:probe-limit 5)))))
```

- [ ] **Step 2: Run the query suite.** Expected: the new test fails,
  `CLAMP-ROW-CAP` unbound or not external.

- [ ] **Step 3: Define the two functions** in `query/dsl.lisp` directly
  after `*query-default-limit*`'s `defvar`:

```lisp
(defun clamp-row-cap (limit)
  "LIMIT as the row cap: a positive integer, at most
*QUERY-DEFAULT-LIMIT*; anything else is the ceiling (GH #328)."
  ...)

(defun probe-limit (cap)
  "Rows to ask for so CAP of them can be shown: one past CAP tells
truncated from exactly full; at the ceiling there is no room, and an
exactly-full page reads as truncated (GH #278)."
  ...)
```

  The bodies are today's `%clamp-cap` and `%probe` in `query/guard.lisp`.
  In `run-query-goals` replace the `cap` binding (`:325-327`) with
  `(cap (clamp-row-cap limit))`. Add `#:clamp-row-cap #:probe-limit`
  to `package.lisp:281` beside `#:*query-default-limit*`.

- [ ] **Step 4: Point every copy at them.** Delete `%clamp-cap` and
  `%probe` from `query/guard.lisp`; `run-guarded-prolog`'s bindings
  become `(cap (graph-db:clamp-row-cap limit))` and
  `(probe (graph-db:probe-limit cap))`. Delete `%clamp-row-cap` and
  `%query-probe-limit` from `gui/api.lisp`; `%query-row-cap`'s body is
  `(graph-db:clamp-row-cap (cdr (assoc :limit dsl)))` and the `probe`
  binding at `:690` is `(graph-db:probe-limit cap)`.
  `gui/prolog.lisp:81` becomes `(graph-db:clamp-row-cap limit)`. In
  `tests/query/guard-tests.lisp:44-45` the comment's `%CLAMP-CAP` and
  `%PROBE` become `CLAMP-ROW-CAP` and `PROBE-LIMIT`.

- [ ] **Step 5: Run the query suite and the gui suite.** Expected: query
  72 checks, all pass; gui at its baseline, all pass.

- [ ] **Step 6: Verify no copy survives.**
  Run: `grep -rn "%clamp-cap\|%probe\b\|%clamp-row-cap\|%query-probe-limit\|min limit \*query-default-limit\*\|min n graph-db::\*query-default-limit\*" query/ gui/ rules/`
  Expected: no output.

- [ ] **Step 7: Docs.** `docs/guarded-query.md:97`: `(%probe)` becomes
  `(probe-limit)`, and the paragraph gains one sentence: the rule's
  home is `clamp-row-cap` and `probe-limit` in `graph-db`, which the
  runner, the guard and the GUI endpoints all call (#328). CHANGELOG
  `## [Unreleased]` → `### Changed`, one entry: **One home for the
  row-cap and probe rule** (#328): `graph-db:clamp-row-cap` and
  `graph-db:probe-limit`; the guard's and the GUI's private copies are
  gone, and `run-query-goals` uses the same function.

- [ ] **Step 8: Commit.**

```bash
git add query/dsl.lisp package.lisp query/guard.lisp gui/api.lisp gui/prolog.lisp tests/query/guard-tests.lisp docs/guarded-query.md CHANGELOG.md
git commit -m "query: one home for the row-cap and probe rule (#328)"
```

### Task 2: The screened reader, exported, and the length cap in the screen (#366)

**Files:**
- Modify: `query/guard.lisp:125-176` (`%scan-query-text`), `:620-645` (`%read-guarded-forms`), new definitions after `%read-guarded-forms`
- Modify: `query/package.lisp:16` (exports)
- Modify: `gui/prolog.lisp:66-76` (`%prolog-request-text`)
- Modify: `docs/guarded-query.md` (new section), `CHANGELOG.md`
- Test: `tests/query/guard-tests.lisp`

**Interfaces:**
- Produces, exported from `graph-db.query`:
  - `scan-query-text (text) => t`, signals `prolog-guard-error`. The
    renamed `%scan-query-text`, now also refusing a text longer than
    `*prolog-max-query-length*`.
  - `call-with-screened-forms (text function) => function's values`:
    screens `text` (the character screen, then the exponent screen),
    reads it into a fresh scratch package, screens non-finite numbers,
    calls `function` with the list of forms, and deletes the scratch
    package in an `unwind-protect`.
  - `with-screened-forms ((forms text) &body body)`: the macro over it.
- Internal, used by Task 3's unchanged `%read-guarded-forms`:
  `%read-screened-forms (text scratch) => forms`, which is
  `%read-guarded-forms` minus its final `%guard-query`.

- [ ] **Step 1: Write the failing tests** at the end of
  `tests/query/guard-tests.lisp`:

```lisp
(test the-length-cap-is-enforced-by-the-screen
  "*prolog-max-query-length* is the screen's own limit, so every entry
refuses an overlong text, not only the GUI endpoint (#366)."
  (with-query-graph (g)
    (let ((long (make-string (1+ graph-db.query:*prolog-max-query-length*)
                             :initial-element #\Space))
          (full (make-string graph-db.query:*prolog-max-query-length*
                             :initial-element #\Space)))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:scan-query-text long))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:run-guarded-prolog long g))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:guard-query-text long g))
      (is (eq t (graph-db.query:scan-query-text full))))))

(test with-screened-forms-reads-into-a-scratch-package-that-leaves-with-it
  (let ((packages (length (list-all-packages)))
        (home nil))
    (graph-db.query:with-screened-forms (forms "(sr-probe-head ?x \"s\" 3)")
      (is (= 1 (length forms)))
      (destructuring-bind (head var str num) (first forms)
        (setf home (package-name (symbol-package head)))
        (is (string= "SR-PROBE-HEAD" (symbol-name head)))
        (is (string= "?X" (symbol-name var)))
        (is (string= "s" str))
        (is (= 3 num))
        (is (eq (symbol-package head) (symbol-package var)))
        (is (not (member (symbol-package head)
                         (list (find-package :graph-db)
                               (find-package :graph-db.query)
                               (find-package :keyword)
                               *package*))))))
    (is (null (find-package home)))
    (is (= packages (length (list-all-packages))))
    (is (null (find-symbol "SR-PROBE-HEAD" :graph-db)))
    (is (null (find-symbol "SR-PROBE-HEAD" :graph-db.query)))))

(test the-screened-reader-refuses-what-the-guard-refuses
  "Each refusal is the guard's own condition, and a refusal mid-read
still deletes the scratch package."
  (let ((packages (length (list-all-packages))))
    (dolist (text '("graph-db::vertex" "(a #.(+ 1 2))" "(a `b)" "(a"
                    "1e999999"))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:call-with-screened-forms text #'identity)))
    (is (= packages (length (list-all-packages))))))
```

- [ ] **Step 2: Run the query suite.** Expected: the three new tests
  fail on unexported or unbound names.

- [ ] **Step 3: The screen.** Rename `%scan-query-text` to
  `scan-query-text` (two call sites: `%read-guarded-forms`, and the
  docstring reference in `query/guard.lisp`'s header if any). As its
  first check, before the character loop:

```lisp
(when (> (length text) *prolog-max-query-length*)
  (%refuse "query is ~D characters; the limit is ~D"
           (length text) *prolog-max-query-length*))
```

  Its docstring gains the length cap in one line.

- [ ] **Step 4: The combinator.** Split `%read-guarded-forms` into
  `%read-screened-forms (text scratch)`, everything up to and including
  `(%screen-non-finite forms)` returning `forms`, and
  `%read-guarded-forms`, now `(%guard-query (%read-screened-forms text
  scratch) ctx)`. Then, after them:

```lisp
(defun call-with-screened-forms (text function)
  "FUNCTION called with TEXT's forms, screened and read as the guard
reads them (GH #366): the screen refuses package markers, every #
macro, backquote and comma, unbalanced or over-deep parentheses, an
unterminated string, an absurd exponent and a text longer than
*PROLOG-MAX-QUERY-LENGTH*; the read interns into a scratch package
that uses nothing and is deleted when FUNCTION returns or unwinds, so
nothing lands in a caller's package and a symbol is to be compared by
SYMBOL-NAME.  Refusals signal PROLOG-GUARD-ERROR.  Returns FUNCTION's
values."
  ...)

(defmacro with-screened-forms ((forms text) &body body)
  "BODY with FORMS bound to TEXT's screened forms; see
CALL-WITH-SCREENED-FORMS."
  `(call-with-screened-forms ,text (lambda (,forms) ,@body)))
```

  The body: `%make-scratch-package`, `unwind-protect` around
  `(funcall function (%read-screened-forms text scratch))`, cleanup
  `(delete-package scratch)`. Export `#:scan-query-text
  #:call-with-screened-forms #:with-screened-forms` from
  `query/package.lisp` under a comment `;; the screened reader, for
  embedders (GH #366)`.

- [ ] **Step 5: The GUI's duplicate.** In `gui/prolog.lisp`'s
  `%prolog-request-text` delete the `(when (> (length text) ...))`
  refusal; keep the `stringp` check. The docstring of
  `%prolog-request-text` loses nothing, the screen refuses with the
  same words.

- [ ] **Step 6: Run the query suite and the gui suite.** Expected:
  query 75 checks, all pass; gui at its baseline, all pass, including
  `prolog-input-caps-refuse`.

- [ ] **Step 7: Docs.** `docs/guarded-query.md`: a new section
  `## The screened reader (GH #366)` after `## guard-query-text`, with
  the three signatures, what is refused (copy the list from the
  docstring), the scratch package's lifetime, that symbols are
  compared by name, and that the length cap is now the screen's, so
  `run-guarded-prolog` and `guard-query-text` enforce it too. Where the
  doc's `run-guarded-prolog` section lists the screen's steps (around
  line 36), add the length cap. CHANGELOG `### Added`: **The screened
  reader is exported** (#366): `scan-query-text`,
  `call-with-screened-forms`, `with-screened-forms`. CHANGELOG
  `### Changed`: **`*prolog-max-query-length*` is enforced by the
  screen** (#366): `run-guarded-prolog` and `guard-query-text` refuse
  an overlong text; it was the GUI endpoint's check alone.

- [ ] **Step 8: Commit.**

```bash
git add query/guard.lisp query/package.lisp gui/prolog.lisp tests/query/guard-tests.lisp docs/guarded-query.md CHANGELOG.md
git commit -m "query: export the screened reader; the length cap is the screen's (#366)"
```

### Task 3: `run-guarded-goals`, the pre-guarded runner (#383)

**Files:**
- Modify: `query/guard.lisp:743-801` (`%run-guarded-goals`, `run-guarded-prolog`)
- Modify: `query/package.lisp` (export)
- Modify: `docs/guarded-query.md:24-27` and a new section, `CHANGELOG.md`
- Test: `tests/query/guard-tests.lisp`

**Interfaces:**
- Consumes: `graph-db:clamp-row-cap`, `graph-db:probe-limit` (Task 1).
- Produces, exported from `graph-db.query`:
  `run-guarded-goals (vars goals graph &key limit offset max-inferences
  timeout (format :data)) => (values columns rows truncated-p)`, taking
  what `guard-query-text` returned. `run-guarded-prolog` becomes read
  then `run-guarded-goals`.
- Internal rename: `%run-guarded-goals` becomes `%guarded-rows`, its
  signature unchanged `(vars goals graph probe &optional skip)`.

- [ ] **Step 1: Write the failing test** at the end of
  `tests/query/guard-tests.lisp`:

```lisp
(test run-guarded-goals-runs-kept-goals-under-the-same-rails
  "A caller that kept guard-query-text's goals runs them with
run-guarded-prolog's clamp, probe, paging, budgets and shaping (#383)."
  (with-query-graph (g)
    (seed g)
    (let ((text "(is-a ?i qt-item) (node-slot-value ?i label ?l)"))
      (multiple-value-bind (vars goals)
          (graph-db.query:guard-query-text text g)
        (multiple-value-bind (columns rows truncated)
            (graph-db.query:run-guarded-goals vars goals g)
          (multiple-value-bind (columns* rows* truncated*)
              (graph-db.query:run-guarded-prolog text g)
            (is (equal columns columns*))
            (is (equal (sort (copy-list rows) #'string< :key #'second)
                       (sort (copy-list rows*) #'string< :key #'second)))
            (is (eq truncated truncated*))))
        (multiple-value-bind (columns rows truncated)
            (graph-db.query:run-guarded-goals vars goals g :limit 2)
          (declare (ignore columns))
          (is (= 2 (length rows)))
          (is (eq t truncated)))
        (multiple-value-bind (columns rows truncated)
            (graph-db.query:run-guarded-goals vars goals g
                                              :limit 2 :offset 2)
          (declare (ignore columns))
          (is (= 1 (length rows)))
          (is (null truncated)))
        (is (graph-db::node-p
             (first (first (nth-value 1 (graph-db.query:run-guarded-goals
                                         vars goals g :format :raw))))))
        (signals type-error
          (graph-db.query:run-guarded-goals vars goals g :format :json))
        ;; The kept goals are re-runnable: a later write is seen.
        (with-transaction ((graph-db::transaction-manager g))
          (graph-db/query-test.schema::make-qt-item
           :graph g :label "d" :rank 4))
        (is (= 4 (length (nth-value 1 (graph-db.query:run-guarded-goals
                                       vars goals g)))))))))
```

  Then add, inside the same test after the `type-error` check, the
  budget rail:

```lisp
        (signals graph-db:prolog-resource-error
          (graph-db.query:run-guarded-goals
           vars (nth-value 1 (graph-db.query:guard-query-text
                              "(is-a ?i qt-item) (is-a ?j qt-item)
                               (is-a ?k qt-item)" g))
           g :max-inferences 2))
```

  (`vars` there is wrong on purpose only in name: bind the second
  `guard-query-text`'s own `vars` and `goals` with a nested
  `multiple-value-bind` instead of reusing the outer ones.)

- [ ] **Step 2: Run the query suite.** Expected: the new test fails,
  `RUN-GUARDED-GOALS` not external.

- [ ] **Step 3: Extract the runner.** Rename `%run-guarded-goals` to
  `%guarded-rows`. Define, between it and `run-guarded-prolog`:

```lisp
(defun run-guarded-goals (vars goals graph &key limit offset
                                                max-inferences timeout
                                                (format :data))
  "Run already-guarded GOALS collecting VARS, as GUARD-QUERY-TEXT
returned them, under RUN-GUARDED-PROLOG's rails: (VALUES COLUMNS ROWS
TRUNCATED-P), the same contract, keyword for keyword.  For a caller
that guards once and runs many times (GH #383)."
  ...)
```

  The body is today's `run-guarded-prolog` from `check-type` through
  the `values` form, minus the scratch package and the read: the
  `cap`/`probe`/`skip` bindings (through Task 1's functions), the two
  budget rebindings, `%guarded-rows`, the truncation reading and the
  shaping. `run-guarded-prolog` keeps its docstring and becomes: make
  the scratch, `unwind-protect` around `(multiple-value-bind (vars
  goals) (%read-guarded-forms text scratch (%guard-context graph
  scratch)) (run-guarded-goals vars goals graph :limit limit :offset
  offset :max-inferences max-inferences :timeout timeout :format
  format))`, cleanup `delete-package`. Export `#:run-guarded-goals`
  beside `#:guard-query-text` in `query/package.lisp`.

- [ ] **Step 4: Run the query suite and the rules suite.** Expected:
  query 76 checks, all pass (the existing `run-guarded-prolog` tests
  are the regression check for the split); rules at its baseline.

- [ ] **Step 5: Docs.** `docs/guarded-query.md:24-27`: the sentence
  that lists `run-query-goals` among `graph-db`'s exports is wrong, it
  is internal; rewrite to: the DSL's exports (`compile-pattern-query`,
  `run-pattern-query`, `def-query`) stay in `graph-db`; the runner
  behind them, `run-query-goals`, is internal, and the exported way to
  run kept goals is `run-guarded-goals` below. New section
  `## run-guarded-goals (GH #383)` after `## guard-query-text`: the
  signature, that it is `run-guarded-prolog` minus the read, that
  `guard-query-text` plus `run-guarded-goals` equals
  `run-guarded-prolog`, and that the goals may be run any number of
  times. In `docs/vivace-graph-v3-doc.org`, find `guard-query-text`;
  where the manual describes it, add one sentence naming
  `run-guarded-goals` as its runner. CHANGELOG `### Added`:
  **`run-guarded-goals`** (#383): `run-guarded-prolog`'s run half,
  exported, for a caller that kept `guard-query-text`'s goals; the doc
  no longer calls `run-query-goals` exported.

- [ ] **Step 6: Commit.**

```bash
git add query/guard.lisp query/package.lisp tests/query/guard-tests.lisp docs/guarded-query.md docs/vivace-graph-v3-doc.org CHANGELOG.md
git commit -m "query: export run-guarded-goals, the pre-guarded runner (#383)"
```

### Landing

- Whole-branch review, then `gh pr create --base experiment`.
- After the merge, read the post-merge run (`gh run list --branch
  experiment`); when it is green, close #328, #366 and #383 each with
  a comment naming the landing SHA and what met the acceptance
  (CLAUDE.md: `Closes #N` does not fire on `experiment`).
