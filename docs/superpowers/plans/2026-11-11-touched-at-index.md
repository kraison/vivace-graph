# Touched-at Index Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A change-feed cursor goal over a claim namespace costs the claims touched after the cursor, not the namespace's history.

**Architecture:** One general ordered index per claim family on `transaction-extent-sexp`, keyed by a canonicalizer that reads the touched instant out of the stored extent; a Lisp reader over it; a Prolog generator `claim-touched-after/4`; and a clause-body rewriter that turns the feed's existing `claim/7` + `claim-touched-at/2` + `instant>/2` text into the generator followed by the `claim/7` filter.

**Tech Stack:** Common Lisp (SBCL verified; `#+ecl`/`#+ccl` arms untouched), FiveAM, the general index (index.lisp), the PAIP-style Prolog compiler's `*clause-body-rewriters*`.

**Spec:** `docs/superpowers/specs/2026-11-11-touched-at-index-design.md`

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments that point at the spec or an issue.
- No new write path: index maintenance is `apply-tx-write-to-secondary-indexes`' own (D1).
- The canonicalizer's domain is a stored extent sexp; a cursor enters the index as a synthetic open extent at the cursor (D2).
- The generator merges stores and families into one ascending sequence by touched instant; this run's derivation last (D3).
- ?family unbound skips a family the store does not declare, on every store; bound keeps `%scope-lookup`'s own-store refusal (D3).
- The rewrite fires only on the two shapes of D4, top level, adjacent; the comparison stays.
- Test names and helpers are new to `graph-db/spacetime-test` and `graph-db/rules-test` (grep before naming); prefixes `tt-`, `tta-`, `ttf-`.
- One FASL cache per checkout: run suites through `tests/run-system-tests.sh`, never two builds of one tree at once.

## Review Focus

1. Two claims touched at the same instant across a page boundary (an explicit `retract-claim :at`): a reader whose cursor is the page's last ?t loses the other. `%st-now` is monotonic per process, so engine stamps never collide; docs/rules.md must say the cursor is the last ?t of a fully consumed page and that explicit equal instants are the caller's.
2. A retracted claim re-opened by `register-node` (a `tx-update` of the extent slot): the row must move to the re-open's instant. No fixture here exercises it; the reviewer reads `apply-tx-write-to-secondary-indexes` against the re-open's save.
3. A reaped claim (GH #429) whose extent slot still holds a sexp sits in the index; `claims-touched-after` drops it by `claim-touched-at` NIL. If reaping clears the slot the release runs on that update. Either way no row escapes; the reviewer confirms which.
4. Shape 1 with ?t already bound by the `claim/7` goal, or B a variable the `claim/7` goal binds: the first fuses and filters, the second is excluded by rule. A B bound by a later goal is unbound at the generator and answers nothing, as the unfused pair would.
5. A fixpoint round whose non-recursive `claim/7` premise fuses (shape 1): the generator applies `%exclude-producers` and unions `%derived-all`, so the round reads what the premise would have. The reviewer compares against `%claim-goal`'s bound-claim route, which applies neither.

---

### Task 1: The index and the Lisp reader

**Files:**
- Modify: `spacetime/claim.lisp` (after `extent-sexp-start-key`; inside `def-claim-classes` after the `claim-relation` index)
- Modify: `spacetime/claim-query.lisp` (after `claims-by-producer`)
- Modify: `spacetime/package.lisp` (exports)
- Create: `tests/spacetime/touched-tests.lisp`
- Modify: `graph-db.asd` (`graph-db/spacetime-test` components, after `link-sweep-tests`)

**Interfaces:**
- Produces: `(extent-sexp-touched-key sexp)` → `(day sec nsec)` | `:unbounded` | NIL; `(claim-touched-at claim)` → `local-time:timestamp` | NIL; `(claims-touched-after graph claim-class after &key limit offset)` → `(values claims more-p)`, ascending by touched instant, AFTER a timestamp or an RFC 3339 string; the index named `claim-touched-at` on every family's parent, slot `transaction-extent-sexp`.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/touched-tests.lisp`, `(in-package #:graph-db/spacetime-test)`, `(in-suite spacetime-suite)`, fixtures `with-claim-graph` and `make-ct-claim-unary` as `claim-query-tests.lisp` uses them. The tests:

```lisp
(test tt-key-of-an-extent-is-its-touched-instant
  ;; open at TS -> TS's key; closed at END -> END's key; NIL -> NIL;
  ;; unknown start and open end -> :unbounded
  (let* ((ts (graph-db.spacetime::%st-now))
         (end (local-time:timestamp+ ts 1 :sec)))
    (is (equal (graph-db.spacetime::%timestamp-key ts)
               (graph-db.spacetime::extent-sexp-touched-key
                (extent->sexp
                 (graph-db.spacetime::%open-transaction-extent ts)))))
    (is (equal (graph-db.spacetime::%timestamp-key end)
               (graph-db.spacetime::extent-sexp-touched-key
                (extent->sexp (make-interval (exact-bound ts)
                                             (exact-bound end)
                                             :semantics :transaction
                                             :standing :asserted)))))
    (is (null (graph-db.spacetime::extent-sexp-touched-key nil)))
    (is (eq :unbounded
            (graph-db.spacetime::extent-sexp-touched-key
             (extent->sexp (make-interval (unknown-bound) (unknown-bound)
                                          :semantics :transaction
                                          :standing :asserted)))))))

(test tt-every-claim-sits-in-the-index-in-touched-order
  ;; three claims k1 k2 k3; INDEX-RANGE on the parent's extent slot
  ;; answers them in creation order; CLAIM-TOUCHED-AT of each is a
  ;; timestamp equal to its CLAIM-RECORDED-AT
  ...)

(test tt-a-retraction-moves-the-row
  ;; retract k1 at (now + 1 s): INDEX-RANGE order is k2 k3 k1;
  ;; (claims-touched-after g 'ct-claim (claim-touched-at k2)) is (k3 k1)
  ...)

(test tt-after-is-strict-and-accepts-a-string
  ;; cursor = k2's touched instant excludes k2; the same instant as a
  ;; 30-character RFC 3339 string answers the same list
  ...)

(test tt-the-open-transactions-own-write-is-seen
  ;; inside WITH-TRANSACTION, make k4; CLAIMS-TOUCHED-AFTER from k3's
  ;; instant answers (k4) before the commit
  ...)

(test tt-limit-and-offset-page
  ;; :limit 1 -> one claim and T; :limit 1 :offset 2 -> one and NIL
  ...)

(test tt-an-unknown-family-signals
  (with-claim-graph (g)
    (signals graph-db.spacetime:unknown-claim-family
      (graph-db.spacetime:claims-touched-after g 'no-such-family
                                               (local-time:now)))))
```

Register the file in `graph-db.asd` after `link-sweep-tests`.

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-system-tests.sh graph-db/spacetime-test 2>&1 | tail -15`
Expected: the `tt-` tests fail or error on undefined `extent-sexp-touched-key`, `claim-touched-at`, `claims-touched-after`; every other test passes.

- [ ] **Step 3: Implement**

`spacetime/claim.lisp`:

```lisp
(defun extent-sexp-touched-key (sexp)
  "The instant SEXP's transaction record last changed, as %TIMESTAMP-KEY's
fixnums: the end's latest edge when bounded, else the start's earliest;
:UNBOUNDED when neither is a timestamp; NIL for NIL.  The :CANONICALIZE
behind the CLAIM-TOUCHED-AT index (GH #392, design D1)."
  ...)
```

In `def-claim-classes`, after the `claim-relation` index:

```lisp
(graph-db:def-index ,parent (transaction-extent-sexp) ,graph-name
    :name claim-touched-at :canonicalize extent-sexp-touched-key)
```

`spacetime/claim-query.lisp`: `(claim-touched-at claim)` by D2's rule, NIL for a reaped claim; `(claims-touched-after graph claim-class after &key limit offset)`: `(claim-family claim-class)` for the signal, AFTER parsed with `local-time:parse-timestring` when a string, `:start` = `(extent->sexp (%open-transaction-extent after))` into `index-range` on `(claim-family-parent family)` and `'transaction-extent-sexp`, then keep the claims whose `claim-touched-at` is a timestamp `timestamp>` AFTER, then `%paginate`. Export `claim-touched-at` and `claims-touched-after` beside `claims-by-producer`.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `tests/run-system-tests.sh graph-db/spacetime-test 2>&1 | tail -8`
Expected: `Did N checks. Fail: 0`, N the previous count plus the new checks.

- [ ] **Step 5: Commit**

```bash
git add spacetime/claim.lisp spacetime/claim-query.lisp spacetime/package.lisp \
        tests/spacetime/touched-tests.lisp graph-db.asd
git commit -m "spacetime: the touched-at index and claims-touched-after (#392)"
```

---

### Task 2: `claim-touched-after/4`, standing, explain, the rule-body refusals

**Files:**
- Modify: `rules/facts.lisp` (after `claim-touched-at/2`; `claim-touched-at/2` itself; the explain registration block)
- Modify: `rules/standing.lisp` (beside `("claim-after/9" . :withheld)`)
- Modify: `rules/compile.lisp` (beside `%mentions-claim-after-p` and its `%refuse`)
- Create: `tests/rules/touched-tests.lisp`
- Modify: `graph-db.asd` (`graph-db/rules-test` components, after `explain-tests`)

**Interfaces:**
- Consumes: Task 1's `claim-touched-at`, `claims-touched-after`.
- Produces: the functor `claim-touched-after/4 (?c ?t ?after ?family)`; `%touched-candidates (after family)` → the merged candidate list (claims, no derivation) for the stores in scope; the strings "that claim", "touched-at index of ~A from the cursor", "touched-at index of every family from the cursor"; the two `compile-rule` refusals.

- [ ] **Step 1: Write the failing tests**

`tests/rules/touched-tests.lisp`, `(in-package #:graph-db/rules-test)`, `(in-suite rules-suite)`. `*cursor*` as facts-tests.lisp declares it, read through `(lisp ?cur *cursor*)`. Retractions stamped `:at (local-time:timestamp+ *cursor* N :sec)` so the order is fixed.

```lisp
(test tta-filters-a-bound-claim
  ;; seed; (claim ?c rt-claim "host" "h1" ?r ?ons ?o)
  ;;       (claim-touched-after ?c ?t "2026-09-01" rt-claim) -> 2 rows,
  ;; each ?t a 30-character string; with "2999-01-01" -> none
  ...)

(test tta-generates-a-familys-claims-after-the-cursor-in-order
  ;; seed-keys 5; *cursor* = %st-now; retract k03 at +1 s, k01 at +2 s;
  ;; (claim-touched-after ?c ?t ?cur rt-claim) after (lisp ?cur *cursor*)
  ;; -> keys (k03 k01) in that order, ?t ascending
  ...)

(test tta-every-family-skips-what-the-store-lacks
  ;; seed and seed-temporal; cursor before both; ?family NIL answers
  ;; rt-claim and rtt-claim rows and signals nothing although rtn-claim
  ;; (declared under :graph-db-rules-norule only) is registered
  ...)

(test tta-a-bound-family-the-store-lacks-signals
  (with-rules-graph (g)
    (signals graph-db:query-precondition-error
      (select-flat (?c) (claim-touched-after ?c ?t "2026-01-01"
                                             rtn-claim)))))

(test tta-an-unbound-cursor-answers-nothing
  ;; (claim-touched-after ?c ?t ?x rt-claim) -> no rows, no error
  ...)

(test tta-excluded-producers-are-not-generated
  ;; bind graph-db::*claim-exclude-producers* to one (producer . relation)
  ;; pair of SEED's claims; the generator omits exactly those
  ...)

(test tta-two-stores-merge-into-one-ascending-sequence
  ;; with-two-stores (a b): seed a, seed-b b, retract one of a's past
  ;; both; *claim-scope* (list a b); rows from both stores and the ?t
  ;; column sorted by string< (one spelling)
  ...)

(test tta-standing-class-is-withheld
  (is (eq :withheld
          (cdr (assoc "claim-touched-after/4"
                      graph-db.rules::*standing-query-functor-classes*
                      :test #'string=)))))

(test tta-explain-names-the-touched-route
  ;; ter-path (explain-tests.lisp): unbound ?c, family rt-claim ->
  ;; "touched-at index of RT-CLAIM from the cursor"; family nil ->
  ;; "every family"; after a claim/7 goal binding ?c -> "that claim"
  ...)

(test tta-compile-rule-refuses-the-hand-written-functor
  ;; as compile-tests.lisp's claim-after-is-not-a-rule-premise, with
  ;; "(claim-touched-after ?c ?t \"2020-01-01\" rt-claim)" in the body
  ...)

(test tta-compile-rule-refuses-a-pair-no-premise-binds
  ;; body "(claim-touched-at ?c ?t) (instant> ?t \"2020-01-01\")" alone
  ;; is refused with a message containing "bind ?c with claim/7 first";
  ;; the same pair after "(claim ?c rt-claim \"host\" ?k \"runs\" ?ons ?o)"
  ;; compiles
  ...)
```

Register the file in `graph-db.asd` after `explain-tests`.

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-system-tests.sh graph-db/rules-test 2>&1 | tail -15`
Expected: the `tta-` tests fail (unknown functor, missing standing entry, no refusal); every other test passes.

- [ ] **Step 3: Implement**

`rules/facts.lisp`:

- `claim-touched-at/2` yields `(let ((at (graph-db.spacetime:claim-touched-at c))) (and at (%instant-string at)))`: one definition of the instant.
- `(defun %touched-candidates (after family) ...)`: FAMILY a `claim-family` struct or NIL. For each store in `%scope-graphs`, for FAMILY or every value of `graph-db.spacetime::*claim-families*`, `claims-touched-after` from AFTER; with FAMILY bound the own store's `query-precondition-error` propagates and a foreign store's is swallowed (as `%scope-lookup`); with FAMILY NIL every store's is swallowed. Merge the lists by `(claim-touched-at c)` with `timestamp<`, stable, scope order then family order on ties; `%exclude-producers` the result.
- The functor:

```lisp
(def-global-prolog-functor claim-touched-after/4 (?c ?t ?after ?family cont)
  "..."  ; D3, in docs/rules.md's words
  (let ((c (%claim-arg ?c))
        (after (%instant-arg ?after)))
    (cond (c ...filter...)
          ((and (%unbound-p ?c) after)
           (let ((family (let ((f (%prolog-index-bound ?family)))
                           (and f (%family-or-ill-typed ?family)))))
             (dolist (claim (append (%touched-candidates after family)
                                    (remove-if-not
                                     (lambda (d) ...touched after AFTER...)
                                     (%derived-all))))
               (%yield (?c claim)
                 (%yield (?t (%instant-string
                              (graph-db.spacetime:claim-touched-at claim)))
                   (funcall cont)))))))))
```

- `%explain-touched-path (bound args graph)` and `(register-access-path 'claim-touched-after/4 #'%explain-touched-path)`: "that claim" when `(nth 0 bound)`, else the family string from `(nth 3 args)` (a symbol → `"touched-at index of ~A from the cursor"` with the symbol's name; NIL or a variable → "every family").

`rules/standing.lisp`: `("claim-touched-after/4" . :withheld)` beside claim-after's.

`rules/compile.lisp`: `%mentions-touched-after-p` as `%mentions-claim-after-p` with `"CLAIM-TOUCHED-AFTER" 4`, refused with `"claim-touched-after/4 is not a rule premise: write claim-touched-at and the comparison, the compiler fuses them (#392)"`; `%touched-at-without-premise-p (goals)`: a top-level `claim-touched-at/2` goal whose first argument is a variable that is the first argument of no `%claim-read-goal-p` goal in GOALS, refused with `"claim-touched-at reads a premise's claim: bind ?c with claim/7 first (#392)"`. Both checks beside the existing one, on the same `goals`.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `tests/run-system-tests.sh graph-db/rules-test 2>&1 | tail -8`
Expected: `Did N checks. Fail: 0`.

- [ ] **Step 5: Commit**

```bash
git add rules/facts.lisp rules/standing.lisp rules/compile.lisp \
        tests/rules/touched-tests.lisp graph-db.asd
git commit -m "rules: claim-touched-after/4, its explain row and the rule-body refusals (#392)"
```

---

### Task 3: The rewrite

**Files:**
- Modify: `rules/facts.lisp` (after `%fuse-key-bounds`' `pushnew`)
- Modify: `prologc.lisp` (`*clause-body-rewriters*` docstring)
- Modify: `tests/rules/touched-tests.lisp` (append)

**Interfaces:**
- Consumes: Task 2's functor; `%engine-functor-goal-p`, `%named-variable-p` (facts.lisp).
- Produces: `%fuse-touched-cursor (goals)` → goals, on `*clause-body-rewriters*`.

- [ ] **Step 1: Write the failing tests**

```lisp
(defparameter *ttf-feed*
  "(claim ?c rt-claim \"host\" ?k \"up\" ?ons ?o) (claim-touched-at ?c ?t) ~
(instant> ?t ~S)"
  "The claim-first feed text, the cursor spliced in.")

(test ttf-the-claim-first-feed-pays-no-key-lookups
  ;; seed-keys 50; cursor = %st-now; retract k10 and k20 at +1 s, +2 s;
  ;; counting-lookups around the select of *TTF-FEED* with the cursor's
  ;; string -> rows (k10 k20), lookups 0.  The unfused spelling -- the
  ;; same goals with (lisp ?cur *cursor*) between the pair -- answers
  ;; the same set and pays one lookup per key (>= 50).
  ...)

(test ttf-the-pair-first-feed-pays-no-key-lookups
  ;; "(claim-touched-at ?c ?t) (instant> ?t <cursor>)
  ;;  (claim ?c rt-claim \"host\" ?k \"up\" ?ons ?o)" -> same rows, 0
  ...)

(test ttf-a-goal-between-leaves-the-pair-a-filter
  ;; (claim ?c rt-claim "host" "k01" ?r ?ons ?o) (claim-current ?c)
  ;; (claim-touched-at ?c ?t) (instant> ?t "2020-01-01") -> 1 row and
  ;; exactly 1 lookup (the subject index)
  ...)

(test ttf-fused-rows-come-in-touched-order
  ;; retract k30 at +1 s then k05 at +2 s: the fused feed answers
  ;; (k30 k05); the ?t column is ascending
  ...)

(test ttf-the-guarded-feed-is-fused-interpreted-and-compiled
  ;; clear-query-cache; run-guarded-prolog *TTF-FEED* twice under
  ;; counting-lookups: both calls 0 lookups and the same rows;
  ;; (getf (query-cache-stats) :compiled) >= 1 after the second
  ...)

(test ttf-a-rule-with-the-feed-shape-derives-as-before
  ;; compile-rule accepts head (claim ?d rt-claim "host" ?k "touched"
  ;; ...) over body (claim ?c rt-claim "host" ?k "runs" ?ons ?o)
  ;; (claim-touched-at ?c ?t) (instant> ?t "2020-01-01"); run-rule on
  ;; SEED derives one claim per "runs" claim, as forward-tests.lisp
  ;; runs a rule
  ...)
```

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-system-tests.sh graph-db/rules-test 2>&1 | tail -15`
Expected: the two "pays no key lookups" tests fail on the lookup count (one per key), the order test on the order; the rest pass or fail on count only.

- [ ] **Step 3: Implement `%fuse-touched-cursor`**

In `rules/facts.lisp`, after `%fuse-key-bounds`:

```lisp
(defun %fuse-touched-cursor (goals)
  "GOALS with each (claim-touched-at ?c ?t) (instant> ?t B) pair fused
into CLAIM-TOUCHED-AFTER/4 by design D4's two shapes; the comparison
stays.  Returns GOALS itself when nothing fuses (#392)."
  ...)

(pushnew '%fuse-touched-cursor *clause-body-rewriters*)
```

Walk the top level with the goals already emitted (reversed) at hand: at a `claim-touched-at/2` goal with named ?c and ?t whose next goal is `instant>/2` on ?t with B a string or a variable, (1) if the last emitted goal is a `claim/7` whose first argument is ?c, ?c occurs nowhere in the goals before it (a small variable-occurrence helper, any depth) and B is not a variable of that goal: pop it and emit `(claim-touched-after ?c ?t B FAMILY)`, the claim goal, the comparison; (2) else if ?c occurs in no emitted goal: emit `(claim-touched-after ?c ?t B nil)` and the comparison; (3) else emit the pair unchanged. Symbols interned in `(symbol-package 'claim/7)` as `%fuse-key-bounds` does.

`prologc.lisp`: the docstring's "same solutions, same order" becomes "the same solutions; the order only where the rewriter's own documentation says so (#392)".

- [ ] **Step 4: Run the tests to verify they pass**

Run: `tests/run-system-tests.sh graph-db/rules-test graph-db/query-test 2>&1 | tail -12`
Expected: both suites `Fail: 0`.

- [ ] **Step 5: Commit**

```bash
git add rules/facts.lisp prologc.lisp tests/rules/touched-tests.lisp
git commit -m "rules: the feed's text fuses into claim-touched-after/4 (#392)"
```

---

### Task 4: Docs and the after rows

**Files:**
- Modify: `docs/rules.md` (functor block and table; "Routes, and what refuses"; the change-feed bullet; "Solution order")
- Modify: `docs/general-index-design.md` (§4 gains the built index; §12 loses the paragraph)
- Modify: `docs/codebase-notes.md` (the claim-reading bullet)
- Modify: `CHANGELOG.md` (Unreleased, beside the #376 entry)
- Modify: `docs/perf-baselines.md` (the unit 7 after entry)
- Create: `tests/perf/results/perf-after-u7-odm-<head>.report`

- [ ] **Step 1: docs/rules.md**

Add `(claim-touched-after ?c ?t after family)` to the functor block and a table row: "`claim/7`'s claims touched after `after`, from the touched-at index in ascending touched order; the compiler writes it for the feed's text (#392)". In "Routes, and what refuses", a passage "The touched-at route (#392)": the pair after a `claim/7` goal whose claim is fresh, or on its own, fuses; the generator runs first and `claim/7` filters; what does not fuse (the `claim-after/9` list, plus `instant>=`); the cursor protocol (last ?t of a fully consumed page; explicit equal instants are the caller's). Replace the change-feed bullet's last sentence ("Both goals still visit ... is #392.") with the index. "Solution order": the fused feed answers in ascending touched instant across the stores in scope, this run's derivation last.

- [ ] **Step 2: The other docs**

`docs/general-index-design.md`: move the §12 paragraph to §4 as "Built (GH #392): ...", naming the canonicalizer. `docs/codebase-notes.md`: one sentence on the claim bullet: a change feed is the three-goal text and costs what changed. `CHANGELOG.md`: an Added entry under Unreleased beside the #376 entry, naming the index, the reader, the functor, the rewrite and the measured change.

- [ ] **Step 3: The after rows**

On odm, main checkout at the branch's head (or the worktree with `CL_SOURCE_REGISTRY` pinned), three runs:

```bash
for i in 1 2 3; do sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:quickload :graph-db/perf-test :silent t)' \
  --eval "(graph-db/perf-test::run-perf-only (function graph-db/perf-test::bench-change-feed-cursor) :tag \"after-u7-$i\")"; done
```

Expected: `change-feed-cursor-rows ROWS 1000` on every run and a `change-feed-cursor` time well under the before's 5.98 s. Copy the third report to `tests/perf/results/perf-after-u7-odm-<head7>.report`; add the perf-baselines entry beside the before's with all three times; post the after table on #392 under the before table.

- [ ] **Step 4: Commit**

```bash
git add docs/rules.md docs/general-index-design.md docs/codebase-notes.md \
        CHANGELOG.md docs/perf-baselines.md tests/perf/results/perf-after-u7-odm-*.report
git commit -m "docs: touched-at index measured; rules.md, index design, changelog (#392)"
```
