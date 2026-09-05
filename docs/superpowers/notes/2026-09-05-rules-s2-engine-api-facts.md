# Engine API facts, verified for rules S2 (#331)

A snapshot, not a maintained document. The recon pass for S2's plan
(`docs/superpowers/plans/2026-09-05-rules-s2-rule-record-and-run.md`,
Task 0): sixteen assumptions A1–A16, each confirmed or refuted from
source, every correction re-checked against a second location before it
was kept. It is committed because it is the record of why the S2 tasks
say what they say — §C is the finding → correction map the plan text was
amended from.

**Pinned to `870d2bd`** (this branch's tip, `feat/rules-s2`). Every
`file:line` is that tree and will drift; the quoted forms are what to
match on, not the numbers. Paths beginning
`~/work/cl-temporal-extent` are the temporal library, not this repo.

**Nothing here was evaluated in an image except A15**, which was settled
by one `sbcl --non-interactive` run (transcript quoted in A15).
Everything else is quoted verbatim from source, so a claim about
*runtime* behaviour is an inference from the code and is marked as such
where it matters.

Reading order:

- **§C — corrections.** Eight defects this pass found in the S2 plan
  before execution began. C1 is blocking: it refutes the plan's
  one-transaction sweep-and-rederive outright.
- **§A — the sixteen items** in order, each with the form quoted.

Line lengths here exceed the repo's 80-column rule in a few dozen places
(quoted forms and tables). Left as verified rather than rewrapped by
hand, which risks silently corrupting a quoted form. Every quoted form is
verbatim; the only edits are a `...` standing for lines dropped from the
middle of a form, and, in three long docstrings, a `...` inside the
string itself. Every code line is byte-for-byte.

---

# §C — corrections to the S2 plan

## C1 (BLOCKING) — sweep-then-rederive in ONE transaction is refused, not clean

**Plan assumes (A7, and the Architecture paragraph):** "`run-rule` sweeps
the rule's previous derivation and derives afresh in one validated
transaction", with `tests/spacetime/claim-query-tests.lisp:139-146` cited
as "the sweep-then-insert precedent".

**Reality: the cited test is the refutation, not the precedent.** It is
`sweep-then-insert-of-an-unchanged-claim-collides-within-one-transaction`
(`tests/spacetime/claim-query-tests.lisp:128-140`), and it asserts the
opposite:

```lisp
(test sweep-then-insert-of-an-unchanged-claim-collides-within-one-transaction
  "Pins #131: MARK-DELETED's release lands in
APPLY-TX-WRITES-TO-UNIQUE-INDEXES, which runs post-durability, after
VALIDATE-UNIQUE-CONSTRAINTS's pre-durability check -- both inside the same
commit.  So a sweep and a reinsert of the identical claim in ONE transaction
always collide; the split into two transactions above is required, not
stylistic (design §6.4)."
  (with-claim-graph (g)
    (with-transaction () (make-b))
    (signals graph-db:unique-constraint-violation
      (with-transaction ()
        (delete-claims-by-producer g 'ct-claim "rule-a")
        (make-b)))))
```

The test immediately above it says the same thing from the other side,
and its `with-transaction`s are split deliberately —
`regeneration-leaves-no-orphan-when-a-rule-stops-producing-a-claim`
(`claim-query-tests.lisp:104-126`), docstring: "Sweep and reinsert are
two SEPARATE transactions, not one ... a same-transaction reinsert of an
unchanged claim would never see its own sweep's release (design §6.4)".

**Verified from source, independently of the tests.**
`validate-unique-constraints` (`unique-constraint.lisp:683-716`) runs
pre-durability and consults the *committed* index:

```lisp
(defun validate-unique-constraints (tx graph)
  "Signal UNIQUE-CONSTRAINT-VIOLATION if any write in TX would duplicate
another live node's unique value/tuple, or duplicate another write in the
same transaction.  Called in %COMMIT's manager-locked region, after
VALIDATE and before durability, so a violation aborts before anything is
journaled.  ..."
  (let ((intra (make-hash-table :test 'equal)))
    (dolist (write (writes tx))
      (let ((node (node write)))
        (unless (deleted-p node)              ; a delete/mark-deleted claims nothing
```

```lisp
;; unique-constraint.lisp:661-674
(defun %check-unique-key (uix key owner-name slot-designator raw-value
                           node-id intra)
  "Signal UNIQUE-CONSTRAINT-VIOLATION if KEY is held by another node -- either
(a) an already-committed row in UIX (the index reflects it, since prior
commits' APPLY ran under this same lock), or (b) another write earlier in
this same transaction, tracked via INTRA.  ..."
  (when key
    (let ((holder (uix-lookup uix key)))
      (when (and holder (not (equalp holder node-id)))
        (error 'unique-constraint-violation
```

The `unless (deleted-p node)` skips the *deleting* write; it does not
release the deleted node's key. The release is
`apply-tx-writes-to-unique-indexes`, called from `apply-transaction`
(`transactions.lisp:1919`) — which `%commit` reaches only after
`finalize-tx-persistence` (`transactions.lisp:3462-3474`), long after
`validate-unique-constraints` (`transactions.lisp:3429`). So the ordering
is structural, not incidental.

**Refutation attempts, all failed.**

- *Does the extent-disjointness validator have the same problem?* No —
  and this half of A7 is CONFIRMED. `%validate-extent-disjointness`
  (`spacetime/temporal.lisp:69-113`) counts over **post-commit** state
  via `make-commit-view`, docstring: "a run retracted in this transaction
  does not count, one created in it does". Only `def-unique` collides.
- *Does the identity tuple differ for a re-derived claim?* Only if
  something in it changed. The tuple is `(producer subject-namespace
  subject-key relation [extent-sexp])` for unary and
  `(producer subject-namespace subject-key object-namespace object-key
  relation [extent-sexp])` for binary (`spacetime/claim.lisp:363-371,
  422-427`); the temporal component is canonicalised through
  `extent-sexp-start-key`. An **unchanged** derivation — the common case
  in a regeneration — renders an identical tuple and collides.
  `transaction-extent-sexp` is **not** in the tuple.
- *Could `retract-claim` be used instead of `mark-deleted`?* No, for the
  same reason: it closes the transaction extent, which is not in the
  identity tuple, so the old claim keeps its unique key and the
  re-assertion still collides. `temporal-tests.lisp:158-162` states it:
  "a NEW claim with the same start collides on identity
  (UNIQUE-CONSTRAINT-VIOLATION), the ordinary 'same claim twice'
  refusal."
- *Is the constraint perhaps unbuilt and therefore inert?* It warns and
  skips only when `%unique-tuple-index-for` returns NIL
  (`unique-constraint.lisp:701-708`). A store that ran
  `def-claim-classes` and opened has it built.

**Fix — pick one, then amend Task 4:**

- (a) **Two transactions**: sweep, commit, then derive and commit. What
  design §6.4 and the existing tests do. Cost: the regeneration is no
  longer atomic — a crash between them leaves the rule's claims gone
  until the next run, and `run-rule`'s "one validated transaction"
  promise in the plan's Architecture paragraph has to go.
- (b) **Diff, then sweep only what is not re-derived** (recommended).
  Derive the new claim set first, key it with
  `graph-db.spacetime:claim-identity-key`, `mark-deleted` only the
  producer's existing claims whose key is absent from that set, and
  construct only the claims whose key is new. One transaction, no
  collision, and strictly better on provenance: an unchanged claim keeps
  its node id and its version chain instead of being churned. **A6 is
  what makes this sound** — the body reads the committed store, so
  deriving before the sweep sees exactly what deriving after it would.
  Cost: `run-rule` needs a "sweep all but these" pass over
  `claims-by-producer` rather than a bare `delete-claims-by-producer`.
- (c) `retract-claim` — does not work, see above.

## C2 (BLOCKING for the Task 2 tests) — a second `def-rules-schema` rebinds `MAKE-RULE`'s default store

**Plan assumes (A4):** a second `(def-rules-schema :other)` registers
everything under `:other` "without unregistering them from `:g`".

**That much is true** — see A4 — but it is not the whole effect, and the
half the plan omits will silently misdirect a two-store test's writes.
`%install-node-type` re-installs the generated helpers on every
expansion, with the *new* store baked in (`schema.lisp:747`):

```lisp
    (%install-node-helpers name kind (node-type-graph-name meta))
```

and the constructor closes over that name (`schema.lisp:482-504`):

```lisp
(defun %make-constructor-closure (name graph-name kind)
  "The MAKE-<NAME> function for node type NAME of KIND in store
GRAPH-NAME (GH #172)."
  ...
      (lambda (&rest make-args
               &key (graph nil) id deleted-p revision
               &allow-other-keys)
        (let ((graph (%default-store-graph name graph-name graph)))
```

```lisp
;; schema.lisp:396-402
(defun %default-store-graph (class-name store-name explicit)
  "R1 resolution: EXPLICIT graph if given, else the OPEN graph named
STORE-NAME, else refuse -- never *GRAPH* (GH #167)."
  (or explicit
      (lookup-graph store-name)
      (error 'default-store-not-open-error
             :class-name class-name :store store-name)))
```

There is one `fdefinition` per class symbol, so after
`(def-rules-schema :other)` a bare `(make-rule :name "r")` writes into
`:other`, whatever `*graph*` or the enclosing `with-transaction` says.
Same for `make-derivation-unary` / `-binary`.

**Fix:** the Task 2 multi-store test must pass `:graph` explicitly on
every constructor call after the second expansion, and `docs/rules.md`
should say so. Do not rely on `with-transaction (:graph g)` to route a
constructor — it binds `*graph*`, which `%default-store-graph`
deliberately ignores.

## C3 — the guard's canonical head comes from the whitelist's home package, not a literal `GRAPH-DB`

**Plan assumes (A11):** "the guard's `%guard-goal` canonical head for
`claim` is `(intern "CLAIM" (find-package :graph-db))`".

**Reality: right answer, wrong mechanism** — and the mechanism is what a
test should assert. `%guard-goal` (`query/guard.lisp:481-486`):

```lisp
        (let* ((home (gethash (cons name arity) (gc-functors ctx)))
               (canonical (or (gethash name (gc-control ctx))
                              ;; Bounded: NAME came out of the registry
                              ;; by string match, so this interns only
                              ;; names the image already registered.
                              (and home (intern name home)))))
```

`home` is the *registered functor symbol's own package*, taken from
`%functor-whitelist` (`query/guard.lisp:305-334`), which stores
`(symbol-package key)` for each key in `*prolog-global-functors*` and
`*user-functors*`. It resolves to `GRAPH-DB` only because
`rules/facts.lisp:10` is `(in-package #:graph-db)` — S1's deviation. If
that ever moves to `graph-db.rules`, the canonical head silently becomes
`graph-db.rules::claim` and any test asserting `(intern "CLAIM"
:graph-db)` starts passing vacuously against the wrong symbol.

**Fix:** in Task 3, assert the head is `EQ` to
`(graph-db::make-functor-symbol 'claim 7)`'s base symbol, or simply to
the symbol `rules/facts.lisp` defines — not to a re-interned literal.
Note the dependency in the task text.

## C4 — rule text may not contain a colon, so no keyword literal anywhere

**Not in the plan at all.** Task 3 routes rule text through the guard.
The guard's character screen refuses `:` outright, before `READ`
(`query/guard.lisp:158-161`):

```lisp
          ((char= ch #\:)
           (%refuse "package-qualified name ~S is not permitted: a ~
query may name only this graph's schema and the registered Prolog ~
functors" (%token-around text i)))
```

So a rule body can contain no keyword — not `:present`, not a `:foo`
namespace — and no package-qualified symbol. Nothing downstream relaxes
this: `%guard-term` (`query/guard.lisp:497-504`) admits only conses
(guarded as goals), symbols, numbers, strings and characters, and
`%guard-symbol` refuses any symbol whose package is not the request's
scratch package (`query/guard.lisp:434-437`).

**This is not a defect in S1's functor surface** — `claim/7`,
`claim-standing/2`, `claim-relation/2` and `claim-producer/2` all answer
and filter on *lowercase wire strings* precisely so that guarded text can
reach them (`rules/facts.lisp:117-127, 202-208`: "?C's standing as the
lowercase wire string (\"inferred\"), the same shape a namespace answers
in"). But Task 3's refusal messages and `docs/rules.md` must say it, and
no example rule may be written with a keyword in it.

## C5 — the temporal dedupe key does not separate an instant from an interval

**Plan assumes (A12):** "the dedupe key in Task 4 can use
`extent-sexp-start-key` directly."

**True, with a trap the plan should record.**
`extent-sexp-start-key` reads only the START bound
(`spacetime/claim.lisp:135-146`):

```lisp
(defun extent-sexp-start-key (sexp)
  "The identity component a temporal family derives from a stored extent
SEXP: its START bound as ((day sec nsec) (day sec nsec)), :UNBOUNDED
where open; NIL for NIL.  ..."
  (when sexp
    (let ((start (extent-start (sexp->extent sexp))))
      (flet ((key (x) (if (eq x :unbounded) x (%timestamp-key x))))
        (list (key (bound-earliest start)) (key (bound-latest start)))))))
```

`extent-kind` never enters it. An `:instant` at T and an `:interval`
starting at T produce the identical key — which is correct for the
*identity* tuple (that is what `:canonicalize` is for) but means a Task 4
dedupe keyed on it will collapse two derivations that differ only in
kind. Either accept that (they are one claim by the family's own
identity rule, so the unique constraint would refuse the second anyway)
or key on `claim-identity-key`, which has the same property.
Say which in the task text.

## C6 — `constraint-violation`'s `define-condition` is in `globals.lisp`, not `package.lisp`

**Plan cites (A1):** `package.lisp:423`. That line is the *export*:

```lisp
;; package.lisp:423
           #:constraint-violation #:vector-dimension-violation
```

The class is `globals.lisp:465`. Substance CONFIRMED — see A1. Fix the
citation in the task text so the reader lands on the form.

## C7 — the `::` fallback A2 offers is unnecessary

**Plan assumes (A2):** "If the registry is keyed by symbol and
`canonical-relation-p` is not exported, the slot spec must write
`graph-db.spacetime::canonical-relation-p`."

It **is** exported (`spacetime/package.lisp:64`), so the single-colon
form the plan prefers is correct and the fallback branch can be struck
from the task text. See A2 for both forms.

## C8 — the 0.3.0 floor is half-landed, and the registry push is load-bearing

**Plan assumes (A15):** the `(:version :cl-temporal-extent "0.3.0")`
floor is satisfied "once the `.asd` says 0.3.0". Two facts to record.

1. **Only half of the bump has happened, and it moved during this pass.**
   Task 1 committed in the extent worktree while this recon was running
   (`3b5c75a feat(allen): extent-intersection, the constructor
   extents-intersect-p implied (#5)`), so `cl-temporal-extent.asd:8` now
   reads `:version "0.3.0"`. `graph-db.asd:559` still floors at
   `(:version :cl-temporal-extent "0.2.0")` — Task 2 must raise it, and
   until it does the floor is not being tested. The A15 image run
   predates that commit and reported `"0.2.0"`; both readings are
   recorded there, with times, because a tree another session is
   committing into is not a stable citation.
2. **The `*central-registry*` pushes are not optional.**
   `~/quicklisp/local-projects/` symlinks `cl-temporal-extent.asd` and
   `graph-db.asd` to the **main checkouts**, which other sessions are
   editing. A run that omits the pushes loads those, not the worktrees,
   and says nothing about it. The pushes win because Quicklisp
   *appends* its searchers (`~/quicklisp/quicklisp/setup.lisp:231-236`),
   leaving `sysdef-central-registry-search` first. Every command in the
   plan already has them; the point is that dropping one is silent.

## Finding → correction map

| finding | item(s) | correction | severity |
|---|---|---|---|
| sweep + reinsert of an unchanged claim in one transaction is refused by `def-unique`; the cited test is the refutation | A7 | C1 | **blocking** |
| a second `def-rules-schema` rebinds `MAKE-RULE`'s default store | A4 | C2 | blocking for Task 2's tests |
| the canonical guarded head comes from the whitelist's home package | A11 | C3 | correctness of a Task 3 assertion |
| the guard refuses `:` before `READ`, so rule text has no keywords | (none — new) | C4 | Task 3 docs, messages, examples |
| `extent-sexp-start-key` ignores `extent-kind` | A12 | C5 | Task 4 dedupe |
| `constraint-violation` is defined in `globals.lisp:465` | A1 | C6 | citation |
| `canonical-relation-p` is exported; the `::` fallback is dead | A2 | C7 | task text |
| the 0.3.0 floor is half-landed (Task 1 committed mid-pass, `graph-db.asd` not raised); the registry pushes are load-bearing | A15 | C8 | Tasks 1–2 sequencing |

Everything else — A3, A5, A6, A8, A9, A10, A13, A14 — is CONFIRMED as
written. A16 is new and answered: **nothing static exists.**

---

# §A — the sixteen items

## A1 — `constraint-violation` — CONFIRMED (citation corrected, C6)

`globals.lisp:465`:

```lisp
(define-condition constraint-violation (error) ()
  (:documentation "Superclass of every DETERMINISTIC commit refusal: the same
write against the same schema is refused again, however often it is retried.
Transient failures (conflicts, I/O) are never subclasses (GH #151)."))
```

No slots, no initargs, therefore none required.
`(define-condition rule-compile-error (graph-db:constraint-violation) (...))`
with its own slots is legal. The sibling in the same file is the
template to copy (`globals.lisp:470-479`, `vector-dimension-violation`:
four slots and a `:report`). It is exported at `package.lisp:423`.

Two things follow that the plan's P3 depends on and that are worth
stating: a `constraint-violation` is by contract **deterministic** — a
retry must be refused again — which is exactly true of a rule that does
not compile; and `call-with-transaction` retries only
`validation-conflict` (see A13), so it propagates.

## A2 — `:check` resolution — CONFIRMED (C7: the `::` fallback is dead)

The registry is an `EQ` hash keyed by symbol
(`runtime-schema.lisp:200`, `242-253`):

```lisp
(defvar *schema-functions* (make-hash-table :test 'eq)
```

```lisp
(defun register-schema-function (name fn)
  "Register FN under NAME for the :CHECK slot option.  Returns NAME.
Re-registering replaces (GH #172, R5).  FN runs inside a transaction's
OCC validation and MUST BE PURE: a conflict retries the transaction, so
FN can run several times for one logical write, and must never itself
mutate state or have other side effects (GH #172, review round 3,
M-5)."
  (check-type name symbol)
  (with-lock-held (*schema-functions-lock*)
    (setf (gethash name *schema-functions*) fn))
  name)
```

Spacetime registers at load (`spacetime/claim.lisp:78-86`):

```lisp
;; Registered at load, so the :CHECK names below resolve in any image
;; that has this file before a tenant's DEF-CLAIM-CLASSES expands.  A
;; lambda, not #'..., so a redefined predicate takes effect immediately.
(graph-db:register-schema-function
 'canonical-relation-p (lambda (v) (canonical-relation-p v)))
(graph-db:register-schema-function
 'canonical-producer-p (lambda (v) (canonical-producer-p v)))
```

`'canonical-relation-p` is read in `(in-package #:graph-db.spacetime)`,
so the key is `GRAPH-DB.SPACETIME::CANONICAL-RELATION-P`. That symbol
**is exported** (`spacetime/package.lisp:64`):

```lisp
   #:canonical-relation-p #:canonical-producer-p      ; GH #160
```

So `:check graph-db.spacetime:canonical-relation-p` written in a
`def-source` slot spec in package `graph-db.rules` reads to that same
symbol and hits the registry by `EQ`. The `::` fallback the plan offers
is unnecessary.

The engine's own use of the same idiom, for comparison
(`spacetime/claim.lisp:150-156`, spliced into `def-claim-classes`'s
expansion from the `+claim-shared-slots+` defparameter):

```lisp
    (relation :initarg :relation :accessor claim-relation
              :check canonical-relation-p)
    (producer :initarg :producer :accessor claim-producer
              :check canonical-producer-p)
```

Resolution is at **check time**, not at definition time
(`value-constraint.lisp:213`: "here, at check time, so a
re-registration takes effect immediately"), so an unregistered `:check`
name is not caught by the macro — it is caught, or not, at commit.
`runtime-schema.lisp:275` shows the name is read straight out of the
slot spec's plist: `for name = (and (consp spec) (getf (rest spec) :check))`.

## A3 — `def-source` inside another macro — CONFIRMED

**The seven facet checks run at macroexpansion of `def-source` itself.**
They are in the macro body, before the backquote
(`spacetime/source.lisp:196-211`):

```lisp
  (let ((missing (append (unless identity-p '(:identity))
                         (unless space-p '(:space))
                         (unless time-p '(:time))
                         (unless attribution-p '(:attribution))
                         (unless sensitivity-p '(:sensitivity))
                         (unless registration-p '(:registration))
                         (unless indexed-text-p '(:indexed-text)))))
    (when missing
      (error 'missing-source-facet :name name :facets missing)))
  (%check-facet :identity identity)
  (%check-facet :space space)
  ...
```

So `(def-rules-schema :g)` expanding to `(def-source rule :g …)` gets
the same refusals, at the same time, as a hand-written `def-source`.

**`NAME`'s package comes from the symbol.** `%schema-symbol-package`
(`schema.lisp:462-466`):

```lisp
(defun %schema-symbol-package (name)
  "The package NAME's generated helpers and functors are interned in.
DEF-NODE-TYPE used to intern them in the expansion-time *PACKAGE*, which
for source code is the package NAME itself was read into (GH #172)."
  (or (symbol-package name) *package*))
```

and the constructor name is computed from it at macroexpansion
(`schema.lisp:806-809`):

```lisp
         :constructor ',(intern (format nil "MAKE-~A" (symbol-name name))
                                (%schema-symbol-package name))
```

A macro template's `rule` is read once, when `rules/schema.lisp` is
compiled under `(in-package #:graph-db.rules)`, and spliced as that
symbol. So `rule` is `graph-db.rules::rule` and `make-rule` is
`graph-db.rules::make-rule`, whatever package the caller expands in.
`%install-node-helpers` (`schema.lisp:521-540`) interns `RULE-P`,
`LOOKUP-RULE` and `MAKE-RULE` in the same package.

**One caveat the plan does not need but a reader will trip on.**
`def-node-type`'s own docstring says "NAME's package comes from the
ambient *PACKAGE* at macroexpansion time, not from this form"
(`schema.lisp:766-768`). That is about the *reader*: nothing forces NAME
into GRAPH-NAME's package. It does not contradict the above. Separately,
the meta records the expansion-site package (`schema.lisp:804`,
`:package (package-name *package*)`), which is the *caller's* package,
not `graph-db.rules`. Its one consumer is `%dsl-resolve-type`
(`query/dsl.lisp:230`), where it is advisory since #322 — `run-query-goals`'s
own docstring: "PACKAGE no longer routes functor resolution".

## A4 — one class in two stores — CONFIRMED for the registries; see C2

`def-node-type`'s comment (`schema.lisp:791-793`):

```lisp
       ;; No cross-graph uniqueness check: type-ids are system-wide as of
       ;; #186, so one class may be instantiated in more than one store.
       ;; Divergent slot sets across stores warn instead (GH #196).
```

All three registries key on the graph name and touch nothing else.

`%register-node-type-meta` (`schema.lisp:600-614`):

```lisp
(defun %register-node-type-meta (meta)
  "Put META in *SCHEMA-NODE-METADATA* under its own store, replacing any
entry for the same class IN PLACE.  A class may be registered under more
than one store (#186); only this store's list is touched, and position
still governs UPDATE-SCHEMA's instantiation order (GH #53, #167)."
  (let* ((graph-name (node-type-graph-name meta))
         (metas (gethash graph-name *schema-node-metadata*))
```

`register-index-spec` (`index.lisp:150-162`):

```lisp
  (let* ((g (index-spec-graph-name spec))
         (id (index-spec-identity spec))
         (existing (gethash g *schema-index-metadata*))
         (hit (find id existing :key #'index-spec-identity :test #'equal)))
    (setf (gethash g *schema-index-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
```

`register-unique-tuple-spec` (`unique-constraint.lisp:285-296`) is the
same shape, keyed on `(unique-tuple-spec-graph-name spec)`. The
`def-source` identity's `def-unique` and `def-index`
(`spacetime/source.lisp:212-230`) and every `def-claim-classes` form
(`spacetime/claim.lisp:422-450`) take `,graph-name` literally, so
`:other` gets its own entries and `:g` keeps its own.

**`%warn-if-cross-file-clobber` is silent when both expansions are in one
file** (`schema.lisp:567-598`), for the reason the plan expects — the
guard compares against `*compile-file-truename*` / `*load-truename*` and
only fires for a *different* file whose registrations have all vanished:

```lisp
  (let ((here (or *compile-file-truename* *load-truename*)))
    (when here
      (let ((regs (gethash graph-name *schema-graph-name-registrants*))
            (metas (gethash graph-name *schema-node-metadata*)))
        (dolist (entry regs)
          (destructuring-bind (file . names) entry
            (when (and (not (equal file here))
                       names
                       (notany (lambda (n)
                                 (find n metas :key #'node-type-name))
                               names))
```

Two more reasons it stays silent here: the registrant list is *per graph
name*, so `:g` and `:other` never compare against each other; and it is a
no-op entirely at the REPL, where `here` is NIL.

`%warn-if-divergent-across-stores` (`schema.lisp:314-337`) is also
silent — identical slot lists, and its docstring says so: "Identical
slots are the multi-store feature and stay silent".

**What the plan missed:** the constructor's default store. See C2.

## A5 — `run-query-goals :format :raw` — CONFIRMED

`run-query-goals` (`query/dsl.lisp:302-349`) hands the callback straight
through to `select`:

```lisp
  (when (and (eq format :raw) (not callback))
    (error "RUN-QUERY-GOALS :FORMAT :RAW requires :CALLBACK."))
  ;; The eval'd SELECT / node-slot-value goals key off *GRAPH*.
  (let* ((*graph* graph)
         (*package* package)
         ...
         (run (lambda (cb)
                ;; the select form is EVAL'd (null lexenv), so pass the
                ;; callback through a special the form references
                ;; rather than a lexical.
                (let ((*pattern-query-callback* cb))
                  (eval `(select (:effects nil :snapshot t
                                  :limit ,cap
                                  :skip ,(when (integerp skip) skip)
                                  :max-inferences
                                  ,*query-default-max-inferences*
                                  :timeout ,*query-default-timeout*
                                  :callback *pattern-query-callback*)
                                 ,vars ,@goals))))))
    (if (eq format :raw)
        (progn (funcall run callback) nil)
        (emit-query-results vars format run))))
```

`:flat` is never set, so the callback gets the full row.
`select/2` builds it (`prolog-functors.lisp:587-626`, `graph-pkg` bound
at `prolog-functors.lisp:565` to `(find-package :graph-db)`):

```lisp
        (let ((r (loop for name in var-names
                    for var in vars
                    collect (let ((var (deref-exp var)))
                              (cond ((and (symbolp var)
                                          (eq graph-pkg (symbol-package var)))
                                     (symbol-name var))
                                    ((and (consp var)
                                          (eq (first var) name)
                                          (symbolp (second var))
                                          (eq graph-pkg
                                              (symbol-package (second var))))
                                     (list name (symbol-name (second var))))
                                    (t var))))))
          ...
            (cond (*select-callback*
                   ;; streaming: hand the row to the callback as it is produced,
                   ;; consing nothing onto *select-list*
                   (funcall *select-callback* (if *select-flat* (first r) r)))
```

One list per solution, in `vars` order. The stringification is gated on
`(eq graph-pkg (symbol-package var))`, so only `GRAPH-DB`-homed symbols
become strings: a keyword (`symbol-package` = `KEYWORD`) falls to
`(t var)` and arrives as the keyword; a node, being neither a symbol nor
a cons, arrives as the node object. All three CONFIRMED.

Worth carrying into Task 4: this also means a variable bound to a
`GRAPH-DB`-homed symbol — a class name, were one ever bound — would
arrive as a *string*. Schema class symbols are homed in the schema's own
package, so this does not bite `claim/7`'s `?family`.

## A6 — `:snapshot t` inside an open transaction — CONFIRMED (both halves)

**No second transaction, no signal.** `select` expands the `:snapshot`
option to a `call-with-read-snapshot` (`prologc.lisp:1126-1133`):

```lisp
              ,(if (cdr (assoc :snapshot options))
                   ;; Run the query under one consistent MVCC read snapshot:
                   ;; all reads resolve at a single epoch (lock-free, stable
                   ;; against concurrent writers).  Inherits an enclosing
                   ;; transaction if one is already active.
                   `(call-with-read-snapshot
                     (lambda () (funcall func #'prolog-ignore)) *graph*)
                   `(funcall func #'prolog-ignore)))
```

and `call-with-read-snapshot` short-circuits on an enclosing read-write
transaction (`transactions.lisp:3347-3355`):

```lisp
    (cond
      ((null tm) (funcall thunk))
      ;; a read-write transaction on this graph already provides a snapshot
      ((and *transaction* (%transaction-covers-graph-p *transaction* graph))
       (funcall thunk))
      ;; already snapshotted this graph -> inherit
      ((and *read-snapshots* (gethash graph *read-snapshots*)) (funcall thunk))
```

Second clause: `run-rule`'s `with-transaction` covers the graph, the
thunk runs directly, nothing is created and nothing is signalled.

**The body does not see its own sweep.** `mark-deleted` never mutates the
node it is given; it records a *copy* with the flag set
(`transactions.lisp:2914-2941`):

```lisp
    (let ((old-node node)
          ;; %COPY, not the public COPY: same dispatch (so an edge's
          ;; FROM/TO/WEIGHT survive), minus the create-set guard -- a node
          ;; created and then MARK-DELETED in this same transaction must
          ;; keep working (GH #135).
          (new-node (%copy node)))
      (setf (bytes new-node) (bytes old-node))
      (setf (deleted-p new-node) t)
      (add-to-object-set (make-instance 'tx-delete
                                        :old-node old-node
                                        :node new-node)
                         (write-set *transaction*)))
```

The write-set is not consulted on the read path. `index-lookup`
(`index.lisp:990-1011`) walks the secondary index and resolves each id
through `%node-by-id` → `lookup-vertex` → `lookup-object`, which reads
the local cache or the store and never the write set
(`transactions.lisp:319-357`):

```lisp
          (dolist (id (ix-lookup six key :prefix prefix))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
```

So the node the body reads back is the *pre-delete* version, whose
`deleted-p` is NIL, and `claim/7` still generates it. This is what
`docs/rules.md` records under "What the functors do not see"
(`docs/rules.md:207-215`):

> They read the **committed** store. `index-lookup` does not see the
> writes of a transaction it runs inside, and unlike `claims-touching`
> — which overlays the open transaction's write set (GH #324) — these
> do not compensate. Uniform across every route, so a query inside a
> `with-transaction` sees the snapshot and nothing of its own writes.
> Slice 2's `run-rule` derives claims inside the transaction it also
> reads in, so it is the first caller this bites (GH #331).

**Consequences to carry into Tasks 3 and 4.** The plan's reading — "the
only claims a body could re-read after its own sweep are its own", hence
a strict cycle check — is right. Note also the contrast the doc draws:
`claims-touching` *does* overlay the write set
(`tests/spacetime/claim-query-tests.lisp:226-257`,
`a-transaction-reads-its-own-retraction-and-assertion`), so a `run-rule`
that reaches for a spacetime query function instead of a functor gets
*different* visibility. And this is the fact that makes C1's fix (b)
sound: deriving before the sweep sees exactly what deriving after it
would.

## A7 — sweep-then-insert in one transaction — **REFUTED**, see C1

The half of A7 about `%validate-extent-disjointness` is CONFIRMED
(`spacetime/temporal.lisp:69-91`):

```lisp
(defun %validate-extent-disjointness (tx graph)
  "The commit validator (GRAPH-DB:*COMMIT-VALIDATORS*).  Every written
live claim of a temporal family must be disjoint in validity from every
other live claim of its base tuple, counted over post-commit state: a
run retracted in this transaction does not count, one created in it
does (GH #296, design §2.3)."
```

The `def-unique` half is refuted. Full evidence, refutation attempts and
the fix options are in C1.

## A8 — names and exports — CONFIRMED (all nine)

| symbol | defined | exported? |
|---|---|---|
| `writes` | `transactions.lisp:362-366`, `(defgeneric writes (transaction) ...)` returning `(append (object-set-list (create-set transaction)) (object-set-list (write-set transaction)))` | yes, `package.lisp:436` (`#:validate-transaction #:writes ; GH #320`) |
| `node` (of a write) | `transactions.lisp:912-915`, the `:reader` of `tx-write`'s `node` slot | **no** — so `graph-db::node`, as the plan writes it, is right |
| `make-commit-view` | `value-constraint.lisp:151-158`, `(defun make-commit-view (graph &optional tx) ...)` | yes, `package.lisp:433` |
| `view-node` | `value-constraint.lisp:179-189` | yes, `package.lisp:433` |
| `deleted-p` | node accessor | yes, `package.lisp:375` |
| `mark-deleted` | `interface.lisp:108-118`, a `defgeneric` on `vertex`/`edge` | yes, `package.lisp:388` |
| `copy` | `interface.lisp` (create-set guard, then `%copy`) | yes, `package.lisp:386` |
| `save` | `interface.lisp:120-127` | yes, `package.lisp:387` |
| `graph-name` | graph accessor | yes, `package.lisp:246` |
| `map-vertices` `:record-reads` | `vertex.lisp:185-188`, in the lambda list, default `t` | yes, `package.lisp:381` |

The two the plan will actually lean on, quoted:

```lisp
;; transactions.lisp:912-915
(defclass tx-write ()
  ((node
    :initarg :node
    :reader node)))
```

```lisp
;; value-constraint.lisp:179-189
(defun view-node (view id)
  "The node ID names as it will be after commit: this transaction's
write of it (NIL if that write deletes it), else the store's vertex or
edge, else NIL.  A node created in this commit is found here, not missed
in the store (evaluator note §3; GH #155)."
```

`make-commit-view`'s `tx` is `&optional`, so `(make-commit-view graph tx)`
is right and `(make-commit-view graph)` gives the store-only view.

`writes` returns *creates first, then updates and deletes* — a validator
walking it sees both, and `tx-delete` is a subclass of `tx-update`
(`transactions.lisp:926-933`), so `(typep w 'tx-update)` is true for a
delete. `view-old-node` relies on exactly that
(`value-constraint.lisp:171-177`).

`map-vertices`' `:record-reads` is documented at `vertex.lisp:210-218`:
"inside a read-write transaction, a scan that records every visited node
makes the transaction conflict with ANY concurrent writer touching
anything it scanned". Relevant to P4.

## A9 — `graph-db.spacetime` exports — CONFIRMED (all fourteen)

Every name checked against `spacetime/package.lisp`:

| symbol | line |
|---|---|
| `spacetime-error` | 16 (with `invalid-standing`, `invalid-bound`, `invalid-extent`) |
| `missing-claim-identity-component` | 17 |
| `exact-bound` | 23 |
| `make-interval` | 28 |
| `extent->sexp` | 32 |
| `def-claim-classes`, `claim-family`, `claim-family-parent` | 45 |
| `claim-family-unary`, `claim-family-binary` | 46 |
| `claim-identity-key` | 54 |
| `split-claim-identity-key` | 55 |
| `claims-by-producer`, `delete-claims-by-producer` | 63 |
| `canonical-relation-p`, `canonical-producer-p` | 64 |
| `claim-family-temporal-p`, `extent-sexp-start-key` | 74 |
| `extents-disjoint-p`, `extents-intersect-p`, `extent-disjointness-violation` | 76 |
| `def-source` | 81 |

The four cl-temporal-extent re-exports are legal because the package
`:use`s the library (`spacetime/package.lisp:12`):

```lisp
(defpackage #:graph-db.spacetime
  ;; The temporal layer is cl-temporal-extent (#159).  It is :USEd and its
  ;; symbols re-exported below, so a consumer of GRAPH-DB.SPACETIME sees the
  ;; same API it always did.
  (:use #:cl #:temporal-extent)
```

So `graph-db.spacetime:extent->sexp` and `temporal-extent:extent->sexp`
are the *same symbol*, and `graph-db.rules` may qualify with either.
Note `extents-disjoint-p` is the library's since its 0.2.0
(`spacetime/package.lisp:75`, comment), so it will be
`extent-intersection`'s neighbour after Task 1 — and Task 2 must add
`#:extent-intersection` to `spacetime/package.lisp` if `graph-db.rules`
is to reach it through spacetime rather than through `temporal-extent`
directly. **The plan does not currently say which.**

## A10 — variables, and the `?` trap — CONFIRMED (both halves)

`variable-p` is purely name-based (`prologc.lisp:278-280`):

```lisp
(defun variable-p (x)
  ;;(and (symbolp x) (not (eq x '??)) (equal (char (symbol-name x) 0) #\?)))
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))
```

No package test and no interning test. A symbol read into a scratch
package, and left uninterned by the `delete-package` that follows, is
still a variable to `compile-arg` / `compile-body`; it is still `EQ` to
itself across every goal it appears in, because `READ` interned it once;
and `select` puts it in `vars` unchanged — the projection in A5 leaves
it alone (its `symbol-package` is not `GRAPH-DB`, and after
`delete-package` it is NIL).

`replace-?-vars` compares by `EQ` to `graph-db::?`
(`prologc.lisp:773-779`):

```lisp
(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (intern (symbol-name (gensym "?"))))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (first exp))
                       (replace-?-vars (rest exp))
                       exp))))
```

`'?` there is `graph-db::?`, read when `prologc.lisp` was compiled.
`compile-arg` uses the same `EQ` test (`prologc.lisp:284`:
`((eq arg '?) '(?))`). So a bare `?` typed into rule text and read into a
scratch package is `SCRATCH::?` — **not** `EQ` to `graph-db::?`, never
replaced, and therefore a *named* variable whose name happens to be "?".

The trap is worse than "it is a named variable": **every `?` in one rule
body is the same symbol**, so they all unify with each other. The guard
confirms this rather than fixing it (`query/guard.lisp:443-445`):

```lisp
      ((char= (char name 0) #\?)
       (pushnew sym (gc-vars ctx))
       sym)
```

`pushnew` keeps one entry, so a body with three `?`s yields one column
that must satisfy all three positions. Document it in `docs/rules.md`
as a trap; do not "fix" it in the guard (the fix would be to
`replace-?-vars` by *name*, which is a core change and out of S2's
scope).

## A11 — the guard's canonical symbols — CONFIRMED (mechanism corrected, C3)

**The head.** `%guard-goal` (`query/guard.lisp:452-497`) refuses a
package-qualified head, then rebuilds from the whitelist:

```lisp
      (let ((name (symbol-name head))
            (pkg (symbol-package head)))
        (when (and pkg (not (eq pkg (gc-package ctx))))
          (%refuse "package-qualified goal ~A::~A is not permitted"
                   (package-name pkg) name))
        ...
        (let* ((home (gethash (cons name arity) (gc-functors ctx)))
               (canonical (or (gethash name (gc-control ctx))
                              ;; Bounded: NAME came out of the registry
                              ;; by string match, so this interns only
                              ;; names the image already registered.
                              (and home (intern name home)))))
```

`home` comes from `%functor-whitelist` (`query/guard.lisp:305-334`),
which enumerates `*prolog-global-functors*` and `*user-functors*` and
stores `(symbol-package key)`. `claim/7` is registered from
`rules/facts.lisp:117` under `(in-package #:graph-db)`
(`rules/facts.lisp:10`), so its key is `GRAPH-DB::CLAIM/7`, `home` is
`GRAPH-DB`, and `canonical` is `graph-db::claim`. Same for
`claim-producer/2` (`rules/facts.lisp:246`) → `graph-db::claim-producer`.
**Right answer; but it is derived, not literal — see C3.**

None of the seven S1 functors is excluded: `*prolog-excluded-predicates*`
is `("%COMMIT" "CALL" "CATCH" "FINDALL" "BAGOF" "SETOF" "MAP-QUERY"
"SELECT" "SHOW-PROLOG-VARS")` (`query/guard.lisp:245-247`), and
`*prolog-cost-unbounded-predicates*` is
`(graph-db:cost-unbounded-predicate-names)` snapshotted at load
(`query/guard.lisp:270`) — S1 declared none of the seven, doing its own
per-goal check instead (`rules/facts.lisp:96-110`).

**A schema symbol argument.** `%guard-symbol` (`query/guard.lisp:428-449`):

```lisp
(defun %guard-symbol (sym ctx)
  "SYM validated and translated, or a refusal naming it."
  (let ((name (symbol-name sym))
        (pkg (symbol-package sym)))
    (cond
      ((null pkg)
       (%refuse "uninterned symbol ~A is not permitted" name))
      ;; The scratch package uses nothing, so a symbol resolved
      ;; anywhere else was package-qualified or inherited.
      ((not (eq pkg (gc-package ctx)))
       (%refuse "package-qualified symbol ~A::~A is not permitted"
                (package-name pkg) name))
      ((zerop (length name)) (%refuse "the empty symbol || is not a term"))
      ((string= name "NIL") nil)
      ((string= name "T") t)
      ((char= (char name 0) #\?)
       (pushnew sym (gc-vars ctx))
       sym)
      ((gethash name (gc-schema ctx)))
      ((gethash name (gc-control ctx)))
      (t
       (%refuse "~A is not a Prolog functor, a schema name of this ~
graph, or a ?variable" (string-downcase name))))))
```

`(gc-schema ctx)` is `%schema-name-table` (`query/guard.lisp:363-379`),
whose values are `(node-type-name meta)` — the schema's own class
symbol, from the same metadata `def-claim-classes` registered. It is
therefore `EQ` to the `*claim-families*` key, which is
`claim-family-parent` (`spacetime/claim.lisp:361-363`, `home` =
`(symbol-package parent)`). CONFIRMED.

Two riders. The table is keyed by `symbol-name`, and it holds slot names
as well as type names, so `RULE`, `DERIVATION`, `DERIVATION-UNARY`,
`DERIVATION-BINARY` and every slot they declare must not collide by name
with anything else in that store's schema. And it only contains types
registered in **this** graph — which is what P8 relies on.

**NIL in an argument position.** `(string= name "NIL") → nil`, the fourth
clause above. This is the mechanism, not `%guard-term`'s
`((null x) nil)` (`query/guard.lisp:501`): the scratch package uses
nothing, so `READ` interns a fresh `SCRATCH::NIL` for the text `nil`,
which is not `cl:nil` and is not `null`. It reaches `%guard-symbol` as a
symbol and comes back as `cl:nil`. CONFIRMED — by the string test.

**And see C4:** the character screen refuses `:` before `READ` runs, so
no keyword can appear in a rule body at all.

## A12 — `extent-sexp-start-key` and `claim-identity-key` — CONFIRMED (trap in C5)

`(extent-sexp-start-key sexp)` — one argument, a *stored sexp*, not an
extent (`spacetime/claim.lisp:135-146`); quoted in full in C5. Returns
`(list earliest-key latest-key)` for the START bound, each key either
`:unbounded` or `%timestamp-key`'s `(day sec nsec)` triple
(`spacetime/claim.lisp:129-133`), and NIL for a NIL sexp. **The same
shape for an interval and an instant** — for an instant, START and END
are the same bound object (`~/work/cl-temporal-extent/src/extent.lisp:17-18`),
so the key is that bound.

`claim-identity-key`'s temporal field is exactly as the plan says
(`spacetime/claim-query.lisp:56-60`):

```lisp
            (when (claim-family-temporal-p family)
              (list (let ((*print-case* :downcase))
                      (prin1-to-string
                       (extent-sexp-start-key
                        (claim-extent-sexp claim)))))))))
    (format nil "~{~a~^|~}" fields)))
```

Field order is producer, subject namespace, subject key, *then the object
pair for a binary*, then relation, then the temporal field
(`spacetime/claim-query.lisp:45-60`) — note relation comes *after* the
object pair, matching `def-unique`'s tuple order. Fields join on `|`
with `|` and `\` escaped (`%identity-key-field`,
`spacetime/claim-query.lisp:12-27`), and `split-claim-identity-key`
inverts it (`spacetime/claim-query.lisp:102-…`), deriving arity and
temporality from the field count, 4 to 7.

So Task 4's dedupe can key on either `extent-sexp-start-key` or the whole
`claim-identity-key`; the latter is the better choice for C1's fix (b),
since it is the key the sweep needs to diff on anyway.

## A13 — `*commit-validators*` — CONFIRMED (the package clause is an inference)

**Called as `(funcall fn tx graph)`, manager-locked, pre-durability**
(`transactions.lisp:3439-3442`):

```lisp
               ;; Subsystem validators (GH #157 4b): same region, same
               ;; abort-before-durability contract.
               (dolist (fn *commit-validators*)
                 (funcall fn tx (graph tx)))
```

Note the second argument is `(graph tx)`, not a separate `graph`
parameter — the same value `%validate-extent-disjointness` receives
(`spacetime/temporal.lisp:69`). The enclosing region begins at
`(with-transaction-manager-lock (tm) ...)` (`transactions.lisp:3419`)
and durability (`finalize-tx-persistence`) is at `transactions.lisp:3465`,
after the validators.

**A condition unwinds to the caller unchanged.** `call-with-transaction`
catches exactly one class (`transactions.lisp:3075-3103`):

```lisp
      (loop
         (when (<= *maximum-transaction-attempts* attempt-count)
           (with-transaction-manager-lock (transaction-manager)
             (return (call-transaction-fun))))
         (handler-case
             (return (call-transaction-fun))
           (validation-conflict ()
             (incf attempt-count)))))))
```

Anything else — a `rule-compile-error`, a `unique-constraint-violation`,
an `extent-disjointness-violation` — propagates. The engine's own test of
this shape (`tests/spacetime/temporal-tests.lisp:145-153`):

```lisp
(test two-overlapping-runs-in-one-transaction-are-refused
  "Neither is in the store yet; only the transaction's own creates can
show the overlap (the view's other edge, as membership)."
  (with-claim-graph (g)
    (signals extent-disjointness-violation
      (with-transaction ()
        (%tt-run "r3" "a" (ts 2022 1 1) (ts 2022 3 31))
        (%tt-run "r3" "a" (ts 2022 3 1) (ts 2022 5 31))))
    (is (null (%tt-runs g "r3")))))
```

(The plan cites `:149-152`; the test is at `145-153`.)

Two riders P3 should know. The commit runs inside an `unwind-protect`
**cleanup** form (`transactions.lisp:3081-3095`), which the source calls
out — "The commit runs INSIDE this cleanup, and it signals ... A signal
in a cleanup form abandons the forms after it". And after
`*maximum-transaction-attempts*`, the *whole* transaction body plus its
commit runs holding the manager lock; the lock is recursive
(`transactions.lisp:2970`, `make-recursive-lock`), so the nested take in
`%commit` is fine.

**`make-commit-view` in a validator: CONFIRMED by precedent.**
`%validate-extent-disjointness` calls it in exactly this region
(`spacetime/temporal.lisp:90-91`), and so do the value-constraint,
cardinality and domain/range validators immediately above the
`*commit-validators*` loop.

**Creating and deleting a package in a validator: INFERENCE, not
verified.** Nothing in the engine creates a package during commit, so
there is no precedent to cite. The reasoning: the region holds only the
graph's own transaction-manager lock; SBCL's package operations take the
global package lock; nothing that can hold the package lock ever waits on
a graph's manager lock, so no cycle exists. Two real costs stand
regardless of deadlock: `make-package`/`delete-package` are
image-global and serialise all commits against every other thread doing
package work, and `%make-scratch-package` retries up to 64 times on a
name race (`query/guard.lisp:196-207`) — inside a commit, under a lock.
**Recommendation for P3:** guard the rule text *outside* the commit
region wherever possible (compile at write time in `run-rule`/`def-rule`
and cache the result), and let the validator re-check a cached
`compiled-rule` rather than re-guard raw text. If the validator must
guard, say so in the ruling and accept the serialisation.

## A14 — cl-temporal-extent internals — CONFIRMED (all five)

Read from `~/work/cl-temporal-extent/.worktrees/extent-intersection`.

**`%effective-start` / `%effective-end` are internal.** Defined at
`src/allen.lisp:71-80` and `58-69`; neither appears in
`src/package.lisp`'s `:export`.

```lisp
(defun %effective-end (e)
  "E's END with its EARLIEST raised to the START's EARLIEST.  An end is
never before its start, so an unknown or wide end bound must not be
compared as if it could precede the start (GH #2).  MAKE-INTERVAL already
guarantees START's earliest <= END's latest, so the result is a bound."
```

```lisp
(defun %effective-start (e)
  "The mirror of %EFFECTIVE-END: START with its LATEST lowered to END's
LATEST."
```

Both return `%make-bound` results directly, bypassing `make-bound`'s
checks — which is the point: the raised/lowered bound may be one
`make-bound` would refuse. Task 1's `extent-intersection` will need the
same two helpers, and being internal it can call them; if it is written
to be callable from `graph-db.rules` it must not *return* anything built
by `%make-bound` that `make-bound` would reject.

**`bound-compare` is exported** (`src/package.lisp:21`), and is the
four-valued comparison (`src/bound.lisp:57-68`):

```lisp
(defun bound-compare (a b)
  "Compare the timestamps A and B stand for: :< :> := or :AMBIGUOUS.
Definite only when no choice within either range could give another
answer, so two overlapping ranges are :AMBIGUOUS even if they coincide
exactly."
```

**`%make-bound` bypasses the reversed check; `make-bound` signals**
(`src/bound.lisp:7-28`):

```lisp
(defstruct (bound (:constructor %make-bound (earliest latest))
                  (:copier nil))
  "EARLIEST and LATEST are each a LOCAL-TIME:TIMESTAMP or :UNBOUNDED, which
denotes negative infinity in EARLIEST and positive infinity in LATEST."
```

```lisp
(defun make-bound (earliest latest)
  "The range [EARLIEST, LATEST], each a TIMESTAMP or :UNBOUNDED.  Signals
INVALID-BOUND on a non-endpoint or a reversed range."
  (unless (and (%endpoint-ok-p earliest) (%endpoint-ok-p latest))
    (error 'invalid-bound :earliest earliest :latest latest
           :reason "endpoints must be a TIMESTAMP or :UNBOUNDED"))
  (when (and (not (eq earliest :unbounded))
             (not (eq latest :unbounded))
             (local-time:timestamp< latest earliest))
    (error 'invalid-bound :earliest earliest :latest latest
           :reason "EARLIEST is after LATEST"))
  (%make-bound earliest latest))
```

`%make-bound` is not exported; `make-bound` is (`src/package.lisp:20`).

**`+precisions+` is coarse to fine** (`src/extent.lisp:7-10`):

```lisp
(defparameter +precisions+
  '(:year :month :day :hour :minute :second :nsec)
  "Granularities a record may be stated at.  PRECISION never enters
comparison -- the bound width already encodes it (design §3.2).")
```

So P9's "the coarser precision" is the one with the smaller `position`
in this list. `%check-precision` (`src/extent.lisp:30-34`) refuses
anything not a member, so the default must be computed, never guessed.

## A15 — the registry — CONFIRMED (evaluated; see C8 for what is not yet true)

**The one image run.** Command as given in the task brief, from the
rules-s2 worktree:

```
#P"/home/raison/work/cl-temporal-extent/.worktrees/extent-intersection/"
"0.2.0"
#P"/home/raison/work/vivace-graph-v3/.worktrees/rules-s2/"
```

(`(asdf:system-source-directory :cl-temporal-extent)`,
`(asdf:component-version (asdf:find-system :cl-temporal-extent))`,
`(asdf:system-source-directory :graph-db)`, after
`(ql:quickload :graph-db/rules :silent t)` with both worktrees pushed.)

Both systems resolve to the worktrees. The version floor half is **not
yet satisfied**: the component version is `"0.2.0"`, matching
`cl-temporal-extent.asd:9`, and `graph-db.asd:559` currently reads

```lisp
               (:version :cl-temporal-extent "0.2.0"))
```

so the *form* the plan proposes — a keyword system designator inside
`(:version …)` — is already in use here and works; only the number has to
move, in both files, in Tasks 1 and 2.

**Why the pushes win, and why they are load-bearing.** Quicklisp
*appends* its searchers rather than pushing them
(`~/quicklisp/quicklisp/setup.lisp:231-236`):

```lisp
(defun setup ()
  (unless (member 'system-definition-searcher
                  asdf:*system-definition-search-functions*)
    (setf asdf:*system-definition-search-functions*
          (append asdf:*system-definition-search-functions*
                  (list 'local-projects-searcher
                        'system-definition-searcher))))
```

so ASDF's own `sysdef-central-registry-search` runs first. That matters
because `~/quicklisp/local-projects/` symlinks both `.asd` files to the
**main checkouts** — see C8.

**The extent worktree moved under this pass — read the version, do not
trust this note for it.** When the image run above was made, HEAD was
`1ca7765` and Task 1's work was present but uncommitted:
`src/allen.lisp` (+79), `src/package.lisp` (+1/-1, adding
`#:extent-intersection` at `src/package.lisp:35`),
`tests/allen-tests.lisp` (+131), with the `.asd` still at `0.2.0` —
which is why the image reported `"0.2.0"`. By the end of this pass Task 1
had committed as `3b5c75a` and `cl-temporal-extent.asd:8` read
`:version "0.3.0"`. So the mechanism (the worktree wins) is verified and
permanent; the version number in the transcript is a timestamp, not a
fact about the tree now. `graph-db.asd:559` was still `"0.2.0"` at both
readings.

## A16 — a static effect classification — **THERE IS NONE**

Asked plainly, answered plainly: **the engine has no static, per-functor
classification of `:write` / `:eval` / `:io` effects.** No registry, no
symbol property, no `declare-functor-effect`. Nothing a guard or a
compiler could consult without running the goal.

The whole of the effect machinery is three specials and one function,
all run-time (`prologc.lisp:1017-1030`):

```lisp
(defvar *allowed-effects* t
  "Effects permitted for the current query: T = all (the default), or a list of
permitted tags drawn from (:write :eval :io).  Graph reads and pure logic are
always allowed and are not tagged.")
(defvar *default-allowed-effects* t
  "Effect policy applied to queries that don't specify :EFFECTS.")

(defun require-effect (effect)
  "Signal PROLOG-PERMISSION-ERROR unless EFFECT is permitted by the current
*ALLOWED-EFFECTS* policy.  Called at entry by every side-effecting functor."
  (unless (or (eq *allowed-effects* t)
              (member effect *allowed-effects*))
    (error 'prolog-permission-error
           :reason (format nil "~A is not permitted in this query" effect)
           :ball (permission-error-ball effect :query nil))))
```

The tag lives *only* in the functor body. Every call site in the tree
(`grep -rn require-effect`, nine in functor bodies plus the definition
and the export):

| functor | tag | line |
|---|---|---|
| `read/1` | `:io` | `prolog-functors.lisp:69` |
| `write/1` | `:io` | `prolog-functors.lisp:73` |
| `nl/0` | `:io` | `prolog-functors.lisp:77` |
| `lisp/2` | `:eval` | `prolog-functors.lisp:145` |
| `lispp/1` | `:eval` | `prolog-functors.lisp:164` |
| `is/2` | `:eval` | `prolog-functors.lisp:201` |
| `trigger/1` | `:eval` | `prolog-functors.lisp:406` |
| `retract/1` | `:write` | `prolog-functors.lisp:930` |
| `retract/3` | `:write` | `prolog-functors.lisp:937` |

`%excluded-predicate-p` (`query/guard.lisp:297-300`) lists names only,
and none of these nine is on either list:

```lisp
(defun %excluded-predicate-p (name)
  "NAME is withheld from free text, for either reason."
  (or (member name *prolog-excluded-predicates* :test #'string=)
      (member name *prolog-cost-unbounded-predicates* :test #'string=)))
```

So the guard **admits** a body naming `retract`, and `run-query-goals`'
hardcoded `:effects nil` (`query/dsl.lisp:339`) turns it into a
`prolog-permission-error` when the goal is reached — not before.
**PF2 is upheld: an effecting functor in a body is a RUN refusal.**

**What does exist, and is the exact shape a static effect registry would
take, is the *cost* classification** — worth quoting because it shows the
cost of building the missing one is small, and because whoever builds it
should copy this and not invent a second idiom
(`prolog-functors.lisp:29-52`):

```lisp
;;; Cost-boundedness classification (GH #285).  %TICK preempts at goal
;;; boundaries only, so a functor that can burn arbitrary time in ONE
;;; atomic Lisp call is invisible to the rails.  Classify such functors
;;; here, engine-side, so every caller -- not just the GUI whitelist --
;;; can know; SELECT refuses them when a resource bound is in effect.
(defvar *cost-unbounded-functors*
  #+sbcl (make-hash-table :test 'eq :synchronized t)
  ...
  "NAME/ARITY functor symbols whose worst-case cost is bounded neither
by the graph nor by the query length (GH #285).")

(defun declare-functor-cost-unbounded (functor-symbol)
  ...
  (setf (gethash functor-symbol *cost-unbounded-functors*) t)
  functor-symbol)

(defun functor-cost-unbounded-p (functor-symbol)
  (values (gethash functor-symbol *cost-unbounded-functors*)))
```

with the static walk over a goal list already written
(`prologc.lisp:982-1007`):

```lisp
(defun %static-goal-functors (goals)
  "The NAME/ARITY functor symbols named by GOALS, walking the control
constructs (and/or/not/if/when/unless).  Conservative and static: a
meta-call through a variable is invisible here, exactly as it is to the
compile-time path (GH #285, #279)."
```

```lisp
(defun %refuse-cost-unbounded (functors)
  "Signal for the first cost-unbounded functor in FUNCTORS when a
resource bound is in effect and the query did not opt out (GH #285).
  ...
    (dolist (f functors)
      (when (functor-cost-unbounded-p f)
        (error 'prolog-cost-unbounded-error :functor f)))))
```

**Bearing on PF2.** A compile-time refusal *is* buildable in S2 without
touching core: walk the guarded goal list with
`graph-db::%static-goal-functors` and refuse against a hand-maintained
list of the nine names above. Two reasons not to, and they are why PF2
stands:

1. **It would drift, silently and in the unsafe direction.** The list
   would be `graph-db.rules`' own, with no tripwire — unlike
   `*prolog-excluded-predicates*`, which
   `prolog-functor-inventory-is-pinned` fails on any registry change
   (`query/guard.lisp:240-243`). A tenth effecting functor added later
   would be admitted at compile and caught only at run, which is the
   status quo minus the illusion of coverage.
2. **`%static-goal-functors` is conservative by construction** — "a
   meta-call through a variable is invisible here" — so a compile
   refusal could never be complete anyway, and the run refusal has to
   exist regardless.

If the engine ever grows `declare-functor-effect` +
`functor-effects` (the `declare-functor-cost-unbounded` shape, tagged at
each `require-effect` site), the compile refusal becomes correct and
maintainable and PF2 should be revisited. That is a core change, and out
of S2's scope.
