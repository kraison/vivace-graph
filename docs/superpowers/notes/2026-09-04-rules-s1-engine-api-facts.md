# Engine API facts, verified for rules S1 (#330)

A snapshot, not a maintained document. Six agents read the engine source and
quoted every form `graph-db/rules` S1 was about to call; fifteen more tried to
refute what they found. It is committed because S2 (#331) calls the same APIs
and re-deriving this costs the same effort again — and because the corrections
in §C are the record of why `rules/facts.lisp` looks the way it does.

**Pinned to `79bfe89`** (the #329 merge, this branch's base). Every
`file:line` is that tree and will drift; the quoted forms are what to match
on, not the numbers. Paths beginning `~/work/cl-temporal-extent` are the
temporal library, not this repo. Nothing here was evaluated in an image —
every form is quoted verbatim from source, so a claim about *runtime*
behaviour is an inference from the code and marked as such where it matters.

Reading order:

- **§C — corrections.** Six defects this pass found in the S1 plan before
  execution began, each verified against source. Historical now: all six were
  ruled on (see `docs/superpowers/decisions/2026-09-04-rules-s1-rulings.md`)
  and the shipped code follows the corrections, not the plan. Kept because
  each one explains a non-obvious choice in `rules/facts.lisp`.
- **Everything after** — the reference proper: registering a global Prolog
  functor, the trail discipline for a generator, claim families and their
  accessors, the indexes claim classes declare, what the guard admits and
  what it signals, the suite and fixture template, and temporal extents.

The contract this produced is `docs/rules.md`; that file, not this one, is
what a caller should read.

Line lengths here exceed the repo's 80-column rule in about 70 places
(tables, and quoted context). Left as verified rather than rewrapped by hand,
which risks silently corrupting a quoted form.

---

# §C — corrections to the S1 plan (historical)

Six distinct defects, all verified.  The 13 numbered findings collapse onto
these; the map is at the end of this section.

## C1 (BLOCKING) — `def-global-prolog-functor` does NOT intern or export in `GRAPH-DB`

**Plan assumes:** "`def-global-prolog-functor` interns and exports in
`graph-db`", so `claim/7` defined in `rules/facts.lisp` lands in `GRAPH-DB` and
a raw goal head `claim` read in `graph-db/rules-test` resolves via
`make-functor-symbol`'s `GRAPH-DB` fallback.  Fallback offered: `(:import-from
#:graph-db #:claim)`.

**Reality.** The macro splices `NAME` **as read** and `export`s from `*package*`
at load time — `prolog-functors.lisp:11-23`:

```lisp
(defmacro def-global-prolog-functor (name lambda-list &body body)
  "Define a global Prolog functor (query predicate) NAME, which must be of the
form PREDICATE/ARITY (e.g. divisible-by/2).  ..."
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (export ',name)
     (setf (gethash ',name *prolog-global-functors*) #',name)))
```

Under `(in-package #:graph-db.rules)` this defines, exports and registers
`GRAPH-DB.RULES::CLAIM/7`.  `GRAPH-DB` never gains a `CLAIM/7`.

`make-functor-symbol` — `prologc.lisp:202-211` — probes only two packages:

```lisp
(defun make-functor-symbol (symbol arity &key define)
  "The NAME/ARITY functor symbol for SYMBOL.  DEFINE (true only from
the definition path -- <- via ADD-CLAUSE, and any other site minting
a symbol for a NEW functor) skips lookup and interns NAME straight
into *PACKAGE* ...  Otherwise resolved by
LOOKUP first: an already-registered functor in SYMBOL's own package,
then in GRAPH-DB ... (GH #322)."
  (let ((name (format nil "~{~a~}" (list symbol '/ arity))))
    (flet ((hit (pkg)
             (and pkg
                  (let ((s (find-symbol name pkg)))
                    (and s (%registered-functor-p s) s)))))
      (or (and (not define) (symbolp symbol)
               (or (hit (symbol-package symbol))
                   (hit (find-package :graph-db))))
          (intern name)))))
```

```lisp
;; prologc.lisp:193
(defun %registered-functor-p (symbol)
  "True when SYMBOL is a live functor -- a user clause (GET-FUNCTOR-FN)
or a global primitive (*PROLOG-GLOBAL-FUNCTORS*).  Both lookups are
read-only: no symbol is created or registered by asking (GH #322)."
  (or (get-functor-fn symbol)
      (nth-value 1 (gethash symbol *prolog-global-functors*))))
```

Probe 1 = the **head symbol's own package**; probe 2 = `GRAPH-DB`; else
`(intern name)` in `*package*` → unregistered → `compile-call`
(`prologc.lisp:230-244`) signals `"unknown Prolog functor CLAIM/7"`.
`graph-db` `(:use #:cl)` only (`package.lisp:4`), so probe 2 misses.

**The fallback in the plan names the wrong symbol.** Resolution consults
`CLAIM/7`, never the bare `CLAIM`; `(:import-from #:graph-db #:claim)` fixes
nothing (and `graph-db` has no `claim` to import).

**Fix — pick one, then be consistent:**

- (a) `(in-package #:graph-db)` at the top of `rules/facts.lisp`.  This is what
  every existing functor file does, `algorithms/prolog.lisp:17` included (a
  different ASDF system, still `(in-package :graph-db)`).  Then probe 2 hits
  from any package.  Cost: every spacetime symbol needs a
  `graph-db.spacetime:` prefix in that file.
- (b) Keep `(in-package #:graph-db.rules)` and make probe 1 hit.  Two ways,
  both correct:
  - export the **bare head** names from `graph-db.rules` (`(:export #:claim
    #:claim-current #:claim-valid-at #:claim-producer #:claim-standing
    #:claim-relation #:claim-rule-version)` — the macro does **not** create
    these, the `defpackage` interns them) and `(:import-from #:graph-db.rules
    …)` them into every package writing a raw `select`; the head's
    `symbol-package` is then `GRAPH-DB.RULES` and probe 1 finds `CLAIM/7`
    there; **or**
  - `(:import-from #:graph-db.rules #:claim/7 #:claim-current/1
    #:claim-valid-at/2 #:claim-producer/2 #:claim-standing/2
    #:claim-relation/2 #:claim-rule-version/2)` into that package, so probe 1
    finds the imported `/ARITY` symbol in the head's own package.

**Do NOT write `(graph-db::def-global-prolog-functor graph-db::claim/7 …)` from
a `graph-db.rules` file.**  The expansion is `(export 'graph-db::claim/7)` with
`*package*` = `GRAPH-DB.RULES`, and `export` signals a `package-error` because
that symbol is not accessible there.

**The guarded path works either way** and needs no fix: `%functor-whitelist`
(`query/guard.lisp:302`) enumerates the registry keeping `(symbol-package key)`
as the home, and `%guard-goal` (`query/guard.lisp:478-483`) rebuilds the head as
`(intern name home)` → `graph-db.rules::claim`, so probe 1 hits.  Only the raw
`select` from a test package breaks.

*Evidence:* `prolog-functors.lisp:11-23`; `prologc.lisp:193`, `:202-211`,
`:218-226`, `:242-244`; `package.lisp:4`; `query/guard.lisp:302`, `:478-483`;
`prolog-functors.lisp:1132-1155` (`%install-edge-functors`, the same-package
precedent).  *(Findings 1, 2, 5, 9.)*

## C2 (BLOCKING) — `select`'s OPTIONS list is required and positional

**Plan assumes:** `(select (?o) (claim ?c rt-claim "host" "h1" "runs" "app" ?o))`
— vars first, no options group.  Six occurrences: plan lines 186, 315, 321, 329,
335 (line 341 is already right).

**Reality** — `prologc.lisp:1030`:

```lisp
(defmacro select (options vars &rest goals)
```

The 2-group `(defmacro select (vars &rest goals)` at `prologc.lisp:827` is
**inside a `#| … |#` block comment** opened at `:826` and closed at `:864`.  It
is not defined.

Worse than an error: `(select (?o) goal)` **compiles cleanly** and returns one
junk row.  `options` is digested by `plist-alist` (`prologc.lisp:1024`, called
at `:1062` — `(options (plist-alist options))`), which tolerates an odd list and
yields an empty alist; `(claim …)` then becomes the **vars** list and `goals` is
empty.  So Task 1 Step 5's expected `"unknown Prolog functor CLAIM/7"` will
never appear.

Write `(select () (?o) (claim …))`.  The canonical empty-options idiom is in the
engine itself — `select-first`, `prologc.lisp:1147`: `` `(first (select ()
,vars ,@goals !)) ``.

**The plan is internally inconsistent about this**: `select-flat`,
`select-count`, `select-first`, `select-one` and `do-query` really *are*
`(vars &rest goals)` (`prologc.lisp:1132`, `:1137`, `:1144`, `:1149`, `:1154`),
so those call sites are correct as written.

*Evidence:* `prologc.lisp:1030` (live), `:826-864` (the commented-out form),
`:1024` and `:1062` (`plist-alist`), `:1132-1152` (the shorthands), `:1147`.
*(Findings 3, 4, 12, 13.)*

## C3 (BLOCKING) — index slot-name lists must be `graph-db.spacetime::`-qualified

**Plan assumes:** `'(subject-namespace subject-key relation)`,
`'(object-namespace object-key)`, `'(producer)` written unqualified from package
`GRAPH-DB.RULES`.

**Reality.** The indexes are declared inside `spacetime/claim.lisp`
(`(in-package #:graph-db.spacetime)`, `claim.lisp:6`), so the stored slot
symbols are `GRAPH-DB.SPACETIME`'s — `claim.lisp:440-450`:

```lisp
       (graph-db:def-index ,parent (subject-namespace subject-key)
           ,graph-name :name claim-subject)
       (graph-db:def-index ,binary (object-namespace object-key)
           ,graph-name :name claim-object)
       (graph-db:def-index ,parent (producer) ,graph-name
                           :name claim-producer)
       (graph-db:def-index ,parent (subject-namespace subject-key
                                    relation)
           ,graph-name :name claim-subject-relation)
```

`def-index` stores them quoted (`index.lisp:897`,
`:slot-names (%normalize-slots ',slot)`), and lookup compares by `EQUAL` over
the symbol list plus a `gethash` on `(cons class-name slot-names)` in an
`:test 'equal` table (`index.lisp:568`):

```lisp
;; index.lisp:936-945  (%secondary-index-lookup)
  (let ((reg (secondary-indexes graph))
        (slot-names (%normalize-slots slot-name)))
    (when reg
      (or (gethash (cons class-name slot-names) reg)
;; index.lisp:966-971  (%def-index-declared-p)
         (some (lambda (spec)
                 (and (equal (index-spec-slot-names spec) slot-names)
                      (subtypep class-name (index-spec-owner-name spec))))
               (%registered-index-specs graph))))
```

`EQUAL` on symbols is `EQ`, so package matters.  From `GRAPH-DB.RULES` the bare
list interns fresh `GRAPH-DB.RULES::SUBJECT-NAMESPACE` … symbols, nothing
matches, and `%require-index` signals (`index.lisp:985-988`):

```lisp
          (t (error 'query-precondition-error
                    :reason (format nil "No secondary index on ~S.~S in ~S"
                                    class-name slot-name
                                    (graph-name graph)))))))
```

**Fix:** qualify, exactly as `tests/spacetime/claim-tests.lisp:150-152` does:

```lisp
'(graph-db.spacetime::subject-namespace
  graph-db.spacetime::subject-key
  graph-db.spacetime::relation)
```

The six slot symbols (`subject-namespace`, `subject-key`, `relation`,
`producer`, `object-namespace`, `object-key`) are **not exported** — the export
list carries only the *accessors* `claim-subject-namespace` etc.
(`spacetime/package.lisp:47`) — so `::` is required whether you qualify at the
call site or `:import-from` them into `graph-db.rules`.

*Evidence:* `spacetime/claim.lisp:6`, `:440-450`; `index.lisp:568`, `:897`,
`:936-945`, `:966-971`, `:985-988`; `spacetime/package.lisp:47`;
`tests/spacetime/claim-tests.lisp:150-152`.  *(Finding 6.)*

## C4 (IMPORTANT) — `extents-disjoint-p`, `make-instant`, `exact-bound` are not in the plan's import list

**Plan assumes:** `claim-valid-at/2` in `GRAPH-DB.RULES` calls all three
unqualified (plan lines 458-465).

**Reality.** The plan's `(defpackage #:graph-db.rules (:use #:cl) …)` (plan
lines 82-91) imports 15 symbols and none of these three.  They are
`TEMPORAL-EXTENT` symbols re-exported by `graph-db.spacetime` —
`spacetime/package.lisp:23` (`#:exact-bound`), `:28` (`#:make-instant`), `:76`
(`#:extents-disjoint-p`) — so either `:import-from #:graph-db.spacetime` them or
write `graph-db.spacetime:extents-disjoint-p` etc.

Also note `extents-disjoint-p` `check-type`s both arguments
(`~/work/cl-temporal-extent/src/allen.lisp:182-192`), so a `NIL` from
`claim-extent` signals a `type-error` rather than answering.  The plan already
guards with `(and e …)` at line 464, so this is a note, not a defect.

*Evidence:* `~/work/cl-temporal-extent/src/allen.lisp:182-192`,
`src/extent.lisp:55-60`, `src/bound.lisp:30-32`; `spacetime/package.lisp:23`,
`:28`, `:76`.  *(Finding 7.)*

## C5 (IMPORTANT) — `%unbound-claim-scan` must pass `:collect-p t`

**Plan assumes:** a side-effect scan — `(map-vertices (lambda (v) (push v out))
graph :vertex-type …)` — then returns `out`.

**Reality.** `map-vertices` runs the walk inside `with-read-pin`
(`vertex.lisp:233`) and materialises node bytes **only** when `:collect-p` is
true — `vertex.lisp:226-232`:

```lisp
         ;; When collecting, each node ESCAPES the scan pin, so materialize its
         ;; bytes before FN sees it.  For a side-effect scan FN runs inside the
         ;; pin, so its lazy reads are already safe and we don't pre-read bytes.
         (fn (if collect-p
                 (let ((user-fn fn))
                   (lambda (node) (ensure-node-bytes node graph) (funcall user-fn node)))
                 fn)))
```

Inside the pin `*read-pinned-p*` is T, so `lookup-object`'s own
escape-materialisation is skipped (`transactions.lisp:304`, `:316-317`).  Nodes
pushed out of a side-effect scan carry lazy, unpinned data blocks; a later slot
read can hit a reaped version.

**Fix:** `(map-vertices #'identity graph :collect-p t :vertex-type
(claim-family-parent family))` and use its return value — or call
`graph-db::ensure-node-bytes` inside the lambda.

`index-lookup` does not have this problem: `%node-by-id` → `lookup-vertex` →
`lookup-object`, standalone, which applies `ensure-node-bytes` itself.

*Evidence:* `vertex.lisp:226-232`, `:233`, `:271`; `transactions.lisp:304`,
`:316-317`.  *(Finding 8.)*

## C6 (IMPORTANT) — two defects in the Task 4 guard work

### C6a — `host` is not a schema name; the test would fail at the wrong layer

**Plan assumes:** `(claim ?c host "host" "h1" ?r ?a ?b)` through the guard
signals `prolog-ill-typed-error` because `host` is "a schema vertex type name
that is not a claim family".

**Reality.** `host` is not a vertex type, edge type or declared slot of
`:graph-db-rules-test` — the fixture declares only `rt-claim{,-unary,-binary}`
and `rtt-claim{,-unary,-binary}`.  `%guard-symbol`'s final clause refuses an
unknown bare name **before any goal runs** — `query/guard.lisp:445-449`:

```lisp
      ((gethash name (gc-schema ctx)))
      ((gethash name (gc-control ctx)))
      (t
       (%refuse "~A is not a Prolog functor, a schema name of this ~
graph, or a ?variable" (string-downcase name))))))
```

`%refuse` signals `prolog-guard-error` (`query/guard.lisp:86-88`), so
`unknown-claim-family` is never reached.

**Fix:** use a real non-family schema name — `rt-claim-unary` is a vertex type
of the graph (so the guard admits it) but is not a key of `*claim-families*`
(only the parent is), so `claim-family` signals `unknown-claim-family` at run
time, which is the path the test means to exercise.

*Evidence:* `query/guard.lisp:86-88`, `:363-379` (`%schema-name-table`),
`:445-449`.  *(Finding 10.)*

### C6b — the `find-symbol` lookup must be deferred to call time

**Plan assumes:** classify `unknown-claim-family` in `%ill-typed-condition-p`
"by find-symbol, as `*no-applicable-method-type*` does".

**Reality.** `*no-applicable-method-type*` is a `defvar` evaluated **once, at
load of `query/guard.lisp`** — `query/guard.lisp:586-598`:

```lisp
(defvar *no-applicable-method-type*
  (or #+sbcl (find-symbol "NO-APPLICABLE-METHOD-ERROR" "SB-PCL")
      #+ccl (find-symbol "NO-APPLICABLE-METHOD-EXISTS" "CCL")
      nil)
  "The implementation's condition class for a generic function called
with arguments no method matches, or NIL where it has none.  ANSI
defines the NO-APPLICABLE-METHOD generic but no condition class, so
this is looked up by name once at load.  ...")
```

`SB-PCL` always exists at that moment; `GRAPH-DB.SPACETIME` need not.
`graph-db/query` is `:depends-on (:graph-db/core)` only (`graph-db.asd:197`) and
is meant to load standalone, so a load-time
`(find-symbol "UNKNOWN-CLAIM-FAMILY" "GRAPH-DB.SPACETIME")` is `NIL` for the
life of the image and the condition falls through to `prolog-server-fault`
(`query/guard.lisp:665`).

**Fix:** resolve the class **at call time**, memoised on the first non-`NIL`
`find-package` + `find-symbol` (or match on `(class-name (class-of c))`'s
`symbol-name`).  There is no in-tree precedent for the deferred form; you are
writing it new.

The classifier as it stands — `query/guard.lisp:600`, tail:

```lisp
  (or (typep c 'graph-db:query-precondition-error)
      (and *no-applicable-method-type*
           (typep c *no-applicable-method-type*))))
```

*Evidence:* `query/guard.lisp:586-598`, `:600`, `:665`; `graph-db.asd:197`;
`spacetime/claim.lisp:8`.  *(Finding 11.)*

## Finding → correction map

| finding | correction |
|---|---|
| 1, 2, 5, 9 (prolog-engine, prolog-select, query-guard) | **C1** |
| 3, 4, 12, 13 (prolog-engine, prolog-select, test-scaffolding) | **C2** |
| 6 (spacetime-claims) | **C3** |
| 7 (spacetime-claims) | **C4** |
| 8 (indexes) | **C5** |
| 10, 11 (query-guard) | **C6a**, **C6b** |

---

# 1. Registering a global Prolog functor

## 1.1 The macro — `prolog-functors.lisp:11` — `graph-db:def-global-prolog-functor` (exported, `package.lisp:475`)

```lisp
(defmacro def-global-prolog-functor (name lambda-list &body body)
  "Define a global Prolog functor (query predicate) NAME, which must be of the
form PREDICATE/ARITY (e.g. divisible-by/2).  LAMBDA-LIST is the predicate's
arguments followed by a final CONT continuation argument.  In BODY, VAR-DEREF
each argument to get its value, and FUNCALL CONT once for each solution to
signal success (not calling CONT means the goal fails).  To bind an unbound
argument, UNIFY it and undo with UNDO-BINDINGS on backtracking.  In a query you
write the predicate WITHOUT the /arity suffix; the compiler appends it from the
goal's argument count."
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (export ',name)
     (setf (gethash ',name *prolog-global-functors*) #',name)))
```

- **Package homing: see C1.**  This is the single highest-risk fact in the file.
- The macro does **no** parsing of `foo/7`; `/7` is inert text in one symbol
  name.  Parsing runs the other way, at goal-compile time
  (`make-functor-symbol`, quoted in C1): the `N` in the name must equal the
  number of non-`cont` parameters, so the lambda list has **arity + 1**
  parameters with `cont` last.  A mismatch means the goal never resolves.
- `cont` last is convention only; the macro checks nothing.  The `?` prefix on
  parameter names carries no meaning.

## 1.2 The registry — `globals.lisp:420` — `graph-db:*prolog-global-functors*` (exported, `package.lisp:479`)

```lisp
#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
```

key = the `NAME/ARITY` **symbol**; value = the **function object**.  Default
`EQL` test, so lookup is symbol identity.

Trap: `prolog-functors.lisp:3` re-declares the same variable with
`:test 'equalp`.  That form is dead — `globals.lisp` loads first
(`graph-db.asd:42` vs `:132`) and `defvar` does not reassign a bound variable.

## 1.3 Success, failure, error — the contract

`compile-call` — `prologc.lisp:230`:

```lisp
(defun compile-call (predicate arity args cont)
  "Compile a call to a prolog predicate."
  (let ((functor (make-functor-symbol predicate arity)))
    `(let ((func (or (get-functor-fn ',functor)
                     (gethash ',functor *prolog-global-functors*))))
       (%tick)                          ; account one inference / enforce bounds
       (when *prolog-trace*
         (format t "TRACE: ~A/~A~A~%" ',predicate ',arity ',args))
       (if (functionp func)
           (funcall func ,@args ,cont)
           ;; Unknown predicate: stay noisy (a mistyped goal surfaces) but carry
           ;; an existence_error ball so catch/3 can recover from it.
           (error 'prolog-error
                  :reason (format nil "unknown Prolog functor ~A" ',functor)
                  :ball (existence-error-ball :procedure ',functor))))))
```

- **A functor's return value is ignored.**  `select` returns `*select-list*` /
  `*select-current-count*` (`prologc.lisp:1128-1130`); the terminal continuation
  is `#'prolog-ignore` (`prologc.lisp:785`), which returns `NIL`.
- success, one solution = one `(funcall cont)`.  Return whatever.
- failure / no solutions = return normally **without** calling `cont`.  Do not
  return `NIL` "to fail" or `T` "to succeed".
- error = `(error 'prolog-error …)` or any subtype; it aborts the query.
  `unknown-claim-family` propagates the same way.
- `(funcall cont)` may never return.  Control constructs exit non-locally:
  `once/1` `(return-from done)` (`prolog-functors.lisp:371`), `or/2`
  `(return-from or/2 t)` (`:421`), `not/1` `(return-from not/1 nil)` (`:414`),
  `select/2` `(throw :prolog-limit-reached nil)` (`:626`), cut
  (`prologc.lisp:751`).  A generator must tolerate that.

## 1.4 `%tick` and the cost-unbounded rule

`%tick` fires only at a **goal boundary** — `prologc.lisp:930-941`:

```lisp
(declaim (inline %tick))
(defun %tick ()
  "Account one inference and abort the query (PROLOG-RESOURCE-ERROR) if a
resource bound is exceeded.  A no-op when no bound is in effect."
  (when *inference-budget*
    (when (> (incf *inference-count*) *inference-budget*)
      (error 'prolog-resource-error
             :reason (format nil "inference budget exceeded (~D)" *inference-budget*)
             :ball (resource-error-ball :inferences))))
  (when (and *query-deadline* (>= (get-internal-real-time) *query-deadline*))
    (error 'prolog-resource-error :reason "query timeout exceeded"
           :ball (resource-error-ball :time))))
```

Exactly three call sites: `compile-call` (`prologc.lisp:235`), `repeat/0`
(`prolog-functors.lisp:84`), `%solve` (`prolog-functors.lisp:247`).  Once
control is inside a functor body no tick runs, which is why a family-wide walk
must refuse under a bound.

The engine's own "is a bound in effect" test — `%refuse-cost-unbounded`,
`prologc.lisp:992`:

```lisp
(defun %refuse-cost-unbounded (functors allow-p)
  "Signal for the first cost-unbounded functor in FUNCTORS when a
resource bound is in effect and ALLOW-P is false (GH #285)."
  (when (and (not allow-p)
             (or *inference-budget* *query-deadline*))
    (dolist (f functors)
      (when (functor-cost-unbounded-p f)
        (error 'prolog-cost-unbounded-error :functor f)))))
```

So `(or graph-db::*inference-budget* graph-db::*query-deadline*)` is the correct
runtime test for `%unbound-claim-scan`.

The condition — `prologc.lisp:963`, `graph-db:prolog-cost-unbounded-error`
(exported, `package.lisp:509`):

```lisp
(define-condition prolog-cost-unbounded-error (prolog-error)
  ((functor :initarg :functor :reader prolog-cost-unbounded-functor))
  (:report
   (lambda (c s)
     (format s "Goal ~(~a~) is cost-unbounded: %TICK cannot preempt ~
                inside one functor call, so :MAX-INFERENCES/:TIMEOUT ~
                cannot bound it.  Run without resource bounds, or pass ~
                :ALLOW-COST-UNBOUNDED T to accept the risk (GH #285)."
             (prolog-cost-unbounded-functor c))))
  (:documentation "A resource-bounded SELECT refused a goal the rails
cannot actually bound (GH #285)."))
```

- One slot, initarg `:functor`, reader `graph-db::prolog-cost-unbounded-functor`
  (**internal**).  **`functor` has no `:initform`** — signalling without
  `:functor` gives an unbound-slot error the moment the condition is printed
  (FiveAM's `signals` prints on failure).  Always pass `:functor 'claim/7`.
- The class name **is** exported; `graph-db:prolog-cost-unbounded-error` works.
- Signalling it from inside a functor body is legal — `select`'s only
  `handler-case` traps `undefined-function` (`prologc.lisp:1114`) — and it
  reaches the caller unchanged through `run-guarded-prolog` (§5.4).
- Do **not** use `graph-db:declare-functor-cost-unbounded`
  (`prolog-functors.lisp:43`) for a conditionally-unbounded functor: it is
  all-or-nothing and would make `select` refuse *every* bounded `claim/7` call
  up front.

Budget specials — `prologc.lisp:912`, `:915`, `:917`, `:919`, `:921`:

```lisp
(defvar *inference-budget* nil
  "Maximum number of inferences (compiled goal calls / meta-call steps) allowed
for the current query, or nil for unlimited.")
(defvar *inference-count* 0
  "Inferences accounted so far in the current query.")
(defvar *query-deadline* nil
  "INTERNAL-REAL-TIME after which the current query aborts, or nil for none.")
(defvar *default-inference-budget* nil
  "Inference budget applied to queries that don't specify :MAX-INFERENCES.")
(defvar *default-query-timeout* nil
  "Default query timeout in seconds for queries that don't specify :TIMEOUT.")
```

Qualification: `graph-db:*inference-budget*` is **exported**
(`package.lisp:511`); `graph-db::*query-deadline*` and
`graph-db::*inference-count*` are **not**.  Both defaults are `NIL`, so a plain
`select` (and every shorthand) has **no bound in effect** — the walk branch runs
there.  Through `run-guarded-prolog` a bound is **always** in effect (§5.4).

---

# 2. The trail discipline for a generator

## 2.1 The template — `index.lisp:1078` — copy this ordering literally

```lisp
(defun %prolog-index-bound (x)
  "A Prolog range bound: NIL for an unbound variable or an explicit NIL, else
the value.  MAP-INDEX already treats NIL as open-ended, so this surfaces that
convention rather than inventing a second one -- and it means a caller need not
know NIL is the sentinel: leaving the argument unbound says the same thing.
Without it an unbound ?START would deref to a variable struct and be handed to
the index as a key."
  (let ((v (var-deref x)))
    (if (var-p v) nil v)))

(def-global-prolog-functor find-by-slot/4 (?node ?class ?slot ?value cont)
  "Yield each node of ?CLASS (and its subclasses) whose indexed ?SLOT equals
?VALUE, via the secondary index.  ?CLASS may name a subclass of the class the
index is declared on -- it resolves to the owning index.  Signals if no index
covers ?CLASS.?SLOT."
  (let ((node-var (var-deref ?node))
        (class (var-deref ?class))
        (slot (var-deref ?slot))
        (value (var-deref ?value)))
    (dolist (node (index-lookup *graph* class slot value))
      (let ((old-trail (fill-pointer *trail*)))
        (when (unify node-var node)
          (funcall cont))
        (undo-bindings old-trail)))))
```

The five steps, in order:

1. `var-deref` **all** arguments once, up front, outside the loop.
2. Materialise the candidate list — the fill-pointer is **not** captured yet.
3. **Per candidate**, first thing in the loop body: capture
   `(fill-pointer *trail*)` into a fresh `old-trail`.
4. `(when (unify …) (funcall cont))` — `cont` runs inside the `when` (only on a
   successful unify) and **before** any undo.
5. `(undo-bindings old-trail)` as a **sibling** of the `when`, so it runs
   whether or not the unify succeeded (a failed unify may still have bound
   sub-terms).

**No `unwind-protect`.**  Nothing in the tree wraps the undo; a cut or the
`:limit` throw unwinds straight past it, and `select` discards the whole
`*trail*` array afterwards.  Do not "fix" this.

## 2.2 Multi-variable solutions — `invoke-view/5`, `prolog-functors.lisp:812`

The in-tree template for binding several variables per solution: **one** trail
mark and a single `and` of unifies, not nested per-variable marks.

```lisp
(def-global-prolog-functor invoke-view/5 (class-name view-name key node value cont)
  (when *prolog-trace*
    (format t "TRACE: INVOKE-VIEW/5(~A ~A ~A ~A ~A)~%" class-name view-name key node value))
  (setq class-name (var-deref class-name)
        view-name (var-deref view-name)
        key (var-deref key)
        value (var-deref value)
        node (var-deref node))
  (dolist (pair (if (and (var-p key) (bound-p key))
                    (invoke-graph-view class-name view-name :key (var-deref key))
                    (invoke-graph-view class-name view-name)))
    (let ((old-trail (fill-pointer *trail*)))
      (when (and (unify key (cdr (assoc :key pair)))
                 (unify value (cdr (assoc :value pair)))
                 (unify node (lookup-vertex (cdr (assoc :id pair)))))
        (funcall cont))
      (undo-bindings old-trail))))
```

The plan's nested `%yield` (mark / unify / undo per variable) is *also correct* —
the marks stage properly — but costs N marks and N `undo-bindings` scans per
solution instead of one.  Prefer the `and` form for the 6-variable claim case.

## 2.3 The substrate

### `var` struct and `var-p` — `prologc.lisp:97`

```lisp
(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))
  (binding +unbound+))
```

`var-p` is the defstruct predicate, home `GRAPH-DB`, **not exported** →
`graph-db::var-p`.

### `bound-p` — `prologc.lisp:102` (internal)

```lisp
(defun bound-p (var) (not (eq (var-binding var) +unbound+)))
```

`+unbound+` is `(alexandria:define-constant +unbound+ :unbound)`
(`globals.lisp:445`).  **Trap:** the sentinel is the keyword `:UNBOUND`; a claim
slot value that happens to be `:unbound` would read as unbound.

### `var-deref` — `prologc.lisp:104` — **a macro** — `graph-db:var-deref` (exported, `package.lisp:491`)

```lisp
(defmacro var-deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
             do (setf ,exp (var-binding ,exp)))
          ,exp))
```

**It `setf`s its argument place and evaluates it repeatedly.**  `EXP` must be a
settable place — a variable or an accessor form.  `(var-deref (some-call))` will
not compile.  `(let ((v (var-deref x))) …)` mutates `x` too; every existing
functor relies on that.

### `unify` — `prologc.lisp:131` — **two arguments** — `graph-db:unify` (exported, `package.lisp:481`)

```lisp
(defun unify (x y)
  "Destructively unify two expressions."
  (cond ((prolog-equal (var-deref x) (var-deref y)) t)
        ((var-p x) (set-binding x y))
        ((var-p y) (set-binding y x))
        ((and (consp x) (consp y))
         (and (unify (first x) (first y))
              (unify (rest x) (rest y))))
        (t nil)))
```

Exactly 2 args — this is not Norvig's 3-arg `unify`, there is no bindings
argument.  It derefs both sides itself.

Equality is `prolog-equal` — `prologc.lisp:117`:

```lisp
(defgeneric prolog-equal (x y)
  …
  (:method ((x string) (y string)) (string= x y))
  …
  (:method ((x node) (y node)) (equalp (id x) (id y)))
  (:method (x y) (equal x y)))
```

Two nodes unify by **id**, not identity.  Strings unify by `string=`
(case-sensitive).

### `set-binding` — `prologc.lisp:141` (internal)

```lisp
(defun set-binding (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)
```

### `undo-bindings` — `prologc.lisp:149` — **one argument** — `graph-db:undo-bindings` (exported, `package.lisp:492`)

```lisp
(defun undo-bindings (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) +unbound+)))
```

Takes the saved **fill-pointer integer**, never a trail object.

### `*trail*` — `globals.lisp:413` — `graph-db:*trail*` (exported, `package.lisp:496`)

```lisp
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
```

`select` rebinds it to a fresh array per query (`prologc.lisp:1073`), so a mark
from one query is meaningless in another.  Save with `(fill-pointer *trail*)`,
never by copying.

### Node predicates

```lisp
;; primitive-node.lisp:16
(defgeneric node-p (thing)
  (:method ((thing node)) t)
  (:method (thing) nil))

;; vertex.lisp:12
(defgeneric vertex-p (thing)
  (:method ((thing vertex)) t)
  (:method (thing) nil))
```

Both home `GRAPH-DB`, **neither exported** → `graph-db::node-p`,
`graph-db::vertex-p`.  Claims are vertices, so either works; `node-p` is the
weaker assertion and is what the plan uses.  Both are total (return `NIL` for a
`var` struct, a string, `NIL`), but **deref first** or a bound variable tests as
a struct.

## 2.4 What a bound value looks like coming back out of `select`

`select/2`, the collector — `prolog-functors.lisp:587-626`.  Trap:

**A bound value that is a symbol whose home package is `GRAPH-DB` comes back as
its `symbol-name` STRING**, not as the symbol.  Symbols homed in `KEYWORD`,
`GRAPH-DB.SPACETIME`, `GRAPH-DB.RULES` or a test package pass through unchanged.
Nodes pass through as node objects in a raw `select`.

---

# 3. Claim families and their accessors

Home package for everything here: **`GRAPH-DB.SPACETIME`** (export list
`spacetime/package.lisp:14-94`), except the standing/bound/extent vocabulary,
which is `TEMPORAL-EXTENT` re-exported from `graph-db.spacetime`
(`spacetime/package.lisp:13`, `:16-43`) — so
`graph-db.spacetime:extents-disjoint-p` and `graph-db.spacetime:make-instant`
both resolve with one colon.

## 3.1 `def-claim-classes` — `spacetime/claim.lisp:321` (exported, `package.lisp:45`)

```lisp
(defmacro def-claim-classes (parent graph-name
                             &key extra-slots temporal
                                  (keep-revisions (1- (expt 2 32))))
```

**Both positionals are unevaluated.**  `graph-name` must be the literal keyword
(`:graph-db-rules-test`), never `*graph-name*` — it is spliced into
`def-vertex`/`def-unique`/`def-index` and `def-node-type` quotes it
(`schema.lisp:805`).  It must match `make-graph`'s NAME (§6.4).

Class names are derived by `format`, in `parent`'s own package —
`spacetime/claim.lisp:361-363`:

```lisp
  (let* ((home (symbol-package parent))
         (unary (intern (format nil "~A-UNARY" parent) home))
         (binary (intern (format nil "~A-BINARY" parent) home))
```

So `(def-claim-classes rt-claim :g)` gives `RT-CLAIM`, `RT-CLAIM-UNARY`,
`RT-CLAIM-BINARY`.

```lisp
;; spacetime/claim.lisp:376-387
    `(progn
       (graph-db:def-vertex ,parent ()
           (,@+claim-shared-slots+ ,@extra-slots)
         ,graph-name)
       (graph-db:def-vertex ,unary (,parent) () ,graph-name
         :keep-revisions ,keep-revisions)
       (graph-db:def-vertex ,binary (,parent) (,@+claim-object-slots+)
           ,graph-name :keep-revisions ,keep-revisions)
```

**`MAKE-<PARENT>` does not exist** — it is `fmakunbound`ed and the two arity
constructors are wrapped — `spacetime/claim.lisp:451-475`:

```lisp
       (fmakunbound ',(intern (format nil "MAKE-~A" parent) home))
       ;; DEF-VERTEX redefines each raw constructor on every expansion, so
       ;; this cannot double-wrap on a re-evaluated DEF-CLAIM-CLASSES form.
       ,@(mapcar
          (lambda (class identity-keys)
            (let ((ctor (intern (format nil "MAKE-~A" class) home)))
              `(let ((%raw (fdefinition ',ctor)))
                 (setf (fdefinition ',ctor)
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
                             c)))))))
          (list unary binary)
          (list +claim-identity-slots+
                (append +claim-identity-slots+
                        +claim-object-identity-slots+)))
```

Use `MAKE-<PARENT>-UNARY` / `MAKE-<PARENT>-BINARY` (and
`LOOKUP-<PARENT>-UNARY` / `-BINARY`), interned in `parent`'s package.

## 3.2 Constructor keywords

```lisp
;; spacetime/claim.lisp:148-184
(defparameter +claim-shared-slots+
  '((subject-namespace :initarg :subject-namespace
                       :accessor claim-subject-namespace)
    (subject-key :initarg :subject-key :accessor claim-subject-key)
    (relation :initarg :relation :accessor claim-relation
              :check canonical-relation-p)
    (producer :initarg :producer :accessor claim-producer
              :check canonical-producer-p)
    (rule-version :initarg :rule-version :accessor claim-rule-version
                  :initform nil)
    (method :initarg :method :accessor claim-method :initform nil)
    (standing :initarg :standing :accessor claim-standing)
    (confidence :initarg :confidence :accessor claim-confidence
                :initform nil)
    (extent-sexp :initarg :extent-sexp :accessor claim-extent-sexp
                 :initform nil)
    (transaction-extent-sexp :initarg :transaction-extent-sexp
                             :accessor claim-transaction-extent-sexp
                             :initform nil)
    (version-stamp :initarg :version-stamp :accessor claim-version-stamp
                   :initform nil)
    (geometry :initarg :geometry :accessor claim-geometry :initform nil)
    (precision-m :initarg :precision-m :accessor claim-precision-m
                 :initform nil)
    (fraction :initarg :fraction :accessor claim-fraction
              :initform 1.0d0))
  "Slots every claim carries, on the PARENT class. …")

;; spacetime/claim.lisp:314-319
(defparameter +claim-object-slots+
  '((object-namespace :initarg :object-namespace
                      :accessor claim-object-namespace)
    (object-key :initarg :object-key :accessor claim-object-key))
  "Slots only BINARY-CLAIM carries. …")
```

| keyword | unary | binary | note |
|---|---|---|---|
| `:subject-namespace` `:subject-key` `:relation` `:producer` | yes | yes | identity, non-NIL required |
| `:object-namespace` `:object-key` | no | yes | identity on binary |
| `:standing` | yes | yes | required in practice; a `NIL` fails `check-standing` |
| `:rule-version` `:method` `:confidence` `:geometry` `:precision-m` `:fraction` | yes | yes | optional |
| `:extent` | yes | yes | wrapper-only, encoded to `:extent-sexp` |
| `:extent-sexp` | yes | yes | persisted slot; mutually exclusive with `:extent` |
| `:transaction-extent` / `:recorded-at` / `:transaction-extent-sexp` | yes | yes | at most one, else `error` |
| `:version-stamp` | yes | yes | defaults to now |
| `:graph` `:id` `:deleted-p` `:revision` | yes | yes | from `def-vertex`'s closure |
| `:extra-slots` initargs | yes | yes | they live on PARENT |

```lisp
;; spacetime/claim.lisp:293-312
(defparameter +claim-identity-slots+
  '(:producer :subject-namespace :subject-key :relation) …)
(defparameter +claim-object-identity-slots+
  '(:object-namespace :object-key) …)
(defun %check-claim-identity (args keys) …
  (dolist (key keys)
    (when (null (getf args key))
      (error 'missing-claim-identity-component :slot key))))
```

```lisp
;; spacetime/claim.lisp:208-215  (%claim-encode-extent-arg)
  (if (%plist-key-p args :extent)
      (progn
        (when (%plist-key-p args :extent-sexp)
          (error "Pass only one of :EXTENT or :EXTENT-SEXP, not both."))
        (let ((extent (getf args :extent)))
          (list* :extent-sexp (and extent (extent->sexp extent))
                 (%plist-remove args :extent))))
      args))
```

`:graph` is real — `schema.lisp:496-504`:

```lisp
      (lambda (&rest make-args
               &key (graph nil) id deleted-p revision
               &allow-other-keys)
        (let ((graph (%default-store-graph name graph-name graph)))
```

and `schema.lisp:396-402`:

```lisp
(defun %default-store-graph (class-name store-name explicit)
  "R1 resolution: EXPLICIT graph if given, else the OPEN graph named
STORE-NAME, else refuse -- never *GRAPH* (GH #167)."
  (or explicit
      (lookup-graph store-name)
      (error 'default-store-not-open-error
             :class-name class-name :store store-name)))
```

A graph-name mismatch signals `DEFAULT-STORE-NOT-OPEN-ERROR`
(`schema.lisp:339`), not a silent fallback to `*GRAPH*`.

## 3.3 What `:temporal t` changes

1. `claim-family-temporal-p` becomes `T` (`claim.lisp:483-484`).
2. `extent-sexp` joins both identity tuples with a positional `:CANONICALIZE`
   of `extent-sexp-start-key` on the last slot — `claim.lisp:364-375`.
3. An extent becomes required **at commit** — `claim.lisp:431-434`:
   ```lisp
       ,@(when temporal
           `((graph-db:def-value-constraint ,parent extent-sexp ,graph-name
               :required t
               :name claim-extent-required)))
   ```
4. An extent becomes required **at construction**:
   `missing-claim-identity-component` with `:slot :extent`
   (`claim.lisp:462-465`).
5. Live claims on one base tuple must have pairwise-disjoint validity —
   `%validate-extent-disjointness` (`spacetime/temporal.lisp:69-113`, registered
   at `:115`), signalling `extent-disjointness-violation`
   (`spacetime/temporal.lisp:12-35`).

Unconditional schema the macro also emits, all **named** so re-declaration
replaces rather than stacks: `standing-vocabulary`
(`:one-of +standings+ :required t`, `claim.lisp:396-399`),
`transaction-extent-transition` (`claim.lisp:403-406`), `def-unique` on each
arity (`claim.lisp:422-427`), the four indexes (§4), and a `save :before` that
stamps `claim-version-stamp` (`claim.lisp:480-482`).

## 3.4 `claim-family` — `spacetime/claim.lisp:29` (exported, `package.lisp:45`)

```lisp
(defun claim-family (parent)
  "The CLAIM-FAMILY registered for PARENT, or signal UNKNOWN-CLAIM-FAMILY."
  (or (gethash parent *claim-families*)
      (error 'unknown-claim-family :parent parent)))
```

A **function**, not an accessor.  Takes the **parent class-name symbol**
(`'rt-claim`), not a keyword.  Returns a `claim-family` **struct**.
`rt-claim-unary` is *not* a key — only the parent is.

The symbol `claim-family` names both this function and the struct type;
importing it gets both.

## 3.5 The struct — `spacetime/claim.lisp:14-24`

```lisp
(defstruct (claim-family (:constructor %make-claim-family
                             (parent unary binary temporal-p))
                         (:copier nil))
  "The three class names DEF-CLAIM-CLASSES generated together.  Registered so
CLAIMS-TOUCHING and DELETE-CLAIMS-BY-PRODUCER can reach the arity subclasses
from the parent name alone.  TEMPORAL-P: the extent start is in the
identity tuple and live runs must be pairwise disjoint (GH #296)."
  (parent nil :read-only t)
  (unary nil :read-only t)
  (binary nil :read-only t)
  (temporal-p nil :read-only t))
```

All four readers are **exported** (`package.lisp:45-46`, `:74`); the predicate
`claim-family-p` is **not**.  They return class **NAMES as symbols**, never
class objects (`claim.lisp:483-484` registers `',parent ',unary ',binary`), so a
name is directly usable as `index-lookup`'s `class-name`, `map-vertices`'
`:vertex-type` and `typep`'s second argument.

## 3.6 The registry — `spacetime/claim.lisp:26` — **internal**

```lisp
(defvar *claim-families* (make-hash-table :test 'eq)
  "Parent class name -> CLAIM-FAMILY.")
```

`graph-db.spacetime::*claim-families*`.  `EQ` table, key = parent symbol, value
= struct.

**It is image-global, not per-graph** — every family declared anywhere in the
image is in it, including families whose graph is not open.  Iterating it to
answer a query must tolerate `graph-db:query-precondition-error` from
`index-lookup` for a family this graph does not index (§4.3).  In-tree iteration
idioms: `(alexandria:hash-table-values *claim-families*)`
(`claim-query.lisp:39-41`) and `(loop for f being the hash-values of
*claim-families* …)` (`temporal.lisp:38`).

## 3.7 `unknown-claim-family` — `spacetime/claim.lisp:8-12` (exported, `package.lisp:71`)

```lisp
(define-condition unknown-claim-family (spacetime-error)
  ((parent :initarg :parent :reader unknown-claim-family-parent))
  (:report (lambda (c s)
             (format s "~S names no claim family; DEF-CLAIM-CLASSES first."
                     (unknown-claim-family-parent c)))))
```

The **only** initarg is `:parent`.  The reader
`unknown-claim-family-parent` is **not exported**.  Parent class
`spacetime-error` comes from `cl-temporal-extent`.

⚠ `claim-query.lisp:37-42` signals it with `:NAME`, which the condition does not
accept — a latent defect on an unreachable path.  **Use `:parent`; do not copy
that line.**

## 3.8 Claim accessors

All are CLOS `:accessor` generic functions created by `defclass` inside
`def-node-type` (`schema.lisp:795-797`); the symbols live in
`GRAPH-DB.SPACETIME`, so **every claim family in the image shares one set**.
Slot reads go through `slot-value-using-class :around` (`clos.lisp:38-43`),
reading the node's DATA alist — which is why a missing value reads as `NIL`
rather than `unbound-slot`.

| accessor | defined | exported | returns |
|---|---|---|---|
| `claim-subject-namespace` | `claim.lisp:149-150` | `package.lisp:47` | **keyword** (`:host`) |
| `claim-subject-key` | `claim.lisp:151` | `:47` | string (an integer key round-trips as its decimal string) |
| `claim-relation` | `claim.lisp:153-154` | `:49` | canonical string `[a-z0-9-]+` |
| `claim-object-namespace` | `claim.lisp:315-316` | `:48` | **keyword**; BINARY only |
| `claim-object-key` | `claim.lisp:317` | `:48` | string; BINARY only |
| `claim-producer` | `claim.lisp:155-156` | `:49` | canonical string `[a-z0-9-/]+` |
| `claim-standing` | `claim.lisp:160` | `:50` | **keyword** from `+standings+` |
| `claim-rule-version` | `claim.lisp:157-158` | `:49` | stored value; `NIL` by default |
| `claim-method` | `claim.lisp:159` | `:50` | string or `NIL` |
| `claim-confidence` | `claim.lisp:161-162` | `:50` | number or `NIL` |
| `claim-extent-sexp` | `claim.lisp:163-164` | `:51` | sexp or `NIL` |
| `claim-transaction-extent-sexp` | `claim.lisp:167-169` | `:59` | sexp or `NIL` |
| `claim-version-stamp` | `claim.lisp:173-174` | `:57` | `local-time:timestamp` or `NIL` |
| `claim-extent` (function) | `claim-query.lisp:329-334` | `:53` | `TEMPORAL-EXTENT` struct or **`NIL`** |
| `claim-current-p` (function) | `claim-query.lisp:383-388` | `:61` | boolean |

```lisp
;; spacetime/claim-query.lisp:329-334
(defun claim-extent (claim)
  "CLAIM's TEMPORAL-EXTENT, decoded from the stored sexp, or NIL.  The stored
form is EXTENT-SEXP; the two never share a name so neither is mistaken for
the other (design §7)."
  (let ((s (claim-extent-sexp claim)))
    (when s (sexp->extent s))))

;; spacetime/claim-query.lisp:383-388
(defun claim-current-p (claim)
  "True while CLAIM is still believed: its transaction period is open, or
absent -- a claim predating the axis was never retracted.  NIL once
RETRACT-CLAIM has closed the period (GH #162)."
  (let ((e (claim-transaction-extent claim)))
    (or (null e) (bound-unknown-p (extent-end e)))))
```

**Namespaces are keywords.**  Nothing on the class enforces it (no `:type`, no
`:check` on those slots), but every writer and reader assumes it:
`%same-base-tuple-p` compares with `EQ` (`temporal.lisp:50`, `:54`),
`%identity-key-namespace` only accepts and produces keywords
(`claim-query.lisp:78-89`), the source contract requires it
(`spacetime/source.lisp:140`, `:169`), and every test writes `:ns` / `:region` /
`:host`.

### ⚠ `claim-object-key` on a UNARY claim signals `no-applicable-method`

`object-namespace` / `object-key` exist only on `<PARENT>-BINARY`
(`claim.lisp:386`, `+claim-object-slots+` `claim.lisp:314-319`), so the reader
generics have **no method** for a unary instance — the call **errors**, it does
not return `NIL`:

```lisp
;; tests/spacetime/claim-tests.lisp:57-59
        (is-true (slot-exists-p b 'graph-db.spacetime::object-key))
        (is-false (slot-exists-p u 'graph-db.spacetime::object-key))))))
```

**Always gate on `(typep claim (claim-family-binary family))` before touching
either object accessor** — the in-tree guard, `spacetime/register.lisp:184-196`:

```lisp
  "…The TYPEP guard
keeps the parent-class lookup's UNARY hits away from the object
accessors, which only BINARY has."
  (and (typep c binary)
       (equal relation (claim-relation c))
       …
```

Every other accessor lives on PARENT and works on both arities.

## 3.9 `retract-claim` — `spacetime/claim-query.lisp:390` (exported, `package.lisp:61`)

```lisp
(defun retract-claim (claim &key (at (%st-now)))
```

**No transaction argument and none required from the caller** — it joins an
ambient transaction or opens its own (`claim-query.lisp:414-416`):

```lisp
    (cond ((not (claim-current-p claim)) claim)
          (graph-db::*transaction* (%retract))
          (t (graph-db:with-transaction () (%retract))))))
```

Not a deletion: the claim keeps its identity tuple and stays visible to
`claims-touching` and to `index-lookup` unless `:current` / `claim-current-p`
filters it.

## 3.10 `claims-touching` — `spacetime/claim-query.lisp:219` (exported, `package.lisp:53`)

```lisp
(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during
                             relation limit offset as-of)
```

Useful in **tests** (the Task 3 test uses it to find a claim to retract), not in
the functors.  `claim-class` is the **parent** class-name symbol; `namespace` is
a keyword; `:role` ∈ `{:subject :object :either}` (`check-type`,
`claim-query.lisp:264`); `:at` and `:during` are mutually exclusive
(`claim-query.lisp:267-268`); the default **returns retracted claims too**.
**Returns two values**, `(values claims more-p)`.  With `:as-of` the result may
contain `reaped-claim` structs, not claims.

---

# 4. Index lookups available on claim classes

## 4.1 The four indexes — all of them, and the only ones

From `def-claim-classes`, `spacetime/claim.lisp:440-450` (quoted verbatim in
C3).  `+claim-shared-slots+` declares no `:index` slot option on any slot;
`spacetime/` contains no other claim `def-index`.

| slot list | owner class | index name |
|---|---|---|
| `(subject-namespace subject-key)` | **parent** | `claim-subject` |
| `(subject-namespace subject-key relation)` | **parent** | `claim-subject-relation` |
| `(object-namespace object-key)` | **binary** | `claim-object` |
| `(producer)` | **parent** | `claim-producer` |

Each `def-index` is registered **per graph name** (`register-index-spec`,
`index.lisp:150`), so a family declared for graph A has no index in graph B —
exactly the case `%require-index` signals for.

## 4.2 `index-lookup` — `index.lisp:990` — `graph-db:index-lookup` (exported, `package.lisp:449`)

```lisp
(defun index-lookup (graph class-name slot-name value
                     &key (collect-p t) prefix)
  "Nodes of CLASS-NAME (and subclasses) whose indexed slot(s) equal VALUE, via
the secondary index.  VALUE is a scalar for a single-slot index; for a
multi-slot index it is a list of component values, one per SLOT-NAME
position, left-to-right -- the full arity for an exact match, or fewer
components with PREFIX T for a scan of every tuple that starts with them (a
tuple with a null component is still findable this way -- see
%INDEX-TUPLE-KEY).  Signals if no index covers CLASS-NAME.SLOT-NAME.  Resolves
ids in GRAPH.  With COLLECT-P NIL, returns T as soon as one match is found
(GH #107)."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => no matches
      (let ((key (%index-key six value))
            (result '()))
        (when key
          (dolist (id (ix-lookup six key :prefix prefix))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
                (if collect-p (push node result) (return-from index-lookup t))))))
        (when collect-p (nreverse result))))))
```

**Returns one value**: a fresh list of vertex objects (`:collect-p t`, default),
`NIL` for no matches *and* for a declared-but-empty index.

The two in-tree call shapes — `spacetime/claim-query.lisp:275-281` (`want` is
`(list namespace key)`, bound at `:272`):

```lisp
         (subjects (when (member role '(:subject :either))
                     (if relation
                         (graph-db:index-lookup
                          graph (claim-family-parent family)
                          '(subject-namespace subject-key relation)
                          (list namespace key relation))
                         (graph-db:index-lookup
                          graph (claim-family-parent family)
                          '(subject-namespace subject-key) want))))
         (objects (when (member role '(:object :either))
                    (graph-db:index-lookup
                     graph (claim-family-binary family)
                     '(object-namespace object-key) want))))
```

and the arity-1 shape, value a **bare scalar** — `claim-query.lisp:437`:

```lisp
         (all (graph-db:index-lookup graph (claim-family-parent family)
                                     '(producer) producer)))
```

**Traps.**

- **Slot symbols must be `graph-db.spacetime::`-qualified — see C3.**
- Arity 1 takes the value **as-is** (`%index-key`, `index.lisp:418`:
  `(vals (if (= arity 1) (list value) value))`).  Passing `(list producer)` to
  `'(producer)` keys on `(("p"))` and silently returns `NIL`, no error.
- Wrong component count on a multi-slot index **signals**
  `query-precondition-error` unless `:prefix t` (`%index-bounds`,
  `index.lisp:456`, error at `:483`).  More components than the arity signals
  even with `:prefix t`.
- A non-list scalar handed to a multi-slot index is a raw Lisp type error, not a
  typed condition.
- Components compare by `EQUAL` (`%index-equal`, `index.lisp:332`).  A namespace
  stored as `:HOST` is **not** found by `"host"` — convert with `find-symbol` in
  `KEYWORD` first.  These four indexes declare no `:canonicalize`.
- `deleted-p` nodes are dropped; **retracted claims are not** (retraction closes
  the transaction extent, it does not delete).
- Secondary indexes are updated on the **commit apply** path
  (`transactions.lisp:1920`), so inside an open transaction `index-lookup` does
  **not** see that transaction's own uncommitted writes.  `claims-touching`
  compensates with `%overlay-transaction` (`claim-query.lisp:194`); a functor
  that skips the overlay answers from the committed store only.

## 4.3 Missing index — `%require-index` and `query-precondition-error`

```lisp
;; index.lisp:973
(defun %require-index (graph class-name slot-name)
  "The SLOT-INDEX for CLASS-NAME.SLOT-NAME.  Returns NIL when the slot is a declared
index (via :INDEX or DEF-INDEX) but no entries exist yet (a legitimately empty
result); signals only when the slot is not indexed at all (a programming error)."
  (let* ((slot-names (%normalize-slots slot-name))
         (six (%secondary-index-lookup graph class-name slot-names)))
    (cond (six six)
          ((or (%slot-index-declared-p class-name slot-names)
               (%def-index-declared-p graph class-name slot-names))
           nil)                                             ; declared, empty
          ;; A caller's error, typed so a server can tell it from a
          ;; defect (GH #286).
          (t (error 'query-precondition-error
                    :reason (format nil "No secondary index on ~S.~S in ~S"
                                    class-name slot-name
                                    (graph-name graph)))))))
```

```lisp
;; globals.lisp:487
(define-condition query-precondition-error (error)
  ((reason :initarg :reason :reader query-precondition-error-reason))
  (:report (lambda (c s)
             (format s "~A" (query-precondition-error-reason c)))))
```

`graph-db:query-precondition-error` and
`graph-db:query-precondition-error-reason` are **exported**
(`package.lisp:277`), so `(handler-case … (graph-db:query-precondition-error ()
nil))` is the right catch for "this graph does not index that family" in
`%producer-candidates`.

Two traps: `graph-db:query-param-error` (`query/dsl.lisp:30`) is a **subclass**,
so the handler catches DSL param errors too; and the *same* condition is
signalled for a **wrong-arity value** — a handler meaning "no index" will also
swallow "you passed 2 values to a 3-slot index".  Get the arity right.

## 4.4 A parent lookup covers unary and binary — and a subclass lookup over-returns

Write side: `%applicable-index-descriptors` (`index.lisp:213`) matches by
`subtypep`, not `eq`, and the descriptor's **owner** keys the physical index
(`%slot-index-for`, `index.lisp:559`).  Read side resolves a subclass name up
the CPL — `%secondary-index-lookup`, `index.lisp:929`:

```lisp
  (let ((reg (secondary-indexes graph))
        (slot-names (%normalize-slots slot-name)))
    (when reg
      (or (gethash (cons class-name slot-names) reg)
          (let ((class (ignore-errors (find-class class-name nil))))
            (when class
              (loop for c in (class-precedence-list class)
                    for k = (cons (class-name c) slot-names)
                    for hit = (and (typep c 'node-class) (gethash k reg))
                    when hit return hit)))))))
```

⚠ **`index-lookup` applies no `typep` filter against `class-name`.**  So:

- `(index-lookup g <parent> '(…subject-namespace …subject-key) want)` → unary +
  binary.  Correct and intended.
- `(index-lookup g <binary> '(…subject-namespace …subject-key) want)` → **also
  returns unary claims**, because that index is owned by the parent.  Filter
  with `typep` yourself if you want binary only.
- `(index-lookup g <binary> '(…object-namespace …object-key) want)` → exact,
  because that index is genuinely owned by `<binary>`.  This is why
  `claims-touching` passes `claim-family-binary` there and
  `claim-family-parent` everywhere else.

## 4.5 `map-vertices` — `vertex.lisp:185` — `graph-db:map-vertices` (exported, `package.lisp:381`)

```lisp
(defun map-vertices (fn graph &key collect-p vertex-type include-vertex-types
                                exclude-vertex-types include-deleted-p
                                (include-subclasses-p t)
                                (record-reads t))
  "Call FN on vertices of GRAPH.

Narrow the set with :VERTEX-TYPE (a single type name or numeric type-id) and/or
:INCLUDE-VERTEX-TYPES (a list of either) -- their union is visited; with no type
given, EVERY vertex is visited.  :EXCLUDE-VERTEX-TYPES (a list) removes types
from that set.  Unless :INCLUDE-SUBCLASSES-P is NIL (default T) each named type
also matches its subtypes (see RESOLVE-NODE-TYPE-IDS).  Deleted vertices are
skipped unless :INCLUDE-DELETED-P.  With :COLLECT-P, collect and return FN's
values as a list; otherwise return NIL.
```

`fn` first, `graph` **positional second**.  `:vertex-type` takes **one** type
name or numeric id (a list goes in `:include-vertex-types`).
`:include-subclasses-p` defaults to **T**, so naming the claim parent covers
unary and binary — no explicit subclass walk.  In-tree precedent,
`spacetime/temporal.lisp:139-145`:

```lisp
          (graph-db:map-vertices
           (lambda (c)
             (incf checked)
             (when (%live-claim-p c)
               (push c (gethash (%base-tuple-key c binary) groups))))
           graph :vertex-type (claim-family-parent family))
```

**Traps.**

- **`:collect-p t` is mandatory if the nodes escape — see C5.**
- Never call it **untyped**: a fully untyped scan walks the raw lhash and
  bypasses MVCC snapshot isolation (`vertex.lisp:204-209`).  A typed scan is
  snapshot-consistent.
- A subclass is expanded in only if registered as a vertex type in **this**
  graph (`resolve-node-type-ids`, `node-class.lisp:325`); a class from another
  graph silently drops out.
- Parent + subtype in one call double-counts (`vertex.lisp:199-202`).
- Returns `NIL` unless `:collect-p`.
- No cheaper exported route exists.  `graph-db:all-vertices`
  (`algorithms/common.lisp:100`) is a thin wrapper and lives in the optional
  `graph-db/algorithms` system.

---

# 5. The guard: what it admits, what it signals

Package `graph-db.query`, `(:use #:cl)` only.  Its export list —
`query/package.lisp:3`:

```lisp
(defpackage #:graph-db.query
  (:use #:cl)
  (:export
   ;; conditions (spec SS3)
   #:prolog-guard-error #:prolog-guard-error-reason
   #:prolog-ill-typed-error #:prolog-server-fault
   ;; the screen's limits
   #:*prolog-max-query-length* #:*prolog-max-depth*
   ;; schema names, shared with the GUI
   #:schema-type-names
   ;; the runner (spec SS4, GH #322)
   #:run-guarded-prolog))
```

Everything else in `query/guard.lisp` is internal → `graph-db.query::`.

## 5.1 `run-guarded-prolog` — `query/guard.lisp:667`

```lisp
(defun run-guarded-prolog (text graph &key limit max-inferences timeout
                                            (format :data))
  "Screen, read, guard and run TEXT against GRAPH; (VALUES COLUMNS ROWS
TRUNCATED-P).  COLUMNS are the variables in first-appearance order as
camelCase wire spelling; ROWS one list per solution, cells JSON-shaped
under :DATA (a node is its id string; strings, numbers, T, NIL pass) or
as bound under :RAW.  LIMIT is clamped to *QUERY-DEFAULT-LIMIT*;
MAX-INFERENCES and TIMEOUT bind the DSL's budgets for this call.
Refusals signal PROLOG-GUARD-ERROR; see the header for the rest of the
condition contract (spec SS4, GH #322)."
```

- **`text` then `graph`, positional.**
- Returns **`(values columns rows truncated-p)`** — columns FIRST.
- `columns` are **strings**, camelCase with `?` stripped, in order of **first
  appearance in the query text** (`%query-var-field`, `query/dsl.lisp:43`,
  `?item-label` → `"itemLabel"`).
- `rows` under the default `:data` maps each cell through
  `graph-db::%query-value->json` (`query/dsl.lisp:47`): node → **id string**,
  unbound var → `nil`, `t` → `t`, keyword passes, other symbol → its
  `symbol-name` string, scalars pass.  `:raw` returns bound values (nodes as
  objects).
- `*prolog-max-query-length*` (4096, `query/guard.lisp:50`) is **not enforced**
  by `run-guarded-prolog` — only the GUI reads it.

## 5.2 What the guard admits

**Functors: enumerated live at query time; there is no static list to edit.**
`%functor-whitelist` — `query/guard.lisp:302`:

```lisp
(defun %functor-whitelist ()
  "(name-string . arity) -> home package, ENUMERATED from the two live
registries: *PROLOG-GLOBAL-FUNCTORS* (globals.lisp:420, which also
carries the per-schema edge functors) and *USER-FUNCTORS*.

Enumerated, never probed: MAKE-FUNCTOR-SYMBOL interns, so asking the
registry whether a client's NAME/ARITY exists would create it.
Uninterned keys are dropped -- SELECT registers a transient gensym
functor per running query, which is nobody's predicate."
  (let ((table (make-hash-table :test 'equal))
        (engine (find-package :graph-db)))
    (flet ((add (key)
             (when (and (symbolp key) (symbol-package key))
               (multiple-value-bind (name arity) (%split-functor-name key)
                 ;; The exclusions name ENGINE predicates, so they only
                 ;; apply to GRAPH-DB-homed keys. ...
                 (when (and name
                            (not (and (eq (symbol-package key) engine)
                                      (%excluded-predicate-p name))))
                   (setf (gethash (cons name arity) table)
                         (symbol-package key)))))))
      (maphash (lambda (k v) (declare (ignore v)) (add k))
               graph-db::*prolog-global-functors*)
      (maphash (lambda (k v) (declare (ignore v)) (add k))
               graph-db::*user-functors*))
    table))
```

A fresh `guard-ctx` per call (`%guard-context`, `query/guard.lisp:387`), so a
functor registered at load of `graph-db/rules` is admitted by the very next
`run-guarded-prolog` call.  **No edit to `query/guard.lisp` is needed to admit
`claim/7` and friends.**  The three hand-maintained lists (`query/guard.lisp:245`,
`:260`, `:270`) can only *subtract*, and none of `CLAIM`, `CLAIM-PRODUCER`,
`CLAIM-CURRENT`, `CLAIM-VALID-AT`, `CLAIM-STANDING`, `CLAIM-RELATION`,
`CLAIM-RULE-VERSION` collides with them.

**Arity is checked, and it is the hash key** — `%guard-goal`,
`query/guard.lisp:451`, the load-bearing lines:

```lisp
        (let* ((home (gethash (cons name arity) (gc-functors ctx)))
               (canonical (or (gethash name (gc-control ctx))
                              ;; Bounded: NAME came out of the registry
                              ;; by string match, so this interns only
                              ;; names the image already registered.
                              (and home (intern name home)))))
          (unless canonical
            (%refuse "~A/~D is not a registered Prolog functor"
                     (string-downcase name) arity))
```

`arity` is `(1- (list-length form))`.  `CLAIM/7` called with 6 or 8 arguments
gives `prolog-guard-error "claim/6 is not a registered Prolog functor"`.
`(and home (intern name home))` is what makes the raw-vs-guarded asymmetry of C1
harmless here.

**Bare symbols are schema-resolved and arrive as SYMBOLS** — `%guard-symbol`,
`query/guard.lisp:427`:

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

`%schema-name-table` (`query/guard.lisp:363`) holds every vertex/edge **type
name** and every **declared slot** of the graph, so `rt-claim`,
`rt-claim-unary`, `rt-claim-binary`, `rtt-claim…`, and every claim slot name
(`producer`, `subject-namespace`, `relation`, `standing`, …) are admissible
bare.  The symbol handed to the functor is the schema's own class symbol —
**`eq` to the key of `*claim-families*`**, so `(claim-family v)` hits.

⚠ A bare symbol that is **not** a schema type/slot of this graph is a
`prolog-guard-error`, not a runtime ill-typed error — **this is C6a**.

**Literals.** Strings, numbers and characters pass through verbatim
(`%guard-term`, `query/guard.lisp:497`), so `"host"`, `"h1"`,
`"2026-02-15T00:00:00Z"` are fine.  **A list argument is impossible**: every
cons is walked as a goal.

**A query binding no `?variable` is refused** (`%guard-query`,
`query/guard.lisp:507`).

## 5.3 What the guard signals

**A colon anywhere outside a string or `|…|` is refused before `READ` runs** —
`%scan-query-text`, `query/guard.lisp:125`, arm at `:158`:

```lisp
          ((char= ch #\:)
           (%refuse "package-qualified name ~S is not permitted: a ~
query may name only this graph's schema and the registered Prolog ~
functors" (%token-around text i)))
```

```lisp
;; query/guard.lisp:86
(defun %refuse (format-string &rest args)
  (error 'prolog-guard-error
         :reason (apply #'format nil format-string args)))
```

So `:host` in query text is `prolog-guard-error`.  This is why namespaces cross
the functor boundary as **strings**.

The three conditions — `query/guard.lisp:57`, `:64`, `:75`, all **exported**:

```lisp
(define-condition prolog-guard-error (error)
  ((reason :initarg :reason :reader prolog-guard-error-reason))
  (:report (lambda (c s)
             (format s "~A" (prolog-guard-error-reason c))))
  (:documentation "A free-text query the guard refused.  Carries the
client-facing reason, which names the offending token."))

(define-condition prolog-ill-typed-error (error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "ill-typed query")))
  (:documentation "..."))

(define-condition prolog-server-fault (error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "internal error")))
  (:documentation "..."))
```

`prolog-ill-typed-error` and `prolog-server-fault` have **no slots and no
initargs**.

The runtime split — `%run-guarded-goals`, `query/guard.lisp:645`:

```lisp
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
```

| cause | signalled |
|---|---|
| any refusal by the screen/read/guard | `prolog-guard-error` |
| `graph-db:prolog-error` + subclasses (`prolog-throw`, `prolog-resource-error`, `prolog-cost-unbounded-error`) | re-signalled unchanged |
| `graph-db:query-param-error` | re-signalled unchanged |
| `graph-db:query-precondition-error`, or SBCL `SB-PCL::NO-APPLICABLE-METHOD-ERROR` | `prolog-ill-typed-error` |
| anything else at runtime | `prolog-server-fault` |

`unknown-claim-family` currently falls in the last row — **that is what C6b
fixes.**

## 5.4 A guarded query is ALWAYS resource-bounded

`run-guarded-prolog` binds both budgets (`query/guard.lisp:681-684`) and
`graph-db::run-query-goals` hard-codes them into the `SELECT` it EVALs —
`query/dsl.lisp:339-346`:

```lisp
                  (eval `(select (:effects nil :snapshot t
                                  :limit ,cap
                                  :skip ,(when (integerp skip) skip)
                                  :max-inferences
                                  ,*query-default-max-inferences*
                                  :timeout ,*query-default-timeout*
                                  :callback *pattern-query-callback*)
                                 ,vars ,@goals))
```

Defaults 1000000 and 30 (`query/dsl.lisp:21`, `:23`).  So
`graph-db::*inference-budget*` and `graph-db::*query-deadline*` are **never
NIL inside a guarded query**, and `%unbound-claim-scan`'s walk branch is
**unreachable through `run-guarded-prolog`** — a fully unbound
`(claim ?c rt-claim ?a ?b ?r ?d ?e)` always refuses there.  Only a raw in-image
`select` without `:max-inferences`/`:timeout` reaches the walk.

---

# 6. The test suite / fixture template

## 6.1 Test package — model: `tests/query/package.lisp` (whole file)

```lisp
;;;; tests/query/package.lisp -- graph-db/query-test (GH #322).

(defpackage #:graph-db/query-test
  (:use #:cl #:fiveam)
  (:import-from #:graph-db #:def-vertex #:def-edge #:make-graph
                #:close-graph #:with-transaction #:string-id)
  (:export #:run-query-tests #:query-suite))
```

`:use #:graph-db` is deliberately avoided (huge, collides); `:use
#:graph-db.spacetime` **is** safe and is what `tests/spacetime/package.lisp:9`
does, with the header saying why.  From `GRAPH-DB` the spacetime suite imports
exactly `serialize`, `deserialize`, `make-graph`, `close-graph`,
`with-transaction`, `open-graph`, `id`, `lookup-vertex`, `def-vertex`,
`geometry`, `make-point`, `make-polygon`, `make-linestring`; `*graph*` and
`*schema-node-metadata*` stay package-qualified at the call sites.

A rules suite will also want, all exported from `graph-db` (single colon,
`package.lisp` line in brackets): `select` [482], `select-count` [523],
`select-one` [527], `select-flat` [528], `select-first` [529], `copy` [386],
`save` [387], `unique-constraint-violation` [391],
`value-constraint-violation` [409], `index-lookup` [449], `lookup-graph` [198].
Internal (`::`): `transaction-manager`, `*schema-node-metadata*`, `node-p`.
Exported but written `graph-db::` by every existing suite: `*system-directory*`
[126], `*type-registry*` [127].

**Plus, per C1(b), the head symbols or `/ARITY` symbols from
`#:graph-db.rules`** if `rules/facts.lisp` stays in `graph-db.rules`.

## 6.2 Suite + fixture — model: `tests/query/suite.lisp` (whole file)

```lisp
;;;; tests/query/suite.lisp -- runner + fixture for graph-db/query
;;;; (GH #322).

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
  `(let* ((dir (graph-db-test-scratch:make-scratch-directory
               "graph-db-query"))
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

Each detail is load-bearing:

| detail | why |
|---|---|
| `(log:config :error)` first line of the runner | log4cl is a `graph-db/core` dependency; without it the run is unreadable. |
| one scratch dir per RUN for `*system-directory*` | type-ids come from the image-wide registry; `MAKE-GRAPH` signals `SYSTEM-DIRECTORY-REQUIRED` when it is NIL (`globals.lisp:79-85`). |
| `graph-db::*type-registry*` forced to `NIL` in the same `let*` | rebinding the directory alone keeps a registry opened from the *old* directory (`globals.lisp:87-90`). |
| `unwind-protect` → `(run 'suite)` / `(explain! results)` / `(results-status results)` | `results-status` is the boolean the `:perform` clause tests. |
| `(graph-db-test-scratch:cleanup-scratch-run)` in the cleanup form | drops the whole per-run parent, `system-dir` included. |
| `(defparameter *graph-name* …)` **and** the literal keyword in the schema forms | `def-vertex` / `def-claim-classes` take the graph name **unevaluated**; `make-graph` **does** evaluate it. |
| the `eval-when` clearing `*schema-node-metadata*` before the schema forms | the one-file-owns-one-graph-name idiom; without it `%warn-if-cross-file-clobber` (`schema.lisp:567`) cannot see a two-files-one-name clobber.  `*schema-node-metadata*` is internal → `graph-db::`. |
| fixture binds `graph-db:*graph*` around the body | exported (`package.lisp:243`). |
| `(ignore-errors (close-graph ,g))` | `close-graph` "Must be called with `*GRAPH*` bound to GRAPH" (`graph.lisp:1226`); the fixtures call it *outside* that binding, so the snapshot may signal.  Keep the `ignore-errors`. |
| `graph-db::transaction-manager` | **not** exported. |
| `:graph g` on every constructor | satisfies `%default-store-graph` explicitly (§3.2). |

For a claim suite with dozens of graphs, add `collect-garbage` after
`close-graph` as the spacetime fixture does — mmap'd regions are released only
when finalizers run.  `tests/spacetime/claim-tests.lisp:16-24`:

```lisp
(defmacro with-claim-graph ((g) &body body)
  "A fresh on-disk graph named *CLAIM-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *claim-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))
```

```lisp
;; tests/spacetime/suite.lisp:46-62
(defun collect-garbage ()
  "Force a full GC between graph-backed tests (mirrors GRAPH-DB/TEST)."
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl (ext:gc t))
```

## 6.3 `graph-db-test-scratch` — `tests/scratch.lisp`

```lisp
;; tests/scratch.lisp:10-20
(defpackage #:graph-db-test-scratch
  (:use #:cl)
  (:export #:scratch-tag
           #:scratch-run-directory
           #:make-scratch-directory
           #:make-scratch-file-name
           #:cleanup-scratch-run
           #:sweep-stale-scratch
           #:*scratch-prefixes*))
```

```lisp
;; tests/scratch.lisp:127-134
(defun make-scratch-directory (&optional (prefix "graph-db-test"))
  "Create and return a fresh scratch directory PREFIX-<tag>/ under the
per-run parent.  Callers still delete it promptly; the parent catches
whatever an aborted run leaves behind."
  (let ((dir (merge-pathnames (format nil "~A-~A/" prefix (scratch-tag))
                              (scratch-run-directory))))
    (ensure-directories-exist dir)
    dir))

;; tests/scratch.lisp:142-150
(defun cleanup-scratch-run ()
  "Delete this image's scratch parent (if any) and forget it, so a later
run in the same image gets a fresh one.  Safe to call when none exists.
Trap: everything under the parent dies -- close mmaps first."
  (let ((dir *scratch-run-directory*))
    (setf *scratch-run-directory* nil)
    (when dir
      (uiop:delete-directory-tree dir :validate t
                                      :if-does-not-exist :ignore))))
```

Package name is **`GRAPH-DB-TEST-SCRATCH`** (hyphens, not `graph-db/…`); both
helpers are exported → single colon.  `make-scratch-directory` returns a
**directory pathname** (already created); `make-graph` wants a string, so
`(namestring dir)`.  PREFIX is optional and **positional**.
`*scratch-prefixes*` needs **no** edit for a new suite — the run parent
`graph-db-test-run-<tag>/` is already covered.

## 6.4 `make-graph` / `close-graph`

```lisp
;; graph.lisp:601-619
(defun make-graph (name location &key master-p slave-p master-host
                                   replication-port replication-key package
                                   replay-txn-dir (buffer-pool-p t)
                                   (buffer-pool-size 100000)
                                   ...
```

`NAME` and `LOCATION` positional; `LOCATION` a directory namestring; `NAME`
**evaluated**, and must equal the graph name in the `def-claim-classes` forms.
Every spacetime/query fixture passes exactly `:buffer-pool-size 1000`.

```lisp
;; graph.lisp:1222
(defmethod close-graph ((graph graph) &key (snapshot-p t))
```

## 6.5 `with-transaction` — `transactions.lisp:421`

```lisp
(defmacro with-transaction ((&rest spec) &body body)
  "Run BODY as a single ACID transaction and return BODY's value.  Three
forms of SPEC:

  (with-transaction () ...)              the current *GRAPH*'s manager
  (with-transaction (TM) ...)            an explicit transaction manager
  (with-transaction (:graph G) ...)      G's manager, with *GRAPH* bound
                                         to G for BODY (GH #175)
...
All mutations -- MAKE-<type> constructors, SAVE, DELETE-NODE/MARK-DELETED --
must run inside a transaction. ... To modify an existing node, COPY it
inside the transaction, mutate the copy, then SAVE it."
```

Three legal shapes only; anything else is a macroexpansion `error`
(`transactions.lisp:456-457`).  **Every claim construction must be inside one.**

## 6.6 Where the claim families go

**At load time, at toplevel, in an ordinary test file** — not inside the fixture
and not inside `def-suite`.  Real examples:

```lisp
;; tests/spacetime/claim-tests.lisp:7-14
(defparameter *claim-graph-name* :graph-db-claim-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *claim-graph-name* graph-db::*schema-node-metadata*) nil))

(def-claim-classes ct-claim :graph-db-claim-test
  :extra-slots ((weight :initarg :weight :accessor ct-weight
                        :initform nil)))

;; tests/spacetime/temporal-tests.lisp:10-11
;; A second family on the claim test graph, this one temporal.
(def-claim-classes tt-claim :graph-db-claim-test :temporal t)
```

Two families on one graph, declared in different files, is a supported and
exercised shape — which is what `rt-claim` + `rtt-claim` needs.

## 6.7 Constructor call sites, verbatim

```lisp
;; tests/spacetime/claim-identity-tests.lisp:7-25
(defun make-b (&key (producer "rule-a") (subject "s1") (object "o1")
                    (relation "r") (standing :inferred) extent rule-version)
  (make-ct-claim-binary :subject-namespace :ns :subject-key subject
                        :relation relation
                        :object-namespace :ns :object-key object
                        :producer producer :standing standing
                        :extent extent :rule-version rule-version))

(defun make-u (&key (producer "rule-a") (subject "s1") (relation "r") extent)
  (make-ct-claim-unary :subject-namespace :ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred :extent extent))
```

```lisp
;; tests/spacetime/temporal-tests.lisp:13-21
(defun %tt-run (subject state from to &key (producer "series")
                                          (relation "in-state"))
  "SUBJECT was in STATE from FROM to TO (exact bounds), as a binary claim."
  (make-tt-claim-binary :subject-namespace :region :subject-key subject
                        :relation relation
                        :object-namespace :state :object-key state
                        :producer producer :standing :observed
                        :extent (exact-interval from to)))
```

```lisp
;; tests/spacetime/claim-identity-tests.lisp:49-53
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-b)))))
```

## 6.8 The defsystems

```lisp
;; graph-db.asd:205-219
(defsystem graph-db/query-test
  :name "VivaceGraph guarded-query test suite"
  :description "FiveAM tests driving GRAPH-DB.QUERY:RUN-GUARDED-PROLOG
directly, web-free (GH #322)."
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "4.0.1"
  :depends-on (:graph-db/query :graph-db/test-scratch :fiveam)
  :pathname "tests/query/"
  :serial t
  :components ((:file "package") (:file "suite") (:file "guard-tests"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :graph-db/query-test
                                       :run-query-tests)
               (error "graph-db query tests failed."))))
```

with the back-link on the producing system, `graph-db.asd:203`:

```lisp
  :in-order-to ((test-op (test-op :graph-db/query-test))))
```

`:perform (test-op (op c) …)` with `uiop:symbol-call` and a bare `(error …)` on
falsehood is the **only** shape that actually runs anything — a `test-op` with
no `:perform` on the test system is a silent no-op.  **Always invoke
`(asdf:test-system :graph-db/rules-test)`, never `:graph-db/rules`.**

`graph-db/test-scratch` — `graph-db.asd:297-301`:

```lisp
(defsystem graph-db/test-scratch
  :name "VivaceGraph shared test scratch-space manager"
  :description "Per-run scratch parent + stale sweep for all suites (GH #214)."
  :pathname "tests/"
  :components ((:file "scratch")))
```

There is **no `graph-db/rules` system in `graph-db.asd`** — it must be added.

## 6.9 `select` and friends — the shapes to write in tests

`(defmacro select (options vars &rest goals)` — see **C2**.  Empty options is
`()`.

**The ten option keywords, exactly** (`prologc.lisp:1063-1095`): `:flat`
`:limit` `:skip` `:max-inferences` `:timeout` `:effects` `:count` `:callback`
`:snapshot` `:allow-cost-unbounded`.

- **An unrecognised key is silently ignored** — a typo (`:max-inference`) gives
  an *unbounded* query with no error.
- Option values are spliced **unevaluated** into a `let*` init form, so they are
  evaluated at runtime in the caller's scope.  Exception: `:effects`, which is
  **quoted** — write `(:effects nil)`, never a form.

Return values (`prologc.lisp:1128-1130`):

```lisp
       (if (or *select-count-only* *select-callback*)
           *select-current-count*
           (nreverse *select-list*)))))
```

- default: list of rows, one list per solution, one value per var in `vars`
  order, in solution order.
- `:flat t` → flat list of values (**every** element of a multi-var row, so use
  one var).
- `:count t` or `:callback` → an **integer**.
- `vars` = `()` with no `:count` → always `NIL`.

The shorthands — `prologc.lisp:1132-1157`, all `(vars &rest goals)`:

```lisp
(defmacro select-flat (vars &rest goals)
  "Like SELECT with :FLAT t: return a flat list of values rather than a list of
tuples.  Most convenient with a single result variable."
  `(select (:flat t) ,vars ,@goals))

(defmacro select-count (vars &rest goals)
  "Run the query GOALS and return the NUMBER of solutions as an integer, without
projecting or consing any bindings.  VARS may be () when only the count matters.
For a capped or offset count use the SELECT (:count t :limit N :skip M ...) form;
SELECT-COUNT itself counts every solution."
  `(select (:count t) ,vars ,@goals))

(defmacro select-first (vars &rest goals)
  "Return only the first solution's tuple for VARS (cuts after the first
match)."
  `(first (select () ,vars ,@goals !)))

(defmacro select-one (vars &rest goals)
  "Return the first value of the first result variable from the first solution
-- ideal for single-result lookups (e.g. find one node by a property)."
  `(first (select (:flat t :limit 1) ,vars ,@goals !)))

(defmacro do-query (&rest goals)
  "Run GOALS purely for their side effects, collecting nothing.  Useful with
goals like (trigger ...) or (retract ...)."
  `(select () () ,@goals))
```

`select-count` takes **no** `:limit` / `:max-inferences`; for a bounded count
expand to `(select (:count t :max-inferences N) vars goals…)` by hand.  Since
`*default-inference-budget*` and `*default-query-timeout*` are both `NIL`, a
bare `select-count` runs with **no bound in effect** — which is what makes the
Task 2 `(= 4 (select-count …))` walk assertion reachable.

## 6.10 The GUI tripwire

`tests/gui/gui-tests.lisp:1497` (`prolog-functor-inventory-is-pinned`) compares
the live registry against the hand-listed `*reviewed-functor-inventory*`
(`:1345`).  **If `graph-db/rules` is ever loaded into an image that also runs
`graph-db/gui-test`, that test fails** with a message telling you to classify
each new functor.  `graph-db/gui-test` does not depend on `graph-db/rules`, so a
rules-only or query-only run is unaffected.

---

# 7. Temporal extents

Home package `TEMPORAL-EXTENT`, `:use`d by `graph-db.spacetime` and
**re-exported** from it, so a test package that `:use`s `#:graph-db.spacetime`
gets them unqualified and needs no extra `:depends-on`.  Export lines:
`exact-bound` `spacetime/package.lisp:23`, `make-instant`/`make-interval` `:28`,
`extents-disjoint-p`/`extents-intersect-p` `:76`, `+standings+`/`check-standing`
`:20-21`.  **See C4** — the plan's `graph-db.rules` package imports none of
them.

## 7.1 Constructors

```lisp
;; ~/work/cl-temporal-extent/src/bound.lisp:17-36
(defun make-bound (earliest latest)
  "The range [EARLIEST, LATEST], each a TIMESTAMP or :UNBOUNDED.  Signals
INVALID-BOUND on a non-endpoint or a reversed range."

(defun exact-bound (timestamp)
  "A bound pinning exactly one timestamp."
  (make-bound timestamp timestamp))

(defun unknown-bound ()
  "A bound spanning all of time -- \"we have no idea when\"."
  (%make-bound :unbounded :unbounded))
```

```lisp
;; ~/work/cl-temporal-extent/src/extent.lisp:36-52
(defun make-interval (start end &key (precision :nsec) (semantics :event)
                                     (standing :observed))
  "An extent spanning [START, END], both BOUNDs, whose endpoints move
independently.  Intervals are closed (design §3.2).  Signals INVALID-EXTENT
when START and END compare := -- that is a point in time; use MAKE-INSTANT
-- or :> -- END precedes START, a reversed and incoherent extent."
  (ecase (bound-compare start end)
    (:=
     (error 'invalid-extent
            :reason
            "START = END exactly -- a point in time; use MAKE-INSTANT"))
    (:>
     (error 'invalid-extent :reason "END precedes START"))
    ((:< :ambiguous) nil))
  (%make-extent :interval start end
                (%check-precision precision) semantics
                (check-standing standing)))

;; ~/work/cl-temporal-extent/src/extent.lisp:54-60
(defun make-instant (bound &key (precision :nsec) (semantics :event)
                                (standing :observed))
  "A degenerate extent: one timestamp, positioned somewhere in BOUND.  START
and END share the bound, so the two endpoints cannot move apart."
```

**`make-interval` and `make-instant` take BOUNDs, not timestamps.**  A
one-instant run must use `(make-instant (exact-bound ts))` — `make-interval`
with `START = END` signals `invalid-extent`.  `:precision` **is** validated
against `+precisions+` = `'(:year :month :day :hour :minute :second :nsec)`;
`:semantics` is **not** validated (values in use: `:event` the default,
`:validity`, `:transaction`).

## 7.2 Comparison

```lisp
;; ~/work/cl-temporal-extent/src/allen.lisp:182-199
(defun extents-disjoint-p (a b)
  "True when extents A and B CERTAINLY share no instant: every relation
possible between them is :BEFORE or :AFTER.  :MEETS is not disjoint --
intervals are closed, so meeting extents share their boundary instant --
and an ambiguous pair is not disjoint either.  Both arguments must be
extents; a caller's NIL convention is the caller's."
  (check-type a temporal-extent)
  (check-type b temporal-extent)
  (and (every (lambda (r) (member r '(:before :after)))
              (temporal-relation-relations (allen-relations a b)))
       t))

(defun extents-intersect-p (a b)
  "True when extents A and B POSSIBLY share an instant -- the negation of
EXTENTS-DISJOINT-P over two extents.  A pair that might overlap
intersects here and is not disjoint there: the one is a possibility, the
other a certainty."
  (not (extents-disjoint-p a b)))
```

⚠ **`check-type` on both arguments** — the `NIL` that `claim-extent` returns for
an extent-less claim signals a `type-error`, it does not answer.  Guard exactly
as the source does — `spacetime/claim-query.lisp:6-10`:

```lisp
(defun %claim-validity-touches-p (claim probe)
  "True when CLAIM's extent possibly shares an instant with PROBE.  A
claim with no extent makes no validity statement and never matches."
  (let ((e (claim-extent claim)))
    (and e (not (extents-disjoint-p e probe)))))
```

## 7.3 Standing

```lisp
;; ~/work/cl-temporal-extent/src/standing.lisp:6-11
(defparameter +standings+
  '(:observed :inferred :asserted
    :searched-empty :determined-empty :uncovered :indeterminate)
  "The closed standing vocabulary.  Deliberately UNORDERED: ASSERTED and
INFERRED cannot be ranked, so no comparison operator over standings exists
in this subsystem (design §4.4).")

;; :23-25
(deftype standing ()
  '(member :observed :inferred :asserted
    :searched-empty :determined-empty :uncovered :indeterminate))

;; :27-29
(defun standingp (x)
  "True when X belongs to the standing vocabulary."
  (and (member x +standings+) t))

;; :40-44
(defun check-standing (x)
  "Return X when it is a standing; signal INVALID-STANDING otherwise."
  (unless (standingp x)
    (error 'invalid-standing :value x))
  x)
```

Also exported: `+absence-standings+`, `standing-absence-p`,
`standing-present-p`.  Three live checks: the `deftype`; `check-standing` in the
constructor wrapper (`claim.lisp:470`), so an omitted `:standing` signals
`invalid-standing`, **not** `missing-claim-identity-component`; and the
commit-time `standing-vocabulary` value constraint (`claim.lisp:396-399`).

## 7.4 Test helpers for timestamps

```lisp
;; tests/spacetime/suite.lisp:26-36
(defun exact-interval (s e)
  "An interval extent with exact endpoints.  Three lines, duplicated from
cl-temporal-extent's own suite rather than shared: coupling two test suites
so one can borrow a fixture costs more than the duplication (#159)."
  (make-interval (exact-bound s) (exact-bound e)))

(defun ts (year month day &optional (hour 0) (minute 0) (sec 0) (nsec 0))
  "A UTC timestamp.  Every test builds times through this, so none of them
can accidentally depend on the host timezone (design §3.5)."
  (encode-timestamp nsec sec minute hour day month year
                    :timezone +utc-zone+))
```

`local-time:parse-timestring` **is** used in the tree
(`tests/spacetime/claim-transaction-tests.lisp:131-132`,
`tests/serialize-tests.lisp:268`) and parses ISO-8601; a malformed string
signals, so wrap it in `ignore-errors` when the string came from a query.
`:local-time` is a declared dependency of both `graph-db/core`
(`graph-db.asd:30`) and `graph-db/spacetime` (`:532-533`), so it is available
transitively; `LOCAL-TIME` symbols are **not** re-exported by
`GRAPH-DB.SPACETIME` — use the `local-time:` prefix or `:import-from`.

---

# STILL UNKNOWN

**NOT FOUND (the symbol does not exist; what exists instead):**

- `claim-family-temporal` — the reader is **`claim-family-temporal-p`**
  (`spacetime/claim.lisp:24`, exported `spacetime/package.lisp:74`).
- `make-claim-family` / `copy-claim-family` — the `defstruct` names
  `%make-claim-family` as its only constructor and sets `(:copier nil)`
  (`spacetime/claim.lisp:14-16`).
- `unknown-claim-family-name` — the reader is `unknown-claim-family-parent`
  (`spacetime/claim.lisp:9`), and it is **not exported**; the only initarg is
  `:parent`.
- `MAKE-<PARENT>` for a claim family — deliberately `fmakunbound`
  (`spacetime/claim.lisp:451`).  Use `MAKE-<PARENT>-UNARY` / `-BINARY`.
- `claim-family-p` exists (defstruct predicate) but is **not exported**.
- `graph-db:var-p`, `graph-db:node-p`, `graph-db:vertex-p`,
  `graph-db:bound-p`, `graph-db:set-binding`,
  `graph-db:prolog-cost-unbounded-functor`, `graph-db:*query-deadline*`,
  `graph-db:*inference-count*`, `graph-db:transaction-manager`,
  `graph-db:*schema-node-metadata*` — none are exported; all need `::`.
- A `graph-db/rules` system in `graph-db.asd` — **absent**; `rg` for "rules" in
  the `.asd` returns nothing.  It must be added.

**Unsettled — a decision or a construction the implementer must make:**

- **Which C1 fix to take** (`(in-package #:graph-db)` in `rules/facts.lisp` vs
  keeping `graph-db.rules` and importing head or `/ARITY` symbols).  Both are
  verified to work; no recon pass chose one.  Whichever is chosen must be
  applied consistently to `rules/package.lisp`, `rules/facts.lisp` and
  `tests/rules/package.lisp`, and recorded in the task report.
- **The deferred `unknown-claim-family` lookup for C6b has no in-tree
  precedent.**  Every existing `find-symbol` class lookup in `query/guard.lisp`
  is load-time.  The call-time memoised form is new code.
- **Whether `%producer-candidates` should overlay the open transaction.**
  `index-lookup` answers from the committed store only
  (`transactions.lisp:1920`); `claims-touching` compensates with
  `%overlay-transaction` (`spacetime/claim-query.lisp:194`), which is internal.
  No recon pass settled whether the rules functors need parity with
  `claims-touching` here.  If the seed data is committed before the query (the
  fixture's shape), it does not matter for S1's tests.
- **Whether `graph-db/rules` will ever be co-loaded with `graph-db/gui-test`**
  in CI (§6.10).  The tripwire is real; the coupling was not checked against
  `.github/workflows/test.yml`.
- **The `select/2` symbol→string projection** (§2.4) was verified for
  `GRAPH-DB`-homed symbols only.  No pass tested what a `GRAPH-DB.RULES`- or
  schema-homed symbol looks like coming back from a raw `select`; the code path
  says it passes through unchanged.

---

# APPENDIX — symbol qualification, for a caller in `GRAPH-DB.RULES`

| symbol | home | exported? | write as |
|---|---|---|---|
| `def-global-prolog-functor` | `graph-db` | yes (`package.lisp:475`) | `graph-db:def-global-prolog-functor` |
| `*prolog-global-functors*` | `graph-db` | yes (:479) | single colon |
| `unify`, `undo-bindings`, `var-deref`, `*trail*`, `*graph*`, `make-functor-symbol` | `graph-db` | yes (:481, :492, :491, :496, :243, :495) | single colon |
| `var-p`, `bound-p`, `node-p`, `vertex-p`, `set-binding` | `graph-db` | **no** | `graph-db::` |
| `*inference-budget*`, `*default-inference-budget*`, `*default-query-timeout*` | `graph-db` | yes (:511, :512, :513) | single colon |
| `*query-deadline*`, `*inference-count*` | `graph-db` | **no** | `graph-db::` |
| `prolog-error`, `prolog-resource-error`, `prolog-cost-unbounded-error` | `graph-db` | yes (:503, :506, :509) | single colon |
| `prolog-cost-unbounded-functor` (reader) | `graph-db` | **no** | `graph-db::` |
| `declare-functor-cost-unbounded`, `cost-unbounded-predicate-names` | `graph-db` | yes (:508) | single colon |
| `index-lookup`, `index-range`, `map-index`, `def-index` | `graph-db` | yes (:449, :398) | single colon |
| `map-vertices` | `graph-db` | yes (:381) | single colon |
| `query-precondition-error`, `-reason`, `query-param-error` | `graph-db` | yes (:277, :275) | single colon |
| `select`, `select-flat`, `select-count`, `select-one`, `select-first` | `graph-db` | yes (:482, :528, :523, :527, :529) | single colon |
| `make-graph`, `close-graph`, `open-graph`, `lookup-graph`, `with-transaction`, `copy`, `save`, `id`, `deleted-p`, `lookup-vertex` | `graph-db` | yes | single colon |
| `%require-index`, `run-query-goals`, `*user-functors*`, `*schema-node-metadata*`, `*transaction*`, `transaction-manager`, `ensure-node-bytes`, `lookup-node-type-by-name`\* | `graph-db` | **no** (\*`lookup-node-type-by-name` is exported, :301) | `graph-db::` |
| `*system-directory*`, `*type-registry*` | `graph-db` | yes (:126, :127) | every suite writes `graph-db::` — copy that |
| `def-claim-classes`, `claim-family`, `claim-family-parent/-unary/-binary/-temporal-p`, `unknown-claim-family`, `claim-extent`, `claim-current-p`, `retract-claim`, `claims-touching`, and every `claim-*` accessor | `graph-db.spacetime` | yes (`spacetime/package.lisp:44-74`) | single colon |
| `*claim-families*` | `graph-db.spacetime` | **no** | `graph-db.spacetime::` |
| the **slot** symbols `subject-namespace`, `subject-key`, `relation`, `producer`, `object-namespace`, `object-key` | `graph-db.spacetime` | **no** | `graph-db.spacetime::` — **C3** |
| `unknown-claim-family-parent` (reader) | `graph-db.spacetime` | **no** | `graph-db.spacetime::` |
| `exact-bound`, `unknown-bound`, `make-bound`, `make-interval`, `make-instant`, `extents-disjoint-p`, `extents-intersect-p`, `+standings+`, `check-standing`, `standingp`, `extent-start`, `extent-end` | `temporal-extent`, re-exported | yes (`spacetime/package.lisp:20-43`, :76) | `graph-db.spacetime:` — **C4** |
| `run-guarded-prolog`, `schema-type-names`, `prolog-guard-error`, `-reason`, `prolog-ill-typed-error`, `prolog-server-fault` | `graph-db.query` | yes (`query/package.lisp:7-14`) | single colon |
| `%ill-typed-condition-p`, `*no-applicable-method-type*`, `%functor-whitelist`, `*prolog-excluded-predicates*` | `graph-db.query` | **no** | `graph-db.query::` |
| `make-scratch-directory`, `cleanup-scratch-run` | `graph-db-test-scratch` | yes (`tests/scratch.lisp:10-20`) | `graph-db-test-scratch:` |
