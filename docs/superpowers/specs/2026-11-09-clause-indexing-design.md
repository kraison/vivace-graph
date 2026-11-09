# Clause indexing for asserted clauses: design

**Issue:** GH #121, unit 2 of the Prolog Engine 5.0 scope
(`2026-11-09-prolog-engine-5.0-scope.md`). **Status:** proposed
2026-11-09; the calls are the author's, Kevin's review pending.

## Intent

Asserting a clause with `<-` today recompiles every clause of the
functor into one Lisp function (`compile-functor`, prologc.lisp:749),
and a call tries every clause in order whatever its first argument is.
The unit 1b bench measured the cost: 500 asserts of one functor at 16
per second (31 s), each `<-` compiling all the clauses so far. This
unit makes an assert compile one clause and a call with a bound first
argument reach only the clauses that can match, with the cut, the
control constructs, the top-level query functors and every caller of
the functor registry behaving as they do now.

## Goals

- `<-` compiles the new clause and nothing else: assert cost is
  independent of the clause count.
- A call whose first argument is bound tries only the clauses whose
  first head argument can unify with it; an unbound first argument
  tries every clause, in source order, as today.
- Solution order is unchanged for every call: clause order is source
  order within the candidates.
- The cut inside an asserted clause means exactly what it means today:
  commit to this clause, discard the clause's earlier choice points and
  the remaining clauses.
- `make-functor :clauses`, `add-functor-clause`, `functor-clauses`,
  `reset-functor`, `delete-functor`, `set-functor-fn`, `get-functor-fn`
  keep their contracts; `tests/concurrency/prolog-tests.lisp` passes
  unchanged.

## Non-goals

- Indexing on any argument but the first, or inside a compound first
  argument (all conses share one bucket).
- Tabling, the trampoline, `explain` (units 3 and 4).
- Changing how a top-level `select` or `?-` compiles its goals: those
  build a clause-less functor and set its function directly, and stay
  as they are.
- Persistent (on-disk) clauses.

## Today's mechanics, for the record

- `add-clause` (prologc.lisp:687) finds or makes the `functor` struct
  (functor.lisp: `name fn clauses lock`) and `add-functor-clause`
  appends the clause under the functor's recursive lock and calls
  `prolog-compile`, which is `compile-functor`: one `(lambda (args...
  cont) (block NAME clause1 (undo-bindings) clause2 ...))`, `eval`ed,
  stored with `set-functor-fn`. The functor's name is `NAME/ARITY`, so
  every clause of a functor has one arity and `prolog-compile-help`'s
  per-arity loop is vestigial.
- `compile-clause` (prologc.lisp:672) turns the head into `=` goals on
  the parameters and compiles the body through `compile-body`, where a
  cut is `(return-from ,*functor* nil)`; `*functor*` is the block name,
  rebound to a barrier gensym by the `not`, `once` and `if` compiler
  macros, and restored by `%commit` for the goals after a committed
  one.
- `compile-call` (prologc.lisp:245) fetches `(get-functor-fn 'NAME/N)`
  at each call and funcalls it with the arguments and the continuation;
  `%solve` does the same for a dynamic meta-call.
- `unify` succeeds on `prolog-equal`: numbers by `=`, strings by
  `string=`, a string and a keyword by `string-equal`, characters by
  `char=`, arrays by `equalp`, nodes by id, else `equal`. It is a
  generic function that callers may specialise.

## Design

### D1. One closure per clause

A clause compiles once, when it is added, into its own function of
`(arg1 ... argN cont)`: `compile-clause`'s code wrapped as

```lisp
(lambda (p1 ... pN cont)
  (let ((cut nil))
    (block CLAUSE-BLOCK
      <compiled body; a cut sets CUT, then returns from CLAUSE-BLOCK>)
    cut))
```

`eval`ed as today (one small lambda, not the whole functor). The
closure returns true when its body executed a cut, nil otherwise. The
`functor` struct gains `arity` and `clause-fns`, a simple vector of
these closures parallel to `functor-clauses`.

### D2. The dispatcher

`prolog-compile` on a functor with clauses installs, once, an
arity-specific dispatcher as the functor's `fn`:

```lisp
(lambda (p1 ... pN cont)
  (let ((old-trail (fill-pointer *trail*)))
    (do-candidates (fn functor p1)        ; D4 chooses them
      (when (funcall fn p1 ... pN cont) (return))
      (undo-bindings old-trail))))
```

Built by `eval` of a template over the arity when the functor gets its
first clause (or at `make-functor :clauses`), closing over the struct.
A later assert does not touch `fn`: the dispatcher reads `clause-fns`
and the index at call time. The clause-less functor keeps today's
`(lambda (&rest args) nil)`. `set-functor-fn` still replaces `fn`
wholesale, for the top-level query functors and tests that swap a
functor's function.

### D3. The cut

`compile-body`'s cut emission becomes

```lisp
(progn ,rest
       ,@(when *clause-cut-flag* `((setf ,*clause-cut-flag* t)))
       (return-from ,*functor* nil))
```

with a new special `*clause-cut-flag*` (globals.lisp, beside
`*functor*`): the symbol of the clause closure's `cut` variable while a
clause body compiles, nil at top level and under a barrier. The three
barrier macros (`not`, `once`, `if`) bind it to nil where they rebind
`*functor*`, and `%commit` restores it with `*functor*`: its goal
becomes `(%commit BARRIER OUTER OUTER-FLAG)`. `compile-clause` takes
`&key cut-flag` and binds the special only when given one, so the
top-level builders in `?-`, `select`, `map-query` and `do-query`, which
compile their goals inline inside their own block, are untouched and
their cut keeps returning from the query's block.

The semantics are the same as today's: the `return-from` unwinds the
clause's earlier choice points, and the flag tells the dispatcher to try
no further clause. A cut under a barrier sets no flag, so `once`, `not`
and `if` stay opaque; the goals after `%commit` set the clause's flag,
so a cut in a `then` branch still cuts the clause.

### D4. First-argument index

Each functor keeps `index`, a hash table from a canonical key to a
vector of clause positions, and `var-positions`, the positions of
clauses whose first head argument is a variable or a value the index
does not cover. The key of a value `x`, after dereferencing:

| value | key |
|---|---|
| unbound variable | no key: every clause, in order |
| `nil` or a non-keyword symbol | the symbol |
| string or keyword | `(:text . <string-upcase of the name>)` |
| number | `(:number . <rational of x>)` |
| character | the character |
| cons | `:cons` |
| anything else (node, timestamp, array, object) | `:opaque` |

The keys are supersets of `prolog-equal`: `1` and `1.0` share a key
through `rational`; a string and a keyword share one case-insensitively
(two strings that differ only in case then share a bucket and
unification tells them apart, which is allowed: a bucket may hold
clauses that fail, never miss one that matches). A clause whose first
head argument is a literal with a key goes under that key; a variable
head, or a literal keyed `:opaque`, goes to `var-positions`. A call
with key `k` tries the merge, by position, of `index[k]` and
`var-positions`; a call with key `:opaque` tries `var-positions` only
(no clause head can hold a node). Arity 0 has no index: every clause.

The merge is a two-pointer walk of two sorted position vectors, done at
call time; nothing is precomputed per key. `do-candidates` in D2 is
that walk.

### D5. Concurrency

Writers (`add-functor-clause`, `reset-functor`) hold the functor's
recursive lock, as today. Readers take no lock on SBCL, CCL and
LispWorks: `clause-fns`, each bucket vector and `var-positions` are
replaced by fresh vectors (copy on write, one `replace`), never
mutated in place, so a reader sees a consistent old or new vector; the
index table is created `:synchronized t` on SBCL and its equivalent
where one exists. On ECL, which has no thread-safe hash tables, the
dispatcher reads the index under the functor's lock, the same rule
`with-user-functors-lock` applies to the registry. Position vectors are
of fixnums; `clause-fns` grows by copy, so an assert is O(clauses) in
copying and O(1) in compilation, which is the point.

### D6. Compatibility

- `functor-clauses` stays the list of clause forms, in order, and is
  what `add-functor-clause` returns.
- `make-functor :clauses` compiles each given clause into `clause-fns`
  and builds the index and dispatcher once.
- `reset-functor` empties clauses, `clause-fns`, the index and
  `var-positions`, and installs the null function; `delete-functor` is
  unchanged.
- `prolog-compile-help` and `clauses-with-arity` go: a functor has one
  arity, from its name's `/N` suffix, checked against each clause's
  head (a mismatch is a `prolog-error`, where today it is compiled into
  a separate function the name can never reach).
- The `*clause-indexing*` switch (globals.lisp, default `t`): when
  nil, the dispatcher tries every clause in order and ignores the
  index. It exists for a `prolog-equal` specialisation that widens
  equality among the indexed types beyond D4's keys, and for the
  before-and-after measurement inside one image.

### D7. Cross-implementation

`eval` of a lambda is what the compiler does today on every
implementation; nothing new is asked of ECL or CCL beyond D5's lock
rule. The `#+ecl` arm of the dispatcher's read path is written with the
rest, not later (CLAUDE.md: don't skip the arms; verify on SBCL).

## Tests

All in the main suite (`graph-db/test`), beside
`tests/prolog-functor-tests.lisp`, with the stress and concurrency suites
unchanged and green:

- **Assert does not recompile.** A counter on clause compilation (a
  `*clause-compile-count*` the compiler increments, exported for tests)
  advances by one per `<-`.
- **Cut in an asserted clause.** `(<- (f ?x) (g ?x) !)` with three
  `g` facts yields one solution; a second clause `(<- (f z))` is not
  reached after the cut; without the cut, four solutions. The same
  with the cut inside `once`, `not` and an `if` then-branch, each
  asserting what the barrier rules say. Removing the `(setf cut t)`
  emission must fail the first of these (the mutation check of
  `~/.claude/memory/mutation-testing-is-the-gate.md`).
- **Index correctness.** A functor with heads `a`, `b`, `"s"`, `:s`,
  `1`, `1.0`, `#\c`, `(x . y)`, `nil` and `?v`: for each call key the
  solutions equal the solutions with `*clause-indexing*` nil, in the
  same order; `1` finds the `1.0` clause and the reverse; `"S"` finds
  the `:s` clause; `(p q)` finds the cons and the variable clauses only;
  a node as first argument finds the variable clause only.
- **Order.** Interleaved variable-headed and keyed clauses return
  solutions in source order for a keyed call.
- **Registry contracts.** `reset-functor` leaves an empty index and no
  candidates; `functor-clauses` is the full ordered list after mixed
  asserts; an arity mismatch signals `prolog-error`.
- **Concurrency.** `tests/concurrency/prolog-tests.lisp` as is.

## Measurement

Before, from unit 1b on odm (SBCL 2.6.6, experiment 4a5422e):
`clause-assert` 16/s, `clause-bound-first-arg` 49,998/s over 500
clauses. After: the same bench, third run, on #121; plus one run of a
10,000-fact table (the issue's "realistic clause count"), assert
throughput and a bound-first-argument lookup, recorded as a bench case
`clause-assert-10k` if its runtime at `:normal` stays under a minute,
else as a one-off number on the issue.

## Left to the builder

- Whether the dispatcher template is one macro over the arity or a
  small family for arities 0 to 8 with an `&rest`/`apply` fallback
  above; measure the call overhead against today's on the bench and
  pick the cheaper.
- The exact ECL table idiom (D5).
- Where `*clause-compile-count*` lives and whether it is exported or
  reached as `graph-db::`.
