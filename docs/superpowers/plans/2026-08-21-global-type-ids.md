# Global Type-IDs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A `type-id` means the same thing in every store of a system, backed by a
persisted image-level registry keyed on package-qualified symbol.

**Architecture:** A single append-only registry file in the **system directory**, which
this unit makes **mandatory**. Assignment moves out of the per-graph `schema-next-*-id`
counters and into the registry, under a short exclusive `flock` held across
read-decide-append. Existing systems adopt via a renumbering replay seeded from the
largest store. The hub's frozen type-table wire format becomes the normal distribution
path, and a handshake refuses a peer whose registry disagrees.

**Tech Stack:** Common Lisp (SBCL primary, CCL/ECL conditionally), CFFI, FiveAM, ASDF.

**Spec:** `docs/superpowers/specs/2026-08-20-namespaces-design.md` — §3.4 (registry, D14,
D15), §10.1 (migration), §12 (acceptance).

## Global Constraints

- **Lisp: spaces only, never tabs. Hard 80-column limit** — code, comments, docstrings and
  strings alike; Org prose at 79. A 96-column line is a defect.
- Comments terse: state the non-obvious fact, reference an issue, do not narrate.
- **This repo is PUBLIC.** No domain specifics in code, comments, docs or commit messages.
  Describe shapes ("a five-store system"), never a deployment's class or store names.
- **The system directory is now mandatory.** `*system-clock*` stays optional; its
  *directory* does not. Decided 2026-08-21; see the "Directory vs clock" note below.
- **Do not reuse the clock's lifetime lock for the registry.** #182 holds `LOCK_EX` for the
  clock's whole life. The registry must NOT do that — see Task 1.
- `peer-type-table-string`'s wire format is a **frozen external contract** parsed by a
  non-Lisp peer. Do not change its grammar.
- Every push that changes source changes docs too; a `PreToolUse` hook enforces it.
- Run tests in the **foreground**. Never two SBCL builds at once — shared FASL cache.
- Assert ASDF resolved to this worktree before trusting any test result.

### Directory vs clock — the distinction that must not blur

#182 gave the *clock* an exclusive `flock` held for its whole lifetime, so only one image
may **allocate epochs**. If the registry reused that lock, making the directory mandatory
would mean only one image could open **any graph at all** — a far larger restriction than
#182 argued for, arriving as a side effect rather than a decision.

So the registry takes its **own** lock, held only across a read-decide-append. Many images
may read the registry concurrently; only assignment serialises.

---

### Task 1: The registry object

**Files:**
- Create: `type-registry.lisp`
- Modify: `graph-db.asd` (component after `system-clock`; `:depends-on ("serialize" "utilities")`)
- Modify: `package.lisp` (exports)
- Create: `tests/type-registry-tests.lisp`, register it in `graph-db.asd`

**Interfaces:**
- Produces:
  - `(open-type-registry location)` → `type-registry`; creates the file if absent.
  - `(registry-id-for registry symbol parent)` → integer or `NIL` (pure read).
  - `(registry-intern registry symbol parent)` → integer; **assigns if absent**, under lock.
  - `(registry-entries registry)` → list of `(symbol parent id)`, oldest first.
  - `(close-type-registry registry)`.
  - Condition `type-registry-busy` with reader `type-registry-busy-location`,
    signalled when the append lock is held elsewhere. (The *disagreement*
    condition D15 needs is Task 4's, not this one's — do not add it here.)
- Consumes: `%posix-flock`, `%posix-open`, `%posix-close`, `+lock-ex+`, `+lock-nb+` (#182).

**Context the brief cannot give you:** the file is `type-registry.log`, a sibling of
`system-clock.dat` and `system-journal.log`. Records are one per line, `~S`-printed and
read back with `*read-eval*` **NIL** — the journal in `system-clock.lisp` is the pattern to
follow, including that it is data and must never execute. Note #191: a torn final record
there makes the whole file unreadable. **Do not repeat that defect** — this reader must
tolerate a truncated *final* record (log it, stop) while still signalling on a malformed
record anywhere earlier.

- [ ] **Step 1: Write the failing tests**

```lisp
;;;; The image-level type-id registry (GH #186).
(in-package #:graph-db/test)

(def-suite type-registry-suite :in graph-db-suite
  :description "The persisted, image-level type-id registry.")
(in-suite type-registry-suite)

(test registry-assigns-distinct-ids-to-distinct-symbols
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (let ((a (graph-db::registry-intern r 'reg-alpha :vertex))
                 (b (graph-db::registry-intern r 'reg-beta :vertex)))
             (is (integerp a))
             (is (/= a b) "two symbols never share an id"))
        (graph-db::close-type-registry r)))))

(test registry-is-idempotent-for-one-symbol
  "The whole point of a registry: asking twice gives the same answer, and
asking in another image after a reopen still does."
  (with-temp-directory (dir)
    (let* ((r (graph-db::open-type-registry (namestring dir)))
           (first (graph-db::registry-intern r 'reg-gamma :vertex)))
      (is (= first (graph-db::registry-intern r 'reg-gamma :vertex)))
      (graph-db::close-type-registry r)
      (let ((r2 (graph-db::open-type-registry (namestring dir))))
        (unwind-protect
             (is (= first (graph-db::registry-intern r2 'reg-gamma :vertex))
                 "the assignment is durable, not in-memory")
          (graph-db::close-type-registry r2))))))

(test registry-separates-vertex-and-edge-spaces
  "Vertices and edges are distinct spaces, as they are per-graph today; the
same symbol may hold a different id in each."
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (let ((v (graph-db::registry-intern r 'reg-delta :vertex))
                 (e (graph-db::registry-intern r 'reg-delta :edge)))
             (is (integerp v)) (is (integerp e))
             (is (= v (graph-db::registry-id-for r 'reg-delta :vertex)))
             (is (= e (graph-db::registry-id-for r 'reg-delta :edge))))
        (graph-db::close-type-registry r)))))

(test registry-distinguishes-same-name-in-two-packages
  "The registry is keyed on the PACKAGE-QUALIFIED symbol.  Keying on the name
would collide two packages' types -- the defect #190 records in the per-graph
keyword alias, which this must not reproduce."
  (with-temp-directory (dir)
    (flet ((pkg (n) (or (find-package n) (make-package n :use '()))))
      (let* ((s1 (intern "SPECIES" (pkg "REG-TEST-1")))
             (s2 (intern "SPECIES" (pkg "REG-TEST-2")))
             (r (graph-db::open-type-registry (namestring dir))))
        (unwind-protect
             (is (/= (graph-db::registry-intern r s1 :vertex)
                     (graph-db::registry-intern r s2 :vertex))
                 "same name, different packages, different ids")
          (graph-db::close-type-registry r))))))

(test registry-tolerates-a-torn-final-record
  "GH #191: the lifecycle journal signals on a truncated tail and loses the
whole file.  The registry is the ONLY record of what a type-id means -- losing
it is worse -- so a torn FINAL record is dropped with a log, while a malformed
record earlier still signals."
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (graph-db::registry-intern r 'reg-keep-1 :vertex)
      (graph-db::registry-intern r 'reg-keep-2 :vertex)
      (graph-db::close-type-registry r))
    (let ((f (merge-pathnames "type-registry.log" dir)))
      (with-open-file (s f :direction :output :if-exists :append)
        (format s "(:SYMBOL REG-TORN :PARENT :VERT"))   ; truncated, no newline
      (let ((r2 (graph-db::open-type-registry (namestring dir))))
        (unwind-protect
             (is (= 2 (length (graph-db::registry-entries r2)))
                 "the two intact records survive a torn tail")
          (graph-db::close-type-registry r2))))))

(test registry-assignment-is-serialised-across-open-file-descriptions
  "Two images assigning concurrently must not hand the same id to different
symbols.  flock attaches to the open file description, so two registries on one
directory in this image contend exactly as two processes would."
  (with-temp-directory (dir)
    (let ((r1 (graph-db::open-type-registry (namestring dir)))
          (r2 (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (let ((a (graph-db::registry-intern r1 'reg-race-a :vertex))
                 (b (graph-db::registry-intern r2 'reg-race-b :vertex)))
             (is (/= a b)
                 "the second assigner re-read the tail under the lock and did
not reuse the first's id"))
        (graph-db::close-type-registry r1)
        (graph-db::close-type-registry r2)))))
```

- [ ] **Step 2: Run them and watch them fail**

Run `type-registry-suite` only. Expected: `OPEN-TYPE-REGISTRY` undefined. A reader error
instead means a paren or package problem — fix that first.

- [ ] **Step 3: Implement the registry**

```lisp
(in-package :graph-db)

;;; The image-level type-id registry (GH #186).  Append-only; hosts read it,
;;; none recompute it (D14).  Keyed on the PACKAGE-QUALIFIED symbol, so two
;;; packages' same-named types never collide (cf. #190).

(defstruct (type-registry (:constructor %make-type-registry))
  (location nil)
  ;; symbol -> id, per parent.  Rebuilt from the file at open.
  (vertex (make-hash-table :test 'eq))
  (edge   (make-hash-table :test 'eq))
  (next-vertex 1 :type (unsigned-byte 32))
  (next-edge   1 :type (unsigned-byte 32))
  (lock (make-recursive-lock "type registry")))

(defun %registry-file (location)
  (make-pathname :name "type-registry" :type "log" :defaults location))

(defun %registry-table (registry parent)
  (ecase parent
    ((:vertex vertex) (type-registry-vertex registry))
    ((:edge edge)     (type-registry-edge registry))))
```

Then `%registry-load` (read records, tolerate a torn **final** record only),
`%registry-append` (one `~S` line + `finish-output`), and the three entry points. The
assignment path is:

```lisp
(defun registry-intern (registry symbol parent)
  "The id for SYMBOL under PARENT, assigning one if absent.  The read-decide-
append runs under an exclusive flock: two images that both find SYMBOL absent
would otherwise assign it different ids, or one id to two symbols (#186)."
  (with-recursive-lock-held ((type-registry-lock registry))
    (or (registry-id-for registry symbol parent)
        (let* ((file (%registry-file (type-registry-location registry)))
               (fd (%posix-open file (logior +o-creat+ +o-rdwr+))))
          (unwind-protect
               (progn
                 (unless (%posix-flock fd (logior +lock-ex+ +lock-nb+))
                   (error 'type-registry-busy
                          :location (type-registry-location registry)))
                 ;; Re-read under the lock: another image may have assigned
                 ;; SYMBOL between our miss above and taking the lock.
                 (%registry-load registry)
                 (or (registry-id-for registry symbol parent)
                     (%registry-assign registry symbol parent)))
            (%posix-close fd))))))
```

- [ ] **Step 4: Run the tests and watch them pass**

Then the **full** suite once, to confirm the new ASDF component did not disturb load order.

- [ ] **Step 5: Ablation**

Remove the re-read under the lock (the `%registry-load` call inside `registry-intern`) and
confirm `registry-assignment-is-serialised-across-open-file-descriptions` **fails**.
Restore; confirm it passes. Report both counts and confirm you re-read the file to verify
the restore landed.

- [ ] **Step 6: Commit**

```bash
git add type-registry.lisp tests/type-registry-tests.lisp graph-db.asd package.lisp
git commit -m "feat(registry): persisted image-level type-id registry (#186)"
```

---

### Task 2: Assignment moves to the registry; the system directory becomes mandatory

**Files:**
- Modify: `schema.lisp` (`get-next-type-id` ~`:149`; `update-node-type` ~`:218`;
  delete `%check-node-class-graph-unique` ~`:234` and its caller)
- Modify: `globals.lisp` (`*system-directory*`)
- Modify: `graph.lisp` (`make-graph` ~`:310`, `open-graph` ~`:520` — require the directory)
- Modify: `package.lisp`, `tests/schema-tests.lisp` (or nearest), `CHANGELOG.md`,
  `docs/vivace-graph-v3-doc.org`

**Interfaces:**
- Consumes: `open-type-registry`, `registry-intern`, `registry-id-for` (Task 1).
- Produces: `*system-directory*` (special, no default); `*type-registry*` (the open
  registry for this image).

**Context the brief cannot give you:**

`get-next-type-id` currently takes `(schema parent)` and bumps a per-graph counter. It must
now consult the registry, which is keyed on **symbol** — so the *symbol* has to reach it.
`update-node-type` (`schema.lisp:218`) is where the type's name is known. Restructure so
assignment happens there, not in a counter bump that never sees the name.

**Delete `%check-node-class-graph-unique` in this task.** It exists only because type-ids
were per-graph; once ids are global it has no job. Its call site is in `def-node-type`.
Removing it is *why* a class may be instantiated in more than one store (D4, and the
acceptance criterion that closes cl-llm#20).

**The keyword alias at `schema.lisp:227` is #190 and is NOT in scope.** Do not fix it here
and do not extend it. If your change makes it more reachable, say so in your report — #190
is already a blocker on #167.

`+max-node-types+` stays `(expt 2 32)`; nothing about the ceiling changes.

- [ ] **Step 1: Write the failing tests**

`with-test-graph` (`tests/suite.lisp:137`) takes only `(g)` and hardcodes
`*integration-graph-name*`, so it **cannot** open the two graphs these tests need.
Write a two-graph helper first:

```lisp
(defmacro with-two-test-graphs ((g1 g2 sysdir) &body body)
  "Two stores in ONE image, sharing SYSDIR as their system directory.  The
existing WITH-TEST-GRAPH binds a single hardcoded graph name and cannot express
this, which is why #186 needs its own helper."
  (let ((d1 (gensym)) (d2 (gensym)))
    `(with-temp-directory (,sysdir)
       (with-temp-directory (,d1)
         (with-temp-directory (,d2)
           (let ((graph-db::*system-directory* (namestring ,sysdir)))
             (let ((,g1 (make-graph :reg-store-1 (namestring ,d1)
                                    :buffer-pool-size 1000))
                   (,g2 (make-graph :reg-store-2 (namestring ,d2)
                                    :buffer-pool-size 1000)))
               (unwind-protect (progn ,@body)
                 (ignore-errors (close-graph ,g1))
                 (ignore-errors (close-graph ,g2))))))))))
```

There is no `lookup-node-type-id`; the id comes from the metadata:

```lisp
(defun %type-id-of (sym parent graph)
  (graph-db::node-type-id
   (graph-db::lookup-node-type-by-name sym parent :graph graph)))

(test two-graphs-in-one-image-share-a-symbol-s-type-id
  "The unit's entire purpose.  Before #186 each graph counted from 1, so the
same symbol got different ids in different stores and different symbols
collided on one id."
  (with-two-test-graphs (g1 g2 sysdir)
    (declare (ignore sysdir))
    (is (= (%type-id-of 'shared-type :vertex g1)
           (%type-id-of 'shared-type :vertex g2))
        "one symbol, one id, both stores")))

(test distinct-symbols-never-collide-across-graphs
  (with-two-test-graphs (g1 g2 sysdir)
    (declare (ignore sysdir))
    (is (/= (%type-id-of 'type-in-one :vertex g1)
            (%type-id-of 'type-in-two :vertex g2))
        "two symbols never share an id, even in different stores")))

(test opening-a-graph-without-a-system-directory-signals
  "The directory is mandatory as of #186: the registry has nowhere to live
without one, and a graph opened outside a system would mint ids that mean
nothing to anyone else.  Refuse rather than silently fall back to per-graph
counters -- a silent fallback is how two id regimes diverge unnoticed."
  (with-temp-directory (dir)
    (let ((graph-db::*system-directory* nil))
      (signals graph-db:system-directory-required
        (make-graph :reg-nodir (namestring dir))))))

(test a-class-may-be-instantiated-in-more-than-one-store
  "Closes cl-llm#20.  %CHECK-NODE-CLASS-GRAPH-UNIQUE refused this and existed
only because ids were per-graph."
  (with-temp-directory (dir)
    (declare (ignore dir))
    (with-two-test-graphs (g1 g2 sysdir)
      (declare (ignore sysdir))
      (is-true (graph-db::lookup-node-type-by-name
                'dual-type :vertex :graph g1))
      (is-true (graph-db::lookup-node-type-by-name
                'dual-type :vertex :graph g2)))))
```

- [ ] **Step 2: Run and watch them fail.** The first two must fail because the ids
      *differ* — confirm you see that, not merely an undefined symbol.

- [ ] **Step 3: Implement.** Assignment consults the registry; `make-graph`/`open-graph`
      signal `system-directory-required` when `*system-directory*` is `NIL`; delete
      `%check-node-class-graph-unique` and its call.

- [ ] **Step 4: Run tests, then the full suite.** This task changes a core path — expect
      fallout in existing schema and multi-graph suites and fix it properly rather than
      relaxing assertions. Report the count.

- [ ] **Step 5: Ablation.** Make `registry-intern` fall back to the per-graph counter when
      the registry lacks the symbol; confirm `two-graphs-in-one-image-share-a-symbol-s-type-id`
      fails. Restore and verify.

- [ ] **Step 6: Docs + commit** — CHANGELOG (breaking: the directory is now required) and
      the manual's schema chapter.

---

### Task 3: Seeding and the renumbering migration

**Files:**
- Modify: `backup.lisp` (`migrate-graph` ~`:271`), `type-registry.lisp` (seeding)
- Modify: `tests/type-id-width-tests.lisp` (make #166's mode explicit), new tests
- Modify: `CHANGELOG.md`, `docs/vivace-graph-v3-doc.org`

**Interfaces:**
- Produces: `(registry-seed-from-stores registry locations)`;
  `migrate-graph` gains `:renumber-p` (default `NIL`).

**Context the brief cannot give you:** read **§10.1** of the spec before starting; it
carries the measurement this task exists because of.

**The registry is already dirty when you arrive, and `migrate-graph` is why.** Found by the
Task 2 review, deferred here because this task owns the function.

`migrate-graph` (`backup.lisp:330-338`) calls `(make-graph name new-location)`. As of Task 2
that path runs `update-schema`, which interns **every** one of the graph's types into the
registry and assigns them real ids. `migrate-graph` then immediately does
`(setf (schema new) old-schema)` — discarding those ids and installing the legacy per-graph
ones.

Two consequences you must handle rather than discover:

1. **The migrated store's ids are not the registry's.** That is precisely the two-regime
   divergence this whole unit exists to prevent, arriving through the migration path.
2. **The registry permanently holds entries for those names at ids no store uses.** So
   `registry-seed-from-stores` cannot assume it is seeding into an empty registry, and a
   naive seed will collide with junk its own migration created.

**This is currently invisible in the suite.** `migrate-v2-graph-to-v3`
(`tests/type-id-width-tests.lisp:365`) passes *because* the schema swap wins — nothing
notices that the registry was consulted and overruled. A test that fails for this reason is
part of this task.

Decide deliberately whether `:renumber-p nil` should still intern into the registry at all.
Preserving legacy ids and *also* polluting the registry is the worst of both; either the
non-renumbering path stays out of the registry entirely, or it seeds it consistently with
what it preserved.

**Seed from the largest store on disk, not the one with the most types.** All but one store
renumbers whichever is favoured, so the cost is bytes. On the measured system, seeding by
type count would have picked a store holding 59 of 95 types and among the *smallest*,
forcing a rewrite of the largest — roughly 4.9 GB of replay instead of 1.1 GB.

**A symbol may already hold two different ids within one store's history.** Those unify
whichever store wins, so such a store is always in the migration set. Seeding must detect
this and report it, not pick arbitrarily.

**#166's guarantee becomes mode-dependent.** `migrate-v1-graph-to-v3` and
`migrate-v2-graph-to-v3` assert type-ids "must survive migration unchanged"
(`tests/type-id-width-tests.lisp:441`). That stays true for `:renumber-p nil`. **Rename or
re-docstring them so they name the mode they pin**, and add the renumbering counterparts —
otherwise the renumbering path inherits tests asserting the reverse of its behaviour.

Required tests: a two-store fixture with colliding ids migrates to distinct global ids with
every node's *class* preserved; a symbol holding two ids unifies and the migration says so;
`:renumber-p nil` still preserves ids exactly as #166 asserts.

Ablation: make the renumbering pass reuse the source id; confirm the collision test fails.

---

### Task 4: Distribution, the handshake guard, and the fixed-width audit

**Files:**
- Modify: `peer-streaming.lisp` (`%peer-type-table-rows` ~`:144`, handshake)
- Modify: `tests/peer-*-tests.lisp`, `CHANGELOG.md`, `docs/vivace-graph-v3-doc.org`

**Context the brief cannot give you:**

**`peer-type-table-string`'s grammar is a FROZEN EXTERNAL CONTRACT** parsed by a non-Lisp
peer. Do not change the grammar. What changes is *what fills it*: the table becomes the
image's registry rather than one graph's schema.

`%peer-type-table-rows` takes a `graph` and reads that graph's `schema-type-table`. Under a
global registry the rows are image-level. **`%peer-validate-type-table-rows` matters more
now, not less** — it refuses two types that `string-downcase` to one name, and pooling every
store's types into one table widens that collision surface. Its error message currently
advises renaming; check that advice still makes sense when the two types come from
different stores.

**D15, the handshake guard.** Compare registries at the handshake and **refuse, naming the
conflicting symbols**. An image with no hub is its own authority — the common case, since
`*peer-hub-enabled*` defaults to `NIL` — so two such images can independently assign the
same symbol different ids. Reconciling would mean a data migration triggered by a network
handshake; a disagreement between two populated stores is an operator event.

**The fixed-width audit (§3.4).** #187 existed because the original analysis asked "what
scales with `+max-node-types+`?" and never asked "**where else is a type-id serialised at a
fixed width?**". Ask that second question of every remaining wire and file format and report
the answer as a list, even if empty. Known: `memory-graph.lisp` fixed by #187;
`peer-streaming.lisp:261` validates and signals. Anything else you find gets an issue, not a
silent fix — a widening outside this unit's scope needs its own review.

Ablation: make the handshake accept a disagreeing registry; confirm the refusal test fails.
