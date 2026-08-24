# Runtime Schema From Persisted Metadata (#172) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A schema defined at runtime survives restart and materialises as
live CLOS classes at load time — before method files compile — with the
schema always viewable as text and exportable as source.

**Architecture:** Refactor `def-node-type` so everything except the literal
`defclass` flows through one functional core (`%install-node-type`); the
runtime path is MOP `ensure-class` + that same core. A system-level
append-only manifest (`schema-manifest.dat`, beside the type registry)
records every namespace and type with provenance; `materialize-schema`
reads it under `eval-when` and rebuilds packages and classes with no
evaluation of data. `describe-schema`/`export-schema-source` are read-only
views of the same metadata.

**Tech Stack:** Common Lisp (SBCL verified; ECL demoted), MOP (`sb-mop`
on SBCL — the package is already `:use`d by `graph-db`), FiveAM.

**Spec:** `docs/superpowers/specs/2026-08-24-runtime-schema-172-design.md`
(R1–R7) arguing from `docs/runtime-schema-example.lisp` (Kevin-approved —
the example's API is the contract; update the example in Task 5 if any
shipped signature drifts, and say so).

## Global Constraints

- Spaces only, **hard 80-column limit**, terse comments citing `(GH #172)`.
- **Restart never evaluates data**: no `eval`, no `load`, no
  `read` with `*read-eval*` on any persisted schema; classes are built via
  `ensure-class`/MOP calls on plists, helpers installed via
  `(setf fdefinition)` closures.
- Manifest discipline = the #167 occupancy sidecar's: printed with
  `*package*` bound to CL (symbols package-qualified), read with
  `*read-eval*` NIL, torn/malformed lines skipped, a failed append never
  aborts the caller, no system directory ⇒ in-image-only degradation.
- Source wins over metadata at materialize; slot divergence signals the
  existing `divergent-node-type-redefinition` (#196) style-warning.
- Test forms: `(with-transaction ((graph-db::transaction-manager g)) ...)`;
  fixtures rebind `graph-db::*system-directory*` (temp dir) +
  `graph-db::*type-registry*` NIL; neutral names (public repo). Test
  system `:graph-db/test`, package `graph-db/test`.
- SBCL from the worktree, `--dynamic-space-size 16384`, FIRST eval
  `(asdf:initialize-source-registry (list :source-registry (list :tree (truename ".")) :inherit-configuration))`.
  Focused run: `(let ((graph-db::*system-directory* (namestring (make-temp-directory))) (graph-db::*type-registry* nil)) (fiveam:run! 'runtime-schema-suite))`.
  One SBCL at a time on this host; never kill foreign processes.
- Docs travel with code (Task 5).

---

### Task 1: R1 — the shared `%install-node-type` path

**Files:**
- Modify: `schema.lisp` (`def-node-type` macro ~lines 399–632; new
  functions above it)
- Modify: `prolog-functors.lisp` only if a helper must live there
  (prefer keeping everything in `schema.lisp`)
- Test: `tests/runtime-schema-tests.lisp` (new; register in
  `graph-db.asd` after `package-namespace-tests`)

**Interfaces:**
- Produces (all internal, consumed by Tasks 2–3):
  - `(%normalize-slot-specs slot-specs)` → the accessor/initarg-filled
    list (extracted verbatim from the macro's existing `setq slot-specs`
    mapcar).
  - `(%install-node-type meta)` → meta. Everything the macro's expansion
    did EXCEPT the `defclass`: install `MAKE-<N>`/`LOOKUP-<N>`/`<N>-P`
    via `(setf (fdefinition (intern ...)))` closures; for `:edge`
    parents install the `<N>/2` and `<N>/3` functors; call
    `%warn-if-divergent-across-stores`; `finalize-inheritance`;
    replace-in-place registration into `*schema-node-metadata*`;
    `instantiate-node-type` into the open default store, if any.
  - `(%install-edge-functors name)` → installs two closures into
    `*prolog-global-functors*` under `(intern "<N>/2" pkg)` /
    `(intern "<N>/3" pkg)` where pkg = `(symbol-package name)`,
    with bodies identical in behaviour to the current macro-generated
    functors (the four map-edges dispatch arms; parameterised by NAME).
- The macro `def-node-type` shrinks to: normalize specs at expansion →
  `(defclass ...)` literal → `(let ((meta (make-node-type ...)))
  (%install-node-type meta))`. Its docstring and def-vertex/def-edge
  are unchanged.

Key implementation notes (read the current macro in full first):
- The constructor closure mirrors the current generated `defun`:

```lisp
(defun %make-constructor-closure (name graph-name kind)
  (if (eq kind :edge)
      (lambda (&rest make-args
               &key graph id deleted-p revision from to weight
               &allow-other-keys)
        (let ((graph (%default-store-graph name graph-name graph))
              (slots (%collect-constructor-slots name make-args)))
          (make-edge (node-type-id
                      (%ensure-type-in-store name :edge graph))
                     from to weight slots
                     :id id :revision revision :deleted-p deleted-p
                     :graph graph)))
      (lambda (&rest make-args
               &key graph id deleted-p revision &allow-other-keys)
        (let ((graph (%default-store-graph name graph-name graph))
              (slots (%collect-constructor-slots name make-args)))
          (make-vertex (node-type-id
                        (%ensure-type-in-store name :vertex graph))
                       slots
                       :id id :revision revision :deleted-p deleted-p
                       :graph graph)))))
```

  with `%collect-constructor-slots` extracted from the current
  `remove-if/mapcar` over `(data-slots (find-class name))` — evaluated
  at CALL time, so slot changes are picked up exactly as today.
- `lookup-<N>` closure: `(lookup-vertex id)`/`(lookup-edge id)` +
  `typep` + deleted-p, as generated today. `<N>-P` closure: `typep`.
- The functor bodies move from the macro into `%install-edge-functors`
  verbatim (the `*prolog-trace*` prints included); the macro arm that
  emitted `def-global-prolog-functor` forms now just relies on
  `%install-node-type` calling `%install-edge-functors`. NOTE
  `def-global-prolog-functor` also `(export ',name)`s — the runtime
  path must export the functor symbols from their package the same way
  (guard: only when the package is not CL/KEYWORD).
- `make-node-type` gains no new fields in this task.
- Constructor/lookup/predicate names remain interned in
  `(symbol-package name)` — TODAY they intern in `*package*` at
  expansion time, which for source code is the defining package.
  Switching to `symbol-package` of the class name is the correct
  generalization (identical for every existing use, where the class
  symbol is interned in the defining package); state this in the
  report if any test disagrees.

- [ ] **Step 1: Failing test** (equivalence pin — the only new test this
  task; the real net is the existing full suite):

```lisp
;;;; Runtime schema (GH #172).  Spec:
;;;; docs/superpowers/specs/2026-08-24-runtime-schema-172-design.md
(in-package #:graph-db/test)

(def-suite runtime-schema-suite :in graph-db-suite
  :description "Class-from-metadata, manifest, materialize (GH #172).")
(in-suite runtime-schema-suite)

(def-vertex rs-static () ((label :type string)) :rs-store)
(def-edge rs-knows () () :rs-store)

(defmacro with-rs-store ((g) &body body)
  (let ((sys (gensym)) (d (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,d)
         (let ((graph-db::*system-directory* (namestring ,sys))
               (graph-db::*type-registry* nil))
           (let ((,g (make-graph :rs-store (namestring ,d)
                                 :buffer-pool-size 1000)))
             (unwind-protect (progn ,@body)
               (let ((live (graph-db:lookup-graph :rs-store)))
                 (when (and live (graph-db::graph-open-p live))
                   (let ((graph-db:*graph* live))
                     (ignore-errors
                      (close-graph live :snapshot-p nil)))))
               (collect-garbage))))))))

(test source-types-behave-unchanged-through-the-shared-path
  "R1 equivalence pin: after the refactor a def-vertex/def-edge type
still constructs, looks up, predicates, and answers its Prolog functor.
The full suite is the broad net; this is the focused canary."
  (with-rs-store (g)
    (let (a b e)
      (with-transaction ((graph-db::transaction-manager g))
        (setq a (make-rs-static :label "a")
              b (make-rs-static :label "b")
              e (make-rs-knows :from (graph-db:id a)
                               :to (graph-db:id b))))
      (is (rs-static-p a))
      (is (typep (lookup-rs-static (graph-db:id a)) 'rs-static))
      (is (rs-knows-p e))
      ;; The functor was installed (macro path now goes through
      ;; %install-edge-functors).
      (is-true (gethash (intern "RS-KNOWS/2" :graph-db/test)
                        graph-db::*prolog-global-functors*))
      (let ((hits (select (?x ?y) (rs-knows ?x ?y))))
        (is (= 1 (length hits)))))))
```

  Adjust the functor-package assertion to wherever the symbol really
  lands (see the interning note above) and the `select` form to this
  test package's Prolog conventions (see `tests/index-prolog-tests.lisp`
  for the working pattern).

- [ ] **Step 2: Run** — fails only until the refactor compiles; the
  purpose is catching behavioural drift, so run it BEFORE the refactor
  too (against the old macro it must PASS except the
  `%install-edge-functors`-specific assertion — note the baseline).
- [ ] **Step 3: Refactor** as specified above. No behaviour change.
- [ ] **Step 4: Run** `runtime-schema-suite`, then the broad net:
  `package-namespace-suite`, `keyword-alias-suite`, `node-class-suite`,
  `index-prolog` suite, `global-type-id-suite`, and the full
  `(asdf:test-system :graph-db)` — this task touches every constructor
  in the image; the full gate is worth it HERE, not only at the end.
- [ ] **Step 5: Commit** `refactor(schema): def-node-type installs through %install-node-type (#172)`

---

### Task 2: R2+R4 — the manifest, `ensure-namespace`, `create-*-type`

**Files:**
- Create: `runtime-schema.lisp` (add to `graph-db.asd` after `schema`:
  `(:file "runtime-schema" :depends-on ("schema"))`)
- Modify: `schema.lisp` (`%install-node-type` appends a manifest record;
  a special `*schema-provenance*` defaults `:source`, bound `:runtime`
  by the create functions)
- Modify: `package.lisp` (export `#:ensure-namespace
  #:create-vertex-type #:create-edge-type`)
- Test: `tests/runtime-schema-tests.lisp`

**Interfaces:**
- Consumes: `%normalize-slot-specs`, `%install-node-type` (Task 1);
  `type-registry-location`, `ensure-type-registry`,
  `system-directory-required` (type-registry.lisp); the sidecar
  discipline patterns in `type-occupancy.lisp`.
- Produces:
  - `(ensure-namespace name &key nicknames)` → package. `name` string
    or symbol; creates via `make-package` (`:use` NIL — a schema
    namespace holds class/accessor symbols, it is not a code package;
    the example's `(:use #:cl #:graph-db)` applies to SOURCE packages a
    developer writes, and `export-schema-source` emits those — note
    this in the docstring) — idempotent; appends
    `(:namespace NAME :nicknames (...) :time T)`.
  - `(create-vertex-type name slot-specs &key parents default-store
    keep-revisions)` → class; `(create-edge-type ...)` same. `name`
    symbol or `"PKG:NAME"` string (parse with `*read-eval*` NIL is NOT
    enough — do NOT read; split on `:` and `intern`/`find-package`
    manually; missing package = error naming `ensure-namespace`).
    Builds the class via `ensure-class`:

```lisp
(defun %ensure-node-class (name parents kind normalized-slots)
  (ensure-class
   name
   :direct-superclasses
   (append parents (list (ecase kind (:vertex 'vertex) (:edge 'edge))))
   :direct-slots
   (mapcar (lambda (spec)
             (destructuring-bind (sname &key accessor initarg type
                                        &allow-other-keys)
                 spec
               (append
                (list :name sname
                      :initargs (list initarg)
                      :readers (list accessor)
                      :writers (list (list 'setf accessor)))
                (when type (list :type type)))))
           normalized-slots)
   :metaclass (find-class 'node-class)))
```

    then `make-node-type` + `%install-node-type` with
    `*schema-provenance*` = `:runtime`. Redefinition (existing class,
    source or runtime) is allowed — same replace/divergence semantics
    as re-evaluating def-vertex (spec R4/point B). `:default-store NIL`
    is legal: `%default-store-graph` already errors cleanly when
    `lookup-graph` of NIL returns NIL — verify the error message reads
    sanely for a NIL store and special-case the report string if not.
  - Manifest I/O in `runtime-schema.lisp`:
    `(%schema-manifest-file)` → path or NIL (same guard shape as
    `%edge-occupancy-file`); `(%append-schema-manifest-record plist)`
    (locked, guarded append; never signals);
    `(read-schema-manifest dir)` → `(values namespace-records
    type-records)` — last-record-per-name wins, `*read-eval*` NIL,
    malformed lines skipped. Record shapes exactly as spec R2.
    `%install-node-type` appends
    `(:type NAME :kind K :parents (...) :slots NORMALIZED :default-store
    S :keep-revisions KR :provenance *schema-provenance* :time
    (get-universal-time))`.
- NOTE the slots in the manifest are the NORMALIZED specs (accessor +
  initarg filled) so materialize needs no re-derivation.

- [ ] **Step 1: Failing tests**

```lisp
(test ensure-namespace-is-cheap-and-idempotent
  (with-rs-store (g)
    g
    (let ((p1 (graph-db:ensure-namespace "RS-TLM" :nicknames '("RST")))
          (p2 (graph-db:ensure-namespace "RS-TLM")))
      (is (eq p1 p2))
      (is (packagep p1))
      ;; No files, no store: the store registry did not grow.
      (is (null (graph-db:lookup-graph :rs-tlm))))))

(test create-vertex-type-yields-a-working-class
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (let ((class (graph-db:create-vertex-type
                  "RS-TLM:READING"
                  '((sensor-id :type string)
                    (value :type double-float))
                  :default-store :rs-store)))
      (is (typep class 'graph-db::node-class))
      (with-transaction ((graph-db::transaction-manager g))
        (funcall (intern "MAKE-READING" :rs-tlm)
                 :sensor-id "s1" :value 1.5d0))
      (let* ((sym (intern "READING" :rs-tlm))
             (hits (graph-db:map-vertices #'identity g :collect-p t
                                          :vertex-type sym)))
        (is (= 1 (length hits)))
        (is (string= "s1"
                     (funcall (intern "SENSOR-ID" :rs-tlm)
                              (first hits))))))))

(test create-edge-type-installs-functors-and-places-by-default
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (graph-db:create-edge-type "RS-TLM:FEEDS" '()
                               :default-store :rs-store)
    (let (a b)
      (with-transaction ((graph-db::transaction-manager g))
        (setq a (funcall (intern "MAKE-READING" :rs-tlm) :value 1d0)
              b (funcall (intern "MAKE-READING" :rs-tlm) :value 2d0))
        (funcall (intern "MAKE-FEEDS" :rs-tlm)
                 :from (graph-db:id a) :to (graph-db:id b)))
      (is-true (gethash (intern "FEEDS/2" :rs-tlm)
                        graph-db::*prolog-global-functors*)))))

(test manifest-records-both-provenances-and-tolerates-damage
  (with-rs-store (g)
    g  ; RS-STATIC/RS-KNOWS registered at load time = :source rows
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (multiple-value-bind (ns types)
        (graph-db::read-schema-manifest graph-db::*system-directory*)
      (is (find "RS-TLM" ns :key (lambda (r) (getf r :namespace))
                :test #'string-equal))
      (let ((row (find (intern "READING" :rs-tlm) types
                       :key (lambda (r) (getf r :type)))))
        (is-true row)
        (is (eq :runtime (getf row :provenance)))
        (is (eq :rs-store (getf row :default-store))))
      (is (eq :source
              (getf (find 'rs-static types
                          :key (lambda (r) (getf r :type)))
                    :provenance))))
    ;; Torn tail: append garbage, read again, intact rows survive.
    (with-open-file (s (graph-db::%schema-manifest-file)
                       :direction :output :if-exists :append)
      (format s "(:type RS-TORN"))
    (multiple-value-bind (ns types)
        (graph-db::read-schema-manifest graph-db::*system-directory*)
      ns
      (is (find (intern "READING" :rs-tlm) types
                :key (lambda (r) (getf r :type)))))))
```

- [ ] **Step 2: Run** — undefined functions.
- [ ] **Step 3: Implement** per the interface block.
- [ ] **Step 4: Run** `runtime-schema-suite` + `package-namespace-suite`.
- [ ] **Step 5: Commit** `feat(schema): runtime namespaces and types, recorded in the system manifest (#172)`

---

### Task 3: R3+R5 — `materialize-schema`, the function registry, `:check`

**Files:**
- Modify: `runtime-schema.lisp`, `value-constraint.lisp` (the `:check`
  enforcement point), `package.lisp` (export `#:materialize-schema
  #:register-schema-function #:find-schema-function
  #:materialize-unresolved-functions`)
- Test: `tests/runtime-schema-tests.lisp`

**Interfaces:**
- Consumes: `read-schema-manifest`, `ensure-namespace`,
  `%ensure-node-class`, `%install-node-type`, `%normalize-slot-specs`.
- Produces:
  - `(register-schema-function name fn)` → name; `(find-schema-function
    name)` → fn or NIL; EQ hash + lock in `runtime-schema.lisp`.
  - Slot option `:check FN-NAME` accepted in slot-specs (runtime AND
    `def-vertex` — the normalizer passes unknown options through to the
    meta's slots already; enforcement reads it): at commit, wherever
    `validate-value-constraints` runs, a slot with `:check` calls
    `(funcall (or (find-schema-function fn-name)
                  (error 'schema-function-unresolved ...)) value)`
    and a NIL result signals the existing
    `value-constraint-violation` shape (read
    `value-constraint.lisp:106` `%value-constraint-violations` and add
    the `:check` pass beside it, same condition class, NULL-exempt like
    `:one-of`). KEEP THIS SMALL — one option, one enforcement point.
  - `(materialize-schema dir &key namespaces)` — a MACRO expanding to
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%materialize-schema ,dir :namespaces ,namespaces))`.
    `%materialize-schema`: read manifest → ensure namespaces → for each
    type row (filtered by `:namespaces` when given, by the symbol's
    package name) with `(find-class name nil)` absent, build via
    `%ensure-node-class` + `make-node-type` + `%install-node-type`
    (`*schema-provenance*` `:runtime` — materialized types keep their
    runtime provenance; do NOT re-append rows for types already in the
    manifest unchanged: skip the append when an identical last row
    exists, or accept benign duplicate rows — implementer's choice,
    say which and why). A class that EXISTS is skipped (source wins;
    divergence → `%warn-if-divergent-across-stores` fires via the
    normal path — verify a warning actually reaches the user in the
    skip case and add a direct check if not). Before building anything:
    scan every row's slot `:check` names, collect unregistered ones,
    and signal ONE `materialize-unresolved-functions` (reader
    `unresolved-function-names`) listing all — fail fast, nothing half
    built. Returns `(:namespaces N :materialized M :skipped-source S)`.
    Dependency order: parents before children (rows sorted so a row
    whose `:parents` include a not-yet-built runtime type builds after
    it; manifest append order already guarantees this for types created
    through the API — a simple stable topological pass is still
    required for redefinition-reordered manifests).

- [ ] **Step 1: Failing tests**

```lisp
(defun %rs-wipe-runtime-state ()
  "Simulate a fresh image for the RS-TLM namespace: unintern the
classes, delete the package, drop the metas.  A real restart is a
different process; this is the closest single-image ablation and it
proves materialize rebuilds everything it needs."
  (let ((pkg (find-package :rs-tlm)))
    (when pkg
      (do-symbols (s pkg)
        (when (find-class s nil) (setf (find-class s) nil)))
      (delete-package pkg)))
  (maphash (lambda (store metas)
             (setf (gethash store graph-db::*schema-node-metadata*)
                   (remove-if (lambda (m)
                                (null (symbol-package
                                       (graph-db::node-type-name m))))
                              metas)))
           graph-db::*schema-node-metadata*))

(test materialize-rebuilds-a-runtime-type-in-a-fresh-image
  "THE acceptance test: create at runtime, write, wipe the in-image
state, materialize from the manifest, and both the CLASS and the DATA
come back -- and a method compiles against the class."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (let (id)
      (with-transaction ((graph-db::transaction-manager g))
        (setq id (graph-db:id (funcall (intern "MAKE-READING" :rs-tlm)
                                       :value 3.5d0))))
      (%rs-wipe-runtime-state)
      (is (null (find-package :rs-tlm)))
      (let ((summary (graph-db:materialize-schema
                      graph-db::*system-directory*)))
        (is (plusp (getf summary :materialized))))
      (let* ((sym (intern "READING" :rs-tlm))
             (class (find-class sym nil)))
        (is-true class)
        ;; A method compiles against the materialized class -- the
        ;; twenty-year problem, pinned.
        (let ((m (compile nil `(lambda (r)
                                 (declare (type ,sym r))
                                 (funcall ',(intern "VALUE" :rs-tlm)
                                          r)))))
          (is (= 3.5d0 (funcall m (graph-db:lookup-vertex
                                   id :graph g)))))))))

(test materialize-skips-source-defined-classes
  "Source wins: RS-STATIC is defined by def-vertex in this image; its
manifest row must be skipped, not rebuilt, and the summary says so."
  (with-rs-store (g)
    g
    (let ((summary (graph-db:materialize-schema
                    graph-db::*system-directory*)))
      (is (plusp (getf summary :skipped-source)))
      (is (typep (make-instance 'rs-static) 'rs-static)))))

(test materialize-fails-fast-on-an-unresolved-check-function
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:register-schema-function 'rs-plausible-p
                                       (lambda (v) (< 0 v 100)))
    (graph-db:create-vertex-type
     "RS-TLM:CAL" '((value :type double-float :check rs-plausible-p))
     :default-store :rs-store)
    (%rs-wipe-runtime-state)
    ;; Fresh image forgot to register the function:
    (graph-db::%unregister-schema-function 'rs-plausible-p)
    (let ((c (handler-case
                 (progn (graph-db:materialize-schema
                         graph-db::*system-directory*)
                        nil)
               (graph-db:materialize-unresolved-functions (e) e))))
      (is-true c)
      (when c
        (is (member 'rs-plausible-p
                    (graph-db:unresolved-function-names c))))
      ;; Fail fast: nothing half-built.
      (is (null (find-class (and (find-package :rs-tlm)
                                 (intern "CAL" :rs-tlm))
                            nil))))))

(test check-constraint-enforces-at-commit
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:register-schema-function 'rs-plausible-p
                                       (lambda (v) (< 0 v 100)))
    (graph-db:create-vertex-type
     "RS-TLM:CAL" '((value :type double-float :check rs-plausible-p))
     :default-store :rs-store)
    (with-transaction ((graph-db::transaction-manager g))
      (funcall (intern "MAKE-CAL" :rs-tlm) :value 50d0))
    (signals graph-db:value-constraint-violation
      (with-transaction ((graph-db::transaction-manager g))
        (funcall (intern "MAKE-CAL" :rs-tlm) :value 5000d0)))))
```

  (Add the tiny `%unregister-schema-function` internal for the test.)
  Adapt the constraint-violation condition name/signal point to what
  `value-constraint.lisp` really signals (read
  `%value-constraint-violations` and its caller first).

- [ ] **Step 2: Run** — undefined functions.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run** `runtime-schema-suite` + `value-constraint` suite.
- [ ] **Step 5: Commit** `feat(schema): materialize-schema rebuilds runtime classes at load time (#172)`

---

### Task 4: R6 — `describe-schema` and `export-schema-source`

**Files:**
- Create: `schema-tools.lisp` (`.asd` after `runtime-schema`, depends on
  it), `package.lisp` exports (`#:describe-schema
  #:export-schema-source`)
- Test: `tests/runtime-schema-tests.lisp`

**Interfaces:**
- Consumes: `read-schema-manifest`, live metas, `node-type-*` readers.
- Produces:
  - `(describe-schema &key namespace store since
    (stream *standard-output*))` → no value; plain text grouped by
    namespace (package name of each type symbol), each type line:
    name, kind, default store, `[source]`/`[runtime <ISO-date>]` from
    its manifest row (a type with no manifest row — pre-#172 store —
    prints `[source]`); slot lines: name, type, `:check` name if any.
    `:since` (universal time or "YYYY-MM-DD" string) filters rows by
    `:time`. `:store` limits to types instantiated in that open store.
  - `(export-schema-source path &key namespace store)` → truename.
    Emits: generated-header comment (tool name + date + the
    idempotence note from the example), one `defpackage` per exported
    namespace (`(:use #:cl #:graph-db)` + `:export` of the class
    names — this IS the source-package shape, per Task 2's
    ensure-namespace docstring note), `in-package`, and
    `def-vertex`/`def-edge` forms rebuilt from metas: slot specs
    printed WITHOUT the auto-filled `:accessor`/`:initarg` when they
    match the defaults (so the output looks hand-written), keeping
    `:type`/`:check`/other options; trailing default-store argument
    (`nil` prints as `nil`); `:keep-revisions` when non-NIL. Printed
    with `*print-case* :downcase`, `*package*` bound per-namespace so
    symbols read back correctly, ≤80 columns via the pretty printer
    (`*print-right-margin*` 79).

- [ ] **Step 1: Failing tests**

```lisp
(test describe-schema-shows-provenance-and-slots
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (let ((text (with-output-to-string (s)
                  (graph-db:describe-schema :stream s))))
      (is (search "RS-TLM" text))
      (is (search "READING" text))
      (is (search "[runtime" text))
      (is (search "RS-STATIC" text :test #'char-equal))
      (is (search "[source]" text)))))

(test export-schema-source-round-trips
  "The promotion path: export the runtime namespace, wipe the image
state, LOAD the exported file (the ordinary source path -- the ENGINE
never does this, the developer's build does), and the type is back
with the SAME registry id."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (let ((id-before (graph-db::node-type-id
                      (graph-db::%find-registered-node-type
                       (intern "READING" :rs-tlm) :vertex)))
          (path (merge-pathnames
                 "exported-schema.lisp"
                 (uiop:ensure-directory-pathname
                  graph-db::*system-directory*))))
      (graph-db:export-schema-source path :namespace :rs-tlm)
      (%rs-wipe-runtime-state)
      (load path)
      (let ((meta (graph-db::%find-registered-node-type
                   (intern "READING" :rs-tlm) :vertex)))
        (is-true meta)
        (is (= id-before (graph-db::node-type-id meta)))))))
```

- [ ] **Step 2: Run** — undefined. **Step 3: Implement.**
- [ ] **Step 4: Run** the suite. **Step 5: Commit**
  `feat(schema): describe-schema and export-schema-source (#172)`

---

### Task 5: Docs and the example reconciled

**Files:**
- Modify: `CHANGELOG.md` (`### Added`: the whole unit, one entry —
  runtime schema, materialize, tooling, the behaviour boundary),
  `docs/vivace-graph-v3-doc.org` (new schema-chapter section: the three
  sessions from the example, the `.asd` ordering recipe, the boundary,
  describe/export), `docs/runtime-schema-example.lisp` (reconcile every
  form with the shipped signatures; keep the design commentary; note
  at top it is now DOCUMENTATION of shipped behaviour),
  `docs/superpowers/specs/2026-08-24-runtime-schema-172-design.md`
  (Built note), `docs/superpowers/specs/2026-08-20-namespaces-design.md`
  (§3.5 Built note; §11 unit 7 → **Done**; if every unit row is now
  Done, say so where §11 introduces the table).
- Sanity pass: exported names exist in `package.lisp`; org SRC balance;
  Lisp examples ≤80 cols. ONE commit
  `docs(schema): runtime schema -- manual, CHANGELOG, example reconciled (#172)`.

---

## Self-review

- Spec coverage: R1→T1, R2→T2, R3→T3, R4→T2, R5→T3, R6→T4, R7 honoured
  (no deletion, no runtime views/indexes, no Emacs mode); acceptance
  table's five rows → T3 tests + branch gate; example promises → T2–T4
  + T5 reconciliation.
- Type consistency: `%install-node-type`, `%normalize-slot-specs`,
  `%ensure-node-class`, `read-schema-manifest`,
  `%append-schema-manifest-record`, `%schema-manifest-file`,
  `register-schema-function`/`find-schema-function`,
  `materialize-unresolved-functions`/`unresolved-function-names`,
  `describe-schema`, `export-schema-source` used consistently.
- Judgment points named for implementers: helper-symbol interning
  package (T1), NIL-default-store error message (T2), duplicate
  manifest rows at materialize (T3), constraint condition name (T3),
  export slot-spec minimization (T4).
