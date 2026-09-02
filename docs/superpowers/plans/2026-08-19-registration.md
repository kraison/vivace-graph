# Registration implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bind a record's geometry to regions in a registry, writing one
claim per binding, with a fraction saying how much of the record falls in
each.

**Architecture:** A core geodesic area op; two slots promoted onto every
claim; a payload for the `:registration` facet; and `register-geometry` /
`register-node` in `graph-db/spacetime`, which query the spatial index
once, compute fractions exactly, and refuse rather than approximate when
they cannot. The place spine then adopts all of it and deletes its own
copy.

**Tech Stack:** Common Lisp (SBCL), ASDF, FiveAM, `graph-db`,
`graph-db/geos` (optional add-on), `cl-temporal-extent`.

**Spec:** `docs/superpowers/specs/2026-08-19-registration-design.md`
(committed `155eefa`, corrected by `e1eee3b` and `0f3ea97` — read the
current file, not the first commit). Executors read both.

## Global Constraints

- **80 columns, hard**, counted in **codepoints not bytes**. Spaces only,
  never tabs.
- **Comments terse**, pointing at a doc, a GH issue or a SHA. Docstrings
  say what it does, what it returns, and the one trap a caller must know.
- **`fraction` is dimensionless, `precision-m` is metres.** Never a
  discount factor, and precision flows in both directions — a source may be
  finer than the region it joins to.
- **A bounding box is over-inclusive.** Registration never writes a claim
  from an approximate candidate set. See Task 4's no-GEOS path.
- **Never catch broader than `geos-error`.** A broader handler swallows the
  multi-graph node-escape class (GH #53).
- **Plain values cross a graph binding; node objects never do** (GH #53).
- **Two repos.** `~/work/vivace-graph-v3` (engine) and
  `~/work/mine-action` (tenant). The tenant's deployed graphs are on this
  host under `/data0/mine-action-dev/graphs/`. **A live server holds them
  open**; a second opener is refused with `.dirty exists`, and running
  recovery on a live graph is worse than the problem. Task 6 works on a
  **copy** and never stops the server.
- **Never run two SBCLs at once.** A `mine-action` server and a `cl-mcp`
  REPL are resident but not building; do not start a second build of your
  own.
- Run tests **detached to a log file and read it**: `nohup sbcl
  --non-interactive ... > /tmp/log 2>&1 &`, capture the PID with `$!`, then
  loop on `kill -0 $PID` until it exits and read the log. Do NOT poll with
  `pgrep -f 'sbcl --non-interactive'` — that pattern matches your own wait
  command and never terminates. Never `timeout N sbcl ... | tail`.
- **Baselines:** record the counts of `graph-db/spacetime-test` and
  `graph-db/test` before your first change and report them with every run.
  Nothing outside your task's suites may move.
- **Do not push. Do not bump any version.**
- **A test whose whole value is that it would fail must be shown to fail.**

## Two rulings this plan makes, which the spec left open (§13)

1. **A traversal's registrations are unordered.** The substrate promises no
   order and returns what the index returned, each with its fraction.
   "Most specific first" is a tenant's notion — the spine sorts by place
   level, a string rank that means nothing to another registry — so a
   tenant sorts for itself. Cost if wrong: a caller wanting order sorts,
   one line.
2. **Fraction tolerance is `1d-6` absolute.** Fractions over a partition
   sum to 1.0 up to GEOS double-counting a shared boundary, which is far
   below that. Tests assert `(< (abs (- 1d0 sum)) 1d-6)`, never equality.

---

### Task 1: `geometry-geodesic-area`

Real area in m², spherical excess, no GEOS. `geometry-area` is no
substitute: it returns squared degrees, and a degree of longitude is a
different distance at every latitude.

**Files:**
- Modify: `geometry-ops.lisp` (append; `+earth-radius-m+` is already there
  at line 11)
- Modify: `package.lisp` (one export line beside `#:geometry-area`)
- Test: `tests/geometry-ops-tests.lisp` (append)

**Interfaces:**
- Produces, relied on by Task 4:
  `(geometry-geodesic-area g)` → `double-float`, m². Zero for `:point` and
  `:linestring`. Holes subtracted. Never signals for a well-formed
  geometry, and needs no add-on.

- [ ] **Step 1: Write the failing tests**

Append to `tests/geometry-ops-tests.lisp`. A one-degree square at the
equator is ~12,309 km²; the assertion is deliberately loose because the
point of the test is the magnitude and the units, not the seventh digit.

```lisp
(test geodesic-area-is-square-metres-not-square-degrees
  "⚠ GEOMETRY-AREA returns squared degrees, which is not a usable measure:
a degree of longitude is a different distance at every latitude, so a
ratio of two such areas is only accidentally right (design §8)."
  (let ((sq (make-geometry :polygon '(((0d0 0d0) (1d0 0d0) (1d0 1d0)
                                       (0d0 1d0) (0d0 0d0))))))
    (let ((m2 (geometry-geodesic-area sq)))
      (is (< 1.2d10 m2 1.3d10))
      (is (typep m2 'double-float)))))

(test geodesic-area-subtracts-holes
  (let* ((outer '((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 1d0) (0d0 0d0)))
         (hole '((0.25d0 0.25d0) (0.75d0 0.25d0) (0.75d0 0.75d0)
                 (0.25d0 0.75d0) (0.25d0 0.25d0)))
         (solid (make-geometry :polygon (list outer)))
         (holed (make-geometry :polygon (list outer hole))))
    (is (< (geometry-geodesic-area holed)
           (geometry-geodesic-area solid)))))

(test geodesic-area-of-a-point-or-line-is-zero
  (is (= 0d0 (geometry-geodesic-area (make-geometry :point '(1d0 1d0)))))
  (is (= 0d0 (geometry-geodesic-area
              (make-geometry :linestring '((0d0 0d0) (1d0 1d0)))))))
```

Check `make-geometry`'s argument shape against a neighbouring test in the
same file before running; use whatever that file already uses to build a
polygon. Do not invent a constructor.

- [ ] **Step 2: Run and confirm RED**

```bash
cd ~/work/vivace-graph-v3
nohup sbcl --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/test)' \
  --eval '(asdf:test-system :graph-db/test)' \
  > /tmp/reg-t1-red.log 2>&1 &
```

Expected: undefined function `GEOMETRY-GEODESIC-AREA`. Record what you saw.

- [ ] **Step 3: Implement**

Append to `geometry-ops.lisp`. This is a port of mine-action's
`%ring-area-m2` / `%polygon-area-m2` (`src/geojson.lisp:106-124`), which
uses the same `+earth-radius-m+` value, so the two agree to floating-point
precision rather than approximately.

```lisp
(defun %ring-geodesic-area-m2 (ring)
  "Unsigned spherical area in m^2 of RING, a closed list of (lon lat)
degree pairs.  Fewer than three vertices is zero, not an error."
  (let* ((v (coerce ring 'vector))
         (n (length v)))
    (if (< n 3)
        0d0
        (let ((total 0d0))
          (dotimes (i n)
            (let* ((p1 (aref v i))
                   (p2 (aref v (mod (1+ i) n)))
                   (lon1 (* (first p1) (/ pi 180d0)))
                   (lon2 (* (first p2) (/ pi 180d0)))
                   (lat1 (* (second p1) (/ pi 180d0)))
                   (lat2 (* (second p2) (/ pi 180d0))))
              (incf total (* (- lon2 lon1)
                             (+ 2d0 (sin lat1) (sin lat2))))))
          (abs (/ (* total +earth-radius-m+ +earth-radius-m+) 2d0))))))

(defun geometry-geodesic-area (g)
  "Area of G in SQUARE METRES, by spherical excess.  Zero for a :POINT or
:LINESTRING.  Holes are subtracted.  ⚠ Not GEOMETRY-AREA, which returns
squared coordinate units and needs GEOS; this needs neither (design §8)."
  (flet ((poly (rings)
           (if (null rings)
               0d0
               (- (%ring-geodesic-area-m2 (first rings))
                  (reduce #'+ (rest rings)
                          :key #'%ring-geodesic-area-m2
                          :initial-value 0d0)))))
    (ecase (geometry-kind g)
      (:polygon (poly (geometry-coordinate-pairs g)))
      (:multipolygon (reduce #'+ (geometry-coordinate-pairs g)
                             :key #'poly :initial-value 0d0))
      ((:point :linestring) 0d0))))
```

- [ ] **Step 4: Export it**

In `package.lisp`, beside `#:geometry-area` (line 322), add
`#:geometry-geodesic-area`.

- [ ] **Step 5: Run and confirm GREEN.** Report the count.

- [ ] **Step 6: Commit**

```bash
git add geometry-ops.lisp package.lisp tests/geometry-ops-tests.lisp
git commit -m "feat(geometry): geodesic area in square metres (#138)

GEOMETRY-AREA returns squared degrees, which is not a measure a
fraction can be built from.  Spherical excess, no GEOS.  [skip-docs]"
```

---

### Task 2: `precision-m` and `fraction` on every claim

**Files:**
- Modify: `spacetime/claim.lisp` (`+claim-shared-slots+`, ~line 51)
- Modify: `spacetime/package.lisp` (exports, beside `#:claim-geometry`)
- Test: `tests/spacetime/claim-tests.lisp` (append)

**Interfaces:**
- Produces, relied on by Tasks 5 and 6: readers `claim-precision-m` and
  `claim-fraction` on every claim class, defaults `nil` and `1.0d0`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/claim-tests.lisp`. `with-claim-graph` and
`ct-claim` already exist at the top of that file — use them, do not define
new ones.

```lisp
(test a-claim-carries-registration-outputs-with-defaults
  "⚠ On the SHARED slots, not a tenant's :EXTRA-SLOTS: unit 3's traversal
weights by fraction without knowing which tenant wrote the claim, so it
must read one accessor (design §2, cl-llm#13)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((c (make-ct-claim-binary :subject-namespace "s" :subject-key "k"
                                   :object-namespace "o" :object-key "ok"
                                   :relation "r" :producer "p"
                                   :standing :asserted)))
      (is (null (claim-precision-m c)))
      (is (= 1.0d0 (claim-fraction c))))))

(test registration-outputs-survive-a-round-trip
  (with-claim-graph (g)
    (let ((key "rt-1"))
      (make-ct-claim-binary :subject-namespace "s" :subject-key key
                            :object-namespace "o" :object-key "ok"
                            :relation "r" :producer "p"
                            :standing :asserted
                            :precision-m 12.5d0 :fraction 0.25d0)
      (let ((c (first (index-lookup g 'ct-claim
                                    '(subject-namespace subject-key)
                                    (list "s" key)))))
        (is (= 12.5d0 (claim-precision-m c)))
        (is (= 0.25d0 (claim-fraction c)))))))
```

Read the index-lookup call in `tests/spacetime/claim-identity-tests.lisp`
first and match its exact symbol qualification — the slot names are
interned in `graph-db.spacetime`, and unqualified symbols here name
different ones and silently miss the index.

- [ ] **Step 2: Run and confirm RED**

```bash
nohup sbcl --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(asdf:test-system :graph-db/spacetime-test)' \
  > /tmp/reg-t2-red.log 2>&1 &
```

Expected: unknown initarg or undefined function `CLAIM-PRECISION-M`.

- [ ] **Step 3: Implement**

In `+claim-shared-slots+`, after the `geometry` slot:

```lisp
    ;; Registration outputs (#138).  PRECISION-M is metres, a real
    ;; quantity that flows in both directions -- a source can be finer
    ;; than the region it joins to -- never a discount factor.
    (precision-m :initarg :precision-m :accessor claim-precision-m
                 :initform nil)
    (fraction :initarg :fraction :accessor claim-fraction
              :initform 1.0d0)
```

- [ ] **Step 4: Export** `#:claim-precision-m` and `#:claim-fraction` in
      `spacetime/package.lisp`.

- [ ] **Step 5: Run and confirm GREEN.** Report the count.

- [ ] **Step 6: Prove the defaults are load-bearing**

Change `fraction`'s initform to `nil`, re-run, and confirm
`a-claim-carries-registration-outputs-with-defaults` goes RED while the
round-trip test stays GREEN as a control. Restore, re-run, confirm green,
and confirm `git status` is clean. Report the observed red.

- [ ] **Step 7: Commit**

```bash
git add spacetime/claim.lisp spacetime/package.lisp \
        tests/spacetime/claim-tests.lisp
git commit -m "feat(spacetime): precision-m and fraction on every claim (#138)

A domain-neutral reader weights by fraction without knowing which tenant
wrote the claim, so it reads one accessor.  [skip-docs]"
```

---

### Task 3: the `:registration` facet's payload

**Files:**
- Modify: `spacetime/source.lisp` (`%check-facet`'s `(:registration value)`
  clause, ~line 154)
- Test: `tests/spacetime/source-tests.lisp` (append)

**Interfaces:**
- Produces, relied on by Task 5: a validated facet plist with keys
  `:registry` `:registry-namespace` `:relation` `:method` `:rule-version`
  `:precision-fn` `:confidence-fn`, or `:none`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/source-tests.lisp`, matching how the tests
already there assert on a bad facet (find the `:space` ones and copy their
shape — they use `signals` with the same condition this must use).

```lisp
(test a-registration-facet-declares-what-it-binds-to
  (finishes
    (%check-facet :registration
                  '(:registry ct-region :registry-namespace "reg"
                    :relation "registered-at" :method "centroid-within"
                    :rule-version "r/1" :precision-fn nil
                    :confidence-fn nil))))

(test registration-none-stays-supported
  "⚠ The map-less tenant declares :NONE and is what proves the spatial
facets are optional rather than merely defaulted (design §3)."
  (finishes (%check-facet :registration :none)))

(test a-registration-facet-missing-its-registry-is-refused
  (signals invalid-source-facet
    (%check-facet :registration
                  '(:relation "registered-at" :method "centroid-within"
                    :rule-version "r/1"))))
```

Replace `invalid-source-facet` with whatever condition the existing facet
tests expect — read one before writing this.

- [ ] **Step 2: Run and confirm RED.** The third test is the one that must
      fail: today the clause is `(:registration value)`, which accepts
      anything.

- [ ] **Step 3: Implement.** Replace the clause:

```lisp
        ;; #138 defines the shape #132 deliberately left opaque.
        ;; :PRECISION-FN and :CONFIDENCE-FN are required KEYS but may be
        ;; NIL -- a source with no measure of either says so explicitly
        ;; rather than by omission.
        (:registration
         (req-symbol :registry)
         (req-string :registry-namespace)
         (req-string :relation)
         (req-string :method)
         (req-string :rule-version)
         (req :precision-fn)
         (req :confidence-fn))
```

Read what `req`, `req-symbol` and `req-string` do before using them; if
`req` rejects a `nil` value, use whichever helper permits a present-but-nil
key, and say in your report which you used and why.

- [ ] **Step 4: Run and confirm GREEN.** Report the count.

- [ ] **Step 5: Commit**

```bash
git add spacetime/source.lisp tests/spacetime/source-tests.lisp
git commit -m "feat(spacetime): the registration facet gains a payload (#138)

#132 stored it verbatim and left the shape to this issue.  Every field
comes from what the spine's rules already pass.  [skip-docs]"
```

---

### Task 4: `register-geometry`

The computation, with no claim-writing and no source contract — so it is
testable on geometries alone.

**Files:**
- Create: `spacetime/register.lisp`
- Modify: `graph-db.asd` (add `register` to `graph-db/spacetime`'s
  components, after `resolve`)
- Modify: `spacetime/package.lisp` (exports)
- Create: `tests/spacetime/register-tests.lisp`
- Modify: `graph-db.asd` (add it to `graph-db/spacetime-test`, last)

**Interfaces:**
- Consumes: `geometry-geodesic-area` (Task 1).
- Produces, relied on by Task 5:
  `(register-geometry geometry registry &key graph)` →
  `(values registrations evaluated-p)`, where each registration is
  `(:region <node> :fraction <double-float>)`. `evaluated-p` is `nil` when
  the scan could not be run at all.

- [ ] **Step 1: Write the failing tests**

Create `tests/spacetime/register-tests.lisp`. Model the graph fixture on
`with-claim-graph` in `claim-tests.lisp`; declare a region vertex class
with a geometry slot and a spatial index the way
`tests/graph-spatial-tests.lisp` does — read both before writing.

```lisp
(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test a-point-registers-to-one-region-at-fraction-one
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-geometry :point '(1d0 1d0)) 'ct-region
                           :graph g)
      (is evaluated)
      (is (= 1 (length regs)))
      (is (= 1.0d0 (getf (first regs) :fraction))))))

(test a-polygon-registers-fractionally-to-every-region-it-overlaps
  "⚠ Registration is PARTIAL AND FRACTIONAL, not boolean (design §1)."
  (with-region-graph (g)
    (%make-region g "west" '((0d0 0d0) (1d0 0d0) (1d0 2d0) (0d0 2d0)
                             (0d0 0d0)))
    (%make-region g "east" '((1d0 0d0) (2d0 0d0) (2d0 2d0) (1d0 2d0)
                             (1d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry
         (make-geometry :polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0)
                                    (1.5d0 1.5d0) (0.5d0 1.5d0)
                                    (0.5d0 0.5d0))))
         'ct-region :graph g)
      (is evaluated)
      (is (= 2 (length regs)))
      (let ((sum (reduce #'+ regs :key (lambda (r) (getf r :fraction))
                                  :initial-value 0d0)))
        (is (< (abs (- 1d0 sum)) 1d-6)
            "the halves partition the subject, so the fractions sum to 1"))
      (dolist (r regs)
        (is (< 0.4d0 (getf r :fraction) 0.6d0))))))

(test a-subject-outside-every-region-registers-to-nothing
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 1d0)
                          (0d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-geometry :point '(50d0 50d0)) 'ct-region
                           :graph g)
      (is evaluated "an empty result is an ANSWER, not a failed scan")
      (is (null regs)))))

(test without-geos-a-polygon-refuses-and-a-point-still-registers
  "⚠ A bounding box is OVER-inclusive, so approximating here would bind
records to regions they never touch.  The point is the control: without
it this cannot tell 'refused correctly' from 'broken everywhere'
(design §6)."
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (let ((graph-db::*geos-available-p* nil))
      (multiple-value-bind (regs evaluated)
          (register-geometry
           (make-geometry :polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0)
                                      (1.5d0 1.5d0) (0.5d0 1.5d0)
                                      (0.5d0 0.5d0))))
           'ct-region :graph g)
        (is (not evaluated))
        (is (null regs)))
      (multiple-value-bind (regs evaluated)
          (register-geometry (make-geometry :point '(1d0 1d0)) 'ct-region
                             :graph g)
        (is evaluated "a point's candidates are exact with or without GEOS")
        (is (= 1 (length regs)))))))
```

Check the real name of `*geos-available-p*`'s home package before binding
it; if it is not special-bindable from the test package, set and restore it
in an `unwind-protect` instead, and say so in your report.

- [ ] **Step 2: Run and confirm RED.** Expected: `REGISTER-GEOMETRY`
      undefined.

- [ ] **Step 3: Implement**

Create `spacetime/register.lisp`:

```lisp
;;;; spacetime/register.lisp -- binding geometry to a registry.
;;;; Design: docs/superpowers/specs/2026-08-19-registration-design.md (#138).

(in-package #:graph-db.spacetime)

(defun %extended-geometry-p (g)
  (member (graph-db:geometry-kind g) '(:polygon :multipolygon :linestring)))

(defun register-geometry (geometry registry &key (graph graph-db:*graph*))
  "Registrations of GEOMETRY against REGISTRY's regions in GRAPH.

Two values: a list of (:REGION node :FRACTION double), and whether the
scan was EVALUATED at all.  A registration is PARTIAL AND FRACTIONAL: a
point takes fraction 1.0, a polygon or line takes its share of each
region it meets.  The list is UNORDERED -- 'most specific' is a tenant's
notion, so a tenant sorts (plan ruling 1).

⚠ Read (VALUES NIL NIL) as 'not answered', never as 'no region here'.
The scan is unevaluated when GEOS is absent for an extended geometry --
the index falls back to a COARSE bounding box, which is over-inclusive,
and a fraction cannot be computed at all -- or when GEOS rejects the
geometry as invalid, which is host-dependent (design §6)."
  (if (and (%extended-geometry-p geometry)
           (not graph-db::*geos-available-p*))
      (values nil nil)
      (handler-case
          (let ((subject-area (graph-db:geometry-geodesic-area geometry)))
            (values
             (loop for region in (graph-db:find-nodes-intersecting
                                  registry geometry :graph graph)
                   for g = (graph-db:node-geometry region)
                   when g
                     collect (list :region region
                                   :fraction
                                   (%overlap-fraction geometry g
                                                      subject-area)))
             t))
        ;; ONLY geos-error: broader would swallow the node-escape class
        ;; (GH #53).
        (graph-db:geos-error () (values nil nil)))))

(defun %overlap-fraction (subject region-geometry subject-area)
  "How much of SUBJECT falls within REGION-GEOMETRY, in [0,1].
A zero-area subject -- a point or a line -- is wholly wherever it is
found, so it takes 1.0 rather than dividing by zero."
  (if (zerop subject-area)
      1.0d0
      (/ (graph-db:geometry-geodesic-area
          (graph-db:geometry-intersection subject region-geometry))
         subject-area)))
```

`find-nodes-intersecting`'s first argument is a SCOPE. Read its docstring
in `spatial-query.lisp:133` and pass whatever scope form names "every live
node of class `registry`" — if that is not a bare class symbol, fix the
call and say so in your report. It tests *region-geometry INTERSECTS area*,
which is the right direction for containment; `find-nodes-within` tests the
opposite and must not be substituted (mine-action runbook §A12).

- [ ] **Step 4: Wire it in.** Add `(:file "register")` to
      `graph-db/spacetime`'s components after `resolve`, add
      `(:file "register-tests")` last in `graph-db/spacetime-test`, and
      export `#:register-geometry` from `spacetime/package.lisp`.

- [ ] **Step 5: Run and confirm GREEN.** Report the count.

- [ ] **Step 6: Prove the no-GEOS refusal is load-bearing**

Delete the `%extended-geometry-p` guard so a polygon takes the normal path
without GEOS. Re-run and confirm
`without-geos-a-polygon-refuses-and-a-point-still-registers` goes RED while
the three GEOS-present tests stay GREEN. Restore, re-run, confirm green and
a clean `git status`. Report the observed red.

- [ ] **Step 7: Commit**

```bash
git add spacetime/register.lisp spacetime/package.lisp graph-db.asd \
        tests/spacetime/register-tests.lisp
git commit -m "feat(spacetime): register-geometry, exact or not at all (#138)

Partial and fractional, never boolean.  Refuses rather than
approximating when GEOS is absent: a bounding box is over-inclusive, so
approximating binds records to regions they never touch.  [skip-docs]"
```

---

### Task 5: `register-node`

**Files:**
- Modify: `spacetime/register.lisp` (append)
- Modify: `spacetime/package.lisp` (export)
- Modify: `tests/spacetime/register-tests.lisp` (append)

**Interfaces:**
- Consumes: `register-geometry` (Task 4), the validated facet (Task 3), the
  promoted slots (Task 2).
- Produces, relied on by Task 6:
  `(register-node node &key graph registry-graph)` →
  `(values claims-written evaluated-p)`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/register-tests.lisp`. Declare a source class
with `def-source` carrying a real `:registration` facet — read
`tests/spacetime/source-tests.lisp` for how one is declared.

```lisp
(test registering-a-node-writes-one-claim-per-region
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (let ((n (%make-subject g "s-1" (make-geometry :point '(1d0 1d0)))))
      (multiple-value-bind (written evaluated) (register-node n :graph g)
        (is evaluated)
        (is (= 1 written)))
      (let ((c (first (index-lookup g 'ct-claim
                                    '(subject-namespace subject-key)
                                    (list "ct-src" "s-1")))))
        (is (= 1.0d0 (claim-fraction c)))
        (is (string= "registered-at" (claim-relation c)))))))

(test registering-the-same-node-twice-writes-one-claim
  "⚠ Idempotent on (subject-namespace subject-key relation object).  A
re-run of an ingest must not double a corpus (design §4)."
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (let ((n (%make-subject g "s-2" (make-geometry :point '(1d0 1d0)))))
      (register-node n :graph g)
      (register-node n :graph g)
      (is (= 1 (length (index-lookup g 'ct-claim
                                     '(subject-namespace subject-key)
                                     (list "ct-src" "s-2"))))))))

(test a-source-declaring-registration-none-writes-nothing
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (let ((n (%make-unregistered-subject g "u-1"
                                         (make-geometry :point
                                                        '(1d0 1d0)))))
      (multiple-value-bind (written evaluated) (register-node n :graph g)
        (is (zerop written))
        (is evaluated ":NONE is an answer, not an unevaluated scan")))))
```

- [ ] **Step 2: Run and confirm RED.**

- [ ] **Step 3: Implement.** Append to `spacetime/register.lisp`:

```lisp
(defun register-node (node &key (graph graph-db:*graph*)
                                (registry-graph graph))
  "Register NODE against its source contract's registry, writing one claim
per region.  Two values: claims written, and whether the scan was
evaluated (see REGISTER-GEOMETRY).

A source declaring :REGISTRATION :NONE writes nothing and reports an
EVALUATED scan -- structural absence, not an unanswered question.

⚠ NODE's slots are read under NODE's own graph and only PLAIN VALUES
cross out; a node object never leaves its graph's binding (GH #53).  The
claim write is registry-graph-local, which 3.0's single-graph write
transaction requires anyway."
  (let ((facet (source-registration (source-contract-for node))))
    (if (eq facet :none)
        (values 0 t)
        (let (geometry subject-key precision confidence)
          (let ((graph-db:*graph* graph))
            (setf geometry (graph-db:node-geometry node)
                  subject-key (%source-key node)
                  precision (%call-or-nil (getf facet :precision-fn) node)
                  confidence (%call-or-nil (getf facet :confidence-fn)
                                           node)))
          (multiple-value-bind (regs evaluated)
              (register-geometry geometry (getf facet :registry)
                                 :graph registry-graph)
            (if (not evaluated)
                (values 0 nil)
                (values (loop for r in regs
                              count (%upsert-registration-claim
                                     r facet subject-key precision
                                     confidence registry-graph))
                        t)))))))
```

`source-contract-for`, `source-registration` and the source's key slot are
whatever `spacetime/source.lisp` and `resolve.lisp` already provide — read
them and use the real names. `%call-or-nil` and
`%upsert-registration-claim` are yours to write; model the upsert on
mine-action's `upsert-spine-claim` (`src/spine-register.lisp:140-200`),
which looks the subject up through the declared index and filters in Lisp
on relation and object, since claims per subject are few.

- [ ] **Step 4: Run and confirm GREEN.** Report the count.

- [ ] **Step 5: Prove idempotency is real**

Make `%upsert-registration-claim` always insert. Confirm
`registering-the-same-node-twice-writes-one-claim` goes RED while
`registering-a-node-writes-one-claim-per-region` stays GREEN. Restore,
re-run, confirm green and clean `git status`. Report the observed red.

- [ ] **Step 6: Commit**

```bash
git add spacetime/register.lisp spacetime/package.lisp \
        tests/spacetime/register-tests.lisp
git commit -m "feat(spacetime): register-node writes the claims (#138)

Idempotent per (subject, relation, object).  Plain values cross the
graph binding; node objects never do (#53).  [skip-docs]"
```

---

### Task 6: the spine adopts it, verified against deployed data

**Files:**
- Modify: `~/work/mine-action/src/spine-schema.lisp` (drop `:extra-slots`)
- Modify: `~/work/mine-action/src/forensics-schema.lisp` and the other
  source declarations that should now carry a real `:registration` facet
- Modify: `~/work/mine-action/src/spine-register.lisp` (reduce to what the
  substrate does not do)
- Test: `~/work/mine-action/tests/` — follow that repo's conventions

**Interfaces:**
- Consumes everything above.

- [ ] **Step 1: Copy the deployed spine graph**

```bash
cp -a /data0/mine-action-dev/graphs/spine /tmp/spine-migration-check
```

⚠ Work only on the copy. The live server holds the original open; a second
opener is refused with `.dirty exists`, and running recovery on a live
graph is worse than the problem. Do not stop the server.

- [ ] **Step 2: Write the failing migration test**

A test that opens the copy, reads a persisted `spine-claim`, and asserts
`claim-precision-m` and `claim-fraction` return the values written under
the old tenant slots. It fails today because those accessors do not exist
in the tenant's package. Record the RED.

- [ ] **Step 3: Validate the geodesic area port against production values**

Over real spine geometries from the copy, assert
`graph-db:geometry-geodesic-area` agrees with
`mine-action::geodesic-polygon-area-ha` × 10000. Both use
`+earth-radius-m+` = 6371000d0 and the same spherical-excess formula, so
assert a **relative** difference below `1d-9`, not equality — and if it
does not hold, stop and report rather than loosening the tolerance to make
it pass.

- [ ] **Step 4: Drop the tenant's `:extra-slots`**

Remove `precision-m` and `fraction` from `def-claim-classes`'s
`:extra-slots` in `src/spine-schema.lisp`. Their accessors were bare
`precision-m` / `fraction` in the `mine-action` package; every call site
becomes `st:claim-precision-m` / `st:claim-fraction`. Find them all with
`git grep -n 'precision-m\|(fraction '` and fix each.

- [ ] **Step 5: Declare real `:registration` facets** on the sources that
      register today, replacing `:none`. Derive each field from what that
      source's rule already passes to `upsert-spine-claim`.

- [ ] **Step 6: Reduce `spine-register.lisp`** to what the substrate does
      not do. Its per-source rules keep their graph-crossing discipline and
      their own precision/confidence functions; the candidate query,
      fraction math and claim upsert come from the substrate. Delete what
      is now duplicated rather than leaving it beside the new path.

- [ ] **Step 7: Run every suite.** The mine-action Lisp suite, the engine
      suites, and the migration test. Report each count against the
      baseline you recorded.

- [ ] **Step 8: Commit** in `~/work/mine-action`, with docs — that repo's
      hook blocks a push whose source changed without documentation. The
      runbook's §A12 spatial-index note and any spine docs describing the
      registration path need updating in this commit.

---

### Task 7: engine documentation

**Files:**
- Modify: `README.md` and whichever `docs/` file documents the spacetime
  add-on — find it with `git grep -ln 'def-source\|claim-standing' docs/`

- [ ] **Step 1: Document** `geometry-geodesic-area` beside `geometry-area`,
      making the units difference explicit; the two new claim slots; the
      registration facet's shape; and `register-geometry` /
      `register-node`, including that `evaluated-p` false means *not
      answered* and that GEOS absence refuses rather than approximates.

- [ ] **Step 2: Run all engine suites** and confirm no count moved. Report
      them.

- [ ] **Step 3: Commit** without `[skip-docs]` — this is the documentation
      commit for the unit.

---

## Self-Review

**Spec coverage.** §1 → Task 4's fractional tests. §2 → Task 2. §3 →
Task 3. §4 → Tasks 4 and 5. §5 (query direction) → Task 4 Step 3's note.
§6 → Task 4's no-GEOS test and `geos-error` handler. §7 (cross-graph) →
Task 5's docstring and implementation. §8 → Task 1. §9 (no claim spatial
index) → nothing to build; no task creates one. §10 → Task 6. §11 → the
tests across Tasks 1-6. §12 → nothing built outside these.

**Placeholders.** None. Where a name must be read from existing code
rather than guessed — `make-geometry`'s argument shape, the facet
condition class, `find-nodes-intersecting`'s scope form, the source
contract accessors — the step says so explicitly and requires the
implementer to report what they found, rather than leaving a blank.

**Type consistency.** `register-geometry` returns
`(:region node :fraction double)` in Task 4 and Task 5 consumes exactly
that. `claim-precision-m` / `claim-fraction` are spelled identically in
Tasks 2, 5 and 6. `geometry-geodesic-area` returns m² in Task 1 and is
divided by an m² subject area in Task 4.

**Known gap, deliberate.** §13's line-traversal ordering is settled by
ruling 1 (unordered), so no task implements ordering. If a tenant needs it,
that is a tenant sort, not a substrate change.
