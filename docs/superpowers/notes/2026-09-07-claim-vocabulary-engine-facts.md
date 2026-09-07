# Engine API facts, verified for the claim vocabulary (#350)

A snapshot, not a maintained document. The recon pass for
kraison/vivace-graph#350 ("a vocabulary index over a claim family"):
nineteen numbered facts, each settled by reading the cited line in this
worktree. It is committed because it is the record of why the #350
tasks will say what they say -- §X is the finding -> correction map and
is read first. Its model is the epoch-axis note
(`docs/superpowers/notes/2026-09-05-epoch-axis-engine-api-facts.md`).

**Pinned to `5bcece8`** (`feat/claim-vocabulary`, which is the spec
commit). Every `file:line` will drift; the quoted forms are what to
match on, not the numbers. Paths are relative to the repo root.

**No image was run.** Every item is `source` evidence, read at the line
cited. Nothing here was executed, and nothing here needs to be: each
claim is a form quoted verbatim from the file it lives in. Where a
behaviour is only implied by the code (not stated in it), the item says
so in its own words.

Reading order:

- **§X -- spec assumptions that do not hold.** Three. X1 is the one that
  changes a ruling's reasoning; X2 and X3 will produce a plausible-
  looking wrong implementation if missed.
- **§A -- the index layer** (facts A1-A7).
- **§B -- the spacetime layer** (facts B8-B14).
- **§C -- tests** (facts C15-C18).
- **§D -- docs** (fact D19).
- **§E -- traps.**

Line lengths here exceed the repo's 80-column rule inside quoted forms
and tables. Left as verified rather than rewrapped by hand, which risks
silently corrupting a quoted form. Every code line is byte-for-byte; the
only edit is a `...` standing for lines dropped from a form's middle.

---

# §X -- spec assumptions that do not hold

## X1 (HEADLINE) -- index entries do NOT outlive their nodes; `ix-remove` runs at commit

**Spec assumes** (§2.3, and R7's justification): "Index entries outlive
their nodes as tombstones; a phantom name is a silent wrong answer."

**Reality: a soft delete removes the node's secondary-index entries on
the commit apply path.** `mark-deleted` is `delete-vertex` is
`delete-node`, which pushes a `tx-delete`; `apply-transaction` then runs
`apply-tx-writes-to-secondary-indexes`, whose `tx-delete` method calls
`%ix-release` twice. There is no tombstone left behind.

The whole chain, verified:

- `interface.lisp:108-118` -- `mark-deleted` dispatches to `delete-vertex`
  / `delete-edge`.
- `vertex.lisp:197-201` -- `delete-vertex` calls `delete-node`.
- `transactions.lisp:3017-3028` -- `delete-node` adds a `tx-delete` to the
  write set:

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
                         (write-set *transaction*)))))
```

- `index.lisp:611-628` -- the maintenance methods, verbatim:

```lisp
(defgeneric apply-tx-write-to-secondary-indexes (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defmethod apply-tx-write-to-secondary-indexes ((write tx-create) graph)
  (%ix-claim (node write) graph))

(defmethod apply-tx-write-to-secondary-indexes ((write tx-update) graph)
  (%ix-release (old-node write) graph)
  (unless (deleted-p (node write))
    (%ix-claim (node write) graph)))

;; tx-delete is a tx-update subclass; the node is marked deleted -> release only.
(defmethod apply-tx-write-to-secondary-indexes ((write tx-delete) graph)
  (%ix-release (old-node write) graph)
  (%ix-release (node write) graph))

(defun apply-tx-writes-to-secondary-indexes (writes graph)
  (dolist (write writes) (apply-tx-write-to-secondary-indexes write graph)))
```

- `transactions.lisp:2008` -- the call site, post-durability, inside
  `apply-transaction`, alongside the view / spatial / unique passes:
  `(apply-tx-writes-to-secondary-indexes writes graph) ; general ordered index`.
  The replication path has its own two call sites,
  `peer-streaming.lisp:1140` and `:1177`.
- `docs/general-index-design.md:125` says it outright:
  "Deleted nodes are filtered (`ix-remove` runs on delete, but guard reads
  anyway, like `edge-exists-p`)."
- `tests/index-tests.lisp:517-523` pins it (`delete-removes-from-index`).
- The two scan builders skip deleted nodes too:
  `rebuild-secondary-indexes` (`index.lisp:654` -- `(unless (deleted-p node)`)
  and `%build-index-for-spec` (`index.lisp:858`).

**Consequence for the plan.** R7's *conclusion* survives -- confirming
one live node per name is still worth doing -- but its *reason* is
wrong, and the plan must not tell an implementer that names accumulate
tombstones. The confirmation earns its place for three other reasons,
each real:

1. `index-lookup` keeps a `(not (deleted-p node))` guard anyway
   (`index.lisp:1009`), and so must the new walk.
2. Index membership is **live**, while `%node-by-id` resolves through
   `lookup-vertex`, which consults `*read-snapshots*`. Under an open
   `with-as-of` extent the resolved version can be deleted, or absent,
   while the live index entry stands (#345, `docs/time-travel.md`
   Bounds). That is exactly the case §4.5 already concedes.
3. A crash between the node write and the sidecar save falls back to
   `rebuild-secondary-indexes` (`graph.lisp:1051-1052`), which is
   authoritative -- but a partially-restored sidecar is left in place on
   purpose (`index.lisp:811-816`).

The spec's testing bullet "a hard-deleted claim's name disappears
without `:current`" is therefore testable and will pass -- but it passes
because `ix-remove` ran, not because the confirmation dropped a phantom.
A test that means to prove the confirmation non-vacuous cannot use
`mark-deleted` to create the phantom.

## X2 -- "holding the index's read lock ... the way `ix-lookup` does" -- `ix-lookup` holds no lock, and the macro is a no-op off ECL

**Spec assumes** (§2.1): "each hop is one seek and one entry read ...
holding the index's read lock per hop the way `ix-lookup` does."

**Reality: `ix-lookup` takes no lock of its own** (`index.lisp:488-498`,
quoted at A4). Whatever locking exists is inside `make-range-cursor` and
the public `cursor-next`, and on the heap skip-list backend
`with-sl-read-lock` expands to `(progn ...)` on every implementation but
ECL (`skip-list.lisp:41-43`):

```lisp
(defmacro with-sl-read-lock ((skip-list) &body body)
  #+ecl `(with-read-lock ((%sl-lock ,skip-list)) ,@body)
  #-ecl `(progn ,@body))
```

So on SBCL -- the platform this unit is built and gated on -- there is no
lock at all on the heap path. The correct instruction to an implementer
is *negative*: **call `make-range-cursor` + `cursor-next` exactly as
`ix-lookup` does and add no lock of your own.** Adding one would nest a
read lock, which `skip-list.lisp:35-40` says deadlocks on ECL:

```lisp
;;; The lock is taken ONCE at each public boundary and is NEVER nested -- nesting
;;; would deadlock this rw-lock: a write holder taking a read lock blocks on its
;;; own active writer, and a reader taking a second read lock can block behind a
;;; waiting writer while still holding the first.  So the internal navigation is
;;; lock-free: write ops call the %FIND-* cores, and the read-lock-held MAP-*
;;; scans step the lock-free %CURSOR-NEXT.
```

The property the walk actually gets is stated at
`skip-list-cursors.lisp:130-132`: "MAKE-RANGE-CURSOR and the public
CURSOR-NEXT each take the read lock themselves (per step), so the scan
is concurrency-safe though not an atomic snapshot." A multi-hop walk is
therefore **not** a snapshot: entries can be inserted or removed between
hops. For a vocabulary listing that is acceptable and matches
`claims-touching`'s own model, but the docstring must say it rather than
imply an atomic listing.

## X3 -- the object-side index is declared on BINARY, not on PARENT

**Spec assumes** (§4.1): "Namespaces: the arity-1 prefixes of
`claim-subject` (`:role :subject`) and of `claim-object` (`:role
:object`)" -- with no note that the two are reached through different
class names.

**Reality:** `claim-subject` and `claim-subject-relation` and
`claim-producer` are declared on `,parent`; `claim-object` is declared on
`,binary` (`spacetime/claim.lisp:440-450`, quoted at B8), and the macro's
own comment says why. `claims-touching` already threads this correctly
(`claim-query.lisp:350-362`, quoted at B10): the subject lookups pass
`(claim-family-parent family)`, the object lookup passes
`(claim-family-binary family)`.

Passing the parent for the object slots does not silently return NIL --
it **signals**. `%secondary-index-lookup` walks `class-precedence-list`
*upwards* (`index.lisp:940-945`), and `ct-claim-binary` is a *subclass*
of `ct-claim`, so it is never reached; `%slot-index-declared-p` refuses
at arity 2 (`index.lisp:958`); and `%def-index-declared-p` tests
`(subtypep class-name (index-spec-owner-name spec))`, i.e.
`(subtypep 'ct-claim 'ct-claim-binary)` -> NIL (`index.lisp:968-971`).
`%require-index` then signals `query-precondition-error`
(`index.lisp:985-988`). Loud, not silent -- but a plan that does not name
this will produce three functions that error on `:role :object`.

---

# §A -- the index layer

## A1 -- `slot-index`, its arity, and `%require-index`

`index.lisp:248-254`, the struct and every accessor a caller needs
(`slot-index-owner-name`, `-slot-names`, `-canonicalizers`,
`-skip-list`; constructor `%make-slot-index`, internal):

```lisp
(defstruct (slot-index (:constructor %make-slot-index))
  owner-name slot-names canonicalizers
  ;; The backing ordered map: a heap skip-list / B+ tree on an on-disk graph, a
  ;; MEM-SKIP-LIST on a memory-graph -- always ordered (needed for range).  Keyed
  ;; by the flat composite (v1 ... vn id) under %INDEX-COMP-LESSP, arity-aware
  ;; since Task 4 (GH #107).
  skip-list)
```

**There is no `slot-index-arity`.** Arity is computed at each use as
`(length (slot-index-slot-names six))` -- `index.lisp:416` (in
`%index-key`), `index.lisp:471` (in `%index-bounds`), `index.lisp:582`
(in `%slot-index-for`). A new primitive should do the same, not add a
slot.

`%require-index`, `index.lisp:973-988`, verbatim -- **NIL is a legitimate
answer**, not an error:

```lisp
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

Both existing consumers guard on it and return the empty answer:
`index-lookup` at `index.lisp:1003` (`(when six ...)` with the comment
"NIL => declared but empty => no matches") and `map-index` at
`index.lisp:1021`. `map-index-prefixes` must call FN zero times and
`index-count` must return 0 in that case.

`%secondary-index-lookup` (`index.lisp:929-945`) resolves an index rooted
at an *ancestor* by walking `class-precedence-list`; `%normalize-slots`
(`index.lisp:91-94`) accepts a bare symbol or a list, so
`slot-name` may be either at every entry point.

## A2 -- `%index-key`, `%index-bounds`, and the sentinel constants

`%index-bounds`, `index.lisp:456-486`, verbatim -- this is the whole
mechanism §2.1 rests on:

```lisp
(defun %index-bounds (six value prefix)
  "Low/high range-cursor bounds (as VALUES) for VALUE -- a canonical
component list from %INDEX-KEY, or a bare scalar at arity 1 -- against SIX.

Full arity: [VALUE+NULL-KEY, VALUE+MAX-KEY], an exact-tuple window (the
pre-Task-7 hardcoded pair).  Fewer components with PREFIX T:
[VALUE, VALUE padded with +MAX-SENTINEL+ per missing slot, +MAX-KEY+],
matching every stored tuple with VALUE as a leading prefix -- the low bound
needs no padding, since a shorter key already sorts below any longer key
sharing it (%INDEX-COMP-LESSP).  Fewer components with PREFIX NIL, or MORE
than the arity regardless of PREFIX, signals: a wrong-length value is
otherwise indistinguishable from an intended prefix and would silently
return a superset -- silent-wrong-answer is this project's dominant defect
class (GH #107)."
  (let* ((vals (if (listp value) value (list value)))
         (arity (length (slot-index-slot-names six)))
         (n (length vals)))
    (cond ((= n arity)
           (values (append vals (list +null-key+))
                   (append vals (list +max-key+))))
          ((and prefix (< n arity))
           (values vals
                   (append vals
                           (make-list (- arity n)
                                      :initial-element +max-sentinel+)
                           (list +max-key+))))
          (t (error 'query-precondition-error
                    :reason (format nil "Index on ~S has arity ~D; got ~D ~
value(s)~:[~; -- pass :PREFIX T for a prefix scan~]"
                                    (slot-index-slot-names six) arity n
                                    (< n arity)))))))
```

Concretely, on an arity-3 index `(ns key rel)`:

| call | low bound | high bound |
|---|---|---|
| `("ops" "e1" "at")` (full) | `("ops" "e1" "at" +null-key+)` | `("ops" "e1" "at" +max-key+)` |
| `("ops")` with `:prefix t` | `("ops")` | `("ops" :gmax :gmax +max-key+)` |
| `("ops" "e1")` with `:prefix t` | `("ops" "e1")` | `("ops" "e1" :gmax +max-key+)` |
| `("ops")` without `:prefix` | signals `query-precondition-error` | -- |
| four components, any `:prefix` | signals `query-precondition-error` | -- |

The high bound is what makes the hop of §2.1 step 2 work: a new cursor
opened *at* that high bound skips every tuple sharing the prefix (values
sort below `:gmax` -- `utilities.lisp:266-277`) and lands on the first
tuple of the next prefix.

`%index-key` (`index.lisp:405-426`) canonicalises a *query* value:

```lisp
  (let* ((arity (length (slot-index-slot-names six)))
         (cans (slot-index-canonicalizers six))
         (vals (if (= arity 1) (list value) value))
         (any nil)
         (key (loop for v in vals
                    for i from 0
                    collect (cond ((null v) +null-component+)
                                  (t (setf any t)
                                     (let ((c (nth i cans)))
                                       (if c (funcall c v) v)))))))
    (when (or any (< (length vals) arity)) key)))
```

Trap in the last line: at **full arity with every component null** it
returns NIL, and `index-lookup` treats NIL as "no matches"
(`index.lisp:1006`). A shorter all-null prefix *is* a real key
(`tests/index-tests.lisp:335-357` pins both halves).

Constants (`globals.lisp`):

| constant | value | line |
|---|---|---|
| `+null-key+` | 16 zero bytes | `globals.lisp:131-133` |
| `+max-key+` | 16 `255` bytes | `globals.lisp:134-136` |
| `+min-sentinel+` | `:gmin` | `globals.lisp:271` |
| `+max-sentinel+` | `:gmax` | `globals.lisp:272` |
| `+null-component+` | `:gnull` | `globals.lisp:277` |
| `+index-tuple+` | `255` | `globals.lisp:392` |

None of them is exported from `GRAPH-DB`; inside `index.lisp` they are
just there, and a test in `graph-db/test` writes
`graph-db::+null-key+` (`tests/index-tests.lisp:853`).

Head/tail keys, `index.lisp:256-262`:

```lisp
(defun %index-head-key (arity)
  "Lower sentinel key for an index of ARITY value components (GH #107)."
  (append (make-list arity :initial-element +min-sentinel+) (list +null-key+)))

(defun %index-tail-key (arity)
  "Upper sentinel key for an index of ARITY value components (GH #107)."
  (append (make-list arity :initial-element +max-sentinel+) (list +max-key+)))
```

Both are used only to build the ordered map's sentinels
(`index.lisp:279-296`), but they are exactly the "head key" / "tail key"
§2.1 asks the walk to start and end at, and they are ordinary functions
callable from the new code.

## A3 -- range cursors: one protocol, three backends, all yielding a SKIP-NODE

The generics, `cursors.lisp:6-11`:

```lisp
(defgeneric cursor-next (cursor &optional eoc))
(defgeneric cursor-prev (cursor &optional eoc))
(defgeneric make-cursor (index &key cursor-class &allow-other-keys))
(defgeneric make-values-cursor (index &key &allow-other-keys))
(defgeneric make-keys-cursor (index &key &allow-other-keys))
(defgeneric make-range-cursor (index start end &key &allow-other-keys))
```

**All three backends return an object whose key is read with `%sn-key`.**
That is the answer to the question the task asked: there is no per-backend
accessor to switch on. Verified one backend at a time:

- **heap skip list** -- `skip-list-cursors.lisp:76-92`; the cursor holds
  a real `skip-node`, and `%cursor-next` returns it (`:14-24`), so
  `(%sn-key node)` is the composite key with the id last:

```lisp
(defmethod make-range-cursor ((sl skip-list) start end &key &allow-other-keys)
  (let ((preds (make-array (%sl-max-level sl)))
        (succs (make-array (%sl-max-level sl))))
    (multiple-value-bind (node level-found preds succs)
        (find-in-skip-list sl start preds succs)
      ;; SUCCS[0] is the leftmost node whose key >= START. ...
      (declare (ignore node level-found preds))
      (when succs
        (make-instance 'skip-list-range-cursor
                       :node (aref succs 0)
                       :end end :skip-list sl)))))
```

  The range `:around` stops at `end` inclusive
  (`skip-list-cursors.lisp:68-74`), comparing with `%sl-comparison` /
  `%sl-key-equal` -- for a secondary index those are `%index-comp-lessp`
  / `%index-equal` (`index.lisp:279-286`).

- **memory skip list** -- `mem-skip-list.lisp:176-184` and `:159-168`;
  the nodes are the same `skip-node` struct (`mem-skip-list.lisp:6-11`
  says so), reached by `%msn-forward`, so `%sn-key` again:

```lisp
(defmethod make-range-cursor ((sl mem-skip-list) start end &key &allow-other-keys)
  (with-read-lock ((mem-skip-list-lock sl))
    (let* ((max (mem-skip-list-max-level sl))
           (preds (make-array max))
           (succs (make-array max)))
      (%mem-find sl start preds succs)
      ;; succs[0] is the leftmost node with key >= START (captures duplicates).
      (make-instance 'mem-sl-range-cursor
                     :node (aref succs 0) :end end :skip-list sl))))
```

- **B+ tree** -- `bplus-tree.lisp:929-933` seeks with `%bpt-leaf-at`
  (`:868-885`), and `cursor-next` (`:893-921`) returns
  `(%bpt-materialize tree entry)`, which **fabricates a skip-node so the
  key accessor stays the same** (`bplus-tree.lisp:887-891`):

```lisp
(defun %bpt-materialize (tree entry)
  "Build a SKIP-NODE from a leaf ENTRY so consumers (views/spatial/unique) read
%SN-KEY / %SN-VALUE unchanged."
  (%make-skip-node :key (first entry)
                   :value (funcall (%bpt-value-deserializer tree) (third entry))))
```

```lisp
(defmethod make-range-cursor ((tree bplus-tree) start end &key &allow-other-keys)
  (with-bpt-read-lock (tree)
    (multiple-value-bind (buf count next-addr idx) (%bpt-leaf-at tree start)
      (make-instance 'bplus-cursor :tree tree :buf buf :count count :index idx
                                   :next-addr next-addr :end end :bounded-p t))))
```

**`:eoc` protocol.** Every caller passes an explicit sentinel and stops
on `(eql node :eoc)` -- `index.lisp:496`, `:540`, `:547`, `:556`. The
memory backend compares with `eq` internally (`mem-skip-list.lisp:190`),
which agrees for a keyword.

**Key shape.** For a secondary index the composite is
`(v1 ... vn id)`: `ix-put` appends the id (`index.lisp:505`), `ix-lookup`
reads it back with `(car (last (%sn-key node)))` (`index.lisp:497`) and
`ix-map` splits it with `(butlast (%sn-key node))` /
`(car (last (%sn-key node)))` (`index.lisp:541-542`). A walk that wants
the first ARITY components takes `(subseq (butlast (%sn-key node)) 0 arity)`
-- `butlast` first, or the id lands in the prefix at arity = n.

**Locks.** See X2. `ix-lookup` wraps nothing; the heap `find-in-skip-list`
method takes the read lock (`skip-list.lisp:616-618`), the mem backend
takes its own, the B+ tree takes `with-bpt-read-lock`. Copy `ix-lookup`;
add nothing.

## A4 -- the query surface: signatures, line ranges, export site

Low level, all in `index.lisp`, all taking a `SLOT-INDEX` (not a graph):

| function | lines | signature |
|---|---|---|
| `ix-lookup` | 488-498 | `(six key &key prefix)` -> list of node ids |
| `ix-put` | 500-505 | `(six key id)` |
| `ix-remove` | 507-509 | `(six key id)` |
| `ix-map` | 511-551 | `(six fn &key start end)` -> calls FN with `(KEY ID)` |
| `ix-count` | 553-557 | `(six)` -> number of live entries, **by full scan** |

`ix-lookup` verbatim, the template for both new primitives:

```lisp
(defun ix-lookup (six key &key prefix)
  "List of node-ids whose indexed tuple matches KEY, the canonical component
list from %INDEX-KEY: an exact match when KEY supplies SIX's full arity, or --
with PREFIX T -- a range scan when KEY supplies fewer, matching every stored
tuple that starts with KEY.  NIL if none.  Bounds via %INDEX-BOUNDS (GH #107)."
  (multiple-value-bind (lo hi) (%index-bounds six key prefix)
    (let ((cur (make-range-cursor (slot-index-skip-list six) lo hi))
          (ids '()))
      (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
            do (push (car (last (%sn-key node))) ids))
      (nreverse ids))))
```

`ix-count` is **not** what `index-count` should be built on -- it is an
unbounded `make-cursor` scan of the entire index (`index.lisp:553-557`):

```lisp
(defun ix-count (six)
  "Number of live entries in SIX."
  (let ((cur (make-cursor (slot-index-skip-list six))) (n 0))
    (loop for node = (cursor-next cur :eoc) until (eql node :eoc) do (incf n))
    n))
```

Spec §2.1's "`index-count` is `ix-lookup`'s range, counted instead of
collected" is right, and the name `index-count` does not collide with
`ix-count`.

Graph level, all in `index.lisp`, all binding `(*graph* graph)`:

| function | lines | signature |
|---|---|---|
| `index-lookup` | 990-1011 | `(graph class-name slot-name value &key (collect-p t) prefix)` |
| `map-index` | 1013-1030 | `(fn graph class-name slot-name &key start end)` |
| `index-range` | 1032-1039 | `(graph class-name slot-name &key start end)` |

`map-index`, verbatim -- the shape the new `map-index-prefixes` should
mirror (FN first, graph second):

```lisp
(defun map-index (fn graph class-name slot-name &key start end)
  "Call FN on each live node of CLASS-NAME (and subclasses) whose SLOT-NAME is in
[START,END] (inclusive; open-ended when NIL), in ascending value order.  START/
END are scalars for a single-slot index; for a multi-slot index each is a
tuple (list of component values) -- full arity, or fewer components, which
bound only on the components given (GH #107).  Resolves ids in GRAPH."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => nothing to map
      (let ((skey (and start (%index-key six start)))
            (ekey (and end   (%index-key six end))))
        (ix-map six
                (lambda (key id)
                  (declare (ignore key))
                  (let ((node (%node-by-id id graph)))
                    (when (and node (not (deleted-p node)))
                      (funcall fn node))))
                :start skey :end ekey)))))
```

**Export site.** `package.lisp:466`, inside the index block that runs
`package.lisp:414-469`:

```lisp
           #:index-lookup #:index-range #:map-index
```

The two new symbols go on that line or immediately after it, before
`;; index-backed generator predicates for Prolog (GH #102)`
(`package.lisp:467`). Neither `index-count` nor `map-index-prefixes`
exists anywhere in the tree today (grep over `--include=*.lisp` returns
nothing).

## A5 -- `def-index` on a graph with existing instances: it scans

`def-index` (`index.lisp:882-904`) registers, then builds **now** if the
graph is open:

```lisp
  `(let ((spec (make-index-spec :owner-name ',owner-class
                                :slot-names (%normalize-slots ',slot)
                                :graph-name ',graph-name
                                :name ',name
                                :canonicalize ,(when canonicalize `',canonicalize))))
     (register-index-spec spec)
     (let ((g (lookup-graph ',graph-name)))
       (when g (%ensure-index-built g spec)))
     spec))
```

`%ensure-index-built` (`index.lisp:868-872`) is idempotent on the
registry key `(owner . slot-names)`:

```lisp
(defun %ensure-index-built (graph spec)
  "Build SPEC's index unless it already exists in GRAPH's registry (idempotent)."
  (let ((key (cons (index-spec-owner-name spec) (index-spec-slot-names spec))))
    (unless (and (secondary-indexes graph) (gethash key (secondary-indexes graph)))
      (%build-index-for-spec graph spec))))
```

`%build-index-for-spec` (`index.lisp:843-866`) **does scan existing
nodes**, typed, skipping deleted ones:

```lisp
    (flet ((index-node (node)
             (unless (deleted-p node)
               (let ((key (ignore-errors
                           (and (%tuple-indexable-p node slot-names)
                                (%index-tuple-key six node)))))
                 (when key (ix-put six key (id node)))))))
      (if (subtypep owner 'edge)
          (map-edges #'index-node graph :edge-type owner)
          (map-vertices #'index-node graph :vertex-type owner)))
```

For a **closed** graph it happens at open. `graph.lisp:1050-1055`:

```lisp
        ;; General ordered indexes: same reopen-or-rebuild story as unique.
        (unless (restore-secondary-index-roots graph)
          (rebuild-secondary-indexes graph))
        ;; Build any def-index'd index not covered by the sidecar (declared before
        ;; this graph existed, or added since the last close); no-op otherwise.
        (install-secondary-indexes graph)
```

`install-secondary-indexes` is `index.lisp:874-880`; the memory-graph
equivalent is `memory-graph.lisp:1124-1125`. `restore-secondary-index-roots`
reconciles each sidecar record against the live schema through
`%index-spec-declared-p` (`index.lisp:777-778`), so the new `(relation)`
declaration survives the reconciliation and its absence from an old
sidecar is exactly the "missing" case `install` builds. **This is R3's
"no migration" claim, and it holds.**

## A6 -- id resolution and the deleted filter

`%node-by-id`, `spatial-query.lisp:36-40` (note the `notinline`, which is
what lets a counting test intercept it by `fdefinition` swap):

```lisp
(declaim (notinline %node-by-id))
(defun %node-by-id (id graph)
  "Resolve a spatial-index id (uuid bytes) to its live node, or NIL."
  (or (lookup-vertex id :graph graph)
      (lookup-edge id :graph graph)))
```

`index-lookup`'s resolution loop, `index.lisp:1006-1011`:

```lisp
        (when key
          (dolist (id (ix-lookup six key :prefix prefix))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
                (if collect-p (push node result) (return-from index-lookup t))))))
        (when collect-p (nreverse result))))))
```

Two guards, both needed: `node` can be NIL, and a resolved node can be
`deleted-p`. `map-index` repeats them verbatim at `index.lisp:1026-1029`.
The confirmation step of R7 is this pair of guards, applied once per
name instead of once per row.

`lookup-vertex` is the MVCC-aware read: it consults `*read-snapshots*`
before the live head, which is why the index (live membership) and the
resolution (possibly a snapshot version) can disagree -- see X1
consequence 2.

## A7 -- when `ix-remove` runs, exactly

Answered in full at **X1**. In one line: at commit apply,
post-durability, from `apply-transaction` (`transactions.lisp:2008`) and
from the two replication apply paths (`peer-streaming.lisp:1140`,
`:1177`), via `apply-tx-write-to-secondary-indexes` on `tx-delete` /
`tx-update` (`index.lisp:617-625`), which call `%ix-release`
(`index.lisp:602-609`) -> `ix-remove` (`index.lisp:507-509`).

Entries never stay behind on the normal path. Inside an **open**
transaction nothing has been applied yet, so the index still holds the
pre-transaction membership -- which is precisely why `%overlay-transaction`
exists (B10, B11).

---

# §B -- the spacetime layer

## B8 -- the `def-claim-classes` index block, and where the fifth index goes

`spacetime/claim.lisp:435-450`, verbatim. **The `(relation)` index is
inserted after line 450**, inside the same `progn`, before the
`fmakunbound` at `:451`:

```lisp
       ;; Subject index on PARENT reaches both arities via SUBTYPEP.  Object
       ;; index on BINARY, where those slots live -- declaring it on PARENT
       ;; also works (%APPLICABLE-INDEX-DESCRIPTORS requires every named slot
       ;; to exist) but reads as a mistake.  PRODUCER index exists so the
       ;; regeneration sweep is not a full scan (design §4, plan note 2).
       (graph-db:def-index ,parent (subject-namespace subject-key)
           ,graph-name :name claim-subject)
       (graph-db:def-index ,binary (object-namespace object-key)
           ,graph-name :name claim-object)
       (graph-db:def-index ,parent (producer) ,graph-name
                           :name claim-producer)
       ;; (subject relation) queries ride this instead of filtering the
       ;; whole endpoint result caller-side (GH #302).
       (graph-db:def-index ,parent (subject-namespace subject-key
                                    relation)
           ,graph-name :name claim-subject-relation)
```

The four indexes, as a table:

| slots | owner | `:name` |
|---|---|---|
| `(subject-namespace subject-key)` | `,parent` | `claim-subject` |
| `(object-namespace object-key)` | `,binary` | `claim-object` |
| `(producer)` | `,parent` | `claim-producer` |
| `(subject-namespace subject-key relation)` | `,parent` | `claim-subject-relation` |

`home`, `unary` and `binary` are bound at `spacetime/claim.lisp:361-363`:

```lisp
  (let* ((home (symbol-package parent))
         (unary (intern (format nil "~A-UNARY" parent) home))
         (binary (intern (format nil "~A-BINARY" parent) home))
```

**`:name` is mandatory on the new declaration.** The macro's own block
says why, `spacetime/claim.lisp:412-421`:

```lisp
       ;; ⚠ EVERY DECLARATION THIS MACRO EMITS IS NAMED -- the value
       ;; constraint above included (GH #139, #140, #149).  This macro
       ;; emits schema on a tenant's behalf, and a LATER VERSION OF IT
       ;; CANNOT NAME WHAT AN EARLIER VERSION EMITTED.  Unnamed, identity
       ;; is (owner . slot-names), so changing what is declared here would
       ;; leave BOTH the old and the new spec live in every long-lived
       ;; image -- the stale unique rejecting writes the current schema
       ;; permits, the stale index built and maintained for nothing.
```

The slot symbols (`subject-namespace`, `subject-key`, `relation`,
`producer`) are `GRAPH-DB.SPACETIME`'s own -- `+claim-shared-slots+`,
`spacetime/claim.lisp:148-184`, whose closing docstring says "Symbols
live in this package so two claim families share one set of accessors".
`object-namespace` / `object-key` are `+claim-object-slots+`,
`spacetime/claim.lisp:314-319`.

## B9 -- `claim-family` and the unknown-family signal

`spacetime/claim.lisp:14-32`, verbatim:

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

(defvar *claim-families* (make-hash-table :test 'eq)
  "Parent class name -> CLAIM-FAMILY.")

(defun claim-family (parent)
  "The CLAIM-FAMILY registered for PARENT, or signal UNKNOWN-CLAIM-FAMILY."
  (or (gethash parent *claim-families*)
      (error 'unknown-claim-family :parent parent)))
```

Accessors: `claim-family-parent`, `-unary`, `-binary`, `-temporal-p`; all
four are exported (`spacetime/package.lisp:45-46`, `:77`).

`unknown-claim-family` is defined in `spacetime/claim.lisp:8-12` (not in
`conditions.lisp`), a `spacetime-error`:

```lisp
(define-condition unknown-claim-family (spacetime-error)
  ((parent :initarg :parent :reader unknown-claim-family-parent))
  (:report (lambda (c s)
             (format s "~S names no claim family; DEF-CLAIM-CLASSES first."
                     (unknown-claim-family-parent c)))))
```

`claims-touching` signals it **implicitly**, by calling `(claim-family
claim-class)` at `spacetime/claim-query.lisp:348`. The three new
functions get the same behaviour for free by doing the same, which is
what §4 means by "an unknown family signals what `claims-touching`
signals". Pinned by `tests/spacetime/claim-query-tests.lisp:42-45`.

## B10 -- `claims-touching`: lambda list and body map

Lambda list, `spacetime/claim-query.lisp:281-283`:

```lisp
(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during
                             relation limit offset as-of as-of-epoch)
```

Body, by line range (docstring runs `:284-336`):

| lines | what |
|---|---|
| 337-345 | argument checks: `(check-type role (member :subject :object :either))`, the two mutual-exclusion `error`s, `%refuse-epoch-axis` |
| 346-348 | `probe`, `family` = `(claim-family claim-class)` |
| 349-358 | subject candidates: `index-lookup` on `(claim-family-parent family)` with `'(subject-namespace subject-key relation)` when `:relation` is given, else `'(subject-namespace subject-key)` |
| 359-362 | object candidates: `index-lookup` on `(claim-family-binary family)` with `'(object-namespace object-key)` |
| 363-368 | the union, de-duped `:key #'graph-db:id :test #'equalp` |
| 371-379 | the `:as-of` / `:as-of-epoch` arms (per-claim version resolution) |
| 380-395 | the neither-axis arm: `%overlay-transaction` with the `admit` lambda |
| 396-400 | `:current` -> `remove-if-not` on `claim-current-p` (reaped claims kept) |
| 401-405 | `:at` / `:during` -> `%claim-validity-touches-p` |
| 406-412 | object-side `:relation` filter (no object relation index) |
| 413 | `(%paginate all limit offset)` |

The index reads, `:349-362`, verbatim -- note **no `:prefix`**: every
call supplies the full arity:

```lisp
         (want (list namespace key))
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

The overlay call site and its `admit` lambda, `:380-395`, verbatim -- the
model for the three new functions' transaction rule:

```lisp
            (t
             (setf all (%overlay-transaction
                        graph all family
                        (lambda (c)
                          (or (and (member role '(:subject :either))
                                   (equal namespace
                                          (claim-subject-namespace c))
                                   (equal key (claim-subject-key c))
                                   (or (null relation)
                                       (equal relation
                                              (claim-relation c))))
                              (and (member role '(:object :either))
                                   (typep c (claim-family-binary family))
                                   (equal namespace
                                          (claim-object-namespace c))
                                   (equal key (claim-object-key c)))))))))
```

`%paginate`, `spacetime/claim-query.lisp:246-254` -- `:limit`/`:offset`
are applied **last**, to the final filtered list, and the second value is
the "more past the cut" flag:

```lisp
(defun %paginate (list limit offset)
  "LIST cut to OFFSET/LIMIT; second value T when entries existed past the
cut (the REST envelope's one-past-the-cap rule, GH #302)."
  (let* ((start (min (or offset 0) (length list)))
         (rest (nthcdr start list)))
    (if limit
        (values (subseq rest 0 (min limit (length rest)))
                (> (length rest) limit))
        (values rest nil))))
```

`claim-keys` should reuse it verbatim: same two return values, same rule.

## B11 -- the commit view

All five live in `value-constraint.lisp` and are exported from `GRAPH-DB`
at `package.lisp:450`
(`#:make-commit-view #:view-node #:view-old-node #:view-writes`).

```lisp
(defstruct (commit-view (:constructor %make-commit-view (graph writes)))   ; :143-149
  ...
  graph writes)

(defun make-commit-view (graph &optional tx)                              ; :151-158
  "The view of GRAPH as TX will leave it; store-only when TX is NIL."
  (%make-commit-view
   graph
   (when tx
     (let ((h (make-hash-table :test 'equalp)))
       (dolist (w (writes tx) h)
         (setf (gethash (id w) h) w))))))

(defun view-writes (view)                                                 ; :160-163
  "The delta: this transaction's TX-WRITEs, or NIL in a store-only view."
  (let ((h (commit-view-writes view)))
    (when h (alexandria:hash-table-values h))))
```

**`view-writes` returns `TX-WRITE` objects, not nodes** -- one per
written id, of class `tx-create`, `tx-update` or `tx-delete`
(`tx-delete` is a `tx-update` subclass, `transactions.lisp:1021`). A
delete is *not* represented as an absence: it is a `tx-delete` whose
`node` carries `deleted-p` T. That is why `%overlay-transaction` calls
`view-node` on each write's id rather than reading `(node w)` --
`view-node` returns NIL for it (`value-constraint.lisp:179-189`):

```lisp
(defun view-node (view id)
  "The node ID names as it will be after commit: this transaction's
write of it (NIL if that write deletes it), else the store's vertex or
edge, else NIL.  A node created in this commit is found here, not missed
in the store (evaluator note §3; GH #155)."
  (let* ((h (commit-view-writes view))
         (w (and h (gethash id h))))
    (if w
        (let ((n (node w))) (and (not (deleted-p n)) n))
        (let ((g (commit-view-graph view)))
          (or (lookup-vertex id :graph g) (lookup-edge id :graph g))))))
```

`view-old-node` takes a **NODE, not an id** (`value-constraint.lisp:165-177`),
and returns NIL only for a create; for an untouched node it returns the
node itself. That is the create test `%overlay-transaction` uses.

`%overlay-transaction`, `spacetime/claim-query.lisp:256-279`, verbatim --
the whole transaction rule of §4.4 in nineteen lines:

```lisp
(defun %overlay-transaction (graph all family admit)
  "ALL as the open transaction will commit it (GH #324): a candidate the
transaction updated is replaced by its written version, one it deleted
drops out, and a claim of FAMILY it created that satisfies ADMIT is
added.  ALL itself outside a transaction.  The commit view is the same
\"store with this transaction's writes on top\" that validation reads."
  (let ((tx graph-db::*transaction*))
    (if (null tx)
        all
        (let* ((view (graph-db:make-commit-view graph tx))
               (out '()))
          (dolist (c all)
            (let ((v (graph-db:view-node view (graph-db:id c))))
              (when v (push v out))))
          (dolist (w (graph-db:view-writes view))
            (let ((n (graph-db:view-node view (graph-db:id w))))
              (when (and n
                         (typep n (claim-family-parent family))
                         (null (graph-db:view-old-node view n))
                         (funcall admit n)
                         (not (find (graph-db:id n) out
                                    :key #'graph-db:id :test #'equalp)))
                (push n out))))
          (nreverse out)))))
```

Note `(graph-db:id w)` on a *write*: `id` is defined on `tx-write`
(`make-commit-view` keys the table with it, `value-constraint.lisp:157`).

§4.4's "the adjustment walks `view-writes` once per call and is bounded
by the write set" is therefore implementable exactly as written -- but a
count adjustment cannot reuse `%overlay-transaction`, which returns a
*list of nodes*, not a delta. It needs its own `view-writes` pass over
the same view.

## B12 -- `claim-current-p`, `retract-claim`, and hard deletion

`spacetime/claim-query.lisp:469-474`:

```lisp
(defun claim-current-p (claim)
  "True while CLAIM is still believed: its transaction period is open, or
absent -- a claim predating the axis was never retracted.  NIL once
RETRACT-CLAIM has closed the period (GH #162)."
  (let ((e (claim-transaction-extent claim)))
    (or (null e) (bound-unknown-p (extent-end e)))))
```

`retract-claim` is `(claim &key (at (%st-now)))`,
`spacetime/claim-query.lisp:476-502`. Two facts a test writer needs:
it **joins an ambient transaction** when one is open and opens its own
otherwise (`:500-502`), and it returns the **saved copy**, not the
original -- an already-retracted claim comes back unchanged.

**There is no hard delete of a claim.** The only destructive path is
`delete-claims-by-producer` (`spacetime/claim-query.lisp:547-561`), which
calls `graph-db:mark-deleted` -- a soft delete that still removes index
entries (X1):

```lisp
    (dolist (c (graph-db:index-lookup graph (claim-family-parent family)
                                      '(producer) producer)
             n)
      (graph-db:mark-deleted c)
      (incf n))))
```

No test in `tests/spacetime/` calls `mark-deleted` on a single claim; the
sweep test (`tests/spacetime/claim-query-tests.lisp:93-102`) is the only
deletion coverage, and it asserts the deleted claims vanish from
`claims-touching`.

## B13 -- the export site in `spacetime/package.lisp`

`spacetime/package.lisp:53`:

```lisp
   #:claim-extent #:claims-touching
```

The three new names belong on the next line, inside the same
`;; claim (GH #131)` block (`:44-68`), with a `; GH #350` comment in the
style of the neighbouring `; GH #303` / `; GH #347` markers.

## B14 -- the error base class, and a refusal to model on

`spacetime-error` is **not defined in this repo** -- it comes from
`cl-temporal-extent` and reaches the package through `:use`.
`spacetime/conditions.lisp:1-7`:

```lisp
;;;; Conditions for graph-db/spacetime's claim and source layers.
;;;;
;;;; The temporal conditions -- SPACETIME-ERROR, INVALID-STANDING,
;;;; INVALID-BOUND, INVALID-EXTENT -- moved to cl-temporal-extent (#159)
;;;; and are inherited through the package's :USE.
```

It is re-exported at `spacetime/package.lisp:16`.

The refusal to model `:as-of` / `:as-of-epoch` on,
`spacetime/conditions.lisp:47-55`:

```lisp
(define-condition resolution-in-transaction (spacetime-error)
  ((namespace :initarg :namespace :reader resolution-in-transaction-namespace)
   (key :initarg :key :reader resolution-in-transaction-key))
  (:report (lambda (c s)
             (format s "RESOLVE-ENDPOINT ~S/~S was called inside a ~
read-write transaction.  Resolution can cross graphs, and a read-write ~
transaction is single-graph; resolve before opening it."
                     (resolution-in-transaction-namespace c)
                     (resolution-in-transaction-key c)))))
```

A closer precedent for a *query* refusal is `epoch-axis-unavailable`,
which subclasses the engine's typed query error so a server can answer
400 (`spacetime/claim-query.lisp:151-172`):

```lisp
(define-condition epoch-axis-unavailable
    (graph-db:query-precondition-error)
  ((graph-name :initarg :graph-name
               :reader epoch-axis-unavailable-graph-name))
  (:documentation "An :AS-OF-EPOCH read of a store with no system clock. ...
The parent's REASON is filled at the signal site. ..."))

(defun %refuse-epoch-axis (graph)
  "Signal EPOCH-AXIS-UNAVAILABLE unless GRAPH is attached to a clock. ..."
  (unless (graph-db:graph-system-clock graph)
    ...))
```

`query-precondition-error` is `globals.lisp:487-490`, exported at
`package.lisp:277` with its `-reason` reader. §4.5 says "signals an
`error`"; the tighter choice, matching this file's own precedent, is a
`query-precondition-error` (or a named subtype of it) so the rules
subsystem's "no facts" handling covers it -- the epoch condition's
docstring spells that trade-off out.

Both existing mutual-exclusion refusals in this file are plain
`(error "...")` calls, `spacetime/claim-query.lisp:341-344`:

```lisp
  (when (and at during)
    (error "Pass only one of :AT or :DURING, not both."))
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
```

---

# §C -- tests

## C15 -- `tests/index-tests.lisp`

- `(in-package #:graph-db/test)` at `:6`.
- Suite, `:93-97`:

```lisp
(def-suite index-suite
  :description "General ordered secondary index (:INDEX / def-index)."
  :in graph-db-suite)

(in-suite index-suite)
```

- Fixtures: `with-ix-graph` `:99-107` (on-disk, `:index-backend` selectable),
  `with-ix-di-graph` `:109-117` (def-index-only schema),
  `with-ix-memory-graph` `:774-781` (memory graph). `with-ix-graph`:

```lisp
(defmacro with-ix-graph ((g &key (backend :skip-list)) &body body)
  "A fresh on-disk graph named *IX-GRAPH-NAME* on BACKEND, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ix-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000 :index-backend ,backend)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))
```

- **A three-slot index already exists** -- the plan does not need to add
  one. `:30-37`:

```lisp
;; Multi-slot: the endpoint-identity shape (namespace, external-key) (GH #107).
(def-vertex ix-claim ()
  ((ns  :initarg :ns  :accessor ix-ns)
   (key :initarg :key :accessor ix-key)
   (rel :initarg :rel :accessor ix-rel))
  :graph-db-index-test)

(def-index ix-claim (ns key rel) :graph-db-index-test)
```

  A second, arity-2 `def-index` on the *same class* with a positional
  canonicalizer sits at `:56-57` -- so `ix-claim` carries two distinct
  indexes, `(ns key rel)` and `(ns key)`. A prefix walk test must name
  the slot list it means.

- Nodes are built and committed with `with-transaction` (imported into
  `graph-db/test`), one form per test; a `:prefix t` test in full,
  `:272-282`:

```lisp
(test prefix-lookup-finds-null-trailing-components
  "A prefix lookup must return a row whose components BEYOND the queried
prefix are null (stored as +NULL-COMPONENT+) -- otherwise storing nulls
rather than skipping them (Task 5) buys nothing (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel nil))
    (let ((hits (index-lookup g 'ix-claim '(ns key rel)
                              (list "ops" "e1") :prefix t)))
      (is (= 1 (length hits)))
      (is (string= "e1" (ix-key (first hits))))
      (is (null (ix-rel (first hits)))))))
```

- The arity refusals are pinned at `:255-262` and `:264-270`; the
  all-null prefix case at `:335-357`; the delete case at `:517-523`.
- The low-level cursor test -- the template for a seek-counting or
  ablation test -- is `:843-858`, and it drives `make-range-cursor` /
  `cursor-next` / `%sn-key` directly on a bare `make-secondary-skip-list`.
- Close-and-reopen tests: `:444-456`, `:540-554`, `:581-609`. All follow
  the same `make-graph` / `close-graph` / `open-graph` shape.
- Registered in `graph-db.asd:720` (`(:file "index-tests")`).

## C16 -- `tests/spacetime/`

- Suite: `tests/spacetime/suite.lisp:5-6`
  (`(def-suite spacetime-suite ...)`, **no `:in`** -- it is a root suite,
  unlike `index-suite`). Every claim test file opens with
  `(in-suite spacetime-suite)`.
- Family and fixture, `tests/spacetime/claim-tests.lisp:7-24`, verbatim:

```lisp
(defparameter *claim-graph-name* :graph-db-claim-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *claim-graph-name* graph-db::*schema-node-metadata*) nil))

(def-claim-classes ct-claim :graph-db-claim-test
  :extra-slots ((weight :initarg :weight :accessor ct-weight
                        :initform nil)))

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

  So the family is `ct-claim` / `ct-claim-unary` / `ct-claim-binary`, and
  `with-claim-graph` **always makes a fresh graph** -- it never reopens
  one. A "the index was built on open over pre-existing claims" test must
  spell out `make-graph` / `close-graph` / `open-graph` by hand.

- `make-u` / `make-b`, `tests/spacetime/claim-identity-tests.lisp:7-18`,
  verbatim (both default the namespace to `:ns`):

```lisp
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

  A third helper, `make-u-at`, adds `:recorded-at` (`:20-25`). Neither
  `make-u` nor `make-b` lets a caller vary the *namespace* -- a namespace
  listing test must call `make-ct-claim-unary` / `-binary` directly, or
  add its own helper.

- **Namespaces in use:** `:ns` everywhere (the two helpers above),
  `:other` only at `tests/spacetime/claim-query-tests.lisp:28` and
  `tests/spacetime/membership-tests.lisp:192`, `:region` throughout
  `tests/spacetime/epoch-tests.lisp:59-67`. They are **keywords**, not
  strings -- which decides the collation (see E5).

- Retraction, `tests/spacetime/claim-query-tests.lisp:236` and
  `:269`; the #324 transaction-visibility test in full,
  `tests/spacetime/claim-query-tests.lisp:226-258`:

```lisp
(test a-transaction-reads-its-own-retraction-and-assertion
  "GH #324: retract-then-assert in one WITH-TRANSACTION -- the idiom
RETRACT-CLAIM's docstring recommends -- reads back inside the
transaction as it will commit: the retracted claim no longer current,
the new one present, both in CLAIMS-TOUCHING and CLAIMS-BY-PRODUCER."
  (with-claim-graph (g)
    (with-transaction () (make-b :object "old"))
    (with-transaction ()
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"
                                       :role :subject :current t))))
        (retract-claim c)
        (is (null (claims-touching g 'ct-claim :ns "s1"
                                   :role :subject :current t)))
        (make-b :object "new")
        (let ((live (claims-touching g 'ct-claim :ns "s1"
                                     :role :subject :current t)))
          (is (= 1 (length live)))
          (is (equal "new" (claim-object-key (first live)))))
        (is (= 2 (length (claims-touching g 'ct-claim :ns "s1"
                                          :role :subject))))
        ;; The object side has no index on the new claim yet: the
        ;; overlay is what admits it.
        (is (= 1 (length (claims-touching g 'ct-claim :ns "new"
                                          :role :object))))
        (is (= 1 (length (claims-touching g 'ct-claim :ns "old"
                                          :role :object))))
        (is (null (claims-touching g 'ct-claim :ns "old"
                                   :role :object :current t)))
        (is (= 2 (length (claims-by-producer g 'ct-claim "rule-a"))))))
    (let ((live (claims-touching g 'ct-claim :ns "s1"
                                 :role :subject :current t)))
      (is (= 1 (length live)))
      (is (equal "new" (claim-object-key (first live)))))))
```

  The commented assertion at `:246-249` is the exact vocabulary case:
  a name introduced inside the transaction is invisible to the index and
  is admitted only by the overlay.

- Close/reopen of a **claim** graph exists in two places:
  `tests/spacetime/claim-query-tests.lisp:64-85`
  (`a-claim-carries-a-temporal-extent-across-a-reopen`) and
  `tests/spacetime/claim-transaction-tests.lisp:81-98` and `:195-220`.
  The shape, from the first:

```lisp
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (id nil))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () ...))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let* ((graph-db:*graph* g2) ...)
               ...)
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
```

- New test files are added to `graph-db.asd:598-614` (the
  `graph-db/spacetime-test` `:components` list, `:serial t`); a
  vocabulary file belongs after `(:file "epoch-tests")` at `:614`.

## C17 -- the two test packages, and the import rule

`tests/spacetime/package.lisp:8-9` **does `:use` the spacetime package**:

```lisp
(defpackage #:graph-db/spacetime-test
  (:use #:cl #:fiveam #:graph-db.spacetime)
```

It does **not** `:use #:graph-db`; it imports a short, explicit list --
`serialize`/`deserialize` at `:16`, `make-graph close-graph
with-transaction open-graph id lookup-vertex` at `:23-24`, `def-vertex
geometry make-point make-polygon make-linestring` at `:28-29` -- and the
file's own comment says the rest stays package-qualified:

```lisp
  ;; *GRAPH* and *SCHEMA-NODE-METADATA* stay package-qualified at the
  ;; call sites instead, matching GRAPH-DB/GEOS-TEST and GRAPH-DB/ALGORITHMS-
  ;; TEST's import lists ...
```

So new spacetime tests use `claim-namespaces` / `claim-relations` /
`claim-keys` unqualified once they are exported (B13), but must write
`graph-db:` (or add an import) for anything from the engine.

`tests/package.lisp` is the opposite: `(:use #:cl #:fiveam)` only
(`:11-12`), with a curated `:import-from #:graph-db` list (`:16` onward).
The region a new index export joins, `tests/package.lisp:276-283`:

```lisp
                #:def-index
                #:index-lookup
                #:index-range
                #:map-index
                #:def-unique
                ;; schema retraction (GH #139, #140)
                #:undef-index
                #:undef-unique
```

Cursor internals the low-level tests already import sit at
`tests/package.lisp:174-186` (`#:%sn-key`, `#:make-range-cursor`,
`#:cursor-next`, `#:make-cursor`).

`tests/README.md:23-34` states the rule and the failure mode:

> A newly exported symbol used unqualified in a test therefore does
> **not** fail at compile time if you forget to add it to that list -- it
> silently interns a *fresh* symbol in `graph-db/test`, and the failure
> only surfaces later as an undefined-function or undefined-variable
> error at fasl-load time (or a mysteriously-never-matching
> `eq`/`find-class`). This bit two separate implementers during this
> work.

## C18 -- running one test and each suite in a fresh image

**The two suites are run differently.** `index-suite` is a child of
`graph-db-suite` in `graph-db/test`
(`tests/index-tests.lisp:93-95`); the spacetime suite is a root suite in
`graph-db/spacetime-test` with its own runner. Both need
`graph-db::*system-directory*` bound, because FiveAM's `run` bypasses the
test system's `:perform`.

`run-spacetime-tests`, `tests/spacetime/suite.lisp:8-24`, verbatim -- it
binds the system directory itself, so it can be called directly:

```lisp
(defun run-spacetime-tests ()
  "Run the spacetime suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/spacetime)."
  (log:config :error)
  ;; Type-ids come from the system-wide registry, so every store this suite
  ;; opens needs a system directory (GH #186).  One for the whole run, which
  ;; is the shape a real system has: many stores, one registry.
  (let* ((system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'spacetime-suite)))
           (explain! results)
           (results-status results))
      ;; system-dir and all test scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run))))
```

`graph-db/test`'s equivalent is `run-tests` (`tests/suite.lisp:44-74`),
which runs the *whole* `graph-db-suite` -- too slow for iteration. Bind
the same three specials by hand instead; `make-temp-directory` is
`tests/suite.lisp:85-87`.

The two commands, adapted from
`docs/superpowers/plans/2026-09-07-time-travel-api.md:59-78` to this
worktree. Both suites, written once to a runner file outside the
worktree:

```lisp
;; /home/raison/work/vg-c3/vg-350-suites.lisp
;; The two suites this unit touches, CI-style, in a fresh image.
;; Mirrors RUN-TESTS (tests/suite.lisp): FiveAM's RUN bypasses the
;; test system's :perform, so the system directory is bound here.
(ql:quickload '(:graph-db/test :graph-db/spacetime-test) :silent t)
(in-package :graph-db/test)
(log:config :error)
(let* ((system-dir (make-temp-directory))
       (graph-db::*system-directory* (namestring system-dir))
       (graph-db::*type-registry* nil)
       (ok t))
  (unwind-protect
       (progn
         (let ((r (fiveam:run 'index-suite)))
           (fiveam:explain! r)
           (format t "~&== index-suite ~a~%"
                   (if (fiveam:results-status r) "PASS" "FAIL"))
           (unless (fiveam:results-status r) (setf ok nil)))
         (unless (graph-db/spacetime-test::run-spacetime-tests)
           (setf ok nil)))
    (graph-db-test-scratch:cleanup-scratch-run))
  (sb-ext:exit :code (if ok 0 1)))
```

```bash
cd /home/raison/work/vg-c3/.worktrees/claim-vocab
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3/vg-350-suites.lisp \
  > /home/raison/work/vg-c3/vg-350-suites.log 2>&1; echo "exit=$?"
grep -E "Did [0-9]+ checks|Fail:|^== " /home/raison/work/vg-c3/vg-350-suites.log
```

A single test while iterating, same fresh-image style (swap
`:graph-db/test` for `:graph-db/spacetime-test` and the package for a
spacetime test):

```bash
cd /home/raison/work/vg-c3/.worktrees/claim-vocab
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(in-package :graph-db/test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

Always read back `Did N checks` -- a suite whose tests were never
registered reports green with zero checks.

---

# §D -- docs

## D19 -- the four documentation targets

**`docs/general-index-design.md`** (204 lines). Headings:

| line | heading |
|---|---|
| 1 | `# General ordered index — design` |
| 7 | `## 1. What it is` |
| 22 | `## 2. Decisions (locked)` |
| 53 | `## 3. Data model` |
| 78 | `## 4. Declaration` |
| 99 | `## 5. Maintenance (apply, post-durability — no enforcement)` |
| 115 | `## 6. Query API (\`interface.lisp\` or \`index.lisp\`)` |
| 128 | `## 7. Persistence & reopen` |
| 146 | `## 8. Peer replication` |
| 153 | `## 9. File-by-file build plan` |
| 169 | `## 10. Build order (each step compiles + tests green before the next)` |
| 180 | `## 11. Test plan (\`tests/index-tests.lisp\`, mirror \`unique-constraint-tests.lisp\`)` |
| 188 | `## 11a. Known limitation: LAZY memory graphs` |
| 200 | `## 12. Deferred (designed-for, not built)` |

The distinct-prefix walk and `index-count` are query API, so the
subsection fits **inside §6, appended after line 127** (the last line of
§6 is `:125-126`, "Deleted nodes are filtered ... Descending v1 = collect
a range then `nreverse`"), as `### 6a. Distinct-prefix walk and counts`.
The precedent for an inserted lettered section is `## 11a` at `:188`.

Line `:125` is also the line X1 contradicts the spec with, and it stays
true -- but it is the natural place to add the one sentence about live
membership under a snapshot.

**`docs/vivace-graph-v3-doc.org`**. The subsection whose examples call
`claims-touching`:

- heading `*** Finding claims, and reading their extent` at **`:5760`**;
- the `claims-touching` examples at `:5766-5772`;
- the sentence naming `claim-subject-relation` at `:5794-5803`;
- the subsection ends at `:5907`, and the **next heading** is
  `*** The source onboarding contract: what a source declares about itself`
  at **`:5908`**.

So the "Vocabulary" subsection (§6 of the spec) is inserted between
`:5907` and `:5908`, at the same `***` level. Neighbouring `***`
headings for context: `*** Temporal claim families: a state series
(~:temporal t~)` `:5644`, `*** Regeneration: sweep, then insert, in two
transactions` `:5718`.

**`CHANGELOG.md`**: `## [Unreleased]` at `:12`, `### Added` at `:14`, and
the first bullet begins at **`:16`** (`- **Node-local time travel**
(#115, Phase C-3): ...`), with the second at `:30`. A new entry goes at
`:16`, pushing the rest down; the house style is a bolded lead-in, the
issue number, and a closing pointer to the doc that carries the detail.

**Spacetime docs listing the family's indexes.** `grep claim-subject-relation docs/`
finds exactly one *documentation* hit --
`docs/vivace-graph-v3-doc.org:5796` -- plus records under
`docs/superpowers/` (`notes/2026-09-04-rules-s1-engine-api-facts.md:198`
and `:1184`, `notes/2026-09-05-rules-s3-engine-api-facts.md:83`, `:94`,
`plans/2026-09-05-rules-s2-rule-record-and-run.md:2137`, and the #350
spec itself). Records are not maintained; only the `.org` sentence needs
the fifth index added to it. `docs/value-constraint-design.md:169`
mentions `claim-relation` the *slot*, not the index, and needs nothing.

---

# §E -- traps

**E1 -- `%index-bounds` signals on a short value without `:prefix t`.**
`index.lisp:482-486`. Every hop of the walk passes a *short* tuple, so
every `%index-bounds` call in the new code must pass `prefix` true. The
mirrored trap: passing **more** components than the arity signals even
*with* `:prefix t` (`tests/index-tests.lisp:264-270`). Compute arity from
`(length (slot-index-slot-names six))` and refuse an over-long `:start`
yourself, with the same typed condition.

**E2 -- `ix-map`'s open-ended path is a full scan.** `index.lisp:543-551`
falls back to `(make-cursor sl)` plus a filter whenever *either* bound is
NIL. The walk must never reach it: use `make-range-cursor` directly with
both bounds, as `ix-lookup` does, or use `ix-map` only with both `:start`
and `:end` supplied. §2.1's "It never uses `ix-map`'s open-ended path" is
an instruction, not an observation.

**E3 -- lock discipline: take none.** See X2. `make-range-cursor` and the
public `cursor-next` own whatever locking exists on each backend; an
outer `with-sl-read-lock` would nest and deadlock on ECL
(`skip-list.lisp:35-40`). The consequence to *document*: a multi-hop walk
is not an atomic snapshot (`skip-list-cursors.lisp:130-132`).

**E4 -- null components come back as `+null-component+`, not NIL.**
The write side stores `:gnull` (`%index-tuple-key`, `index.lisp:428-444`),
so a key read off a cursor holds `:gnull`, and §2.2's "the walk reports
such a prefix with NIL in that component" requires an explicit
substitution on the way out. Symmetrically, a caller's NIL in `:start`
must be mapped to `+null-component+` on the way in -- which is exactly
what `%index-key` does (`index.lisp:422`), so route `:start` through
`%index-key` rather than building the component list by hand.
`+null-component+` sorts above `+min-sentinel+` and below every real
value (`utilities.lisp:279-300`), so a null-leading prefix is the first
one the walk reports.

**E5 -- "index order" for a keyword namespace is `string<` on the
SYMBOL-NAME.** `utilities.lisp:307`:
`(:method ((x symbol) (y symbol)) (string< (symbol-name x) (symbol-name y)))`.
The test family's namespaces are keywords (`:ns`, `:other`, `:region`),
so the order is `"NS" < "OTHER" < "REGION"` -- uppercase names, not the
lowercase printed form, and a string namespace sorts *after* every
symbol one (`utilities.lisp:334-335`). Any ordering assertion must be
written against that, and the docstring should say "index order" rather
than "alphabetical".

**E6 -- the object index is on BINARY.** See X3. Use
`(claim-family-binary family)` for `'(object-namespace object-key)` and
`(claim-family-parent family)` for the other three, exactly as
`claims-touching` does (`claim-query.lisp:350-362`).

**E7 -- `%require-index` returns NIL for a declared-but-empty index.**
`index.lisp:973-988`. A brand-new `(relation)` index on a family with no
claims yet resolves to NIL, not to an empty `slot-index`. Guard it the
way `index-lookup` (`:1003`) and `map-index` (`:1021`) do; do not let it
fall through to a `%index-bounds` call on NIL.

**E8 -- `%index-key` returns NIL at full arity when every component is
null.** `index.lisp:426`. Harmless for a prefix walk (a short all-null
prefix *is* a real key, pinned at `tests/index-tests.lisp:335-357`) but a
trap for `index-count` at full arity: NIL means "matches nothing", 0, not
"scan everything".

**E9 -- a cursor key is arity+1 long; never derive the arity from it.**
The composite is `(v1 ... vn id)`, so `(length (%sn-key node))` is
`n + 1`. Take the arity from `(length (slot-index-slot-names six))`
(A1), never from a key read off a cursor -- a walk that used the key's
own length as the arity would pad one sentinel too many into the next
hop's high bound and skip a whole prefix. Extracting the prefix is
`(subseq (%sn-key node) 0 arity)` (safe for any `arity <= n`, since the
id sits at position `n`), or `butlast` first, matching `ix-map`'s
`(butlast (%sn-key node))` / `(car (last (%sn-key node)))` split
(`index.lisp:541-542`). `tests/index-tests.lisp:791-802` pins the
arity+1 property on the head and tail sentinels.

**E10 -- a seek-counting test cannot swap an `fdefinition`.**
`make-range-cursor` and `find-in-skip-list` are **generic functions**
(`cursors.lisp:11`, `skip-list.lisp:614`), so the counting wrapper §5
asks for is a `:before`/`:around` method on the backend class, or
`trace`, not an `fdefinition` swap. (`%node-by-id` *is* a plain function
and is declared `notinline` for exactly that purpose --
`spatial-query.lisp:36` -- which is the precedent that misleads here.)
Whatever mechanism is used, prove the probe fired and restore it
afterwards; a break that never lands looks identical to a vacuous test.

**E11 -- forward references / load order.** `index.lisp` is
`graph-db/core`, `(:file "index" :depends-on ("unique-constraint"
"spatial-query" "interface"))` at `graph-db.asd:139`; the cursor files
load far earlier (`cursors` `:80`, `skip-list-cursors` `:82`,
`mem-skip-list` `:83`, `bplus-tree` `:88`). `graph-db/spacetime` is a
separate system (`graph-db.asd:558-579`) depending on `:graph-db/core`,
`:serial t`, with `claim.lisp` before `claim-query.lisp`
(`graph-db.asd:566-569`). So: the engine primitives must be defined and
**exported from `GRAPH-DB`** before any spacetime file can call them
unqualified-through-`graph-db:`, and nothing in `index.lisp` may
reference a spacetime symbol. Two commits, engine first, is the safe
order.

**E12 -- the test package import rule.** See C17 and
`tests/README.md:23-34`. A new `graph-db` export used unqualified in
`tests/index-tests.lisp` **must** be added to `tests/package.lisp`'s
`:import-from #:graph-db` list (region `:276-283`), or it silently
interns a fresh symbol and fails only at fasl-load time. The spacetime
test package `:use`s `graph-db.spacetime`, so the three new spacetime
exports need no import there -- but anything from `graph-db` does.

**E13 -- `def-index` needs `:name`, and re-declaring replaces.**
`spacetime/claim.lisp:412-421` (quoted at B8) and
`%spec-identity` (`index.lisp:99-115`). An unnamed fifth index would be
identified by `(owner . slot-names)` and could not be re-shaped by a
later version of the macro. Name it `claim-relation`, matching the
existing four.

**E14 -- 80 columns, spaces only.** The repo's rule; `index.lisp` and
`spacetime/claim-query.lisp` both hold to it, and the docstrings of the
three new functions carry the `:current` rule, the transaction rule and
the one trap (names are live membership) -- which is a lot of prose to
fit. Break the form; put the long version in the `.org` chapter.

**E15 -- a stale FASL survives a broken edit.** After any edit that
changes a package export, run the suites in a *fresh* image (C18), never
in a REPL that already loaded the old system: ASDF will happily load a
stale FASL and report a correct tree as red, or a broken one as green.
