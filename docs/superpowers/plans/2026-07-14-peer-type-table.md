# Peer Type Table (Plan 6A — engine) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The hub ships an explicit `(kind, type-id, name, direct-super)` type table to peer devices in the auth-ok plist, so a non-Lisp peer can tell what a `type-id` MEANS instead of re-deriving it by luck.

**Architecture:** Two pure functions in `peer-streaming.lisp` (encoder + inverse parser) plus a one-line change to the auth-ok plist. The table rides as a single delimited **string** because the plist control channel is flat-only (`check-packet-plist` signals `PLIST-TOO-FANCY-ERROR` on a nested list) — the same reason id-lists ride as concatenated hex via `peer-ids->string`. The change is **additive and forward/backward compatible**: every plist read on both sides of the peer path is a bare `getf`, with no whitelist, no `destructuring-bind`, and no unknown-key rejection.

**Tech Stack:** Common Lisp (SBCL + ECL), FiveAM, ASDF. Repo `/Users/kraison/work/vivace-graph-v3`, branch `experiment`.

**Why this exists:** see `mine-action-android/docs/superpowers/specs/2026-07-14-vg-type-identity-and-read-layer-design.md` §2–3. Two verified engine facts drive it:

1. **Vertex and edge type-ids collide.** `next-vertex-id` and `next-edge-id` are separate counters, **both `:initform 1`** (`schema.lisp:15-16`, `get-next-type-id` at `:139-149`). Verified live: `((1 "find-of-type" :EDGE) (1 "ordnance-type" :VERTEX))`. A consumer MUST key on `(kind, id)`, never `id` alone.
2. **Type-ids come from `def-vertex`/`def-edge` evaluation order** and **no type NAME is ever on the wire** — tx packets carry the raw `uint16` and the receiver resolves it against its *own* schema (`vertex.lisp:65-68`). Two graphs agree only if they loaded byte-identical schema forms in the same order (`tests/peer-replication/schema.lisp:3` says exactly this). The Kotlin device has no `schema.lisp` to evaluate, so this table is the only thing that can close the gap.

**Style:** per `CLAUDE.md`, **indent Lisp with spaces only — never tabs.**

---

## File Structure

- **Modify** `peer-streaming.lisp` — add `%peer-split`, `%peer-type-direct-super`, `peer-type-table-string`, `peer-parse-type-table` next to the existing `peer-ids->string` / `peer-string->ids` helpers (~line 70); add `:type-table` to the auth-ok plist (line 412).
- **Create** `tests/peer-type-table-tests.lisp` — FiveAM suite, following the `tests/peer-lamport-tests.lisp` pattern.
- **Modify** `graph-db.asd` — register the new test file in the `graph-db/test` system.

No exports are needed: the new functions are internal, and `graph-db/test` reaches internals via the `graph-db::` prefix.

---

## Task 1: The encoder + parser

**Files:**
- Modify: `peer-streaming.lisp` (insert after `peer-string->ids`, which ends at line 70)
- Test: `tests/peer-type-table-tests.lisp` (created in Task 2)

This task is pure functions only — no wire change yet.

- [ ] **Step 1: Add the four functions to `peer-streaming.lisp`,** immediately after `peer-string->ids` (ends line 70) and before `peer-portable-string` (line 72):

```lisp
(defun %peer-split (char string)
  "Split STRING on CHAR.  Empty fields are preserved, so a trailing separator
yields a trailing empty string (which the type-table format relies on: an empty
DIRECT-SUPER field means \"roots directly at VERTEX/EDGE\")."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start (or pos (length string)))
        while pos
        do (setf start (1+ pos))))

(defun %peer-type-direct-super (name)
  "The downcased name of type NAME's direct graph superclass, or NIL when NAME roots
directly at VERTEX/EDGE.

This CANNOT come from NODE-TYPE-PARENT-TYPE: that slot holds :VERTEX or :EDGE -- the
KIND, not the superclass (schema.lisp:262 sets it from (LAST1 PARENT-TYPES)).  The
inheritance graph lives only in CLOS, never in the persisted schema.  FIND-GRAPH-PARENT-CLASSES
is also wrong here: it returns TRANSITIVE ancestors, and we want only the direct parent
so the consumer can rebuild the closure itself."
  (let* ((class (find-class name nil))
         (super (and class
                     (first (remove-if
                             (lambda (c)
                               (member (class-name c)
                                       '(vertex edge primitive-node node standard-object t)))
                             (class-direct-superclasses class))))))
    (and super (string-downcase (symbol-name (class-name super))))))

(defun peer-type-table-string (&optional (graph *graph*))
  "Encode GRAPH's node-type registry as ONE delimited string, for the plist control
channel.  A nested list would trip PLIST-TOO-FANCY-ERROR, so the table rides as a
single string -- the same dodge as PEER-IDS->STRING.

Format: records separated by #\\; , fields by #\\, :

    kind,id,name,direct-super

KIND is \"v\" or \"e\"; ID is the decimal type-id; NAME is the downcased type name;
DIRECT-SUPER is the downcased direct graph superclass, or EMPTY when the type roots
directly at VERTEX/EDGE.  Example:

    v,1,site,;v,2,ord-mine,ordnance-type;e,1,find-of-type,

Both KIND and ID are required: NEXT-VERTEX-ID and NEXT-EDGE-ID are separate counters
that BOTH start at 1 (schema.lisp:15-16), so a vertex type and an edge type routinely
share a numeric id.  A consumer must key on (KIND . ID), never ID alone.

The sub-tables are triple-keyed (id -> meta, symbol -> id, keyword -> id; see
UPDATE-NODE-TYPE, schema.lisp:208-222), hence the (INTEGERP K) filter -- without it
every type is emitted three times."
  (with-output-to-string (s)
    (let ((firstp t))
      (dolist (kind '(:vertex :edge))
        (let ((sub (gethash kind (schema-type-table (schema graph)))))
          (when sub
            (dolist (id (sort (loop for k being the hash-keys in sub
                                    when (integerp k) collect k)
                              #'<))
              (let ((meta (gethash id sub)))
                (when (node-type-p meta)
                  (if firstp (setf firstp nil) (write-char #\; s))
                  (format s "~A,~D,~A,~A"
                          (if (eq kind :vertex) "v" "e")
                          id
                          (string-downcase (symbol-name (node-type-name meta)))
                          (or (%peer-type-direct-super (node-type-name meta)) "")))))))))))

(defun peer-parse-type-table (string)
  "Inverse of PEER-TYPE-TABLE-STRING.  Returns a list of (KIND ID NAME SUPER), where
KIND is :VERTEX or :EDGE, ID an integer, NAME a string, and SUPER a string or NIL.
NIL or \"\" (an old hub that sends no table) parses to NIL."
  (unless (or (null string) (string= string ""))
    (loop for record in (%peer-split #\; string)
          collect (destructuring-bind (kind id name &optional (super "")) (%peer-split #\, record)
                    (list (if (string= kind "v") :vertex :edge)
                          (parse-integer id)
                          name
                          (if (string= super "") nil super))))))
```

- [ ] **Step 2: Verify it compiles and produces a sane table, against the LIVE image.**

The eval server is the fastest loop here. Kevin runs `tools/repl-up.sh` (SWANK on 4005, eval server on 4006). Then:

```bash
tools/lisp-eval.sh '(in-package :graph-db)' '(compile-file "peer-streaming.lisp")'
```

Expected: compiles with no errors (style-warnings about unused vars are fine).

If the REPL server is NOT running, fall back to a full load:

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db)' 2>&1 | tail -5
```

Expected: loads clean.

- [ ] **Step 3: Commit**

```bash
git add peer-streaming.lisp
git commit -m "feat(peer): encode/parse a (kind,id,name,super) type table for non-Lisp peers"
```

---

## Task 2: FiveAM tests for the encoder/parser

**Files:**
- Create: `tests/peer-type-table-tests.lisp`
- Modify: `graph-db.asd` (register the test file)

- [ ] **Step 1: Create `tests/peer-type-table-tests.lisp`:**

```lisp
;;;; The peer type table: (kind, type-id, name, direct-super) shipped to devices.
;;;;
;;;; A type-id is meaningless on its own.  Ids are handed out by DEF-VERTEX/DEF-EDGE
;;;; evaluation order (GET-NEXT-TYPE-ID), persisted per graph, and NO type NAME ever
;;;; crosses the wire -- a receiver resolves the raw uint16 against its OWN schema.  A
;;;; Lisp device gets away with that only because it evaluates the same schema.lisp; a
;;;; non-Lisp peer (the Kotlin/SQLite device) cannot.  So the hub ships the mapping.
;;;;
;;;; The load-bearing detail these tests pin down: NEXT-VERTEX-ID and NEXT-EDGE-ID are
;;;; SEPARATE counters that BOTH start at 1, so a vertex type and an edge type routinely
;;;; share a numeric id.  Any consumer keying on the id ALONE silently confuses them.

(in-package #:graph-db/test)

(def-suite peer-type-table-suite
  :description "The (kind,id,name,super) type table shipped in the peer auth-ok plist."
  :in graph-db-suite)

(in-suite peer-type-table-suite)

;;; No new schema: the g-* schema from graph-tests (loaded earlier in the system) is
;;; exactly the shape this needs -- two vertex types where G-EMPLOYEE subclasses
;;; G-PERSON, and two edge types.  Both id spaces therefore start at 1, which is what
;;; makes the collision assertion below meaningful.  WITH-TEST-GRAPH (suite.lisp:113)
;;; builds a graph of *INTEGRATION-GRAPH-NAME* carrying it.

(test type-table-round-trips
  "PEER-PARSE-TYPE-TABLE inverts PEER-TYPE-TABLE-STRING."
  (with-test-graph (g)
    (let* ((s (graph-db::peer-type-table-string g))
           (parsed (graph-db::peer-parse-type-table s)))
      (is (stringp s))
      (is (plusp (length parsed)))
      ;; every record is (kind id name super)
      (dolist (row parsed)
        (is (member (first row) '(:vertex :edge)))
        (is (integerp (second row)))
        (is (stringp (third row)))
        (is (or (null (fourth row)) (stringp (fourth row))))))))

(test type-table-carries-kind-because-ids-collide
  "REGRESSION GUARD.  NEXT-VERTEX-ID and NEXT-EDGE-ID both start at 1 (schema.lisp:15-16),
so a vertex type and an edge type share a numeric id.  The table must therefore be keyed
on (KIND . ID).  If this ever fails because the ids no longer collide, the KIND field is
STILL required -- do not 'simplify' it away."
  (with-test-graph (g)
    (let* ((parsed (graph-db::peer-parse-type-table (graph-db::peer-type-table-string g)))
           (vertex-ids (loop for row in parsed when (eq (first row) :vertex) collect (second row)))
           (edge-ids (loop for row in parsed when (eq (first row) :edge) collect (second row))))
      (is (plusp (length vertex-ids)))
      (is (plusp (length edge-ids)))
      ;; the collision is real: at least one id is used by BOTH a vertex and an edge
      (is (intersection vertex-ids edge-ids))
      ;; ...yet (kind . id) is unique
      (let ((keys (loop for row in parsed collect (cons (first row) (second row)))))
        (is (= (length keys) (length (remove-duplicates keys :test #'equal))))))))

(test type-table-reports-direct-superclass-only
  "A subclass reports its DIRECT parent; a root type reports NIL (not VERTEX/EDGE)."
  (with-test-graph (g)
    (let ((parsed (graph-db::peer-parse-type-table (graph-db::peer-type-table-string g))))
      (flet ((super-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        (is (string= "g-person" (super-of "g-employee")))
        (is (null (super-of "g-person")))
        (is (null (super-of "g-knows")))))))

(test type-table-survives-the-plist-channel
  "The whole point of encoding the table as a STRING: a nested list would trip
PLIST-TOO-FANCY-ERROR.  Serialize the actual auth-ok plist and read it back."
  (with-test-graph (g)
    (let* ((table (graph-db::peer-type-table-string g))
           (plist (list :peer-control :auth-ok :type-table table))
           (bytes (graph-db::serialize-packet-plist plist))
           (back (graph-db::deserialize-packet-plist bytes)))
      (is (eq :auth-ok (getf back :peer-control)))
      (is (string= table (getf back :type-table)))
      ;; and it still parses after the round trip
      (is (equal (graph-db::peer-parse-type-table table)
                 (graph-db::peer-parse-type-table (getf back :type-table)))))))

(test type-table-absent-parses-to-nil
  "An OLD hub sends no :type-table.  (getf plist :type-table) -> NIL must parse to NIL,
not signal -- that is the device's back-compat fallback path."
  (is (null (graph-db::peer-parse-type-table nil)))
  (is (null (graph-db::peer-parse-type-table ""))))
```

- [ ] **Step 2: Register the test file in `graph-db.asd`.**

In the `graph-db/test` system's `:components` list, add `(:file "peer-type-table-tests")` immediately after the existing `(:file "peer-conflict-tests")` line:

```lisp
               (:file "peer-rehome-tests")
               (:file "peer-conflict-tests")
               (:file "peer-type-table-tests")
               (:file "view-tests")
```

- [ ] **Step 3: Run the new suite, verify it passes**

```bash
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(in-package :graph-db/test)' \
     --eval '(fiveam:run! (quote peer-type-table-suite))' 2>&1 | tail -20
```

Expected: `Did N checks. Pass: N (100%) Fail: 0`.

**If `type-table-carries-kind-because-ids-collide` fails on the `intersection` check, do NOT delete the assertion** — investigate instead. The `g-*` schema has two vertex types (`g-person`, `g-employee`) and two edge types (`g-knows`, `g-likes`), and both counters start at 1, so ids 1 and 2 must appear in *both* kinds. A failure here means either the encoder is dropping a kind or the `(integerp k)` filter is wrong — it does not mean the assertion is too strict.

Note the test file must be registered **after** `graph-tests` in the `.asd` (it reuses that file's `g-*` schema); the position given in Step 2 satisfies this.

- [ ] **Step 4: Commit**

```bash
git add tests/peer-type-table-tests.lisp graph-db.asd
git commit -m "test(peer): type table round-trip, (kind,id) collision guard, plist channel"
```

---

## Task 3: Ship the table in the auth-ok plist

**Files:**
- Modify: `peer-streaming.lisp:412` (inside `make-peer-session-handler`)

- [ ] **Step 1: Change the auth-ok write.**

Current (line 412):

```lisp
                   (peer-write-plist (list :peer-control :auth-ok) socket)
```

Replace with:

```lisp
                   ;; The type table rides on auth-ok, not on the hello: this is
                   ;; POST-authentication, so an unauthenticated peer never sees the
                   ;; schema.  Additive and back-compatible -- every peer-path plist
                   ;; read on both sides is a bare GETF, so an old device simply never
                   ;; asks for this key, and a new device against an old hub gets NIL.
                   (peer-write-plist (list :peer-control :auth-ok
                                           :type-table (peer-type-table-string graph))
                                     socket)
```

**Use `peer-write-plist`, never `write-plist-packet`** — the wrapper coerces string values so SBCL doesn't emit `#A((n) BASE-CHAR . "...")`, which ECL's reader cannot parse (`peer-portable-string`, `peer-streaming.lisp:72-82`). The type table is a string, so this matters.

- [ ] **Step 2: Update the `make-peer-session-handler` docstring** (line 401-403) to mention it:

Current:

```lisp
  "Return a thunk handling one device connection on SOCKET (hub side): announce,
authenticate, serve one pull, then RECEIVE the device's push and re-home it, close.
Models MAKE-SLAVE-SESSION-HANDLER but stays distinct from it."
```

Replace with:

```lisp
  "Return a thunk handling one device connection on SOCKET (hub side): announce,
authenticate, ship the schema type table, serve one pull, then RECEIVE the device's
push and re-home it, close.  Models MAKE-SLAVE-SESSION-HANDLER but stays distinct
from it.

The auth-ok plist carries :TYPE-TABLE (PEER-TYPE-TABLE-STRING) so a non-Lisp peer can
resolve a raw type-id to a type NAME.  It is not otherwise recoverable: ids come from
DEF-VERTEX/DEF-EDGE evaluation order and no name crosses the wire."
```

- [ ] **Step 3: Verify the existing peer harness still passes.**

This is the real regression check — a live hub↔device sync over a socket, which is what would break if the plist were malformed or non-portable:

```bash
tests/peer-replication/run-peer-test.sh 2>&1 | tail -20
```

Expected: the harness's existing pass line (12/12 in previous runs), unchanged. The device is a Lisp peer that ignores `:type-table` entirely — which is exactly the "old device, new hub" case, so **this run also proves the backward compatibility.**

- [ ] **Step 4: Commit**

```bash
git add peer-streaming.lisp
git commit -m "feat(peer): ship the type table to devices in the auth-ok plist"
```

---

## Task 4: Full-suite regression on SBCL and ECL

The plist channel is the one place where SBCL↔ECL string representation has bitten before (`peer-portable-string` exists solely because of it), so **both** implementations must run.

- [ ] **Step 1: Full suite on SBCL**

```bash
sbcl --non-interactive --eval '(asdf:test-system :graph-db)' 2>&1 | tail -15
```

Expected: 0 failures (the suite was last green at 1956/0).

- [ ] **Step 2: Full suite on ECL**

```bash
ecl --eval '(asdf:test-system :graph-db)' --eval '(quit)' 2>&1 | tail -15
```

Expected: 0 failures.

If ECL fails to build on a cold fasl cache with `RUN-PROGRAM does not have a file handle`, warm it over a TTY first — this is a known environment quirk, not a code problem.

- [ ] **Step 3: Report the actual counts.** Paste the real pass/fail lines from both. Do not claim green without them.

- [ ] **Step 4: Commit (only if Step 1-3 required a fix; otherwise nothing to commit)**

---

## Self-Review Notes

- **The table is sent on EVERY auth-ok**, unconditionally. It costs ~1.2 KB per sync. That is deliberate: it means the device's normal path always has a fresh, authoritative table, and the *cached* table (with the digest guard) is only a fallback for an old hub. Do not "optimize" this into a conditional send keyed on a device-supplied version — that reintroduces the stale-table failure mode the digest guard exists to close.
- **The digest is NOT re-sent on auth-ok.** It is already in the hub hello (`peer-hub-handshake-plist`, `:schema-digest`), which the device reads first. The device pairs the received table with that digest when caching.
- **No engine-side consumer.** The Lisp device path (`peer-sync`, `peer-streaming.lisp:989-1006`) deliberately ignores `:type-table` — a Lisp peer has its own schema. `peer-parse-type-table` exists for the tests and for any future Lisp consumer; the real consumer is Kotlin (Plan 6B).
- **Not exported.** These are internal symbols; `graph-db/test` reaches them with `graph-db::`. If a future caller outside the system needs them, export then — not now (YAGNI).
- **Follow-on (Plan 6B, `mine-action-android`):** `TypeRegistry.kt` parses this string into the `types(kind, type_id, name, super)` table, rebuilds the subclass closure for `type_id IN (…)` expansion, and enforces the digest guard on the cached table.
