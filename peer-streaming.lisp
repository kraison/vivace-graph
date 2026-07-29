(in-package :graph-db)

;;; ===========================================================================
;;; Peer replication transport (WP-4 / WP-6 / WP-8).
;;;
;;; The network half of peer replication: a hub serves authority-scoped pulls to
;;; many devices; a device pulls its closed disclosable subgraph and applies it
;;; read-only.  Built ALONGSIDE -- never on top of -- the master/slave transport
;;; in transaction-streaming.lisp, which is left byte-for-byte as it is (design
;;; §1.3).  Reuses that file's framed-packet primitives verbatim
;;; (read-packet/write-packet, the 8-byte size framing, read/write-plist-packet,
;;; deserialize-tx-header-vector / deserialize-tx-write-vector).
;;;
;;; See docs/peer-replication-design.md (v2) and
;;; docs/peer-replication-branch-a-plan.md.  PT-n tags point at the §12
;;; pressure-test invariants.
;;;
;;; WIRE (peer-protocol v1).  Every entry is a self-size-framed packet; the type
;;; byte at offset 9 discriminates:
;;;   - peer-meta (#\M, 51 bytes, from transactions.lisp WP-0): origin-id, op-id,
;;;     lamport, op-class.  For op-class authored(0)/state-create(1) it is
;;;     followed by the ordinary tx packets ([tx-header][tx-write...]); the inner
;;;     tx machinery is reused unchanged.
;;;   - purge (#\U, below): a state-sync scope-exit -- a count then N*16-byte
;;;     node UUIDs (design §4/§7).
;;;   - plist (#\p): handshake, the :pull-end control marker, and the ack.
;;;
;;; OP CLASSES (design §4).  Branch A's pull ships only STATE-SYNC ops -- they
;;; assert current scope membership, are keyed by node UUID, are idempotent, are
;;; never deduped by op-id and never propagate back:
;;;   - state-create: a node currently in scope, shipped as a full create
;;;     (idempotent upsert by UUID -- so a re-pull simply refreshes it);
;;;   - purge: a node that LEFT scope, removed from the device (it still lives on
;;;     the hub).
;;; AUTHORED ops (op-id-deduped, cursor-advancing) are handled on the device
;;; apply side for forward-compat, but the cursor-resumed authored-op STREAM that
;;; emits them from the hub feed is a later increment (see PEER-PULL-PHASE);
;;; ongoing changes still converge here via membership reconciliation on each
;;; connection.
;;; ===========================================================================

(defvar *peer-protocol-version* 1
  "Peer wire protocol version, independent of *REPLICATION-PROTOCOL-VERSION*.")

;;; ---------------------------------------------------------------------------
;;; Small encodings: ids on the plist channel, and the purge packet.
;;; ---------------------------------------------------------------------------

(defun peer-id->hex (id)
  "A 16-byte node/origin id as a 32-char lowercase hex string (plist-safe)."
  (string-id id))

(defun peer-hex->id (hex)
  "Inverse of PEER-ID->HEX: a 32-char hex string back to a 16-byte id vector."
  (let ((v (make-byte-vector 16)))
    (dotimes (i 16 v)
      (setf (aref v i)
            (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun peer-ids->string (ids)
  "Encode a list of 16-byte ids as one 32-chars-per-id hex string.  The plist
control channel only carries scalars (a list value trips PLIST-TOO-FANCY-ERROR),
so id lists ride as a single concatenated string."
  (apply #'concatenate 'string (mapcar #'peer-id->hex ids)))

(defun peer-string->ids (string)
  "Inverse of PEER-IDS->STRING: split a concatenated 32-hex-per-id STRING into a
list of 16-byte ids."
  (loop for start from 0 below (length string) by 32
        collect (peer-hex->id (subseq string start (+ start 32)))))

;;; ---------------------------------------------------------------------------
;;; The type table: (kind, id, name, supers), for non-Lisp peers.  A FROZEN EXTERNAL
;;; CONTRACT -- see PEER-TYPE-TABLE-STRING for the format and the strict parse rules.
;;;
;;; A TYPE-ID is an opaque uint16 on the wire and NO type NAME ever crosses it --
;;; a receiver resolves the raw id against its OWN schema (vertex.lisp).  A Lisp
;;; device gets away with that only because it evaluates the same schema.lisp; a
;;; Kotlin/SQLite peer cannot.  So the hub ships the mapping in the auth-ok plist.
;;; ---------------------------------------------------------------------------

(defun %peer-split (char string)
  "Split STRING on CHAR.  Empty fields are preserved, so a trailing separator
yields a trailing empty string (which the type-table format relies on: an empty
SUPERS field means \"roots directly at VERTEX/EDGE\")."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start (or pos (length string)))
        while pos
        do (setf start (1+ pos))))

(defparameter *peer-type-name-reserved-chars*
  '(#\, #\; #\Space #\" #\\ #\Newline #\Tab)
  "Characters a type-table NAME may not contain.  #\\, and #\\; are the field and record
separators; #\\Space separates the supers within their field; the rest are excluded
because they are what a non-Lisp consumer's tokenizer, string literal or line reader is
most likely to choke on.  A CL symbol name may contain ANY character (|escaped syntax|)
and DEF-VERTEX constrains nothing, so this is checked at ENCODE time -- see
PEER-TYPE-TABLE-STRING.")

(defun %peer-type-direct-supers (name)
  "The downcased names of type NAME's direct graph superclasses, as a LIST of strings.
NIL when NAME roots directly at VERTEX/EDGE.

A LIST, not a single name: DEF-NODE-TYPE's docstring claims single inheritance but does
NOT enforce it -- it splices PARENT-TYPES straight into DEFCLASS -- so
(DEF-VERTEX M-UAV (M-HAZARD M-ASSET) ...) really does define a type that IS an M-ASSET
on the hub.  Emitting only the first parent would drop M-UAV from a device's \"all
m-assets\" closure while the hub kept it: silent, per-peer divergent wrong answers.

This CANNOT come from NODE-TYPE-PARENT-TYPE: that slot holds :VERTEX or :EDGE -- the
KIND, not the superclass (DEF-NODE-TYPE sets it from (LAST1 PARENT-TYPES)).  The
inheritance graph lives only in CLOS, never in the persisted schema.
FIND-GRAPH-PARENT-CLASSES is also wrong here: it returns TRANSITIVE ancestors, and we
want only the direct parents so the consumer can rebuild the closure itself."
  (let ((class (find-class name nil)))
    (when class
      (loop for super in (class-direct-superclasses class)
            unless (member (class-name super)
                           '(vertex edge primitive-node node standard-object t))
              collect (string-downcase (symbol-name (class-name super)))))))

(defun %peer-check-wire-name (name type what)
  "Signal unless NAME -- a name about to be emitted into the type table -- is
representable on the peer wire.  TYPE is the node-type SYMBOL whose row it came from and
WHAT names the field, so the error points at the schema element to fix.

The encoder is the ONLY possible defense.  A #\\, in a name splits it into two FIELDS
(the parser then throws); a #\\; splits it into two RECORDS, the first of which parses
SILENTLY as a bogus type.  Either way the table reaching a Kotlin/SQLite device is
corrupt and unrecoverable there, so a loud error on the hub is strictly better."
  (when (zerop (length name))
    (error "Node type ~S is UNREPRESENTABLE on the peer wire: its ~A is the empty string.  ~
The peer type table cannot encode it; rename the type."
           type what))
  (let ((bad (find-if (lambda (c) (member c *peer-type-name-reserved-chars*)) name)))
    (when bad
      (error "Node type ~S is UNREPRESENTABLE on the peer wire: its ~A ~S contains the ~
reserved character ~S.  The peer type table is a delimited external contract (see ~
PEER-TYPE-TABLE-STRING); a name may not contain a comma, a semicolon, a space, a double ~
quote, a backslash, a newline or a tab.  Rename the type."
             type what name bad))))

(defun %peer-type-table-rows (graph)
  "GRAPH's node types as a list of (KIND-STRING ID NAME SUPERS TYPE): vertex types then
edge types, each sorted by type-id.  SUPERS is a list of downcased names; TYPE is the
node-type symbol, carried only so a validation error can name the type the developer
actually wrote (the emitted NAME is downcased and may be the very thing at fault).

The schema sub-tables are triple-keyed (id -> meta, symbol -> id, keyword -> id; see
UPDATE-NODE-TYPE in schema.lisp), hence the (INTEGERP K) filter -- without it every type
appears three times."
  (loop for kind in '(:vertex :edge)
        for kind-string in '("v" "e")
        append (let ((sub (gethash kind (schema-type-table (schema graph)))))
                 (when sub
                   (loop for id in (sort (loop for k being the hash-keys in sub
                                               when (integerp k) collect k)
                                         #'<)
                         for meta = (gethash id sub)
                         when (node-type-p meta)
                           collect (let ((type (node-type-name meta)))
                                     (list kind-string
                                           id
                                           (string-downcase (symbol-name type))
                                           (%peer-type-direct-supers type)
                                           type)))))))

(defun %peer-validate-type-table-rows (rows)
  "Signal if ROWS cannot be encoded unambiguously: a name carrying a reserved character
or empty, or two types emitting the SAME name.  Returns ROWS.

Names are otherwise globally unique (each is a CLOS class name), but STRING-DOWNCASE is
NOT injective -- P-PERSON and |P-Person| are distinct classes that emit one name, as can
same-named symbols from different packages.  A device would then resolve one type-id's
name to the other type's row."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (row rows rows)
      (destructuring-bind (kind id name supers type) row
        (declare (ignore kind id))
        (%peer-check-wire-name name type "name")
        (dolist (super supers)
          (%peer-check-wire-name super type "superclass name"))
        (let ((other (gethash name seen)))
          (when other
            (error "Node types ~S and ~S are UNREPRESENTABLE together on the peer wire: ~
both emit the name ~S (STRING-DOWNCASE is not injective).  The peer type table maps ~
type-id -> name and a device cannot tell them apart; rename one."
                   other type name))
          (setf (gethash name seen) type))))))

(defun peer-type-table-string (&optional (graph *graph*))
  "Encode GRAPH's node-type registry as ONE delimited string, for the plist control
channel.  A nested list would trip PLIST-TOO-FANCY-ERROR, so the table rides as a
single string -- the same dodge as PEER-IDS->STRING.

*** THIS FORMAT IS A FROZEN EXTERNAL CONTRACT. *** It is parsed by non-Lisp peers (the
Kotlin/SQLite device).  PEER-PARSE-TYPE-TABLE is the reference implementation and its
strictness is part of the spec: match it, do not out-tolerate it.

Format: records separated by #\\; , fields by #\\, :

    kind,id,name,supers

KIND is \"v\" or \"e\"; ID is the decimal type-id; NAME is the downcased type name;
SUPERS is a SPACE-separated list of the downcased DIRECT graph superclass names, EMPTY
when the type roots directly at VERTEX/EDGE.  Example:

    v,1,site,;v,2,ord-mine,ordnance-type;v,3,m-uav,m-hazard m-asset;e,1,find-of-type,

SUPERS is a LIST because multiple inheritance WORKS: DEF-NODE-TYPE does not enforce the
single inheritance its docstring claims (see %PEER-TYPE-DIRECT-SUPERS).  A consumer
rebuilds the transitive closure itself from the direct edges.

*** CONSUMERS MUST TWO-PASS. ***  Read ALL rows first, THEN resolve superclass names.
Type-ids are STABLE across schema evolution, so this table is sorted by ID and is NOT
topologically sorted: inserting a superclass ABOVE an existing type produces a FORWARD
reference, e.g.

    v,1,t-b,t-a;v,2,t-a,

where T-B (id 1) names T-A (id 2), which has not been read yet.  A single-pass consumer
that resolves supers as it reads works fine until the first hierarchy refactor and then
breaks in the field.

STRICT PARSE RULES (the contract; PEER-PARSE-TYPE-TABLE enforces all of them):
  - every record has EXACTLY 4 fields -- neither 3 nor 5 is tolerated;
  - KIND is exactly \"v\" or \"e\" -- nothing else, and it is case-sensitive;
  - ID is decimal digits only, in 0..65535 (type-ids are (UNSIGNED-BYTE 16) on the wire);
  - NAME is non-empty and contains none of *PEER-TYPE-NAME-RESERVED-CHARS* (this encoder
    signals rather than emit one);
  - no record is empty -- no leading, trailing or doubled #\\; -- and no super is empty;
  - the WHOLE table may be empty (\"\", or the key absent): an old hub sends no table.

Both KIND and ID are required: NEXT-VERTEX-ID and NEXT-EDGE-ID are separate counters
that BOTH start at 1 (schema.lisp), so a vertex type and an edge type routinely share a
numeric id.  A consumer must key on (KIND . ID), never ID alone.

Signals an error if the schema cannot be represented -- see
%PEER-VALIDATE-TYPE-TABLE-ROWS.  That is deliberate: a corrupt table is undebuggable on
the device, so the failure belongs at the hub.  Note WHERE it lands: this runs on the
auth-ok path, so an unrepresentable schema fails every device CONNECTION, not the
offending DEF-VERTEX.  Loud and hub-side either way, but the traceback points at a
session, not at the schema form that caused it."
  (let ((rows (%peer-validate-type-table-rows (%peer-type-table-rows graph))))
    (with-output-to-string (s)
      (loop for row in rows
            for firstp = t then nil
            do (destructuring-bind (kind id name supers type) row
                 (declare (ignore type))
                 (unless firstp (write-char #\; s))
                 (format s "~A,~D,~A,~{~A~^ ~}" kind id name supers))))))

(defun %peer-parse-type-id (field record)
  "The ID field of a type-table RECORD as an integer: decimal digits only, 0..65535.
Type-ids are (UNSIGNED-BYTE 16) on the wire, so no sign, no whitespace and no bignum --
a consumer holding ids in a 16-bit integer must be able to read every id we emit."
  (let ((id (and (plusp (length field))
                 (every #'digit-char-p field)
                 (parse-integer field))))
    (unless (typep id '(unsigned-byte 16))
      (error "Malformed peer type table record ~S: id ~S is not a decimal integer in ~
0..65535 (type-ids are (UNSIGNED-BYTE 16))."
             record field))
    id))

(defun %peer-parse-type-supers (field record)
  "The SUPERS field of a type-table RECORD as a list of names.  Empty field -> NIL (the
type roots directly at VERTEX/EDGE)."
  (unless (string= field "")
    (let ((supers (%peer-split #\Space field)))
      (when (member "" supers :test #'string=)
        (error "Malformed peer type table record ~S: the supers field ~S has an empty ~
name (a leading, trailing or doubled space)."
               record field))
      supers)))

(defun %peer-parse-type-record (record)
  "One RECORD of a type table as (KIND ID NAME SUPERS).  Strict by design: this parser is
the SPEC the non-Lisp implementations are written against, so every laxness here is a
place a Kotlin parser will silently diverge from the hub."
  (when (string= record "")
    (error "Malformed peer type table: empty record (a leading, trailing or doubled #\\;)."))
  (let ((fields (%peer-split #\, record)))
    (unless (= 4 (length fields))
      (error "Malformed peer type table record ~S: expected exactly 4 comma-separated ~
fields (kind,id,name,supers), got ~D."
             record (length fields)))
    (destructuring-bind (kind id name supers) fields
      (let ((kind-key (cond ((string= kind "v") :vertex)
                            ((string= kind "e") :edge)
                            (t
                             ;; NOT a fallback to :EDGE.  Silently treating an unknown
                             ;; kind as an edge is exactly how a mangled record becomes a
                             ;; wrong answer instead of an error.
                             (error "Malformed peer type table record ~S: kind must be ~
\"v\" or \"e\", not ~S."
                                    record kind)))))
        (when (string= name "")
          (error "Malformed peer type table record ~S: the name field is empty." record))
        (list kind-key
              (%peer-parse-type-id id record)
              name
              (%peer-parse-type-supers supers record))))))

(defun peer-parse-type-table (string)
  "Inverse of PEER-TYPE-TABLE-STRING, and the REFERENCE PARSER for the frozen peer wire
format -- see that function's docstring for the format and the strict rules enforced
here.  Returns a list of (KIND ID NAME SUPERS), where KIND is :VERTEX or :EDGE, ID an
integer in 0..65535, NAME a string, and SUPERS a LIST of strings (NIL when the type roots
directly at VERTEX/EDGE).

Signals an error on any malformed record.  NIL or \"\" -- an old hub that sends no table
-- parses to NIL: that is the device's back-compat path, and the one laxness kept.

Order-independent: consumers MUST two-pass (read all rows, then resolve superclass
names), because the table is sorted by id and may carry forward references."
  (unless (or (null string) (string= string ""))
    (loop for record in (%peer-split #\; string)
          collect (%peer-parse-type-record record))))

(defun peer-portable-string (value)
  "Coerce a STRING VALUE to a general (CHARACTER) string; pass non-strings through.
The plist control channel serializes with *PRINT-READABLY* T (via
WITH-STANDARD-IO-SYNTAX), under which SBCL prints a BASE-STRING -- e.g.
(SYMBOL-NAME (GRAPH-NAME g)) -- as a specialized-array literal
#A((n) BASE-CHAR . \"...\"), which ECL's reader cannot parse (it CARs the
contents).  A (SIMPLE-ARRAY CHARACTER (*)) prints as a plain \"...\" on both, so
coercing makes the handshake/auth/ack plists cross SBCL<->ECL."
  (if (stringp value)
      (coerce value '(simple-array character (*)))
      value))

(defun peer-write-plist (plist socket)
  "WRITE-PLIST-PACKET with every string value coerced portable (PEER-PORTABLE-STRING),
so peer handshake/auth/ack plists survive a cross-implementation hop.  Keys are
keywords and pass through untouched."
  (write-plist-packet
   (loop for (k v) on plist by #'cddr collect k collect (peer-portable-string v))
   socket))

(alexandria:define-constant +peer-purge-type-code+ (char-code #\U))

(defun serialize-purge-packet (ids)
  "Build a self-framed purge packet for IDS (a list of 16-byte node UUIDs):
size(8) flags(1) type(1=#\\U) count(8) then count*16 raw uuid bytes."
  (let* ((n (length ids))
         (size (+ 8 1 1 8 (* 16 n)))
         (v (make-byte-vector size))
         (i 0))
    (serialize-uint64 v size i) (incf i 8)
    (setf (aref v i) 0) (incf i)                      ; flags (unused)
    (setf (aref v i) +peer-purge-type-code+) (incf i) ; type
    (serialize-uint64 v n i) (incf i 8)
    (dolist (id ids v)
      (replace v id :start1 i :end2 16)
      (incf i 16))))

(defun deserialize-purge-packet (packet)
  "Parse a purge packet, returning the list of 16-byte node UUIDs."
  (assert (= (aref packet 9) +peer-purge-type-code+))
  (let ((n (deserialize-uint64 packet 10))
        (i 18)
        (ids '()))
    (dotimes (k n (nreverse ids))
      (push (subseq packet i (+ i 16)) ids)
      (incf i 16))))

;;; ---------------------------------------------------------------------------
;;; Schema-version compatibility (WP-6 / PT-6 / design §14).
;;; The gate is SAME-MAJOR, not digest-equality: an additive (minor) drift still
;;; syncs degraded-safe; only a major mismatch rejects the pull.  schema-digest
;;; is still exchanged as a within-major integrity signal.
;;; ---------------------------------------------------------------------------

(define-condition peer-schema-incompatible-error (error)
  ((hub-version    :initarg :hub-version    :reader peer-schema-hub-version)
   (device-version :initarg :device-version :reader peer-schema-device-version))
  (:report (lambda (c s)
             (format s "Peer schema majors differ: hub ~S, device ~S"
                     (peer-schema-hub-version c) (peer-schema-device-version c)))))

(defun peer-schema-major (version) (first version))

(defun peer-schema-compatible-p (a b)
  "True iff schema versions A and B share a MAJOR (additive minor drift is fine)."
  (eql (peer-schema-major a) (peer-schema-major b)))

;;; ---------------------------------------------------------------------------
;;; Device registry (hub side).  Engine-side, app-populated entry holding the
;;; per-device clearance scope + cursors + manifest.  The app owns the policy
;;; (who maps to what scope); the engine only reads these to drive a pull.
;;; ---------------------------------------------------------------------------

(defstruct (peer-device (:constructor %make-peer-device))
  (origin-id nil)              ; 16-byte device origin UUID (registry key)
  (roots '())                  ; scope roots: list of vertices/ids for SCOPE-NODE-SET
  (scope nil)                  ; opaque device-scope handed to DISCLOSABLE-P
  (edge-types nil)             ; optional edge-type bound for the closure walk
  (key nil)                    ; per-device replication key (string); NIL = fall back to the hub's shared key
  (manifest '())               ; node-ids the device is KNOWN to hold (PT-4)
  (pull-cursor 0)              ; highest hub authored-feed seq the device applied
  (push-ack 0)                 ; highest device-seq the hub acknowledged (Branch B)
  (apply-cursor 0)             ; highest device-seq the hub applied (Branch B)
  (last-pushed-schema-version nil) ; drain-and-update barrier signal (§14)
  (lock (make-recursive-lock "peer-device")))

(defun ensure-device-registry (hub)
  "Return HUB's device registry, creating an equalp hash-table if unset."
  (or (device-registry hub)
      (setf (device-registry hub) (make-hash-table :test 'equalp))))

(defun find-peer-device (hub origin-id)
  "The PEER-DEVICE registered under ORIGIN-ID (a 16-byte id), or NIL."
  (let ((reg (device-registry hub)))
    (and reg (gethash origin-id reg))))

(defun register-peer-device (hub &key origin-id roots scope edge-types key
                                   (manifest '()))
  "Register (or replace) a device in HUB's registry.  The app calls this to grant
ORIGIN-ID a clearance SCOPE rooted at ROOTS; the engine consumes it on pull.  KEY is the
device's own replication key (per-device provisioning); NIL falls back to the hub's shared
REPLICATION-KEY at authentication.  Returns the PEER-DEVICE."
  (let ((reg (ensure-device-registry hub))
        (dev (%make-peer-device :origin-id origin-id :roots roots :scope scope
                                :edge-types edge-types :key key :manifest manifest)))
    (setf (gethash origin-id reg) dev)
    dev))

(defun unregister-peer-device (hub origin-id)
  "Remove ORIGIN-ID from HUB's device registry (revocation): it can no longer authenticate or
pull.  Targeted -- leaves every other device's manifest/cursors intact.  Returns T if present."
  (let ((reg (device-registry hub)))
    (and reg (remhash origin-id reg))))

;;; ---------------------------------------------------------------------------
;;; Hub: send a node as a state-sync create, and send a purge op.
;;; ---------------------------------------------------------------------------

(defun write-node-as-create (socket graph node)
  "Ship NODE to a device as a state-sync membership-create: a peer-meta packet
(op-class=state-create, op-id=node-UUID since state-sync is UUID-keyed, lamport
slot carries the node's commit epoch) followed by a single-write create tx.  The
node's data is materialized + serialized so the wire copy is self-contained."
  (maybe-init-node-data node :graph graph)
  (setf (bytes node) (serialize (data node)))
  (let* ((origin (or (origin-id graph) +peer-null-origin+))
         (epoch (or (commit-epoch node) 0))
         (write (make-instance 'tx-create :node node))
         (txh (make-instance 'tx-header :transaction-id epoch
                                        :writes (list write) :graph graph)))
    (write-packet (serialize-peer-meta origin (id node) epoch +peer-op-state-create+)
                  socket)
    (write-packet (tx-header-vector txh epoch) socket)
    (write-packet (tx-write-vector write) socket)))

(defun write-purge-op (socket graph ids)
  "Ship a state-sync PURGE for IDS as a single self-describing purge packet (#\\U).
A purge needs no peer-meta envelope: it is node-UUID-keyed state-sync (design §4)
-- no origin/op-id/lamport applies -- and the distinct packet type already routes
it on the device side."
  (declare (ignore graph))
  (write-packet (serialize-purge-packet ids) socket))

;;; ---------------------------------------------------------------------------
;;; Hub: the cursor-resumed authored-op stream (WP-4.3, delta mode).
;;;
;;; Membership reconciliation (below) ships ENTERED nodes as state-creates and
;;; PURGES departed ones, but does NOT re-ship a node the device already holds.
;;; The edits to those HELD nodes flow here instead: the hub re-reads its own
;;; replication feed (replication-*.log; a peer-graph journals every commit as
;;; [peer-meta][tx-header][writes], WP-0/WP-2) from the device's pull-cursor and
;;; forwards the in-scope authored writes.  This is the volume path -- it carries
;;; O(changes-since-cursor), not O(working-set), on every reconnect.
;;;
;;; WRITE-LEVEL disclosure filter (design §7): an authored transaction may touch
;;; several nodes; we forward only the writes whose node is HELD (in scope AND
;;; already on the device).  That is the export-side analog of the slave's
;;; RECONCILE-SLAVE-WRITES, keyed on scope membership.  Newly-entered nodes are
;;; NOT streamed (a membership-create ships their current state, so replaying
;;; their pre-entry history -- with stale MVCC epochs -- is avoided); departed
;;; nodes are NOT streamed (a purge removes them).
;;; ---------------------------------------------------------------------------

(defun peer-tx-write-node-id (wpkt)
  "The 16-byte node id of a serialized tx-write packet, read WITHOUT deserializing:
the (new) node begins at offset 11 (tx-write header) and its uuid at +10."
  (subseq wpkt 21 37))

(defun patch-tx-header-vector (txh-vec write-count write-size)
  "A copy of the tx-header packet TXH-VEC with WRITE-COUNT (offset 18) and
WRITE-SIZE (offset 26) rewritten -- used to re-frame a header after the
write-level disclosure filter drops some of its writes.  TX-ID (offset 10) and
the packet size are unchanged."
  (let ((v (copy-seq txh-vec)))
    (serialize-uint64 v write-count 18)
    (serialize-uint64 v write-size 26)
    v))

(defun stream-peer-authored-ops (socket graph held min-tx-id max-tx-id)
  "Forward the hub's in-scope authored ops with tx-id in [MIN-TX-ID, MAX-TX-ID]
to SOCKET.  HELD is an id-table of the node-ids the device holds AND still has in
scope; only writes touching those are forwarded (write-level disclosure filter).
Each surviving op is re-framed (peer-meta + patched tx-header + kept writes).
Ops with no in-scope write are skipped.

Reads the replication log tail concurrently with the hub appending to it; entries
with tx-id <= MAX-TX-ID were committed before the pull snapshot and are complete,
so any short/torn read is necessarily a later (tx-id > MAX) commit -- treated as
end-of-stream.  The log is strictly tx-id-ordered (FINALIZE-TX-PERSISTENCE assigns
the id and appends under the same manager lock), so we stop at the first tx-id >
MAX."
  (when (zerop (hash-table-count held))
    (return-from stream-peer-authored-ops))    ; nothing held -> nothing to stream
  (flet ((safe-read (in) (handler-case (read-stream-packet in) (error () nil))))
    (dolist (log (applicable-replication-logs min-tx-id graph))
      (with-open-file (in log :element-type '(unsigned-byte 8))
        (loop
          (let ((meta (safe-read in)))
            (unless meta (return))                 ; EOF / torn -> next log
            (let ((txh-vec (safe-read in)))
              (unless txh-vec (return))
              (let* ((txh (deserialize-tx-header-vector txh-vec))
                     (tx-id (transaction-id txh)))
                (when (> tx-id max-tx-id)
                  (return-from stream-peer-authored-ops))
                (let ((wpkts (loop repeat (write-count txh)
                                   for w = (safe-read in)
                                   while w collect w)))
                  (when (and (>= tx-id min-tx-id)
                             (= (length wpkts) (write-count txh)))
                    (multiple-value-bind (origin op-id lamport op-class)
                        (deserialize-peer-meta meta)
                      (declare (ignore origin op-id lamport))
                      (when (= op-class +peer-op-authored+)
                        (let ((keep (remove-if-not
                                     (lambda (w) (gethash (peer-tx-write-node-id w) held))
                                     wpkts)))
                          (when keep
                            (write-packet meta socket)
                            (write-packet (patch-tx-header-vector
                                           txh-vec (length keep)
                                           (reduce #'+ keep :key #'length))
                                          socket)
                            (dolist (w keep) (write-packet w socket))))))))))))))))

;;; ---------------------------------------------------------------------------
;;; Hub: handshake + authority-scoped pull.
;;; ---------------------------------------------------------------------------

(defun peer-hub-handshake-plist (graph)
  "What the hub announces on a new connection (schema-version split into plist-
safe integers; schema-digest carried as a within-major integrity signal)."
  (list :peer-protocol-version *peer-protocol-version*
        :name (symbol-name (graph-name graph))
        :schema-major (first (peer-schema-version graph))
        :schema-minor (second (peer-schema-version graph))
        :schema-digest (schema-digest (schema graph))))

(defun peer-authenticate-device (graph auth)
  "Validate a device AUTH plist against GRAPH (the hub): the same-major schema gate
(WP-6/PT-6), a known origin in the device registry, and the replication key.  The key
checked is the device's OWN key when it has one (per-device provisioning), else the hub's
shared REPLICATION-KEY (back-compat for un-keyed devices).  The device is looked up FIRST
so its key is known before the key check.  Returns the PEER-DEVICE, or signals.  Records the
device's last-pushed schema version for the drain-and-update barrier signal (§14)."
  (let ((hub-version (peer-schema-version graph))
        (dev-version (list (getf auth :schema-major) (getf auth :schema-minor))))
    (unless (peer-schema-compatible-p hub-version dev-version)
      (error 'peer-schema-incompatible-error
             :hub-version hub-version :device-version dev-version))
    (let* ((origin (peer-hex->id (getf auth :origin-id)))
           (device (find-peer-device graph origin)))
      (unless device
        (error 'invalid-auth-data-error))
      (let ((expected (or (peer-device-key device) (replication-key graph))))
        (unless (and expected (equal (getf auth :replication-key) expected))
          (error 'invalid-auth-data-error)))
      (setf (peer-device-last-pushed-schema-version device) dev-version)
      device)))

(defun peer-pull-phase (graph socket device &key full-resync-p (min-cursor 0))
  "Serve DEVICE one authority-scoped pull over SOCKET, in DELTA mode.  Under one
read snapshot, partition the device's closed disclosable subgraph (SCOPE-NODE-SET,
WP-5) against the stored manifest:
  - ENTERED (scope \\ manifest) -> ship as a state-create (vertices BEFORE edges,
    the closed rule) with current state;
  - HELD (scope ∩ manifest) -> NOT re-shipped; its edits since MIN-CURSOR stream
    via STREAM-PEER-AUTHORED-OPS (the cursor-resumed authored-op feed);
  - LEFT (manifest \\ scope) -> purge.
The pull frontier T (the snapshot's highest visible tx-id) rides on the pull-end
so the device can advance its pull-cursor to T even when no authored op touched a
held node.  FULL-RESYNC-P treats the manifest as empty (everything ENTERED -> a
full re-ship + purge of departed), the seed/recovery path.

Manifest advances ONLY on the device ack (PT-4), and INCREMENTALLY: manifest :=
(manifest ∪ acked-created) \\ acked-purged.  A dropped create isn't acked -> stays
ENTERED -> re-emitted; a dropped purge isn't acked -> stays -> re-purged
(fail-closed disclosure).  MVCC-epoch safety: a newly-ENTERED node ships current
state and is never streamed, so its pre-entry history (with stale epochs) is never
replayed onto the device."
  (let ((frontier 0))
    (with-read-snapshot (graph)
      ;; A reader with start-tx-id S sees commits with commit-epoch < S; so the
      ;; snapshot's frontier -- the highest tx-id it reflects -- is S-1.  The
      ;; snapshot lives in *READ-SNAPSHOTS*, not *TRANSACTION* (GH #53).
      (setf frontier (1- (start-tx-id (read-transaction graph))))
      (multiple-value-bind (vset eset)
          (scope-node-set graph (peer-device-roots device) (peer-device-scope device)
                          :edge-types (peer-device-edge-types device))
        (let ((scope-ids (make-id-table))
              (manifest (make-id-table))
              (held (make-id-table)))
          (flet ((mark (id node) (declare (ignore node)) (setf (gethash id scope-ids) t)))
            (maphash #'mark vset)
            (maphash #'mark eset))
          (unless full-resync-p
            (dolist (id (peer-device-manifest device)) (setf (gethash id manifest) t)))
          (maphash (lambda (id v) (declare (ignore v))
                     (when (gethash id manifest) (setf (gethash id held) t)))
                   scope-ids)
          ;; ENTERED -> membership-create, vertices before edges.
          (maphash (lambda (id node)
                     (unless (gethash id manifest) (write-node-as-create socket graph node)))
                   vset)
          (maphash (lambda (id node)
                     (unless (gethash id manifest) (write-node-as-create socket graph node)))
                   eset)
          ;; HELD nodes' edits since the cursor.
          (stream-peer-authored-ops socket graph held (1+ min-cursor) frontier)
          ;; LEFT -> purge.
          (let ((purges '()))
            (maphash (lambda (id v) (declare (ignore v))
                       (unless (gethash id scope-ids) (push id purges)))
                     manifest)
            (when purges (write-purge-op socket graph purges))))))
    (peer-write-plist (list :peer-control :pull-end :cursor frontier) socket)
    ;; Incremental manifest advance + pull-cursor, from the ack only (PT-4).
    (let ((ack (read-plist-packet socket)))
      (with-recursive-lock-held ((peer-device-lock device))
        (let ((mset (make-id-table)))
          (dolist (id (peer-device-manifest device)) (setf (gethash id mset) t))
          (dolist (id (peer-string->ids (or (getf ack :created) ""))) (setf (gethash id mset) t))
          (dolist (id (peer-string->ids (or (getf ack :purged) ""))) (remhash id mset))
          (setf (peer-device-manifest device)
                (loop for id being the hash-keys of mset collect id)))
        (when (getf ack :pull-cursor)
          (setf (peer-device-pull-cursor device) (getf ack :pull-cursor))))
      ack)))

(defun make-peer-session-handler (graph socket)
  "Return a thunk handling one device connection on SOCKET (hub side): announce,
authenticate, ship the schema type table, serve one pull, then RECEIVE the device's
push and re-home it, close.  Models MAKE-SLAVE-SESSION-HANDLER but stays distinct
from it.

The auth-ok plist carries :TYPE-TABLE (PEER-TYPE-TABLE-STRING) so a non-Lisp peer can
resolve a raw type-id to a type NAME.  It is not otherwise recoverable: ids come from
DEF-VERTEX/DEF-EDGE evaluation order and no name crosses the wire."
  (lambda ()
    (let ((*graph* graph))
      (handler-case
          (unwind-protect
               (progn
                 (peer-write-plist (peer-hub-handshake-plist graph) socket)
                 (let* ((auth (read-plist-packet socket))
                        (device (peer-authenticate-device graph auth)))
                   ;; The type table rides on auth-ok, not on the hello: this is
                   ;; POST-authentication, so an unauthenticated peer never sees the
                   ;; schema.  Additive and back-compatible -- every peer-path plist
                   ;; read on both sides is a bare GETF, so an old device simply never
                   ;; asks for this key, and a new device against an old hub gets NIL.
                   (peer-write-plist (list :peer-control :auth-ok
                                           :type-table (peer-type-table-string graph))
                                     socket)
                   (peer-pull-phase graph socket device
                                    :full-resync-p (and (getf auth :full-resync) t)
                                    :min-cursor (or (getf auth :pull-cursor) 0))
                   ;; B2d-2b: then receive the device's pushed authored ops and
                   ;; re-home each (§5), acking the highest device feed-seq applied.
                   (peer-receive-push graph socket device)))
            (ignore-errors (usocket:socket-close socket)))
        (error (c)
          (log:error "peer hub session error: ~A" c))))))

(defun peer-server-accept-loop (graph)
  "Hub accept loop: bind REPLICATION-PORT and hand each device connection to its
own session thread.  Mirrors SERVER-ACCEPT-LOOP (incl. the binary :element-type
on the listener so accepted sockets are binary on CCL)."
  (let ((port (replication-port graph))
        (address usocket:*wildcard-host*))
    (usocket:with-socket-listener (listener address port :reuse-address t
                                            :element-type '(unsigned-byte 8))
      (loop
        (when (stop-replication-p graph)
          (return))
        (when (usocket:wait-for-input listener :timeout 1 :ready-only t)
          (let ((socket (usocket:socket-accept listener
                                               :element-type '(unsigned-byte 8))))
            (log:debug "Peer hub got a connection: ~A" socket)
            (bordeaux-threads:make-thread
             (make-peer-session-handler graph socket)
             :name "peer hub session")))))))

;;; ---------------------------------------------------------------------------
;;; Device: the single-writer apply funnel (WP-8 / PT-5).
;;;
;;; The connection thread DECODES + ENQUEUES received ops; one writer thread
;;; drains the mailbox and performs every graph mutation.  A single writer means
;;; the device never contends on OCC (correct by construction on ECL, PT-5).
;;; ---------------------------------------------------------------------------

(defstruct (peer-op (:constructor make-peer-op))
  kind        ; :state-create :authored :purge :barrier :shutdown :local-write
  op-id origin lamport tx-id
  writes      ; list of tx-write (state-create / authored)
  ids         ; list of node-ids (purge)
  thunk       ; a 0-arg fn to run on the writer thread (:local-write) -- funnels a
              ; user/app write through the SAME single writer as replication apply
  reply)      ; reply mailbox (barrier / local-write)

;;; ---------------------------------------------------------------------------
;;; Branch B: apply an authored op THROUGH the merge resolver (B2d).
;;;
;;; When an incoming authored update touches a node this replica already holds and
;;; a MERGE-POLICY is set, resolve the divergence field-by-field (peer-merge.lisp)
;;; instead of overwriting.  This runs on the DEVICE pull-apply here and, later, on
;;; the hub push re-home -- conflicts are symmetric.  With no policy it is a plain
;;; overwrite (Branch A behaviour).
;;; ---------------------------------------------------------------------------

(defun peer-local-node (id graph)
  "The replica's live local vertex or edge with ID, or NIL."
  (or (lookup-vertex id :graph graph) (lookup-edge id :graph graph)))

;;; RECORD-PEER-CONFLICT / GET-PEER-CONFLICTS + the enumeration/resolution API now
;;; live in peer-merge.lisp (the durable, idempotent B3 conflict store).

(defun peer-merge-write (graph write lamport origin)
  "Resolve one incoming authored WRITE against the locally-held node.  Returns
(values FINAL-WRITE STAMP-UPDATES CONFLICTS): FINAL-WRITE is the tx-write to apply
(the incoming write, or -- for a divergent vertex update -- a merged tx-update off
the local current version); STAMP-UPDATES a list of (node-id slot lamport origin);
CONFLICTS a list of PEER-CONFLICT.  A create/delete/edge, or an update with no
policy or no local copy, applies verbatim and stamps every field with the incoming
(lamport, origin); a vertex update to a held node is merged field-by-field."
  (let* ((policy (merge-policy graph))
         (new (node write))
         (nid (id new))
         (local (and policy (typep write 'tx-update) (not (typep write 'tx-delete))
                     (vertex-p new) (peer-local-node nid graph))))
    (if (null local)
        (values write
                (loop for (slot . nil) in (data new) collect (list nid slot lamport origin))
                nil)
        (multiple-value-bind (merged-data stamps conflicts)
            (merge-authored-fields policy (type-of new) nid
                                   (data local) (data new) (data (old-node write))
                                   lamport origin (node-field-stamps graph nid))
          (let ((merged (copy new)))
            (setf (data merged) merged-data
                  (bytes merged) (serialize merged-data))
            (values (make-instance 'tx-update :node merged :old-node local)
                    (loop for (slot . stamp) in stamps
                          collect (list nid slot (car stamp) (cdr stamp)))
                    conflicts))))))

(defun apply-peer-create-writes (graph tx-id writes &optional origin)
  "Apply state-sync create WRITES on the device with no cursor advance (state-sync
is out of the authored-feed band -- it must not move the pull cursor).  Idempotent
upsert by node UUID: *ADD-TO-INDEXES-UNLESS-PRESENT-P* makes a refresh re-pull
safe.  TX-ID is the shipped node's hub commit epoch, used as the MVCC stamp.  ORIGIN
is the shipped node's origin, recorded for any :ORIGIN-scoped unique partition (#6)."
  (let ((*commit-epoch* tx-id)
        (*add-to-indexes-unless-present-p* t)
        (*peer-apply-origin* origin))
    (apply-tx-writes writes graph)
    (apply-tx-writes-to-views writes graph)
    (apply-tx-writes-to-spatial-index writes graph)
    (apply-tx-writes-to-unique-indexes writes graph)   ; #6: keep the device index complete
    (apply-tx-writes-to-secondary-indexes writes graph) ; general ordered index
    (reap-old-versions writes graph)
    ;; Keep the device's tx-id-counter above this pulled node's hub epoch so a later
    ;; LOCAL edit transaction can see (and thus modify) it (B2d-2b).
    (peer-observe-epoch graph tx-id)))

(defun apply-peer-authored-op (graph op)
  "Apply an AUTHORED op on the device: dedup by op-id (PT-1), apply, advance the
pull cursor, then record the op-id.  Returns T if applied, NIL if a duplicate.

NB (PT-3): full crash-atomicity of the op-id-index update with the apply lands
with the durable authored-op .txn path in the cursor-resumed-stream increment;
here state-sync (the only thing Branch A's pull emits) is UUID-idempotent and
needs no op-id dedup, so this path is exercised only once the hub emits authored
ops."
  (if (op-applied-p graph (peer-op-op-id op))
      nil
      (let ((*commit-epoch* (peer-op-tx-id op))
            (*add-to-indexes-unless-present-p* t)
            (*peer-apply-origin* (peer-op-origin op))   ; #6 :ORIGIN partition
            (lamport (peer-op-lamport op))
            (origin (peer-op-origin op))
            (final-writes '())
            (stamp-updates '())
            (conflicts '()))
        ;; B2d: resolve each write through the merge policy (a no-op overwrite when
        ;; no policy is set or the node isn't held / not a vertex update).
        (dolist (w (peer-op-writes op))
          (multiple-value-bind (fw stamps confs) (peer-merge-write graph w lamport origin)
            (push fw final-writes)
            (setf stamp-updates (nconc stamp-updates stamps))
            (setf conflicts (nconc conflicts confs))))
        (setf final-writes (nreverse final-writes))
        (apply-tx-writes final-writes graph)
        (apply-tx-writes-to-views final-writes graph)
        (apply-tx-writes-to-spatial-index final-writes graph)
        (apply-tx-writes-to-unique-indexes final-writes graph)   ; #6: index pulled nodes
        (apply-tx-writes-to-secondary-indexes final-writes graph) ; general ordered index
        (reap-old-versions final-writes graph)
        ;; Persist the per-field stamps of the fields that took a new value, and
        ;; retain any surfaced conflicts.
        (dolist (s stamp-updates)
          (destructuring-bind (nid slot lam org) s
            (set-node-field-stamp graph nid slot lam org)))
        (dolist (c conflicts)
          (setf (peer-conflict-op-id c) (peer-op-op-id op))
          (record-peer-conflict graph c))
        (persist-peer-pull-cursor (peer-op-tx-id op) graph)
        (peer-observe-epoch graph (peer-op-tx-id op))
        (record-applied-op graph (peer-op-op-id op) lamport)
        ;; B1/PT-8: advance our Lamport clock past the stamp we just applied.
        (peer-observe-lamport graph lamport)
        t)))

;;; ---------------------------------------------------------------------------
;;; Branch B: HUB re-home of a device-pushed authored op (B2d-2).
;;;
;;; The device pull-apply above (APPLY-PEER-AUTHORED-OP) runs on the single-writer
;;; funnel and does NOT re-journal -- a device never re-journals ops (design §5).
;;; The hub is the system of record: when it applies a device push it must RE-JOURNAL
;;; the result under a fresh hub-seq so every OTHER device pulls it, while preserving
;;; the ORIGINAL op-id/origin/lamport (so the author dedups its own bounce-back).  So
;;; re-home goes through a normal journaling WITH-TRANSACTION -- and, because the hub
;;; is multi-writer, the merge must run read-merge-write INSIDE the transaction body
;;; so an OCC retry re-reads the local node and re-merges (design §13).
;;; ---------------------------------------------------------------------------

(defun edge-multiplicity-conflicts (graph policy edge origin lamport)
  "B3-2b: if EDGE is a :safety-surface-edge (find-of-type) whose SOURCE already holds a
live edge of the same type -- a concurrent reclassification -- surface ONE multiplicity
conflict (keyed on the source find; both edges are kept, since edges union).  Returns a
list of 0 or 1 peer-conflict.  Runs at re-home only (the hub is where the reclassifi-
cations converge); a device just pulls both edges via the union, no per-device surface."
  (when (and policy (typep edge 'edge)
             (eq :safety-surface-edge (merge-policy-edge-bucket policy (type-of edge))))
    (let* ((src (lookup-vertex (from edge) :graph graph))
           (siblings (and src (outgoing-edges src :graph graph :edge-type (type-of edge)
                                              :include-subclasses-p nil))))
      (setf siblings (remove-if (lambda (e) (equalp (id e) (id edge))) siblings))
      (when siblings
        (list (make-peer-conflict
               :node-id (from edge) :slot :find-of-type :bucket :safety-surface-edge
               :kept-value (to (first siblings)) :kept-origin nil
               :loser-value (to edge) :loser-origin origin :loser-lamport lamport))))))

(defun rehome-one-write (graph policy write lamport origin)
  "Apply one re-homed WRITE inside the CURRENT hub transaction, returning (values
STAMP-UPDATES CONFLICTS).  A held-vertex update with a policy is merged field-by-
field (off the live local node, read here so an OCC retry re-merges); a create,
delete, edge, or unheld/no-policy update applies verbatim.  STAMP-UPDATES is a list
of (node-id slot lamport origin)."
  (let* ((incoming (node write))
         (nid (id incoming))
         (local (peer-local-node nid graph)))
    (cond
      ((typep write 'tx-delete)
       ;; "Still live at the hub" = present AND not already soft-deleted (a purged/
       ;; removed edge is still returned by lookup, deleted-p T).
       (let ((live (and local (not (deleted-p local)))))
         (cond
           ;; B3-2: a device removing a safety-bearing edge that is STILL LIVE at the hub
           ;; SURFACES (direction-gated: only this re-home path, only when live) and the
           ;; edge is KEPT -- a safety relationship is never dropped from the authoritative
           ;; record without operator sign-off.  A hub-originated removal reaches devices on
           ;; the pull path (peer-merge-write applies deletes verbatim, no surface), so
           ;; authoritative deletions still propagate cleanly.
           ((and live policy (typep incoming 'edge)
                 (eq :safety-edge (merge-policy-edge-bucket policy (type-of incoming))))
            (values nil
                    (list (make-peer-conflict
                           :node-id nid :slot :presence :bucket :safety-edge
                           :kept-value (string-downcase (symbol-name (type-of local)))
                           :kept-origin nil
                           :loser-value "removed"
                           :loser-origin origin :loser-lamport lamport))))
           (t
            (when live (delete-node local graph))
            (remove-node-field-stamps graph nid)
            (values nil nil)))))
      ((null local)
       ;; New to the hub -- recreate with the incoming state, stamp every field.  For
       ;; an edge, carry FROM/TO (separate slots, not in DATA) so it indexes correctly.
       ;; B3-2b: a 2nd concurrent :safety-surface-edge (find-of-type) on the same source
       ;; SURFACES a multiplicity conflict (both edges kept -- edges union).  Check
       ;; BEFORE the create so the incoming edge is not its own sibling.
       (let ((conflicts (edge-multiplicity-conflicts graph policy incoming origin lamport))
             (n (make-instance (type-of incoming)
                               :id nid :type-id (type-id incoming) :revision 0)))
         (setf (data n) (copy-tree (data incoming))
               (bytes n) (serialize (data incoming)))
         (when (typep incoming 'edge)
           (setf (from n) (from incoming) (to n) (to incoming)))
         (create-node n graph)
         (values (loop for (slot . nil) in (data incoming)
                       collect (list nid slot lamport origin))
                 conflicts)))
      ((and policy (vertex-p incoming))
       (multiple-value-bind (merged-data stamps conflicts)
           (merge-authored-fields policy (type-of incoming) nid
                                  (data local) (data incoming) (data (old-node write))
                                  lamport origin (node-field-stamps graph nid))
         (let ((c (copy local)))
           (setf (data c) merged-data)
           (update-node c graph))
         (values (loop for (slot . stamp) in stamps
                       collect (list nid slot (car stamp) (cdr stamp)))
                 conflicts)))
      (t
       ;; No policy, or an edge (edge merge is B3) -- overwrite (Branch A), stamp changed.
       (let ((c (copy local)))
         (setf (data c) (data incoming))
         (update-node c graph))
       (values (loop for slot in (authored-changed-slots write)
                     collect (list nid slot lamport origin))
               nil)))))

(defun rehome-authored-op (graph op)
  "Hub re-home of a device-pushed AUTHORED op: dedup by op-id, then apply through a
JOURNALING transaction (so every other device pulls the result), MERGING each held-
node update field-by-field inside the transaction body, and preserving the original
op-id/origin/lamport on the re-journaled feed entry (via *PEER-REHOME-OP*, design
§5).  The merge's per-field stamps + surfaced conflicts are applied AFTER the commit
(once, not per OCC attempt).  Returns T if applied, NIL if a duplicate."
  (if (op-applied-p graph (peer-op-op-id op))
      nil
      (let ((policy (merge-policy graph))
            (lamport (peer-op-lamport op))
            (origin (peer-op-origin op))
            (op-id (peer-op-op-id op))
            (stamp-updates '())
            (conflicts '()))
        (let ((*peer-rehome-op* (list op-id origin lamport))
              (*graph* graph))
          (with-transaction ((transaction-manager graph))
            ;; Reset per OCC attempt -- only the committed attempt's values survive.
            (setf stamp-updates '() conflicts '())
            (dolist (w (peer-op-writes op))
              (multiple-value-bind (stamps confs)
                  (rehome-one-write graph policy w lamport origin)
                (setf stamp-updates (nconc stamp-updates stamps)
                      conflicts (nconc conflicts confs))))))
        ;; Committed: apply the merge's per-field stamps + retain conflicts once.
        (dolist (s stamp-updates)
          (destructuring-bind (nid slot lam org) s
            (set-node-field-stamp graph nid slot lam org)))
        (dolist (c conflicts)
          (setf (peer-conflict-op-id c) op-id)
          (record-peer-conflict graph c))
        ;; Advance the hub clock past the op it just re-homed (its next authored op
        ;; is causally after it), matching the device pull-apply.
        (peer-observe-lamport graph lamport)
        t)))

(defun peer-receive-push (graph socket device)
  "Hub push-receive (B2d-2b): read the device's streamed authored ops until its
:PUSH-END control, RE-HOME each (§5 -- merge + re-journal under a fresh hub-seq
preserving the author's op-id), and send back the new :PUSH-ACK -- the highest device
feed-seq the hub saw.  Runs on the session thread (the hub is multi-writer, and
REHOME-AUTHORED-OP journals + merges under its own transaction).  A dropped op leaves
PUSH-ACK below it, so the device re-streams it next time (re-deduped by op-id)."
  (let ((*graph* graph)
        (high (peer-device-push-ack device)))
    (loop
      (let* ((packet (read-packet socket))
             (type (aref packet 9)))
        (cond
          ((= type +peer-meta-type-code+)
           (multiple-value-bind (origin op-id lamport op-class)
               (deserialize-peer-meta packet)
             (let* ((txh (deserialize-tx-header-vector (read-packet socket)))
                    (writes (loop repeat (write-count txh)
                                  collect (deserialize-tx-write-vector (read-packet socket))))
                    (seq (transaction-id txh)))
               (when (= op-class +peer-op-authored+)
                 (rehome-authored-op graph
                                     (make-peer-op :kind :authored :op-id op-id
                                                   :origin origin :lamport lamport
                                                   :tx-id seq :writes writes))
                 (when (> seq high) (setf high seq))))))
          ((= type +plist-packet-type-code+)
           (let ((pl (deserialize-plist-packet packet)))
             (when (eq (getf pl :peer-control) :push-end)
               (return))))
          (t (error "Unknown peer push packet type ~A" type)))))
    (with-recursive-lock-held ((peer-device-lock device))
      (setf (peer-device-push-ack device) high))
    (peer-write-plist (list :peer-control :push-ack :push-ack high) socket)
    high))

(defun peer-purge-node (graph node)
  "Hard-remove NODE from the device: drop it from every index/view (including the
spatial index, for a geometry-bearing vertex OR edge), then from its lhash table
and the cache.
Unlike a tombstone this leaves NO trace -- a captured device must not even reveal
the existence/id of purged (undisclosed) work (design §7)."
  ;; #6: release the node's unique keys BEFORE removal (reads its :ORIGIN partition,
  ;; still recorded) so the index does not keep a stale holder that would falsely
  ;; reject a future value; then drop its origin.  Guarded so a graph that never
  ;; used unique constraints pays nothing.
  (when (unique-indexes graph)
    (%uix-release node graph))
  ;; General ordered index: release the purged node's index entries too (guarded, so
  ;; a graph with no secondary indexes pays nothing).
  (when (secondary-indexes graph)
    (%ix-release node graph))
  ;; Spatial index: an EDGE can carry geometry exactly as a vertex can (the write
  ;; path, REBUILD-SPATIAL-INDEXES and REGENERATE-SPATIAL-INDEX all have an edge
  ;; path), so this is released for both kinds, before the etypecase rather than
  ;; inside one of its branches.  A purge leaves no tombstone and no node, so
  ;; nothing ever visits that index key again: a missed unindex orphans the entry
  ;; permanently, and the id it still names now resolves to nothing.
  (%spatial-unindex-node graph node)
  (etypecase node
    (edge
     (remove-from-ve-index node graph)
     (remove-from-vev-index node graph)
     (remove-from-type-index node graph)
     (remove-from-views graph node)
     (lhash-remove (edge-table graph) (id node)))
    (vertex
     (remove-from-type-index node graph)
     (remove-from-views graph node)
     (lhash-remove (vertex-table graph) (id node))))
  (remove-node-origin graph (id node))
  (remhash (id node) (cache graph)))

(defun apply-peer-purge (graph ids)
  "Purge IDS from the device, edges before vertices (so an edge is gone before the
endpoint it indexes on).  Missing ids are skipped (idempotent)."
  (let ((nodes (loop for id in ids
                     for node = (or (lookup-edge id :graph graph)
                                    (lookup-vertex id :graph graph))
                     when node collect node)))
    (dolist (n nodes) (when (typep n 'edge)   (peer-purge-node graph n)))
    (dolist (n nodes) (when (typep n 'vertex) (peer-purge-node graph n)))))

(defun peer-writer-loop (graph)
  "The device's single writer (WP-8): drain the mailbox and apply each op.  All
device graph mutations funnel through here, so OCC never contends (PT-5).
Accumulates the ids it durably holds/removed for the pull ack; a :barrier op
reports + resets that accumulator, a :shutdown op ends the thread."
  (let ((*graph* graph)
        (mailbox (peer-writer-mailbox graph))
        (created '())     ; state-create node-ids -> manifest additions this pull
        (purged '()))     ; purge node-ids -> manifest removals this pull
    (loop
      (let ((op (receive-message mailbox :timeout 1)))
        (when op
          ;; Progress heartbeat: PEER-SYNC's barrier wait reads this to distinguish a
          ;; slow-but-advancing apply from a wedged writer (see PEER-SYNC).
          (incf (peer-writer-progress graph))
          (ecase (peer-op-kind op)
            (:state-create
             ;; A membership-create: a node ENTERING the device's scope.  Its id
             ;; joins the manifest (reported at the barrier).
             (apply-peer-create-writes graph (peer-op-tx-id op) (peer-op-writes op)
                                       (peer-op-origin op))
             (dolist (w (peer-op-writes op))
               (pushnew (id (node w)) created :test #'equalp)))
            (:authored
             ;; An edit to a node the device already HOLDS: apply (op-id-deduped),
             ;; but membership is unchanged -- it does NOT join CREATED.
             (apply-peer-authored-op graph op))
            (:purge
             (apply-peer-purge graph (peer-op-ids op))
             (dolist (pid (peer-op-ids op))
               (push pid purged)
               (setf created (remove pid created :test #'equalp))))
            (:barrier
             ;; Advance the pull-cursor to the hub's frontier T (carried in TX-ID),
             ;; so it moves even when no authored op touched a held node.  Kept
             ;; separate from HIGHEST-TRANSACTION-ID (the device's own feed-seq) so a
             ;; Branch B device that authors locally does not conflate the two spaces.
             (let ((frontier (peer-op-tx-id op)))
               (when (and frontier (> frontier (load-peer-pull-cursor graph)))
                 (persist-peer-pull-cursor frontier graph))
               ;; The frontier T dominates every epoch applied this pull; keep the
               ;; live counter above it so local edits can see all pulled nodes.
               (peer-observe-epoch graph frontier))
             (send-message (peer-op-reply op)
                           (list :created (copy-list created)
                                 :purged (copy-list purged)))
             (setf created '() purged '()))
            (:local-write
             ;; A user/app write funneled here (WP-8) so it runs on the SAME thread as
             ;; the replication apply -- never a second writer contending with this loop
             ;; (intermittent deadlock on ECL otherwise).  Run the thunk, reply its value
             ;; (or the error), protecting the writer thread from a throw.
             (when (peer-op-reply op)
               (send-message (peer-op-reply op)
                             (list (handler-case (funcall (peer-op-thunk op))
                                     (error (e) (format nil "local-write ERROR: ~A" e)))))))
            (:shutdown
             (return))))))))

(defun peer-enqueue-write (graph thunk)
  "Run THUNK (a graph mutation) on GRAPH's single writer thread and return its value.
Funnels an app/user write through the SAME writer as the replication apply (WP-8), so
on ECL a user write never runs concurrently with the writer-loop (which otherwise
deadlocks intermittently).  Blocks until the writer applies it -- while it waits, it
re-checks writer liveness (see %PEER-AWAIT-BARRIER) rather than racing a deadline, so a
write queued behind a long pull-apply still completes.  Requires GRAPH be a :peer-role
:device peer (its writer-loop is running); signals an error otherwise."
  (let ((mailbox (peer-writer-mailbox graph)))
    (unless mailbox
      (error "peer-enqueue-write: no writer mailbox (graph is not a :device peer)"))
    (let ((reply (make-mailbox)))
      (send-message mailbox (make-peer-op :kind :local-write :thunk thunk :reply reply))
      (car (%peer-await-barrier graph reply 30 300)))))

;;; ---------------------------------------------------------------------------
;;; Device: receive a pull and ack it.
;;; ---------------------------------------------------------------------------

(defun peer-read-and-enqueue (socket graph mailbox)
  "Read ONE framed packet from SOCKET and enqueue the decoded op to MAILBOX (the
single writer).  Returns :PULL-END on the hub's end-of-pull control packet, else
T.  Deserialization (which resolves node type-ids against the schema, so it needs
*GRAPH*) happens here on the receive side; the actual graph mutation happens on
the writer (WP-8)."
  (let* ((*graph* graph)
         (packet (read-packet socket))
         (type (aref packet 9)))
    (cond
      ((= type +peer-meta-type-code+)
       (multiple-value-bind (origin op-id lamport op-class)
           (deserialize-peer-meta packet)
         (let* ((txh (deserialize-tx-header-vector (read-packet socket)))
                (writes (loop repeat (write-count txh)
                              collect (deserialize-tx-write-vector (read-packet socket)))))
           (send-message
            mailbox
            (make-peer-op :kind (if (= op-class +peer-op-authored+) :authored :state-create)
                          :op-id op-id :origin origin :lamport lamport
                          :tx-id (transaction-id txh) :writes writes))
           t)))
      ((= type +peer-purge-type-code+)
       (send-message mailbox (make-peer-op :kind :purge
                                           :ids (deserialize-purge-packet packet)))
       t)
      ((= type +plist-packet-type-code+)
       (let ((pl (deserialize-plist-packet packet)))
         ;; The pull-end control plist (it carries :CURSOR) is returned to the
         ;; caller so it can advance the pull-cursor; any other control is ignored.
         (if (eq (getf pl :peer-control) :pull-end) pl t)))
      (t
       (error "Unknown peer packet type ~A" type)))))

(defun peer-device-auth-plist (graph &key full-resync)
  "What a device sends to authenticate + drive a pull: origin, its PULL-CURSOR
(the op-stream lower bound), the same-major schema gate inputs, and optionally
:FULL-RESYNC to ask the hub to re-ship the whole scope (seed/recovery)."
  (list :origin-id (peer-id->hex (origin-id graph))
        :pull-cursor (load-peer-pull-cursor graph)
        :push-ack 0
        :full-resync (and full-resync t)
        :schema-major (first (peer-schema-version graph))
        :schema-minor (second (peer-schema-version graph))
        :replication-key (replication-key graph)))

(defun peer-push-phase (graph socket)
  "Device push-send (B2d-2b): stream the device's OWN authored ops with feed-seq >
PUSH-ACK to the hub, then a :PUSH-END control.  A device journals only its own
authored commits, so its whole replication feed is pushable -- no held-filter, unlike
the hub's cursor-resumed pull stream.  Read the hub's :PUSH-ACK reply and persist it
so the next push resumes after it.  The log is read while the device may be appending
new commits; a short/torn tail read is treated as end-of-log (that entry pushes next
time).  Returns the acked feed-seq."
  (let* ((*graph* graph)
         (from (load-peer-push-ack graph)))
    (flet ((safe-read (in) (handler-case (read-stream-packet in) (error () nil))))
      (dolist (log (applicable-replication-logs (1+ from) graph))
        (with-open-file (in log :element-type '(unsigned-byte 8))
          (loop
            (let ((meta (safe-read in)))
              (unless meta (return))
              (let ((txh-vec (safe-read in)))
                (unless txh-vec (return))
                (let* ((txh (deserialize-tx-header-vector txh-vec))
                       (seq (transaction-id txh))
                       (wpkts (loop repeat (write-count txh)
                                    for w = (safe-read in) while w collect w)))
                  (when (and (> seq from) (= (length wpkts) (write-count txh)))
                    (write-packet meta socket)
                    (write-packet txh-vec socket)
                    (dolist (w wpkts) (write-packet w socket))))))))))
    (peer-write-plist (list :peer-control :push-end) socket)
    (let* ((ack (read-plist-packet socket))
           (acked (getf ack :push-ack)))
      (when (and acked (> acked from))
        (persist-peer-push-ack acked graph))
      (or acked from))))

(defun %peer-await-barrier (graph reply poll stall)
  "Wait for the writer's :barrier reply on REPLY.  The apply of an initial pull can
take arbitrarily long -- it scales with the pulled node count and device speed (a
765-node pull is ~41 s on an ECL/Android device; a bigger working set, minutes) --
so we must NOT race a wall-clock deadline: that spuriously abandons a healthy but
slow apply, leaving the membership un-acked (the hub then re-ships the whole scope
every pull, and purge never fires).  Instead re-check writer LIVENESS every POLL
seconds and keep waiting while the writer thread is alive and its progress counter
is advancing.  Give up only on a genuine fault: the writer thread died, or it is
alive but made no progress for STALL seconds (wedged -- shouldn't happen in the
single-writer funnel, but bounds an otherwise-infinite hang).  Returns the reply
plist or signals an error."
  (let ((writer (peer-writer-thread graph))
        (last (peer-writer-progress graph))
        (idle 0))
    (loop
      (let ((r (receive-message reply :timeout poll)))
        (when r (return r))
        (let ((now (peer-writer-progress graph)))
          (cond
            ((not (and (threadp writer) (thread-alive-p writer)))
             (error "peer-sync: device writer thread died while applying the pull"))
            ((> now last) (setf last now idle 0))            ; progressing -> keep waiting
            (t (incf idle poll)
               (when (>= idle stall)
                 (error "peer-sync: device writer made no progress for ~D s (wedged?)"
                        stall)))))))))

(defun peer-sync (graph &key (attempts 10) full-resync (barrier-poll 30) (barrier-stall 300))
  "Device entry point: connect to the hub, handshake (same-major schema gate,
WP-6/PT-6), receive one authority-scoped DELTA pull, apply it through the single
writer (WP-8), advance the pull-cursor to the hub frontier T, and ACK the
membership delta (PT-4: manifest advances incrementally, only on this ack).  One
connection, one pull -- a resync is just another call; :FULL-RESYNC forces a full
re-ship.  Returns the ack result plist (:created / :purged id lists)."
  (let ((*graph* graph)
        (socket nil))
    (unwind-protect
         (progn
           (setf socket (connect-slave-to-master (peer-host graph)
                                                 (replication-port graph)
                                                 :attempts attempts))
           (let ((hs (read-plist-packet socket)))
             (unless (eql (getf hs :peer-protocol-version) *peer-protocol-version*)
               (error "peer protocol version mismatch: hub ~S, device ~S"
                      (getf hs :peer-protocol-version) *peer-protocol-version*))
             (let ((hub-version (list (getf hs :schema-major) (getf hs :schema-minor)))
                   (my-version (peer-schema-version graph)))
               (unless (peer-schema-compatible-p hub-version my-version)
                 (error 'peer-schema-incompatible-error
                        :hub-version hub-version :device-version my-version))))
           (peer-write-plist (peer-device-auth-plist graph :full-resync full-resync) socket)
           (let ((ctrl (read-plist-packet socket)))
             (unless (eq (getf ctrl :peer-control) :auth-ok)
               (error "peer auth rejected by hub: ~S" ctrl)))
           (let* ((mailbox (peer-writer-mailbox graph))
                  ;; Drain the pull until the pull-end control plist (carries T).
                  (end (loop for r = (peer-read-and-enqueue socket graph mailbox)
                             when (consp r) return r))
                  (frontier (or (getf end :cursor) 0)))
             ;; Barrier: the writer drains, advances the pull-cursor to T, and
             ;; reports the membership delta (created/purged) it durably applied.
             (let ((reply (make-mailbox)))
               (send-message mailbox (make-peer-op :kind :barrier :tx-id frontier
                                                   :reply reply))
               (let ((result (%peer-await-barrier graph reply barrier-poll barrier-stall)))
                 (peer-write-plist
                  (list :created (peer-ids->string (getf result :created))
                        :purged  (peer-ids->string (getf result :purged))
                        :pull-cursor (load-peer-pull-cursor graph))
                  socket)
                 ;; B2d-2b: after the pull ack, push the device's own authored ops to
                 ;; the hub for re-home (§5).  Same connection, both directions.
                 (peer-push-phase graph socket)
                 result))))
      (when socket (ignore-errors (usocket:socket-close socket))))))

;;; ---------------------------------------------------------------------------
;;; Lifecycle: start/stop-replication specialized on peer-graph.
;;;
;;; MAKE-GRAPH / OPEN-GRAPH already call (start-replication graph) and
;;; CLOSE-GRAPH calls (stop-replication graph) for every graph, so specializing
;;; the existing generics on peer-graph wires the peer transport into the normal
;;; lifecycle with no changes to graph.lisp.  Master/slave methods are untouched.
;;; ---------------------------------------------------------------------------

(defmethod start-replication ((graph peer-graph) &key package)
  (declare (ignore package))
  (setf (stop-replication-p graph) nil)
  (ecase (peer-role graph)
    (:hub
     (setf (peer-thread graph)
           (bordeaux-threads:make-thread
            (lambda () (peer-server-accept-loop graph))
            :name (format nil "~A-peer-hub" (graph-name graph)))))
    (:device
     ;; Start the single-writer funnel; pulls are driven explicitly by PEER-SYNC
     ;; (a Branch A device is read-only and pulls on demand, not a live stream).
     (setf (peer-writer-mailbox graph) (make-mailbox))
     (setf (peer-writer-thread graph)
           (bordeaux-threads:make-thread
            (lambda () (peer-writer-loop graph))
            :name (format nil "~A-peer-writer" (graph-name graph)))))))

(defmethod stop-replication ((graph peer-graph))
  (setf (stop-replication-p graph) t)
  (ecase (peer-role graph)
    (:hub
     (let ((thread (peer-thread graph)))
       (when (and (threadp thread) (thread-alive-p thread))
         (join-thread thread))))
    (:device
     (let ((mailbox (peer-writer-mailbox graph)))
       (when mailbox
         (send-message mailbox (make-peer-op :kind :shutdown))))
     (let ((thread (peer-writer-thread graph)))
       (when (and (threadp thread) (thread-alive-p thread))
         (join-thread thread))))))
