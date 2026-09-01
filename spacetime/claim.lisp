;;;; The reified claim: a relation stored as a NODE, carrying the provenance
;;;; an edge cannot.  Endpoints are stored VALUES, never graph references --
;;;; a read-write transaction is single-graph, so an edge to a foreign node
;;;; could not be created at all (GH #131, design §2).

(in-package #:graph-db.spacetime)

(define-condition unknown-claim-family (spacetime-error)
  ((parent :initarg :parent :reader unknown-claim-family-parent))
  (:report (lambda (c s)
             (format s "~S names no claim family; DEF-CLAIM-CLASSES first."
                     (unknown-claim-family-parent c)))))

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

;;; RELATION and PRODUCER are canonical strings (GH #160).  Both are in
;;; the DEF-UNIQUE identity tuple and compared with EQUAL, so a second
;;; spelling of one name (:x, "X", " x") is a second claim, never an
;;; update.  Two predicates, not one: relation is vocabulary, producer is
;;; provenance, and a deployed producer is path-like ("tenant/rule").

(defun %canonical-name-p (value extra-chars)
  "True when VALUE is a non-empty string over [a-z0-9-] plus EXTRA-CHARS."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (ch)
                (or (char<= #\a ch #\z)
                    (char<= #\0 ch #\9)
                    (char= ch #\-)
                    (find ch extra-chars)))
              value)))

(defun canonical-relation-p (value)
  "True when VALUE is a canonical RELATION: a non-empty lowercase kebab
string, [a-z0-9-] only.  The :CHECK behind the RELATION slot (GH #160)."
  (%canonical-name-p value ""))

(defun canonical-producer-p (value)
  "True when VALUE is a canonical PRODUCER: as CANONICAL-RELATION-P, plus
/ for a path-like rule name.  The :CHECK behind the PRODUCER slot
(GH #160)."
  (%canonical-name-p value "/"))

;; Registered at load, so the :CHECK names below resolve in any image
;; that has this file before a tenant's DEF-CLAIM-CLASSES expands.  A
;; lambda, not #'..., so a redefined predicate takes effect immediately.
(graph-db:register-schema-function
 'canonical-relation-p (lambda (v) (canonical-relation-p v)))
(graph-db:register-schema-function
 'canonical-producer-p (lambda (v) (canonical-producer-p v)))

(defun %extent-open-p (e)
  (bound-unknown-p (extent-end e)))

(defun %bound-same-p (a b)
  "True when bounds A and B pin the same range."
  (flet ((same (x y)
           (or (and (eq x :unbounded) (eq y :unbounded))
               (and (typep x 'local-time:timestamp)
                    (typep y 'local-time:timestamp)
                    (local-time:timestamp= x y)))))
    (and (same (bound-earliest a) (bound-earliest b))
         (same (bound-latest a) (bound-latest b)))))

(defun transaction-extent-step (old new)
  "True when a claim's transaction extent may go from OLD to NEW, both
stored sexps: from absent to anything (a stamp, or a legacy claim being
stamped, GH #148); open -> closed with the same start (RETRACT-CLAIM,
GH #162); closed -> open starting no earlier than the close (re-assertion,
%UPDATE-REGISTRATION-CLAIM).  Everything else -- clearing it, moving the
start, re-closing, junk -- is refused.  The :TRANSITION behind the slot
(GH #158): the audit field's rule, enforced at commit on every write path
including REST, rather than only at the accessor."
  (or (null old)
      (and new
           (handler-case
               (let ((o (sexp->extent old))
                     (n (sexp->extent new)))
                 (or (and (%extent-open-p o) (not (%extent-open-p n))
                          (%bound-same-p (extent-start o)
                                         (extent-start n)))
                     (and (not (%extent-open-p o)) (%extent-open-p n)
                          (let ((closed (bound-latest (extent-end o)))
                                (start (bound-earliest (extent-start n))))
                            (and (typep closed 'local-time:timestamp)
                                 (typep start 'local-time:timestamp)
                                 (local-time:timestamp>= start closed))))))
             (error () nil)))))

;; A lambda, as the canonical-name predicates above: a redefinition takes
;; effect immediately.
(graph-db:register-schema-function
 'transaction-extent-step (lambda (o n) (transaction-extent-step o n)))

(defun %timestamp-key (ts)
  "TS as LOCAL-TIME's three fixnums: EQUAL-comparable, LESS-THAN-orderable
and SERIALIZE-able, which a TIMESTAMP object is not (GH #296)."
  (list (local-time:day-of ts) (local-time:sec-of ts)
        (local-time:nsec-of ts)))

(defun extent-sexp-start-key (sexp)
  "The identity component a temporal family derives from a stored extent
SEXP: its START bound as ((day sec nsec) (day sec nsec)), :UNBOUNDED
where open; NIL for NIL.  Fixnums, not TIMESTAMP objects -- the memory
backend's unique index is an EQUAL hash table and structures are EQUAL
only when EQ -- and not nanoseconds since the epoch, a bignum off SBCL
that SERIALIZE cannot write.  The :CANONICALIZE behind the temporal
identity tuple (GH #296, design §2.2)."
  (when sexp
    (let ((start (extent-start (sexp->extent sexp))))
      (flet ((key (x) (if (eq x :unbounded) x (%timestamp-key x))))
        (list (key (bound-earliest start)) (key (bound-latest start)))))))

(defparameter +claim-shared-slots+
  '((subject-namespace :initarg :subject-namespace
                       :accessor claim-subject-namespace)
    (subject-key :initarg :subject-key :accessor claim-subject-key)
    ;; Canonical strings, enforced at commit on every write path (GH #160).
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
    ;; The transaction axis (GH #148).  Same codec as EXTENT-SEXP; the two
    ;; never share a name so neither is mistaken for the other.
    (transaction-extent-sexp :initarg :transaction-extent-sexp
                             :accessor claim-transaction-extent-sexp
                             :initform nil)
    (geometry :initarg :geometry :accessor claim-geometry :initform nil)
    ;; Registration outputs (#138).  PRECISION-M is metres, a real
    ;; quantity that flows in both directions -- a source can be finer
    ;; than the region it joins to -- never a discount factor.
    (precision-m :initarg :precision-m :accessor claim-precision-m
                 :initform nil)
    (fraction :initarg :fraction :accessor claim-fraction
              :initform 1.0d0))
  "Slots every claim carries, on the PARENT class.  Symbols live in this
package so two claim families share one set of accessors (design §5).")

(defun %plist-remove (plist key)
  "PLIST with every KEY/value pair removed."
  (loop for (k v) on plist by #'cddr
        unless (eq k key)
          collect k and collect v))

(defun %plist-key-p (plist key)
  "True when KEY occupies a KEY position in PLIST.  MEMBER would also match
KEY sitting in a VALUE position -- :EXTENT is a legal namespace keyword,
so (MAKE-B :SUBJECT-NAMESPACE :EXTENT ...) must not spuriously trip an
:EXTENT check (GH #131 finding 6; relations are strings since GH #160)."
  (loop for (k) on plist by #'cddr thereis (eq k key)))

(defun %claim-encode-extent-arg (args)
  "Rewrite a claim constructor's ARGS: an :EXTENT is encoded via
EXTENT->SEXP and passed through as :EXTENT-SEXP, the slot that actually
persists.  Handing it to the constructor is the preferred form -- a
just-built claim is complete in one call, with extent validation next to
the other construction-time checks -- not a persistence requirement:
(SETF CLAIM-EXTENT) on the resulting node works too (GH #135 is fixed).
Signals if both :EXTENT and :EXTENT-SEXP are given, rather than picking
one."
  (if (%plist-key-p args :extent)
      (progn
        (when (%plist-key-p args :extent-sexp)
          (error "Pass only one of :EXTENT or :EXTENT-SEXP, not both."))
        (let ((extent (getf args :extent)))
          (list* :extent-sexp (and extent (extent->sexp extent))
                 (%plist-remove args :extent))))
      args))

(defun %open-transaction-extent (timestamp)
  "The transaction period [TIMESTAMP, open).  :ASSERTED means still
believed; an end genuinely unknown is :INDETERMINATE (GH #148)."
  (make-interval (exact-bound timestamp) (unknown-bound)
                 :semantics :transaction :standing :asserted))

(defun %stamp-now (args)
  "ARGS with a fresh open :TRANSACTION-EXTENT-SEXP stamp prepended -- the
default whenever none of the three transaction initargs carries a value.
A key present with a NIL value counts as this default too: the caller is
saying \"nothing to say about transaction time\", not \"leave this
unstamped\" (design, Stamping; GH #148 review)."
  (list* :transaction-extent-sexp
         (extent->sexp (%open-transaction-extent (local-time:now)))
         args))

(defun %claim-encode-transaction-arg (args)
  "Rewrite a claim constructor's ARGS so the transaction axis is always
populated: :TRANSACTION-EXTENT and :RECORDED-AT are encoded to
:TRANSACTION-EXTENT-SEXP, and an unstamped claim is stamped now.  Signals
if more than one KEY is given, rather than picking one -- checked on key
presence, not on value, so passing two keys still conflicts even when one
is NIL (GH #148)."
  (let ((n (count-if (lambda (k) (%plist-key-p args k))
                     '(:transaction-extent :recorded-at
                       :transaction-extent-sexp))))
    (when (> n 1)
      (error "Pass only one of :TRANSACTION-EXTENT, :RECORDED-AT or ~
:TRANSACTION-EXTENT-SEXP."))
    (cond
      ((%plist-key-p args :transaction-extent)
       (let ((e (getf args :transaction-extent))
             (rest (%plist-remove args :transaction-extent)))
         (if e
             (list* :transaction-extent-sexp (extent->sexp e) rest)
             (%stamp-now rest))))
      ((%plist-key-p args :recorded-at)
       (let ((ts (getf args :recorded-at))
             (rest (%plist-remove args :recorded-at)))
         (if ts
             (list* :transaction-extent-sexp
                    (extent->sexp (%open-transaction-extent ts))
                    rest)
             (%stamp-now rest))))
      ((%plist-key-p args :transaction-extent-sexp)
       (if (getf args :transaction-extent-sexp)
           args
           (%stamp-now (%plist-remove args :transaction-extent-sexp))))
      (t (%stamp-now args)))))

(defparameter +claim-identity-slots+
  '(:producer :subject-namespace :subject-key :relation)
  "Every claim's identity components -- the UNARY constraint tuple.
BINARY adds +CLAIM-OBJECT-IDENTITY-SLOTS+.  DEF-UNIQUE exempts any tuple
containing a null, so a caller who omits one of these is silently exempt
from the constraint unless every component is checked non-nil first
(design §3.1, GH #131 finding 1).")

(defparameter +claim-object-identity-slots+
  '(:object-namespace :object-key)
  "BINARY-CLAIM's identity components beyond +CLAIM-IDENTITY-SLOTS+.")

(defun %check-claim-identity (args keys)
  "Signal MISSING-CLAIM-IDENTITY-COMPONENT naming the first of KEYS that is
absent or NIL in ARGS.  Checked on the raw constructor arguments, before
the node is built, so a caller who omits one never gets even a transient
claim (design §3.1, GH #131 finding 1)."
  (dolist (key keys)
    (when (null (getf args key))
      (error 'missing-claim-identity-component :slot key))))

(defparameter +claim-object-slots+
  '((object-namespace :initarg :object-namespace
                      :accessor claim-object-namespace)
    (object-key :initarg :object-key :accessor claim-object-key))
  "Slots only BINARY-CLAIM carries.  Their absence from UNARY-CLAIM is what
makes a unary claim unable to carry an object (design §3.1).")

(defmacro def-claim-classes (parent graph-name &key extra-slots temporal)
  "Define PARENT and its UNARY/BINARY subclasses in GRAPH-NAME, and register
the family.  The subsystem cannot ship these classes: DEF-VERTEX binds a node
type to a graph name and class names are globally unique, so a shipped class
would collide between tenants (design §4).

PARENT is deliberately given no constructor -- it exists to hold the shared
slots and the shared indexes, and carries no uniqueness constraint of its own
(design §3.3).  :EXTRA-SLOTS go on PARENT, so both arities inherit them.

STANDING is checked by wrapping each arity's MAKE-<NAME>, not by a CLOS
construction hook (design §5).  Construction goes through CHANGE-CLASS
(%MAKE-VERTEX promotes a pooled VERTEX to the target class), so
INITIALIZE-INSTANCE never fires; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS does
fire, but also on the READ path (deserializing an existing node), where the
DATA alist is not populated yet -- it would reject already-valid claims.

STANDING is ALSO declared as a value constraint on PARENT, so the closed
vocabulary is enforced at commit on every write path, not only through the
MAKE-<NAME> wrapper (GH #149).

RELATION and PRODUCER are canonical strings -- CANONICAL-RELATION-P and
CANONICAL-PRODUCER-P, as :CHECK slot options on PARENT, enforced at commit
on every write path (GH #160).  A keyword is refused, and so is a case or
whitespace variant: both slots are in the identity tuple, so a variant
would be a second claim rather than an update.

The TRANSACTION STAMP is a :TRANSITION constraint on PARENT --
TRANSACTION-EXTENT-STEP: start immutable, end closeable once, re-openable
by a later re-assertion -- enforced at commit on every write path, REST
included (GH #148, #158, #162).  The accessor's own refusal stays as the
fast-fail with the better error site.

:TEMPORAL T makes the family a STATE SERIES (GH #296): the extent START
joins both identity tuples (EXTENT-SEXP-START-KEY), an extent is required
at construction and at commit, and live claims sharing a base tuple must
have pairwise disjoint validity (spacetime/temporal.lisp).  Same
declaration names, so flipping the flag re-declares rather than stacks."
  (let* ((unary (intern (format nil "~A-UNARY" parent)))
         (binary (intern (format nil "~A-BINARY" parent)))
         (extent-slot (when temporal '(extent-sexp)))
         (unary-slots (append '(producer subject-namespace subject-key
                                relation)
                              extent-slot))
         (binary-slots (append '(producer subject-namespace subject-key
                                 object-namespace object-key relation)
                               extent-slot)))
    (flet ((identity-options (slots)
             ;; Positional :CANONICALIZE, the start key on the last slot.
             (when temporal
               `(:canonicalize ,(append (make-list (1- (length slots)))
                                        '(extent-sexp-start-key))))))
    `(progn
       (graph-db:def-vertex ,parent ()
           (,@+claim-shared-slots+ ,@extra-slots)
         ,graph-name)
       (graph-db:def-vertex ,unary (,parent) () ,graph-name)
       (graph-db:def-vertex ,binary (,parent) (,@+claim-object-slots+)
           ,graph-name)
       ;; The closed vocabulary, enforced on every write path -- not only at
       ;; construction, where CHECK-STANDING alone left it (GH #149).
       ;; :ONE-OF is evaluated, so this names +STANDINGS+ rather than
       ;; duplicating it and cannot drift from STANDINGP.  :NAME is load-
       ;; bearing, not decoration -- see the ⚠ block below: this macro
       ;; cannot name what an earlier version of itself emitted, so a
       ;; stable name is what makes re-declaration REPLACE rather than
       ;; stack (%SPEC-IDENTITY, index.lisp:99).
       (graph-db:def-value-constraint ,parent standing ,graph-name
         :one-of +standings+
         :required t
         :name standing-vocabulary)
       ;; The audit field's rule (GH #148/#162), as a commit-time check
       ;; rather than an accessor guard a raw slot write walks past --
       ;; which REST-PUT-VERTEX does (GH #158).
       (graph-db:def-value-constraint ,parent transaction-extent-sexp
           ,graph-name
         :transition transaction-extent-step
         :name transaction-extent-transition)
       ;; The unary constraint goes on UNARY, never on PARENT: PARENT has
       ;; exactly the unary slot set, so declaring it there would bind
       ;; BINARY too (CLASS-UNIQUE-TUPLE-SPECS matches on SUBTYPEP) and
       ;; forbid one producer relating a subject to several objects
       ;; (design §3.2).
       ;; ⚠ EVERY DECLARATION THIS MACRO EMITS IS NAMED -- the value
       ;; constraint above included (GH #139, #140, #149).  This macro
       ;; emits schema on a tenant's behalf, and a LATER VERSION OF IT
       ;; CANNOT NAME WHAT AN EARLIER VERSION EMITTED.  Unnamed, identity
       ;; is (owner . slot-names), so changing what is declared here would
       ;; leave BOTH the old and the new spec live in every long-lived
       ;; image -- the stale unique rejecting writes the current schema
       ;; permits, the stale index built and maintained for nothing.  A
       ;; name is stable across a change of shape, so re-declaring
       ;; replaces.  #138 changes this record.
       (graph-db:def-unique ,unary ,unary-slots
         ,graph-name :name claim-unary-identity
         ,@(identity-options unary-slots))
       (graph-db:def-unique ,binary ,binary-slots
         ,graph-name :name claim-binary-identity
         ,@(identity-options binary-slots))
       ;; A temporal claim without an extent would sit under NO identity
       ;; (DEF-UNIQUE's null exemption), so the extent is required on
       ;; every write path, not only at MAKE-<NAME> (GH #296).
       ,@(when temporal
           `((graph-db:def-value-constraint ,parent extent-sexp ,graph-name
               :required t
               :name claim-extent-required)))
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
       (fmakunbound ',(intern (format nil "MAKE-~A" parent)))
       ;; DEF-VERTEX redefines each raw constructor on every expansion, so
       ;; this cannot double-wrap on a re-evaluated DEF-CLAIM-CLASSES form.
       ,@(mapcar
          (lambda (class identity-keys)
            (let ((ctor (intern (format nil "MAKE-~A" class))))
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
                                          (%claim-encode-transaction-arg
                                           args))))
                             (check-standing (claim-standing c))
                             c)))))))
          (list unary binary)
          (list +claim-identity-slots+
                (append +claim-identity-slots+
                        +claim-object-identity-slots+)))
       (setf (gethash ',parent *claim-families*)
             (%make-claim-family ',parent ',unary ',binary
                                 ,(and temporal t)))
       ',parent))))
