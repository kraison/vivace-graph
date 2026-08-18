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
                             (parent unary binary))
                         (:copier nil))
  "The three class names DEF-CLAIM-CLASSES generated together.  Registered so
CLAIMS-TOUCHING and DELETE-CLAIMS-BY-PRODUCER can reach the arity subclasses
from the parent name alone."
  (parent nil :read-only t)
  (unary nil :read-only t)
  (binary nil :read-only t))

(defvar *claim-families* (make-hash-table :test 'eq)
  "Parent class name -> CLAIM-FAMILY.")

(defun claim-family (parent)
  "The CLAIM-FAMILY registered for PARENT, or signal UNKNOWN-CLAIM-FAMILY."
  (or (gethash parent *claim-families*)
      (error 'unknown-claim-family :parent parent)))

(defparameter +claim-shared-slots+
  '((subject-namespace :initarg :subject-namespace
                       :accessor claim-subject-namespace)
    (subject-key :initarg :subject-key :accessor claim-subject-key)
    (relation :initarg :relation :accessor claim-relation)
    (producer :initarg :producer :accessor claim-producer)
    (rule-version :initarg :rule-version :accessor claim-rule-version
                  :initform nil)
    (method :initarg :method :accessor claim-method :initform nil)
    (standing :initarg :standing :accessor claim-standing)
    (confidence :initarg :confidence :accessor claim-confidence
                :initform nil)
    (extent-sexp :initarg :extent-sexp :accessor claim-extent-sexp
                 :initform nil)
    (geometry :initarg :geometry :accessor claim-geometry :initform nil))
  "Slots every claim carries, on the PARENT class.  Symbols live in this
package so two claim families share one set of accessors (design §5).")

(defun %plist-remove (plist key)
  "PLIST with every KEY/value pair removed."
  (loop for (k v) on plist by #'cddr
        unless (eq k key)
          collect k and collect v))

(defun %plist-key-p (plist key)
  "True when KEY occupies a KEY position in PLIST.  MEMBER would also match
KEY sitting in a VALUE position -- :EXTENT is a legal open-vocabulary
RELATION, so (MAKE-B :RELATION :EXTENT ...) must not spuriously trip an
:EXTENT check (GH #131 finding 6)."
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

(defmacro def-claim-classes (parent graph-name &key extra-slots)
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
MAKE-<NAME> wrapper (GH #149)."
  (let ((unary (intern (format nil "~A-UNARY" parent)))
        (binary (intern (format nil "~A-BINARY" parent))))
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
       (graph-db:def-unique ,unary
           (producer subject-namespace subject-key relation)
         ,graph-name :name claim-unary-identity)
       (graph-db:def-unique ,binary
           (producer subject-namespace subject-key
            object-namespace object-key relation)
         ,graph-name :name claim-binary-identity)
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
                         (let ((c (apply %raw
                                        (%claim-encode-extent-arg args))))
                           (check-standing (claim-standing c))
                           c))))))
          (list unary binary)
          (list +claim-identity-slots+
                (append +claim-identity-slots+
                        +claim-object-identity-slots+)))
       (setf (gethash ',parent *claim-families*)
             (%make-claim-family ',parent ',unary ',binary))
       ',parent)))
