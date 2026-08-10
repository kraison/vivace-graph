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

(defparameter +claim-object-slots+
  '((object-namespace :initarg :object-namespace
                      :accessor claim-object-namespace)
    (object-key :initarg :object-key :accessor claim-object-key))
  "Slots only BINARY-CLAIM carries.  Their absence from UNARY-CLAIM is what
makes a unary claim unable to carry an object (design §3.1).")

(defclass claim-standing-mixin () ()
  (:documentation "Specialisation point for the STANDING check.  Holds no
slots and is never persisted; a plain STANDARD-CLASS superclass of a
NODE-CLASS is accepted (verified on SBCL, GH #131)."))

(defmethod initialize-instance :after ((c claim-standing-mixin) &key)
  ;; STANDING is required and validated here rather than by convention
  ;; (design §5).  Specialised on the mixin, never on T -- a T method would
  ;; run on every object created anywhere in the image.
  (check-standing (claim-standing c)))

;; NODE-CLASS instances (MAKE-<name>, the only public constructor) are never
;; built by MAKE-INSTANCE on SBCL/CCL/LispWorks -- %MAKE-VERTEX promotes a
;; pooled base VERTEX via CHANGE-CLASS, whose protocol calls
;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, not INITIALIZE-INSTANCE (see
;; CHANGE-NODE-CLASS, primitive-node.lisp).  Keep the INITIALIZE-INSTANCE
;; method above too: ECL's %MAKE-VERTEX builds via MAKE-INSTANCE directly
;; (GH #131).
;;
;; CHANGE-CLASS also fires on the READ path (DESERIALIZE-VERTEX-HEAD, e.g.
;; during SNAPSHOT/BACKUP): there the DATA alist is deliberately left
;; unhydrated at this point (*INITIALIZING-NODE* defers it -- see that
;; variable's docstring), so STANDING would read NIL and this method would
;; reject an already-valid, already-persisted claim.  DATA-POINTER
;; distinguishes the two: 0 only for a node that has never been written
;; (genuine construction); the read path always supplies a real address.
(defmethod update-instance-for-different-class :after
    ((old t) (c claim-standing-mixin) &key)
  (declare (ignore old))
  (when (zerop (graph-db::data-pointer c))
    (check-standing (claim-standing c))))

(defmacro def-claim-classes (parent graph-name &key extra-slots)
  "Define PARENT and its UNARY/BINARY subclasses in GRAPH-NAME, and register
the family.  The subsystem cannot ship these classes: DEF-VERTEX binds a node
type to a graph name and class names are globally unique, so a shipped class
would collide between tenants (design §4).

PARENT is deliberately given no constructor -- it exists to hold the shared
slots and the shared indexes, and carries no uniqueness constraint of its own
(design §3.3).  :EXTRA-SLOTS go on PARENT, so both arities inherit them."
  (let ((unary (intern (format nil "~A-UNARY" parent)))
        (binary (intern (format nil "~A-BINARY" parent))))
    `(progn
       (graph-db:def-vertex ,parent (claim-standing-mixin)
           (,@+claim-shared-slots+ ,@extra-slots)
         ,graph-name)
       (graph-db:def-vertex ,unary (,parent) () ,graph-name)
       (graph-db:def-vertex ,binary (,parent) (,@+claim-object-slots+)
           ,graph-name)
       (fmakunbound ',(intern (format nil "MAKE-~A" parent)))
       (setf (gethash ',parent *claim-families*)
             (%make-claim-family ',parent ',unary ',binary))
       ',parent)))
