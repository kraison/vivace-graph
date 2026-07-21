(in-package :graph-db)

(define-condition slave-auth-error (error)
  ((reason :initarg :reason)
   (host :initarg :host))
  (:report (lambda (error stream)
             (with-slots (reason host) error
               (format stream "Slave auth error ~A: ~A." host reason)))))

(define-condition transaction-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason) error
               (format stream "Transaction error: ~A." reason)))))

(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a."
                       instance reason)))))

(define-condition deserialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Deserialization failed for ~a because of ~a."
                       instance reason)))))

(define-condition stale-revision-error (error)
  ((instance :initarg :instance)
   (current-revision :initarg :current-revision))
  (:report (lambda (error stream)
             (with-slots (instance current-revision) error
               (format stream "Attempt to update stale revision ~S of ~S."
                       instance current-revision)))))

(define-condition duplicate-key-error (error)
  ((instance :initarg :instance)
   (key :initarg :key))
  (:report (lambda (error stream)
             (with-slots (instance key) error
               (format stream "Duplicate key ~S in ~S."
                       key instance)))))

(define-condition nonexistent-key-error (error)
  ((instance :initarg :instance)
   (key :initarg :key))
  (:report (lambda (error stream)
             (with-slots (instance key) error
               (format stream "Nonexistent key ~S in ~S."
                       key instance)))))

(define-condition node-already-deleted-error (error)
  ((node :initarg :node))
  (:report (lambda (error stream)
             (with-slots (node) error
               (format stream "Node ~A already deleted" node)))))

(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ())

(define-condition edge-already-deleted-error (node-already-deleted-error)
  ())

(define-condition invalid-view-error (error)
  ((class-name :initarg :class-name)
   (view-name :initarg :view-name))
  (:report (lambda (error stream)
             (with-slots (class-name view-name) error
               (format stream
                       "No such graph view: ~A/~A"
                       class-name view-name)))))

(define-condition view-lock-error (error)
  ((message :initarg :message))
  (:report (lambda (error stream)
             (with-slots (message) error
               (format stream
                       "View locking error: '~A'"
                       message)))))

;;; Spatial / GEOS conditions.  Defined in core (GEOS-free) so the refine seam
;;; and its callers can reference them whether or not the graph-db/geos add-on
;;; is loaded.

(define-condition geos-error (error)
  ((message :initarg :message :initform nil :reader geos-error-message))
  (:report (lambda (error stream)
             (format stream "GEOS error: ~A"
                     (or (geos-error-message error) "(no message)"))))
  (:documentation "Signalled when a GEOS operation fails or reports an error."))

(define-condition geos-required-for-operation (error)
  ((operation :initarg :operation :initform nil :reader geos-required-operation))
  (:report (lambda (error stream)
             (format stream
                     "Operation ~A requires the graph-db/geos add-on (libgeos_c), ~
which is not loaded/available."
                     (or (geos-required-operation error) "(unknown)"))))
  (:documentation "Signalled when an exact-topology operation has no
dependency-free fallback and GEOS is unavailable."))

(define-condition vector-segment-capacity-exhausted (error)
  ((owner :initarg :owner :reader vsce-owner)
   (slot :initarg :slot :reader vsce-slot)
   (required :initarg :required :reader vsce-required)
   (reserved :initarg :reserved :reader vsce-reserved)
   (needed-bytes :initarg :needed-bytes :reader vsce-needed-bytes))
  (:report (lambda (c s)
             (format s "vector segment ~A/~A: growing to hold ~D entries needs ~D bytes, ~
but its mmap reservation is ~D. Reopen the graph (the reservation is recomputed ~
from the file's current size, which grants roughly 8x more headroom), or raise ~
GRAPH-DB::*MMAP-RESERVATION-MULTIPLIER* / GRAPH-DB::*MMAP-MIN-RESERVATION* before opening."
                     (vsce-owner c) (vsce-slot c) (vsce-required c)
                     (vsce-needed-bytes c) (vsce-reserved c))))
  (:documentation "Signalled pre-durability when applying a transaction would
require growing a vector segment past its mmap reservation."))
