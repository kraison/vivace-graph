;;;; Shared schema for the two-process peer-replication PUSH conflict test (B2d-2b).
;;;;
;;;; Loaded IDENTICALLY by both the hub and the device (byte-for-byte) so their
;;;; schema-digests match and node type-ids line up on the wire.
;;;;
;;;; A single mine-action-shaped find with two mergeable fields:
;;;;   NOTE   -- a plain :lww field (higher (lamport, origin) wins)
;;;;   HAZARD -- a :safety field: DANGEROUS unless exactly "SAFE".  A release
;;;;             (dangerous local -> safe incoming) is REJECTED (keep local + surface);
;;;;             a re-open toward danger auto-applies.
;;;; The find is its own scope root, so the device's authority scope is just the find.

(in-package :graph-db)

(def-vertex pf-find ()
  ((name    :type string)
   (note    :type string)
   (hazard  :type string)
   (disclosable :type integer))
  :push-test-app)

;;; A fixed device origin id known to BOTH processes.
(defparameter *device-origin*
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(9 9 9 9 0 0 0 0 0 0 0 0 0 0 0 2)))

;;; Everything registered is disclosable (scope entry/exit is not what this test
;;; exercises -- the conflict merge on push is).
(defun push-disclosable (vertex graph scope)
  (declare (ignore graph scope))
  (= 1 (slot-value vertex 'disclosable)))

;;; The Branch B conflict contract (app seam): HAZARD is :safety, everything else
;;; :lww.  Dangerous unless exactly "SAFE"; a release keeps the dangerous value and
;;; surfaces, a toward-danger change auto-applies, same-class falls back to LWW.
(defun push-field-bucket (type slot)
  (declare (ignore type))
  (case slot (:hazard :safety) (t :lww)))

(defun push-safety-merge (slot local incoming local-newer-p)
  (declare (ignore slot))
  (let ((ld (not (equal local "SAFE")))
        (id (not (equal incoming "SAFE"))))
    (cond ((and ld (not id)) (values local t))               ; release -> keep + surface
          ((and id (not ld)) (values incoming nil))           ; re-open -> take incoming
          (t (values (if local-newer-p local incoming) nil))))) ; same class -> LWW

(defun push-merge-policy ()
  (make-merge-policy :field-bucket #'push-field-bucket
                     :safety-merge #'push-safety-merge))
