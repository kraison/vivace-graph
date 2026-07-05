;;;; Shared schema for the two-process peer-replication test.
;;;;
;;;; Loaded IDENTICALLY by both the hub and the device so their schema-digests
;;;; match and node type-ids line up on the wire (keep the def-vertex / def-edge
;;;; forms byte-for-byte identical on both sides).
;;;;
;;;; A small mine-action-shaped graph: a SITE has SURVEYs, a survey has FINDs.
;;;; Every node carries a DISCLOSABLE flag (1/0); the hub's export predicate
;;;; ships a node only when it is 1, which lets the test drive scope entry/exit
;;;; (a find flipped to 0 must be PURGED from the device on the next pull).

(in-package :graph-db)

(def-vertex p-site ()
  ((name :type string)
   (disclosable :type integer))
  :peer-test-app)

(def-vertex p-survey ()
  ((name :type string)
   (disclosable :type integer))
  :peer-test-app)

(def-vertex p-find ()
  ((name :type string)
   (disclosable :type integer))
  :peer-test-app)

(def-edge p-has-survey () () :peer-test-app)   ; site  -> survey
(def-edge p-has-find   () () :peer-test-app)    ; survey -> find

;;; A fixed device origin id known to BOTH processes (the hub mints it for real;
;;; here a constant avoids passing it through a file).
(defparameter *device-origin*
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(7 7 7 7 0 0 0 0 0 0 0 0 0 0 0 1)))

;;; The disclosure seam (design §7): a node is disclosable iff its own flag is 1.
;;; Sites/surveys stay 1 throughout, so the predicate is downward-closed along the
;;; site->survey->find chain (a disclosable find's ancestors are disclosable).
(defun peer-test-disclosable (vertex graph scope)
  (declare (ignore graph scope))
  (= 1 (slot-value vertex 'disclosable)))
