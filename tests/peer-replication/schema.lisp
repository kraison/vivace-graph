;;;; Shared schema for the two-process peer-replication test.
;;;;
;;;; Loaded IDENTICALLY by both the hub and the device so their schema-digests
;;;; match and node type-ids line up on the wire (keep the def-vertex / def-edge
;;;; forms byte-for-byte identical on both sides).
;;;;
;;;; A small field-app-shaped graph: a DEPOT has INSPECTIONs, an inspection has
;;;; ITEMs.
;;;; Every node carries a DISCLOSABLE flag (1/0); the hub's export predicate
;;;; ships a node only when it is 1, which lets the test drive scope entry/exit
;;;; (a item flipped to 0 must be PURGED from the device on the next pull).

(in-package :graph-db)

(def-vertex p-depot ()
  ((name :type string)
   (disclosable :type integer))
  :peer-test-app)

(def-vertex p-inspection ()
  ((name :type string)
   (disclosable :type integer))
  :peer-test-app)

(def-vertex p-item ()
  ((name :type string)
   (disclosable :type integer))
  :peer-test-app)

(def-edge p-has-inspection () () :peer-test-app)   ; depot  -> inspection
(def-edge p-has-item   () () :peer-test-app)    ; inspection -> item

;;; A fixed device origin id known to BOTH processes (the hub mints it for real;
;;; here a constant avoids passing it through a file).
(defparameter *device-origin*
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(7 7 7 7 0 0 0 0 0 0 0 0 0 0 0 1)))

;;; The disclosure seam (design §7): a node is disclosable iff its own flag is 1.
;;; Depots/inspections stay 1 throughout, so the predicate is downward-closed
;;; along the
;;; depot->inspection->item chain (a disclosable item's ancestors are
;;; disclosable).
(defun peer-test-disclosable (vertex graph scope)
  (declare (ignore graph scope))
  (= 1 (slot-value vertex 'disclosable)))
