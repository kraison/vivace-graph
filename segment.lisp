(in-package :graph-db)

;;; Vector segment: a derived, mmap-backed index holding one fixed-width
;;; single-float vector per node, addressable by node id.  See
;;; docs/superpowers/specs/2026-07-20-vector-segments-design.md sec 5.
;;;
;;; This file is the FILE FORMAT and its unit operations ONLY.  Transaction
;;; hooks, rebuild-from-nodes, and scan/score are later steps.
;;;
;;; The on-disk id array is authoritative.  ID->SLOT is a RAM-only hash rebuilt
;;; at open by sweeping it (sec 5.1); it is never persisted.

(defstruct (vector-segment (:constructor %make-vector-segment)
                           (:predicate vector-segment-p))
  (mmap nil)                 ; a mapped-file (mmap.lisp)
  (dimension 0 :type fixnum) ; fixed at create time
  (id->slot nil))            ; equalp hash: 16-byte id vector -> slot index
