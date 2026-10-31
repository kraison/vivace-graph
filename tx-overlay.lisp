;;;; tx-overlay.lisp -- a transaction's own writes as the read paths see
;;;; them (GH #408).  Spec: docs/superpowers/specs/
;;;; 2026-10-29-read-your-own-writes-design.md, §4.3.
;;;;
;;;; The overlay is the write set keyed by id, later writes winning (the
;;;; MAKE-COMMIT-VIEW rule).  Every enumeration point skips a committed id
;;;; that has an effective write here and then appends the own live nodes
;;;; that match its shape.  The first implementation scans the table per
;;;; query; maintained buckets can replace it behind the same functions
;;;; (spec §6).

(in-package #:graph-db)

(defstruct (tx-overlay (:constructor %make-tx-overlay (writes ordered)))
  "WRITES: an id table, id -> the TX-WRITE that wins for that id.
ORDERED: every write in apply order, creates first (GH #411)."
  writes ordered)

(defun %build-overlay (tx)
  (let ((h (make-id-table))
        (ordered (writes tx)))
    (dolist (w ordered (%make-tx-overlay h ordered))
      (setf (gethash (id w) h) w))))

(defun overlay-of (tx)
  "TX's overlay, built on first use after a write and cached on TX."
  (or (%overlay-slot tx)
      (setf (%overlay-slot tx) (%build-overlay tx))))

(defun %invalidate-overlay (tx)
  "Drop TX's cached overlay; the next OVERLAY-OF rebuilds it."
  (setf (%overlay-slot tx) nil))

(defun overlay-effective-write (ov id)
  "The TX-WRITE that wins for ID under OV, or NIL."
  (values (gethash id (tx-overlay-writes ov))))

(defun overlay-writes (ov)
  "Every TX-WRITE under OV in apply order, a create then a delete of
one id both present.  For a reader that must predict the apply pass
write for write -- the count index's counters -- where the id table's
one effective write per id would hide the create (GH #411)."
  (tx-overlay-ordered ov))

(defun own-write-p (tx id)
  "True when TX has written ID."
  (and (overlay-effective-write (overlay-of tx) id) t))

(defun current-overlay (graph)
  "The open read-write transaction's overlay when reads on GRAPH should
see its writes: *READ-OWN-WRITES* set, *TRANSACTION* a TX on GRAPH with
at least one write.  NIL otherwise, and every enumeration point then
reads committed state only (spec §4.6)."
  (let ((tx *transaction*))
    (and *read-own-writes*
         (typep tx 'tx)
         (eq (graph tx) graph)
         (plusp (write-count tx))
         (overlay-of tx))))

(defun %overlay-select (ov pred include-deleted-p)
  (loop for w being the hash-values of (tx-overlay-writes ov)
        for n = (node w)
        when (and (or include-deleted-p (not (deleted-p n)))
                  (funcall pred n))
          collect n))

(defun overlay-live-nodes (ov)
  "The node of every effective write under OV that is not deleted."
  (%overlay-select ov (constantly t) nil))

(defun overlay-nodes (ov kind type-ids &key include-deleted-p)
  "Own nodes of KIND (:VERTEX or :EDGE) whose TYPE-ID is in TYPE-IDS."
  (let ((class (ecase kind (:vertex 'vertex) (:edge 'edge))))
    (%overlay-select ov
                     (lambda (n)
                       (and (typep n class)
                            (member (type-id n) type-ids)))
                     include-deleted-p)))

(defun overlay-edges-at (ov vertex-id direction type-ids
                         &key include-deleted-p)
  "Own edges of a type in TYPE-IDS hanging off VERTEX-ID: FROM it for
DIRECTION :OUT, TO it for :IN."
  (%overlay-select ov
                   (lambda (n)
                     (and (typep n 'edge)
                          (member (type-id n) type-ids)
                          (equalp vertex-id
                                  (ecase direction
                                    (:out (from n))
                                    (:in (to n))))))
                   include-deleted-p))

(defun overlay-edges-between (ov from-id to-id type-ids
                              &key include-deleted-p)
  "Own edges of a type in TYPE-IDS from FROM-ID to TO-ID."
  (%overlay-select ov
                   (lambda (n)
                     (and (typep n 'edge)
                          (member (type-id n) type-ids)
                          (equalp from-id (from n))
                          (equalp to-id (to n))))
                   include-deleted-p))
