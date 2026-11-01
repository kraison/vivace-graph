;;;; #60: a node is indexed under EVERY declared geometry slot, and a query
;;;; names the slot it tests.  Spec: docs/superpowers/specs/
;;;; 2026-11-01-multi-slot-spatial-indexing-design.md

(in-package #:graph-db/test)

(def-suite spatial-multi-slot-suite
  :description "Per-slot spatial indexing and the :SLOT argument (#60)."
  :in graph-db-suite)

(in-suite spatial-multi-slot-suite)

;; The outline is a degree across: precision 5 keeps its cover under the cell
;; cap, so no coarsening warning muddies the output (as register-tests does).
(def-vertex ms-area ()
  ((centroid :type geometry :index t)
   (outline  :type geometry :index t :spatial-precision 5))
  :graph-db-integration-test)

(defun ms-rect (min-lon min-lat max-lon max-lat)
  (make-polygon (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(defun ms-in-index-p (g slot id min-lon min-lat max-lon max-lat)
  "True when the MS-AREA index for SLOT answers ID inside the window."
  (let ((idx (spatial-index-for g 'ms-area slot)))
    (and idx (has-p id (spatial-index-query-bbox idx min-lon min-lat
                                                 max-lon max-lat))
         t)))

;; The centroid's cell and the outline's cells are disjoint, so each index
;; can be asked about its own window and the other's.
(defparameter *ms-centroid* (list 30d0 50d0))
(defparameter *ms-outline* (list 35d0 55d0 36d0 56d0))
(defparameter *ms-centroid-window* '(29.9d0 49.9d0 30.1d0 50.1d0))
(defparameter *ms-outline-window* '(34.9d0 54.9d0 36.1d0 56.1d0))

(defun ms-make-area ()
  (make-ms-area :centroid (apply #'make-point *ms-centroid*)
                :outline (apply #'ms-rect *ms-outline*)))

(defun ms-in-centroid-window-p (g slot id)
  (apply #'ms-in-index-p g slot id *ms-centroid-window*))

(defun ms-in-outline-window-p (g slot id)
  (apply #'ms-in-index-p g slot id *ms-outline-window*))

;;; ---------------------------------------------------------------------------
;;; Spec §5.2: index membership follows NODE-GEOMETRIES on every write path.
;;; ---------------------------------------------------------------------------

(test create-indexes-both-slots
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (ms-make-area))))
      (is-true (ms-in-centroid-window-p g 'centroid id))
      (is-true (ms-in-outline-window-p g 'outline id))
      (is-false (ms-in-outline-window-p g 'centroid id))
      (is-false (ms-in-centroid-window-p g 'outline id)))))

(test update-that-nils-a-slot-leaves-its-index
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (ms-make-area))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'outline) nil)
          (save v)))
      (is-true (ms-in-centroid-window-p g 'centroid id))
      (is-false (ms-in-outline-window-p g 'outline id))
      (is (spatial-index-p (spatial-index-for g 'ms-area 'outline))
          "the index itself survives; only the entry left"))))

(test update-that-swaps-the-slots-reindexes-each
  "Review Focus 1: the centroid moves into the old outline's window and the
outline shrinks around the old centroid; each index must hold the node in its
NEW window only."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (ms-make-area))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'centroid) (make-point 35.5d0 55.5d0)
                (slot-value v 'outline) (ms-rect 29.95d0 49.95d0
                                                 30.05d0 50.05d0))
          (save v)))
      (is-true (ms-in-outline-window-p g 'centroid id))
      (is-false (ms-in-centroid-window-p g 'centroid id))
      (is-true (ms-in-centroid-window-p g 'outline id))
      (is-false (ms-in-outline-window-p g 'outline id)))))

(test delete-removes-from-both-indexes
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (ms-make-area))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is-false (ms-in-centroid-window-p g 'centroid id))
      (is-false (ms-in-outline-window-p g 'outline id)))))

(test rebuild-recreates-both-indexes
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (ms-make-area))))
      (is (= 2 (rebuild-spatial-indexes g)) "one insert per geometry")
      (is-true (ms-in-centroid-window-p g 'centroid id))
      (is-true (ms-in-outline-window-p g 'outline id)))))

(test regenerate-one-slot-leaves-the-other
  "Review Focus 2."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (ms-make-area))))
      (is (= 1 (regenerate-spatial-index g 'ms-area 'outline)))
      (is-true (ms-in-outline-window-p g 'outline id))
      (is-true (ms-in-centroid-window-p g 'centroid id)))))

(test overlay-own-node-is-a-candidate-under-either-slot
  "GH #410 inside #60: an own node written in the open transaction is a
candidate under whichever slot the query names."
  (if t
      (skip "Task 4 adds :SLOT")
      (with-test-graph (g)
        (declare (ignore g))
        (with-transaction ()
          (let ((n (ms-make-area)))
            (is (member n (find-nodes-within 'ms-area
                                             (ms-rect 29d0 49d0 31d0 51d0)
                                             :slot 'centroid)))
            (is (member n (find-nodes-within 'ms-area
                                             (ms-rect 34d0 54d0 37d0 57d0)
                                             :slot 'outline))))))))
