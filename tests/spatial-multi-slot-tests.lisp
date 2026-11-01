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
  (with-test-graph (g)
    (declare (ignore g))
    (with-transaction ()
      (let ((n (ms-make-area)))
        (is (member n (find-nodes-within 'ms-area
                                         (ms-rect 29d0 49d0 31d0 51d0)
                                         :slot 'centroid)))
        (is (member n (find-nodes-within 'ms-area
                                         (ms-rect 34d0 54d0 37d0 57d0)
                                         :slot 'outline)))))))

;;; ---------------------------------------------------------------------------
;;; Spec §5.3-5.4, §6.1: a query names the slot it tests.
;;; ---------------------------------------------------------------------------

(def-vertex ms-base () ((loc :type geometry :index t))
  :graph-db-integration-test)
(def-vertex ms-special (ms-base)
  ((precise-loc :type geometry :index t))
  :graph-db-integration-test)
(def-vertex ms-only-sub (ms-base) () :graph-db-integration-test)

(defparameter *ms-area-of-interest* (list 29.8d0 49.8d0 30.2d0 50.2d0))

(defun ms-area-of-interest () (apply #'ms-rect *ms-area-of-interest*))

(defmacro ms-signals-naming (class-name &body body)
  "True when BODY signals QUERY-PRECONDITION-ERROR whose text names CLASS-NAME."
  `(handler-case (progn ,@body nil)
     (query-precondition-error (c)
       (and (search ,class-name (string-upcase (princ-to-string c))) t))))

(test within-by-centroid-and-by-outline-differ
  "Spec §2: a centroid query is a centroid query.  B's outline crosses the area
but its centroid lies far outside it."
  (with-test-graph (g)
    (declare (ignore g))
    (let (a b (area (ms-area-of-interest)))
      (with-transaction ()
        (setq a (make-ms-area :centroid (make-point 30d0 50d0)
                              :outline (ms-rect 29d0 49d0 31d0 51d0))
              b (make-ms-area :centroid (make-point 40d0 60d0)
                              :outline (ms-rect 29.5d0 49.5d0 30.5d0 50.5d0))))
      (is (equal (list a) (find-nodes-within 'ms-area area :slot 'centroid)))
      (is (equal (list a)
                 (find-nodes-intersecting 'ms-area area :slot 'centroid)))
      (is (= 2 (length (find-nodes-intersecting 'ms-area area
                                                :slot 'outline))))
      (if graph-db::*geos-available-p*
          (is (null (find-nodes-within 'ms-area area :slot 'outline))
              "neither outline lies within the area")
          (skip "GEOS not available: exact containment of outlines")))))

(test near-distance-is-measured-to-the-named-slot
  (with-test-graph (g)
    (declare (ignore g))
    (with-transaction ()
      (make-ms-area :centroid (make-point 30d0 50d0)
                    :outline (ms-rect 30.5d0 50.5d0 30.7d0 50.7d0)))
    (let ((by-centroid (find-nodes-near 'ms-area 50d0 30d0 1d5
                                        :slot 'centroid))
          (by-outline (find-nodes-near 'ms-area 50d0 30d0 1d5
                                       :slot 'outline))
          (expected (geodesic-distance 50d0 30d0 50.6d0 30.6d0)))
      (is (= 1 (length by-centroid)))
      (is (< (cdr (first by-centroid)) 1d0))
      (is (= 1 (length by-outline)))
      (is (< (abs (- (cdr (first by-outline)) expected)) (* 0.01 expected))
          "distance to the outline's representative point"))))

(test nearest-k-orders-by-the-named-slot
  (with-test-graph (g)
    (declare (ignore g))
    (let (p q)
      (with-transaction ()
        (setq p (make-ms-area :centroid (make-point 30.01d0 50.01d0)
                              :outline (ms-rect 30.1d0 50.1d0 30.2d0 50.2d0))
              q (make-ms-area :centroid (make-point 30.1d0 50.1d0)
                              :outline (ms-rect 29.99d0 49.99d0
                                                30.03d0 50.03d0))))
      (is (equal (list p q)
                 (mapcar #'car (find-nearest-k 'ms-area 50d0 30d0 2
                                               :slot 'centroid))))
      (is (equal (list q p)
                 (mapcar #'car (find-nearest-k 'ms-area 50d0 30d0 2
                                               :slot 'outline)))))))

(test unnamed-query-on-a-multi-slot-class-follows-the-policy
  (with-test-graph (g)
    (declare (ignore g))
    (let ((area (ms-area-of-interest)))
      (with-transaction () (ms-make-area))
      (let ((graph-db::*spatial-unnamed-slot-policy* :error))
        (is-true (ms-signals-naming "MS-AREA"
                   (find-nodes-within 'ms-area area)))
        (is-true (ms-signals-naming "MS-AREA"
                   (find-nodes-intersecting 'ms-area area)))
        (is-true (ms-signals-naming "MS-AREA"
                   (find-nodes-near 'ms-area 50d0 30d0 1d4)))
        (is-true (ms-signals-naming "MS-AREA"
                   (find-nearest-k 'ms-area 50d0 30d0 1))))
      (let ((graph-db::*spatial-unnamed-slot-policy* :first-declared)
            (warnings 0))
        (remhash (find-class 'ms-area)
                 graph-db::*spatial-first-declared-warned*)
        (handler-bind ((warning (lambda (c)
                                  (when (search "FIRST-DECLARED"
                                                (string-upcase
                                                 (princ-to-string c)))
                                    (incf warnings))
                                  (muffle-warning c))))
          (is (equal (find-nodes-within 'ms-area area :slot 'centroid)
                     (find-nodes-within 'ms-area area)))
          (is (equal (find-nodes-near 'ms-area 50d0 30d0 1d4 :slot 'centroid)
                     (find-nodes-near 'ms-area 50d0 30d0 1d4))))
        (is (= 1 warnings) "one warning per class")))))

(test single-slot-class-is-unchanged-with-or-without-slot
  (with-test-graph (g)
    (declare (ignore g))
    (let ((area (ms-rect 12.3d0 45.6d0 12.4d0 45.7d0)) (warned nil))
      (with-transaction ()
        (make-scope-probe :geom (make-point 12.3424d0 45.6720d0)))
      (handler-bind ((warning (lambda (c) (setf warned c) (muffle-warning c))))
        (dolist (policy '(:error :first-declared))
          (let ((graph-db::*spatial-unnamed-slot-policy* policy))
            (is (= 1 (length (find-nodes-within 'scope-probe area))))
            (is (equal (find-nodes-within 'scope-probe area)
                       (find-nodes-within 'scope-probe area :slot 'geom)))
            (is (equal (find-nodes-near 'scope-probe 45.672d0 12.3424d0 100d0)
                       (find-nodes-near 'scope-probe 45.672d0 12.3424d0 100d0
                                        :slot 'geom))))))
      (is (null warned)))))

(test parent-scope-reaches-a-subclass-slot
  "The asymmetry folded into #60: a scope on the parent reaches an index a
subclass declares on its own slot, by name."
  (with-test-graph (g)
    (declare (ignore g))
    (let (special only-sub (area (ms-area-of-interest)))
      (with-transaction ()
        (setq special (make-ms-special :loc (make-point 40d0 60d0)
                                       :precise-loc (make-point 30d0 50d0))
              only-sub (make-ms-only-sub :loc (make-point 30.05d0 50.05d0))))
      (is (equal (list special)
                 (find-nodes-within 'ms-base area :slot 'precise-loc)))
      (is (equal (list only-sub)
                 (find-nodes-within 'ms-base area :slot 'loc)))
      (let ((graph-db::*spatial-unnamed-slot-policy* :error))
        (is-true (ms-signals-naming "MS-SPECIAL"
                   (find-nodes-within 'ms-base area))
                 "the subclass is multi-slot: unnamed parent scope signals"))
      (is (member special (find-nodes-within :all area :slot 'precise-loc)))
      (is (equal (list only-sub) (find-nodes-within 'ms-only-sub area))
          "a subclass with one inherited slot works unnamed"))))

(test bad-slot-signals
  "Review Focus 3: a slot nothing declares, in any spelling, signals."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((area (ms-area-of-interest)))
      (with-transaction () (ms-make-area))
      (is-true (ms-signals-naming "MS-AREA"
                 (find-nodes-within 'ms-area area :slot 'nothing)))
      (is-true (ms-signals-naming "MS-AREA"
                 (find-nodes-within 'ms-area area :slot :centroid)))
      (is-true (ms-signals-naming "MS-AREA"
                 (find-nodes-within 'ms-area area :slot "centroid"))))))

(test custom-method-class-with-slot
  "Spec §5.1/§5.3: an application NODE-GEOMETRY method keys each node once; a
:SLOT reaches the nodes keyed under that declared slot and nothing else."
  (with-test-graph (g)
    (declare (ignore g))
    (let (n (area (ms-area-of-interest)))
      (with-transaction ()
        (setq n (make-scope-two-geoms-reporting
                 :centroid (make-point 30d0 50d0)
                 :outline (scope-rect 35d0 55d0 36d0 56d0))))
      (is (equal (list n) (find-nodes-within 'scope-two-geoms-reporting area
                                             :slot 'centroid)))
      (is (null (find-nodes-within 'scope-two-geoms-reporting
                                   (scope-rect 34d0 54d0 37d0 57d0)
                                   :slot 'outline))
          "the method never keys a node under OUTLINE")
      (is-true (ms-signals-naming "SCOPE-TWO-GEOMS-REPORTING"
                 (find-nodes-within 'scope-two-geoms-reporting area
                                    :slot 'nothing)))
      (is-true (ms-signals-naming "SCOPE-SITE"
                 (find-nodes-within 'scope-site area :slot 'where))
               "a one-value method class has no named slot")
      (finishes (find-nodes-within :all area :slot 'precise-loc)))))

(def-vertex ms-redef () ((a :type geometry :index t))
  :graph-db-integration-test)

(test redefining-a-class-to-two-slots-flips-the-unnamed-rule
  "Review Focus 4: the memo follows the class."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((area (ms-area-of-interest))
          (graph-db::*spatial-unnamed-slot-policy* :error))
      (with-transaction () (make-ms-redef :a (make-point 30d0 50d0)))
      (is (= 1 (length (find-nodes-within 'ms-redef area))))
      (unwind-protect
           (progn
             (def-vertex ms-redef ()
               ((a :type geometry :index t) (b :type geometry :index t))
               :graph-db-integration-test)
             (is-true (ms-signals-naming "MS-REDEF"
                        (find-nodes-within 'ms-redef area))))
        (def-vertex ms-redef () ((a :type geometry :index t))
          :graph-db-integration-test))
      (is (= 1 (length (find-nodes-within 'ms-redef area)))))))

;;; ---------------------------------------------------------------------------
;;; Spec §6.2: the slot-taking Prolog arities, next to their unnamed twins.
;;; ---------------------------------------------------------------------------

(test prolog-arities-take-the-slot
  "find-within/4, find-intersects/4, find-near/6 and find-nearest/6 carry the
slot in third position.  Both outlines are centred on (30 50); only A's
centroid is."
  (with-test-graph (g)
    (declare (ignore g))
    (let (a)
      (with-transaction ()
        (setq a (make-ms-area :centroid (make-point 30d0 50d0)
                              :outline (ms-rect 29d0 49d0 31d0 51d0)))
        (make-ms-area :centroid (make-point 40d0 60d0)
                      :outline (ms-rect 29.5d0 49.5d0 30.5d0 50.5d0)))
      (is (equal (list a)
                 (select-flat (?n)
                   (is ?area (ms-area-of-interest))
                   (find-within ?n ms-area centroid ?area))))
      (is (= 2 (length (select-flat (?n)
                         (is ?area (ms-area-of-interest))
                         (find-intersects ?n ms-area outline ?area)))))
      (is (equal (list a)
                 (select-flat (?n)
                   (find-near ?n ms-area centroid 50d0 30d0 1d4))))
      (is (= 2 (length (select-flat (?n)
                         (find-near ?n ms-area outline 50d0 30d0 1d4)))))
      (is (equal (list a)
                 (select-flat (?n)
                   (find-nearest ?n ms-area centroid 50d0 30d0 1))))
      (is (= 2 (length (select-flat (?n)
                         (find-nearest ?n ms-area outline 50d0 30d0 2))))))))

(test prolog-literal-list-scope-with-slot
  (with-test-graph (g)
    (declare (ignore g))
    (let (sub)
      (with-transaction ()
        (setq sub (make-ms-only-sub :loc (make-point 30.05d0 50.05d0))))
      (is (equal (list sub)
                 (select-flat (?n)
                   (is ?area (ms-area-of-interest))
                   (find-within ?n (ms-base ms-only-sub) loc ?area)))
          "a literal list scope with a slot, deduped across the two classes"))))

(test nearest-k-finds-an-own-node-before-any-index-exists
  "Review Focus 5: GH #410's rule under :SLOT -- no index yet, the open
transaction's own node is the answer."
  (with-test-graph (g)
    (declare (ignore g))
    (with-transaction ()
      (let ((n (ms-make-area)))
        (is (equal (list n)
                   (mapcar #'car (find-nearest-k 'ms-area 55.5d0 35.5d0 1
                                                 :slot 'outline))))))))
