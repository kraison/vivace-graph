;;;; Tests for the index-backed spatial queries (spatial-query.lisp): the Lisp
;;;; API (find-nodes-within / find-nodes-near) and the Prolog functors
;;;; (find-within/3, find-near/5).  Reuses the GEO-PLACE vertex + NODE-GEOMETRY
;;;; method defined in spatial-hook-tests.lisp.  Every query is scoped to
;;;; GEO-PLACE -- the one class these fixtures create.

(in-package #:graph-db/test)

(def-suite spatial-query-suite
  :description "Index-backed spatial queries: find-nodes-within/near + functors."
  :in graph-db-suite)

(in-suite spatial-query-suite)

;; Three synthetic places: A and B are ~400 m apart; FAR is ~900 km away.
(defparameter *q-a*   '(12.3424312d0 45.6720584d0))   ; lon lat
(defparameter *q-b*   '(12.3473283d0 45.6736314d0))
(defparameter *q-far* '(2.4682919d0 41.7763233d0))

(defmacro with-three-places ((g ida idb idfar) &body body)
  `(with-test-graph (,g)
     (let (,ida ,idb ,idfar)
       (with-transaction ()
         (setq ,ida   (id (make-geo-place :loc (make-point (first *q-a*) (second *q-a*))))
               ,idb   (id (make-geo-place :loc (make-point (first *q-b*) (second *q-b*))))
               ,idfar (id (make-geo-place :loc (make-point (first *q-far*) (second *q-far*))))))
       ;; LOCALLY so a test body may open with (declare (ignore ...)).
       (locally ,@body))))

(defun id-set (nodes)
  (mapcar #'id nodes))

(defun has-id-p (id nodes)
  (member id (id-set nodes) :test 'equalp))

;;; ---- Lisp API ----------------------------------------------------------

(test find-nodes-near-lisp
  "find-nodes-near returns local nodes within radius (nearest first), not distant."
  (with-three-places (g ida idb idfar)
    (let* ((hits (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 600d0 :graph g))
           (nodes (mapcar #'car hits)))
      (is (has-id-p ida nodes))
      (is (has-id-p idb nodes))
      (is (not (has-id-p idfar nodes)))
      ;; distances are sorted ascending; the self-node (A) is nearest
      (is (equalp ida (id (car (first hits)))))
      (is (every (lambda (nd) (<= (cdr nd) 600d0)) hits)))))

(test find-nodes-near-tight-radius
  "A 100 m radius excludes the ~400 m neighbour."
  (with-three-places (g ida idb idfar)
    (declare (ignore idfar))
    (let ((nodes (mapcar #'car (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 100d0 :graph g))))
      (is (has-id-p ida nodes))
      (is (not (has-id-p idb nodes))))))

(test find-nodes-near-wraps-the-antimeridian-and-the-poles
  "GH #287, end to end: the query path inherits the index window, so a
node across the dateline or across the pole must come back with its
true distance."
  (with-test-graph (g)
    (let (east west north-a north-b)
      (with-transaction ()
        (setq east    (id (make-geo-place :loc (make-point 179.5d0 0d0)))
              west    (id (make-geo-place :loc (make-point -179.5d0 0d0)))
              north-a (id (make-geo-place :loc (make-point 100d0 89.9d0)))
              north-b (id (make-geo-place :loc (make-point -100d0 89.9d0)))))
      (let ((nodes (mapcar #'car (find-nodes-near 'geo-place 0d0 179.5d0
                                                  200000d0 :graph g))))
        (is (has-id-p east nodes))
        (is (has-id-p west nodes) "the node across the dateline")
        (is (= 2 (length nodes))))
      (let ((hits (find-nodes-near 'geo-place 90d0 0d0 100000d0 :graph g)))
        (is (has-id-p north-a (mapcar #'car hits)))
        (is (has-id-p north-b (mapcar #'car hits)))
        (is (every (lambda (h) (< (cdr h) 12000d0)) hits)
            "both are ~11.1 km from the pole")))))

(test find-nodes-within-lisp
  "find-nodes-within returns nodes inside the region polygon only."
  (with-three-places (g ida idb idfar)
    (let* ((aoi (make-polygon '(((12.340 45.670) (12.350 45.670)
                                 (12.350 45.676) (12.340 45.676)
                                 (12.340 45.670)))))
           (nodes (find-nodes-within 'geo-place aoi :graph g)))
      (is (has-id-p ida nodes))
      (is (has-id-p idb nodes))
      (is (not (has-id-p idfar nodes))))))

(test find-nodes-within-continent-scale
  "REGRESSION: find-nodes-within with a country-sized AREA must complete (not OOM
in the index's bbox covering) and still return the nodes inside the polygon.  All
three places are inside the polygon, so a country-scale area returns all of
    them."
  (with-three-places (g ida idb idfar)
    (let* ((country (make-polygon '(((1.0d0 40.0d0) (16.0d0 40.0d0)
                                     (16.0d0 48.0d0) (1.0d0 48.0d0)
                                     (1.0d0 40.0d0)))))
           (nodes (find-nodes-within 'geo-place country :graph g)))
      (is (has-id-p ida nodes))
      (is (has-id-p idb nodes))
      (is (has-id-p idfar nodes) "FAR is inside the polygon too"))))

(test find-nodes-intersecting-continent-scale
  "REGRESSION: the find-nodes-intersecting path shares the same bbox covering and
must likewise survive a continent-sized window."
  (with-three-places (g ida idb idfar)
    (let* ((country (make-polygon '(((1.0d0 40.0d0) (16.0d0 40.0d0)
                                     (16.0d0 48.0d0) (1.0d0 48.0d0)
                                     (1.0d0 40.0d0)))))
           (nodes (find-nodes-intersecting 'geo-place country :graph g)))
      (is (has-id-p ida nodes))
      (is (has-id-p idb nodes))
      (is (has-id-p idfar nodes)))))

(test deleted-nodes-excluded
  "A deleted node is not returned by spatial queries."
  (with-three-places (g ida idb idfar)
    (declare (ignore idb idfar))
    (is (has-id-p ida (mapcar #'car (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 600d0 :graph g))))
    (with-transaction () (mark-deleted (lookup-vertex ida)))
    (is (not (has-id-p ida (mapcar #'car (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 600d0 :graph g)))))))

;;; ---- Prolog functors ---------------------------------------------------

(test find-near-functor
  "find-near/5 yields the local nodes in a query, composing with is-a."
  (with-three-places (g ida idb idfar)
    (declare (ignore g))
    (let ((ids (id-set (select-flat (?n)
                         (is-a ?n geo-place)
                         (find-near ?n geo-place 45.6720584d0 12.3424312d0
                           600d0)))))
      (is (member ida ids :test 'equalp))
      (is (member idb ids :test 'equalp))
      (is (not (member idfar ids :test 'equalp))))))

(test find-within-functor
  "find-within/3 yields nodes inside an area (area built via the is/2 escape)."
  (with-three-places (g ida idb idfar)
    (declare (ignore g))
    (let ((ids (id-set (select-flat (?n)
                         (is ?area (make-polygon '(((12.340d0 45.670d0)
                                                     (12.350d0 45.670d0)
                                                    (12.350d0 45.676d0)
                                                        (12.340d0 45.676d0)
                                                    (12.340d0 45.670d0)))))
                         (find-within ?n geo-place ?area)))))
      (is (member ida ids :test 'equalp))
      (is (member idb ids :test 'equalp))
      (is (not (member idfar ids :test 'equalp))))))

;;; ---- rebuild-spatial-indexes -------------------------------------------

;; GEO-PLACE has a hand-written NODE-GEOMETRY method and no :INDEX slot, so its
;; nodes are keyed by (GEO-PLACE . NIL): GEO-PLACE is the most general class
;; carrying that method -- see %NODE-SPATIAL-OWNER-NAME.
(defun q-place-index (g)
  (spatial-index-for g 'geo-place nil))

(test rebuild-repopulates-after-index-loss
  "Wiping the registry then rebuilding from the nodes restores query results."
  (with-three-places (g ida idb idfar)
    ;; simulate a lost/empty index: drop every index and clear the registry
    (dolist (idx (all-spatial-indexes g)) (delete-spatial-index idx))
    (clrhash (spatial-indexes g))
    (is (null (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 600d0 :graph g))
        "index is empty after the wipe")
    (let ((n (rebuild-spatial-indexes g)))
      (is (= n 3) "all three geometry-bearing nodes re-indexed")
      (let ((nodes (mapcar #'car (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 600d0 :graph g))))
        (is (has-id-p ida nodes))
        (is (has-id-p idb nodes))
        (is (not (has-id-p idfar nodes)))))))

(test rebuild-changes-precision
  "Changing the graph's default precision and rebuilding changes the grid and
keeps results correct."
  (with-three-places (g ida idb idfar)
    (declare (ignore idb idfar))
    (is (= 7 (spatial-index-precision (q-place-index g))))
    (setf (graph-default-spatial-precision g) 9)
    (rebuild-spatial-indexes g)
    (is (= 9 (spatial-index-precision (q-place-index g))))
    (is (has-id-p ida (mapcar #'car (find-nodes-near 'geo-place (second *q-a*) (first *q-a*) 600d0 :graph g))))))

;;; ---- kNN (find-nearest-k) ----------------------------------------------

(test find-nearest-k-orders-by-distance
  "find-nearest-k from A returns the K closest, nearest first: A then B (the
~400 m neighbour) before FAR ever appears."
  (with-three-places (g ida idb idfar)
    ;; k=1 -> just A (itself, distance ~0)
    (let ((one (find-nearest-k 'geo-place (second *q-a*) (first *q-a*) 1 :graph g)))
      (is (= 1 (length one)))
      (is (equalp ida (id (car (first one))))))
    ;; k=2 -> A then B, FAR excluded
    (let* ((two (find-nearest-k 'geo-place (second *q-a*) (first *q-a*) 2 :graph g))
           (ids (mapcar (lambda (nd) (id (car nd))) two)))
      (is (= 2 (length two)))
      (is (equalp ida (first ids)))
      (is (equalp idb (second ids)))
      (is (not (member idfar ids :test 'equalp)))
      ;; distances are non-decreasing
      (is (<= (cdr (first two)) (cdr (second two)))))))

(test find-nearest-k-caps-at-node-count
  "Asking for more neighbours than exist within MAX-RADIUS returns all of them
(here a 3-node cluster, all within ~1 km), sorted nearest-first.  Uses a local
cluster -- find-nearest-k is bounded by MAX-RADIUS (25 km default), so the far
FAR node of with-three-places would not be reached."
  (with-test-graph (g)
    (let (ida idb idc)
      (with-transaction ()
        (setq ida (id (make-geo-place :loc (make-point 12.3424d0 45.6720d0)))
              idb (id (make-geo-place :loc (make-point 12.3473d0 45.6736d0)))
                  ; ~400 m
              idc (id (make-geo-place :loc (make-point 12.3520d0 45.6780d0)))))
                  ; ~1 km
      (let* ((hits (find-nearest-k 'geo-place 45.6720d0 12.3424d0 10 :graph g))
             (ids (mapcar (lambda (nd) (id (car nd))) hits)))
        (is (= 3 (length hits)) "all 3 nodes returned (k exceeds node count)")
        (is (member ida ids :test 'equalp))
        (is (member idb ids :test 'equalp))
        (is (member idc ids :test 'equalp))
        (is (equalp ida (first ids)) "nearest is the query point itself")
        (is (equalp idc (car (last ids))) "farthest of the cluster is last")
        (is (apply #'<= (mapcar #'cdr hits)) "distances non-decreasing")))))

(test find-nearest-k-bounded-by-max-radius
  "find-nearest-k does not chase nodes past MAX-RADIUS: the FAR node (~900 km
away) is excluded even when K is large, and the call returns promptly
without enumerating a continent of grid cells."
  (with-three-places (g ida idb idfar)
    (let* ((hits (find-nearest-k 'geo-place (second *q-a*) (first *q-a*) 10 :graph g))
           (ids (mapcar (lambda (nd) (id (car nd))) hits)))
      (is (= 2 (length hits)) "only the two in-range nearby nodes")
      (is (member ida ids :test 'equalp))
      (is (member idb ids :test 'equalp))
      (is (not (member idfar ids :test 'equalp)) "FAR is beyond MAX-RADIUS"))))

(test find-nearest-functor
  "find-nearest/5 yields the k nearest nodes in a query, composing with is-a."
  (with-three-places (g ida idb idfar)
    (declare (ignore g idfar))
    (let ((ids (id-set (select-flat (?n)
                         (is-a ?n geo-place)
                         (find-nearest ?n geo-place 45.6720584d0 12.3424312d0
                           2)))))
      (is (= 2 (length ids)))
      (is (member ida ids :test 'equalp))
      (is (member idb ids :test 'equalp)))))

(test find-nearest-k-degenerate-args
  "Non-positive K (or no index) yields NIL rather than erroring."
  (with-three-places (g ida idb idfar)
    (declare (ignore ida idb idfar))
    (is (null (find-nearest-k 'geo-place (second *q-a*) (first *q-a*) 0 :graph g)))
    (is (null (find-nearest-k 'geo-place (second *q-a*) (first *q-a*) -3 :graph g)))))

;;; ---- GH #410: the transaction overlay ----------------------------------

(defparameter *own-box*
  (make-polygon '(((12.340d0 45.670d0) (12.350d0 45.670d0)
                   (12.350d0 45.676d0) (12.340d0 45.676d0)
                   (12.340d0 45.670d0))))
  "A polygon holding A, B and the created C; FAR and the moved B are out.")

(defmacro with-own-spatial-writes ((g ida idb idfar idc idd) &body body)
  "WITH-THREE-PLACES plus a committed D ~25 m from A, then one open
transaction that creates C ~60 m from A, nudges D ~10 m (still beside
A), moves B ~900 km to FAR's neighbourhood and deletes A.  BODY runs
inside that transaction and again after it commits: both must agree
\(GH #410).  D is the node the walk still finds under its old cell AND
the overlay supplies: without the walk's skip it would answer twice."
  `(with-three-places (,g ,ida ,idb ,idfar)
     (declare (ignorable ,ida ,idb ,idfar))
     (let (,idc ,idd)
       (declare (ignorable ,idc ,idd))
       (with-transaction ()
         (setq ,idd (id (make-geo-place
                         :loc (make-point 12.3426d0 45.6722d0)))))
       (flet ((check () ,@body))
         (with-transaction ()
           (setq ,idc (id (make-geo-place
                           :loc (make-point 12.3430d0 45.6725d0))))
           (let ((d (copy (lookup-vertex ,idd))))
             (setf (slot-value d 'loc) (make-point 12.3427d0 45.6722d0))
             (save d))
           (let ((b (copy (lookup-vertex ,idb))))
             (setf (slot-value b 'loc) (make-point 2.4690d0 41.7770d0))
             (save b))
           (mark-deleted (lookup-vertex ,ida))
           (check))
         (check)))))

(test find-nodes-near-sees-own-writes
  "GH #410: FIND-NODES-NEAR finds the place the transaction created, the
one it nudged once, drops the one it deleted, and finds the one it moved
at its new location only."
  (with-own-spatial-writes (g ida idb idfar idc idd)
    (let ((near-a (mapcar #'car (find-nodes-near 'geo-place (second *q-a*)
                                                 (first *q-a*) 600d0
                                                 :graph g)))
          (near-far (mapcar #'car (find-nodes-near 'geo-place
                                                   (second *q-far*)
                                                   (first *q-far*) 2000d0
                                                   :graph g))))
      (is (has-id-p idc near-a) "the created place")
      (is (has-id-p idd near-a) "the nudged place")
      (is (= 2 (length near-a)) "each once: C and D")
      (is (not (has-id-p ida near-a)) "the deleted place")
      (is (not (has-id-p idb near-a)) "the moved place, not where it was")
      (is (has-id-p idb near-far) "the moved place, where it is now")
      (is (has-id-p idfar near-far)))))

(test find-nodes-within-sees-own-writes
  "GH #410: FIND-NODES-WITHIN on the box holding A, B, C and D answers C
and D, each once, with A deleted and B moved out."
  (with-own-spatial-writes (g ida idb idfar idc idd)
    (let ((in (find-nodes-within 'geo-place *own-box* :graph g)))
      (is (= 2 (length in)))
      (is (has-id-p idc in))
      (is (has-id-p idd in)))))

(test find-nodes-intersecting-sees-own-writes
  "GH #410: FIND-NODES-INTERSECTING, the same shape."
  (with-own-spatial-writes (g ida idb idfar idc idd)
    (let ((in (find-nodes-intersecting 'geo-place *own-box* :graph g)))
      (is (= 2 (length in)))
      (is (has-id-p idc in))
      (is (has-id-p idd in)))))

(test find-nearest-k-sees-own-writes
  "GH #410: FIND-NEAREST-K at A asks for three and gets D then C: A is
deleted and B is beyond MAX-RADIUS now."
  (with-own-spatial-writes (g ida idb idfar idc idd)
    (let ((hits (find-nearest-k 'geo-place (second *q-a*) (first *q-a*) 3
                                :graph g)))
      (is (= 2 (length hits)))
      (is (equalp idd (id (car (first hits)))))
      (is (equalp idc (id (car (second hits))))))))
