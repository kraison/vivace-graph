;;;; Tests for map and map-reduce views (views.lisp).
;;;;
;;;; Views are defined against a *live* graph, and are maintained as nodes are
;;;; saved -- so each test defines its views (via DEFINE-TEST-VIEWS) on the
;;;; fresh WITH-TEST-GRAPH graph *before* inserting data.  Reuses the schema
;;;; (g-person, g-likes) defined in graph-tests.lisp.

(in-package #:graph-db/test)

(defun define-test-views ()
  "Define the views used by this suite against the current *graph*."
  ;; A simple map view: persons indexed by name.
  (def-view people-by-name :lessp (g-person :graph-db-integration-test)
    (:map (lambda (p)
            (when (slot-value p 'name)
              (yield (slot-value p 'name) nil)))))
  ;; The same map view sorted :greaterp (descending) -- exercises the
  ;; greaterp + :key / range path (issue #18).
  (def-view people-by-name-desc :greaterp (g-person :graph-db-integration-test)
    (:map (lambda (p)
            (when (slot-value p 'name)
              (yield (slot-value p 'name) nil)))))
  ;; A map-reduce view: number of likes received, keyed by target id.
  (def-view likes-received :greaterp (g-likes :graph-db-integration-test)
    (:map (lambda (e) (yield (string-id (to e)) 1)))
    (:reduce (lambda (keys vals) (declare (ignore keys)) (apply #'+ vals)))))

(def-suite view-suite
  :description "map and map-reduce views."
  :in graph-db-suite)

(in-suite view-suite)

(test map-view-lookup-by-key
  "A map view returns, for a key, the ids of the nodes that yielded it."
  (with-test-graph (g)
    (define-test-views)
    (let (alice-id)
      (with-transaction ()
        (setq alice-id (id (make-g-person :name "Alice")))
        (make-g-person :name "Bob"))
      (let ((hits (invoke-graph-view 'g-person 'people-by-name :key "Alice")))
        (is (= 1 (length hits)))
        (is (string= "Alice" (cdr (assoc :key (first hits)))))
        (is (equalp alice-id (cdr (assoc :id (first hits)))))))))

(test map-view-missing-key
  (with-test-graph (g)
    (define-test-views)
    (with-transaction () (make-g-person :name "Alice"))
    (is (null (invoke-graph-view 'g-person 'people-by-name :key "Nobody")))))

(test map-view-reflects-new-nodes
  "Nodes inserted after the view exists are indexed incrementally."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction () (make-g-person :name "Zed"))
    (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Zed"))))
    (with-transaction () (make-g-person :name "Zed"))
    (is (= 2 (length (invoke-graph-view 'g-person 'people-by-name :key "Zed"))))))

(test reduce-view-sums-per-key
  "A map-reduce view aggregates values per key (likes received per target)."
  (with-test-graph (g)
    (define-test-views)
    (let (pie-id cake-id)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B"))
              (c (make-g-person :name "C"))
              (pie (make-g-person :name "Pie"))
              (cake (make-g-person :name "Cake")))
          (setq pie-id (id pie) cake-id (id cake))
          (make-g-likes :from a :to pie)
          (make-g-likes :from b :to pie)
          (make-g-likes :from c :to cake)))
      (let ((counts (map-reduced-view (lambda (key id value)
                                        (declare (ignore id))
                                        (cons key value))
                                      'g-likes 'likes-received
                                      :collect-p t)))
        (is (= 2 (cdr (assoc (string-id pie-id) counts :test #'string=))))
        (is (= 1 (cdr (assoc (string-id cake-id) counts :test #'string=))))))))

;;; ---------------------------------------------------------------------------
;;; :greaterp (descending) map views  --  regression for issue #18
;;; ---------------------------------------------------------------------------

(test greaterp-map-view-lookup-by-key
  "Regression for issue #18: a :greaterp map view returns the matching node for
a :key lookup (it previously returned nothing because the per-key range bounds
were not reversed for descending order)."
  (with-test-graph (g)
    (define-test-views)
    (let (bob-id)
      (with-transaction ()
        (make-g-person :name "Alice")
        (setq bob-id (id (make-g-person :name "Bob")))
        (make-g-person :name "Carol"))
      (let ((hits (invoke-graph-view 'g-person 'people-by-name-desc :key "Bob")))
        (is (= 1 (length hits)) "greaterp view :key must find the node (issue #18)")
        (is (string= "Bob" (cdr (assoc :key (first hits)))))
        (is (equalp bob-id (cdr (assoc :id (first hits))))))
      ;; and a key with no entry still yields nothing
      (is (null (invoke-graph-view 'g-person 'people-by-name-desc :key "Nobody"))))))

(test greaterp-vs-lessp-scan-order
  "A full scan of a :greaterp view is descending by key; the :lessp view ascends."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (make-g-person :name "Alice")
      (make-g-person :name "Bob")
      (make-g-person :name "Carol"))
    (flet ((keys (view)
             (mapcar (lambda (h) (cdr (assoc :key h)))
                     (invoke-graph-view 'g-person view))))
      (is (equal '("Alice" "Bob" "Carol") (keys 'people-by-name)))
      (is (equal '("Carol" "Bob" "Alice") (keys 'people-by-name-desc))))))

(test greaterp-map-view-range
  "A :start-key/:end-key range on a :greaterp view returns the descending slice
(start = high key, end = low key)."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (dolist (n '("a" "b" "c" "d" "e")) (make-g-person :name n)))
    (let ((slice (mapcar (lambda (h) (cdr (assoc :key h)))
                         (invoke-graph-view 'g-person 'people-by-name-desc
                                            :start-key "d" :end-key "b"))))
      (is (equal '("d" "c" "b") slice)
          "descending slice from d down to b should be (d c b); got ~S" slice))))

;;; ---------------------------------------------------------------------------
;;; View maintenance: updates, deletions, paging, delete-view, persistence
;;; ---------------------------------------------------------------------------

(test view-reflects-slot-update
  "Updating a node's indexed slot moves it in the view: the old key loses the
entry and the new key gains it (update-in-views: remove + re-add)."
  (with-test-graph (g)
    (define-test-views)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "Alice"))))
      (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Alice"))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'name) "Alicia")
          (save v)))
      (is (null (invoke-graph-view 'g-person 'people-by-name :key "Alice"))
          "old key should no longer be indexed")
      (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Alicia")))
          "new key should be indexed"))))

(test view-reflects-deletion
  "Deleting a node removes it from the view (remove-from-views)."
  (with-test-graph (g)
    (define-test-views)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "Gone"))))
      (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Gone"))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is (null (invoke-graph-view 'g-person 'people-by-name :key "Gone"))
          "deleted node should not appear in the view"))))

(test map-view-count-and-skip-paging
  "A view scan honours :count (limit) and :skip (offset), and they compose."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (dolist (n '("a" "b" "c" "d" "e")) (make-g-person :name n)))
    (flet ((ks (&rest args)
             (mapcar (lambda (h) (cdr (assoc :key h)))
                     (apply #'invoke-graph-view 'g-person 'people-by-name args))))
      (is (equal '("a" "b") (ks :count 2)) ":count limits results")
      (is (equal '("c" "d" "e") (ks :skip 2)) ":skip offsets results")
      (is (equal '("b" "c") (ks :skip 1 :count 2)) ":skip + :count page"))))

(test delete-view-then-invoke-signals
  "After delete-view the view is gone and invoking it signals invalid-view-error."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction () (make-g-person :name "X"))
    (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "X"))))
    (delete-view g 'g-person 'people-by-name)
    (signals graph-db::invalid-view-error
      (invoke-graph-view 'g-person 'people-by-name :key "X"))))

(test invoke-nonexistent-view-signals
  "Invoking a view that was never defined signals invalid-view-error."
  (with-test-graph (g)
    (define-test-views)
    (signals graph-db::invalid-view-error
      (invoke-graph-view 'g-person 'no-such-view :key "X"))))

(test views-persist-across-reopen
  "A view's definition and index survive close-graph + open-graph: it is
restored (restore-views) and remains queryable without re-defining it."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (define-test-views)
          (with-transaction ()
            (setq id (id (make-g-person :name "Persisted")))
            (make-g-person :name "Other")))
        (close-graph g :snapshot-p nil))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((hits (invoke-graph-view 'g-person 'people-by-name :key "Persisted")))
                 (is (= 1 (length hits)) "view should be restored and queryable")
                 (is (equalp id (cdr (assoc :id (first hits))))))
               (is (= 2 (length (invoke-graph-view 'g-person 'people-by-name)))
                   "restored view sees all indexed nodes"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test reduced-view-count-limits-groups
  "map-reduced-view honours :count, limiting the number of reduced groups."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (let ((p1 (make-g-person :name "P1"))
            (p2 (make-g-person :name "P2"))
            (p3 (make-g-person :name "P3"))
            (liker (make-g-person :name "Liker")))
        (make-g-likes :from liker :to p1)
        (make-g-likes :from liker :to p2)
        (make-g-likes :from liker :to p3)))
    (let ((all (map-reduced-view (lambda (k id v) (declare (ignore id v)) k)
                                 'g-likes 'likes-received :collect-p t))
          (two (map-reduced-view (lambda (k id v) (declare (ignore id v)) k)
                                 'g-likes 'likes-received :count 2 :collect-p t)))
      (is (= 3 (length all)) "three liked targets -> three groups")
      (is (= 2 (length two)) ":count 2 limits to two groups"))))

;;; ---------------------------------------------------------------------------
;;; Declarative / idempotent def-view  --  issue #49
;;;
;;; def-view registers a spec and reconciles against the graph (like def-vertex),
;;; rebuilding the persisted index ONLY when the definition actually changed.  The
;;; tests instrument regenerate-view to prove a restart / unchanged reload is O(1).
;;; ---------------------------------------------------------------------------

(defvar *view49-regens* nil
  "When bound to a cons, REGENERATE-VIEW increments its car -- lets a test assert
that a restart / unchanged reload does NOT rebuild the index.")

(defmethod graph-db::regenerate-view :after
    ((graph graph-db::graph) (class-name symbol) (view-name symbol))
  (when *view49-regens* (incf (car *view49-regens*))))

(defparameter *view49-graph-name* :graph-db-view49-test)

(defun define-view49-schema ()
  (def-vertex vv-item () ((bucket :initarg :bucket :accessor vv-bucket))
    :graph-db-view49-test))

;; Two literal view definitions -- distinct :MAP code, so VIEW-SPEC-DIFF sees the
;; second as a change (the code is marshalled to a string, so it cannot close over
;; a runtime divisor).
(defun define-view49-view/10 ()
  (def-view vv-by-bucket :lessp (vv-item :graph-db-view49-test)
    (:map (lambda (x) (yield (floor (slot-value x 'bucket) 10) 1)))
    (:reduce (lambda (keys vals &optional r) (declare (ignore keys r)) (reduce #'+ vals)))))

(defun define-view49-view/5 ()
  (def-view vv-by-bucket :lessp (vv-item :graph-db-view49-test)
    (:map (lambda (x) (yield (floor (slot-value x 'bucket) 5) 1)))
    (:reduce (lambda (keys vals &optional r) (declare (ignore keys r)) (reduce #'+ vals)))))

(defun view49-buckets (graph)
  (let ((*graph* graph))
    (sort (map-reduced-view (lambda (k id v) (declare (ignore id)) (cons k v))
                            'vv-item 'vv-by-bucket :graph graph :collect-p t)
          #'< :key #'car)))

(defun reset-view49-registry ()
  "Isolate a test's view registry from earlier tests (specs accumulate globally)."
  (remhash *view49-graph-name* graph-db::*schema-view-metadata*))

(test def-view-restart-is-o1
  "Restart does NOT rebuild an unchanged persisted view (issue #49 core): after
close + open the index is restored and queryable with ZERO regenerate-view calls."
  (with-temp-directory (dir)
    (reset-view49-registry)
    (let ((path (namestring dir)))
      (let ((g (make-graph *view49-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (define-view49-schema)
          (define-view49-view/10)
          (with-transaction () (dotimes (i 20) (make-vv-item :bucket (+ 20 i)))))
        (is (equal '((2 . 10) (3 . 10)) (view49-buckets g)))
        (close-graph g :snapshot-p nil))
      (let ((*view49-regens* (list 0)))
        (let ((g2 (open-graph *view49-graph-name* path)))
          (unwind-protect
               (progn
                 (is (= 0 (car *view49-regens*))
                     "reopen must NOT regenerate an unchanged view (got ~D)"
                     (car *view49-regens*))
                 (is (equal '((2 . 10) (3 . 10)) (view49-buckets g2))
                     "restored view is queryable and correct"))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test def-view-reload-unchanged-is-o1
  "Re-evaluating an UNCHANGED def-view on an open graph does not rebuild."
  (with-temp-directory (dir)
    (reset-view49-registry)
    (let ((path (namestring dir)))
      (let ((g (make-graph *view49-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (progn
               (let ((*graph* g))
                 (define-view49-schema)
                 (define-view49-view/10)
                 (with-transaction () (dotimes (i 10) (make-vv-item :bucket (+ 20 i)))))
               (let ((*view49-regens* (list 0)))
                 (let ((*graph* g)) (define-view49-view/10)) ; re-eval, unchanged
                 (is (= 0 (car *view49-regens*))
                     "re-evaluating an unchanged def-view must not rebuild")))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test def-view-change-rebuilds
  "Changing a view's :MAP rebuilds the index once, and the new results reflect the
new mapping."
  (with-temp-directory (dir)
    (reset-view49-registry)
    (let ((path (namestring dir)))
      (let ((g (make-graph *view49-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (progn
               (let ((*graph* g))
                 (define-view49-schema)
                 (define-view49-view/10)
                 (with-transaction () (dotimes (i 20) (make-vv-item :bucket (+ 20 i)))))
               (is (equal '((2 . 10) (3 . 10)) (view49-buckets g)))
               (let ((*view49-regens* (list 0)))
                 (let ((*graph* g)) (define-view49-view/5)) ; changed :MAP
                 (is (= 1 (car *view49-regens*)) "a changed def-view rebuilds once"))
               (is (equal '((4 . 5) (5 . 5) (6 . 5) (7 . 5)) (view49-buckets g))
                   "rebuilt view reflects the new mapping"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test def-view-before-open
  "A view can be defined while the graph is CLOSED (declarative): def-view just
registers -- no error, no rebuild -- and open-graph builds it."
  (with-temp-directory (dir)
    (reset-view49-registry)
    (let ((path (namestring dir)))
      (let ((g (make-graph *view49-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (define-view49-schema)
          (with-transaction () (dotimes (i 10) (make-vv-item :bucket (+ 20 i)))))
        (close-graph g :snapshot-p nil))
      (let ((*view49-regens* (list 0)))
        (finishes (define-view49-view/10))
        (is (= 0 (car *view49-regens*)) "def-view on a closed graph must not rebuild"))
      (let ((*view49-regens* (list 0)))
        (let ((g2 (open-graph *view49-graph-name* path)))
          (unwind-protect
               (progn
                 (is (= 1 (car *view49-regens*)) "open builds the co-located view once")
                 (is (equal '((2 . 10)) (view49-buckets g2))))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test open-graph-regenerate-views-forces-rebuild
  "open-graph :regenerate-views t forcibly rebuilds all views even when unchanged."
  (with-temp-directory (dir)
    (reset-view49-registry)
    (let ((path (namestring dir)))
      (let ((g (make-graph *view49-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (define-view49-schema)
          (define-view49-view/10)
          (with-transaction () (dotimes (i 10) (make-vv-item :bucket (+ 20 i)))))
        (close-graph g :snapshot-p nil))
      (let ((*view49-regens* (list 0)))
        (let ((g2 (open-graph *view49-graph-name* path :regenerate-views t)))
          (unwind-protect
               (progn
                 (is (= 1 (car *view49-regens*)) ":regenerate-views t forces one rebuild")
                 (is (equal '((2 . 10)) (view49-buckets g2))))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))
