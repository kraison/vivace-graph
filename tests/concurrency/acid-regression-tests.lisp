;;;; CONCURRENT-ACID-REGRESSION-SUITE
;;;;
;;;; Regression guards for known ACID correctness issues in the transaction
;;;; engine, plus infrastructure-level safety checks (view/lhash ordering,
;;;; allocator isolation).  These tests are kept separate so they evolve in
;;;; lockstep with targeted bug fixes rather than being mixed into the
;;;; general suite.

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-acid-regression-suite
  :description "Regression tests for ACID correctness issues in the transaction engine."
  :in concurrency-suite)

(in-suite concurrent-acid-regression-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: multi-vertex lost-update regression
;;;
;;; The original sustained-conflict-storm test revealed that with exactly 2
;;; threads per vertex (low per-vertex contention), ~16% of increments were
;;; silently lost.  With low contention the exclusive-lock fallback rarely
;;; fires, exposing a subtle race in the MVCC validation path.
;;;
;;; This test replicates the original failing pattern as a permanent regression
;;; guard: 8 threads, 5 hot vertices, 2 threads per vertex, 20 increments each.
;;; Each vertex must end at exactly 40 (2 threads × 20 increments).
;;; ---------------------------------------------------------------------------

(test multi-vertex-lost-update-regression
  "10 threads × 5 vertices, 2 threads per vertex, 20 increments each;
each vertex final value must equal 40 with no lost updates."
  (let* ((n-threads  10)
         (n-vertices 5)
         (k          20))    ; increments per thread → 2 threads × 20 = 40 per vertex
    (with-conc-graph (g)
      (let ((counter-ids (make-array n-vertices)))
        (dotimes (v n-vertices)
          (with-transaction ()
            (setf (aref counter-ids v) (id (make-c-item :value 0)))))
        ;; Thread i always increments vertex (floor i 2):
        ;;   threads 0-1 → vertex 0, threads 2-3 → vertex 1, …, threads 8-9 → vertex 4.
        ;; This gives exactly 2-thread contention per vertex — the low-contention
        ;; path that bypasses the exclusive-lock fallback and exposed the lost-update.
        (run-threads n-threads
                     (lambda (i)
                       (let ((vid (aref counter-ids (floor i 2))))
                         (dotimes (_ k)
                           (with-transaction ()
                             (let* ((item (copy (lookup-vertex vid)))
                                    (old  (slot-value item 'value)))
                               (setf (slot-value item 'value) (1+ old))
                               (save item)))))))
        (dotimes (v n-vertices)
          (let ((actual   (slot-value (lookup-vertex (aref counter-ids v)) 'value))
                (expected (* 2 k)))
            (is (= expected actual)
                "Vertex ~D: expected ~D (2 threads × ~D increments); got ~D"
                v expected k actual)))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: view never exposes uncommitted data
;;;
;;; Every vertex ID returned by map-view must be present in the lhash (i.e.,
;;; lookup-vertex must return non-nil) and must have written-p = true.  This
;;; guards against the view index being updated before the lhash commit
;;; completes, which would briefly expose a vertex that can't be retrieved.
;;;
;;; 8 threads each do 10 write+read cycles: insert one vertex, then scan the
;;; view and verify every entry is retrievable.  The simultaneous writes and
;;; reads create the interleaving needed to expose the invariant.
;;; ---------------------------------------------------------------------------

(test view-never-exposes-uncommitted-data
  "Every vertex ID visible in the view must be present and written in the lhash."
  (with-conc-graph (g)
    (define-concurrency-views)
    (let ((violations 0)
          (vlock (make-rw-lock)))
      (run-threads 8
                   (lambda (i)
                     (declare (ignore i))
                     (dotimes (_ 10)
                       (with-transaction ()
                         (make-c-item :value (random 10000)))
                       ;; After each write, verify the view invariant.
                       ;; map-view calls fn with (key vertex-id value).
                       (map-view (lambda (key vid value)
                                   (declare (ignore key value))
                                   (let ((v (lookup-vertex vid)))
                                     (unless (and v (written-p v))
                                       (with-write-lock (vlock)
                                         (incf violations)))))
                                 'c-item 'c-item-by-value
                                 :graph g))))
      (is (= 0 violations)
          "~D view entries had no corresponding committed lhash entry"
          violations))))

;;; ---------------------------------------------------------------------------
;;; Test 3: concurrent allocator produces non-overlapping ranges
;;;
;;; 8 threads each allocate 200 blocks on the graph heap.  After all threads
;;; complete, every (offset, size) pair is sorted by offset and checked for
;;; overlap with the next pair.  A single allocation covers
;;; [offset, offset + header-size + data-size) on the mmap'd region.
;;; ---------------------------------------------------------------------------

(test concurrent-allocator-no-overlap
  "8 threads × 200 allocations on the graph heap must produce non-overlapping ranges."
  (with-conc-graph (g)
    (let* ((n-threads 8)
           (n-allocs  200)
           (results   (make-array (* n-threads n-allocs) :initial-element nil))
           (result-idx (make-array 1 :initial-element 0))
           (result-lock (make-rw-lock)))
      (run-threads n-threads
                   (lambda (i)
                     (declare (ignore i))
                     (dotimes (_ n-allocs)
                       (multiple-value-bind (offset data-size)
                           (allocate (heap g) 64)
                         (with-write-lock (result-lock)
                           (let ((idx (aref result-idx 0)))
                             (setf (aref results idx) (cons offset data-size))
                             (setf (aref result-idx 0) (1+ idx))))))))
      ;; Sort by offset, check no two ranges overlap
      (let* ((header-size 8)  ; +allocation-header-size+
             (pairs (remove nil (coerce results 'list)))
             (sorted (sort pairs #'< :key #'car)))
        (is (= (* n-threads n-allocs) (length pairs))
            "Expected ~D allocations; got ~D"
            (* n-threads n-allocs) (length pairs))
        (loop for (a b) on sorted
              while b
              do (let* ((a-end   (+ (car a) header-size (cdr a)))
                        (b-start (car b)))
                   (is (<= a-end b-start)
                       "Allocation overlap: [~D,~D) overlaps [~D,...)"
                       (car a) a-end b-start)))))))

(test unrecorded-scan-reads-stay-out-of-the-read-set
  "GH #92: MAP-VERTICES :record-reads nil keeps a scan's visits out of
the transaction's read-set -- the explicit opt-out of serializability
against the scanned corpus -- while the default still records them and
the transaction's own writes still validate."
  (with-conc-graph (g)
    (with-transaction ()
      (dotimes (k 10)
        (make-c-item :value k)))
    (with-transaction ()
      (map-vertices #'identity g :vertex-type 'c-item
                    :record-reads nil)
      (is (zerop (graph-db::object-set-count
                  (graph-db::read-set graph-db::*transaction*)))
          "an unrecorded scan joined the read-set")
      ;; the transaction still functions: it can write and commit
      (make-c-item :value 99))
    (with-transaction ()
      (map-vertices #'identity g :vertex-type 'c-item)
      (is (plusp (graph-db::object-set-count
                  (graph-db::read-set graph-db::*transaction*)))
          "the default no longer records scan reads"))))
