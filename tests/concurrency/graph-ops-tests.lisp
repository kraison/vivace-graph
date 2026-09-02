;;;; CONCURRENT-GRAPH-OPS-SUITE
;;;;
;;;; Tests concurrent access to the graph storage layer:
;;;;   - ve-index (edge adjacency) lock-vector under concurrent inserts
;;;;   - type-index lock-vector under concurrent inserts + scans
;;;;   - interleaved inserts and deletions

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-graph-ops-suite
  :description "Graph storage-layer correctness under concurrent load."
  :in concurrency-suite)

(in-suite concurrent-graph-ops-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: concurrent edge inserts — all edges must appear in the ve-index
;;; ---------------------------------------------------------------------------

(test concurrent-edge-inserts
  "N threads each insert M edges; all N×M must be present in the ve-index."
  (let* ((n  *thread-count*)
         (m  20))
    (with-conc-graph (g)
      ;; Pre-create N vertices; thread i links vertex[i] → vertex[(i+1)%n].
      (let ((vids (make-array n)))
        (with-transaction ()
          (dotimes (i n)
            (setf (aref vids i) (id (make-c-item :value i)))))
        (run-threads n
                     (lambda (i)
                       (let ((fid (aref vids i))
                             (tid (aref vids (mod (1+ i) n))))
                         (dotimes (_ m)
                           (with-transaction ()
                             (make-c-link
                              :from (lookup-vertex fid)
                              :to   (lookup-vertex tid)))))))
        (is (= (* n m)
               (length (map-edges #'identity g
                                  :collect-p t
                                  :edge-type 'c-link)))
            "Expected ~D edges; found ~D" (* n m)
            (length (map-edges #'identity g
                               :collect-p t
                               :edge-type 'c-link)))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: concurrent type-index access (writers + readers)
;;; ---------------------------------------------------------------------------

(test concurrent-type-index-access
  "N writers inserting and N readers scanning the type-index must not error;
final count must equal N×M."
  (let* ((n  *thread-count*)
         (m  25)
         (r  50))   ; read iterations per reader thread
    (with-conc-graph (g)
      (run-threads (* 2 n)
                   (lambda (i)
                     (if (< i n)
                         ;; Writer
                         (dotimes (_ m)
                           (with-transaction ()
                             (make-c-item :value (random 100000))))
                         ;; Reader — count must be a non-negative integer; errors abort
                         (dotimes (_ r)
                           (let ((cnt (length (map-vertices #'identity g
                                                            :collect-p t
                                                            :vertex-type 'c-item))))
                             (declare (ignore cnt)))))))
      (is (= (* n m)
             (length (map-vertices #'identity g
                                   :collect-p t
                                   :vertex-type 'c-item)))
          "Expected ~D items after concurrent inserts; got ~D"
          (* n m)
          (length (map-vertices #'identity g
                                :collect-p t
                                :vertex-type 'c-item))))))

;;; ---------------------------------------------------------------------------
;;; Test 3: interleaved inserts and deletions
;;;
;;; Half the threads insert only; the other half insert and immediately delete.
;;; Invariant: final live count = (n/2) × M.
;;; ---------------------------------------------------------------------------

(test concurrent-delete-and-insert
  "Interleaved inserts and deletions: live count must equal inserters × M."
  (let* ((n         *thread-count*)
         (inserters (floor n 2))
         (deleters  (- n inserters))
         (m         20))
    (with-conc-graph (g)
      (run-threads n
                   (lambda (i)
                     (if (< i inserters)
                         ;; Pure inserter
                         (dotimes (_ m)
                           (with-transaction ()
                             (make-c-item :value i)))
                         ;; Insert-then-delete
                         (dotimes (_ m)
                           (let (vid)
                             (with-transaction ()
                               (setq vid (id (make-c-item :value (+ 10000 i)))))
                             (with-transaction ()
                               (mark-deleted (lookup-vertex vid))))))))
      (let ((live (length (map-vertices #'identity g
                                        :collect-p t
                                        :vertex-type 'c-item))))
        (declare (ignore deleters))
        (is (= (* inserters m) live)
            "Expected ~D live vertices; found ~D" (* inserters m) live)))))

;;; ---------------------------------------------------------------------------
;;; Test 4: concurrent vertex deletion during map-vertices
;;;
;;; N deleter threads each mark-deleted a distinct range of pre-created
;;; vertices while N scanner threads call map-vertices in a loop.  The test
;;; verifies that the scanner never crashes — the key risk is a scanner
;;; dereferencing an in-progress deletion.
;;; ---------------------------------------------------------------------------

(test concurrent-vertex-deletion-during-map
  "Threads deleting vertices concurrently with map-vertices must not crash."
  (let* ((n (floor *thread-count* 2))
         (m 30))
    (with-conc-graph (g)
      (let ((ids (make-array (* n m))))
        (with-transaction ()
          (dotimes (i (* n m))
            (setf (aref ids i) (id (make-c-item :value i)))))
        (run-threads (* 2 n)
                     (lambda (i)
                       (if (< i n)
                           ;; Deleter: owns a non-overlapping range of vertices.
                           (let ((base (* i m)))
                             (dotimes (j m)
                               (with-transaction ()
                                 (mark-deleted
                                  (lookup-vertex (aref ids (+ base j)))))))
                           ;; Scanner: must not crash or error.
                           (dotimes (_ 20)
                             (map-vertices #'identity g
                                           :collect-p nil
                                           :vertex-type 'c-item)))))
        (pass)))))

;;; ---------------------------------------------------------------------------
;;; Test 5: concurrent mixed workload
;;;
;;; Four roles run simultaneously: inserters, deleters, Prolog query threads,
;;; and view readers.  The test is a smoke test for the whole stack under
;;; realistic concurrent access — if any thread errors or deadlocks the test
;;; fails.
;;; ---------------------------------------------------------------------------

(test concurrent-mixed-workload
  "Inserters, deleters, query threads, and view readers all active
simultaneously must complete without errors."
  (let* ((n *thread-count*)
         (k 10))
    (with-conc-graph (g)
      (define-concurrency-views)
      ;; Pre-populate so deleters and readers have work from the start.
      (with-transaction ()
        (dotimes (i (* n k))
          (make-c-item :value i)))
      (run-threads n
                   (lambda (i)
                     (dotimes (_ k)
                       (case (mod i 4)
                         (0  ; inserter
                          (with-transaction ()
                            (make-c-item :value (random 1000000))))
                         (1  ; deleter
                          (let ((vs (map-vertices #'identity g
                                                  :collect-p t
                                                  :vertex-type 'c-item)))
                            (when vs
                              (with-transaction ()
                                (mark-deleted (first vs))))))
                         (2  ; Prolog query
                          (select-flat (?x) (is-a ?x c-item)))
                         (3  ; view reader
                          (invoke-graph-view 'c-item 'c-item-by-value
                                             :graph g))))))
      (pass))))

(test concurrent-uuid-generation-is-collision-free
  "GEN-V8-UUID under concurrency must never mint the same id twice: RANDOM
on a shared RANDOM-STATE races, and a duplicate node id silently swallows
a node (GH #298 -- 590 duplicates in 200k concurrent draws before the
id lock).  10 x 20,000 draws reproduced it reliably; zero is the spec."
  (let* ((n 10) (m 20000)
         (results (make-array n)))
    (run-threads n
                 (lambda (i)
                   (let ((v (make-array m)))
                     (dotimes (k m)
                       (setf (aref v k) (graph-db::gen-v8-uuid 1)))
                     (setf (aref results i) v))))
    (let ((seen (make-hash-table :test 'equalp)) (dups 0))
      (loop for v across results do
        (loop for id across v do
          (if (gethash id seen) (incf dups) (setf (gethash id seen) t))))
      (is (zerop dups) "~D duplicate uuids in ~D concurrent draws"
          dups (* n m)))))
