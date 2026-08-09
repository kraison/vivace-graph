;;;; CONCURRENT-UNIQUE-STORM-SUITE
;;;;
;;;; Concurrent :UNIQUE-constraint stress: many threads racing for the SAME keys
;;;; must yield exactly one holder per key (the commit-boundary check defeats the
;;;; phantom under real concurrency + load), with no corruption.

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-unique-storm-suite
  :description "Concurrent :unique-constraint contention storm."
  :in concurrent-stress-suite)

(in-suite concurrent-unique-storm-suite)

;;; ---------------------------------------------------------------------------
;;; Every thread tries to claim the same K unique keys.  Only one insert per key
;;; may win; the rest abort with UNIQUE-CONSTRAINT-VIOLATION.  After all join,
;;; exactly K live nodes exist -- no double-claim, no lost/corrupt index.
;;; ---------------------------------------------------------------------------

(test unique-storm-contended-keys
  "T threads each race for the same K :unique keys; exactly one node wins per key."
  (let* ((t-count (min *stress-thread-count* 8))
         (k       200))
    (with-cstress-graph (g)
      (let ((start (get-internal-real-time)))
        (run-threads t-count
                     (lambda (i)
                       (declare (ignore i))
                       (dotimes (j k)
                         ;; losers signal UNIQUE-CONSTRAINT-VIOLATION -- expected;
                         ;; any OTHER error propagates and fails the test.
                         (handler-case
                             (with-transaction () (make-cu-item :ukey j :label "us"))
                           (unique-constraint-violation () nil)))))
        (record-throughput "unique-storm-attempts" (* t-count k)
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (let ((live (length (map-vertices #'identity g :collect-p t
                                        :vertex-type 'cu-item))))
        (is (= k live)
            "expected exactly ~D live cu-items (one per contended key); got ~D"
            k live)))))

;;; ---------------------------------------------------------------------------
;;; Disjoint keys: every thread inserts its OWN key range (no collisions), so all
;;; commits succeed -- exercises the maintenance path under write concurrency.
;;; ---------------------------------------------------------------------------

(test unique-storm-disjoint-keys
  "T threads each insert M disjoint :unique keys; all commit; total = T*M."
  (let* ((t-count (min *stress-thread-count* 8))
         (m       150))
    (with-cstress-graph (g)
      (run-threads t-count
                   (lambda (i)
                     (dotimes (j m)
                       (with-transaction ()
                         (make-cu-item :ukey (+ (* i m) j) :label "ud")))))
      (is (= (* t-count m)
             (length (map-vertices #'identity g :collect-p t :vertex-type 'cu-item)))
          "expected ~D live cu-items" (* t-count m)))))
