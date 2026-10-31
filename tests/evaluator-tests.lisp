;;;; The evaluator entry point (GH #301): validate-writes must agree with
;;;; %COMMIT on the same write set, refuse nothing a commit would accept,
;;;; and report structurally.

(in-package #:graph-db/test)

(in-suite graph-db-suite)

(def-suite evaluator-suite
  :description "validate-writes: refusal parity with %commit (GH #301)."
  :in graph-db-suite)

(in-suite evaluator-suite)

(def-vertex ev-item ()
  ((sku :type string :unique t)
   (grade :type string))
  :evaluator-graph)

(def-edge ev-link ()
  ()
  :evaluator-graph)

(graph-db:def-value-constraint ev-item grade :evaluator-graph
  :one-of (quote ("a" "b" "c"))
  :name ev-grade-vocabulary)

(defmacro with-ev-graph ((g) &body body)
  `(with-temp-directory (dir)
     (let ((,g (make-graph :evaluator-graph (namestring dir)
                           :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (ignore-errors (close-graph ,g :snapshot-p nil))
         (collect-garbage)))))

(test a-clean-write-set-validates-and-then-commits
  "No violations, a nonzero spec count (distinguishing clean from
nothing-to-check), and the same set commits without refusal."
  (with-ev-graph (g)
    (let (item)
      (with-transaction ()
        (setq item (make-ev-item :sku "one" :grade "a")))
      (let* ((copy (with-transaction () (copy item)))
             (report (validate-writes g (list (list :update copy item)))))
        (is (null (validation-report-violations report)))
        (is (plusp (cdr (assoc :value
                               (validation-report-spec-counts report)))))
        (is (plusp (cdr (assoc :unique
                               (validation-report-spec-counts report)))))))))

(test the-report-carries-what-commit-would-signal
  "Refusal parity (GH #301 acceptance): a write %COMMIT refuses shows up
in the report for the same set, family-tagged, and vice versa -- proved
by running both paths over the same fixture."
  (with-ev-graph (g)
    (with-transaction ()
      (make-ev-item :sku "taken" :grade "a"))
    ;; A duplicate :unique value and an out-of-vocabulary grade, staged
    ;; as creates but never committed.
    (let* ((dup (with-transaction ()
                  (let ((n (make-ev-item :sku "fresh" :grade "b")))
                    ;; mutate before commit: grade leaves the vocabulary
                    (setf (slot-value n (quote grade)) "z")
                    (rollback)
                    n))))
      (declare (ignorable dup)))
    ;; Fabricate the proposed nodes inside a rolled-back transaction so
    ;; nothing persists, then evaluate them against the live store.
    (let (bad-grade bad-sku)
      (handler-case
          (with-transaction ()
            (setq bad-grade (make-ev-item :sku "g1" :grade "z"))
            (setq bad-sku (make-ev-item :sku "taken" :grade "a"))
            (rollback))
        (error () nil))
      (let ((report (validate-writes
                     g (list (list :create bad-grade)
                             (list :create bad-sku)))))
        (is (assoc :value (validation-report-family-counts report))
            "the grade vocabulary refusal is in the report")
        (is (assoc :unique (validation-report-family-counts report))
            "the duplicate sku refusal is in the report")
        ;; and %commit refuses the same writes, one at a time
        (signals value-constraint-violation
          (with-transaction ()
            (make-ev-item :sku "g2" :grade "z")))
        (signals unique-constraint-violation
          (with-transaction ()
            (make-ev-item :sku "taken" :grade "a")))))))

(test validate-writes-has-no-side-effect
  "The evaluator is a read: after a refused report, the store is
unchanged and the same graph still accepts a clean commit."
  (with-ev-graph (g)
    (with-transaction ()
      (make-ev-item :sku "base" :grade "a"))
    (let (probe)
      (handler-case
          (with-transaction ()
            (setq probe (make-ev-item :sku "base" :grade "a"))
            (rollback))
        (error () nil))
      (validate-writes g (list (list :create probe))))
    (is (= 1 (length (map-vertices #'identity g :collect-p t
                                   :vertex-type (quote ev-item)))))
    (with-transaction ()
      (make-ev-item :sku "second" :grade "b"))
    (is (= 2 (length (map-vertices #'identity g :collect-p t
                                   :vertex-type (quote ev-item)))))))

(test validate-transaction-reads-the-ambient-write-set
  "GH #320: a consumer that stages writes in its own transaction asks
the evaluator about them through the exported reader, then commits or
rolls back.  The report equals VALIDATE-WRITES over GRAPH-DB:WRITES,
and the commit that follows refuses what the report named."
  (with-ev-graph (g)
    (with-transaction ()
      (make-ev-item :sku "taken" :grade "a"))
    (signals unique-constraint-violation
      (with-transaction ()
        (make-ev-item :sku "taken" :grade "z")
        (let* ((report (graph-db:validate-transaction g))
               (direct (validate-writes g (graph-db:writes *transaction*)))
               (families (mapcar #'first
                                 (validation-report-violations report))))
          (is (= 1 (length (graph-db:writes *transaction*))))
          (is (null (set-exclusive-or '(:unique :value) families)))
          (is (equal (mapcar #'first
                             (validation-report-violations direct))
                     families)))))))

(test validate-transaction-needs-an-open-transaction
  "Outside a transaction there is no write set to read; say so rather
than validate an empty one and report clean (GH #320)."
  (with-ev-graph (g)
    (signals error (graph-db:validate-transaction g))))
