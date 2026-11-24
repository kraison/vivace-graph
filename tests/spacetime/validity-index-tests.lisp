;;;; The claim-validity index (GH #499, design §1).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter %validity-slots
  '(graph-db.spacetime::subject-namespace
    graph-db.spacetime::relation
    graph-db.spacetime::transaction-extent-sexp
    graph-db.spacetime::extent-sexp
    graph-db.spacetime::subject-key)
  "The CLAIM-VALIDITY index's slots, in key order (GH #499).")

(defun %validity-entries (g class)
  "Every distinct CLAIM-VALIDITY prefix of CLASS in G, in index order."
  (let (out)
    (graph-db:map-index-prefixes
     (lambda (prefix) (push prefix out))
     g class %validity-slots :arity 5)
    (nreverse out)))

(defun %validity-lookup (g class tuple)
  "The nodes of CLASS in G whose full CLAIM-VALIDITY tuple is TUPLE."
  (graph-db:index-lookup g class %validity-slots tuple))

(defun %validity-open-claim (start &key (subject "r1"))
  "A TT-CLAIM whose validity begins at START and is open-ended."
  (make-tt-claim-binary
   :subject-namespace :region :subject-key subject
   :relation "in-state" :object-namespace :state :object-key "a"
   :producer "series" :standing :observed
   :extent (make-interval (exact-bound start) (unknown-bound))))

(defun %validity-close (claim end)
  "Close CLAIM's validity at END: copy, set the extent, save."
  (let* ((c (graph-db:copy claim))
         (start (bound-earliest (extent-start (claim-extent claim)))))
    (setf (claim-extent c) (exact-interval start end))
    (graph-db:save c)))

(defun %validity-current (subject)
  "The tuple of an ordinary stamped CT-CLAIM with no validity extent."
  (list :ns "r" graph-db.spacetime::+currency-current+ nil subject))

;;; --- the canonical values ------------------------------------------------

(test the-currency-key-reads-the-transaction-period
  (let* ((start (ts 2022 1 1))
         (open (extent->sexp (make-interval (exact-bound start)
                                            (unknown-bound)
                                            :semantics :transaction
                                            :standing :asserted)))
         (closed (extent->sexp
                  (make-interval (exact-bound start)
                                 (exact-bound (ts 2022 2 1))
                                 :semantics :transaction
                                 :standing :asserted))))
    (is (eq graph-db.spacetime::+currency-current+
            (graph-db.spacetime::extent-sexp-currency-key open)))
    (is (eq graph-db.spacetime::+currency-retracted+
            (graph-db.spacetime::extent-sexp-currency-key closed)))
    (is (eq graph-db.spacetime::+currency-current+
            (graph-db.spacetime::extent-sexp-currency-key
             graph-db.spacetime::+currency-current+)))
    (is (eq graph-db.spacetime::+currency-retracted+
            (graph-db.spacetime::extent-sexp-currency-key
             graph-db.spacetime::+currency-retracted+)))))

(test the-end-key-is-open-or-the-latest-edge
  (let ((start (ts 2022 1 1))
        (end (ts 2022 6 30 23 59 59)))
    ;; An unbounded end is open.
    (is (eq graph-db.spacetime::+validity-open+
            (graph-db.spacetime::extent-sexp-validity-end-key
             (extent->sexp (make-interval (exact-bound start)
                                          (unknown-bound))))))
    ;; An unknown end counts as open too.
    (is (eq graph-db.spacetime::+validity-open+
            (graph-db.spacetime::extent-sexp-validity-end-key
             (extent->sexp (make-interval (unknown-bound)
                                          (unknown-bound))))))
    ;; An exact end is the latest edge as (day sec nsec).
    (is (equal (graph-db.spacetime::%timestamp-key end)
               (graph-db.spacetime::extent-sexp-validity-end-key
                (extent->sexp (make-interval (exact-bound start)
                                             (exact-bound end))))))
    ;; Canonical values pass through unchanged.
    (is (eq graph-db.spacetime::+validity-open+
            (graph-db.spacetime::extent-sexp-validity-end-key
             graph-db.spacetime::+validity-open+)))
    (is (equal (graph-db.spacetime::%timestamp-key end)
               (graph-db.spacetime::extent-sexp-validity-end-key
                (graph-db.spacetime::%timestamp-key end))))
    (is (null (graph-db.spacetime::extent-sexp-validity-end-key nil)))))

(test the-end-key-orders-inside-a-second
  (let* ((t1 (ts 2022 1 1 0 0 0 0))
         (t2 (timestamp+ t1 1 :nsec))
         (k1 (graph-db.spacetime::extent-sexp-validity-end-key
              (extent->sexp (make-instant (exact-bound t1)))))
         (k2 (graph-db.spacetime::extent-sexp-validity-end-key
              (extent->sexp (make-instant (exact-bound t2)))))
         (id (make-array 16 :element-type '(unsigned-byte 8)))
         (tuple (lambda (k)
                  (list "ns" "rel"
                        graph-db.spacetime::+currency-current+ k "key" id))))
    (is (timestamp< t1 t2))
    (is-true (graph-db::%index-comp-lessp (funcall tuple k1)
                                          (funcall tuple k2)))
    (is-false (graph-db::%index-comp-lessp (funcall tuple k2)
                                           (funcall tuple k1)))))

(test open-sorts-before-every-closed-end
  (let ((id (make-array 16 :element-type '(unsigned-byte 8))))
    (flet ((tuple (end)
             (list "ns" "rel"
                   graph-db.spacetime::+currency-current+ end "k" id)))
      (dolist (end (list (graph-db.spacetime::%timestamp-key
                          (ts 1900 1 1))
                         (graph-db.spacetime::%timestamp-key
                          (ts 2022 1 1 0 0 0 1))
                         (graph-db.spacetime::%timestamp-key
                          (ts 9999 12 31 23 59 59))))
        (is-true (graph-db::%index-comp-lessp
                  (tuple graph-db.spacetime::+validity-open+)
                  (tuple end)))
        (is-false (graph-db::%index-comp-lessp
                   (tuple end)
                   (tuple graph-db.spacetime::+validity-open+)))))))

;;; --- the index the maintenance path writes -------------------------------

(test a-close-moves-the-entry
  (with-claim-graph (g)
    (let* ((start (ts 2022 1 1))
           (end (ts 2022 3 1))
           (claim (with-transaction () (%validity-open-claim start)))
           (open (list :region "in-state"
                       graph-db.spacetime::+currency-current+
                       graph-db.spacetime::+validity-open+ "r1")))
      (is (= 1 (length (%validity-lookup g 'tt-claim open))))
      (with-transaction () (%validity-close claim end))
      (is (null (%validity-lookup g 'tt-claim open)))
      (is (= 1 (length (%validity-lookup
                        g 'tt-claim
                        (list :region "in-state"
                              graph-db.spacetime::+currency-current+
                              (graph-db.spacetime::%timestamp-key end)
                              "r1"))))))))

(test a-retraction-moves-the-entry
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((claim (with-transaction () (make-u))))
      (is (= 1 (length (%validity-lookup g 'ct-claim
                                         (%validity-current "s1")))))
      (retract-claim claim)
      (is (null (%validity-lookup g 'ct-claim
                                  (%validity-current "s1"))))
      (is (= 1 (length (%validity-lookup
                        g 'ct-claim
                        (list :ns "r"
                              graph-db.spacetime::+currency-retracted+
                              nil "s1"))))))))

(test a-reassertion-returns-to-the-open-bucket
  (with-claim-graph (g)
    (let* ((claim (with-transaction ()
                    (%validity-open-claim (ts 2022 1 1))))
           (retracted (retract-claim claim))
           (retracted-tuple
             (list :region "in-state"
                   graph-db.spacetime::+currency-retracted+
                   graph-db.spacetime::+validity-open+ "r1"))
           (open-tuple (list :region "in-state"
                             graph-db.spacetime::+currency-current+
                             graph-db.spacetime::+validity-open+
                             "r1")))
      (is (= 1 (length (%validity-lookup g 'tt-claim retracted-tuple))))
      (reopen-claim retracted)
      (is (= 1 (length (%validity-lookup g 'tt-claim open-tuple))))
      (is (null (%validity-lookup g 'tt-claim retracted-tuple))))))

(test unstamped-and-extentless-claims-sit-under-the-null-component
  (with-claim-graph (g)
    (declare (ignorable g))
    ;; An ordinary claim is stamped and carries no validity extent.
    (with-transaction () (make-u :subject "s1"))
    (is (= 1 (length (%validity-lookup g 'ct-claim
                                       (%validity-current "s1")))))
    ;; Withdraw the stamp: a claim predating the axis.
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (with-legacy-stamps
        (with-transaction ()
          (let ((copy (graph-db:copy c)))
            (setf (claim-transaction-extent-sexp copy) nil)
            (graph-db:save copy)))))
    (is (= 1 (length (%validity-lookup
                      g 'ct-claim
                      (list :ns "r" nil nil "s1")))))))

(test the-rebuilt-index-equals-the-maintained-one
  (with-claim-graph (g)
    (let ((open (with-transaction ()
                  (%validity-open-claim (ts 2022 1 1) :subject "r1")))
          (closed (with-transaction ()
                    (%validity-open-claim (ts 2022 2 1) :subject "r2"))))
      (with-transaction () (%validity-close closed (ts 2022 3 1)))
      (retract-claim open)
      (let ((before (%validity-entries g 'tt-claim)))
        (is-true before)
        (graph-db:regenerate-secondary-indexes g)
        (is (equal before (%validity-entries g 'tt-claim)))))))

;;; --- persistence ---------------------------------------------------------

(test the-index-survives-a-reopen
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (before nil))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (%validity-open-claim (ts 2022 1 1) :subject "r1"))
               (let ((closed (with-transaction ()
                               (%validity-open-claim (ts 2022 2 1)
                                                     :subject "r2"))))
                 (with-transaction ()
                   (%validity-close closed (ts 2022 3 1))))
               (setf before (%validity-entries g 'tt-claim)))
          (close-graph g)))
      (is-true before)
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (is (equal before (%validity-entries g2 'tt-claim))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
