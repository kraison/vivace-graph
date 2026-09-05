;;;; The epoch axis for claim reads (GH #347): a version's commit
;;;; epoch, and :AS-OF-EPOCH on the two readers.  Facts in
;;;; docs/superpowers/notes/2026-09-05-epoch-axis-engine-api-facts.md.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *ep-a-name* :graph-db-ep-a)
(defparameter *ep-b-name* :graph-db-ep-b)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ep-a-name* graph-db::*schema-node-metadata*) nil)
  (setf (gethash *ep-b-name* graph-db::*schema-node-metadata*) nil))

;; Two stores need two families: DEF-CLAIM-CLASSES binds node types to
;; one graph name and class names are global (#347 recon F).
(def-claim-classes ea-claim :graph-db-ep-a)
(def-claim-classes eb-claim :graph-db-ep-b)
(def-claim-classes ek-claim :graph-db-ep-a :keep-revisions 1)

(defmacro with-clocked-stores ((a b) &body body)
  "Two fresh stores A and B on ONE system clock, all in scratch dirs.
The attach is asserted inside the fixture: a store that silently failed
to attach would let every epoch test pass for the wrong reason."
  (let ((cdir (gensym "CDIR")) (da (gensym "DA")) (db (gensym "DB"))
        (clock (gensym "CLOCK")))
    `(with-temp-directory (,cdir)
       (with-temp-directory (,da)
         (with-temp-directory (,db)
           (let ((,clock (graph-db:open-system-clock (namestring ,cdir))))
             (unwind-protect
                  (let ((,a (make-graph *ep-a-name* (namestring ,da)
                                        :buffer-pool-size 1000
                                        :system-clock ,clock))
                        (,b (make-graph *ep-b-name* (namestring ,db)
                                        :buffer-pool-size 1000
                                        :system-clock ,clock)))
                    (unwind-protect
                         (progn
                           (is (eq (graph-db:graph-system-clock ,a)
                                   (graph-db:graph-system-clock ,b))
                               "fixture: both stores on one clock")
                           ,@body)
                      (ignore-errors (close-graph ,a))
                      (ignore-errors (close-graph ,b))
                      (collect-garbage)))
               (graph-db:close-system-clock ,clock))))))))

(defun %tx (graph thunk)
  "Run THUNK in a transaction on GRAPH; return the committed epoch --
the transaction's id, readable after the commit (#347 recon E2)."
  (graph-db::transaction-id
   (with-transaction (:graph graph)
     (funcall thunk)
     graph-db:*transaction*)))

(defun %unary (graph maker key &key extent)
  "Make one unary claim with MAKER in GRAPH on (:region KEY)."
  (apply maker :graph graph
               :subject-namespace :region :subject-key key
               :relation "verified" :producer "audit" :standing :observed
               (and extent (list :extent extent))))

(defun %one (graph family key)
  "The live version of the one claim of FAMILY on (:region KEY)."
  (first (claims-touching graph family :region key :role :subject)))

(test epochs-form-one-sequence-across-two-stores
  "#347 part 1 (recon C1, E2): under one clock the commits A, B, A get
distinct increasing ids, and each claim's CLAIM-COMMIT-EPOCH read back
from the store is its own transaction's id."
  (with-clocked-stores (a b)
    (let ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
          (e2 (%tx b (lambda () (%unary b #'make-eb-claim-unary "r1"))))
          (e3 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r2")))))
      (is (< e1 e2 e3))
      (is (= e1 (claim-commit-epoch (%one a 'ea-claim "r1"))))
      (is (= e2 (claim-commit-epoch (%one b 'eb-claim "r1"))))
      (is (= e3 (claim-commit-epoch (%one a 'ea-claim "r2")))))))

(test claim-commit-epoch-is-nil-for-a-reaped-claim
  "Part 1's reader must survive the REAPED-CLAIM structs :AS-OF mixes
into a result list."
  (with-clocked-stores (a b)
    (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1")))
    (let ((c (%one a 'ea-claim "r1")))
      (is (integerp (claim-commit-epoch c)) "control: a live version")
      (is (null (claim-commit-epoch
                 (graph-db.spacetime::%make-reaped-claim (id c))))))))

(test claim-commit-epoch-reads-on-a-clockless-store
  "A store with no system clock still stamps versions from its own
counter, so part 1's reader answers an integer there; only the
:AS-OF-EPOCH reader refuses (recon E9).  *SYSTEM-CLOCK* is bound
explicitly so the premise does not depend on run order."
  (let ((graph-db:*system-clock* nil))
    (with-temp-directory (dir)
      (let ((g (make-graph *ep-a-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (progn
               (is (null (graph-db:graph-system-clock g))
                   "control: no clock")
               (%tx g (lambda () (%unary g #'make-ea-claim-unary "r1")))
               (is (integerp (claim-commit-epoch (%one g 'ea-claim "r1")))))
          (ignore-errors (close-graph g))
          (collect-garbage))))))

(test as-of-epoch-selects-the-version-committed-at-or-before
  "#347 part 2 (recon C2): create at E1, commit in B, update the extent
at E2.  :AS-OF-EPOCH E1 -> old extent; E2 -> new; EB, B's epoch between
them, -> old.  The E2 case fails against a strict < comparison, the one
the engine's own snapshot predicate uses."
  (with-clocked-stores (a b)
    (let* ((old (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
           (new (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
           (e1 (%tx a (lambda ()
                        (%unary a #'make-ea-claim-unary "r1" :extent old))))
           (eb (%tx b (lambda () (%unary b #'make-eb-claim-unary "x"))))
           (e2 (%tx a (lambda ()
                        (let ((k (graph-db:copy (%one a 'ea-claim "r1"))))
                          (setf (claim-extent k) new)
                          (graph-db:save k))))))
      (is (< e1 eb e2) "control: B's commit sits between E1 and E2")
      (flet ((at (e)
               (claims-touching a 'ea-claim :region "r1" :role :subject
                                :as-of-epoch e)))
        (is (= 1 (length (at e1))))
        (is (extent-equals-p old (claim-extent (first (at e1)))))
        (is (extent-equals-p new (claim-extent (first (at e2)))))
        (is (extent-equals-p old (claim-extent (first (at eb)))))
        (is (null (at (1- e1))) "not yet created one epoch earlier")))))

(test as-of-epoch-drops-a-claim-retracted-at-or-before
  "#347 part 2 (recon E4): a retraction is a version whose epoch is the
retracting transaction's id.  Create at E1, retract at E2: E1 and EB
(B's epoch between them) return the claim, E2 returns NIL, and the
live version's CLAIM-COMMIT-EPOCH is E2."
  (with-clocked-stores (a b)
    (let* ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
           (eb (%tx b (lambda () (%unary b #'make-eb-claim-unary "x"))))
           (e2 (%tx a (lambda () (retract-claim (%one a 'ea-claim "r1"))))))
      (is (< e1 eb e2) "control: B's commit sits between E1 and E2")
      (flet ((at (e)
               (claims-touching a 'ea-claim :region "r1" :role :subject
                                :as-of-epoch e)))
        (is (= 1 (length (at e1))))
        (is (claim-current-p (first (at e1))))
        (is (= 1 (length (at eb))))
        (is (null (at e2)))
        (is (= e2 (claim-commit-epoch (%one a 'ea-claim "r1"))))))))

(test as-of-epoch-composes-with-current-and-at
  "The downstream filters already guard on REAPED-CLAIM-P, so :CURRENT
and :AT apply to the RESOLVED version exactly as they do under :AS-OF."
  (with-clocked-stores (a b)
    (let* ((old (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
           (new (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
           (e1 (%tx a (lambda ()
                        (%unary a #'make-ea-claim-unary "r1" :extent old))))
           (e2 (%tx a (lambda ()
                        (let ((k (graph-db:copy (%one a 'ea-claim "r1"))))
                          (setf (claim-extent k) new)
                          (graph-db:save k))))))
      (is (null (claims-touching a 'ea-claim :region "r1" :role :subject
                                 :as-of-epoch e1 :at (ts 2022 5 1)))
          "May is outside the E1 version's validity")
      (is (= 1 (length (claims-touching a 'ea-claim :region "r1"
                                        :role :subject
                                        :as-of-epoch e2 :at (ts 2022 5 1)
                                        :current t)))))))

(test as-of-epoch-tells-reaped-from-created-after
  "#347 recon C4: with :KEEP-REVISIONS 1, the E1 version of a claim
updated twice is reaped -> REAPED-CLAIM; a claim created AFTER E1 in
the same store -> NIL.  Both exhaust the chain identically; only the
oldest retained REVISION tells them apart, so the second case is the
non-vacuity control for the first."
  (with-clocked-stores (a b)
    (let ((e1 (%tx a (lambda () (%unary a #'make-ek-claim-unary "kr")))))
      (dotimes (i 2)
        (%tx a (lambda ()
                 (let ((k (graph-db:copy (%one a 'ek-claim "kr"))))
                   (setf (claim-confidence k) (* 0.1 (1+ i)))
                   (graph-db:save k)))))
      (%tx a (lambda () (%unary a #'make-ek-claim-unary "late")))
      (let ((then (claims-touching a 'ek-claim :region "kr" :role :subject
                                   :as-of-epoch e1)))
        (is (= 1 (length then)))
        (is (reaped-claim-p (first then))
            "the E1 version is past the window: reaped, not substituted")
        (is (equalp (id (%one a 'ek-claim "kr"))
                    (reaped-claim-id (first then)))))
      (is (null (claims-touching a 'ek-claim :region "late" :role :subject
                                 :as-of-epoch e1))
          "created after E1: absent, not reaped")
      (let ((by (claims-by-producer a 'ek-claim "audit" :as-of-epoch e1)))
        (is (= 1 (length by)) "by producer: the reaped one, not the late one")
        (is (reaped-claim-p (first by)))))))

(test as-of-epoch-refuses-a-clockless-store-but-as-of-still-answers
  "#347 recon E9: a store with no system clock draws epochs from its own
counter, so :AS-OF-EPOCH refuses with EPOCH-AXIS-UNAVAILABLE naming the
graph -- a QUERY-PRECONDITION-ERROR whose reason prints -- while :AS-OF
on the same store keeps answering.  *SYSTEM-CLOCK* is bound explicitly
so the premise does not depend on run order."
  (let ((graph-db:*system-clock* nil))
    (with-temp-directory (dir)
      (let ((g (make-graph *ep-a-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((t0 (graph-db.spacetime::%st-now)))
               (is (null (graph-db:graph-system-clock g))
                   "control: no clock")
               (sleep 0.01)
               (%tx g (lambda () (%unary g #'make-ea-claim-unary "r1")))
               (signals epoch-axis-unavailable
                 (claims-touching g 'ea-claim :region "r1" :role :subject
                                  :as-of-epoch 1))
               (signals epoch-axis-unavailable
                 (claims-by-producer g 'ea-claim "audit" :as-of-epoch 1))
               (handler-case
                   (claims-touching g 'ea-claim :region "r1"
                                    :role :subject :as-of-epoch 1)
                 (epoch-axis-unavailable (c)
                   (is (typep c 'graph-db:query-precondition-error))
                   (is (eq *ep-a-name*
                           (epoch-axis-unavailable-graph-name c)))
                   (is (search "no system clock"
                               (graph-db:query-precondition-error-reason
                                c)))))
               (is (null (claims-touching g 'ea-claim :region "r1"
                                          :role :subject :as-of t0))
                   "wall clock still answers: not yet created at T0")
               (is (= 1 (length (claims-touching
                                 g 'ea-claim :region "r1" :role :subject
                                 :as-of (graph-db.spacetime::%st-now))))))
          (ignore-errors (close-graph g))
          (collect-garbage))))))

(test as-of-and-as-of-epoch-are-exclusive
  "Passing both axes signals rather than silently preferring one, on
both readers."
  (with-clocked-stores (a b)
    (let ((now (graph-db.spacetime::%st-now)))
      (signals simple-error
        (claims-touching a 'ea-claim :region "r1" :role :subject
                         :as-of now :as-of-epoch 1))
      (signals simple-error
        (claims-by-producer a 'ea-claim "audit" :as-of now :as-of-epoch 1))
      (is (null (claims-touching a 'ea-claim :region "r1" :role :subject
                                 :as-of-epoch 1))
          "control: one axis alone is accepted"))))

(test claims-by-producer-as-of-epoch-unwinds-an-update
  "The producer index takes the same resolver: E1 answers the old
extent, E2 the new one."
  (with-clocked-stores (a b)
    (let* ((old (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
           (new (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
           (e1 (%tx a (lambda ()
                        (%unary a #'make-ea-claim-unary "r1" :extent old))))
           (e2 (%tx a (lambda ()
                        (let ((k (graph-db:copy (%one a 'ea-claim "r1"))))
                          (setf (claim-extent k) new)
                          (graph-db:save k))))))
      (is (extent-equals-p
           old (claim-extent
                (first (claims-by-producer a 'ea-claim "audit"
                                           :as-of-epoch e1)))))
      (is (extent-equals-p
           new (claim-extent
                (first (claims-by-producer a 'ea-claim "audit"
                                           :as-of-epoch e2))))))))
