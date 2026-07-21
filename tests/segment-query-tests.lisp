;;;; Tests for the vector-segment query layer (segment-scan, score-subset,
;;;; vector-search, concurrency).

(in-package #:graph-db/test)

(def-suite segment-query-suite
  :description "segment-scan / segment-score-subset / vector-search."
  :in graph-db-suite)

(in-suite segment-query-suite)

(defun %qpath ()
  (format nil "/var/tmp/vgquery-~a.dat" (get-internal-real-time)))

(defun %qvec (dim &rest floats)
  "A DIM-long single-float vector from FLOATS, zero-padded."
  (let ((v (make-array dim :element-type 'single-float :initial-element 0.0)))
    (loop for f in floats for i from 0 do (setf (aref v i) (coerce f 'single-float)))
    v))

(defun %qid (n)
  (let ((v (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref v 0) n)
    v))

(defun %brute-force (pairs query)
  "Reference ranking: (score . id) best first, full cosine, id-asc tiebreak."
  (let ((scored (mapcar (lambda (p)
                          (cons (graph-db::%cosine query (car p)) (cdr p)))
                        pairs)))
    (sort scored (lambda (a b) (graph-db::%score-before-p (car a) (cdr a)
                                                          (car b) (cdr b))))))

(test scan-matches-brute-force
  "segment-scan's top-k equals a brute-force full-cosine ranking."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8))
               (pairs '()))
           (unwind-protect
                (progn
                  (loop for n from 1 to 6
                        for v = (%qvec 4 (coerce n 'single-float) 1.0 0.0 0.0)
                        do (segment-put s (%qid n) v)
                           (push (cons v (%qid n)) pairs))
                  (let* ((q (%qvec 4 1.0 1.0 0.0 0.0))
                         (got (segment-scan s q 3))
                         (want (subseq (%brute-force (nreverse pairs) q) 0 3)))
                    (is (= 3 (length got)) "expected 3 hits, got ~S" got)
                    (loop for g in got for w in want
                          do (is (equalp (cdr g) (cdr w)) "id order differs")
                             (is (< (abs (- (car g) (car w))) 1e-5)
                                 "score differs: ~A vs ~A" (car g) (car w)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-k-larger-than-occupancy
  "k greater than the number of stored vectors returns them all, no padding.
Asserts hand-computed cosine values (not the %cosine-under-test oracle): with
query (1,0,0,0), the id-1 vector (1,0,0,0) is an exact direction match
(score 1.0) and the id-2 vector (0,1,0,0) is orthogonal (score 0.0) -- values
a bare dot product would ALSO get right here, but the explicit id order and
tight tolerance still pin the ranking and the (score . id) shape."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 0.0 1.0 0.0 0.0))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 10)))
                    (is (= 2 (length got)) "expected 2 hits, got ~S" got)
                    (when (= 2 (length got))
                      (destructuring-bind ((s1 . i1) (s2 . i2)) got
                        (is (equalp (%qid 1) i1) "expected id 1 ranked first")
                        (is (< (abs (- s1 1.0)) 1e-6)
                            "expected top score 1.0, got ~A" s1)
                        (is (equalp (%qid 2) i2) "expected id 2 ranked second")
                        (is (< (abs s2) 1e-6)
                            "expected second score ~~0.0, got ~A" s2)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-cosine-not-dot-product
  "Pins full cosine, not a bare dot product: a NON-unit stored vector (2,0,0,0)
against query (1,0,0,0) must score 1.0 (cosine of parallel vectors) -- a bare
dot product would score 2.0.  This is the assertion a bare-dot mutation of
%cosine cannot pass."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 2.0 0.0 0.0 0.0))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 1)))
                    (is (= 1 (length got)) "expected 1 hit, got ~S" got)
                    (when (= 1 (length got))
                      (is (< (abs (- (car (first got)) 1.0)) 1e-6)
                          "expected cosine 1.0 for a non-unit vector, got ~A -- looks like a bare dot product"
                          (car (first got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-skips-removed-slots
  "A removed id does not appear in scan results.  Before removal, id 1 and
id 2 store IDENTICAL vectors, so their scores tie exactly -- scanning with
k=1 first pins the score-DESC/id-ASC tiebreak (id 1 must win), then removal
is exercised and only id 2 remains."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 1.0 0.0 0.0 0.0))
                  (let ((tie (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 1)))
                    (is (= 1 (length tie)) "expected 1 hit, got ~S" tie)
                    (when (= 1 (length tie))
                      (is (equalp (%qid 1) (cdr (first tie)))
                          "expected id 1 to win the score tie (id-ascending tiebreak), got ~A"
                          (cdr (first tie)))))
                  (segment-remove s (%qid 1))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 10)))
                    (is (= 1 (length got)) "expected 1 hit, got ~S" got)
                    (when (= 1 (length got))
                      (is (equalp (%qid 2) (cdr (first got)))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-rejects-wrong-length-query
  "A query vector whose length doesn't match the segment's dimension is
rejected loudly, mirroring segment-put's write-side check -- not silently
scored against a prefix (or, worse, silently scored as all-zero when the
query happens to be zero only within the segment's dimension and nonzero
beyond it, which used to slip past the zero-norm check entirely)."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (signals error (segment-scan s (%qvec 3 1.0 0.0 0.0) 1))
                  (signals error
                      (segment-scan s (%qvec 8 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0) 1)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-across-growth
  "segment-scan sweeps the CURRENT capacity after multiple growths and returns
the correct full ranking.  Growth is the riskiest interaction for the scan: it
relocates the whole vector block (the sweep must read the NEW capacity) and
free-marks [old-cap, new-cap) (the sweep must skip a large sparse upper
range).  Vector n (1-indexed) is (1, n-1, 0, 0) against query (1,0,0,0): its
cosine is 1/sqrt(1+(n-1)^2), which is STRICTLY DECREASING as n increases (the
denominator strictly grows) -- so there are no ties, and the expected
best-first order is simply ids 1..12, verifiable by inspection without calling
%cosine.  :initial-capacity 4 with 12 puts forces %seg-grow twice (4 -> 8 ->
16)."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (loop for n from 1 to 12
                        do (segment-put s (%qid n)
                                        (%qvec 4 1.0 (coerce (1- n) 'single-float) 0.0 0.0)))
                  (is (= 16 (segment-capacity s))
                      "expected capacity to have grown to 16 after 12 puts from an initial 4, got ~D"
                      (segment-capacity s))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 12)))
                    (is (= 12 (length got)) "expected all 12 hits, got ~D" (length got))
                    (when (= 12 (length got))
                      (loop for n from 1 to 12
                            for pair in got
                            do (is (equalp (%qid n) (cdr pair))
                                   "expected id ~D at rank ~D, got a different id" n n)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-empty-and-zero-k
  "An empty segment and k=0 both return NIL, not an error."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (is (null (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 5)))
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (is (null (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 0))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-zero-norm-scores-zero
  "A zero-norm query returns nothing; a zero-norm stored vector scores 0.0
rather than signalling a divide error."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 0.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 1.0 0.0 0.0 0.0))
                  ;; zero query -> empty
                  (is (null (segment-scan s (%qvec 4 0.0 0.0 0.0 0.0) 5)))
                  ;; zero stored vector scores 0.0 and ranks last
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 5)))
                    (is (= 2 (length got)))
                    (is (equalp (%qid 2) (cdr (first got))))
                    (is (< (abs (car (second got))) 1e-6)
                        "zero-norm vector should score 0.0, got ~A" (car (second got)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-agrees-with-scan
  "Scoring a candidate set gives the same scores and order as a full scan
restricted to those ids.  This is the ANN seam: a future index proposes
candidates and this scores them exactly."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (loop for n from 1 to 5
                        do (segment-put s (%qid n)
                                        (%qvec 4 (coerce n 'single-float) 1.0 0.0 0.0)))
                  (let* ((q (%qvec 4 1.0 1.0 0.0 0.0))
                         (subset (list (%qid 2) (%qid 4)))
                         (got (segment-score-subset s q subset))
                         (full (segment-scan s q 10)))
                    (is (= 2 (length got)) "expected 2 scored, got ~S" got)
                    ;; every subset result must match that id's score in the full scan
                    (dolist (pair got)
                      (let ((from-scan (find (cdr pair) full :key #'cdr :test #'equalp)))
                        (is (not (null from-scan)) "id missing from full scan")
                        (is (< (abs (- (car pair) (car from-scan))) 1e-6)
                            "score differs from scan: ~A vs ~A"
                            (car pair) (car from-scan))))
                    ;; and they must be in best-first order
                    (is (>= (car (first got)) (car (second got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-skips-unknown-ids
  "Ids absent from the segment are skipped, not errors."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (let ((got (segment-score-subset s (%qvec 4 1.0 0.0 0.0 0.0)
                                                   (list (%qid 1) (%qid 99)))))
                    (is (= 1 (length got)))
                    (is (equalp (%qid 1) (cdr (first got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-empty-inputs
  "An empty id list, and a zero-norm query, both return NIL."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (is (null (segment-score-subset s (%qvec 4 1.0 0.0 0.0 0.0) '())))
                  (is (null (segment-score-subset s (%qvec 4 0.0 0.0 0.0 0.0)
                                                  (list (%qid 1))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-hand-computed-non-unit-vector
  "Reference score is computed BY HAND, not via %cosine, so a bare
dot-product mutation is caught even though segment-scan uses the same
scoring path.  Stored vector (2,0,0,0) against query (1,0,0,0): true cosine
of two parallel vectors is 1.0 regardless of magnitude, but a bare dot
product would give 2.0."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 2.0 0.0 0.0 0.0))
                  (let ((got (segment-score-subset s (%qvec 4 1.0 0.0 0.0 0.0)
                                                   (list (%qid 1)))))
                    (is (= 1 (length got)) "expected 1 scored, got ~S" got)
                    (when (= 1 (length got))
                      (is (< (abs (- (car (first got)) 1.0)) 1e-6)
                          "expected cosine 1.0 for parallel vectors, got ~A (a bare dot product would give 2.0)"
                          (car (first got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-tiebreak-orders-by-ascending-id
  "Equal-score candidates are ordered by ascending node-id, matching
%score-before-p's tiebreak -- mirrors segment-scan's tiebreak test but goes
through the subset path."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  ;; three identical vectors -> identical scores -> id-ascending order
                  (segment-put s (%qid 3) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 1.0 0.0 0.0 0.0))
                  (let ((got (segment-score-subset s (%qvec 4 1.0 0.0 0.0 0.0)
                                                   (list (%qid 3) (%qid 1) (%qid 2)))))
                    (is (= 3 (length got)) "expected 3 scored, got ~S" got)
                    (when (= 3 (length got))
                      (loop for n from 1 to 3
                            for pair in got
                            do (is (equalp (%qid n) (cdr pair))
                                   "expected id ~D at rank ~D under the ascending-id tiebreak, got a different id"
                                   n n)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-wrong-length-query-errors
  "A query vector whose length does not match the segment dimension signals
an error, mirroring segment-scan's check, rather than silently scoring
against a prefix."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (signals error
                    (segment-score-subset s (%qvec 3 1.0 0.0 0.0) (list (%qid 1)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test vector-search-finds-the-nearest-node
  "vector-search resolves the owner segment and returns nearest node ids."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (near nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf near (id (make-si-doc :title "near"
                                             :embedding (%qvec 8 1.0 0.0)))))
               (with-transaction ()
                 (make-si-doc :title "far" :embedding (%qvec 8 0.0 1.0))))
             (let ((got (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 2)))
               (is (= 2 (length got)) "expected 2 hits, got ~S" got)
               (is (equalp near (cdr (first got)))
                   "nearest node should rank first")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test vector-search-empty-when-nothing-indexed
  "A declared slot with no segment yet returns NIL, not an error (segments are
created lazily on first conforming write)."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (is (null (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 5)))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test vector-search-spans-subclasses
  "Model B: querying by the SUBCLASS name must resolve to the ANCESTOR's owner
segment (%vector-index-slot-owner-name maps (si-sub, embedding) -> si-doc) and
find instances of BOTH classes stored there.  This is the discriminating
direction: a resolver that used the queried class name directly (ignoring
inheritance) would look up a (si-sub . embedding) key that no segment is ever
stored under -- since the write path always keys on the declaring ancestor --
and this test would see an empty result instead of the two hits below."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (sub-id nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (make-si-doc :title "parent" :embedding (%qvec 8 0.0 1.0)))
               (with-transaction ()
                 (setf sub-id (id (make-si-sub :title "child" :extra "x"
                                               :embedding (%qvec 8 1.0 0.0))))))
             ;; query via the SUBCLASS name, not the declaring owner class
             (let ((got (vector-search g 'si-sub 'embedding (%qvec 8 1.0 0.0) 5)))
               (is (= 2 (length got)) "owner segment should hold both, got ~S" got)
               (is (equalp sub-id (cdr (first got)))
                   "the subclass instance should be found and rank first")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test vector-search-truncates-to-k
  "K is passed through to the scan and actually truncates: with THREE nodes
stored, asking for k=2 must return exactly the two nearest, in order.  Every
other vector-search test uses a k >= the stored count, so each of them would
still pass if k were dropped on the floor or replaced by a large constant --
this one is the assertion that pins the pass-through.  The three embeddings
have strictly decreasing cosine against the query (1,0,...): (1,0)=1.0,
(1,1)=0.707, (0,1)=0.0, so the expected top-2 is unambiguous and the excluded
node is identified by id."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (best nil) (mid nil) (worst nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf best (id (make-si-doc :title "best"
                                             :embedding (%qvec 8 1.0 0.0)))))
               (with-transaction ()
                 (setf mid (id (make-si-doc :title "mid"
                                            :embedding (%qvec 8 1.0 1.0)))))
               (with-transaction ()
                 (setf worst (id (make-si-doc :title "worst"
                                              :embedding (%qvec 8 0.0 1.0))))))
             ;; sanity: all three really are in the segment, so a k=2 result of
             ;; length 2 means TRUNCATION and not "only two were ever stored"
             (let ((all (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 10)))
               (is (= 3 (length all)) "expected 3 nodes stored, got ~S" all))
             (let ((got (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 2)))
               (is (= 2 (length got))
                   "k=2 over 3 stored nodes must return 2, got ~D" (length got))
               (when (= 2 (length got))
                 (is (equalp best (cdr (first got))) "nearest should rank first")
                 (is (equalp mid (cdr (second got))) "second-nearest should rank second")
                 (is (null (find worst got :key #'cdr :test #'equalp))
                     "the third-nearest node must be truncated away by k=2"))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(defparameter *scan-race-rounds* 12
  "How many independent writer/scanner rounds SCAN-IS-SAFE-AGAINST-GROWING-WRITES
runs.  The race is timing-dependent, so one round is not evidence: each round
is a fresh capacity-2 segment grown to 128 slots, i.e. ~6 %SEG-GROW relocations,
so a run covers ~70 grow windows with thousands of concurrent scans across
them.  Rounds (rather than one long round) because scan cost is O(capacity) --
a single big segment quickly makes each scan so expensive that only a handful
run, which is exactly how this test would have become vacuous.")

(defun %scan-race-round (dim puts nscanners)
  "One round of the growing-writes race: a fresh capacity-2 segment, PUTS
distinct growing commits on this thread, NSCANNERS threads scanning throughout.

Returns (values SCANS BAD SHORT ERRS CAPS), where BAD holds scores that were
not a real number in [-1, 1], SHORT holds (committed-before-scan . hits-seen)
pairs where a scan missed an already-committed id, ERRS holds errors that
killed a scanner thread, and CAPS holds the distinct SEGMENT-CAPACITY values
observed by scanners across the round -- evidence that scans actually landed
inside growth, not just that enough of them ran.  Each scanner accumulates
into its OWN lists and hands them back through JOIN-THREAD, so nothing is
pushed onto a shared list from two threads at once (which would itself be a
race, and could lose the very evidence this test exists to collect)."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path dim :initial-capacity 2))
               (stop nil)
               (committed 0)
               (scanners '())
               (scans 0) (bad '()) (short '()) (errs '()) (caps '()))
           (unwind-protect
                (let ((q (let ((v (make-array dim :element-type 'single-float
                                                  :initial-element 0.0)))
                           (setf (aref v 0) 1.0)
                           v)))
                  (dotimes (i nscanners)
                    (push (bordeaux-threads:make-thread
                           (lambda ()
                             (let ((scans 0) (bad '()) (short '()) (err nil) (caps '()))
                               (handler-case
                                   (loop until stop
                                         do (let* (;; sampled BEFORE the scan:
                                                   ;; every put counted here
                                                   ;; completed before the scan
                                                   ;; took the read lock, so all
                                                   ;; of them must be visible
                                                   (n0 committed)
                                                   (hits (segment-scan s q (* 2 puts))))
                                              (incf scans)
                                              ;; sampled AFTER the scan, so a capacity
                                              ;; change mid-scan is still captured --
                                              ;; this is evidence scans overlapped
                                              ;; %seg-grow, not just that enough of
                                              ;; them ran
                                              (pushnew (segment-capacity s) caps)
                                              (when (< (length hits) n0)
                                                (push (cons n0 (length hits)) short))
                                              (dolist (hit hits)
                                                (let* ((score (car hit))
                                                       ;; a NaN fails both
                                                       ;; comparisons -- and on
                                                       ;; SBCL merely comparing
                                                       ;; one may trap, which is
                                                       ;; equally a bad read
                                                       (ok (handler-case
                                                               (and (realp score)
                                                                    (<= score 1.0001)
                                                                    (>= score -1.0001))
                                                             (arithmetic-error () nil))))
                                                  (unless ok
                                                    (push (princ-to-string score) bad))))))
                                 ;; a torn read that blows up rather than lying:
                                 ;; the relocated-but-capacity-not-yet-flipped
                                 ;; window leaves all-ones free-marker bytes
                                 ;; where vectors used to be, and those decode
                                 ;; to a float32 overflow
                                 (error (e) (setf err (princ-to-string e))))
                               (list scans bad short err caps)))
                           :name "segment-scanner")
                          scanners))
                  (dotimes (i puts)
                    (let ((v (make-array dim :element-type 'single-float
                                             :initial-element 0.0))
                          (id (make-array 16 :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
                      (setf (aref v (mod i dim)) 1.0
                            (aref id 0) (mod i 256)
                            (aref id 1) (floor i 256))
                      (segment-put s id v)
                      (incf committed)))
                  (setf stop t))
             ;; Cleanup runs on EVERY exit path from the protected form above --
             ;; including one where the writer loop (segment-put, above) itself
             ;; signals, which is exactly what a real regression would do.  STOP
             ;; and the joins used to live in the body, after the writer loop:
             ;; if the writer loop signalled, STOP was never set, the scanner
             ;; threads were never joined, and this cleanup form went straight
             ;; to CLOSE-VECTOR-SEGMENT while three threads were still actively
             ;; reading the mapping -- SIGSEGV / hard image death, or scanner
             ;; threads orphaned to walk freed memory for the rest of the suite,
             ;; instead of a readable FiveAM failure.  Setting STOP and joining
             ;; here, unconditionally, before CLOSE-VECTOR-SEGMENT, guarantees no
             ;; thread can still be touching the mapping when it is unmapped.
             ;;
             ;; Each JOIN-THREAD is wrapped in HANDLER-CASE: the scanner lambda
             ;; already catches everything internally and returns a plain list,
             ;; so JOIN-THREAD should never signal here, but this is the last
             ;; line of defense between a misbehaving thread and the unmap --
             ;; a join failure is folded into ERRS (a real test failure) rather
             ;; than propagating past CLOSE-VECTOR-SEGMENT below.
             (progn
               (setf stop t)
               (dolist (th scanners)
                 (handler-case
                     (destructuring-bind (n b sh e c) (bordeaux-threads:join-thread th)
                       (incf scans n)
                       (setf bad (append b bad)
                             short (append sh short)
                             caps (union c caps))
                       (when e (push e errs)))
                   (error (e) (push (princ-to-string e) errs))))
               (close-vector-segment s)))
           (values scans bad short errs caps))
      (ignore-errors (delete-file path)))))

(test scan-is-safe-against-growing-writes
  "Scanners running continuously against a writer doing GROWING commits never
observe a torn read.  %SEG-GROW relocates the WHOLE vector block and stores the
new capacity LAST; without the per-segment rw-lock a scanner can see the new
capacity against a not-yet-relocated block (or the reverse), and score bytes
that are half one block and half another.  Three independent detectors, any of
which fires on a torn read:

  (1) no scanner thread may die -- a half-relocated block hands the scan the
      all-ones free-slot-marker bytes as float32, which decodes to a
      scale-float overflow, so a torn read surfaces as an arithmetic error
      rather than a quietly wrong number;
  (2) every score must be a real number in [-1, 1] (cosine is normalised, so
      any value outside that came from garbage, and a NaN fails both
      comparisons);
  (3) no COMMITTED id may go missing -- the committed count is sampled BEFORE
      each scan, so every put it counts completed before the scan took the
      read lock and must therefore be visible.

Deliberately NOT a single long round: scan cost is O(capacity), so one big
segment yields only a handful of scans (measured: exactly ONE for a 400-put
capacity-512 segment) and proves almost nothing.  ~12 short rounds instead,
each covering ~6 grow relocations, gives thousands of scans across ~70 grow
windows.  The check that scans actually happened is asserted, so a starved or
dead scanner fails the test loudly instead of passing it silently.

The scan-count floor (10x *SCAN-RACE-ROUNDS*) only rules out total starvation
-- it says nothing about WHERE those scans landed, and ignores NSCANNERS and
PUTS entirely.  A later tweak to :initial-capacity or the round shape could
quietly collapse coverage to a single small capacity while staying above the
floor.  So each scanner also samples SEGMENT-CAPACITY on every iteration; the
distinct values collected across all rounds are asserted to span more than
one capacity, which is only possible if scans actually ran while %SEG-GROW
was relocating the block -- i.e. structural evidence that scans overlapped
growth, not just that enough of them happened.

VERIFIED LOAD-BEARING: with the rw-lock removed from segment-scan/segment-put
and the %seg-grow window widened by a sleep between the relocation and the
capacity store, this test FAILS."
  (let ((scans 0) (bad '()) (short '()) (errs '()) (caps '()))
    (dotimes (round *scan-race-rounds*)
      (declare (ignorable round))
      (multiple-value-bind (n b sh e c) (%scan-race-round 128 128 3)
        (incf scans n)
        (setf bad (append b bad)
              short (append sh short)
              errs (append e errs)
              caps (union c caps))))
    (is (null errs)
        "~D scanner thread(s) died on a torn read: ~S"
        (length errs) (subseq errs 0 (min 3 (length errs))))
    (is (plusp scans) "the scanners never ran -- test proves nothing")
    (is (> scans (* 10 *scan-race-rounds*))
        "only ~D scans across ~D rounds -- too few to have raced the writer"
        scans *scan-race-rounds*)
    (is (null bad)
        "~D torn reads: out-of-range scores ~S"
        (length bad) (subseq bad 0 (min 5 (length bad))))
    (is (null short)
        "~D scans missed already-committed ids (committed . seen): ~S"
        (length short) (subseq short 0 (min 5 (length short))))
    (is (> (length caps) 1)
        "scanners only ever observed ~D distinct capacit~:@P ~S -- scans never ~
overlapped a %seg-grow window, so this run provides no growth coverage"
        (length caps) caps)))

(test ranking-is-deterministic-across-rebuild
  "Scanning a segment, rebuilding it from nodes, and scanning again gives an
identical ranking -- including ties.  Slot order is meaningless under free-list
reuse, so this only holds because the tiebreak is carried through eviction.

All six embeddings tie exactly, so the ranking is entirely the node-id
tiebreak %SCORE-BEFORE-P documents (score DESC, node-id ASC).  Comparing
BEFORE to AFTER alone would only pin the ranking to ITSELF: since MAP-VERTICES'
sweep order happens to differ from creation order for ~719/720 random UUID
sets, that comparison catches a tiebreak regression by luck, not by structure
-- a rebuild that silently dropped the tiebreak and fell back to slot/sweep
order would still pass it whenever before and after happened to sweep the
same way.  So this also asserts BEFORE and AFTER independently equal the six
ids sorted by %ID-LESS-P -- the actual documented order -- which no amount of
sweep-order luck can satisfy by accident."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (ids '()))
      (unwind-protect
           (progn
             (let ((*graph* g))
               ;; several nodes sharing a score with the query, to force ties
               (dotimes (i 6)
                 (with-transaction ()
                   (push (id (make-si-doc :title (format nil "n~d" i)
                                          :embedding (%qvec 8 1.0 1.0)))
                         ids))))
             (let* ((q (%qvec 8 1.0 1.0))
                    (expected (sort (copy-list ids) #'%id-less-p))
                    (before (vector-search g 'si-doc 'embedding q 6)))
               (is (= 6 (length ids)) "expected 6 created ids, got ~S" ids)
               (is (= 6 (length before)) "expected 6 hits, got ~S" before)
               (is (equalp expected (mapcar #'cdr before))
                   "BEFORE ranking does not match the documented (score DESC, ~
node-id ASC) tiebreak order: expected ~S, got ~S"
                   expected (mapcar #'cdr before))
               (rebuild-vector-segment g 'si-doc 'embedding)
               (let ((after (vector-search g 'si-doc 'embedding q 6)))
                 (is (= (length before) (length after)))
                 (is (equalp expected (mapcar #'cdr after))
                     "AFTER ranking does not match the documented (score DESC, ~
node-id ASC) tiebreak order: expected ~S, got ~S"
                     expected (mapcar #'cdr after))
                 (when (= (length before) (length after))
                   (loop for b in before for a in after
                         do (is (equalp (cdr b) (cdr a))
                                "ranking changed across rebuild: ~S vs ~S"
                                (cdr b) (cdr a))
                            (is (< (abs (- (car b) (car a))) 1e-6)))))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
