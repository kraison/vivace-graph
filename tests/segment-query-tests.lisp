;;;; Tests for the vector-segment query layer (segment-scan, score-subset,
;;;; vector-search, concurrency).

(in-package #:graph-db/test)

(def-suite segment-query-suite
  :description "segment-scan / segment-score-subset / vector-search."
  :in graph-db-suite)

(in-suite segment-query-suite)

;; Unique per process, not per image-relative clock -- see %SEG-PATH.
(defun %qpath ()
  (namestring (make-temp-file-name "vgquery" "dat")))

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

(defun %race-id (i)
  "The 16-byte node id the concurrency helpers use for index I: I little-endian
in the first two bytes, every other byte zero.  Distinct for I < 65536, and
never all-ones in its first 8 bytes (which %SEG-CHECK-ID rejects).

Being able to run this BACKWARDS is the point -- see %RACE-ID-INDEX: a scan
that hands back an id no writer ever stored is a torn read of the id array,
and that is only detectable because the id encoding is checkable."
  (let ((id (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref id 0) (mod i 256)
          (aref id 1) (floor i 256))
    id))

(defun %race-id-index (id limit)
  "The index %RACE-ID encoded into ID, or NIL if ID is not a well-formed race
id drawn from a universe of LIMIT ids.  A torn id read -- e.g. a scan that
checked the free marker, then read the 16 bytes after a concurrent
SEGMENT-REMOVE stamped +FREE-SLOT-MARKER+ over the first half -- fails the
all-zero tail check loudly instead of decoding to a plausible index."
  (and (typep id '(array (unsigned-byte 8) (*)))
       (= (length id) 16)
       (loop for k from 2 below 16 always (zerop (aref id k)))
       (let ((n (+ (aref id 0) (* 256 (aref id 1)))))
         (and (< n limit) n))))

(defun %race-pattern (dim which)
  "One of exactly TWO vectors, A (WHICH = 0, all +1.0) and B (WHICH = 1, all
-1.0), used by the overwrite and churn races.

They are chosen so that, against the query vector A, their cosines are exactly
+1.0 and -1.0 -- the two extreme, maximally separated legal values -- and so
that they differ in EVERY component.  That second property is what makes a torn
read arithmetically impossible to mistake for a legal one: splicing the first J
components of one onto the tail of the other gives dot = J - (DIM - J) over
norms sqrt(DIM) * sqrt(DIM), i.e. a cosine of (2J - DIM)/DIM.  That equals a
legal +/-1.0 ONLY at J = DIM or J = 0 (i.e. no tear at all); every other splice
point lands at least 2/DIM = 0.0156 away from both bands, 15x the 1e-3
tolerance.  A tear in the middle of a 4-byte element is even louder: it decodes
to an Inf/NaN pattern, which %SEG-DECODE-INTO turns into a FLOATING-POINT
error that kills the scanner thread.

This is why the bands are +/-1 rather than the more obvious 1.0 / 0.0: with an
orthogonal B a torn read still scores inside [-1, 1] and near the middle of the
range, where a plain range check is blind."
  (make-array dim :element-type 'single-float
                  :initial-element (if (zerop which) 1.0 -1.0)))

(defun %race-band (score)
  "0 if SCORE is the cosine of pattern A against the query, 1 if it is pattern
B's, NIL if it is neither -- i.e. a torn read.  A NaN fails both comparisons,
and merely comparing one may trap on SBCL, which is equally a bad read."
  (handler-case
      (cond ((not (realp score)) nil)
            ((< (abs (- score 1.0)) 1e-3) 0)
            ((< (abs (+ score 1.0)) 1e-3) 1)
            (t nil))
    (arithmetic-error () nil)))

(defun %scan-race-round (dim puts nscanners &key reader)
  "One round of the growing-writes race: a fresh capacity-2 segment, PUTS
distinct growing commits on this thread, NSCANNERS threads scanning throughout.

READER, if given, is the READ OPERATION under test: a function of (SEGMENT
QUERY-VECTOR) returning a list of (score . id) hits.  It defaults to
SEGMENT-SCAN with k = 2*PUTS (i.e. unbounded in practice), which is what
SCAN-IS-SAFE-AGAINST-GROWING-WRITES uses.  Parameterising it lets
SEGMENT-SCORE-SUBSET be raced against the identical writer, with the identical
three detectors, rather than duplicating this whole harness -- the two readers
differ only in how they choose which slots to touch, and every torn-read
signature is the same for both.

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
                           v))
                      (read-op (or reader
                                   (lambda (seg query)
                                     (segment-scan seg query (* 2 puts))))))
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
                                                   (hits (funcall read-op s q)))
                                              (incf scans)
                                              ;; sampled AFTER the scan, so a capacity
                                              ;; change mid-scan is still captured --
                                              ;; this is evidence scans overlapped
                                              ;; %seg-grow, not just that enough of
                                              ;; them ran
                                              (pushnew (segment-capacity s) caps)
                                              (when (< (length hits) n0)
                                                ;; The short scan IS the failure; the
                                                ;; immediate RE-SCAN is what makes the
                                                ;; next occurrence self-diagnosing
                                                ;; (GH #95).  Costs nothing on the
                                                ;; passing path -- it runs only here.
                                                (push (list :n0 n0 :first (length hits)
                                                            :rescan (length (funcall read-op s q))
                                                            :cap (segment-capacity s))
                                                      short))
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

      GH #95: detector (3) is the ONLY one that has ever fired in the wild, and
      it fires with clean scores and no dead thread -- a consistent but STALE
      view, not the torn read this test was built to catch.  Investigation could
      not reproduce it: 20/20 passes running this suite alone on ECL at load
      2-5, and a full suite passes under induced CPU load of 8-10.  The three
      observed failures all happened with two or three FULL SUITES running
      concurrently, which adds GC and I/O pressure that CPU load does not (ECL
      is GC-bound late in the suite, #43).  So each short scan now records an
      immediate re-scan; see the assertion message for how to read it.  Do not
      relax this detector to silence the flake -- it is load-bearing (removing
      the rw-lock makes it fail), and the capture is what will identify the
      cause the next time it fires naturally.

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
        "~D scans missed already-committed ids: ~S~@
         Read RESCAN (GH #95): >= :N0 means the data WAS present and the first ~
         scan missed it -- a real read-visibility race.  < :N0 means the ids ~
         were not in the segment yet, so COMMITTED over-counts and detector ~
         (3)'s premise is wrong (a test bug, not an engine bug)."
        (length short) (subseq short 0 (min 5 (length short))))
    (is (> (length caps) 1)
        "scanners only ever observed ~D distinct capacit~:@P ~S -- scans never ~
overlapped a %seg-grow window, so this run provides no growth coverage"
        (length caps) caps)))

(test score-subset-is-safe-against-growing-writes
  "SEGMENT-SCORE-SUBSET's READ LOCK, proven the same way SEGMENT-SCAN's is.
The identical writer (a fresh capacity-2 segment grown to 128 slots by 128
distinct commits, ~6 %SEG-GROW relocations per round), the identical three
detectors, but the read operation under test is SEGMENT-SCORE-SUBSET over a
candidate set holding every id the writer will ever commit.

This is a SEPARATE hazard from the scan's, not a restatement of it.
SEGMENT-SCORE-SUBSET resolves each candidate through the RAM ID->SLOT hash and
then reads that slot's vector at an offset derived from the CURRENT capacity
(%SEG-VEC-OFFSET).  Both halves race a growing commit independently: the offset
can be computed from the new capacity before the relocated bytes are visible
(or from the old capacity after the id array has already been extended over the
old block), and the hash itself is a plain, unsynchronized EQUALP table that
SEGMENT-PUT is concurrently (SETF GETHASH)-ing into.  Neither is reachable
through SEGMENT-SCAN, which never touches the hash at all.

Passing the FULL committed id universe as the candidate set is what keeps
detector (3) -- no committed id may go missing -- meaningful here: every id
counted by the pre-scan COMMITTED sample is in the candidate list, so a legal
result can never be shorter than that count.  Ids not yet committed are
absent from the segment and are silently skipped, which is
SEGMENT-SCORE-SUBSET's documented contract.

VERIFIED LOAD-BEARING: with only SEGMENT-SCORE-SUBSET's read lock removed
 (SEGMENT-SCAN's left in place), this test FAILS."
  (let ((candidates (loop for i from 0 below 128 collect (%race-id i)))
        (scans 0) (bad '()) (short '()) (errs '()) (caps '()))
    (dotimes (round *scan-race-rounds*)
      (declare (ignorable round))
      (multiple-value-bind (n b sh e c)
          ;; SIX scorer threads, not the growing-writes test's three: one
          ;; SEGMENT-SCORE-SUBSET call over 128 candidates costs far more than
          ;; one scan of the same round's (mostly small) capacity -- 128 EQUALP
          ;; hash lookups on 16-byte keys dominate -- so at three threads a
          ;; whole run managed only ~200 scorings, uncomfortably close to the
          ;; 120 floor below.  Six threads take it to ~1500, a 13x margin, so a
          ;; slower or loaded machine cannot drift the floor into flakiness.
          (%scan-race-round 128 128 6
                            :reader (lambda (seg query)
                                      (segment-score-subset seg query candidates)))
        (incf scans n)
        (setf bad (append b bad)
              short (append sh short)
              errs (append e errs)
              caps (union c caps))))
    (is (null errs)
        "~D scorer thread(s) died on a torn read: ~S"
        (length errs) (subseq errs 0 (min 3 (length errs))))
    (is (plusp scans) "the scorers never ran -- test proves nothing")
    (is (> scans (* 10 *scan-race-rounds*))
        "only ~D subset scorings across ~D rounds -- too few to have raced the writer"
        scans *scan-race-rounds*)
    (is (null bad)
        "~D torn reads: out-of-range scores ~S"
        (length bad) (subseq bad 0 (min 5 (length bad))))
    (is (null short)
        "~D scorings missed already-committed ids: ~S~@
         Read RESCAN (GH #95): >= :N0 means the data WAS present and the first ~
         scoring missed it -- a real read-visibility race.  < :N0 means the ids ~
         were not in the segment yet, so COMMITTED over-counts and detector ~
         (3)'s premise is wrong (a test bug, not an engine bug)."
        (length short) (subseq short 0 (min 5 (length short))))
    (is (> (length caps) 1)
        "scorers only ever observed ~D distinct capacit~:@P ~S -- they never ~
overlapped a %seg-grow window, so this run provides no growth coverage"
        (length caps) caps)))

(defparameter *churn-race-rounds* 6
  "Independent rounds SCAN-IS-SAFE-AGAINST-PUT-REMOVE-CHURN runs.  Rounds rather
than one long run for the same reason the growing-writes test uses them: the
race is timing-dependent, so a single round is an anecdote.")

(defun %churn-race-round (dim nids nscanners passes)
  "One round of the put/remove churn race: a segment prefilled with NIDS ids,
a writer doing PASSES full passes of remove/remove/put/put on this thread, and
NSCANNERS threads scanning throughout.

The writer pairs id I with id (I + NIDS/2), removes BOTH, then re-puts them in
the SAME order -- so the first put claims the free-list head, which is the
SECOND id's slot, and the two ids SWAP slots.  That is deliberate: a
remove-then-put of a single id would pop the slot it just pushed and land back
where it started, the free list would recycle without ever moving anything, and
the duplicate-id detector below would be dead code.  Swapping across half the
array instead means every id migrates a long way on every pass, which is what
makes a sweep able to see one twice.

Each pass also rewrites the vectors: id I gets pattern A or B by the parity of
 (I + PASS).  Because the writer advances one PAIR at a time, both patterns are
present in the segment at every instant, so a correctly-locked scan always sees
both bands -- a deterministic non-vacuity check that does not depend on winning
any race.

Returns (values SCANS BAD ILLEGAL DUPS MONO MISSES ERRS WERR CAP LIVE):

  BAD      scores in neither legal band (torn vector read),
  ILLEGAL  returned ids that no writer ever stored (torn id-array read),
  DUPS     ids returned TWICE by one scan (a slot swap seen from both ends),
  MONO     scans that saw only one of the two patterns,
  MISSES   scans returning fewer than NIDS-2 hits (at most one pair is ever
           mid-swap, so no legal scan can be shorter than that),
  ERRS     errors that killed a scanner, WERR one that killed the writer,
  CAP/LIVE the segment's final capacity and live count.

As in %SCAN-RACE-ROUND, each scanner accumulates into its OWN lists and hands
them back through JOIN-THREAD, and STOP plus the joins run in the CLEANUP form
ahead of CLOSE-VECTOR-SEGMENT so a signalling writer can never unmap the
segment out from under a live scanner."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path dim :initial-capacity nids))
               (stop nil)
               (scanners '())
               (scans 0) (bad '()) (illegal '()) (dups '()) (mono 0)
               (misses '()) (errs '()) (werr nil) (cap 0) (live 0))
           (unwind-protect
                (let* ((pa (%race-pattern dim 0))
                       (pb (%race-pattern dim 1))
                       (q pa)
                       (half (floor nids 2)))
                  (dotimes (i nids)
                    (segment-put s (%race-id i) (if (evenp i) pa pb)))
                  (dotimes (n nscanners)
                    (declare (ignorable n))
                    (push (bordeaux-threads:make-thread
                           (lambda ()
                             (let ((scans 0) (bad '()) (illegal '()) (dups '())
                                   (mono 0) (misses '()) (err nil))
                               (handler-case
                                   (loop until stop
                                         do (let ((hits (segment-scan s q (* 2 nids)))
                                                  (seen (make-hash-table :test 'equalp))
                                                  (band0 0) (band1 0))
                                              (incf scans)
                                              (dolist (hit hits)
                                                (let ((b (%race-band (car hit)))
                                                      (id (cdr hit)))
                                                  (case b
                                                    (0 (incf band0))
                                                    (1 (incf band1))
                                                    (t (push (princ-to-string (car hit)) bad)))
                                                  (unless (%race-id-index id nids)
                                                    (push (princ-to-string id) illegal))
                                                  (if (gethash id seen)
                                                      (push (princ-to-string id) dups)
                                                      (setf (gethash id seen) t))))
                                              (when (< (length hits) (- nids 2))
                                                (push (length hits) misses))
                                              (when (or (zerop band0) (zerop band1))
                                                (incf mono))))
                                 (error (e) (setf err (princ-to-string e))))
                               (list scans bad illegal dups mono misses err)))
                           :name "segment-churn-scanner")
                          scanners))
                  (handler-case
                      (loop for p from 1 to passes
                            do (dotimes (i half)
                                 (let ((a (%race-id i))
                                       (b (%race-id (+ i half))))
                                   (segment-remove s a)
                                   (segment-remove s b)
                                   (segment-put s a (if (evenp (+ i p)) pa pb))
                                   (segment-put s b (if (evenp (+ i half p)) pa pb)))))
                    (error (e) (setf werr (princ-to-string e))))
                  (setf cap (segment-capacity s)
                        live (segment-live-count s))
                  (setf stop t))
             (progn
               (setf stop t)
               (dolist (th scanners)
                 (handler-case
                     (destructuring-bind (n b il d m ms e)
                         (bordeaux-threads:join-thread th)
                       (incf scans n)
                       (incf mono m)
                       (setf bad (append b bad)
                             illegal (append il illegal)
                             dups (append d dups)
                             misses (append ms misses))
                       (when e (push e errs)))
                   (error (e) (push (princ-to-string e) errs))))
               (close-vector-segment s)))
           (values scans bad illegal dups mono misses errs werr cap live))
      (ignore-errors (delete-file path)))))

(test scan-is-safe-against-put-remove-churn
  "SEGMENT-REMOVE's WRITE LOCK.  A writer doing continuous remove/put churn --
so the free list is actively recycling slots and ids MIGRATE between them --
against continuously scanning threads.

The invariant the growing-writes test relies on does not survive here: with
removes in flight an id may legitimately vanish, so \"no committed id is
missing\" is no longer assertable.  What replaces it is strictly structural,
and every one of these is impossible under the write lock (a scan holds the
read lock for its WHOLE sweep, so no mutation can interleave with it):

  (1) no scanner thread may die;
  (2) every returned id must be an id the writer actually stores -- a scan that
      checks a slot's free marker and then reads its 16 bytes after a concurrent
      SEGMENT-REMOVE has stamped +FREE-SLOT-MARKER+ over the first half returns
      an id with 0xFF bytes in a region %RACE-ID always leaves zero;
  (3) no id may appear TWICE in one scan result -- only possible if an id
      migrated from an already-swept slot to a not-yet-swept one MID-SWEEP,
      which the read lock forbids outright;
  (4) every score must be within 1e-3 of one of the two legal pattern cosines
      (+1.0 / -1.0), so a torn vector read is caught as well;
  (5) no scan may return fewer than NIDS-2 hits -- at most one PAIR is ever
      mid-swap.

Non-vacuity is asserted three ways: a floor on the scan count (a starved or
dead scanner fails loudly rather than passing silently); the writer must not
have died; and every scan must have observed BOTH vector patterns, which is
deterministic rather than race-dependent because the writer advances one pair
at a time and so leaves both patterns present at every instant.  The free list
must also demonstrably have RECYCLED: ~6 x 400 x 16 puts against a capacity
asserted to still be NIDS means every put after the first NIDS reused a freed
slot rather than appending.

VERIFIED LOAD-BEARING: with only SEGMENT-REMOVE's write lock removed, this
test FAILS."
  (let ((nids 16)
        (scans 0) (bad '()) (illegal '()) (dups '()) (mono 0)
        (misses '()) (errs '()) (werrs '()) (caps '()) (lives '()))
    (dotimes (round *churn-race-rounds*)
      (declare (ignorable round))
      (multiple-value-bind (n b il d m ms e we cap live)
          (%churn-race-round 128 nids 3 400)
        (incf scans n)
        (incf mono m)
        (setf bad (append b bad)
              illegal (append il illegal)
              dups (append d dups)
              misses (append ms misses)
              errs (append e errs))
        (when we (push we werrs))
        (pushnew cap caps)
        (pushnew live lives)))
    (is (null errs)
        "~D scanner thread(s) died during put/remove churn: ~S"
        (length errs) (subseq errs 0 (min 3 (length errs))))
    (is (null werrs)
        "~D writer thread(s) died during put/remove churn: ~S"
        (length werrs) (subseq werrs 0 (min 3 (length werrs))))
    (is (plusp scans) "the scanners never ran -- test proves nothing")
    (is (> scans (* 10 *churn-race-rounds*))
        "only ~D scans across ~D rounds -- too few to have raced the churn"
        scans *churn-race-rounds*)
    (is (null illegal)
        "~D scan hits returned an id no writer ever stored (torn id-array read): ~S"
        (length illegal) (subseq illegal 0 (min 5 (length illegal))))
    (is (null dups)
        "~D ids appeared twice in a single scan (an id migrated mid-sweep): ~S"
        (length dups) (subseq dups 0 (min 5 (length dups))))
    (is (null bad)
        "~D torn vector reads: scores in neither legal band ~S"
        (length bad) (subseq bad 0 (min 5 (length bad))))
    (is (null misses)
        "~D scans returned fewer than ~D hits, but only one pair is ever ~
mid-swap: ~S"
        (length misses) (- nids 2) (subseq misses 0 (min 5 (length misses))))
    (is (zerop mono)
        "~D scans saw only ONE of the two vector patterns -- the writer was not ~
actually churning under them, so this run proves nothing" mono)
    (is (equal (list nids) caps)
        "capacity should have stayed ~D (every put after the prefill reusing a ~
freed slot); observed ~S -- the free list did not recycle, so removes were not ~
exercised" nids caps)
    (is (equal (list nids) lives)
        "live-count should end at ~D every round, observed ~S" nids lives)))

(defparameter *overwrite-race-rounds* 6
  "Independent rounds SCAN-IS-SAFE-AGAINST-CONCURRENT-OVERWRITES runs.")

(defun %overwrite-race-round (dim nids nscanners passes)
  "One round of the CONCURRENT-OVERWRITE race (spec sec 3.1's second hazard):
a segment prefilled with NIDS ids that are never added to or removed, and a
writer that repeatedly REWRITES their vectors in place -- SEGMENT-PUT's
existing-id branch, which no other test in this file ever runs under a
concurrent reader -- while NSCANNERS threads scan.

Every slot holds one of exactly two patterns (%RACE-PATTERN), flipped by the
parity of (id-index + pass), so a correctly-locked scan may only ever see
cosines of +1.0 and -1.0, and BOTH are present at every instant (the writer
advances one slot at a time).

Returns (values SCANS BAD DUPS MONO SHORT ERRS WERR): BAD holds scores in
neither band -- the torn-read detector -- DUPS ids seen twice in one scan,
MONO scans that saw only one pattern, SHORT scans that did not return exactly
NIDS hits (nothing is ever added or removed, so any other length is a
failure), ERRS scanner deaths and WERR a writer death.

STOP and the joins are in the CLEANUP form ahead of CLOSE-VECTOR-SEGMENT, so a
signalling writer cannot unmap the segment out from under a live scanner."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path dim :initial-capacity nids))
               (stop nil)
               (scanners '())
               (scans 0) (bad '()) (dups '()) (mono 0) (short '())
               (errs '()) (werr nil))
           (unwind-protect
                (let* ((pa (%race-pattern dim 0))
                       (pb (%race-pattern dim 1))
                       (q pa))
                  (dotimes (i nids)
                    (segment-put s (%race-id i) (if (evenp i) pa pb)))
                  (dotimes (n nscanners)
                    (declare (ignorable n))
                    (push (bordeaux-threads:make-thread
                           (lambda ()
                             (let ((scans 0) (bad '()) (dups '()) (mono 0)
                                   (short '()) (err nil))
                               (handler-case
                                   (loop until stop
                                         do (let ((hits (segment-scan s q (* 2 nids)))
                                                  (seen (make-hash-table :test 'equalp))
                                                  (band0 0) (band1 0))
                                              (incf scans)
                                              (dolist (hit hits)
                                                (let ((b (%race-band (car hit)))
                                                      (id (cdr hit)))
                                                  (case b
                                                    (0 (incf band0))
                                                    (1 (incf band1))
                                                    (t (push (princ-to-string (car hit)) bad)))
                                                  (if (gethash id seen)
                                                      (push (princ-to-string id) dups)
                                                      (setf (gethash id seen) t))))
                                              (unless (= (length hits) nids)
                                                (push (length hits) short))
                                              (when (or (zerop band0) (zerop band1))
                                                (incf mono))))
                                 (error (e) (setf err (princ-to-string e))))
                               (list scans bad dups mono short err)))
                           :name "segment-overwrite-scanner")
                          scanners))
                  (handler-case
                      (loop for p from 1 to passes
                            do (dotimes (i nids)
                                 (segment-put s (%race-id i)
                                              (if (evenp (+ i p)) pa pb))))
                    (error (e) (setf werr (princ-to-string e))))
                  (setf stop t))
             (progn
               (setf stop t)
               (dolist (th scanners)
                 (handler-case
                     (destructuring-bind (n b d m sh e)
                         (bordeaux-threads:join-thread th)
                       (incf scans n)
                       (incf mono m)
                       (setf bad (append b bad)
                             dups (append d dups)
                             short (append sh short))
                       (when e (push e errs)))
                   (error (e) (push (princ-to-string e) errs))))
               (close-vector-segment s)))
           (values scans bad dups mono short errs werr))
      (ignore-errors (delete-file path)))))

(test scan-is-safe-against-concurrent-overwrites
  "SEGMENT-PUT's WRITE LOCK on the OVERWRITE path -- the hazard the design spec
names in sec 3.1 (\"overwriting a multi-kilobyte vector with SET-BYTES is not
atomic, so a lock-free scan could read one candidate mid-write and score
garbage\") and that nothing else in this file exercises: every put in the
growing-writes race uses a DISTINCT id, so SEGMENT-PUT's existing-id branch
never once runs under a concurrent scanner.

Here the id set is fixed and fully present from the start; the writer only ever
rewrites vectors in place, and %SEG-WRITE-VECTOR's SET-BYTES of DIM*4 bytes is
the unprotected window.

THE DETECTOR IS THE POINT.  A range check on [-1, 1] is nearly useless for this
hazard -- a half-overwritten vector almost always still scores inside the legal
cosine range, so a test built on it would be green and prove nothing.  Instead
every slot holds one of exactly TWO patterns whose cosines against the query
are the two EXTREMES, +1.0 and -1.0, and which differ in every component (see
%RACE-PATTERN).  A correctly-locked scan can therefore only ever return scores
within 1e-3 of +1.0 or -1.0.  A torn read splices a prefix of one pattern onto
the tail of the other and scores (2J - DIM)/DIM, which for any splice point
other than the ends is at least 0.0156 from BOTH bands -- 15x the tolerance, so
a tear is arithmetically incapable of masquerading as a legal read.  A tear
inside a 4-byte element is louder still: it decodes to Inf/NaN and kills the
scanner outright.

Non-vacuity: a floor on the scan count; the writer must not have died; every
scan must return exactly NIDS hits (nothing is added or removed); and every
scan must have observed BOTH patterns -- deterministic, not race-dependent,
because the writer flips one slot at a time and so leaves both present at every
instant.

VERIFIED LOAD-BEARING: with only SEGMENT-PUT's write lock removed, this test
FAILS."
  (let ((scans 0) (bad '()) (dups '()) (mono 0) (short '())
        (errs '()) (werrs '()))
    (dotimes (round *overwrite-race-rounds*)
      (declare (ignorable round))
      (multiple-value-bind (n b d m sh e we)
          (%overwrite-race-round 128 16 3 600)
        (incf scans n)
        (incf mono m)
        (setf bad (append b bad)
              dups (append d dups)
              short (append sh short)
              errs (append e errs))
        (when we (push we werrs))))
    (is (null errs)
        "~D scanner thread(s) died on a torn overwrite: ~S"
        (length errs) (subseq errs 0 (min 3 (length errs))))
    (is (null werrs)
        "~D writer thread(s) died: ~S"
        (length werrs) (subseq werrs 0 (min 3 (length werrs))))
    (is (plusp scans) "the scanners never ran -- test proves nothing")
    (is (> scans (* 10 *overwrite-race-rounds*))
        "only ~D scans across ~D rounds -- too few to have raced the overwriter"
        scans *overwrite-race-rounds*)
    (is (null bad)
        "~D torn overwrite reads: scores in neither legal band (+1.0/-1.0) ~S"
        (length bad) (subseq bad 0 (min 5 (length bad))))
    (is (null dups)
        "~D ids appeared twice in a single scan: ~S"
        (length dups) (subseq dups 0 (min 5 (length dups))))
    (is (null short)
        "~D scans did not return all 16 ids, though none is ever added or ~
removed: ~S"
        (length short) (subseq short 0 (min 5 (length short))))
    (is (zerop mono)
        "~D scans saw only ONE of the two vector patterns -- the writer was not ~
actually overwriting under them, so this run proves nothing" mono)))

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
