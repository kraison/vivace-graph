;;;; The Allen algebra over interval extents (design §4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun exact-interval (s e)
  "An interval extent with exact endpoints, for the exactness tests."
  (make-interval (exact-bound s) (exact-bound e)))

(test the-vocabulary-is-thirteen-and-inversion-is-an-involution
  (is (= 13 (length +allen-relations+)))
  (is (= 13 (length (remove-duplicates +allen-relations+))))
  (is (eq :equals (allen-inverse :equals)))
  (dolist (r +allen-relations+)
    (is (eq r (allen-inverse (allen-inverse r)))
        "~S must invert back to itself" r)))

(test exact-intervals-give-a-singleton-matching-classical-allen
  (let ((cases
          ;; a-start a-end b-start b-end  expected
          '((1 2 3 4 :before)   (1 2 2 3 :meets)
            (1 3 2 4 :overlaps) (1 4 2 4 :finished-by)
            (1 5 2 4 :contains) (1 2 1 3 :starts)
            (1 2 1 2 :equals)   (1 3 1 2 :started-by)
            (2 3 1 4 :during)   (2 4 1 4 :finishes)
            (2 4 1 3 :overlapped-by)
            (2 3 1 2 :met-by)   (3 4 1 2 :after))))
    (dolist (c cases)
      (destructuring-bind (as ae bs be expected) c
        (let ((a (exact-interval (ts 2026 1 as) (ts 2026 1 ae)))
              (b (exact-interval (ts 2026 1 bs) (ts 2026 1 be))))
          (is (eq expected (allen-relation a b))
              "[~D,~D] vs [~D,~D] should be ~S, got ~S"
              as ae bs be expected (allen-relation a b))
          (is-true (allen-definite-p a b)))))))

(test every-relation-is-reachable-and-they-are-disjoint
  "Jointly exhaustive and pairwise disjoint for exact intervals (§7.3)."
  (let ((seen '()))
    (loop for as from 1 to 4 do
      (loop for ae from (1+ as) to 5 do
        (loop for bs from 1 to 4 do
          (loop for be from (1+ bs) to 5 do
            (let ((r (allen-relation (exact-interval (ts 2026 1 as)
                                                     (ts 2026 1 ae))
                                     (exact-interval (ts 2026 1 bs)
                                                     (ts 2026 1 be)))))
              (is-true r "exact intervals must give a singleton")
              (pushnew r seen))))))
    (is (null (set-difference +allen-relations+ seen))
        "unreached relations: ~S" (set-difference +allen-relations+ seen))))

(test inversion-holds-for-exact-intervals
  (loop for as from 1 to 3 do
    (loop for ae from (1+ as) to 4 do
      (loop for bs from 1 to 3 do
        (loop for be from (1+ bs) to 4 do
          (let ((a (exact-interval (ts 2026 1 as) (ts 2026 1 ae)))
                (b (exact-interval (ts 2026 1 bs) (ts 2026 1 be))))
            (is (eq (allen-relation a b)
                    (allen-inverse (allen-relation b a))))))))))

(test an-imprecise-interval-yields-a-set-not-a-wrong-answer
  "Two extents recorded as \"January 2026\" as INTERVALS are genuinely
EQUALS -- their endpoints are exact.  The uncertainty case is the instant,
which Task 5 covers."
  (let ((jan (make-granule-interval (ts 2026 1 15) :month)))
    (is (eq :equals (allen-relation jan jan)))))

(test a-wholly-unknown-interval-relates-to-everything
  "Design §3.1: total ignorance comes back as all thirteen, in the
algebra's own terms, not as NIL."
  (let ((unknown (make-interval (unknown-bound) (unknown-bound)
                                :standing :indeterminate))
        (known (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (is (= 13 (length (temporal-relation-relations
                       (allen-relations unknown known)))))
    (is-false (allen-relation unknown known))
    (is-false (allen-definite-p unknown known))))

(test the-relation-set-is-never-empty
  (let ((a (exact-interval (ts 2026 1 1) (ts 2026 1 2)))
        (b (exact-interval (ts 2026 6 1) (ts 2026 6 2))))
    (is-true (temporal-relation-relations (allen-relations a b)))))

(test a-relation-carries-both-standings-and-both-semantics
  "Design §4.4: the set, not a collapsed weakest value."
  (let* ((a (exact-interval (ts 2026 1 1) (ts 2026 1 2)))
         (b (make-interval (exact-bound (ts 2026 6 1))
                           (exact-bound (ts 2026 6 2))
                           :standing :inferred :semantics :validity))
         (r (allen-relations a b)))
    (is (null (set-difference '(:observed :inferred)
                              (temporal-relation-standings r))))
    (is (null (set-difference '(:event :validity)
                              (temporal-relation-semantics r))))))

(test predicates-are-set-membership
  (let ((a (exact-interval (ts 2026 1 1) (ts 2026 1 2)))
        (b (exact-interval (ts 2026 1 3) (ts 2026 1 4))))
    (is-true (extent-before-p a b))
    (is-false (extent-after-p a b))
    (is-true (extent-after-p b a))))
