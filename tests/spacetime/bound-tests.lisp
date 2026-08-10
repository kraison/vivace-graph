;;;; Bounds: a range within which one timestamp lies (design §3.2).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test bound-rejects-a-reversed-range
  (signals invalid-bound (make-bound (ts 2026 5 1) (ts 2026 1 1)))
  (signals invalid-bound (make-bound 17 (ts 2026 1 1))))

(test bound-exactness
  (is-true (bound-exact-p (exact-bound (ts 2026 1 1))))
  (is-false (bound-exact-p (make-bound (ts 2026 1 1) (ts 2026 1 2))))
  (is-false (bound-exact-p (unknown-bound)))
  (is-true (bound-unknown-p (unknown-bound)))
  (is-false (bound-unknown-p (exact-bound (ts 2026 1 1)))))

(test bound-compare-is-definite-when-ranges-are-disjoint
  (let ((a (make-bound (ts 2026 1 1) (ts 2026 1 31)))
        (b (make-bound (ts 2026 3 1) (ts 2026 3 31))))
    (is (eq :< (bound-compare a b)))
    (is (eq :> (bound-compare b a)))))

(test bound-compare-is-equal-only-when-both-are-exact
  (let ((a (exact-bound (ts 2026 1 1)))
        (b (exact-bound (ts 2026 1 1)))
        (wide (make-bound (ts 2026 1 1) (ts 2026 1 31))))
    (is (eq := (bound-compare a b)))
    ;; Two ranges that merely COINCIDE are not equal: the timestamps they
    ;; stand for may differ anywhere inside.
    (is (eq :ambiguous (bound-compare wide wide)))))

(test bound-compare-is-ambiguous-when-ranges-overlap
  (let ((a (make-bound (ts 2026 1 1) (ts 2026 2 15)))
        (b (make-bound (ts 2026 2 1) (ts 2026 3 1))))
    (is (eq :ambiguous (bound-compare a b)))
    (is (eq :ambiguous (bound-compare b a)))))

(test unbounded-never-produces-a-verdict-but-does-not-prevent-one
  "Design §3.2: :UNBOUNDED cannot satisfy a strict inequality, but the OTHER
endpoint pair can still settle the comparison."
  (let ((late  (make-bound (ts 2030 1 1) :unbounded))
        (early (make-bound :unbounded (ts 2020 1 1)))
        (any   (unknown-bound)))
    (is (eq :> (bound-compare late early)))
    (is (eq :< (bound-compare early late)))
    (is (eq :ambiguous (bound-compare any late)))
    (is (eq :ambiguous (bound-compare any any)))))
