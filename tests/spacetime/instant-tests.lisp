;;;; Degenerate extents against the thirteen (design §3.3.1).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun exact-instant (timestamp)
  (make-instant (exact-bound timestamp)))

(test an-instant-at-an-interval-start-is-starts-not-meets
  "The collision §3.3.1 resolves: E1 = S2 satisfies :MEETS and S1 = S2 with
E1 < E2 satisfies :STARTS.  Under closed intervals the point is INSIDE, so
:STARTS states strictly more and wins."
  (let ((p (exact-instant (ts 2026 1 2)))
        (i (exact-interval (ts 2026 1 2) (ts 2026 1 3))))
    (is (eq :starts (allen-relation p i)))
    (is-false (extent-meets-p p i))))

(test an-instant-relates-to-an-interval-by-the-five-reachable-relations
  (let ((i (exact-interval (ts 2026 1 10) (ts 2026 1 20))))
    (is (eq :before (allen-relation (exact-instant (ts 2026 1 5)) i)))
    (is (eq :starts (allen-relation (exact-instant (ts 2026 1 10)) i)))
    (is (eq :during (allen-relation (exact-instant (ts 2026 1 15)) i)))
    (is (eq :finishes (allen-relation (exact-instant (ts 2026 1 20)) i)))
    (is (eq :after (allen-relation (exact-instant (ts 2026 1 25)) i)))))

(test the-unreachable-relations-never-appear-for-an-instant
  "Design §7.3: a reintroduced collision shows up here first."
  (let ((i (exact-interval (ts 2026 1 10) (ts 2026 1 20)))
        (forbidden '(:meets :overlaps :contains :finished-by :started-by
                     :equals :met-by :overlapped-by)))
    (loop for day from 5 to 25 do
      (let ((rels (temporal-relation-relations
                   (allen-relations (exact-instant (ts 2026 1 day)) i))))
        (is (null (intersection forbidden rels))
            "day ~D produced forbidden ~S" day
            (intersection forbidden rels))))))

(test an-interval-versus-an-instant-is-the-inverse
  (let ((i (exact-interval (ts 2026 1 10) (ts 2026 1 20))))
    (dolist (day '(5 10 15 20 25))
      (let ((p (exact-instant (ts 2026 1 day))))
        (is (eq (allen-relation i p)
                (allen-inverse (allen-relation p i)))
            "day ~D" day)))))

(test two-instants-relate-only-three-ways
  (let ((a (exact-instant (ts 2026 1 10))))
    (is (eq :before (allen-relation a (exact-instant (ts 2026 1 11)))))
    (is (eq :equals (allen-relation a (exact-instant (ts 2026 1 10)))))
    (is (eq :after (allen-relation a (exact-instant (ts 2026 1 9)))))
    (let ((rels (temporal-relation-relations
                 (allen-relations (make-instant (unknown-bound))
                                  (make-instant (unknown-bound))))))
      (is (null (set-difference rels '(:before :equals :after)))))))

(test an-uncertain-instant-is-constrained-by-its-coupling
  "Plan correction 2: a wholly unknown INSTANT against an interval yields the
five reachable relations, not thirteen.  The coupling constrains the answer
even when the position does not."
  (let ((rels (temporal-relation-relations
               (allen-relations (make-instant (unknown-bound))
                                (exact-interval (ts 2026 1 10)
                                                (ts 2026 1 20))))))
    (is (= 5 (length rels)))
    (is (null (set-difference
               rels '(:before :starts :during :finishes :after))))))

(test two-granule-instants-in-one-month-are-not-equal
  "The §3.3 payoff: \"sometime in January\" twice is genuinely uncertain,
where two January INTERVALS are exactly EQUALS."
  (let ((a (make-granule-instant (ts 2026 1 4) :month))
        (b (make-granule-instant (ts 2026 1 27) :month)))
    (is-false (allen-definite-p a b))
    (is (null (set-difference (temporal-relation-relations
                               (allen-relations a b))
                              '(:before :equals :after))))))
