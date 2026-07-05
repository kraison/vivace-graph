;;;; Ranking-algorithm tests: PageRank (+ distribution), HITS, SimRank.

(in-package #:graph-db/algorithms-test)

(def-suite ranking-suite
  :description "PageRank, HITS hubs/authorities, SimRank."
  :in graph-db-algorithms-suite)

(in-suite ranking-suite)

(defun approx= (a b &optional (eps 1d-3))
  (< (abs (- a b)) eps))

(defun score-of (ranked name)
  "RANK of the vertex named NAME in a (VERTEX . SCORE) list."
  (cdr (assoc name ranked :key (lambda (v) (slot-value v 'name)) :test #'string=)))

;;; ---- PageRank ----------------------------------------------------

;; A symmetric 3-cycle A->B->C->A: the stationary distribution is uniform (1/3).
(test pagerank-symmetric-cycle-uniform
  (with-populated-graph (g h '("A" "B" "C") '(("A" "B") ("B" "C") ("C" "A")))
    (let ((ranked (page-rank :graph g)))
      (is (approx= 1/3 (score-of ranked "A")))
      (is (approx= 1/3 (score-of ranked "B")))
      (is (approx= 1/3 (score-of ranked "C")))
      ;; ranks form a probability distribution
      (is (approx= 1.0 (reduce #'+ ranked :key #'cdr) 1d-6)))))

(test pagerank-dangling-sums-to-one
  "With a dangling sink D, mass is redistributed so ranks still sum to 1, and the
sink (everyone points at it) ranks highest."
  (with-populated-graph (g h '("A" "B" "C" "D")
                            '(("A" "D") ("B" "D") ("C" "D") ("D" "A")))
    (let ((ranked (page-rank :graph g)))
      (is (approx= 1.0 (reduce #'+ ranked :key #'cdr) 1d-6))
      (is (string= "D" (slot-value (car (first ranked)) 'name))))))

(test pagerank-converges-before-cap
  "Convergence stops well before the iteration cap on a tiny graph."
  (with-populated-graph (g h '("A" "B" "C") '(("A" "B") ("B" "C") ("C" "A")))
    (multiple-value-bind (ranked iters) (page-rank :graph g :max-iterations 100)
      (declare (ignore ranked))
      (is (< iters 100)))))

(test pagerank-distribution-counts
  "Distribution bins partition all nodes (counts sum to N)."
  (with-populated-graph (g h '("A" "B" "C" "D")
                            '(("A" "D") ("B" "D") ("C" "D") ("D" "A")))
    (let ((bins (page-rank-distribution :graph g :bin-count 4)))
      (is (= 4 (reduce #'+ bins :key #'third))))))

;;; ---- HITS --------------------------------------------------------

;; Hubs H1,H2 point at authorities A1,A2:  H1->A1, H1->A2, H2->A1.
;; A1 is pointed at by both hubs (more authoritative); H1 points at both auths
;; (better hub).
(test hits-authority-and-hub-ordering
  (with-populated-graph (g h '("H1" "H2" "A1" "A2")
                            '(("H1" "A1") ("H1" "A2") ("H2" "A1")))
    (multiple-value-bind (auth hub) (hub-authority-values :graph g)
      ;; A1 is the top authority, and strictly above A2 (> 0)
      (is (string= "A1" (slot-value (car (first auth)) 'name)))
      (is (> (score-of auth "A1") (score-of auth "A2")))
      (is (> (score-of auth "A2") 0))
      ;; H1 is the top hub, strictly above H2
      (is (string= "H1" (slot-value (car (first hub)) 'name)))
      (is (> (score-of hub "H1") (score-of hub "H2"))))))

(test hits-vectors-l2-normalized
  "Authority and hub vectors are L2-normalized each iteration (sum of squares ~1)."
  (with-populated-graph (g h '("H1" "H2" "A1" "A2")
                            '(("H1" "A1") ("H1" "A2") ("H2" "A1")))
    (multiple-value-bind (auth hub) (hub-authority-values :graph g)
      (is (approx= 1.0 (reduce #'+ auth :key (lambda (c) (expt (cdr c) 2)))))
      (is (approx= 1.0 (reduce #'+ hub :key (lambda (c) (expt (cdr c) 2))))))))

;;; ---- SimRank -----------------------------------------------------

;; A points at both C and D, so In(C) = In(D) = {A}; SimRank(C,D) = decay * 1.
(test simrank-shared-in-neighbor
  (with-populated-graph (g h '("A" "C" "D") '(("A" "C") ("A" "D")))
    (is (approx= 1.0 (sim-rank (gethash "A" h) (gethash "A" h) :graph g)))
    (is (approx= 0.8 (sim-rank (gethash "C" h) (gethash "D" h)
                               :graph g :decay 0.8d0 :max-depth 3)))
    ;; symmetric
    (is (approx= (sim-rank (gethash "C" h) (gethash "D" h) :graph g)
                 (sim-rank (gethash "D" h) (gethash "C" h) :graph g)))))

(test simrank-no-common-structure
  "A node with no in-neighbours is dissimilar (0) to any distinct node."
  (with-populated-graph (g h '("A" "C" "D") '(("A" "C") ("A" "D")))
    ;; In(A) is empty, so A is dissimilar to C
    (is (approx= 0.0 (sim-rank (gethash "A" h) (gethash "C" h) :graph g)))))
