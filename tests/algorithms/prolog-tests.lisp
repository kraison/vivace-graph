;;;; Prolog predicate wrapper tests: shortest-path/4, distance/3, reachable/2,
;;;; connected-component/2, degree/2, page-rank/2, authority-value/2, hub-value/2.
;;;; Queries pin specific vertices with (is-a ?v an) (node-slot-value ?v name ..).

(in-package #:graph-db/algorithms-test)

(def-suite prolog-suite
  :description "Prolog query predicates over the graph algorithms."
  :in graph-db-algorithms-suite)

(in-suite prolog-suite)

(test prolog-shortest-path-binds-path-and-cost
  "(shortest-path ?from ?to ?path ?cost) binds the weighted path and cost."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (let ((cost (first (select-flat (?cost)
                         (is-a ?from an) (node-slot-value ?from name "A")
                         (is-a ?to an)   (node-slot-value ?to name "D")
                         (shortest-path ?from ?to ?path ?cost))))
          (path (first (select-flat (?path)
                         (is-a ?from an) (node-slot-value ?from name "A")
                         (is-a ?to an)   (node-slot-value ?to name "D")
                         (shortest-path ?from ?to ?path ?cost)))))
      (is (= 3.0 cost))
      (is (equal '("A" "B" "C" "D") (path-names path))))))

(test prolog-distance
  "(distance ?from ?to ?d) binds the weighted shortest-path distance."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (is (= 3.0 (first (select-flat (?d)
                        (is-a ?from an) (node-slot-value ?from name "A")
                        (is-a ?to an)   (node-slot-value ?to name "D")
                        (distance ?from ?to ?d)))))))

(test prolog-reachable
  "(reachable ?from ?to) succeeds along a directed path and fails otherwise."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (is (select-flat (?x)
          (is-a ?from an) (node-slot-value ?from name "A")
          (is-a ?to an)   (node-slot-value ?to name "D")
          (reachable ?from ?to) (= ?x t)))
    (is (null (select-flat (?x)
                (is-a ?from an) (node-slot-value ?from name "D")
                (is-a ?to an)   (node-slot-value ?to name "A")
                (reachable ?from ?to) (= ?x t))))))

(test prolog-degree
  "(degree ?node ?d) binds the total degree; C in the cycle+tail graph has 3."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (is (= 3 (first (select-flat (?d)
                      (is-a ?node an) (node-slot-value ?node name "C")
                      (degree ?node ?d)))))))

(test prolog-connected-component
  "(connected-component ?node ?member) enumerates the weakly-connected component."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (let ((members (select-flat (?name)
                     (is-a ?node an) (node-slot-value ?node name "A")
                     (connected-component ?node ?member)
                     (node-slot-value ?member name ?name))))
      (is (equal '("A" "B" "C" "D") (sort members #'string<))))))

(test prolog-page-rank-enumerates-and-sums-to-one
  "(page-rank ?node ?rank) enumerates every node; ranks sum to 1."
  (with-populated-graph (g h '("A" "B" "C") '(("A" "B") ("B" "C") ("C" "A")))
    (let ((ranks (select-flat (?r) (page-rank ?node ?r))))
      (is (= 3 (length ranks)))
      (is (approx= 1.0 (reduce #'+ ranks) 1d-6)))))

(test prolog-hits-enumerates
  "(authority-value ?node ?v) and (hub-value ?node ?v) enumerate all nodes."
  (with-populated-graph (g h '("H1" "A1") '(("H1" "A1")))
    (is (= 2 (length (select-flat (?v) (authority-value ?node ?v)))))
    (is (= 2 (length (select-flat (?v) (hub-value ?node ?v)))))))
