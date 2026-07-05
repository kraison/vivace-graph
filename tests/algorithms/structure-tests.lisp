;;;; Structural-algorithm tests: degree, distance-map, components, spanning
;;;; tree, graph center.

(in-package #:graph-db/algorithms-test)

(def-suite structure-suite
  :description "Degree, BFS distances, components, spanning tree, center."
  :in graph-db-algorithms-suite)

(in-suite structure-suite)

;; Mixed graph: a directed cycle A->B->C->A, a tail C->D, and an isolated E.
(defparameter +struct-nodes+ '("A" "B" "C" "D" "E"))
(defparameter +struct-edges+ '(("A" "B") ("B" "C") ("C" "A") ("C" "D")))

(test degree-counts
  "out/in/total degree on the cycle+tail graph."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (is (= 1 (out-degree (gethash "A" h) :graph g)))   ; A->B
    (is (= 1 (in-degree (gethash "A" h) :graph g)))    ; C->A
    (is (= 2 (degree (gethash "A" h) :graph g)))
    (is (= 2 (out-degree (gethash "C" h) :graph g)))   ; C->A, C->D
    (is (= 1 (in-degree (gethash "C" h) :graph g)))    ; B->C
    (is (= 3 (degree (gethash "C" h) :graph g)))
    (is (= 0 (degree (gethash "E" h) :graph g)))))

(test degree-distribution-total
  "Total-degree distribution: E=0, D=1, A=B=2, C=3."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (is (equal '((0 . 1) (1 . 1) (2 . 2) (3 . 1))
               (degree-distribution :graph g :which :total)))))

(test distance-map-out
  "Outbound BFS from A reaches B@1, C@2, D@3 (and A@0)."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (let ((dm (distance-map (gethash "A" h) :graph g :direction :out)))
      (is (equal '(("A" . 0) ("B" . 1) ("C" . 2) ("D" . 3))
                 (mapcar (lambda (c) (cons (slot-value (car c) 'name) (cdr c)))
                         dm))))))

(test connected-components-weak
  "Weakly-connected components: {A,B,C,D} and {E}."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (let ((comps (connected-components :graph g :direction :both)))
      (is (= 2 (length comps)))
      (is (equal '(4 1) (mapcar #'length comps)))
      (is (equal '("A" "B" "C" "D")
                 (sort (path-names (first comps)) #'string<))))))

(test spanning-tree-covers-component
  "A spanning tree rooted at A spans its 4-node component with 3 edges."
  (with-populated-graph (g h +struct-nodes+ +struct-edges+)
    (multiple-value-bind (edges root)
        (spanning-tree :graph g :root (gethash "A" h) :direction :both)
      (is (equalp (id (gethash "A" h)) (id root)))
      (is (= 3 (length edges)))
      ;; every edge connects a parent already in the tree to a fresh child;
      ;; the covered vertex set is exactly the component
      (let ((covered (remove-duplicates
                      (cons "A" (mapcar (lambda (e) (slot-value (cdr e) 'name))
                                        edges))
                      :test #'string=)))
        (is (equal '("A" "B" "C" "D") (sort covered #'string<)))))))

;; A simple undirected path A-B-C-D-E for an unambiguous center (C, ecc 2).
(defparameter +path-nodes+ '("A" "B" "C" "D" "E"))
(defparameter +path-edges+ '(("A" "B") ("B" "C") ("C" "D") ("D" "E")))

(test eccentricity-on-path
  "On the 5-path (undirected), the endpoints have eccentricity 4 and the middle
node C has eccentricity 2."
  (with-populated-graph (g h +path-nodes+ +path-edges+)
    (is (= 4 (eccentricity (gethash "A" h) :graph g :direction :both)))
    (is (= 2 (eccentricity (gethash "C" h) :graph g :direction :both)))))

(test graph-center-on-path
  "The center of the 5-path is the single middle node C (min eccentricity 2)."
  (with-populated-graph (g h +path-nodes+ +path-edges+)
    (multiple-value-bind (centers ecc)
        (graph-center :graph g :direction :both)
      (is (= 2 ecc))
      (is (equal '("C") (path-names centers))))))
