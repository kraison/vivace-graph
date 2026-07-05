;;;; Import (GML, Pajek) and Graphviz export tests.

(in-package #:graph-db/algorithms-test)

(def-suite io-suite
  :description "GML/Pajek import and Graphviz DOT export."
  :in graph-db-algorithms-suite)

(in-suite io-suite)

(defun write-temp-file (content ext)
  (let ((path (merge-pathnames (format nil "gda-io-~36R.~A" (random (expt 36 10)) ext)
                               (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string content out))
    path))

(defun io-vc () (lambda (i label) (declare (ignore i)) (make-an :name label)))
(defun io-ec () (lambda (a b w) (make-ae :from a :to b :weight (coerce w 'float))))

(defparameter +pajek-text+
  "*Vertices 3
1 \"A\" 0.0 0.0
2 \"B\" 0.0 0.0
3 \"C\" 0.0 0.0
*Arcs
1 2 1.0
2 3 2.5
")

(test import-pajek-builds-graph
  "Pajek import creates the labeled vertices and weighted directed edges."
  (with-algo-graph (g)
    (let* ((file (write-temp-file +pajek-text+ "net"))
           (verts (unwind-protect
                       (import-pajek file :graph g
                                     :vertex-constructor (io-vc)
                                     :edge-constructor (io-ec))
                    (ignore-errors (delete-file file)))))
      (is (= 3 (length verts)))
      (is (equal '("A" "B" "C") (path-names verts)))
      (is (= 2 (gen-edge-count g)))
      ;; A -> B with weight 1.0
      (let ((adj (adjacent-vertices (first verts) :graph g :direction :out)))
        (is (= 1 (length adj)))
        (is (string= "B" (slot-value (car (first adj)) 'name)))
        (is (= 1.0 (cdr (first adj))))))))

(defparameter +gml-text+
  "graph [
  directed 1
  node [ id 1 label \"A\" ]
  node [ id 2 label \"B\" ]
  node [ id 3 label \"C\" ]
  edge [ source 1 target 2 value 1 ]
  edge [ source 2 target 3 value 2 ]
]
")

(test import-gml-builds-graph
  "GML import creates the labeled vertices and directed edges."
  (with-algo-graph (g)
    (let* ((file (write-temp-file +gml-text+ "gml"))
           (verts (unwind-protect
                       (import-gml file :graph g
                                   :vertex-constructor (io-vc)
                                   :edge-constructor (io-ec))
                    (ignore-errors (delete-file file)))))
      (is (= 3 (length verts)))
      (is (equal '("A" "B" "C") (sort (path-names verts) #'string<)))
      (is (= 2 (gen-edge-count g))))))

(test graph->dot-emits-digraph
  "graph->dot emits a digraph with the edge in DOT syntax."
  (with-populated-graph (g h '("A" "B") '(("A" "B")))
    (let ((dot (with-output-to-string (s)
                 (graph->dot :graph g :stream s
                             :label-fn (lambda (v) (slot-value v 'name))))))
      (is (search "digraph" dot))
      (is (search "\"A\" -> \"B\"" dot))
      ;; weight must be a Graphviz-valid number, not Lisp's "1.0d0"
      (is (search "label=\"1.0\"" dot))
      (is (not (search "d0" dot))))))
