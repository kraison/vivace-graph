;;;; Import (GML, Pajek) and Graphviz export tests.

(in-package #:graph-db/algorithms-test)

(def-suite io-suite
  :description "GML/Pajek import and Graphviz DOT export."
  :in graph-db-algorithms-suite)

(in-suite io-suite)

(defun write-temp-file (content ext)
  (let ((path (graph-db-test-scratch:make-scratch-file-name
               "gda-io" ext)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string content out))
    path))

(defun io-vc () (lambda (i label) (declare (ignore i)) (make-an :name label)))
(defun io-ec () (lambda (a b w) (make-ae :from a :to b :weight (coerce w 'float))))

(test gml-lexer-token-stream
  "Hand-rolled SCAN-GML (GH #240) yields the dso-lex-era token stream."
  (let ((line
         (concatenate
          'string
          "  node [ id 1 label \"say \"\"hi\"\"\" tag 'it''s' ]  ")))
    (is (equal
         (list (list 'graph-db-aio::val "node")
               (list 'graph-db-aio::bracket "[")
               (list 'graph-db-aio::val "id")
               (list 'graph-db-aio::val "1")
               (list 'graph-db-aio::val "label")
               (list 'graph-db-aio::val "say \"hi\"")
               (list 'graph-db-aio::val "tag")
               (list 'graph-db-aio::val "it's")
               (list 'graph-db-aio::bracket "]"))
         (graph-db-aio::lex-gml 'graph-db-aio::scan-gml line)))))

(test gml-lexer-single-token-classes
  "Each rule in isolation: bare val, brackets, quoted vals."
  (flet ((one (s)
           (first (graph-db-aio::lex-gml 'graph-db-aio::scan-gml s))))
    (is (equal (list 'graph-db-aio::val "42.5") (one "42.5")))
    (is (equal (list 'graph-db-aio::bracket "[") (one "[")))
    (is (equal (list 'graph-db-aio::bracket "]") (one "]")))
    (is (equal (list 'graph-db-aio::val "a b") (one "'a b'")))
    (is (equal (list 'graph-db-aio::val "a b") (one "\"a b\"")))
    ;; LEX-GML drops empty images, so "" lexes to no token at all --
    ;; exactly as under dso-lex.
    (is (null (one "\"\"")))
    ;; Doubled escape flush against the closing quote at end of input.
    (is (equal (list 'graph-db-aio::val "a\"") (one "\"a\"\"\"")))))

(test gml-lexer-unterminated-string-yields-nil
  "An unterminated quoted string makes LEX-GML return NIL, as the
dso-lex lexer did (no rule matched => silent NIL)."
  (is (null (graph-db-aio::lex-gml 'graph-db-aio::scan-gml
                                   "label \"oops")))
  (is (null (graph-db-aio::lex-gml 'graph-db-aio::scan-gml
                                   "label 'oops"))))

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
