;;;; graph-db/algorithms-io -- graph import (GML, Pajek) and Graphviz export.
;;;;
;;;; The OPTIONAL io add-on for graph-db/algorithms.  It pulls in the parsing
;;;; dependencies (cl-ppcre, yacc, dso-lex, parse-number) that the core algorithm
;;;; add-on deliberately avoids, so importers stay out of the embeddable core.
;;;;
;;;; The file parsers are vendored from graph-utils: they build an in-memory
;;;; projection graph (labels as node values, edge weights), which IMPORT-GML /
;;;; IMPORT-PAJEK then materialize into a real VivaceGraph in one transaction via
;;;; caller-supplied vertex/edge constructors -- the same schema-agnostic pattern
;;;; as GENERATE-GRAPH.  Graphviz export (GRAPH->DOT / VISUALIZE) is VG-native.

;;; ==================================================================
;;; Vendored graph-utils file parsers (-> projection graph)
;;; ==================================================================

(defpackage :graph-db-algorithms-io
  (:use :cl :cl-ppcre :yacc :dso-lex)
  (:nicknames :graph-db-aio)
  (:export #:parse-gml #:parse-pajek))

(in-package :graph-db-algorithms-io)

(defun snip (s) (subseq s 1 (1- (length s))))
(defun un-squote (s) (regex-replace-all "''" (snip s) "'"))
(defun un-dquote (s) (regex-replace-all "\"\"" (snip s) "\""))
(defun strip-whitespace (s) (regex-replace-all "(^\\s+|\\s+$)" s ""))

(deflexer scan-gml ()
  ("\\s+" whitespace strip-whitespace)
  ("[^\"'\\[\\]\\s]+" val strip-whitespace)
  ("(\\[|\\])" bracket)
  ("'(?:[^']|'')*'" val un-squote)
  ("\"(?:[^\"]|\"\")*\"" val un-dquote))

(defun lex-gml (lexer input)
  (labels ((my-scan (start tokens)
             (if (> (length input) start)
                 (multiple-value-bind (class image remainder)
                     (funcall lexer input start)
                   (when class
                     (if (> (length image) 0)
                         (my-scan remainder (cons (list class image) tokens))
                         (my-scan remainder tokens))))
                 (nreverse tokens))))
    (my-scan 0 '())))

(define-parser *gml-parser*
  (:start-symbol lst-opt)
  (:terminals (val bracket sq-val dq-val))
  (lst-opt lst ())
  (lst (keyvalue lst-opt))
  (keyvalue (key value))
  (key val)
  (value val (bracket lst bracket)))

(defun build-node (node-def)
  "Build a node as (id label value)."
  (let ((node (make-list 3)))
    (labels ((walk-node (n)
               (cond ((null n) nil)
                     ((atom n) nil)
                     ((consp n)
                      (cond ((consp (first n)) (walk-node (first n)))
                            ((equal "id" (first n)) (setf (nth 0 node) (second n)))
                            ((equal "label" (first n)) (setf (nth 1 node) (second n)))
                            ((equal "value" (first n)) (setf (nth 2 node) (second n))))
                      (walk-node (second n))))))
      (walk-node (second node-def))
      node)))

(defun build-edge (edge-def)
  "Build an edge as (source target value)."
  (let ((edge (make-list 3)))
    (labels ((walk-edge (n)
               (cond ((null n) nil)
                     ((atom n) nil)
                     ((consp n)
                      (cond ((consp (first n)) (walk-edge (first n)))
                            ((equal "value" (first n)) (setf (nth 2 edge) (second n)))
                            ((equal "source" (first n)) (setf (nth 0 edge) (second n)))
                            ((equal "target" (first n)) (setf (nth 1 edge) (second n))))
                      (walk-edge (second n))))))
      (walk-edge (second edge-def))
      edge)))

(defun build-graph (gml-tree)
  (let ((graph nil) (directed? nil) (nodes nil) (edges nil)
        (id-table (make-hash-table :test 'equal)))
    (labels ((walk-tree (tree)
               (cond ((null tree) nil)
                     ((atom tree) nil)
                     ((consp tree)
                      (let ((this (first tree)))
                        (cond ((and (equal (first this) "directed")
                                    (equal (second this) "1"))
                               (setq directed? t))
                              ((equal (first this) "node") (push (build-node this) nodes))
                              ((equal (first this) "edge") (push (build-edge this) edges))))
                      (walk-tree (second tree))))))
      (walk-tree gml-tree))
    (setq graph (graph-db.projection:make-graph :directed? directed?))
    (dolist (node (nreverse nodes))
      (graph-db.projection:add-node graph (or (second node) (first node)))
      (setf (gethash (first node) id-table) (or (second node) (first node))))
    (dolist (edge (nreverse edges))
      (let ((n1 (graph-db.projection:lookup-node graph (gethash (first edge) id-table)))
            (n2 (graph-db.projection:lookup-node graph (gethash (second edge) id-table)))
            (w (if (third edge) (parse-number:parse-number (third edge)) 1)))
        (when (and n1 n2)
          (graph-db.projection:add-edge graph n1 n2 :weight w))))
    graph))

(defun parse-gml (file)
  "Parse a GML FILE into a projection graph."
  (let ((tokens nil) graph)
    (with-open-file (in file :direction :input)
      (do ((input (read-line in nil :eof) (read-line in nil :eof)))
          ((or (eql input :eof) (equal input "")))
        (setq tokens (nconc tokens (lex-gml 'scan-gml input)))))
    (let ((tree (parse-with-lexer (lambda () (values-list (pop tokens))) *gml-parser*)))
      ;; tree shape is (("graph" ("[" inner "]")) NIL): walk each top-level branch,
      ;; and for the "graph" key build from its bracketed inner list.  (graph-utils
      ;; assumed an extra nesting level here -- (first (first branch)) -- which did
      ;; not match this yacc build's output.)
      (dolist (branch tree)
        (when (and (consp branch) (equal "graph" (first branch)))
          (setq graph (build-graph (second (second branch)))))))
    graph))

(defun parse-pajek (file)
  "Parse a Pajek .net FILE into a projection graph."
  (let ((graph nil) (vertices? nil) (vertex-queue nil) (arcs? nil)
        (index (make-hash-table :test 'equal)))
    (with-open-file (in file :direction :input)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (setq line (regex-replace "^\\s+" line ""))
        (cond ((scan "^%" line) nil)
              ((scan "^\\*[Vv]ertices" line)
               (setq vertices? t arcs? nil))
              ((scan "^\\*[Aa]rcs" line)
               (setq graph (graph-db.projection:make-graph :directed? t))
               (dolist (value (nreverse vertex-queue))
                 (graph-db.projection:add-node graph value))
               (setq arcs? t vertices? nil))
              ((scan "^\\*[Ee]dge" line)
               (setq graph (graph-db.projection:make-graph :directed? nil))
               (dolist (value (nreverse vertex-queue))
                 (graph-db.projection:add-node graph value))
               (setq arcs? t vertices? nil))
              (vertices?
               (do-register-groups (id value rest)
                   ("^([0-9]+)\\s+(\"(?:[^\"]|\"\")*\"|\\w+)\\s+(.*)$"
                    line nil :start 0 :sharedp t)
                 (declare (ignore rest))
                 (setq value (regex-replace-all "\"" value ""))
                 (setf (gethash id index) value)
                 (push value vertex-queue)))
              (arcs?
               (destructuring-bind (source destination &optional value)
                   (split "\\s+" line)
                 (graph-db.projection:add-edge
                  graph
                  (graph-db.projection:lookup-node graph (or (gethash source index) source))
                  (graph-db.projection:lookup-node graph (or (gethash destination index) destination))
                  :weight (if (and value (scan "^[0-9.]+$" value))
                              (parse-number:parse-number value)
                              1)))))))
    graph))

;;; ==================================================================
;;; VivaceGraph-side import + Graphviz export
;;; ==================================================================

(in-package :graph-db)

(defun %materialize-parsed (pg graph vertex-constructor edge-constructor)
  "Materialize a parsed projection graph PG into VG GRAPH in one transaction.
VERTEX-CONSTRUCTOR is (INDEX LABEL) -> vertex; EDGE-CONSTRUCTOR is (FROM TO
WEIGHT) -> edge.  Returns the created vertices in index order."
  (let* ((n (graph-db.projection:node-count pg))
         (idx->vertex (make-array n :initial-element nil))
         (directed (graph-db.projection:directed? pg))
         (*graph* graph))
    (with-transaction ()
      (dotimes (i n)
        (setf (aref idx->vertex i)
              (funcall vertex-constructor i (graph-db.projection:lookup-node pg i))))
      (let ((seen (make-hash-table :test 'equal)))
        (dolist (edge (graph-db.projection:list-edges pg))
          (destructuring-bind (i j) edge
            (let ((key (if directed (cons i j) (cons (min i j) (max i j)))))
              (unless (gethash key seen)
                (setf (gethash key seen) t)
                (funcall edge-constructor (aref idx->vertex i) (aref idx->vertex j)
                         (graph-db.projection:edge-weight pg i j))))))))
    (coerce idx->vertex 'list)))

(defun import-gml (file &key (graph *graph*) vertex-constructor edge-constructor)
  "Import a GML FILE into GRAPH transactionally.  VERTEX-CONSTRUCTOR is a function
of (INDEX LABEL) returning a VG vertex; EDGE-CONSTRUCTOR a function of
\(FROM-VERTEX TO-VERTEX WEIGHT) creating an edge.  Returns the created vertices."
  (assert vertex-constructor () "import-gml requires :vertex-constructor")
  (assert edge-constructor () "import-gml requires :edge-constructor")
  (%materialize-parsed (graph-db-aio:parse-gml file) graph
                       vertex-constructor edge-constructor))

(defun import-pajek (file &key (graph *graph*) vertex-constructor edge-constructor)
  "Import a Pajek .net FILE into GRAPH transactionally.  See IMPORT-GML for the
constructor contract."
  (assert vertex-constructor () "import-pajek requires :vertex-constructor")
  (assert edge-constructor () "import-pajek requires :edge-constructor")
  (%materialize-parsed (graph-db-aio:parse-pajek file) graph
                       vertex-constructor edge-constructor))

(defun graph->dot (&key (graph *graph*) (stream *standard-output*) label-fn
                        edge-type vertex-type)
  "Write GRAPH to STREAM in Graphviz DOT format (a digraph).  LABEL-FN maps a
vertex to its node label (default: its id string); edge labels are edge weights.
EDGE-TYPE / VERTEX-TYPE may each be a single type or a list of types."
  (let ((label (or label-fn (lambda (v) (string-id (id v))))))
    (format stream "digraph graphdb {~%")
    (apply #'map-vertices (lambda (v) (format stream "  ~S;~%" (funcall label v)))
           graph (%type-args vertex-type :vertex-type :include-vertex-types))
    (apply #'map-edges
           (lambda (e)
             (let ((from (lookup-vertex (from e) :graph graph))
                   (to (lookup-vertex (to e) :graph graph)))
               (when (and from to)
                 ;; Quote + ~F the weight: edge weights round-trip as
                 ;; double-floats, and ~A would emit Lisp syntax like "1.0d0",
                 ;; which Graphviz rejects.
                 (format stream "  ~S -> ~S [label=~S];~%"
                         (funcall label from) (funcall label to)
                         (format nil "~F" (weight e))))))
           graph (%type-args edge-type :edge-type :include-edge-types))
    (format stream "}~%")
    (values)))

(defun visualize (&key (graph *graph*) (file "/var/tmp/graph.dot") render
                       (format "svg") label-fn edge-type vertex-type)
  "Write GRAPH to FILE as DOT.  When RENDER is non-nil, run Graphviz to produce a
FORMAT (e.g. \"svg\"/\"png\") rendering and return its path; RENDER selects the
layout engine: :hierarchical (dot), :circular (circo), :radial (twopi), :spring
\(fdp), or T (dot).  EDGE-TYPE / VERTEX-TYPE may each be a single type or a list
of types.  Without RENDER, returns the DOT file path."
  (with-open-file (out file :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (graph->dot :graph graph :stream out :label-fn label-fn
                :edge-type edge-type :vertex-type vertex-type))
  (if render
      (let ((program (case render
                       (:hierarchical "dot") (:circular "circo")
                       (:radial "twopi") (:spring "fdp") (otherwise "dot")))
            (out-file (cl-ppcre:regex-replace "\\.[a-z]+$" file
                                              (format nil ".~A" format))))
        (multiple-value-bind (output err status)
            (trivial-shell:shell-command
             (format nil "~A -T~A -o ~A ~A" program format out-file file))
          (declare (ignore output))
          (unless (= 0 status)
            (error "graphviz ~A exited ~A: ~A" program status err))
          out-file))
      file))
