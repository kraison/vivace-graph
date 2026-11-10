;;;; tests/query/compiled-cache-tests.lisp -- the compiled query cache
;;;; (GH #376, design
;;;; docs/superpowers/specs/2026-11-10-compiled-query-cache-design.md).
;;;; Every name here is prefixed CQ-.

(in-package #:graph-db/query-test)

(in-suite query-suite)

(defun cq-canon (vars goals)
  "(VALUES SHAPE LITERALS) of VARS and GOALS."
  (graph-db::%canonical-query vars goals))

(defun cq-shape (vars goals)
  (nth-value 0 (cq-canon vars goals)))

(test cq-canonical-shape
  "Variables renamed by first appearance everywhere, literals lifted in
term positions only, two equal literals two entries, ? kept, nested
lists walked; a different projection, goal order or symbol is another
shape; a shape with no literal has none (design D1, Review Focus 5)."
  (multiple-value-bind (s1 l1)
      (cq-canon '(?s) '((find-by-slot ?s qt-item label "a")))
    (multiple-value-bind (s2 l2)
        (cq-canon '(?s) '((find-by-slot ?s qt-item label "b")))
      (is (equal s1 s2))
      (is (equalp #("a") l1))
      (is (equalp #("b") l2))
      (is (equal '((graph-db::?v0)
                   (find-by-slot graph-db::?v0 qt-item label
                                 (graph-db::%lit . 0)))
                 s1))))
  (is (not (equal (cq-shape '(?s) '((find-by-slot ?s qt-item label "a")))
                  (cq-shape '(?s ?l) '((find-by-slot ?s qt-item label "a"))))))
  (is (not (equal (cq-shape '(?s) '((find-by-slot ?s qt-item label "a")))
                  (cq-shape '(?s) '((find-by-slot ?s qt-item label a))))))
  (is (not (equal (cq-shape '(?x ?y) '((qt-links ?x ?y)))
                  (cq-shape '(?y ?x) '((qt-links ?x ?y))))))
  (is (equal (cq-shape '(?x) '((qt-links ?x ?y) (is-a ?y qt-item)))
             (cq-shape '(?a) '((qt-links ?a ?b) (is-a ?b qt-item)))))
  (is (not (equal (cq-shape '(?x) '((qt-links ?x ?y) (is-a ?y qt-item)))
                  (cq-shape '(?x) '((is-a ?y qt-item) (qt-links ?x ?y))))))
  ;; ? kept; nested lists walked
  (is (equal '((graph-db::?v0) (qt-links graph-db::?v0 ?))
             (cq-shape '(?x) '((qt-links ?x ?)))))
  (multiple-value-bind (s l) (cq-canon '(?x) '((= ?x (a 1 "b"))))
    (is (equal '((graph-db::?v0)
                 (= graph-db::?v0 (a (graph-db::%lit . 0)
                                     (graph-db::%lit . 1))))
               s))
    (is (equalp #(1 "b") l)))
  ;; two equal literals, two entries
  (is (equalp #("a" "a")
              (nth-value 1 (cq-canon '(?x ?y) '((= ?x "a") (= ?y "a"))))))
  ;; the aggregate spec: variables renamed, nothing lifted; its goal walked
  (multiple-value-bind (s l)
      (cq-canon '(?t) '((aggregate-all (sum ?r)
                                       (node-slot-value ?i rank ?r) ?t)
                        (aggregate-all count
                                       (node-slot-value ?j rank 2) ?n)))
    ;; written order: the spec's ?r is seen before the goal's ?i
    (is (equal '(aggregate-all (sum graph-db::?v1)
                               (node-slot-value graph-db::?v2 rank
                                                graph-db::?v1)
                               graph-db::?v0)
               (second s)))
    (is (equalp #(2) l)))
  ;; a lisp expression: not a term position
  (is (equalp #() (nth-value 1 (cq-canon '(?x) '((lisp ?x (+ 1 2)))))))
  ;; no literal, no variable beyond the projection
  (is (equalp #() (nth-value 1 (cq-canon '(?x) '((is-a ?x qt-item)))))))

(test cq-form-binds-literals
  "The cached form binds the literals first, through %bind-literals,
and reads its options from specials; a literal-free shape has no
binding goal (design D1, D2)."
  (let ((form (graph-db::%cached-query-form
               (cq-shape '(?s) '((find-by-slot ?s qt-item label "a")
                                 (node-slot-value ?s rank 2)))
               2)))
    (is (eq 'graph-db:select (first form)))
    (is (equal '(graph-db::%bind-literals graph-db::?l0 graph-db::?l1)
               (fourth form)))
    (is (equal '(find-by-slot graph-db::?v0 qt-item label graph-db::?l0)
               (fifth form)))
    (is (eq 'graph-db::*query-run-limit* (getf (second form) :limit)))
    (is (eq 'graph-db::*query-default-max-inferences*
            (getf (second form) :max-inferences))))
  (let ((form (graph-db::%cached-query-form
               (cq-shape '(?x) '((is-a ?x qt-item))) 0)))
    (is (equal '(is-a graph-db::?v0 qt-item) (fourth form)))))

(test cq-interpreted-canonical-rows
  "The runner's rows are unchanged by canonicalisation: a battery over
the fixture through RUN-GUARDED-PROLOG."
  (with-query-graph (g)
    (seed g)
    (flet ((rows (text) (nth-value 1 (q g text :format :raw))))
      (is (equal '((2))
                 (mapcar #'last (rows "(is-a ?i qt-item)
                                       (node-slot-value ?i label \"b\")
                                       (node-slot-value ?i rank ?r)"))))
      (is (= 2 (length (rows "(is-a ?i qt-item) (node-slot-value ?i rank ?r)
                               (> ?r 1)"))))
      (is (equal '(6)
                 (mapcar (lambda (r) (first (last r)))
                         (rows "(aggregate-all (sum ?r)
                                  (node-slot-value ?i rank ?r) ?t)"))))
      (is (= 1 (length (rows "(qt-links ?a ?b) (qt-links ?b ?c)"))))
      (is (= 1 (length (rows "(is-a ?i qt-item) (node-slot-value ?i label ?l)
                               (= ?l \"c\")"))))
      (is (= 2 (length (rows "(is-a ?i qt-item) (node-slot-value ?i label ?l)
                               (not (= ?l \"c\"))")))))))
