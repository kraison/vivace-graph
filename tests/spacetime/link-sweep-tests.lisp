;;;; The linking sweep and the write-time switch (GH #372, spec sec.5,
;;;; sec.11).  Fixtures come from endpoint-edge-tests.lisp, which loads
;;;; first (graph-db.asd): WITH-EE-GRAPH, EE-THING, EE-B, EE-LINKED-TO,
;;;; SAME-NODE-P, MAKE-EE-TWIN; WITH-SOURCE-GRAPH / MAKE-ST-REPORT from
;;;; source-tests.lisp.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test the-write-time-switch-defers-linking
  "Spec sec.9's knob: with *LINK-CLAIMS-AT-WRITE* NIL a claim about
present sources is written key-only, and no edge type is adopted."
  (is-true *link-claims-at-write*)
  (with-ee-graph (g)
    (let (c)
      (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
      (let ((*link-claims-at-write* nil))
        (with-transaction () (setq c (ee-b))))
      (is (null (ee-linked-to c 'subject-of g)))
      (is (null (ee-linked-to c 'object-of g)))
      (is (= 1 (length (claims-touching g 'ee-claim :ee-things "t-1"))))
      ;; The default is untouched outside the binding: a second claim
      ;; links as U1 does.
      (let ((c2 (with-transaction () (ee-b :relation "r2"))))
        (is-true (ee-linked-to c2 'subject-of g))))))
