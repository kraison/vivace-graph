;;;; Cardinality constraints: declaration, the commit-view count, commit-time
;;;; enforcement, the audit pass.
;;;; Design: docs/superpowers/specs/2026-08-31-ontology-evaluator-design.md
;;;; §3 (GH #155, unit 2 of #109).

(in-package #:graph-db/test)

(in-suite graph-db-suite)

(defparameter *cd-graph-name* :graph-db-cd-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *cd-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex cd-area () ((name :initarg :name :accessor cd-area-name))
  :graph-db-cd-test)
(def-vertex cd-zone () ((name :initarg :name :accessor cd-zone-name)
                        (note :initarg :note :accessor cd-zone-note))
  :graph-db-cd-test)
(def-edge cd-zone-of () () :graph-db-cd-test)
;; A subtype, to pin that it counts under its parent (OUTGOING-EDGES's
;; :INCLUDE-SUBCLASSES-P default).
(def-edge cd-zone-of-sub (cd-zone-of) () :graph-db-cd-test)

(defmacro with-cd-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *cd-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %cd-specs ()
  (gethash *cd-graph-name* graph-db::*schema-cardinality-metadata*))

;; The registry is keyed by graph NAME and outlives the fixture.
(defun %cd-clear ()
  (setf (gethash *cd-graph-name* graph-db::*schema-cardinality-metadata*)
        nil))

(defun %cd-count (vertex edge-type direction)
  (length (if (eq direction :out)
              (outgoing-edges vertex :edge-type edge-type)
              (incoming-edges vertex :edge-type edge-type))))

;;; --- declaration ---------------------------------------------------------

(test cardinality-declaration-registers-one-spec
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :min 1 :max 1 :name zone-has-one-area)
  (is (= 1 (length (%cd-specs))))
  (let ((s (first (%cd-specs))))
    (is (eq :out (graph-db::cardinality-spec-direction s)))
    (is (= 1 (graph-db::cardinality-spec-min s)))
    (is (= 1 (graph-db::cardinality-spec-max s))))
  (%cd-clear))

(test redeclaring-a-named-cardinality-replaces-rather-than-stacks
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :max 1 :name zone-has-one-area)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :max 2 :name zone-has-one-area)
  (is (= 1 (length (%cd-specs))))
  (is (= 2 (graph-db::cardinality-spec-max (first (%cd-specs)))))
  (is-true (undef-cardinality cd-zone :graph-db-cd-test
                              :name zone-has-one-area))
  (is (null (%cd-specs))))

(test undef-cardinality-withdraws-by-edge-type-and-direction
  (%cd-clear)
  (def-cardinality cd-area cd-zone-of :graph-db-cd-test
    :direction :in :max 3)
  (is-false (undef-cardinality cd-area :graph-db-cd-test
                               :edge-type cd-zone-of :direction :out)
            "the other direction is a different declaration")
  (is-true (undef-cardinality cd-area :graph-db-cd-test
                              :edge-type cd-zone-of :direction :in))
  (is (null (%cd-specs))))

(test a-cardinality-that-bounds-nothing-or-impossibly-is-refused
  (%cd-clear)
  (signals error
    (eval '(graph-db:def-cardinality cd-zone cd-zone-of :graph-db-cd-test
            :name cd-nothing)))
  (signals error
    (eval '(graph-db:def-cardinality cd-zone cd-zone-of :graph-db-cd-test
            :min 3 :max 1 :name cd-inverted)))
  (signals error
    (eval '(graph-db:def-cardinality cd-zone cd-zone-of :graph-db-cd-test
            :direction :sideways :max 1 :name cd-dir)))
  (is (null (%cd-specs))))

;;; --- commit-time enforcement ------------------------------------------

(test a-max-cardinality-refuses-the-edge-past-the-bound
  "GH #155.  The edge write is what triggers the check on its FROM
vertex: the first ZONE-OF commits, the second is refused, and the store
still holds exactly one."
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :max 1 :name zone-has-one-area)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let (z a b)
      (with-transaction ()
        (setq z (make-cd-zone :name "z")
              a (make-cd-area :name "a")
              b (make-cd-area :name "b")))
      (finishes (with-transaction () (make-cd-zone-of :from z :to a)))
      (signals cardinality-violation
        (with-transaction () (make-cd-zone-of :from z :to b)))
      (is (= 1 (%cd-count (lookup-vertex (id z)) 'cd-zone-of :out)))))
  (%cd-clear))

(test a-min-cardinality-is-counted-against-post-commit-state
  "GH #155, the overlay: a zone created WITH its edge in one commit is
one answer, not a min violation followed by a fix -- the store has
neither yet, so only the transaction's writes can say the count is 1.
A zone created alone is refused."
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :min 1 :name zone-needs-an-area)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let ((a (with-transaction () (make-cd-area :name "a"))))
      (finishes
        (with-transaction ()
          (let ((z (make-cd-zone :name "with-edge")))
            (make-cd-zone-of :from z :to a))))
      (signals cardinality-violation
        (with-transaction () (make-cd-zone :name "alone")))
      (is (= 1 (length (map-vertices 'identity g :vertex-type 'cd-zone
                                     :collect-p t)))
          "the refused zone was never written")))
  (%cd-clear))

(test deleting-the-last-edge-below-min-is-refused
  "GH #155.  A delete is an edge write too; the FROM vertex is re-counted
against the post-commit state, where the edge is gone."
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :min 1 :name zone-needs-an-area)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let (z a e)
      (with-transaction ()
        (setq z (make-cd-zone :name "z") a (make-cd-area :name "a"))
        (setq e (make-cd-zone-of :from z :to a)))
      (signals cardinality-violation
        (with-transaction () (mark-deleted (lookup-edge (id e)))))
      (is (= 1 (%cd-count (lookup-vertex (id z)) 'cd-zone-of :out))
          "the refused delete left the edge live")))
  (%cd-clear))

(test an-in-direction-cardinality-counts-edges-to-the-vertex
  (%cd-clear)
  (def-cardinality cd-area cd-zone-of :graph-db-cd-test
    :direction :in :max 2 :name area-holds-two-zones)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let (a z1 z2 z3)
      (with-transaction ()
        (setq a (make-cd-area :name "a")
              z1 (make-cd-zone :name "1") z2 (make-cd-zone :name "2")
              z3 (make-cd-zone :name "3")))
      (finishes (with-transaction ()
                  (make-cd-zone-of :from z1 :to a)
                  (make-cd-zone-of :from z2 :to a)))
      (signals cardinality-violation
        (with-transaction () (make-cd-zone-of :from z3 :to a)))
      (is (= 2 (%cd-count (lookup-vertex (id a)) 'cd-zone-of :in)))))
  (%cd-clear))

(test a-subtype-edge-counts-under-its-parent-type
  "Mirrors OUTGOING-EDGES's :INCLUDE-SUBCLASSES-P default: a constraint on
the parent edge type sees the subtype's edges."
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :max 1 :name zone-has-one-area)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let (z a b)
      (with-transaction ()
        (setq z (make-cd-zone :name "z")
              a (make-cd-area :name "a") b (make-cd-area :name "b")))
      (finishes (with-transaction () (make-cd-zone-of-sub :from z :to a)))
      (signals cardinality-violation
        (with-transaction () (make-cd-zone-of :from z :to b)))))
  (%cd-clear))

(test an-edge-created-and-deleted-in-one-commit-counts-zero
  (%cd-clear)
  (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
    :max 1 :name zone-has-one-area)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let (z a)
      (with-transaction ()
        (setq z (make-cd-zone :name "z") a (make-cd-area :name "a")))
      (with-transaction () (make-cd-zone-of :from z :to a))
      (finishes
        (with-transaction ()
          (let ((e (make-cd-zone-of :from z :to a)))
            (mark-deleted e)))
        "a second edge that never survives the commit is not a second edge")))
  (%cd-clear))

(test the-report-names-the-bound-and-the-count
  (let ((text (princ-to-string
               (make-condition 'cardinality-violation
                               :class-name 'cd-zone :edge-type 'cd-zone-of
                               :direction :out :actual 2 :min nil :max 1
                               :node-id (graph-db::gen-vertex-id)))))
    (is (search "CD-ZONE-OF" text))
    (is (search "2 out edges" text))
    (is (search "at most 1" text))))

;;; --- the documented hazard, and the audit pass -------------------------

(test a-pre-existing-violation-blocks-an-unrelated-update
  "⚠ As unit 1's :REQUIRED: the check reads whole post-commit state, so a
vertex already outside the bound cannot be touched until it is fixed.
Documented in DEF-CARDINALITY; the audit pass is how such nodes are found
before a constraint is declared."
  (%cd-clear)
  (with-cd-graph (g)
    (declare (ignorable g))
    (let ((z (with-transaction () (make-cd-zone :name "z"))))
      (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
        :min 1 :name zone-needs-an-area)
      (signals cardinality-violation
        (with-transaction ()
          (let ((c (copy (lookup-vertex (id z)))))
            (setf (cd-zone-note c) "touched")
            (save c))))))
  (%cd-clear))

(test the-audit-pass-finds-pre-existing-violations-and-counts-specs
  (%cd-clear)
  (with-cd-graph (g)
    (let (z1 z2 a)
      (with-transaction ()
        (setq z1 (make-cd-zone :name "bound") z2 (make-cd-zone :name "free")
              a (make-cd-area :name "a"))
        (make-cd-zone-of :from z1 :to a))
      (multiple-value-bind (violations checked specs)
          (check-cardinality-constraints g :vertex-type 'cd-zone)
        (is (null violations))
        (is (= 2 checked))
        (is (zerop specs) "zero specs: unchecked, not clean"))
      (def-cardinality cd-zone cd-zone-of :graph-db-cd-test
        :min 1 :name zone-needs-an-area)
      (multiple-value-bind (violations checked specs)
          (check-cardinality-constraints g :vertex-type 'cd-zone)
        (is (= 1 (length violations)))
        (is (equalp (id z2) (graph-db::cd-violation-node-id
                             (first violations))))
        (is (= 0 (graph-db::cd-violation-actual (first violations))))
        (is (= 2 checked))
        (is (= 1 specs)))))
  (%cd-clear))
