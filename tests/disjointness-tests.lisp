;;;; Disjointness over vertex types (unit 4a): a schema lint at DEF-DISJOINT
;;;; time, at every later node-type definition, and in an audit pass.
;;;; Design: docs/superpowers/specs/2026-08-30-disjointness-design.md
;;;; (GH #157).

(in-package #:graph-db/test)

(in-suite graph-db-suite)

(defparameter *dj-graph-name* :graph-db-dj-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *dj-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex dj-observation () () :graph-db-dj-test)
(def-vertex dj-fortification () () :graph-db-dj-test)
(def-vertex dj-unclassified () () :graph-db-dj-test)
(def-vertex dj-bunker (dj-fortification) () :graph-db-dj-test)   ; a subtype

(defun %dj-specs ()
  (gethash *dj-graph-name* graph-db::*schema-disjointness-metadata*))

(defun %dj-clear ()
  (setf (gethash *dj-graph-name* graph-db::*schema-disjointness-metadata*)
        nil))

(defmacro %with-dj-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *dj-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

;;; --- declaration ---------------------------------------------------------

(test a-disjointness-declaration-registers-once-in-either-order
  "The trap the note names: (disjoint a b) and (disjoint b a) are one
declaration.  Asserted on the registry length, not on behaviour."
  (%dj-clear)
  (def-disjoint (dj-observation dj-fortification) :graph-db-dj-test
    :name dj-classes)
  (def-disjoint (dj-fortification dj-observation) :graph-db-dj-test
    :name dj-classes)
  (is (= 1 (length (%dj-specs))))
  (is (equal '(dj-fortification dj-observation)
             (graph-db::disjointness-spec-classes (first (%dj-specs))))
      "canonicalised by name")
  (is-true (undef-disjoint :graph-db-dj-test :name dj-classes))
  (is (null (%dj-specs))))

(test an-unnamed-or-too-small-disjointness-is-refused
  (%dj-clear)
  (signals error
    (eval '(graph-db:def-disjoint (dj-observation dj-fortification)
            :graph-db-dj-test)))
  (signals error
    (eval '(graph-db:def-disjoint (dj-observation dj-observation)
            :graph-db-dj-test :name dj-one)))
  (is (null (%dj-specs))))

;;; --- the lint at DEF-DISJOINT time ------------------------------------

(test declaring-a-class-disjoint-from-its-own-supertype-is-refused
  "⚠ The discriminating case (note, Testing): BUNKER is a FORTIFICATION,
so declaring them disjoint contradicts the class graph and must go red
at definition -- the vacuous version of this test is the default outcome."
  (%dj-clear)
  (let ((e (handler-case
               (progn
                 (eval '(graph-db:def-disjoint (dj-bunker dj-fortification)
                         :graph-db-dj-test :name dj-contradiction))
                 nil)
             (disjointness-violation (e) e))))
    (is-true e)
    (is (eq 'dj-bunker (djv-offender e)))
    (is (null (%dj-specs)) "a contradicted declaration is not recorded")))

(test unrelated-classes-may-be-declared-disjoint
  (%dj-clear)
  (finishes
    (def-disjoint (dj-observation dj-fortification dj-unclassified)
        :graph-db-dj-test :name dj-three))
  (is (= 1 (length (%dj-specs))))
  (%dj-clear))

(test a-class-not-yet-defined-may-be-named
  "The declaration may precede the class; it is checked when the class
is defined (next test)."
  (%dj-clear)
  (finishes
    (def-disjoint (dj-observation dj-not-yet-defined) :graph-db-dj-test
      :name dj-forward))
  (%dj-clear))

;;; --- the lint at DEF-VERTEX time ---------------------------------------

(test defining-a-class-under-two-disjoint-classes-is-refused
  "The half the note says is easy to omit: a LATER class that inherits
from two declared-disjoint classes is refused as it is defined, through
*NODE-TYPE-DEFINITION-HOOKS*."
  (%dj-clear)
  (def-disjoint (dj-observation dj-fortification) :graph-db-dj-test
    :name dj-classes)
  (let ((e (handler-case
               (progn
                 (eval '(graph-db:def-vertex dj-both
                         (dj-observation dj-fortification) ()
                         :graph-db-dj-test))
                 nil)
             (disjointness-violation (e) e))))
    (is-true e "a class under both is refused at definition")
    (is (eq 'dj-both (djv-offender e)))
    (is (eq 'dj-classes (graph-db::djv-name e))))
  ;; and a class under ONE of them is fine
  (finishes
    (eval '(graph-db:def-vertex dj-only-one (dj-observation) ()
            :graph-db-dj-test)))
  (%dj-clear))

;;; --- the audit pass -----------------------------------------------------

(test the-disjointness-audit-reports-specs-and-finds-an-offender
  (%dj-clear)
  (%with-dj-graph (g)
    (multiple-value-bind (violations specs) (check-disjointness g)
      (is (null violations))
      (is (zerop specs) "zero specs: unchecked, not clean"))
    (def-disjoint (dj-observation dj-unclassified) :graph-db-dj-test
      :name dj-classes)
    (multiple-value-bind (violations specs) (check-disjointness g)
      (is (null violations))
      (is (= 1 specs)))
    ;; An offender that slipped past definition time (simulated by
    ;; registering the spec behind the lint's back).
    (push (graph-db::make-disjointness-spec
           :classes '(dj-bunker dj-fortification)
           :graph-name *dj-graph-name* :name 'dj-smuggled)
          (gethash *dj-graph-name* graph-db::*schema-disjointness-metadata*))
    (multiple-value-bind (violations specs) (check-disjointness g)
      (is (= 2 specs))
      (is (= 1 (length violations)))
      (is (eq 'dj-bunker (cdr (first violations))))))
  (%dj-clear))
