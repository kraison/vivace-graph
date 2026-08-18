;;;; Declarative value constraints: declaration, retraction, applicability.
;;;; Design: docs/superpowers/specs/2026-08-17-value-constraints-design.md
;;;; (GH #149).

(in-package #:graph-db/test)

(in-suite graph-db-suite)

(defparameter *vc-graph-name* :graph-db-vc-test)

;; Idempotent-reload guard, as graph-tests.lisp does: re-loading this file
;; must not stack duplicate node metadata.
(eval-when (:load-toplevel :execute)
  (setf (gethash *vc-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex vc-doc ()
  ((status :initarg :status :accessor vc-doc-status)
   (note :initarg :note :accessor vc-doc-note))
  :graph-db-vc-test)

(def-vertex vc-report (vc-doc) () :graph-db-vc-test)

(defparameter +vc-statuses+ '(:draft :final :withdrawn))

;; A fresh on-disk graph per test, verbatim after WITH-UQ-GRAPH
;; (tests/unique-constraint-tests.lisp:54).
(defmacro with-vc-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *vc-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %vc-specs ()
  (gethash *vc-graph-name*
           graph-db::*schema-value-constraint-metadata*))

;; The registry is keyed by graph NAME, so it outlives the fixture's
;; teardown and must be cleared per test.
(defun %vc-clear ()
  (setf (gethash *vc-graph-name*
                 graph-db::*schema-value-constraint-metadata*)
        nil))

(test value-constraint-declaration-registers-one-spec
  "The declarative registry, mirroring *SCHEMA-UNIQUE-METADATA*."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (is (= 1 (length (%vc-specs))))
  (is (equal +vc-statuses+
             (graph-db::value-constraint-spec-one-of
              (first (%vc-specs))))))

(test redeclaring-a-named-constraint-replaces-rather-than-stacks
  "GH #139: replacing in place is what stops the table growing one entry
per evaluation.  A file is loaded more than once in a session."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of '(:draft :final) :name vc-status)
  (is (= 1 (length (%vc-specs))))
  (is (equal '(:draft :final)
             (graph-db::value-constraint-spec-one-of
              (first (%vc-specs))))))

(test one-of-is-evaluated-not-quoted
  "⚠ Deliberately unlike :SLOTS and :NAME.  This is what lets the standing
constraint name +STANDINGS+ instead of duplicating the vocabulary, so the
constraint and STANDINGP cannot drift apart (design, \"The macro\")."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (is (equal '(:draft :final :withdrawn)
             (graph-db::value-constraint-spec-one-of
              (first (%vc-specs))))
      "a quoted :ONE-OF would have stored the SYMBOL +VC-STATUSES+"))

(test undef-value-constraint-withdraws-by-name
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (is-true (undef-value-constraint vc-doc :graph-db-vc-test
                                   :name vc-status))
  (is (= 0 (length (%vc-specs)))))

(test undef-value-constraint-withdraws-by-slot
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+)
  (is-true (undef-value-constraint vc-doc :graph-db-vc-test
                                   :slot status))
  (is (= 0 (length (%vc-specs)))))

(test undef-value-constraint-is-a-no-op-when-nothing-matches
  (%vc-clear)
  (is-false (undef-value-constraint vc-doc :graph-db-vc-test
                                    :name no-such-constraint)))

(test a-constraint-on-a-parent-applies-to-its-subclass
  "⚠ Load-bearing, not incidental: STANDING lives on the PARENT claim class
and DEF-CLAIM-CLASSES generates two arities, so one declaration must cover
both (design, \"Registry\")."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (with-vc-graph (g)
    (is (= 1 (length (graph-db::class-value-constraint-specs
                      (find-class 'vc-report) g))))))

(test a-constraint-naming-an-absent-slot-does-not-apply
  (%vc-clear)
  (def-value-constraint vc-doc no-such-slot :graph-db-vc-test
    :one-of '(:a :b) :name vc-absent)
  (with-vc-graph (g)
    (is (= 0 (length (graph-db::class-value-constraint-specs
                      (find-class 'vc-doc) g))))))

(test a-constraint-that-declares-nothing-is-refused
  "⚠ :ONE-OF NIL with :REQUIRED NIL constrains nothing while reading as a
guard -- the counter-that-cannot-fail shape.  Refused at declaration."
  (%vc-clear)
  (signals error
    (eval '(graph-db:def-value-constraint
            vc-doc status :graph-db-vc-test))))

;;; --- the evaluator -------------------------------------------------------

;; ⚠ ORDERING, in every test below: CREATE THE NODE FIRST, DECLARE THE
;; CONSTRAINT SECOND.  Task 3 makes a violating commit signal, so a node
;; created under a live constraint could not be committed at all -- and
;; creating it first is also the only way to obtain the pre-constraint damage
;; the audit pass (Task 4) exists to find.
(defun %vc-make (g &rest initargs)
  "Commit a VC-DOC and return it, read back from G."
  (let ((id (with-transaction () (id (apply #'make-vc-doc initargs)))))
    (lookup-vertex id)))

(defun %vc-violations-for (node g)
  (graph-db::%value-constraint-violations node g))

(test a-value-outside-the-enumeration-is-a-violation
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status :nonsense)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (let ((vs (%vc-violations-for v g)))
        (is (= 1 (length vs)))
        (is (eq :not-in-vocabulary
                (graph-db::vc-violation-reason (first vs))))
        (is (eq :nonsense (graph-db::vc-violation-actual (first vs))))
        (is (equal +vc-statuses+
                   (graph-db::vc-violation-expected (first vs))))))))

(test every-member-of-the-enumeration-is-accepted
  "⚠ A guard bought by refusing everything is not a guard."
  (%vc-clear)
  (with-vc-graph (g)
    (let ((nodes (mapcar (lambda (s) (%vc-make g :status s))
                         +vc-statuses+)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (dolist (v nodes)
        (is (null (%vc-violations-for v g))
            "~S is in the vocabulary and must not be a violation"
            (vc-doc-status v))))))

(test nil-is-exempt-without-required
  "Matches DEF-UNIQUE's null rule: \"if present, it must be one of these\".
Diverging would be the trap GH #107 named."
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (is (null (%vc-violations-for v g))))))

(test nil-is-a-violation-under-required
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :required t :name vc-status)
      (let ((vs (%vc-violations-for v g)))
        (is (= 1 (length vs)))
        (is (eq :missing (graph-db::vc-violation-reason (first vs))))))))

(test required-alone-checks-presence-only
  (%vc-clear)
  (with-vc-graph (g)
    (let ((present (%vc-make g :status :anything))
          (absent (%vc-make g :status nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :required t :name vc-status)
      (is (null (%vc-violations-for present g)))
      (is (= 1 (length (%vc-violations-for absent g)))))))

(test two-constraints-on-one-node-both-report
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status :nonsense :note nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (def-value-constraint vc-doc note :graph-db-vc-test
        :required t :name vc-note)
      (is (= 2 (length (%vc-violations-for v g)))))))

(test the-report-names-the-vocabulary-it-expected
  "⚠ This is why :ONE-OF is an enumeration rather than :SATISFIES a
predicate -- a predicate could only say that it returned NIL."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (let ((text (princ-to-string
               (make-condition 'value-constraint-violation
                               :class-name 'vc-doc :slot-name 'status
                               :value :nonsense :expected +vc-statuses+
                               :reason :not-in-vocabulary
                               :node-id (graph-db::gen-vertex-id)))))
    (is (search "NONSENSE" text))
    (is (search "WITHDRAWN" text)
        "the report must name the vocabulary, not merely the bad value")))
