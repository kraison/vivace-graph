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
  "Commit a VC-DOC and return it, read back through *GRAPH*."
  (declare (ignore g))
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
  (let* ((expected (graph-db::value-constraint-spec-one-of
                    (first (%vc-specs))))
         (text (princ-to-string
                (make-condition 'value-constraint-violation
                                :class-name 'vc-doc :slot-name 'status
                                :value :nonsense :expected expected
                                :reason :not-in-vocabulary
                                :node-id (graph-db::gen-vertex-id)))))
    (is (search "NONSENSE" text))
    (is (search "WITHDRAWN" text)
        "the report must name the vocabulary, not merely the bad value"))
  (%vc-clear))

(test the-report-names-the-slot-as-required-when-missing
  "The :MISSING branch of VALUE-CONSTRAINT-VIOLATION's report, untested
until now -- a :REQUIRED-only spec has EXPECTED = NIL, so the vocabulary
branch would silently name nothing (review of #149 commit 5b9a671)."
  (let ((text (princ-to-string
               (make-condition 'value-constraint-violation
                               :class-name 'vc-doc :slot-name 'status
                               :value nil :expected nil
                               :reason :missing
                               :node-id (graph-db::gen-vertex-id)))))
    (is (search "STATUS" text))
    (is (search "required" text))))

(test one-of-uses-equal-so-a-string-vocabulary-works
  "⚠ Pins value-constraint.lisp's EQUAL choice: every vocabulary elsewhere
in this file is keywords, so EQL would pass here too and the choice would
go untested (review of #149 commit 5b9a671)."
  (%vc-clear)
  (with-vc-graph (g)
    (let ((ok (%vc-make g :status "draft"))
          (bad (%vc-make g :status "nope")))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of '("draft" "final") :name vc-status)
      (is (null (%vc-violations-for ok g)))
      (is (= 1 (length (%vc-violations-for bad g)))))))

;;; --- commit-time enforcement --------------------------------------------

(test an-invalid-value-is-refused-at-commit
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (signals value-constraint-violation
      (with-transaction ()
        (make-vc-doc :status :nonsense)))
    (is (null (map-vertices #'identity g :collect-p t
                            :vertex-type 'vc-doc))
        "the refused create left nothing behind")))

(test a-valid-value-commits
  "The guard must not have been bought by refusing everything."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (finishes
      (with-transaction ()
        (make-vc-doc :status :final)))))

(test an-invalid-value-is-refused-on-the-UPDATE-path
  "⚠ THE REASON THIS UNIT EXISTS.  A construction-time check cannot see
this: COPY + SETF + SAVE never goes through the constructor (#149)."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (let ((id (with-transaction ()
                (id (make-vc-doc :status :final)))))
      (signals value-constraint-violation
        (with-transaction ()
          (let ((v (copy (lookup-vertex id))))
            (setf (vc-doc-status v) :nonsense)
            (save v))))
      (is (eq :final (vc-doc-status (lookup-vertex id)))
          "the refused update did not reach the store"))))

(test an-invalid-required-value-is-refused-on-the-UPDATE-path
  "Task 5 depends on :REQUIRED refusing a NIL through the commit path."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (let ((id (with-transaction ()
                (id (make-vc-doc :status :final)))))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :required t :name vc-status)
      (signals value-constraint-violation
        (with-transaction ()
          (let ((v (copy (lookup-vertex id))))
            (setf (vc-doc-status v) nil)
            (save v)))))))

(test a-withdrawn-constraint-stops-being-enforced
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (undef-value-constraint vc-doc :graph-db-vc-test :name vc-status)
    (finishes
      (with-transaction ()
        (make-vc-doc :status :nonsense)))))

(test deleting-a-node-is-not-blocked-by-its-own-violation
  "A delete claims nothing, exactly as in VALIDATE-UNIQUE-CONSTRAINTS --
otherwise a store holding pre-constraint damage could not be repaired."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (let ((id (with-transaction ()
                (id (make-vc-doc :status :final)))))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of '(:nothing-matches) :name vc-status)
      (finishes
        (with-transaction ()
          (mark-deleted (lookup-vertex id)))))))

;;; --- the audit pass ------------------------------------------------------

(test the-audit-pass-finds-damage-written-before-the-constraint
  "⚠ Not speculative tooling.  The probe on #149 proves invalid values are
writable today, so an existing store may already hold them -- a guard that
only protects future writes would leave that undetectable."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (multiple-value-bind (violations checked specs)
        (check-value-constraints g :vertex-type 'vc-doc)
      (is (= 1 (length violations)))
      (is (eq :not-in-vocabulary
              (graph-db:vc-violation-reason (first violations))))
      (is (= 1 checked)
          "a violation count with no population is not a result")
      (is (= 1 specs)))))

(test the-audit-pass-reports-the-population-it-checked
  "⚠ This programme's most repeated error is a count with no population.
Zero violations over zero specs is an unchecked graph, not a clean one, and
the caller must be able to tell them apart."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense))
    (multiple-value-bind (violations checked specs)
        (check-value-constraints g :vertex-type 'vc-doc)
      (is (null violations))
      (is (= 0 specs) "no constraints are declared, so nothing was checked")
      (is (= 1 checked)
          "⚠ the graph is NOT empty -- zero violations here means unchecked,
which is exactly what SPECS lets the caller tell apart"))))

(test the-audit-pass-collects-every-violation-not-only-the-first
  "⚠ Every other audit test damages exactly one node with exactly one
violated spec, so nothing would notice an audit that returned only the
first find -- which is exactly what signalling would do, hence this also
replaces a dedicated does-not-signal test (review of #149 commit 492c0ec).
Two damaged nodes, one of them doubly damaged, closes that gap."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense :note "ok"))
    (with-transaction () (make-vc-doc :status :nonsense :note nil))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (def-value-constraint vc-doc note :graph-db-vc-test
      :required t :name vc-note)
    (multiple-value-bind (violations checked specs)
        (finishes (check-value-constraints g :vertex-type 'vc-doc))
      (is (= 3 (length violations))
          "1 from the first node, 2 from the second -- collected, not
stopped at the first find")
      (is (= 2 checked))
      (is (= 2 specs)))))

(test the-audit-pass-does-not-see-a-repaired-deleted-node
  "⚠ Pins the exclusion, not just observes it.  MAP-VERTICES only skips
deleted nodes because this call omits :INCLUDE-DELETED-P; deletion is the
repair path (deleting-a-node-is-not-blocked-by-its-own-violation), so a
later change adding :INCLUDE-DELETED-P T to \"see everything\" must not
pass silently -- it would report damage on a node already repaired."
  (%vc-clear)
  (with-vc-graph (g)
    (let ((id (with-transaction ()
                (id (make-vc-doc :status :nonsense)))))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (multiple-value-bind (violations checked specs)
          (check-value-constraints g :vertex-type 'vc-doc)
        (is (null violations))
        (is (= 0 checked))
        (is (= 1 specs))))))

(test the-audit-pass-defaults-to-an-untyped-scan
  "The untyped branch is a materially different path through MAP-VERTICES
-- raw MAP-LHASH rather than the type index -- and every other audit test
passes :VERTEX-TYPE, leaving it untested."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (multiple-value-bind (violations checked specs)
        (check-value-constraints g)
      (is (= 1 (length violations)))
      (is (= 1 checked))
      (is (= 1 specs)))))

(test a-refused-commit-leaves-no-committing-transaction-behind
  "GH #150.  %COMMIT signals from inside CALL-WITH-TRANSACTION's cleanup
form, and CLEANUP-TRANSACTION came AFTER it in the same cleanup list --
so a refused commit left its transaction in the manager table in
:COMMITTING state, where MINIMUM-START-TRANSACTION-ID counts it and the
prune floor stays pinned for the life of the image.  The same path
leaked every retried VALIDATION-CONFLICT.  After the refusal nothing
must be in flight: the floor reads NIL."
  (%vc-clear)
  (with-vc-graph (g)
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (let ((tm (graph-db::transaction-manager g)))
      (signals value-constraint-violation
        (with-transaction () (make-vc-doc :status :nonsense)))
      (let ((in-flight 0))
        (graph-db::do-transactions (tx tm)
          (when (member (graph-db::state tx) '(:active :committing))
            (incf in-flight)))
        (is (zerop in-flight)
            "~D transaction(s) still in flight after a refused commit"
            in-flight))
      (is (null (graph-db::minimum-start-transaction-id tm))
          "a leaked :COMMITTING transaction pins the prune floor")
      ;; and the store still works: the refusal took nothing with it
      (finishes (with-transaction () (make-vc-doc :status :draft)))))
  (%vc-clear))

;;; --- :TRANSITION / :WRITE-ONCE and the commit view (GH #158) --------------

(defun %vc-update (node slot value)
  "COPY NODE inside a transaction, SETF SLOT to VALUE, SAVE -- the ordinary
update idiom, and the tx-update whose OLD-NODE the evaluator now sees."
  (with-transaction ()
    (let ((c (copy node)))
      (setf (slot-value c slot) value)
      (save c))))

(defun %vc-current (node)
  (lookup-vertex (id node)))

(test write-once-registers-as-a-transition-spec
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :write-once t :name vc-status-once)
  (is (= 1 (length (%vc-specs))))
  (is (eq :write-once
          (graph-db::value-constraint-spec-transition (first (%vc-specs)))))
  (%vc-clear))

(test write-once-and-transition-together-are-refused
  (signals error
    (eval '(graph-db:def-value-constraint vc-doc status :graph-db-vc-test
            :write-once t :transition vc-anything :name vc-both))))

(test a-transition-that-is-not-a-name-is-refused
  (signals error
    (graph-db::register-value-constraint-spec
     (graph-db::make-value-constraint-spec
      :owner-name 'vc-doc :slot-name 'status :graph-name :graph-db-vc-test
      :transition (lambda (o n) (declare (ignore o n)) t)
      :name 'vc-lambda))))

(test a-write-once-slot-is-settable-at-creation-and-once-after
  "GH #158.  NIL is 'not yet written': a create passes, NIL -> value
passes once, value -> value is refused, value -> NIL (clearing the audit
field) is refused, and an update to an UNRELATED slot still commits."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :write-once t :name vc-status-once)
    (let ((created (%vc-make g :status :draft :note "n")))
      (is (eq :draft (vc-doc-status created)) "a create is never refused")
      (signals value-constraint-violation
        (%vc-update created 'status :final))
      (is (eq :draft (vc-doc-status (%vc-current created)))
          "the refused write left the slot as it was")
      (signals value-constraint-violation
        (%vc-update created 'status nil))
      (finishes (%vc-update (%vc-current created) 'note "changed"))
      (is (string= "changed" (vc-doc-note (%vc-current created)))
          "an unrelated update is not a transition of STATUS"))
    (let ((blank (%vc-make g :status nil)))
      (finishes (%vc-update blank 'status :draft))
      (is (eq :draft (vc-doc-status (%vc-current blank)))
          "the first write may come after creation")
      (signals value-constraint-violation
        (%vc-update (%vc-current blank) 'status :final))))
  (%vc-clear))

(test rewriting-a-write-once-slot-to-its-own-value-is-not-a-change
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :write-once t :name vc-status-once)
    (let ((v (%vc-make g :status :draft)))
      (finishes (%vc-update v 'status :draft))))
  (%vc-clear))

(test an-unrelated-update-of-a-node-holding-an-object-is-not-a-change
  "⚠ Found by the spacetime suite: a write-once slot holding a TIMESTAMP
(or any object) reads back as a fresh instance on every deserialization,
so the pre-image and the copy are never EQUAL.  'Unchanged' must mean
the same STORED value, or every unrelated update is refused (GH #158)."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc note :graph-db-vc-test
      :write-once t :name vc-note-once)
    (let ((v (%vc-make g :status :draft
                         :note (local-time:encode-timestamp
                                0 0 0 12 1 1 2026))))
      (finishes (%vc-update v 'status :final)
                "an update that never touches NOTE must commit")
      (signals value-constraint-violation
        (%vc-update (%vc-current v) 'note
                    (local-time:encode-timestamp 0 0 0 13 1 1 2026))
        "a real change is still refused")))
  (%vc-clear))

(test a-transition-function-decides-which-changes-are-legal
  "GH #158.  :TRANSITION names an (OLD NEW) schema function; :WRITE-ONCE
is its degenerate case.  Forward-only here: draft -> final is a step,
final -> draft is not."
  (%vc-clear)
  (graph-db:register-schema-function
   'vc-forward-only
   (lambda (old new)
     (let ((order '(:draft :final :withdrawn)))
       (< (or (position old order) -1) (or (position new order) -1)))))
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :transition vc-forward-only :name vc-status-step)
    (let ((v (%vc-make g :status :draft)))
      (finishes (%vc-update v 'status :final))
      (signals value-constraint-violation
        (%vc-update (%vc-current v) 'status :draft))
      (is (eq :final (vc-doc-status (%vc-current v))))
      (let ((e (handler-case
                   (progn (%vc-update (%vc-current v) 'status :draft) nil)
                 (value-constraint-violation (e) e))))
        (is (eq :transition-refused (graph-db::vcv-reason e)))
        (is (eq 'vc-forward-only (graph-db::vcv-expected e))
            "the report names the transition that refused")
        (is (search "VC-FORWARD-ONLY" (princ-to-string e))))))
  (%vc-clear))

(test an-unresolved-transition-function-signals-at-check-time
  "As :CHECK does (GH #172): the name is resolved when a change is
evaluated, so an image that never registered it refuses loudly rather
than letting every change through."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :transition vc-never-registered :name vc-status-step)
    (let ((v (%vc-make g :status :draft)))
      (finishes (%vc-update v 'status :draft) "no change: never consulted")
      (signals graph-db::schema-function-unresolved
        (%vc-update (%vc-current v) 'status :final))))
  (%vc-clear))

(test the-audit-pass-counts-the-transitions-it-cannot-check
  "GH #158.  A transition is a fact about a change; a store-only view sees
none.  The fourth value is what stops '0 violations' reading as audited."
  (%vc-clear)
  (with-vc-graph (g)
    (def-value-constraint vc-doc status :graph-db-vc-test
      :write-once t :name vc-status-once)
    (def-value-constraint vc-doc note :graph-db-vc-test
      :required t :name vc-note)
    (%vc-make g :status :draft :note "n")
    (multiple-value-bind (violations checked specs unaudited)
        (check-value-constraints g :vertex-type 'vc-doc)
      (is (null violations))
      (is (= 1 checked))
      (is (= 2 specs))
      (is (= 1 unaudited) "one of the two specs is a transition")))
  (%vc-clear))

(test rest-put-cannot-rewrite-a-write-once-slot
  "⚠ The live hole #158 was filed for: REST-PUT-VERTEX writes raw slots
inside a transaction, past every accessor guard.  It is a TX-UPDATE like
any other, so the same transition refuses it."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :write-once t :name vc-status-once)
    (let* ((v (%vc-make g :status :draft))
           (params (list (cons "username" "u") (cons "password" "p")
                         (cons :graph-name
                               (json:lisp-to-camel-case
                                (symbol-name *vc-graph-name*)))
                         (cons :node-id (string-id (id v)))
                         (cons "status" "rewritten"))))
      (with-rest-env ()
        (signals value-constraint-violation
          (graph-db::rest-put-vertex params)))
      (is (eq :draft (vc-doc-status (%vc-current v))))))
  (%vc-clear))

