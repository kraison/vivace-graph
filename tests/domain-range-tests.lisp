;;;; Domain and range: declaration, the other endpoint through the commit
;;;; view, dangling vs wrong-typed, the audit pass.
;;;; Design: docs/superpowers/specs/2026-08-31-ontology-evaluator-design.md
;;;; §3 (GH #156, unit 3 of #109).

(in-package #:graph-db/test)

(in-suite graph-db-suite)

(defparameter *dr-graph-name* :graph-db-dr-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *dr-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex dr-site () ((name :initarg :name :accessor dr-site-name))
  :graph-db-dr-test)
(def-vertex dr-field-site (dr-site) () :graph-db-dr-test)   ; a subtype
(def-vertex dr-person () ((name :initarg :name :accessor dr-person-name))
  :graph-db-dr-test)
(def-edge dr-surveyed () () :graph-db-dr-test)              ; person -> site
(def-edge dr-surveyed-sub (dr-surveyed) () :graph-db-dr-test)

(defmacro with-dr-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *dr-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %dr-specs ()
  (gethash *dr-graph-name* graph-db::*schema-domain-range-metadata*))

(defun %dr-clear ()
  (setf (gethash *dr-graph-name* graph-db::*schema-domain-range-metadata*)
        nil))

(defun %dr-violation (thunk)
  "The DOMAIN-RANGE-VIOLATION THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (domain-range-violation (e) e)))

;;; --- declaration ---------------------------------------------------------

(test domain-range-declaration-registers-one-spec
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test
    :domain dr-person :range dr-site :name surveyed-endpoints)
  (is (= 1 (length (%dr-specs))))
  (is (equal '(dr-person)
             (graph-db::domain-range-spec-domain (first (%dr-specs)))))
  (is (equal '(dr-site)
             (graph-db::domain-range-spec-range (first (%dr-specs)))))
  (is-true (undef-domain-range dr-surveyed :graph-db-dr-test
                               :name surveyed-endpoints))
  (is (null (%dr-specs))))

(test an-unnamed-domain-range-is-one-per-edge-type
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test :range dr-site)
  (def-domain-range dr-surveyed :graph-db-dr-test :range dr-person)
  (is (= 1 (length (%dr-specs))) "replaced, not stacked")
  (is-true (undef-domain-range dr-surveyed :graph-db-dr-test))
  (is (null (%dr-specs))))

(test a-domain-range-that-constrains-neither-end-is-refused
  (%dr-clear)
  (signals error
    (eval '(graph-db:def-domain-range dr-surveyed :graph-db-dr-test
            :name dr-nothing)))
  (is (null (%dr-specs))))

;;; --- commit-time enforcement ------------------------------------------

(test a-wrong-typed-endpoint-is-refused-and-named
  "GH #156.  The discriminating case: a correctly typed pair passes
whether or not the range is checked, so the wrong-typed one must be shown
to go red.  PERSON -> PERSON under a PERSON -> SITE rule."
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test
    :domain dr-person :range dr-site :name surveyed-endpoints)
  (with-dr-graph (g)
    (declare (ignorable g))
    (let (p q s)
      (with-transaction ()
        (setq p (make-dr-person :name "p") q (make-dr-person :name "q")
              s (make-dr-site :name "s")))
      (finishes (with-transaction () (make-dr-surveyed :from p :to s)))
      (let ((e (%dr-violation
                (lambda ()
                  (with-transaction () (make-dr-surveyed :from p :to q))))))
        (is-true e "PERSON as the TO end is refused")
        (is (eq :wrong-type (drv-reason e)))
        (is (eq :to (drv-end e)))
        (is (eq 'dr-person (drv-actual e))))
      (let ((e (%dr-violation
                (lambda ()
                  (with-transaction () (make-dr-surveyed :from s :to s))))))
        (is-true e "SITE as the FROM end is refused")
        (is (eq :from (drv-end e))))
      (is (= 1 (length (outgoing-edges (lookup-vertex (id p))
                                       :edge-type 'dr-surveyed)))
          "only the legal edge exists")))
  (%dr-clear))

(test a-dangling-endpoint-is-a-different-failure-from-a-wrong-typed-one
  "GH #156.  MAKE-EDGE takes a raw id without checking it exists, so a
reference to nothing is reachable -- and it is :DANGLING, not
:WRONG-TYPE: absence and a value are kept apart."
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test
    :range dr-site :name surveyed-endpoints)
  (with-dr-graph (g)
    (declare (ignorable g))
    (let ((p (with-transaction () (make-dr-person :name "p")))
          (nowhere (graph-db::gen-vertex-id)))
      (let ((e (%dr-violation
                (lambda ()
                  (with-transaction ()
                    (make-dr-surveyed :from p :to nowhere))))))
        (is-true e)
        (is (eq :dangling (drv-reason e)))
        (is (eq :to (drv-end e)))
        (is (null (drv-actual e))))))
  (%dr-clear))

(test an-endpoint-created-in-the-same-commit-is-found-in-the-writes
  "GH #156, the ordinary case the note calls out: a commit that creates
both endpoints AND the edge.  The store has neither vertex yet; the view
answers from the writes.  Without the overlay this would read as
:DANGLING and reject legitimate writes."
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test
    :domain dr-person :range dr-site :name surveyed-endpoints)
  (with-dr-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (let ((p (make-dr-person :name "p")) (s (make-dr-site :name "s")))
          (make-dr-surveyed :from p :to s))))
    (let ((e (%dr-violation
              (lambda ()
                (with-transaction ()
                  (let ((p (make-dr-person :name "p2"))
                        (q (make-dr-person :name "q2")))
                    (make-dr-surveyed :from p :to q)))))))
      (is-true e "and a wrong-typed endpoint created in the same commit ~
is still wrong-typed")
      (is (eq :wrong-type (drv-reason e)))))
  (%dr-clear))

(test a-subtype-endpoint-is-admitted-and-a-subtype-edge-inherits-the-rule
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test
    :domain dr-person :range dr-site :name surveyed-endpoints)
  (with-dr-graph (g)
    (declare (ignorable g))
    (let (p fs)
      (with-transaction ()
        (setq p (make-dr-person :name "p")
              fs (make-dr-field-site :name "fs")))
      (finishes (with-transaction () (make-dr-surveyed :from p :to fs))
                "a FIELD-SITE is a SITE")
      (finishes (with-transaction () (make-dr-surveyed-sub :from p :to fs))
                "the subtype edge is admitted too")
      (is-true (%dr-violation
                (lambda ()
                  (with-transaction () (make-dr-surveyed-sub :from p :to p))))
               "and the subtype edge inherits the rule")))
  (%dr-clear))

(test an-endpoint-deleted-in-the-same-commit-is-dangling
  "The view's other edge: a vertex this commit deletes is absent to it,
so an edge pointed at it in the same commit dangles."
  (%dr-clear)
  (def-domain-range dr-surveyed :graph-db-dr-test
    :range dr-site :name surveyed-endpoints)
  (with-dr-graph (g)
    (declare (ignorable g))
    (let (p s)
      (with-transaction ()
        (setq p (make-dr-person :name "p") s (make-dr-site :name "s")))
      (let ((e (%dr-violation
                (lambda ()
                  (with-transaction ()
                    (mark-deleted (lookup-vertex (id s)))
                    (make-dr-surveyed :from p :to s))))))
        (is-true e)
        (is (eq :dangling (drv-reason e))))))
  (%dr-clear))

(test the-report-names-the-end-and-the-classes
  (let ((wrong (princ-to-string
                (make-condition 'domain-range-violation
                                :edge-type 'dr-surveyed :end :to
                                :reason :wrong-type :actual 'dr-person
                                :expected '(dr-site)
                                :node-id (graph-db::gen-edge-id)
                                :endpoint-id (graph-db::gen-vertex-id))))
        (gone (princ-to-string
               (make-condition 'domain-range-violation
                               :edge-type 'dr-surveyed :end :from
                               :reason :dangling :actual nil
                               :expected '(dr-person)
                               :node-id (graph-db::gen-edge-id)
                               :endpoint-id (graph-db::gen-vertex-id)))))
    (is (search "to endpoint is a DR-PERSON" wrong))
    (is (search "DR-SITE" wrong))
    (is (search "from endpoint" gone))
    (is (search "does not exist" gone))))

;;; --- the audit pass -----------------------------------------------------

(test the-domain-range-audit-finds-pre-existing-violations
  (%dr-clear)
  (with-dr-graph (g)
    (let (p q s)
      (with-transaction ()
        (setq p (make-dr-person :name "p") q (make-dr-person :name "q")
              s (make-dr-site :name "s"))
        (make-dr-surveyed :from p :to s)
        (make-dr-surveyed :from p :to q))        ; legal: no rule yet
      (multiple-value-bind (violations checked specs)
          (check-domain-range-constraints g :edge-type 'dr-surveyed)
        (is (null violations))
        (is (= 2 checked))
        (is (zerop specs) "zero specs: unchecked, not clean"))
      (def-domain-range dr-surveyed :graph-db-dr-test
        :domain dr-person :range dr-site :name surveyed-endpoints)
      (multiple-value-bind (violations checked specs)
          (check-domain-range-constraints g :edge-type 'dr-surveyed)
        (is (= 1 (length violations)))
        (is (eq :wrong-type (graph-db::dr-violation-reason
                             (first violations))))
        (is (equalp (id q) (graph-db::dr-violation-endpoint-id
                            (first violations))))
        (is (= 2 checked))
        (is (= 1 specs)))))
  (%dr-clear))
