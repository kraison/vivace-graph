;;;; The source onboarding contract: declaration and validation (GH #132).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *source-graph-name* :graph-db-source-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *source-graph-name* graph-db::*schema-node-metadata*) nil))

(def-source st-report :graph-db-source-test
    ((headline :initarg :headline :accessor st-headline)
     (report-id :initarg :report-id :accessor st-report-id))
  :identity     (:namespace :st-reports :key-slot report-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC-BY-4.0" :citation "Example Reports")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(test a-source-records-all-seven-facets
  (let ((c (source-contract 'st-report)))
    (is (eq 'st-report (source-facets-class c)))
    (is (equal '(:namespace :st-reports :key-slot report-id)
               (source-facets-identity c)))
    (is (eq :none (source-facets-space c)))
    (is (eq :none (source-facets-time c)))
    (is (equal '(:class :public) (source-facets-sensitivity c)))
    (is (eq :none (source-facets-registration c)))
    (is (eq :none (source-facets-indexed-text c)))))

(test the-vocabulary-is-seven-facets
  (is (= 7 (length +source-facets+)))
  (is (= 7 (length (remove-duplicates +source-facets+)))))

(test omitting-any-facet-fails-to-expand
  "Design §2: enforcement is structural.  A non-conforming source class
cannot be defined at all, so the violation surfaces at macroexpansion."
  (dolist (omit +source-facets+)
    (let ((form `(def-source st-bad :graph-db-source-test ((a :initarg :a))
                   ,@(loop for f in +source-facets+
                           unless (eq f omit)
                             append (list f :none)))))
      (signals missing-source-facet (macroexpand-1 form)))))

(test none-is-accepted-for-every-facet
  "Design §1: the rule is uniform, with no exceptions.  One expansion with
every facet :NONE proves it for all seven at once -- looping over the facets
here would expand an identical form seven times and assert nothing extra."
  (finishes
    (macroexpand-1
     `(def-source st-allnone :graph-db-source-test ((a :initarg :a))
        ,@(loop for g in +source-facets+ append (list g :none))))))

(test a-malformed-facet-signals-and-names-the-facet
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad2 :graph-db-source-test ((a :initarg :a))
        :identity (:namespace :x)          ; missing :KEY-SLOT
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad3 :graph-db-source-test ((a :initarg :a))
        :identity :none :space :none :time :none
        :attribution (:licence "x")        ; missing :CITATION
        :sensitivity :none :registration :none :indexed-text :none))))

(test source-contract-signals-for-a-non-source
  "Design §5: \"declared nothing\" and \"is not a source\" are different
facts; NIL for both would let a consumer treat an unconverted class as a
conforming one with empty facets."
  (signals not-a-source (source-contract 'uq-claim)))
