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

(test explicit-nil-differs-from-omission-for-a-facet
  "Design §1: the whole subtlety of DEF-SOURCE is telling a facet that was
never mentioned apart from one explicitly given as NIL -- that is why the
macro checks the &KEY supplied-p variables rather than the facet values.
An explicit NIL is a (malformed) value and signals INVALID-SOURCE-FACET;
an omitted facet was never given a value at all and signals
MISSING-SOURCE-FACET.  A 'simplification' that tested values instead of
supplied-p would collapse this distinction while still passing every other
test in the suite."
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad4 :graph-db-source-test ((a :initarg :a))
        :identity nil
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals missing-source-facet
    (macroexpand-1
     `(def-source st-bad5 :graph-db-source-test ((a :initarg :a))
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none))))

(test source-contract-signals-for-a-non-source
  "Design §5: \"declared nothing\" and \"is not a source\" are different
facts; NIL for both would let a consumer treat an unconverted class as a
conforming one with empty facets."
  (signals not-a-source (source-contract 'uq-claim)))

(defmacro with-source-graph ((g) &body body)
  "A fresh on-disk graph named *SOURCE-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *source-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(def-source st-photo :graph-db-source-test
    ((sha :initarg :sha :accessor st-sha))
  :identity     (:namespace :st-media :key-slot sha)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  (:class :restricted)
  :registration :none
  :indexed-text :none)

(def-source st-note :graph-db-source-test
    ((body :initarg :body :accessor st-body))
  :identity     :none
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

(def-source st-clip :graph-db-source-test
    ((clip-id :initarg :clip-id :accessor st-clip-id))
  :identity     (:namespace :st-media :key-slot clip-id)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

(test identity-registers-the-class-under-its-namespace
  (is (member 'st-report (namespace-sources :st-reports)))
  (is (member 'st-photo (namespace-sources :st-media)))
  (is-false (member 'st-photo (namespace-sources :st-reports))))

(test identity-registration-accumulates-within-a-namespace
  "Task-2 review finding 1: ST-PHOTO and ST-CLIP share :ST-MEDIA.  A SETF
that overwrote instead of PUSHNEW-ing would leave only the most recently
loaded class registered, and every other test here stays green regardless
-- Task 4 depends on multiple classes sharing one namespace, so accumulation
must hold now, proven directly rather than incidentally."
  (is (member 'st-photo (namespace-sources :st-media)))
  (is (member 'st-clip (namespace-sources :st-media))))

(test identity-none-registers-nothing
  "Plan clarification: :IDENTITY :NONE means records of this class are never
endpoint targets.  It is legal, and it registers no namespace."
  (dolist (ns '(:st-reports :st-media))
    (is-false (member 'st-note (namespace-sources ns)))))

(test identity-none-emits-no-index
  "Task-2 review finding 2: the other half of :IDENTITY :NONE is no index,
not just no namespace entry.  Asserts the SIGNAL, not an empty result --
an empty result is also what a declared-but-empty index returns, which is
a different state from no index at all (INDEX-LOOKUP / %REQUIRE-INDEX)."
  (with-source-graph (g)
    (signals error (graph-db:index-lookup g 'st-note '(body) "x"))))

(test an-unregistered-namespace-signals
  "Design §4: an unknown namespace is a programming error, distinct from a
key that simply matches nothing."
  (signals unknown-namespace (namespace-sources :st-no-such)))

(test identity-emits-an-index-on-the-key-slot
  "Without this index RESOLVE-ENDPOINT would be a full scan.  Proved by
using the index rather than by inspecting the registry: INDEX-LOOKUP signals
if no index covers the class and slot, so a successful call IS the evidence."
  (with-source-graph (g)
    (with-transaction () (make-st-report :headline "x" :report-id "idx-1"))
    (is (= 1 (length (graph-db:index-lookup g 'st-report '(report-id)
                                            "idx-1"))))))

(test identity-registration-is-idempotent
  "Task-2 review finding 3: PUSHNEW makes re-evaluating a DEF-SOURCE form
safe.  Calls %REGISTER-IDENTITY directly -- the property under test is
registry accumulation, not macroexpansion, so re-expanding a whole
DEF-SOURCE would add nothing."
  (graph-db.spacetime::%register-identity
   'st-report '(:namespace :st-reports :key-slot report-id))
  (graph-db.spacetime::%register-identity
   'st-report '(:namespace :st-reports :key-slot report-id))
  (is (= 1 (count 'st-report (namespace-sources :st-reports)))))
