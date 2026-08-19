;;;; The source onboarding contract: declaration and validation (GH #132).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *source-graph-name* :graph-db-source-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *source-graph-name* graph-db::*schema-node-metadata*) nil)
  ;; Finding 4 (S1c review): ST-ELSEWHERE below declares this graph but
  ;; nothing ever opens it -- WITH-SOURCE-GRAPH only opens
  ;; *SOURCE-GRAPH-NAME*, above -- so RESOLVE-ENDPOINT must find it
  ;; genuinely unopened, not merely never-cleared from a prior run.
  (setf (gethash :graph-db-source-test-elsewhere
                 graph-db::*schema-node-metadata*)
        nil))

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

(test a-plist-checked-by-position-rejects-a-value-only-match
  "Finding 1 (S1c review).  %PLIST-HAS-P used MEMBER over the whole list,
so a facet's own VALUE could stand in for a missing KEY.  Both forms below
were accepted before the fix: the first registered ST-BAD6 under namespace
:KEY-SLOT with a NIL key slot; the second could never resolve, silently,
because *NAMESPACE-SOURCES* is an EQ table and \"reports\" is a string."
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad6 :graph-db-source-test ((a :initarg :a))
        :identity (:namespace :key-slot)   ; no :KEY-SLOT key at all
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad7 :graph-db-source-test ((a :initarg :a))
        :identity (:namespace "reports" :key-slot a) ; string, not keyword
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none))))

(test each-facets-sub-keys-are-type-checked
  "Finding 1 (S1c review): rigour applied uniformly, not just to :IDENTITY."
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad8 :graph-db-source-test ((a :initarg :a))
        :identity :none
        :space (:geometry-slot "not-a-symbol" :kind :point :precision :city)
        :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad9 :graph-db-source-test ((a :initarg :a))
        :identity :none :space :none
        :time (:extent-fn "not-a-symbol")
        :attribution :none
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad10 :graph-db-source-test ((a :initarg :a))
        :identity :none :space :none :time :none
        :attribution (:licence :not-a-string :citation "x")
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad11 :graph-db-source-test ((a :initarg :a))
        :identity :none :space :none :time :none :attribution :none
        :sensitivity (:class "not-a-keyword")
        :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad12 :graph-db-source-test ((a :initarg :a))
        :identity :none :space :none :time :none :attribution :none
        :sensitivity :none :registration :none
        :indexed-text (:text-fn 42)))))

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

(test re-registering-with-a-changed-namespace-updates-both-registries
  "Finding 2 (S1c review).  Before the fix, *SOURCE-CONTRACTS* is
overwritten by SETF but *NAMESPACE-SOURCES* only ever PUSHNEW-s, so
editing a class's :NAMESPACE and re-evaluating DEF-SOURCE -- ordinary
practice -- left it registered under the OLD namespace forever, and
RESOLVE-ENDPOINT would keep answering for a namespace no class declares."
  (eval '(def-source st-movable :graph-db-source-test
          ((k :initarg :k :accessor st-movable-k))
          :identity     (:namespace :st-movable-a :key-slot k)
          :space        :none
          :time         :none
          :attribution  :none
          :sensitivity  :none
          :registration :none
          :indexed-text :none))
  (is (member 'st-movable (namespace-sources :st-movable-a)))
  (eval '(def-source st-movable :graph-db-source-test
          ((k :initarg :k :accessor st-movable-k))
          :identity     (:namespace :st-movable-b :key-slot k)
          :space        :none
          :time         :none
          :attribution  :none
          :sensitivity  :none
          :registration :none
          :indexed-text :none))
  (is (member 'st-movable (namespace-sources :st-movable-b)))
  ;; The old namespace has nothing left declaring it: NAMESPACE-SOURCES
  ;; signals rather than returning a stale, now-meaningless list.
  (signals unknown-namespace (namespace-sources :st-movable-a)))

(test re-registering-to-identity-none-clears-the-old-namespace
  "Finding 2 (S1c review), the second half: changing a class's :IDENTITY
to :NONE must also clear its old namespace entry -- before the fix, the
stale entry not only lingered, RESOLVE-ENDPOINT would then hit it and
evaluate (GETF :NONE :KEY-SLOT), a raw TYPE-ERROR rather than a
SPACETIME-ERROR."
  (eval '(def-source st-orphanable :graph-db-source-test
          ((k :initarg :k :accessor st-orphanable-k))
          :identity     (:namespace :st-orphanable :key-slot k)
          :space        :none
          :time         :none
          :attribution  :none
          :sensitivity  :none
          :registration :none
          :indexed-text :none))
  (is (member 'st-orphanable (namespace-sources :st-orphanable)))
  (eval '(def-source st-orphanable :graph-db-source-test
          ((k :initarg :k :accessor st-orphanable-k))
          :identity     :none
          :space        :none
          :time         :none
          :attribution  :none
          :sensitivity  :none
          :registration :none
          :indexed-text :none))
  (signals unknown-namespace (namespace-sources :st-orphanable)))

;; Deliberately shares :ST-REPORTS with ST-REPORT, to exercise §4.2.
(def-source st-summary :graph-db-source-test
    ((topic :initarg :topic :accessor st-topic)
     (summary-id :initarg :summary-id :accessor st-summary-id))
  :identity     (:namespace :st-reports :key-slot summary-id)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

;; Finding 5 (S1c review): DEF-SOURCE hardcoded () for DEF-VERTEX's
;; PARENT-TYPES, so no source class could inherit.  ST-BASE-THING is a
;; plain (non-source) vertex; ST-DERIVED is a source that inherits it.
(graph-db:def-vertex st-base-thing ()
    ((label :initarg :label :accessor st-base-label))
  :graph-db-source-test)

(def-source st-derived :graph-db-source-test
    ((extra :initarg :extra :accessor st-derived-extra))
  :parent-types (st-base-thing)
  :identity     (:namespace :st-derived :key-slot extra)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

;; Finding 4 (S1c review): a source class in a graph that WITH-SOURCE-GRAPH
;; never opens, so RESOLVE-ENDPOINT must meet a genuinely unopened graph.
(def-source st-elsewhere :graph-db-source-test-elsewhere
    ((eid :initarg :eid :accessor st-elsewhere-id))
  :identity     (:namespace :st-elsewhere-ns :key-slot eid)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

(test a-source-class-can-inherit-from-a-parent-vertex-type
  "Finding 5 (S1c review): PARENT-TYPES now reaches DEF-VERTEX, so
ST-DERIVED is a genuine subtype of ST-BASE-THING and inherits its slot."
  (is (subtypep 'st-derived 'st-base-thing))
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((n (make-st-derived :label "l" :extra "e9")))
        (is (string= "l" (st-base-label n)))
        (is (string= "e9" (st-derived-extra n)))))))

;; Fix 2 (fix-wave review): ST-PSRC and ST-CSRC both a source, ST-CSRC
;; inheriting ST-PSRC and sharing its namespace.  GRAPH-DB:INDEX-LOOKUP
;; matches a class and its subclasses (index.lisp), so one physical
;; ST-CSRC record answers under both class names -- resolve-tests.lisp
;; proves RESOLVE-ENDPOINT must not treat that as ambiguity.
(def-source st-psrc :graph-db-source-test
    ((pid :initarg :pid :accessor st-psrc-pid))
  :identity     (:namespace :st-inherited :key-slot pid)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

(def-source st-csrc :graph-db-source-test
    ()
  :parent-types (st-psrc)
  :identity     (:namespace :st-inherited :key-slot pid)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

(test a-source-class-can-inherit-from-a-source-parent
  (is (subtypep 'st-csrc 'st-psrc))
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((n (make-st-csrc :pid "inh-1")))
        (is (string= "inh-1" (st-psrc-pid n)))))))

(test a-keyword-key-slot-signals-and-names-the-mistake
  "Fix 1 (fix-wave review).  :KEY-SLOT :PID passed the old REQ-SYMBOL
check -- a non-NIL symbol -- because keywords are symbols too.  The class
defined, records wrote, and RESOLVE-ENDPOINT returned NIL for the life of
the class, because keyword :PID never matches slot name PID.  This is the
likeliest slip a user makes, and must be caught at macroexpansion."
  (handler-case
      (progn
        (macroexpand-1
         `(def-source st-badkw :graph-db-source-test
              ((pid :initarg :pid :accessor st-badkw-pid))
            :identity (:namespace :st-badkw-ns :key-slot :pid) ; keyword!
            :space :none :time :none :attribution :none
            :sensitivity :none :registration :none :indexed-text :none))
        (is-true nil "expected INVALID-SOURCE-FACET"))
    (invalid-source-facet (c)
      (is (search "keyword"
                  (graph-db.spacetime::invalid-source-facet-reason c))))))

(test a-malformed-plist-signals-instead-of-a-raw-type-error
  "Fix 3 (fix-wave review).  A dropped value -- the likeliest typo -- left
an odd-length plist, which reached SBCL's malformed-property-list error
straight out of GETF.  Every other malformed declaration in this unit
signals INVALID-SOURCE-FACET; this one must too."
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-baddrop :graph-db-source-test ((a :initarg :a))
        :identity (:namespace :st-baddrop-ns :key-slot) ; dropped value
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none))))

(test a-registration-facet-declares-what-it-binds-to
  (finishes
    (graph-db.spacetime::%check-facet
     :registration
     '(:registry ct-region :registry-namespace "reg"
       :relation "registered-at" :method "centroid-within"
       :rule-version "r/1" :precision-fn nil
       :confidence-fn nil))))

(test registration-none-stays-supported
  "The map-less tenant declares :NONE and is what proves the spatial
facets are optional rather than merely defaulted (design §3)."
  (finishes (graph-db.spacetime::%check-facet :registration :none)))

(test a-registration-facet-missing-its-registry-is-refused
  (signals invalid-source-facet
    (graph-db.spacetime::%check-facet
     :registration
     '(:relation "registered-at" :method "centroid-within"
       :rule-version "r/1"))))
