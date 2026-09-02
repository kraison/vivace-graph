;;;; Endpoint resolution, and the fail-closed sensitivity predicate (#132).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

;; WITH-SOURCE-GRAPH, *SOURCE-GRAPH-NAME*, and the ST-* source classes come
;; from source-tests.lisp, which loads before this file (graph-db.asd).

(test resolve-endpoint-finds-a-record-by-external-key
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "one" :report-id "r-1")
      (make-st-report :headline "two" :report-id "r-2"))
    (let ((n (resolve-endpoint :st-reports "r-2")))
      (is-true n)
      (is (string= "two" (st-headline n))))))

(test a-key-that-matches-nothing-returns-nil
  "Distinct from an unknown namespace, which signals (design §4)."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "r-1"))
    (is (null (resolve-endpoint :st-reports "r-99")))))

(test an-unknown-namespace-signals-rather-than-returning-nil
  "Collapsing both to NIL would make a misspelled namespace
indistinguishable from an absent record."
  (with-source-graph (g)
    (declare (ignorable g))
    (signals unknown-namespace (resolve-endpoint :st-nope "r-1"))))

(test resolve-endpoint-refuses-to-run-in-a-read-write-transaction
  "Design §4.1: resolution can cross graphs, and the 3.0 contract permits
cross-graph reads only from a read-only snapshot or outside a transaction.
The caller's mistake is the call site, not the lookup."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "r-1"))
    (signals resolution-in-transaction
      (with-transaction () (resolve-endpoint :st-reports "r-1")))))

(test two-classes-answering-one-key-signals-and-names-both
  "Design §4.2: DEF-UNIQUE cannot catch this -- the classes have different
owners and the constraint registry keys on owner."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "r" :report-id "shared")
      (make-st-summary :topic "s" :summary-id "shared"))
    (handler-case (progn (resolve-endpoint :st-reports "shared")
                         (is-true nil "expected AMBIGUOUS-ENDPOINT"))
      (ambiguous-endpoint (c)
        (is (= 2 (length (ambiguous-endpoint-classes c))))))))

(test distinct-keys-in-a-shared-namespace-still-resolve
  "Sharing a namespace is legal; only a shared KEY is a violation."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "r" :report-id "k-1")
      (make-st-summary :topic "s" :summary-id "k-2"))
    (is (string= "r" (st-headline (resolve-endpoint :st-reports "k-1"))))
    (is (string= "s" (st-topic (resolve-endpoint :st-reports "k-2"))))))

(test def-source-makes-a-same-class-duplicate-key-unrepresentable
  "Finding 3, layer 1 (S1c review).  Before the fix, two ST-REPORT records
sharing one REPORT-ID resolved silently to whichever was created first --
the ambiguity check only ever compared how many CLASSES answered, and both
hits here come from one.  DEF-SOURCE now emits a DEF-UNIQUE on the key
slot, so the second CREATE cannot even commit."
  (with-source-graph (g)
    (declare (ignorable g))
    (signals graph-db:unique-constraint-violation
      (with-transaction ()
        (make-st-report :headline "one" :report-id "dup")
        (make-st-report :headline "two" :report-id "dup")))))

(test resolve-endpoint-guards-a-same-class-multi-hit-directly
  "Finding 3, layer 2 (S1c review).  DEF-UNIQUE (layer 1, above) protects
only prospectively: it exempts any tuple with a NULL component outright,
and cannot retroactively catch data written before it existed, which a
later TOLERANT re-open leaves untouched (DEF-UNIQUE's own docstring,
unique-constraint.lisp) -- so GRAPH-DB:INDEX-LOOKUP can still, in
principle, return more than one hit for ONE class.  A single-slot
secondary index does not retain a NULL-keyed row at all
(%INDEX-TUPLE-KEY, index.lisp -- confirmed directly: a NIL REPORT-ID never
reaches the index to begin with), so that specific case cannot be driven
through the public API here.  This exercises RESOLVE-ENDPOINT's guard
directly, by making INDEX-LOOKUP answer as stale duplicate data would."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "solo"))
    (let ((real (fdefinition 'graph-db:index-lookup)))
      (unwind-protect
          (progn
            (setf (fdefinition 'graph-db:index-lookup)
                  (lambda (graph class-name slot-name value
                          &key collect-p prefix)
                    (declare (ignore graph class-name slot-name value
                                     collect-p prefix))
                    (list :stale-hit-1 :stale-hit-2)))
            (handler-case (progn (resolve-endpoint :st-reports "solo")
                                 (is-true nil "expected AMBIGUOUS-ENDPOINT"))
              (ambiguous-endpoint (c)
                (is (equal '(st-report) (ambiguous-endpoint-classes c))))))
        (setf (fdefinition 'graph-db:index-lookup) real)))))

(test resolve-endpoint-signals-a-clear-condition-for-an-unopened-graph
  "Finding 4 (S1c review).  Before the fix, a registered class whose graph
is not open made RESOLVE-ENDPOINT pass NIL into GRAPH-DB:INDEX-LOOKUP,
failing at a low level.  ST-ELSEWHERE's graph is never opened by
WITH-SOURCE-GRAPH, so it stands in for that case; the condition must name
the class and the graph, not just crash."
  (with-source-graph (g)
    (declare (ignorable g))
    (handler-case (progn (resolve-endpoint :st-elsewhere-ns "whatever")
                         (is-true nil "expected UNOPENED-SOURCE-GRAPH"))
      (unopened-source-graph (c)
        (is (eq 'st-elsewhere (unopened-source-graph-class c)))
        (is (eq :graph-db-source-test-elsewhere
               (unopened-source-graph-graph-name c)))))))

(test inheriting-a-source-with-one-record-resolves-cleanly
  "GH #132 review, gap 2 (25ffa22).  GRAPH-DB:INDEX-LOOKUP matches a class and
its
subclasses (index.lisp), so ST-PSRC's own index-lookup call and ST-CSRC's
own both find the SAME physical ST-CSRC record.  Before the fix, that made
RESOLVE-ENDPOINT see two classes answering and signal AMBIGUOUS-ENDPOINT
for a namespace that has, in truth, exactly one record -- permanently
unresolvable, since nothing about the data ever changes that count.

Runs with the node cache OFF, which is what makes the test discriminate.
Cached, both lookups return one shared instance and even an EQL de-dup
passes; uncached, LOOKUP-NODE builds a fresh node per call and the two
ids are EQUALP but not EQL.  De-duplicating by anything narrower than
EQUALP fails here and only here."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-csrc :pid "inh-solo"))
    (let ((graph-db::*cache-enabled* nil))
      (let ((n (resolve-endpoint :st-inherited "inh-solo")))
        (is-true n)
        (is (string= "inh-solo" (st-psrc-pid n)))))))

(test resolve-endpoint-guards-cross-class-ambiguity-after-dedup
  "GH #132 review, gap 2 (25ffa22), the other half: de-duplicating HITS by node
id
must not swallow genuine ambiguity -- two DIFFERENT physical records
answering under two different classes.  A live ST-PSRC/ST-CSRC pair
cannot drive this through the public API: DEF-UNIQUE turns out to make a
subtype honour its ancestor's own uniqueness too (the same inheritance-
awareness FIX 2 corrects on the read side), so creating both records
signals UNIQUE-CONSTRAINT-VIOLATION before RESOLVE-ENDPOINT ever runs.
So, as the layer-2 test above does, this exercises the guard directly by
making INDEX-LOOKUP answer with two real, but distinct, nodes."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-psrc :pid "solo-a")
      (make-st-csrc :pid "solo-b"))
    (let ((real (fdefinition 'graph-db:index-lookup))
          (a (first (graph-db:index-lookup g 'st-psrc '(pid) "solo-a")))
          (b (first (graph-db:index-lookup g 'st-csrc '(pid) "solo-b"))))
      (unwind-protect
          (progn
            (setf (fdefinition 'graph-db:index-lookup)
                  (lambda (graph class-name slot-name value
                          &key collect-p prefix)
                    (declare (ignore graph slot-name value
                                     collect-p prefix))
                    (list (if (eq class-name 'st-psrc) a b))))
            (handler-case
                (progn (resolve-endpoint :st-inherited "whatever")
                       (is-true nil "expected AMBIGUOUS-ENDPOINT"))
              (ambiguous-endpoint (c)
                (is (= 2 (length (ambiguous-endpoint-classes c)))))))
        (setf (fdefinition 'graph-db:index-lookup) real)))))

(test source-disclosable-p-is-fail-closed
  "Design §3.2.  An unrecognised class, and :NONE, are treated as MORE
restricted than every known one -- never less.  If this test is ever
inverted the facet becomes worse than nothing, because a caller trusts it."
  (is-true (source-disclosable-p :public :public))
  (is-true (source-disclosable-p :public :restricted))
  (is-false (source-disclosable-p :restricted :public))
  (is-true (source-disclosable-p :restricted :restricted))
  (is-false (source-disclosable-p :no-such-class :restricted))
  (is-false (source-disclosable-p :none :restricted))
  (is-false (source-disclosable-p :public :no-such-clearance)))
