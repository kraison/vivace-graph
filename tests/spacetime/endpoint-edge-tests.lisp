;;;; Edges under claims, U1 (GH #369, spec 2026-09-12 sec.3-4, 6, 11).
;;;;
;;;; One store holds the sources AND the claims here: that is the shape
;;;; the consumer runs (spec sec.10) and the only one U1 links in.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *ee-graph-name* :graph-db-ee-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ee-graph-name* graph-db::*schema-node-metadata*) nil))

(def-source ee-thing :graph-db-ee-test
    ((label :initarg :label :accessor ee-label)
     (thing-id :initarg :thing-id :accessor ee-thing-id))
  :identity     (:namespace :ee-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "EE fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

;; A second source class in the SAME namespace: two records under one
;; key are the ambiguity case of spec sec.4.1 step 3.
(def-source ee-twin :graph-db-ee-test
    ((twin-id :initarg :twin-id :accessor ee-twin-id))
  :identity     (:namespace :ee-things :key-slot twin-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "EE fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(def-claim-classes ee-claim :graph-db-ee-test)

(defmacro with-ee-graph ((g) &body body)
  "A fresh on-disk graph named *EE-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ee-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun ee-b (&key (subject "t-1") (object "t-2") (relation "r")
                  (producer "p") (subject-namespace :ee-things)
                  (object-namespace :ee-things) subject-node object-node)
  "A binary EE-CLAIM.  NIL for a -NODE key means 'not given'."
  (make-ee-claim-binary :subject-namespace subject-namespace
                        :subject-key subject :relation relation
                        :object-namespace object-namespace
                        :object-key object :producer producer
                        :standing :inferred
                        :subject-node subject-node
                        :object-node object-node))

(defun ee-thing (id)
  (make-ee-thing :label id :thing-id id))

(defun same-node-p (a b)
  (and a b (equalp (id a) (id b))))

(test edge-classes-exist-with-no-default-store
  "Spec sec.3: shipped, slotless, placed only by :GRAPH."
  (is-true (find-class 'subject-of nil))
  (is-true (find-class 'object-of nil))
  (is-true (subtypep 'subject-of 'graph-db:edge))
  (is-true (fboundp 'make-subject-of))
  (is-true (fboundp 'make-object-of))
  (signals graph-db:default-store-not-open-error
    (make-subject-of :from nil :to nil))
  (signals graph-db:default-store-not-open-error
    (make-object-of :from nil :to nil)))

(test edge-classes-are-placed-by-graph-in-any-store
  "The claim's store adopts the type lazily on first write (#167 R3)."
  (with-ee-graph (g)
    (let (c n)
      (with-transaction ()
        (setq n (ee-thing "t-1"))
        (setq c (ee-b :subject "nope" :object "nope-either"))
        (make-subject-of :from c :to n :graph g))
      (is (= 1 (length (graph-db:outgoing-edges
                        c :graph g :edge-type 'subject-of))))
      (is-true (graph-db:lookup-node-type-by-name 'subject-of :edge
                                                  :graph g)))))

(defun ee-linked-to (claim edge-type g)
  "The id the CLAIM's EDGE-TYPE edge points at, or NIL."
  (let ((e (first (graph-db:outgoing-edges claim :graph g
                                                 :edge-type edge-type))))
    (and e (graph-db:to e))))

(test auto-link-in-two-transactions
  "Spec sec.4.1: sources committed first, the claim later; both link."
  (with-ee-graph (g)
    (let (s o c)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction () (setq c (ee-b)))
      (is (equalp (id s) (ee-linked-to c 'subject-of g)))
      (is (equalp (id o) (ee-linked-to c 'object-of g))))))

(test auto-link-in-one-transaction
  "Spec sec.4.1 'Same-transaction visibility': the index cannot see the
sources yet; the commit-view overlay must."
  (with-ee-graph (g)
    (let (s o c)
      (with-transaction ()
        (setq s (ee-thing "t-1") o (ee-thing "t-2"))
        (setq c (ee-b)))
      (is (equalp (id s) (ee-linked-to c 'subject-of g)))
      (is (equalp (id o) (ee-linked-to c 'object-of g))))))

(test key-only-when-nothing-resolves
  "An unknown namespace, and a known one with no such key, both leave
the claim key-only and legal; CLAIMS-TOUCHING still finds it."
  (with-ee-graph (g)
    (let (s c1 c2)
      (with-transaction () (setq s (ee-thing "t-1")))
      (with-transaction ()
        (setq c1 (ee-b :object-namespace :ee-nowhere :object "x"))
        (setq c2 (ee-b :object "t-missing")))
      (is (equalp (id s) (ee-linked-to c1 'subject-of g)))
      (is (null (ee-linked-to c1 'object-of g)))
      (is (null (ee-linked-to c2 'object-of g)))
      (is (= 2 (length (claims-touching g 'ee-claim :ee-things "t-1"
                                        :role :subject)))))))

(test ambiguity-warns-once-and-writes-unlinked
  "Spec sec.4.1 step 3 / sec.7: two candidates -> no edge, one
ENDPOINT-LINK-SKIPPED, the write commits."
  (with-ee-graph (g)
    (let ((warned 0) c)
      (with-transaction ()
        (ee-thing "dup")
        (make-ee-twin :twin-id "dup"))
      (handler-bind ((endpoint-link-skipped
                       (lambda (w)
                         (incf warned)
                         (is (equal "dup" (endpoint-link-skipped-key w)))
                         (is (= 2 (length
                                   (endpoint-link-skipped-classes w))))
                         (muffle-warning w))))
        (with-transaction () (setq c (ee-b :subject "dup"))))
      (is (= 1 warned))
      (is (null (ee-linked-to c 'subject-of g)))
      (is (= 1 (length (claims-touching g 'ee-claim :ee-things "dup")))))))

(test caller-resolved-cross-store-endpoint-is-linked
  "Spec sec.4.2: the object lives in another store; the caller resolved
it before the transaction and hands it over; the edge carries a
foreign id."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (declare (ignorable sg))
      (let (n c)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1")))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (is-true n)
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            (setq c (ee-b :object-namespace :st-reports :object "r-1"
                          :object-node n))))
        (is (equalp (id n) (ee-linked-to c 'object-of g)))))))

(test caller-resolved-mismatch-refuses-the-write
  "Spec sec.4.2 step 1 / sec.7: a node that is not a source of the
namespace, or whose key is not the claim's, is ENDPOINT-MISMATCH and
nothing commits."
  (with-ee-graph (g)
    (let (s)
      (with-transaction () (setq s (ee-thing "t-1")))
      ;; wrong class for the namespace
      (signals endpoint-mismatch
        (with-transaction ()
          (ee-b :object-namespace :st-reports :object "r-1"
                :object-node s)))
      ;; right class, wrong key
      (signals endpoint-mismatch
        (with-transaction ()
          (ee-b :object "t-2" :object-node s)))
      (is (null (claims-touching g 'ee-claim :ee-things "t-1"))))))

(test claim-endpoints-answers-the-linked-nodes
  (with-ee-graph (g)
    (let (s o c u)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction ()
        (setq c (ee-b))
        (setq u (make-ee-claim-unary :subject-namespace :ee-things
                                     :subject-key "t-1" :relation "u"
                                     :producer "p" :standing :inferred)))
      (multiple-value-bind (cs co) (claim-endpoints c)
        (is-true (same-node-p s cs))
        (is-true (same-node-p o co)))
      (multiple-value-bind (us uo) (claim-endpoints u)
        (is-true (same-node-p s us))
        (is (null uo)))
      (let ((k (with-transaction () (ee-b :subject "nobody" :object "x"))))
        (is (equal '(nil nil) (multiple-value-list (claim-endpoints k))))))))

(test node-claims-is-the-adjacency-twin-of-claims-touching
  "Spec sec.6.1: same filters, same meaning; linked claims only."
  (with-ee-graph (g)
    (let (s o c1 c2 c3)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction ()
        (setq c1 (ee-b :relation "likes"))
        (setq c2 (ee-b :relation "knows"))
        ;; T-2 as the SUBJECT of a claim about a key-only object.
        (setq c3 (ee-b :subject "t-2" :object "elsewhere" :relation "r")))
      (flet ((ids (claims) (sort (mapcar (lambda (c) (graph-db:string-id
                                                        (id c)))
                                          claims)
                                 #'string<)))
        (is (equal (ids (list c1 c2)) (ids (node-claims s))))
        (is (equal (ids (list c1 c2 c3)) (ids (node-claims o))))
        (is (equal (ids (list c3)) (ids (node-claims o :role :subject))))
        (is (equal (ids (list c1 c2)) (ids (node-claims o :role :object))))
        (is (equal (ids (list c1))
                   (ids (node-claims s :relation "likes"))))
        (is (equal (ids (list c1 c2))
                   (ids (node-claims s :family 'ee-claim))))
        (is (null (node-claims s :family 'ct-claim)))
        ;; A key-only claim is absent here and present in CLAIMS-TOUCHING.
        (with-transaction () (ee-b :subject "ghost"))
        (is (null (node-claims s :relation "ghost")))
        (is (= 1 (length (claims-touching g 'ee-claim :ee-things "ghost"))))
        ;; Pagination.
        (multiple-value-bind (page more) (node-claims s :limit 1)
          (is (= 1 (length page)))
          (is-true more))))))

(test retraction-keeps-the-edges-and-current-filters
  "Spec R3 / sec.4.3: RETRACT-CLAIM touches no edge; :CURRENT hides it."
  (with-ee-graph (g)
    (let (s c)
      (with-transaction () (setq s (ee-thing "t-1")) (ee-thing "t-2"))
      (with-transaction () (setq c (ee-b)))
      (retract-claim c)
      (is (= 1 (length (node-claims s))))
      (is (null (node-claims s :current t)))
      (is-true (same-node-p s (claim-endpoints c))))))

(test regeneration-drops-the-old-edges-and-links-the-new
  "Spec sec.4.3: delete, then insert; ACTIVE-EDGE-P hides the deleted
claim's edges, the new claim links at write."
  (with-ee-graph (g)
    (let (s c2)
      (with-transaction () (setq s (ee-thing "t-1")) (ee-thing "t-2"))
      (with-transaction () (ee-b :producer "gen"))
      (is (= 1 (length (node-claims s))))
      (is (= 1 (delete-claims-by-producer g 'ee-claim "gen")))
      (is (null (node-claims s)))
      (with-transaction () (setq c2 (ee-b :producer "gen")))
      (is (= 1 (length (node-claims s))))
      (is-true (same-node-p s (claim-endpoints c2))))))

(defun ee-ids (nodes)
  (sort (mapcar (lambda (n) (format nil "~A" (id n))) nodes) #'string<))

(test traverse-walks-source-to-source-through-a-claim
  "Spec sec.6.2: T-1 <-subject-of- C -object-of-> T-2, both directions;
T-1 itself is re-reached along the back-edge (global uniqueness)."
  (with-ee-graph (g)
    (let (s o c)
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (with-transaction () (setq c (ee-b)))
      (let ((reached (graph-db:traverse
                      s :graph g :direction :both
                      :edge-type '(or subject-of object-of))))
        ;; S is re-reached along the back-edge under global uniqueness,
        ;; as tests/traverse-tests.lisp (traverse-direction-both)
        ;; documents.
        (is (equal (ee-ids (list c o s)) (ee-ids reached))))
      (is (null (graph-db:traverse s :graph g :direction :both)))
      ;; From the claim, both endpoints are one hop out.
      (is (equal (ee-ids (list s o))
                 (ee-ids (graph-db:traverse
                          c :graph g :direction :out
                          :edge-type '(or subject-of object-of))))))))

(test traverse-lands-a-cross-store-endpoint-without-walking-past-it
  "Spec sec.11 (pinned for #368): a caller-resolved endpoint in another
store lands in the results -- the vertex while its store is open, the
UNRESOLVED-NODE marker once it is closed -- and nothing beyond it is
walked."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (let (n c)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1"))
          ;; A neighbour in the far store that a continuation WOULD reach.
          (make-st-report :headline "two" :report-id "r-2"))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            (setq c (ee-b :object-namespace :st-reports :object "r-1"
                          :object-node n))))
        (let ((open (graph-db:traverse c :graph g :direction :out
                                         :edge-type 'object-of)))
          (is (= 1 (length open)))
          (is-true (same-node-p n (first open))))
        (close-graph sg :snapshot-p nil)
        (let ((closed (graph-db:traverse c :graph g :direction :out
                                           :edge-type 'object-of)))
          (is (= 1 (length closed)))
          (is-true (graph-db:unresolved-node-p (first closed))))))))

(defun ee-q (g text &rest keys)
  "RUN-GUARDED-PROLOG on G; rows as bound (:RAW)."
  (nth-value 1 (apply #'graph-db.query:run-guarded-prolog text g
                      :format :raw keys)))

(defun ee-seed (g)
  "T-1 likes T-2 and knows T-3; T-2 likes T-3; one retracted T-1 hates T-3.
G is *GRAPH* already (WITH-EE-GRAPH binds it); taken for the call site's
readability."
  (declare (ignorable g))
  (let (c)
    (with-transaction ()
      (ee-thing "t-1") (ee-thing "t-2") (ee-thing "t-3"))
    (with-transaction ()
      (ee-b :subject "t-1" :object "t-2" :relation "likes")
      (ee-b :subject "t-1" :object "t-3" :relation "knows")
      (ee-b :subject "t-2" :object "t-3" :relation "likes")
      (setq c (ee-b :subject "t-1" :object "t-3" :relation "hates")))
    (retract-claim c)
    c))

(defun ee-labels (rows col)
  (sort (mapcar (lambda (r) (ee-label (nth col r))) rows) #'string<))

(test related-solves-from-a-bound-subject-current-only
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(is-a ?s ee-thing)
                         (node-slot-value ?s thing-id \"t-1\")
                         (related ?s ?r ?o)")))
      (is (= 2 (length rows)))
      (is (equal '("knows" "likes") (sort (mapcar #'second rows) #'string<)))
      (is (equal '("t-2" "t-3") (ee-labels rows 2))))))

(test related-solves-from-a-bound-object
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(is-a ?o ee-thing)
                         (node-slot-value ?o thing-id \"t-3\")
                         (related ?s ?r ?o)")))
      ;; knows(t-1,t-3), likes(t-2,t-3); hates is retracted.  Columns
      ;; are first-appearance order (O then S): S is col 1 (GH #369).
      (is (= 2 (length rows)))
      (is (equal '("t-1" "t-2") (ee-labels rows 1))))))

(test related-scans-by-relation-when-only-it-is-bound
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(related ?s \"likes\" ?o)")))
      (is (= 2 (length rows)))
      (is (equal '("t-1" "t-2") (ee-labels rows 0))))))

(test related-refuses-an-unbounded-scan
  (with-ee-graph (g)
    (ee-seed g)
    (signals graph-db.query:prolog-ill-typed-error
      (ee-q g "(related ?s ?r ?o)"))))

(test claimed-exposes-the-claim-and-keeps-history
  (with-ee-graph (g)
    (ee-seed g)
    (let ((rows (ee-q g "(is-a ?s ee-thing)
                         (node-slot-value ?s thing-id \"t-1\")
                         (claimed ?c ?s ?r ?o)")))
      ;; likes, knows, and the RETRACTED hates.  Columns are first-
      ;; appearance order (S then C): C is col 1 (GH #369).
      (is (= 3 (length rows)))
      (is (every (lambda (r) (typep (second r) 'ee-claim)) rows))
      (is (equal '("hates" "knows" "likes")
                 (sort (mapcar #'third rows) #'string<))))))

(test a-cross-store-endpoint-is-linked-but-does-not-unify
  "Spec sec.8 as amended: the functors resolve with LOOKUP-VERTEX on the
claim's graph, so the foreign endpoint yields no row in #367."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (let (n)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1")))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            (ee-b :object-namespace :st-reports :object "r-1"
                  :object-node n)))
        (is (null (ee-q g "(is-a ?s ee-thing) (related ?s ?r ?o)")))
        ;; The same-store half still solves.
        (is (= 1 (length (ee-q g "(is-a ?s ee-thing) (subject-of ?c ?s)"))))))))
