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
