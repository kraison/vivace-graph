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
