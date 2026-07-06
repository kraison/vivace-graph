;;;; Tests for the general ordered secondary index (:INDEX slot option / DEF-INDEX).
;;;; See docs/general-index-design.md.  The index is "\:unique minus enforcement":
;;;; equality lookup + ascending range, NIL-exempt, class-scoped (spans subclasses),
;;;; maintained on the commit apply path, durable (on-disk sidecar), backend-agnostic.

(in-package #:graph-db/test)

(defparameter *ix-graph-name* :graph-db-index-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ix-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex ix-person ()
  ((name  :initarg :name  :accessor ix-name  :index t)              ; plain index
   (age   :initarg :age   :accessor ix-age   :index t)              ; numeric range
   (email :initarg :email :accessor ix-email :index string-downcase) ; canonicalized
   (note  :initarg :note  :accessor ix-note))                       ; NOT indexed
  :graph-db-index-test)

;; A subclass: an :INDEX slot on the parent is one shared index across subclasses.
(def-vertex ix-employee (ix-person)
  ((title :initarg :title :accessor ix-title))
  :graph-db-index-test)

(def-suite index-suite
  :description "General ordered secondary index (:INDEX / def-index)."
  :in graph-db-suite)

(in-suite index-suite)

(defmacro with-ix-graph ((g &key (backend :skip-list)) &body body)
  "A fresh on-disk graph named *IX-GRAPH-NAME* on BACKEND, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ix-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000 :index-backend ,backend)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun ix-names (nodes) (sort (mapcar #'ix-name nodes) #'string<))

;;; --- equality ---------------------------------------------------------------

(test lookup-returns-all-sharing-nodes
  "index-lookup returns every node with the value (a non-unique slot -> many ids)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)
      (make-ix-person :name "b" :age 30)
      (make-ix-person :name "c" :age 40))
    (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
    (is (equal '("c")     (ix-names (index-lookup g 'ix-person 'age 40))))
    (is (null (index-lookup g 'ix-person 'age 99)))))

(test lookup-canonicalized
  "A canonicalizer (string-downcase) index matches case-insensitively; the probe is
canonicalized too."
  (with-ix-graph (g)
    (with-transaction () (make-ix-person :name "a" :email "Alice@X.com"))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'email "alice@x.com"))))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'email "ALICE@X.COM"))))))

(test null-and-unbound-excluded
  "A NULL / unbound indexed slot is not indexed (SQL-style); a declared-but-empty
index is a legitimate empty result, not an error."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)                ; email unbound
      (make-ix-person :name "b" :age 30 :email nil)     ; email NIL
      (make-ix-person :name "c" :age 30 :email "c@x"))  ; email present
    ;; only the node with a real email is indexed
    (is (equal '("c") (ix-names (index-lookup g 'ix-person 'email "c@x"))))
    (is (= 1 (graph-db::ix-count (graph-db::%secondary-index-lookup g 'ix-person 'email))))
    ;; querying a declared index for a value nobody holds -> empty, not an error
    (is (null (index-lookup g 'ix-person 'email "nobody@x")))))

(test unindexed-slot-errors
  "index-lookup on a slot with no :index is a programming error (signals)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-person :name "a" :age 30 :note "hi"))
    (signals error (index-lookup g 'ix-person 'note "hi"))))

;;; --- range ------------------------------------------------------------------

(test range-bounded-and-open
  "index-range returns the ordered subset in [start,end]; open ends work."
  (with-ix-graph (g)
    (with-transaction ()
      (dolist (a '(10 20 30 40 50))
        (make-ix-person :name (format nil "n~D" a) :age a)))
    (is (equal '(20 30 40)
               (mapcar #'ix-age (index-range g 'ix-person 'age :start 20 :end 40))))
    (is (equal '(10 20)
               (mapcar #'ix-age (index-range g 'ix-person 'age :end 20))))
    (is (equal '(40 50)
               (mapcar #'ix-age (index-range g 'ix-person 'age :start 40))))
    (is (equal '(10 20 30 40 50)
               (mapcar #'ix-age (index-range g 'ix-person 'age))))))

;;; --- maintenance: update / delete -------------------------------------------

(test update-moves-node-between-values
  "Updating an indexed slot releases the old value and claims the new one."
  (with-ix-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-ix-person :name "a" :age 30))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (ix-age v) 31)
          (save v)))
      (is (null (index-lookup g 'ix-person 'age 30)) "old value released")
      (is (equal '("a") (ix-names (index-lookup g 'ix-person 'age 31))) "new value claimed"))))

(test delete-removes-from-index
  "A deleted node drops out of the index."
  (with-ix-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-ix-person :name "a" :age 30))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is (null (index-lookup g 'ix-person 'age 30))))))

;;; --- scope: subclasses ------------------------------------------------------

(test subclasses-share-parent-index
  "An index on a parent slot covers subclass instances (one shared index)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person   :name "p" :age 30)
      (make-ix-employee :name "e" :age 30 :title "boss"))
    ;; querying via the parent class sees both
    (is (equal '("e" "p") (ix-names (index-lookup g 'ix-person 'age 30))))
    ;; querying via the subclass sees the subclass instance (rooted at ancestor)
    (is (member "e" (mapcar #'ix-name (index-lookup g 'ix-employee 'age 30)) :test #'string=))))

;;; --- reopen (on-disk sidecar) -----------------------------------------------

(test reopen-restores-index
  "The on-disk index reopens from its sidecar (no rebuild needed) and still answers."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-ix-person :name "a" :age 30)
               (make-ix-person :name "b" :age 30)))
        (close-graph g)))
    (let ((g (open-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))

;;; --- dual backend -----------------------------------------------------------

(test bplus-backend-equality-and-range
  "The index works identically on the B+ tree backend."
  (with-ix-graph (g :backend :bplus-tree)
    (with-transaction ()
      (dolist (a '(10 20 20 30))
        (make-ix-person :name (format nil "n~D" a) :age a)))
    (is (= 2 (length (index-lookup g 'ix-person 'age 20))))
    (is (equal '(10 20 20)
               (mapcar #'ix-age (index-range g 'ix-person 'age :end 20))))))

;;; --- wrong-graph discipline -------------------------------------------------

(test index-resolves-in-passed-graph
  "index-lookup / index-range resolve ids in the GRAPH argument, not the ambient
*graph* (the wrong-graph audit discipline)."
  (with-ix-graph (b)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)
      (make-ix-person :name "b" :age 30))
    (with-temp-directory (dir-a)
      (let ((a (make-graph :ix-decoy (namestring dir-a) :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* a))            ; ambient graph is the WRONG one
               (is (equal '("a" "b") (ix-names (index-lookup b 'ix-person 'age 30))))
               (is (= 2 (length (index-range b 'ix-person 'age :start 30 :end 30)))))
          (ignore-errors (close-graph a :snapshot-p nil))
          (collect-garbage))))))
