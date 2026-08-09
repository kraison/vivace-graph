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

;; The standalone declaration surface: index NOTE, which is NOT marked :index t on
;; the slot, with a canonicalizer.  Declared before any graph is open -> registered,
;; then built at open (or maintained on apply for a fresh make-graph).
(def-index ix-person note :graph-db-index-test :canonicalize string-downcase)

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

;;; --- def-index (standalone declaration surface) -----------------------------

(test def-index-maintains-and-queries
  "A def-index on a slot NOT marked :index t is maintained on apply and queryable;
its :canonicalize (string-downcase) makes it case-insensitive."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :note "Hello")
      (make-ix-person :name "b" :note "world")
      (make-ix-person :name "c" :note "hello"))
    (is (equal '("a" "c") (ix-names (index-lookup g 'ix-person 'note "HELLO"))))
    (is (equal '("b")     (ix-names (index-lookup g 'ix-person 'note "world"))))))

(test def-index-empty-is-nil-not-error
  "Querying a declared def-index with no entries yet is an empty result, not an error."
  (with-ix-graph (g)
    (is (null (index-lookup g 'ix-person 'note "nobody")))))

(test def-index-reopen
  "A def-index'd index is durable and reopens from the sidecar."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (make-ix-person :name "a" :note "Hi")))
        (close-graph g)))
    (let ((g (open-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (is (equal '("a") (ix-names (index-lookup g 'ix-person 'note "HI"))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))

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

(test secondary-sidecar-torn-write-falls-back-to-rebuild
  "GH #63: a truncated secondary-index sidecar must not prevent the graph from
opening.  Before the fix, CL-STORE:RESTORE's error propagated straight out of
OPEN-GRAPH (via RESTORE-SECONDARY-INDEX-ROOTS) and the open itself failed; now
it falls back to REBUILD-SECONDARY-INDEXES, exactly as the spatial sidecar
already does."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-person :name "a" :age 30)
                 (make-ix-person :name "b" :age 30)))
          (close-graph g)))
      ;; Truncate the sidecar mid-record, as an interrupted write would.
      (let* ((file (graph-db::secondary-index-root-file path))
             (bytes (with-open-file (in file :element-type '(unsigned-byte 8))
                      (let ((b (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                        (read-sequence b in)
                        b))))
        (with-open-file (out file :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence bytes out :end (floor (length bytes) 2))))
      (handler-bind ((warning #'muffle-warning))    ; the torn-sidecar warning
        (let ((g (open-graph *ix-graph-name* path :buffer-pool-size 1000)))
          (unwind-protect
               (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30)))
                   "the index was rebuilt from the still-intact nodes")
            (ignore-errors (close-graph g))
            (collect-garbage)))))))

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

;;; --- memory backend ---------------------------------------------------------

(defmacro with-ix-memory-graph ((g) &body body)
  "A fresh in-memory graph named *IX-GRAPH-NAME*, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (graph-db::make-memory-graph *ix-graph-name* (namestring ,dir))))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(test memory-backend-equality-and-range
  "The index works on a memory-graph (mem-skip-list backing), :index t and def-index."
  (with-ix-memory-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30 :note "Hi")
      (make-ix-person :name "b" :age 30)
      (make-ix-person :name "c" :age 40))
    (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
    (is (equal '(30 30 40) (mapcar #'ix-age (index-range g 'ix-person 'age))))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'note "HI"))))))

(test memory-backend-reopen-rebuilds
  "A memory-graph rebuilds its indexes on reopen from the restored nodes."
  (with-temp-directory (dir)
    (let ((g (graph-db::make-memory-graph *ix-graph-name* (namestring dir))))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-ix-person :name "a" :age 30 :note "Hi")
               (make-ix-person :name "b" :age 30)))
        (close-graph g)))            ; checkpoint image + journal
    (let ((g (graph-db::open-memory-graph *ix-graph-name* (namestring dir))))
      (unwind-protect
           (progn
             (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
             (is (equal '("a") (ix-names (index-lookup g 'ix-person 'note "HI")))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))
