;;;; Tests for unique constraints (:UNIQUE slot option) -- issue #6.
;;;;
;;;; Enforcement is a commit-boundary check (VALIDATE-UNIQUE-CONSTRAINTS pre-
;;;; durability + APPLY-TX-WRITES-TO-UNIQUE-INDEXES post-durability, both under
;;;; %COMMIT's manager lock).  These exercise create/update/delete, the
;;;; canonicalizer + EQUALP forms, NULL-exemption, cross-subtype sharing, the
;;;; concurrent race (the phantom the commit lock defeats), reopen rebuild, and
;;;; the memory backend.

(in-package #:graph-db/test)

(defparameter *uq-graph-name* :graph-db-unique-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *uq-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex uq-user ()
  ((uname :initarg :uname :accessor uq-uname :unique t)
   (email :initarg :email :accessor uq-email :unique string-downcase)
   (note  :initarg :note  :accessor uq-note))
  :graph-db-unique-test)

;; A subclass: inherits UNAME's uniqueness, so it must share one index with UQ-USER.
(def-vertex uq-admin (uq-user)
  ((level :initarg :level :accessor uq-level))
  :graph-db-unique-test)

(def-suite unique-constraint-suite
  :description "Unique constraints (:UNIQUE) -- issue #6."
  :in graph-db-suite)

(in-suite unique-constraint-suite)

(defmacro with-uq-graph ((g) &body body)
  "A fresh on-disk graph named *UQ-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *uq-graph-name* (namestring ,dir) :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun uq-index-size (g owner slot)
  (let ((uix (gethash (cons owner slot) (graph-db::unique-indexes g))))
    (and uix (graph-db::uix-count uix))))

(test reject-duplicate-create
  "A create duplicating a live node's unique value is rejected, and the rejected
transaction leaves nothing behind (clean pre-durability abort)."
  (with-uq-graph (g)
    (with-transaction () (make-uq-user :uname "alice" :email "a@x.com"))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-uq-user :uname "alice" :email "b@x.com")))
    (is (= 1 (length (map-vertices #'identity g :collect-p t :vertex-type 'uq-user)))
        "the duplicate was not committed")))

(test allow-distinct-and-null
  "Distinct values commit; NULL/unbound unique slots are exempt (many allowed)."
  (with-uq-graph (g)
    (finishes (with-transaction ()
                (make-uq-user :uname "alice" :email "a@x.com")
                (make-uq-user :uname "bob"   :email "b@x.com")))
    ;; two nodes with NIL uname -> allowed
    (finishes (with-transaction ()
                (make-uq-user :uname nil :email "c@x.com")
                (make-uq-user :uname nil :email "d@x.com")))
    (is (= 4 (length (map-vertices #'identity g :collect-p t :vertex-type 'uq-user))))))

(test update-to-duplicate-vs-self
  "Updating a node's unique slot to another node's value is rejected; updating a
node while keeping its own value (or changing a non-unique slot) is fine."
  (with-uq-graph (g)
    (let (bid)
      (with-transaction ()
        (make-uq-user :uname "alice" :email "a@x.com")
        (setq bid (id (make-uq-user :uname "bob" :email "b@x.com"))))
      (signals graph-db:unique-constraint-violation
        (with-transaction ()
          (let ((v (copy (lookup-vertex bid)))) (setf (uq-uname v) "alice") (save v))))
      (finishes                          ; changing a non-unique slot on bob
        (with-transaction ()
          (let ((v (copy (lookup-vertex bid)))) (setf (uq-note v) "hi") (save v))))
      (finishes                          ; "updating" bob's uname to its own value
        (with-transaction ()
          (let ((v (copy (lookup-vertex bid)))) (setf (uq-uname v) "bob") (save v)))))))

(test canonicalizer-case-insensitive
  "A :UNIQUE canonicalizer (string-downcase) makes the constraint case-insensitive."
  (with-uq-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-uq-user :uname "alice" :email "Alice@X.com"))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-uq-user :uname "bob" :email "ALICE@x.COM")))
    (finishes (with-transaction () (make-uq-user :uname "carol" :email "carol@x.com")))))

(test reuse-value-after-delete
  "Deleting a node releases its unique value, so it can be reused."
  (with-uq-graph (g)
    (declare (ignorable g))
    (let (aid)
      (with-transaction () (setq aid (id (make-uq-user :uname "alice" :email "a@x.com"))))
      (with-transaction () (mark-deleted (lookup-vertex aid)))
      (finishes (with-transaction () (make-uq-user :uname "alice" :email "a2@x.com"))))))

(test cross-subtype-uniqueness
  "A :UNIQUE slot on a parent is enforced across subclasses via one shared index:
a UQ-ADMIN cannot take a UNAME already held by a UQ-USER."
  (with-uq-graph (g)
    (with-transaction () (make-uq-user :uname "alice" :email "a@x.com"))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-uq-admin :uname "alice" :email "b@x.com" :level 1)))
    (is (= 1 (uq-index-size g 'uq-user 'uname))
        "one shared uq-user/uname index owns both types' claims")))

(test reopen-restores-durable-index-and-enforces
  "On-disk the unique index is a persistent skip-list: close saves its root, open
reopens it from the sidecar WITHOUT scanning nodes (rebuild not called), and it still
enforces."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *uq-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (make-uq-user :uname "alice" :email "a@x.com")
            (make-uq-user :uname "bob"   :email "b@x.com")))
        (close-graph g))
      (is-true (probe-file (graph-db::unique-index-root-file path))
               "close-graph saved the unique-index root sidecar")
      (let ((rebuilt nil) (orig (fdefinition 'graph-db::rebuild-unique-indexes)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::rebuild-unique-indexes)
                     (lambda (gr) (setf rebuilt t) (funcall orig gr)))
               (let ((g2 (open-graph *uq-graph-name* path)))
                 (unwind-protect
                      (let ((*graph* g2))
                        (is (null rebuilt) "reopen restored from the sidecar, no scan")
                        (is (= 2 (uq-index-size g2 'uq-user 'uname)) "index restored")
                        (signals graph-db:unique-constraint-violation
                          (with-transaction () (make-uq-user :uname "alice" :email "c@x.com")))
                        (finishes (with-transaction () (make-uq-user :uname "carol" :email "c@x.com"))))
                   (ignore-errors (close-graph g2))
                   (collect-garbage))))
          (setf (fdefinition 'graph-db::rebuild-unique-indexes) orig))))))

(test concurrent-race-exactly-one-wins
  "The phantom the commit lock defeats: N threads racing to create the same unique
value -- exactly one commits, the rest get UNIQUE-CONSTRAINT-VIOLATION."
  (with-uq-graph (g)
    (let ((oks 0) (rejects 0) (lock (bt:make-lock)) (threads nil))
      (dotimes (i 8)
        (push (bt:make-thread
               (lambda ()
                 (let ((*graph* g))
                   (handler-case
                       (progn (with-transaction () (make-uq-user :uname "race"))
                              (bt:with-lock-held (lock) (incf oks)))
                     (graph-db:unique-constraint-violation ()
                       (bt:with-lock-held (lock) (incf rejects)))))))
              threads))
      (mapc #'bt:join-thread threads)
      (is (= 1 oks) "exactly one thread committed the value (got ~D)" oks)
      (is (= 7 rejects) "the other seven were rejected (got ~D)" rejects)
      (is (= 1 (length (map-vertices #'identity g :collect-p t :vertex-type 'uq-user)))
          "one node exists"))))

(test memory-backend-enforces-and-reopens
  "Uniqueness works on the in-memory backend (shared commit path) and survives a
checkpoint + reopen."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (graph-db::make-memory-graph *uq-graph-name* path)))
        (let ((*graph* g))
          (with-transaction () (make-uq-user :uname "alice" :email "a@x.com")))
        (signals graph-db:unique-constraint-violation
          (let ((*graph* g))
            (with-transaction () (make-uq-user :uname "alice" :email "b@x.com"))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *uq-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 1 (uq-index-size g2 'uq-user 'uname)) "index restored on memory reopen")
               (signals graph-db:unique-constraint-violation
                 (with-transaction () (make-uq-user :uname "alice" :email "c@x.com"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test memory-index-is-durable-not-rebuilt
  "The memory-backend unique index is DURABLE: it rides the checkpoint image, so a
reopen restores it WITHOUT scanning nodes (REBUILD-UNIQUE-INDEXES is not called)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (graph-db::make-memory-graph *uq-graph-name* path)))
        (let ((*graph* g))
          (with-transaction ()
            (make-uq-user :uname "alice" :email "a@x.com")
            (make-uq-user :uname "bob"   :email "b@x.com")))
        (close-graph g :snapshot-p t))
      (let ((rebuilt nil) (orig (fdefinition 'graph-db::rebuild-unique-indexes)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::rebuild-unique-indexes)
                     (lambda (gr) (setf rebuilt t) (funcall orig gr)))
               (let ((g2 (graph-db::open-memory-graph *uq-graph-name* path)))
                 (unwind-protect
                      (let ((*graph* g2))
                        (is (null rebuilt) "reopen loaded the index from the image, no scan")
                        (is (= 2 (uq-index-size g2 'uq-user 'uname)) "restored from image")
                        (signals graph-db:unique-constraint-violation
                          (with-transaction () (make-uq-user :uname "alice" :email "c@x.com"))))
                   (ignore-errors (close-graph g2 :snapshot-p nil))
                   (collect-garbage))))
          (setf (fdefinition 'graph-db::rebuild-unique-indexes) orig))))))

(test memory-index-durable-lazy-does-not-materialize
  "On a LAZY memory graph, restoring the unique index from the image does NOT
materialize nodes (they stay LZNODE blobs) -- the durable index is fault-on-access
safe, unlike rebuild-on-open."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (graph-db::make-memory-graph *uq-graph-name* path :lazy t)))
        (let ((*graph* g))
          (with-transaction ()
            (dotimes (i 20) (make-uq-user :uname (format nil "u~D" i)
                                          :email (format nil "u~D@x.com" i)))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *uq-graph-name* path :lazy t)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 20 (uq-index-size g2 'uq-user 'uname)) "all keys restored from image")
               (is-true (loop for v being the hash-values of
                              (graph-db::mem-table-data (graph-db::vertex-table g2))
                              always (graph-db::lznode-p v))
                        "no node was materialized by the unique-index restore")
               (signals graph-db:unique-constraint-violation
                 (with-transaction () (make-uq-user :uname "u7" :email "dup@x.com"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
