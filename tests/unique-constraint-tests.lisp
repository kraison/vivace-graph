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

;; DEF-UNIQUE fixture (GH #107): a multi-slot constraint on (NS KY), where KY
;; is optional -- the forcing case for the null-exempts-the-tuple semantic.
(def-vertex uq-claim ()
  ((ns :initarg :ns :accessor uqc-ns)
   (ky :initarg :ky :accessor uqc-ky :initform nil))
  :graph-db-unique-test)

(def-unique uq-claim (ns ky) :graph-db-unique-test)

(defun uq-fresh-class-name (prefix)
  "A fresh class name for a per-run DEF-VERTEX -- INTERNED, not a bare
GENSYM: every registered DEF-UNIQUE is installed at graph creation now, and
the memory checkpoint cannot serialize an uninterned owner name (GH #129)."
  (intern (symbol-name (gensym prefix)) :graph-db/test))

(defun uq-boom-canon (v)
  "A canonicalizer that signals on one value -- the stand-in for the legacy
node a build scan chokes on (GH #129)."
  (if (equal v "boom") (error "canonicalizer refuses ~S" v) v))

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

(test unique-sidecar-torn-write-falls-back-to-rebuild
  "GH #63: a truncated unique-index sidecar must not prevent the graph from
opening.  Before the fix, CL-STORE:RESTORE's error propagated straight out of
OPEN-GRAPH (via RESTORE-UNIQUE-INDEX-ROOTS) and the open itself failed; now it
falls back to REBUILD-UNIQUE-INDEXES, exactly as the spatial sidecar already
does."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *uq-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (make-uq-user :uname "alice" :email "a@x.com")
            (make-uq-user :uname "bob"   :email "b@x.com")))
        (close-graph g))
      ;; Truncate the sidecar mid-record, as an interrupted write would.
      (let* ((file (graph-db::unique-index-root-file path))
             (bytes (with-open-file (in file :element-type '(unsigned-byte 8))
                      (let ((b (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                        (read-sequence b in)
                        b))))
        (with-open-file (out file :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence bytes out :end (floor (length bytes) 2))))
      (handler-bind ((warning #'muffle-warning))    ; the torn-sidecar warning
        (let ((g2 (open-graph *uq-graph-name* path :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (is (= 2 (uq-index-size g2 'uq-user 'uname))
                     "the index was rebuilt from the still-intact nodes")
                 (signals graph-db:unique-constraint-violation
                   (with-transaction () (make-uq-user :uname "alice" :email "c@x.com")))
                 (finishes (with-transaction () (make-uq-user :uname "carol" :email "c@x.com"))))
            (ignore-errors (close-graph g2))
            (collect-garbage)))))))

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

;;;; DEF-UNIQUE -- multi-slot uniqueness constraint (GH #107).  UQ-CLAIM (NS
;;;; KY) is the fixture: KY is optional, so a null KY is the forcing case for
;;;; the null-exempts-the-tuple semantic (opposite of an ordinary index's
;;;; +NULL-COMPONENT+ substitution).

(test multi-slot-unique-rejects-duplicate-tuple
  "The same (ns, ky) pair twice must signal at the commit boundary (#107)."
  (with-uq-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-uq-claim :ns "ops" :ky "e1"))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-uq-claim :ns "ops" :ky "e1")))))

(test multi-slot-unique-exempts-null-component
  "Two tuples sharing their populated component but both null elsewhere do NOT
collide -- SQL semantics, and the unary-claim case (#107)."
  (with-uq-graph (g)
    (with-transaction () (make-uq-claim :ns "ops" :ky nil))
    (finishes (with-transaction () (make-uq-claim :ns "ops" :ky nil)))
    (is (= 2 (length (map-vertices #'identity g :collect-p t
                                   :vertex-type 'uq-claim)))
        "both null-ky claims were committed -- neither was exempt from
being WRITTEN, only from the CONSTRAINT")))

(test multi-slot-unique-distinct-tuples-allowed
  "Tuples differing in either component are distinct claims."
  (with-uq-graph (g)
    (declare (ignorable g))
    (finishes (with-transaction ()
                (make-uq-claim :ns "ops" :ky "e1")
                (make-uq-claim :ns "ops" :ky "e2")
                (make-uq-claim :ns "eng" :ky "e1")))))

(test multi-slot-unique-update-and-delete-release
  "Updating a claim's tuple to another live claim's tuple is rejected;
deleting a claim releases its tuple so it can be reclaimed."
  (with-uq-graph (g)
    (let (bid)
      (with-transaction ()
        (make-uq-claim :ns "ops" :ky "e1")
        (setq bid (id (make-uq-claim :ns "ops" :ky "e2"))))
      (signals graph-db:unique-constraint-violation
        (with-transaction ()
          (let ((v (copy (lookup-vertex bid))))
            (setf (uqc-ky v) "e1") (save v))))
      (with-transaction () (mark-deleted (lookup-vertex bid)))
      (finishes (with-transaction () (make-uq-claim :ns "ops" :ky "e2"))))))

(test multi-slot-unique-declared-after-open-scans-strictly
  "DEF-UNIQUE evaluated while the graph is already open builds the index
now, STRICTLY (%BUILD-UNIQUE-TUPLE-FOR-SPEC :STRICT-P T): a pre-existing
duplicate tuple signals once the scan completes, so the published index still
covers every node.  Distinct from INSTALL-UNIQUE-TUPLE-CONSTRAINTS's tolerant
reopen path (GH #107).

Uses a class GENSYMed fresh each run, defined and DEF-UNIQUE'd entirely at
test-run time via EVAL, rather than the file's load-time UQ-CLAIM fixture:
UQ-CLAIM's constraint is already globally registered before any test's
graph opens, so seeding a genuine pre-existing duplicate for it is
impossible -- the seeding transaction itself would hit the intra-
transaction check first.  A fresh class sidesteps that, and also keeps
this test idempotent across repeated FIVEAM runs within one Lisp image
(DEF-UNIQUE's registry is global and permanent for the session)."
  (with-uq-graph (g)
    (declare (ignorable g))
    (let* ((*package* (find-package :graph-db/test))
           (cls (uq-fresh-class-name "UQ-SOLO"))
           (mk (intern (format nil "MAKE-~A" cls))))
      (eval `(def-vertex ,cls () (a b) :graph-db-unique-test))
      (with-transaction ()
        (funcall mk :a "x" :b "y")
        (funcall mk :a "x" :b "y"))
      (signals graph-db:unique-constraint-violation
        (eval `(def-unique ,cls (a b) :graph-db-unique-test))))))

(test multi-slot-unique-failed-strict-build-covers-every-node
  "GH #107 (whole-branch review): a failed STRICT DEF-UNIQUE left a half-built,
LIVE constraint.  %UNIQUE-TUPLE-INDEX-FOR published the UIX before the scan and
the strict path signalled on the FIRST duplicate, so the constraint covered
only the prefix scanned before the error -- duplicates in the un-scanned tail
committed with no complaint, and %ENSURE-UNIQUE-TUPLE-BUILT never retried, the
key being present.

Both halves are needed and are asserted here: the scan now RUNS TO COMPLETION
before the strict signal (so the published index is whole), and a scan that
dies for any other reason unregisters what it created.  Unregistering ALONE
would not have fixed this: the enforcement path get-or-creates an EMPTY UIX on
the next commit, which enforces nothing at all.

Fresh GENSYMed class per run, for the reason
MULTI-SLOT-UNIQUE-DECLARED-AFTER-OPEN-SCANS-STRICTLY documents.  The count
assertion, not an ordering assumption, is what proves the whole scan ran:
MAP-VERTICES walks the type index, whose order is not the insertion order."
  (with-uq-graph (g)
    (let* ((*package* (find-package :graph-db/test))
           (cls (uq-fresh-class-name "UQ-TAIL"))
           (mk (intern (format nil "MAKE-~A" cls))))
      (eval `(def-vertex ,cls () (a b) :graph-db-unique-test))
      (with-transaction ()
        (funcall mk :a "dup"  :b "1")     ; the conflicting pair
        (funcall mk :a "dup"  :b "1")
        (funcall mk :a "tail" :b "2")     ; and two innocent bystanders
        (funcall mk :a "tail" :b "3"))
      (signals graph-db:unique-constraint-violation
        (eval `(def-unique ,cls (a b) :graph-db-unique-test)))
      (is (= 3 (uq-index-size g cls '(a b)))
          "every node was scanned: the kept-first duplicate plus both tails")
      (signals graph-db:unique-constraint-violation
        (with-transaction () (funcall mk :a "tail" :b "3"))))))

(test multi-slot-unique-reopen-restores-durable-index-and-enforces
  "On-disk the multi-slot unique index is a persistent skip-list too: close
saves its root, open reopens it from the sidecar WITHOUT scanning nodes
(rebuild not called), and it still enforces (#107)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *uq-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (make-uq-claim :ns "ops" :ky "e1")
            (make-uq-claim :ns "ops" :ky "e2")))
        (close-graph g))
      (let ((rebuilt nil) (built nil)
            (orig-rebuild (fdefinition 'graph-db::rebuild-unique-indexes))
            (orig-build (fdefinition 'graph-db::%build-unique-tuple-for-spec)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::rebuild-unique-indexes)
                     (lambda (gr) (setf rebuilt t) (funcall orig-rebuild gr)))
               ;; Scoped to UQ-CLAIM's own spec: *SCHEMA-UNIQUE-METADATA* is a
               ;; global, session-lifetime registry keyed by graph NAME, so a
               ;; different test's GENSYMed class (also :GRAPH-DB-UNIQUE-TEST)
               ;; is reconciled here too, harmlessly -- BUILT must not flag on
               ;; that unrelated spec (GH #107).
               (setf (fdefinition 'graph-db::%build-unique-tuple-for-spec)
                     (lambda (gr spec &key strict-p)
                       (when (eq (graph-db::unique-tuple-spec-owner-name spec)
                                 'uq-claim)
                         (setf built t))
                       (funcall orig-build gr spec :strict-p strict-p)))
               (let ((g2 (open-graph *uq-graph-name* path)))
                 (unwind-protect
                      (let ((*graph* g2))
                        (is (null rebuilt)
                            "reopen restored from the sidecar, no scan")
                        (is (null built)
                            "the multi-slot index was restored, not rescanned")
                        (is (= 2 (uq-index-size g2 'uq-claim '(ns ky)))
                            "index restored")
                        (signals graph-db:unique-constraint-violation
                          (with-transaction ()
                            (make-uq-claim :ns "ops" :ky "e1")))
                        (finishes
                         (with-transaction ()
                           (make-uq-claim :ns "ops" :ky "e3"))))
                   (ignore-errors (close-graph g2))
                   (collect-garbage))))
          (setf (fdefinition 'graph-db::rebuild-unique-indexes) orig-rebuild)
          (setf (fdefinition 'graph-db::%build-unique-tuple-for-spec)
                orig-build))))))

(test multi-slot-unique-memory-image-reopens-and-enforces
  "GH #107 (whole-branch review): a MEMORY graph carrying any DEF-UNIQUE was
UNOPENABLE after a clean close.  %DUMP-UNIQUE-INDEXES had no multi-slot branch
-- it wrote the singular SLOT-NAME, which is NIL for a tuple index -- and the
reopen fed that to the single-slot resolver, ending in (FDEFINITION NIL).  The
checkpoint image is the ONLY durable copy of a cleanly-closed memory graph (the
journal is cleared at checkpoint), so that was data loss.

The suite missed it because the memory-backend unique tests close with
:SNAPSHOT-P NIL and never create a node on a memory graph -- so this one does
both."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (graph-db::make-memory-graph *uq-graph-name* path)))
        (let ((*graph* g))
          (with-transaction () (make-uq-claim :ns "ops" :ky "e1")))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *uq-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 1 (uq-index-size g2 'uq-claim '(ns ky)))
                   "the multi-slot constraint came back from the image")
               (signals graph-db:unique-constraint-violation
                 (with-transaction () (make-uq-claim :ns "ops" :ky "e1")))
               (finishes
                (with-transaction () (make-uq-claim :ns "ops" :ky "e2"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test multi-slot-unique-lazy-memory-reopen-installs-and-enforces
  "GH #107 (whole-branch review), the second half of the image gap: on a LAZY
memory graph INSTALL-UNIQUE-TUPLE-CONSTRAINTS sat inside the (UNLESS (LAZY-P
GRAPH) ...) block, so a constraint the image did not carry came back SILENTLY
ABSENT -- no enforcement, no complaint.  The install is now outside that guard,
the same trade REBUILD-UNIQUE-INDEXES already makes: a missing constraint stops
enforcing, which is worse than materializing the owner's blobs.

The REMHASH stands in for \"the DEF-UNIQUE was declared while the graph was
closed\": *SCHEMA-UNIQUE-METADATA* is a global, session-lifetime registry, so
un-declaring UQ-CLAIM's constraint for one test is not possible -- dropping its
UIX before the checkpoint reproduces the same state the image would have."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (graph-db::make-memory-graph *uq-graph-name* path :lazy t)))
        (let ((*graph* g))
          (with-transaction () (make-uq-claim :ns "ops" :ky "e1")))
        (remhash (cons 'uq-claim '(ns ky)) (graph-db::unique-indexes g))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *uq-graph-name* path :lazy t)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 1 (uq-index-size g2 'uq-claim '(ns ky)))
                   "install rebuilt the constraint the image did not carry")
               (signals graph-db:unique-constraint-violation
                 (with-transaction () (make-uq-claim :ns "ops" :ky "e1"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test multi-slot-unique-concurrent-race-exactly-one-wins
  "The phantom the commit lock defeats, on a multi-slot tuple: N threads racing
to create the same (ns, ky) tuple -- exactly one commits, the rest get
UNIQUE-CONSTRAINT-VIOLATION (#107)."
  (with-uq-graph (g)
    (let ((oks 0) (rejects 0) (lock (bt:make-lock)) (threads nil))
      (dotimes (i 8)
        (push (bt:make-thread
               (lambda ()
                 (let ((*graph* g))
                   (handler-case
                       (progn (with-transaction ()
                                (make-uq-claim :ns "race" :ky "tuple"))
                              (bt:with-lock-held (lock) (incf oks)))
                     (graph-db:unique-constraint-violation ()
                       (bt:with-lock-held (lock) (incf rejects)))))))
              threads))
      (mapc #'bt:join-thread threads)
      (is (= 1 oks) "exactly one thread committed the tuple (got ~D)" oks)
      (is (= 7 rejects) "the other seven were rejected (got ~D)" rejects)
      (is (= 1 (length (map-vertices #'identity g :collect-p t
                                     :vertex-type 'uq-claim)))
          "one node exists"))))

(test multi-slot-unique-build-scan-tolerates-one-bad-node
  "GH #129: the DEF-UNIQUE build scan skips a node whose key cannot be
built, exactly as %BUILD-INDEX-FOR-SPEC (index.lisp) already does for a
legacy node whose slots predate the DEF-INDEX.  Before this, one such node
aborted the whole scan and the constraint was left unbuilt.

Fresh class per run (UQ-FRESH-CLASS-NAME), for the reason
MULTI-SLOT-UNIQUE-DECLARED-AFTER-OPEN-SCANS-STRICTLY documents.  The bad
node is seeded BEFORE the constraint is declared, so the canonicalizer
never runs on the commit path -- that path is deliberately intolerant."
  (with-uq-graph (g)
    (let* ((*package* (find-package :graph-db/test))
           (cls (uq-fresh-class-name "UQ-BOOM"))
           (mk (intern (format nil "MAKE-~A" cls))))
      (eval `(def-vertex ,cls () (a b) :graph-db-unique-test))
      (with-transaction ()
        (funcall mk :a "boom" :b "1")   ; the node the scan chokes on
        (funcall mk :a "ok"   :b "2")
        (funcall mk :a "ok"   :b "3"))
      (finishes
       (eval `(def-unique ,cls (a b) :graph-db-unique-test
                :canonicalize (uq-boom-canon nil))))
      (is (= 2 (uq-index-size g cls '(a b)))
          "the two good nodes were indexed, the bad one skipped")
      (signals graph-db:unique-constraint-violation
        (with-transaction () (funcall mk :a "ok" :b "3"))))))

(test multi-slot-unique-absent-index-is-not-conjured-empty
  "GH #129: after a build scan unwinds, the registry entry is absent -- and
absence must mean \"not built yet\" to EVERY path.  A commit used to
get-or-create an empty UIX (%UIX-CLAIM, then VALIDATE-UNIQUE-CONSTRAINTS),
which enforced nothing and permanently blocked the retry, because
%ENSURE-UNIQUE-TUPLE-BUILT short-circuits on registry presence.

The unwind is simulated by calling %UNREGISTER-UNIQUE-TUPLE-INDEX directly:
that is precisely the state the unwind arm leaves, and the residual triggers
for it (a heap or storage error mid-scan) are not reproducible in a test."
  (with-uq-graph (g)
    (with-transaction () (make-uq-claim :ns "ops" :ky "e1"))
    (graph-db::%unregister-unique-tuple-index g 'uq-claim '(ns ky))
    (is (null (uq-index-size g 'uq-claim '(ns ky)))
        "precondition: the constraint is unbuilt")
    (finishes (with-transaction () (make-uq-claim :ns "ops" :ky "e2")))
    (is (null (uq-index-size g 'uq-claim '(ns ky)))
        "a commit consults the registry; it does not populate it")
    (dolist (spec (graph-db::%registered-unique-tuple-specs g))
      (when (eq (graph-db::unique-tuple-spec-owner-name spec) 'uq-claim)
        (graph-db::%ensure-unique-tuple-built g spec)))
    (is (= 2 (uq-index-size g 'uq-claim '(ns ky)))
        "absence was recoverable: the rebuild scanned both nodes")
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-uq-claim :ns "ops" :ky "e1")))))
