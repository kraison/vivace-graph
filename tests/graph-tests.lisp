;;;; Graph-integration tests: the public object-graph API on a real on-disk
;;;; graph -- def-vertex/def-edge schema, with-transaction, vertex/edge CRUD,
;;;; adjacency (which drives the ve indexes), deletion, and node update.
;;;;
;;;; The schema is defined once at load time against *integration-graph-name*;
;;;; each test builds a fresh graph of that name via WITH-TEST-GRAPH.

(in-package #:graph-db/test)

;; Start from a clean slate so reloading this file doesn't register the type
;; metadata more than once for our test graph.
(eval-when (:load-toplevel :execute)
  (setf (gethash *integration-graph-name* *schema-node-metadata*) nil))

(def-vertex g-person ()
  ((name :type string)
   (age))
  :graph-db-integration-test)

;; A subclass, for inheritance / subclass-filtering tests.
(def-vertex g-employee (g-person)
  ((title))
  :graph-db-integration-test)

;; For the dense float-vector serialization round trip.
(def-vertex g-embedded ()
  ((payload))
  :graph-db-integration-test)

;; Persistent slots carrying an :INITFORM.  No fixture declared one until
;; GH #128, which is why nothing caught an initform shadowing the stored
;; value on reopen.  GAP is deliberately never written, standing in for a
;; slot added to the schema after a node was already stored.
(def-vertex g-defaulted ()
  ((name :initform "DEFAULT")
   (gap  :initform "FILLER"))
  :graph-db-integration-test)

(def-edge g-knows ()
  ((since))
  :graph-db-integration-test)

(def-edge g-likes ()
  ()
  :graph-db-integration-test)

(def-suite graph-suite
  :description "Graph model: schema, transactions, CRUD, adjacency, deletion."
  :in graph-db-suite)

(in-suite graph-suite)

(test create-and-lookup-vertex
  "A vertex created in a transaction is retrievable afterward with its slot
values intact."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-g-person :name "Alice" :age 30))))
      (let ((v (lookup-vertex id)))
        (is-true v)
        (is (string= "Alice" (slot-value v 'name)))
        (is (= 30 (slot-value v 'age)))))))

(test vertex-count-by-type
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-person :name "C"))
    (is (= 3 (length (map-vertices #'identity g
                                   :collect-p t :vertex-type 'g-person))))))

(test lookup-missing-vertex-is-nil
  (with-test-graph (g)
    (is (null (lookup-vertex (gen-id))))))

(test edge-adjacency
  "An edge shows up on its source's outgoing set and its target's incoming
set, with the right endpoints and weight."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (make-g-knows :from a :to b :weight 2.5)))
      (let ((outs (outgoing-edges (lookup-vertex aid)))
            (ins (incoming-edges (lookup-vertex bid))))
        (is (= 1 (length outs)))
        (is (= 1 (length ins)))
        (is (equalp bid (to (first outs))))
        (is (equalp aid (from (first outs))))
        (is (= 2.5 (weight (first outs))))))))

(test outgoing-edges-filtered-by-type
  (with-test-graph (g)
    (let (aid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B"))
              (c (make-g-person :name "C")))
          (setq aid (id a))
          (make-g-knows :from a :to b)
          (make-g-likes :from a :to c)))
      (let ((a (lookup-vertex aid)))
        (is (= 2 (length (outgoing-edges a))))
        (is (= 1 (length (outgoing-edges a :edge-type 'g-knows))))
        (is (= 1 (length (outgoing-edges a :edge-type 'g-likes))))))))

(test mark-deleted-vertex
  "A deleted vertex is excluded from type queries and its typed lookup."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-g-person :name "Doomed"))))
      (with-transaction ()
        (mark-deleted (lookup-vertex id)))
      ;; typed lookup filters deleted nodes
      (is (null (lookup-g-person id)))
      (is (zerop (length (map-vertices #'identity g
                                       :collect-p t :vertex-type 'g-person)))))))

(test mark-deleted-edge-drops-from-adjacency
  (with-test-graph (g)
    (let (aid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B")))
          (setq aid (id a))
          (make-g-knows :from a :to b)))
      (with-transaction ()
        (mark-deleted (first (outgoing-edges (lookup-vertex aid)))))
      (is (zerop (length (outgoing-edges (lookup-vertex aid))))))))

(test update-vertex-slot
  "Copy-modify-save inside a transaction persists the new slot value."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-g-person :name "Old" :age 1))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'name) "New")
          (save v)))
      (is (string= "New" (slot-value (lookup-vertex id) 'name)))
      ;; untouched slot survives
      (is (= 1 (slot-value (lookup-vertex id) 'age))))))

(test subclass-membership
  "An employee is a person; subclass filtering and inherited slots work."
  (with-test-graph (g)
    (let (eid)
      (with-transaction ()
        (setq eid (id (make-g-employee :name "Boss" :title "CEO"))))
      ;; vertex-type person, including subclasses, sees the employee
      (is (= 1 (length (map-vertices #'identity g :collect-p t
                                              :vertex-type 'g-person
                                              :include-subclasses-p t))))
      ;; and it is retrievable as an employee with both its own and inherited slots
      (let ((e (lookup-vertex eid)))
        (is (string= "Boss" (slot-value e 'name)))
        (is (string= "CEO" (slot-value e 'title)))))))

;;; ---------------------------------------------------------------------------
;;; map-vertices / map-edges must use their GRAPH argument, not *graph*
;;;
;;; The all-types (no :vertex-type / :edge-type) branch used to read the dynamic
;;; *graph* instead of the passed graph, so mapping a graph that isn't the
;;; current *graph* errored (NO-APPLICABLE-METHOD on VERTEX-TABLE/EDGE-TABLE with
;;; NIL).  That also broke CLOSE-GRAPH's default snapshot (snapshot ->
;;; check-data-integrity -> map-vertices) on a non-current graph.
;;; ---------------------------------------------------------------------------

(test map-all-uses-graph-arg-not-dynamic
  "map-vertices / map-edges (all-types branch) honor their GRAPH argument even
when *graph* is bound to a different graph (or nil)."
  (with-test-graph (g)
    (with-transaction ()
      (let ((a (make-g-person :name "A"))
            (b (make-g-person :name "B")))
        (make-g-knows :from a :to b)))
    ;; Rebind *graph* away from G; the maps must still see G's contents.
    (let ((*graph* nil))
      (is (= 2 (length (map-vertices #'identity g :collect-p t))))
      (is (= 1 (length (map-edges #'identity g :collect-p t)))))))

;;; ---------------------------------------------------------------------------
;;; TRAVERSE / EDGE-EXISTS-P / MAKE-<type> must operate on their :GRAPH argument
;;; -- resolving both node ids AND schema type-ids there -- even when *graph* is
;;; bound to a DIFFERENT open graph.  This is the wrong-graph (*graph*) class of
;;; bug: the index / adjacency is read from :GRAPH, but id/type resolution used
;;; to leak to the dynamic *graph* (traverse endpoints via LOOKUP-VERTEX,
;;; edge-exists-p's type lookup + ACTIVE-EDGE-P, MAKE-<type>'s type-id, and the
;;; ve-index index-list heap).  With *graph* pointing at a graph that lacks the
;;; g-* schema, the leak is unmistakable.
;;; ---------------------------------------------------------------------------

(test cross-graph-ops-target-explicit-graph
  "Graph ops with an explicit :GRAPH resolve node ids and schema type-ids in that
graph, not the ambient *graph*, when the two differ."
  (with-test-graph (b)
    (let (aid bid cid)
      ;; Populate B (a-knows->b, a-knows->c); *graph* is B inside with-test-graph.
      (with-transaction ()
        (let ((va (make-g-person :name "A"))
              (vb (make-g-person :name "B"))
              (vc (make-g-person :name "C")))
          (setq aid (id va) bid (id vb) cid (id vc))
          (make-g-knows :from va :to vb)
          (make-g-knows :from va :to vc)))
      ;; A second, unrelated open graph whose schema does NOT know the g-* types.
      (with-temp-directory (dir-a)
        (let ((a (make-graph :cross-graph-decoy (namestring dir-a)
                             :buffer-pool-size 1000)))
          (unwind-protect
               ;; Bind *graph* to the WRONG graph; every op below names B.
               (let ((*graph* a))
                 ;; traverse: edge endpoints must resolve in B.
                 (is (= 2 (length (traverse (lookup-vertex aid :graph b)
                                            :graph b :direction :out
                                            :edge-type 'g-knows))))
                 ;; edge-exists-p: type-id + endpoint liveness must resolve in B.
                 (is (graph-db:edge-exists-p 'g-knows
                                             (lookup-vertex aid :graph b)
                                             (lookup-vertex bid :graph b)
                                             :graph b))
                 ;; make-<type>: type-id must come from B's schema (pre-fix: erred
                 ;; on (node-type-id NIL) because it looked g-knows up in *graph*).
                 (with-transaction ((graph-db::transaction-manager b))
                   (make-g-knows :from (lookup-vertex bid :graph b)
                                 :to (lookup-vertex cid :graph b)
                                 :graph b))
                 (is (graph-db:edge-exists-p 'g-knows
                                             (lookup-vertex bid :graph b)
                                             (lookup-vertex cid :graph b)
                                             :graph b))
                 ;; ve-index index-list heap: B's adjacency stays sound.
                 (is (= 2 (length (outgoing-edges (lookup-vertex aid :graph b)
                                                  :graph b))))
                 (is (= 1 (length (outgoing-edges (lookup-vertex bid :graph b)
                                                  :graph b)))))
            (ignore-errors (close-graph a :snapshot-p nil))
            (collect-garbage)))))))

(test close-graph-default-snapshot-without-current-graph
  "CLOSE-GRAPH with the default :SNAPSHOT-P T succeeds even when *graph* is not
bound to the graph being closed (snapshot walks the graph via map-vertices)."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (let ((*graph* g))
        (with-transaction () (make-g-person :name "Solo")))
      ;; *graph* is NOT bound to g here; default snapshot must not crash.
      (finishes (close-graph g))
      (collect-garbage))))

;;; ---------------------------------------------------------------------------
;;; slot-boundp / slot-makunbound on persistent slots (issue #41)
;;;
;;; Persistent slot values live in the node's DATA alist, not in real CLOS
;;; slots, so SLOT-BOUNDP / SLOT-MAKUNBOUND must consult the alist.
;;; ---------------------------------------------------------------------------

(test slot-boundp-on-persistent-slots
  "slot-boundp on a persistent slot reflects whether it has a stored value: a
set slot is bound, an unset one is not."
  (with-test-graph (g)
    (let (v)
      (with-transaction () (setq v (make-g-person :name "A")))  ; no :age
      (is (slot-boundp v 'name) "a set persistent slot is bound")
      (is (not (slot-boundp v 'age)) "an unset persistent slot is unbound"))))

(test slot-boundp-distinguishes-nil-value
  "A persistent slot explicitly set to NIL is bound -- slot-boundp tests
presence, not non-NIL value."
  (with-test-graph (g)
    (let (v)
      (with-transaction ()
        (setq v (make-g-person :name "A"))
        (setf (slot-value v 'age) nil))    ; explicit NIL
      (is (slot-boundp v 'age) "a slot explicitly set to NIL is bound")
      (is (null (slot-value v 'age)) "...and its value is NIL"))))

(test slot-boundp-survives-reopen
  "slot-boundp reflects the stored data after a close + reopen from disk
(exercises the maybe-init-node-data materialization path)."
  (with-temp-directory (dir)
    (let (id)
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction () (setq id (id (make-g-person :name "A")))))  ; no age
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let* ((*graph* g) (v (lookup-vertex id)))
               (is (slot-boundp v 'name) "set slot still bound after reopen")
               (is (not (slot-boundp v 'age)) "unset slot still unbound after reopen")
               (is (string= "A" (slot-value v 'name))))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test stored-value-beats-initform-across-reopen
  "A persistent slot's STORED value survives a reopen even when the slot
declares an :INITFORM, and an initform still fills a slot the stored data
has no entry for -- stored data wins per slot, the initform fills gaps
(GH #128).  Before the fix the initform populated the DATA alist during
CHANGE-NODE-CLASS, so MAYBE-INIT-NODE-DATA's (NULL (DATA NODE)) guard
declined to deserialize and every such slot read back as its default."
  (with-temp-directory (dir)
    (let (id)
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (setq id (id (make-g-defaulted :name "STORED")))))
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let* ((*graph* g) (v (lookup-vertex id)))
               (is (string= "STORED" (slot-value v 'name))
                   "stored value must beat the initform after reopen")
               (is (string= "FILLER" (slot-value v 'gap))
                   "initform still fills a slot the stored data lacks"))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test stored-value-beats-initform-in-scans
  "The same holds on the scan path, not only LOOKUP-VERTEX -- MAP-VERTICES
materializes through the same guard (GH #128)."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (make-g-defaulted :name "SCANNED")))
        (close-graph g :snapshot-p nil)))
    (let ((g (open-graph *integration-graph-name* (namestring dir))))
      (unwind-protect
           (let ((seen '()))
             (map-vertices (lambda (v) (push (slot-value v 'name) seen))
                           g :vertex-type 'g-defaulted)
             (is (equal '("SCANNED") seen)
                 "a typed scan must see the stored value, not the initform"))
        (close-graph g :snapshot-p nil)
        (collect-garbage)))))

(test slot-makunbound-clears-persistent-slot
  "slot-makunbound on a persistent slot removes its stored value (and stays
removed across a reopen)."
  (with-temp-directory (dir)
    (let (id)
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction () (setq id (id (make-g-person :name "A" :age 30))))
               (with-transaction ()
                 (let ((c (copy (lookup-vertex id))))
                   (slot-makunbound c 'age)
                   (save c)))
               (let ((v (lookup-vertex id)))
                 (is (not (slot-boundp v 'age)) "makunbound -> slot is unbound")
                 (is (null (node-slot-value v :age)) "...and the value is gone")
                 (is (slot-boundp v 'name) "the other slot is untouched")))
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((*graph* g))
               (is (not (slot-boundp (lookup-vertex id) 'age))
                   "still unbound after reopen")
               (is (slot-boundp (lookup-vertex id) 'name)))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test slot-boundp-meta-slot-regression
  "slot-boundp on a meta slot (e.g. DATA) still uses the standard method
(mirrors backup.lisp's (slot-boundp v 'data))."
  (with-test-graph (g)
    (let (v)
      (with-transaction () (setq v (make-g-person :name "A")))
      ;; The meta slot is GRAPH-DB::DATA (the base node class is in graph-db);
      ;; this mirrors backup.lisp's own (slot-boundp v 'data) inside that package.
      (is (slot-boundp v 'graph-db::data) "the meta DATA slot is bound"))))

(test slot-boundp-inherited-persistent-slot
  "slot-boundp works on a persistent slot inherited from a parent type."
  (with-test-graph (g)
    (let (v)
      (with-transaction () (setq v (make-g-employee :name "A" :title "Boss")))
      (is (slot-boundp v 'name)  "inherited slot, set -> bound")
      (is (slot-boundp v 'title) "own slot, set -> bound")
      (is (not (slot-boundp v 'age)) "inherited slot, unset -> unbound"))))

;;; ---------------------------------------------------------------------------
;;; dense float-vector slot durability (Task 3: serialization round-trip gate)
;;; ---------------------------------------------------------------------------

(test float-vector-slot-survives-close-and-reopen
  "A single-float vector stored in a vertex slot reads back bit-exactly after a
close/reopen cycle."
  (with-temp-directory (dir)
    (let ((v (make-array 512 :element-type 'single-float))
          (id nil))
      (dotimes (i 512)
        (setf (aref v i) (coerce (/ i 512.0) 'single-float)))
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setf id (id (make-g-embedded :payload v)))))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let* ((*graph* g)
                    (back (slot-value (lookup-vertex id :graph g) 'payload)))
               (is (typep back '(simple-array single-float (*)))
                   "reopened slot has type ~S" (type-of back))
               (is (= 512 (length back)))
               (is (every #'= v back)))
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))

(test octet-vector-slot-survives-close-and-reopen
  "An (unsigned-byte 8) octet vector stored in a vertex slot reads back bit-exactly after a close/reopen cycle (issue #68)."
  (with-temp-directory (dir)
    (let ((bytes (make-array 6 :element-type '(unsigned-byte 8)
                               :initial-contents '(#x41 #x42 #x43 #x44 #x45 #x46)))
          (id nil))
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setf id (id (make-g-embedded :payload bytes)))))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let* ((*graph* g)
                    (back (slot-value (lookup-vertex id :graph g) 'payload)))
               (is (typep back '(vector (unsigned-byte 8)))
                   "reopened slot has type ~S" (type-of back))
               (is (= 6 (length back)))
               (is (equalp bytes back)))
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))


;;; ---------------------------------------------------------------------------
;;; A failed snapshot must not abort CLOSE-GRAPH (GH #120)
;;;
;;; CLOSE-GRAPH deregisters the graph and THEN snapshots, so a snapshot that
;;; signals used to strand every mmap open with .dirty still on disk -- forcing
;;; recovery on a graph whose data was intact, and leaving nothing able to find
;;; it by name to retry.  For a disk graph the snapshot is a logical backup, not
;;; the durability mechanism, so the close must complete and report the failure.
;;; ---------------------------------------------------------------------------

;; Two failure shapes.  The STORAGE one is the point of the exercise: SBCL's
;; HEAP-EXHAUSTED-ERROR is a STORAGE-CONDITION, which is NOT an ERROR subtype,
;; so a guard written on ERROR would miss exactly the failure GH #119 is about.
(define-condition snapshot-test-error (error) ()
  (:report (lambda (c s) (declare (ignore c))
             (format s "simulated snapshot failure"))))

(define-condition snapshot-test-storage-condition (storage-condition) ()
  (:report (lambda (c s) (declare (ignore c))
             (format s "simulated heap exhaustion during snapshot"))))

(defmacro with-failing-snapshot ((condition-class) &body body)
  "Run BODY with GRAPH-DB:SNAPSHOT replaced by one that signals CONDITION-CLASS."
  (let ((orig (gensym "ORIG")))
    `(let ((,orig (fdefinition 'graph-db:snapshot)))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db:snapshot)
                    (lambda (graph &rest args)
                      (declare (ignore graph args))
                      (error ',condition-class)))
              ,@body)
         (setf (fdefinition 'graph-db:snapshot) ,orig)))))

(defun dirty-file-present-p (dir)
  (probe-file (format nil "~A/.dirty" (namestring dir))))

(test close-graph-completes-when-the-snapshot-signals
  "A snapshot that signals must not abort the close: .dirty is removed, the
mmaps are released, and the graph reopens WITHOUT a recovery pass."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (let ((*graph* g))
        (with-transaction () (make-g-person :name "Survivor")))
      (is (dirty-file-present-p dir) "sanity: an open graph is marked dirty")
      (let ((problem :none))
        (handler-bind ((warning #'muffle-warning))
          (with-failing-snapshot (snapshot-test-error)
            (multiple-value-bind (returned p) (close-graph g)
              (is (eq g returned) "CLOSE-GRAPH still returns the graph")
              (setq problem p))))
        (is (typep problem 'condition)
            "the snapshot failure must be REPORTED as the second value, got ~S"
            problem))
      ;; The whole teardown ran.
      (is (null (graph-db::graph-open-p g)) "graph must be marked closed")
      (is (null (graph-db::heap g)) "the heap must be released")
      (is (not (dirty-file-present-p dir))
          ".dirty must be gone -- an intact graph must not be forced through ~
           recovery because its BACKUP failed")
      (collect-garbage)
      ;; ...and it really does reopen, with the data intact.
      (let ((g2 (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 1 (length (map-vertices #'identity g2 :collect-p t)))
                   "the node written before the failed snapshot must survive"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test close-graph-survives-a-storage-condition
  "The guard is on SERIOUS-CONDITION, not ERROR: SBCL's HEAP-EXHAUSTED-ERROR is
a STORAGE-CONDITION, which is NOT an ERROR subtype, and heap exhaustion on a
large graph is the failure this exists for (GH #119).  A handler on ERROR would
let this one through and strand the graph."
  (is (not (subtypep 'storage-condition 'error))
      "premise: STORAGE-CONDITION must not be an ERROR subtype")
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (let ((*graph* g))
        (with-transaction () (make-g-person :name "Storage")))
      (handler-bind ((warning #'muffle-warning))
        (with-failing-snapshot (snapshot-test-storage-condition)
          (multiple-value-bind (returned problem) (close-graph g)
            (declare (ignore returned))
            (is (typep problem 'storage-condition)
                "a STORAGE-CONDITION must be caught and reported, got ~S"
                problem))))
      (is (null (graph-db::graph-open-p g)))
      (is (not (dirty-file-present-p dir)))
      (collect-garbage))))

(test close-graph-warns-once-the-close-is-complete
  "The failure is never swallowed: a WARNING is signalled -- after the teardown,
so it cannot be read as the close itself having failed."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (warned nil))
      (let ((*graph* g))
        (with-transaction () (make-g-person :name "Warned")))
      (handler-bind ((warning (lambda (w)
                                ;; The graph must ALREADY be closed when the
                                ;; warning arrives.
                                (setq warned (list (graph-db::graph-open-p g)
                                                   (dirty-file-present-p dir)))
                                (muffle-warning w))))
        (with-failing-snapshot (snapshot-test-error)
          (close-graph g)))
      (is (not (null warned)) "a warning must be signalled")
      (when warned
        (is (null (first warned))
            "the graph must already be closed when the warning is signalled")
        (is (null (second warned))
            ".dirty must already be gone when the warning is signalled"))
      (collect-garbage))))

(test close-graph-reports-data-integrity-issues
  "SNAPSHOT reports integrity problems by RETURNING :DATA-INTEGRITY-ISSUES
rather than signalling; CLOSE-GRAPH discarded that, so such a graph closed with
no snapshot taken and no sign of it.  It is now the second value."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (orig (fdefinition 'graph-db:snapshot)))
      (let ((*graph* g))
        (with-transaction () (make-g-person :name "Integrity")))
      (handler-bind ((warning #'muffle-warning))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db:snapshot)
                     (lambda (graph &rest args)
                       (declare (ignore graph args))
                       :data-integrity-issues))
               (multiple-value-bind (returned problem) (close-graph g)
                 (declare (ignore returned))
                 (is (eq :data-integrity-issues problem))))
          (setf (fdefinition 'graph-db:snapshot) orig)))
      (is (null (graph-db::graph-open-p g)))
      (is (not (dirty-file-present-p dir)))
      (collect-garbage))))

(test close-graph-clean-snapshot-reports-no-problem
  "The ordinary path is unchanged: a successful snapshotting close returns a NIL
second value and signals nothing."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (let ((*graph* g))
        (with-transaction () (make-g-person :name "Clean")))
      (let ((*graph* g))
        (multiple-value-bind (returned problem) (close-graph g)
          (is (eq g returned))
          (is (null problem) "a successful snapshot must report no problem")))
      (is (null (graph-db::graph-open-p g)))
      (is (not (dirty-file-present-p dir)))
      (collect-garbage))))
