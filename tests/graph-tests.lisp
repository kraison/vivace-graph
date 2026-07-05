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
