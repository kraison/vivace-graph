;;;; Packages as namespaces (GH #167).  Spec:
;;;; docs/superpowers/specs/2026-08-23-packages-167-design.md
(in-package #:graph-db/test)

(def-suite package-namespace-suite :in graph-db-suite
  :description "Placement defaults, lazy adoption, occupancy (GH #167).")
(in-suite package-namespace-suite)

;; Two stores under one *SYSTEM-DIRECTORY* so type-ids come from one
;; registry.  Classes are defined at load time below (DEF-VERTEX is a
;; top-level macro); each test opens fresh store directories.
(defparameter *pn-store-a* :pn-store-a)
(defparameter *pn-store-b* :pn-store-b)

(def-vertex pn-item () ((label :type string)) :pn-store-a)
(def-edge pn-link () () :pn-store-b)
;; Scratch class registered under BOTH stores (#186 pattern) for the
;; preferred-store-determinism test below; never written to, so
;; neither store need be open.
(def-vertex pn-dual () () :pn-store-a)
(def-vertex pn-dual () () :pn-store-b)

;; Two scratch packages, same symbol-name TWIN, both declared into
;; store A -- DEF-VERTEX interns MAKE-<NAME>/<NAME>-P in the class
;; symbol's own package, so each gets its own MAKE-TWIN (GH #167, #172).
(defpackage #:pn-pkg-one (:use #:cl #:graph-db))
(defpackage #:pn-pkg-two (:use #:cl #:graph-db))
(in-package #:pn-pkg-one)
(graph-db:def-vertex twin () () :pn-store-a)
(in-package #:pn-pkg-two)
(graph-db:def-vertex twin () () :pn-store-a)
(in-package #:graph-db/test)

(defmacro with-pn-stores ((ga gb) &body body)
  "Two open stores :PN-STORE-A / :PN-STORE-B under a fresh system dir.
Resets both metadata lists' instantiation state by reopening from empty
directories each time; the DEF-VERTEX/DEF-EDGE above re-register their
metas at load time, which is all the constructors need."
  (let ((sys (gensym)) (da (gensym)) (db (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,da)
         (with-temp-directory (,db)
           (let ((graph-db::*system-directory* (namestring ,sys))
                 (graph-db::*type-registry* nil))
             (let ((,ga (make-graph :pn-store-a (namestring ,da)
                                    :buffer-pool-size 1000))
                   (,gb nil))
               (unwind-protect
                    (progn
                      (setq ,gb (make-graph :pn-store-b (namestring ,db)
                                            :buffer-pool-size 1000))
                      ,@body)
                 (dolist (name '(:pn-store-b :pn-store-a))
                   (let ((live (graph-db:lookup-graph name)))
                     (when (and live (graph-db::graph-open-p live))
                       (let ((graph-db:*graph* live))
                         (ignore-errors
                          (close-graph live :snapshot-p nil))))))
                 (collect-garbage)))))))))

(test constructor-defaults-to-the-declared-store-not-graph
  "R1 ablation: *GRAPH* bound to store B, no :GRAPH argument -- the
PN-ITEM lands in store A (its declared store).  Under the old
*GRAPH* default this vertex would land in B."
  (with-pn-stores (ga gb)
    (let ((graph-db:*graph* gb))
      (with-transaction ((graph-db::transaction-manager ga))
        (make-pn-item :label "in-a")))
    (is (= 1 (length (graph-db:map-vertices #'identity ga :collect-p t
                                            :vertex-type 'pn-item))))
    (is (= 0 (length (graph-db:map-vertices #'identity gb :collect-p t
                                            :vertex-type 'pn-item))))))

(test constructor-explicit-graph-overrides-the-default
  "R1: :GRAPH overrides the class's declared default store; PN-ITEM's
type is adopted into GB lazily at this first write (R3)."
  (with-pn-stores (ga gb)
    ga
    (with-transaction ((graph-db::transaction-manager gb))
      (make-pn-item :label "in-b" :graph gb))
    (is (= 1 (length (graph-db:map-vertices #'identity gb :collect-p t
                                            :vertex-type 'pn-item))))))

(test constructor-refuses-when-the-default-store-is-closed
  "R1's third clause: no :GRAPH and :PN-STORE-A closed -- refuse by
name, never fall back to *GRAPH* (the spec-rejected behaviour)."
  (with-pn-stores (ga gb)
    (let ((graph-db:*graph* gb))
      (let ((graph-db:*graph* ga)) (close-graph ga :snapshot-p nil))
      (let ((c (handler-case (progn (make-pn-item :label "x") nil)
                 (graph-db:default-store-not-open-error (e) e))))
        (is-true c)
        (when c
          (is (eq 'pn-item (graph-db:default-store-not-open-class c)))
          (is (eq :pn-store-a
                  (graph-db:default-store-not-open-store c)))))
      ;; And no vertex leaked into the ambient store B.
      (is (= 0 (length (graph-db:map-vertices #'identity gb :collect-p t
                                              :vertex-type 'pn-item)))))))

(test edge-places-by-its-own-class-default
  "PN-LINK's declared store is B: with both endpoints in A and *GRAPH*
bound to A, the edge still lands in B (spec 4: an edge is a node and is
placed the same way)."
  (with-pn-stores (ga gb)
    (let (v1 v2)
      (with-transaction ((graph-db::transaction-manager ga))
        (setq v1 (make-pn-item :label "v1")
              v2 (make-pn-item :label "v2")))
      (let ((graph-db:*graph* ga))
        (with-transaction ((graph-db::transaction-manager gb))
          (make-pn-link :from (graph-db:id v1) :to (graph-db:id v2))))
      (is (= 1 (length (graph-db:map-edges #'identity gb :collect-p t
                                           :edge-type 'pn-link))))
      (is (= 0 (length (graph-db:map-edges #'identity ga :collect-p t
                                           :edge-type 'pn-link)))))))

(test class-is-instantiable-in-a-second-store
  "cl-llm#20's acceptance: PN-ITEM (declared store A) written into B via
explicit :GRAPH -- B adopts the type at first write, both stores hold
their own instances, and the type-id is the same system-wide id."
  (with-pn-stores (ga gb)
    (with-transaction ((graph-db::transaction-manager ga))
      (make-pn-item :label "a1"))
    (with-transaction ((graph-db::transaction-manager gb))
      (make-pn-item :label "b1" :graph gb))
    (is (= 1 (length (graph-db:map-vertices #'identity ga :collect-p t
                                            :vertex-type 'pn-item))))
    (is (= 1 (length (graph-db:map-vertices #'identity gb :collect-p t
                                            :vertex-type 'pn-item))))
    (let ((ma (graph-db:lookup-node-type-by-name 'pn-item :vertex
                                                 :graph ga))
          (mb (graph-db:lookup-node-type-by-name 'pn-item :vertex
                                                 :graph gb)))
      (is (= (graph-db::node-type-id ma) (graph-db::node-type-id mb))))))

(test adopted-type-survives-reopen
  "The adoption persisted via the store's own schema.dat: close B,
reopen it, and the foreign-typed node reads back typed."
  (with-pn-stores (ga gb)
    ga
    (let (id (loc (namestring (graph-db::location gb))))
      (with-transaction ((graph-db::transaction-manager gb))
        (setq id (graph-db:id (make-pn-item :label "b1" :graph gb))))
      (let ((graph-db:*graph* gb)) (close-graph gb :snapshot-p nil))
      (let ((gb2 (open-graph :pn-store-b loc)))
        (let ((v (graph-db:lookup-vertex id :graph gb2)))
          (is (typep v 'pn-item))
          (is (string= "b1" (label v))))))))

(test two-packages-share-a-symbol-name-without-collision
  "Distinct packages, same symbol-name, both usable -- pinned end to
end, not just at definition."
  (with-pn-stores (ga gb)
    gb
    ;; The classes are defined at load time above in two scratch
    ;; packages; both declare store A.
    (with-transaction ((graph-db::transaction-manager ga))
      (funcall (intern "MAKE-TWIN" :pn-pkg-one) :graph ga)
      (funcall (intern "MAKE-TWIN" :pn-pkg-two) :graph ga))
    (let ((c1 (find-class (intern "TWIN" :pn-pkg-one)))
          (c2 (find-class (intern "TWIN" :pn-pkg-two))))
      (is (not (eq c1 c2))))
    (is (= 1 (length (graph-db:map-vertices
                      #'identity ga :collect-p t
                      :vertex-type (intern "TWIN" :pn-pkg-one)))))
    (is (= 1 (length (graph-db:map-vertices
                      #'identity ga :collect-p t
                      :vertex-type (intern "TWIN" :pn-pkg-two)))))))

(test a-class-registered-in-two-stores-keeps-both-metas
  "PN-DUAL is declared under both :PN-STORE-A and :PN-STORE-B (#186):
both metas survive, and %FIND-REGISTERED-NODE-TYPE's PREFER-STORE
argument deterministically picks the requested store's own meta
(GH #167 review round 1, reversed)."
  (let ((meta-a (find 'pn-dual
                      (gethash :pn-store-a
                               graph-db::*schema-node-metadata*)
                      :key #'graph-db::node-type-name))
        (meta-b (find 'pn-dual
                      (gethash :pn-store-b
                               graph-db::*schema-node-metadata*)
                      :key #'graph-db::node-type-name)))
    (is-true meta-a)
    (is-true meta-b)
    (when (and meta-a meta-b)
      (is (eq :pn-store-a (graph-db::node-type-graph-name meta-a)))
      (is (eq :pn-store-b (graph-db::node-type-graph-name meta-b)))
      (is (eq meta-a
              (graph-db::%find-registered-node-type
               'pn-dual :vertex :pn-store-a)))
      (is (eq meta-b
              (graph-db::%find-registered-node-type
               'pn-dual :vertex :pn-store-b))))))

(test edge-occupancy-tracks-stores-and-defaults-to-no-hint
  "PN-LINK instantiated into B (its default) and adopted by A: the
occupancy set names both; an undefined class name yields NIL (= sweep,
the fail-safe)."
  (with-pn-stores (ga gb)
    (let (v1 v2)
      (with-transaction ((graph-db::transaction-manager ga))
        (setq v1 (make-pn-item :label "v1")
              v2 (make-pn-item :label "v2")))
      (with-transaction ((graph-db::transaction-manager gb))
        (make-pn-link :from (graph-db:id v1) :to (graph-db:id v2)))
      (with-transaction ((graph-db::transaction-manager ga))
        (make-pn-link :from (graph-db:id v1) :to (graph-db:id v2)
                      :graph ga))
      (is (null (set-difference '(:pn-store-a :pn-store-b)
                                (graph-db:edge-type-stores 'pn-link))))
      (is (null (graph-db:edge-type-stores 'pn-no-such-edge))))))

(test edge-occupancy-persists-and-tolerates-a-torn-tail
  "A fresh image (simulated: clear the in-image cache) reloads the set
from edge-occupancy.dat; a torn final line is dropped, not an error."
  (with-pn-stores (ga gb)
    (let (v1 v2)
      (with-transaction ((graph-db::transaction-manager ga))
        (setq v1 (make-pn-item :label "v1")
              v2 (make-pn-item :label "v2")))
      (with-transaction ((graph-db::transaction-manager gb))
        (make-pn-link :from (graph-db:id v1) :to (graph-db:id v2)))
      (graph-db::%clear-edge-occupancy-cache)
      (is (member :pn-store-b (graph-db:edge-type-stores 'pn-link)))
      (let ((file (graph-db::%edge-occupancy-file)))
        (with-open-file (out file :direction :output
                             :if-exists :append)
          (format out "(PN-TORN"))
        (graph-db::%clear-edge-occupancy-cache)
        (is (member :pn-store-b
                    (graph-db:edge-type-stores 'pn-link)))))))

(test edge-occupancy-load-failure-degrades-to-no-hint
  "%LOAD-EDGE-OCCUPANCY must never signal: point the sidecar path at a
directory (PROBE-FILE succeeds, WITH-OPEN-FILE :INPUT does not) and
confirm the lookup still returns cleanly with no hint (GH #167, final
review I1)."
  (with-pn-stores (ga gb)
    ga gb
    (let ((orig (fdefinition 'graph-db::%edge-occupancy-file)))
      (unwind-protect
           (progn
             (setf (fdefinition 'graph-db::%edge-occupancy-file)
                   (lambda () (uiop:temporary-directory)))
             (graph-db::%clear-edge-occupancy-cache)
             (is (null (graph-db:edge-type-stores 'pn-link))))
        (setf (fdefinition 'graph-db::%edge-occupancy-file) orig)
        (graph-db::%clear-edge-occupancy-cache)))))

(test edge-occupancy-append-failure-does-not-abort-the-write
  "A failed sidecar append (disk full, permissions, fd exhaustion) must
degrade to in-image-only, never abort the caller's real edge write --
R4's fail-safe applies to the WRITE side too, not only reads (GH #167,
review round 1).  PN-LINK's declared store (:PN-STORE-B) is already
instantiated by the time this test's body runs (MAKE-GRAPH eagerly
instantiates a store's OWN declared types), so the failure is injected
around the lazy ADOPTION into :PN-STORE-A instead -- that is the write
path that actually reaches %NOTE-EDGE-OCCUPANCY after this test starts."
  (with-pn-stores (ga gb)
    (let (v1 v2 (orig (fdefinition 'graph-db::%edge-occupancy-file)))
      (with-transaction ((graph-db::transaction-manager ga))
        (setq v1 (make-pn-item :label "v1")
              v2 (make-pn-item :label "v2")))
      (unwind-protect
           (progn
             ;; %EDGE-OCCUPANCY-FILE now names a path under a directory
             ;; that does not exist, so the append inside
             ;; %NOTE-EDGE-OCCUPANCY signals a file error.
             (setf (fdefinition 'graph-db::%edge-occupancy-file)
                   (lambda ()
                     (merge-pathnames
                      "edge-occupancy.dat"
                      #P"/nonexistent-167-review-round-1/")))
             (is (eq :made
                     (with-transaction ((graph-db::transaction-manager ga))
                       (make-pn-link :from (graph-db:id v1)
                                     :to (graph-db:id v2)
                                     :graph ga)
                       :made)))
             ;; Still answers from the in-image cache the PUSH populated
             ;; before the append failed -- checked while the swap is
             ;; still in effect, so no reload masks a real regression.
             (is (member :pn-store-a (graph-db:edge-type-stores 'pn-link))))
        (setf (fdefinition 'graph-db::%edge-occupancy-file) orig)))))
