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

;; Two scratch packages, same symbol-name TWIN, both declared into
;; store A -- DEF-VERTEX interns MAKE-<NAME>/<NAME>-P in *PACKAGE* at
;; expansion time, so each package gets its own MAKE-TWIN (GH #167).
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
