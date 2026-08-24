;;;; Runtime schema (GH #172).  Spec:
;;;; docs/superpowers/specs/2026-08-24-runtime-schema-172-design.md
(in-package #:graph-db/test)

(def-suite runtime-schema-suite :in graph-db-suite
  :description "Class-from-metadata, manifest, materialize (GH #172).")
(in-suite runtime-schema-suite)

(def-vertex rs-static () ((label :type string)) :rs-store)
(def-edge rs-knows () () :rs-store)

(defmacro with-rs-store ((g) &body body)
  "One open :RS-STORE under a fresh system dir, bound to *GRAPH* too so
Prolog queries (which read *GRAPH*) work inside BODY."
  (let ((sys (gensym)) (d (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,d)
         (let ((graph-db::*system-directory* (namestring ,sys))
               (graph-db::*type-registry* nil))
           (let ((,g (make-graph :rs-store (namestring ,d)
                                 :buffer-pool-size 1000)))
             (unwind-protect
                  (let ((graph-db:*graph* ,g))
                    ,@body)
               (let ((live (graph-db:lookup-graph :rs-store)))
                 (when (and live (graph-db::graph-open-p live))
                   (let ((graph-db:*graph* live))
                     (ignore-errors
                      (close-graph live :snapshot-p nil)))))
               (collect-garbage))))))))

(test source-types-behave-unchanged-through-the-shared-path
  "R1 equivalence pin: after the refactor a def-vertex/def-edge type
still constructs, looks up, predicates, and answers its Prolog functor.
The full suite is the broad net; this is the focused canary."
  (with-rs-store (g)
    (let (a b e)
      (with-transaction ((graph-db::transaction-manager g))
        (setq a (make-rs-static :label "a")
              b (make-rs-static :label "b")
              e (make-rs-knows :from a :to b)))
      (is (rs-static-p a))
      (is (typep (lookup-rs-static (graph-db:id a)) 'rs-static))
      (is (rs-knows-p e))
      ;; The functor symbol lands in the class symbol's package (the
      ;; macro's *PACKAGE* at expansion == that package here).
      (is-true (gethash (intern "RS-KNOWS/2" :graph-db/test)
                        graph-db::*prolog-global-functors*))
      (is-true (fboundp (intern "RS-KNOWS/2" :graph-db/test)))
      (let ((hits (select (:flat nil) (?x ?y) (rs-knows ?x ?y))))
        (is (= 1 (length hits)))))))
