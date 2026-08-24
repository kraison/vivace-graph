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

(test ensure-namespace-is-cheap-and-idempotent
  (with-rs-store (g)
    g
    (let ((p1 (graph-db:ensure-namespace "RS-TLM" :nicknames '("RST")))
          (p2 (graph-db:ensure-namespace "RS-TLM")))
      (is (eq p1 p2))
      (is (packagep p1))
      ;; No files, no store: the store registry did not grow.
      (is (null (graph-db:lookup-graph :rs-tlm))))))

(test create-vertex-type-yields-a-working-class
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (let ((class (graph-db:create-vertex-type
                  "RS-TLM:READING"
                  '((sensor-id :type string)
                    (value :type double-float))
                  :default-store :rs-store)))
      (is (typep class 'graph-db::node-class))
      (with-transaction ((graph-db::transaction-manager g))
        (funcall (intern "MAKE-READING" :rs-tlm)
                 :sensor-id "s1" :value 1.5d0))
      (let* ((sym (intern "READING" :rs-tlm))
             (hits (graph-db:map-vertices #'identity g :collect-p t
                                          :vertex-type sym)))
        (is (= 1 (length hits)))
        (is (string= "s1"
                     (funcall (intern "SENSOR-ID" :rs-tlm)
                              (first hits))))))))

(test create-edge-type-installs-functors-and-places-by-default
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (graph-db:create-edge-type "RS-TLM:FEEDS" '()
                               :default-store :rs-store)
    (let (a b)
      (with-transaction ((graph-db::transaction-manager g))
        (setq a (funcall (intern "MAKE-READING" :rs-tlm) :value 1d0)
              b (funcall (intern "MAKE-READING" :rs-tlm) :value 2d0))
        (funcall (intern "MAKE-FEEDS" :rs-tlm)
                 :from (graph-db:id a) :to (graph-db:id b)))
      (is-true (gethash (intern "FEEDS/2" :rs-tlm)
                        graph-db::*prolog-global-functors*)))))

(test manifest-records-both-provenances-and-tolerates-damage
  (with-rs-store (g)
    g  ; RS-STATIC/RS-KNOWS registered at load time = :source rows
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (multiple-value-bind (ns types)
        (graph-db::read-schema-manifest graph-db::*system-directory*)
      (is (find "RS-TLM" ns :key (lambda (r) (getf r :namespace))
                :test #'string-equal))
      (let ((row (find (intern "READING" :rs-tlm) types
                       :key (lambda (r) (getf r :type)))))
        (is-true row)
        (is (eq :runtime (getf row :provenance)))
        (is (eq :rs-store (getf row :default-store))))
      (is (eq :source
              (getf (find 'rs-static types
                          :key (lambda (r) (getf r :type)))
                    :provenance))))
    ;; Torn tail: append garbage, read again, intact rows survive.
    (with-open-file (s (graph-db::%schema-manifest-file)
                       :direction :output :if-exists :append)
      (format s "(:type RS-TORN"))
    (multiple-value-bind (ns types)
        (graph-db::read-schema-manifest graph-db::*system-directory*)
      ns
      (is (find (intern "READING" :rs-tlm) types
                :key (lambda (r) (getf r :type)))))))
