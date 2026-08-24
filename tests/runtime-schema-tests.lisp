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

;;; ---------------------------------------------------------------------
;;; Fix round 1 (review of the first submission): I1, I2, I3.
;;; ---------------------------------------------------------------------

(test ensure-namespace-manifest-keeps-nicknames-across-calls
  "I1: a later no-nickname ENSURE-NAMESPACE call must not make the
manifest's last row for the namespace forget an earlier call's
nicknames -- the row must record the UNIONED set actually applied to
the package, not just this call's own :NICKNAMES argument (GH #172,
review round 1)."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM-I1" :nicknames '("RSTI1"))
    (graph-db:ensure-namespace "RS-TLM-I1")
    (multiple-value-bind (ns types)
        (graph-db::read-schema-manifest graph-db::*system-directory*)
      types
      (let ((row (find "RS-TLM-I1" ns :key (lambda (r) (getf r :namespace))
                       :test #'string-equal)))
        (is-true row)
        (is (member "RSTI1" (getf row :nicknames) :test #'string=))))))

(test create-vertex-type-refuses-locked-package-strings
  "I2: \"COMMON-LISP:X\" and \"KEYWORD:X\" must signal GRAPH-DB's own
refusal (naming ENSURE-NAMESPACE), not SBCL's raw package-lock error or
a silent KEYWORD intern followed by a later, unrelated failure
(GH #172, review round 1)."
  (with-rs-store (g)
    g
    (dolist (bad '("COMMON-LISP:RS-I2-PROBE" "KEYWORD:RS-I2-PROBE"))
      (let ((c (handler-case
                   (progn (graph-db:create-vertex-type bad '()
                                                        :default-store
                                                        :rs-store)
                          nil)
                 (error (e) e))))
        (is-true c "~A should have signaled" bad)
        (when c
          (is (search "ENSURE-NAMESPACE" (format nil "~A" c))
              "~A's condition should name ENSURE-NAMESPACE: ~A" bad c))))))

(test manifest-uses-the-registered-meta-not-a-divergent-stray
  "I3a: INSTANTIATE-NODE-TYPE's manifest re-append is sourced from the
REGISTERED meta for the type's own declared store, not whatever META
object happens to be passed in -- a stray, never-registered meta (as a
foreign-store adoption or a stale on-disk read might carry) must never
overwrite the canonical row with divergent slots (GH #172, review
round 1)."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:DIVROW" '((a-slot :type string))
                                 :default-store :rs-store)
    (let* ((name (intern "DIVROW" :rs-tlm))
           (canonical (graph-db::node-type-slots
                       (graph-db::%find-registered-node-type
                        name :vertex :rs-store)))
           ;; A hand-built, never-registered, divergent meta for the
           ;; same class name -- bypasses %INSTALL-NODE-TYPE entirely.
           (stray (graph-db::make-node-type
                   :name name :parent-type :vertex
                   :graph-name :rs-store-b
                   :slots '((b-slot :type string :accessor b-slot
                            :initarg :b-slot)))))
      (handler-bind ((warning #'muffle-warning))
        (graph-db::instantiate-node-type stray g))
      (multiple-value-bind (ns types)
          (graph-db::read-schema-manifest graph-db::*system-directory*)
        ns
        (let ((row (find name types :key (lambda (r) (getf r :type)))))
          (is-true row)
          (is (equal canonical (getf row :slots))))))))

(test manifest-row-count-does-not-grow-on-reopen
  "I3b: repeated INSTANTIATE-NODE-TYPE calls (what a reopen does) for an
unchanged type must not keep growing the manifest -- the cached last-
written row makes the second and third calls no-ops (GH #172, review
round 1)."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:COUNTROW" '((c :type string))
                                 :default-store :rs-store)
    (let ((name (intern "COUNTROW" :rs-tlm)))
      (flet ((row-count ()
               (with-open-file (s (graph-db::%schema-manifest-file))
                 (loop for line = (read-line s nil :eof)
                       until (eq line :eof)
                       count (let ((rec (graph-db::%parse-schema-manifest-line
                                        line)))
                               (and rec (eq (getf rec :type) name)))))))
        (let ((c1 (row-count)))
          (graph-db::instantiate-node-type
           (graph-db::%find-registered-node-type name :vertex :rs-store)
           g)
          (graph-db::instantiate-node-type
           (graph-db::%find-registered-node-type name :vertex :rs-store)
           g)
          (is (= c1 (row-count))))))))

(test manifest-append-retries-after-a-transient-write-failure
  "Review round 2: %SCHEMA-MANIFEST-APPEND-IF-CHANGED must not cache a
row as written before the write's outcome is known.  Swap
%SCHEMA-MANIFEST-FILE to an unwritable location for one instantiate
(the append degrades to in-image-only), restore it, and instantiate
again with the same, already-current, changed slots: the manifest must
gain the row on this retry -- proving the earlier failure did not
poison the cache into skipping it for the rest of the session
(GH #172, review round 2)."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:RETRYROW" '((x :type string))
                                 :default-store :rs-store)
    (let ((name (intern "RETRYROW" :rs-tlm))
          (orig (fdefinition 'graph-db::%schema-manifest-file)))
      (unwind-protect
          (progn
            (setf (fdefinition 'graph-db::%schema-manifest-file)
                  (lambda ()
                    (merge-pathnames
                     "schema-manifest.dat"
                     #P"/nonexistent-172-review-round-2/")))
            ;; The class/meta still change here -- only the manifest
            ;; write itself fails.
            (graph-db:create-vertex-type "RS-TLM:RETRYROW"
                                         '((x :type string)
                                           (y :type string))
                                         :default-store :rs-store))
        (setf (fdefinition 'graph-db::%schema-manifest-file) orig))
      ;; The retry: same current (2-slot) meta, real location restored.
      (graph-db::instantiate-node-type
       (graph-db::%find-registered-node-type name :vertex :rs-store)
       g)
      (multiple-value-bind (ns types)
          (graph-db::read-schema-manifest graph-db::*system-directory*)
        ns
        (let ((row (find name types :key (lambda (r) (getf r :type)))))
          (is-true row)
          (is (= 2 (length (getf row :slots)))))))))
