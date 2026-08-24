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

;;; ---------------------------------------------------------------------
;;; Task 3 (R3+R5): MATERIALIZE-SCHEMA, the schema-function registry and
;;; the :CHECK slot option.
;;; ---------------------------------------------------------------------

(defun %rs-wipe-runtime-state (&optional graph)
  "Simulate a fresh image for the RS-TLM namespace: unintern the
classes, delete the package, drop the metas, and empty GRAPH's node
cache -- a fresh image has no cached instances either, and a cache hit
would serve the pre-wipe object of the old class.  A real restart is a
different process; this is the closest single-image ablation and it
proves materialize rebuilds everything it needs."
  (when graph (clrhash (graph-db::cache graph)))
  (let ((pkg (find-package :rs-tlm)))
    (when pkg
      (do-symbols (s pkg)
        (when (find-class s nil) (setf (find-class s) nil)))
      (delete-package pkg)))
  (%rs-drop-orphaned-metas))

(defun %rs-drop-orphaned-metas ()
  "Drop every registered meta whose class this ablation removed -- the
symbol lost its home package (DELETE-PACKAGE uninterns it) or lost its
class.  A later UPDATE-SCHEMA would otherwise FIND-CLASS something that
no longer exists."
  (maphash (lambda (store metas)
             (setf (gethash store graph-db::*schema-node-metadata*)
                   (remove-if (lambda (m)
                                (let ((n (graph-db::node-type-name m)))
                                  (or (null (symbol-package n))
                                      (null (find-class n nil)))))
                              metas)))
           graph-db::*schema-node-metadata*))

(test materialize-rebuilds-a-runtime-type-in-a-fresh-image
  "THE acceptance test: create at runtime, write, wipe the in-image
state, materialize from the manifest, and both the CLASS and the DATA
come back -- and a method compiles against the class."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (let (id)
      (with-transaction ((graph-db::transaction-manager g))
        (setq id (graph-db:id (funcall (intern "MAKE-READING" :rs-tlm)
                                       :value 3.5d0))))
      (%rs-wipe-runtime-state g)
      (is (null (find-package :rs-tlm)))
      (let ((summary (graph-db:materialize-schema
                      graph-db::*system-directory*)))
        (is (plusp (getf summary :materialized))))
      (let* ((sym (intern "READING" :rs-tlm))
             (class (find-class sym nil)))
        (is-true class)
        ;; A method compiles against the materialized class -- the
        ;; twenty-year problem, pinned.
        (let ((m (compile nil `(lambda (r)
                                 (declare (type ,sym r))
                                 (funcall ',(intern "VALUE" :rs-tlm)
                                          r)))))
          (is (= 3.5d0 (funcall m (graph-db:lookup-vertex
                                   id :graph g)))))))))

(test materialize-skips-source-defined-classes
  "Source wins: RS-STATIC is defined by def-vertex in this image; its
manifest row must be skipped, not rebuilt, and the summary says so."
  (with-rs-store (g)
    g
    (let ((summary (graph-db:materialize-schema
                    graph-db::*system-directory*)))
      (is (plusp (getf summary :skipped-existing)))
      (is (typep (make-instance 'rs-static) 'rs-static)))))

(test materialize-warns-when-a-skipped-class-diverges
  "Source wins, but not silently: a manifest row whose slots disagree
with the live class must reach the user as the #196 divergence warning
(GH #172, R3)."
  (with-rs-store (g)
    g
    ;; Rewrite RS-STATIC's row with a slot set the live class lacks.
    (with-open-file (s (graph-db::%schema-manifest-file)
                       :direction :output :if-exists :append)
      (let ((*package* (find-package "COMMON-LISP"))
            (*print-pretty* nil))
        (format s "~S~%"
                (list :type 'rs-static :kind :vertex :parents nil
                      :slots '((not-a-real-slot :accessor
                                not-a-real-slot :initarg
                                :not-a-real-slot))
                      :default-store :rs-store :keep-revisions nil
                      :provenance :source :time 0))))
    (let ((warned nil))
      (handler-bind ((graph-db:divergent-node-type-redefinition
                       (lambda (c) (setq warned t) (muffle-warning c))))
        (graph-db:materialize-schema graph-db::*system-directory*))
      (is-true warned))))

(test materialize-fails-fast-on-an-unresolved-check-function
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:register-schema-function 'rs-plausible-p
                                       (lambda (v) (< 0 v 100)))
    (graph-db:create-vertex-type
     "RS-TLM:CAL" '((value :type double-float :check rs-plausible-p))
     :default-store :rs-store)
    (%rs-wipe-runtime-state)
    ;; Fresh image forgot to register the function:
    (graph-db::%unregister-schema-function 'rs-plausible-p)
    (let ((c (handler-case
                 (progn (graph-db:materialize-schema
                         graph-db::*system-directory*)
                        nil)
               (graph-db:materialize-unresolved-functions (e) e))))
      (is-true c)
      (when c
        (is (member 'rs-plausible-p
                    (graph-db:unresolved-function-names c))))
      ;; Fail fast: nothing half-built.
      (is (null (find-class (and (find-package :rs-tlm)
                                 (intern "CAL" :rs-tlm))
                            nil))))))

(test materialize-orders-parents-before-children
  "A child row that precedes its parent in the manifest still builds:
the topological pass, not append order, decides (GH #172, R3)."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:BASE" '((a :type string))
                                 :default-store :rs-store)
    (graph-db:create-vertex-type "RS-TLM:DERIVED" '((b :type string))
                                 :parents (list (intern "BASE" :rs-tlm))
                                 :default-store :rs-store)
    ;; Re-append BASE's row so it is now LAST: read order would then
    ;; try DERIVED first.
    (graph-db:create-vertex-type "RS-TLM:BASE" '((a :type string)
                                                 (a2 :type string))
                                 :default-store :rs-store)
    (%rs-wipe-runtime-state)
    (graph-db:materialize-schema graph-db::*system-directory*)
    (let ((derived (find-class (intern "DERIVED" :rs-tlm) nil)))
      (is-true derived)
      (is-true (subtypep (intern "DERIVED" :rs-tlm)
                         (intern "BASE" :rs-tlm))))))

(test materialize-invents-a-package-for-an-orphan-type-row
  "A hand-trimmed manifest whose type row names a package with no
namespace record must still materialize -- warn, create the package,
build the class (GH #172, R3)."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-ORPHAN")
    (graph-db:create-vertex-type "RS-ORPHAN:THING" '((a :type string))
                                 :default-store :rs-store)
    ;; Drop the namespace row, keep the type row.
    (let* ((file (graph-db::%schema-manifest-file))
           (lines (with-open-file (s file)
                    (loop for l = (read-line s nil :eof)
                          until (eq l :eof) collect l))))
      (with-open-file (s file :direction :output :if-exists :supersede)
        (dolist (l lines)
          (unless (search "RS-ORPHAN\"" l) (write-line l s)))))
    (let ((pkg (find-package :rs-orphan)))
      (when pkg
        (do-symbols (s pkg) (when (find-class s nil)
                              (setf (find-class s) nil)))
        (delete-package pkg)))
    (%rs-drop-orphaned-metas)
    (handler-bind ((warning #'muffle-warning))
      (graph-db:materialize-schema graph-db::*system-directory*))
    (is-true (find-package :rs-orphan))
    (is-true (find-class (intern "THING" :rs-orphan) nil))))

(test schema-function-registry-round-trips
  (graph-db:register-schema-function 'rs-registry-probe #'evenp)
  (is (eq #'evenp (graph-db:find-schema-function 'rs-registry-probe)))
  (graph-db::%unregister-schema-function 'rs-registry-probe)
  (is (null (graph-db:find-schema-function 'rs-registry-probe))))

(test create-vertex-type-refuses-an-unregistered-check-function
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db::%unregister-schema-function 'rs-never-registered-p)
    (signals graph-db:schema-function-unresolved
      (graph-db:create-vertex-type
       "RS-TLM:NOFN"
       '((value :type double-float :check rs-never-registered-p))
       :default-store :rs-store))))

(test check-constraint-enforces-at-commit
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:register-schema-function 'rs-plausible-p
                                       (lambda (v) (< 0 v 100)))
    (graph-db:create-vertex-type
     "RS-TLM:CAL" '((value :type double-float :check rs-plausible-p))
     :default-store :rs-store)
    (with-transaction ((graph-db::transaction-manager g))
      (funcall (intern "MAKE-CAL" :rs-tlm) :value 50d0))
    (signals graph-db:value-constraint-violation
      (with-transaction ((graph-db::transaction-manager g))
        (funcall (intern "MAKE-CAL" :rs-tlm) :value 5000d0)))))

(test check-constraint-is-null-exempt-and-inherited
  "NULL is exempt (the :ONE-OF rule), and a :CHECK on a parent slot is
enforced on a subclass (GH #172, R5)."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:register-schema-function 'rs-plausible-p
                                       (lambda (v) (< 0 v 100)))
    (graph-db:create-vertex-type
     "RS-TLM:CBASE" '((value :type double-float :check rs-plausible-p))
     :default-store :rs-store)
    (graph-db:create-vertex-type "RS-TLM:CSUB" '((tag :type string))
                                 :parents (list (intern "CBASE" :rs-tlm))
                                 :default-store :rs-store)
    ;; NIL passes.
    (with-transaction ((graph-db::transaction-manager g))
      (funcall (intern "MAKE-CSUB" :rs-tlm) :tag "t"))
    (signals graph-db:value-constraint-violation
      (with-transaction ((graph-db::transaction-manager g))
        (funcall (intern "MAKE-CSUB" :rs-tlm) :tag "t" :value 5000d0)))))

;;; The image provides the behaviour its schema names -- at load time,
;;; before any manifest row referencing it is materialized (GH #172).
(graph-db:register-schema-function 'rs-source-plausible-p
                                   (lambda (v) (< 0 v 100)))

(def-vertex rs-checked () ((value :type double-float
                                  :check rs-source-plausible-p))
  :rs-store)

(test def-vertex-accepts-and-enforces-check
  "Parity: :CHECK is a slot option DEF-VERTEX takes too (GH #172, R5)."
  (with-rs-store (g)
    (with-transaction ((graph-db::transaction-manager g))
      (make-rs-checked :value 10d0))
    (signals graph-db:value-constraint-violation
      (with-transaction ((graph-db::transaction-manager g))
        (make-rs-checked :value 900d0)))))

;;; ---------------------------------------------------------------------
;;; Fix round 1 (review of task 3): I-2, M-1.
;;; ---------------------------------------------------------------------

(test materialize-fails-fast-on-a-parent-outside-the-build-set
  "I-2: a build set that excludes a parent must abort the whole call --
ENSURE-CLASS would otherwise leave a FORWARD-REFERENCED-CLASS stub for
it, which every later materialization would skip as \"already
defined\", poisoning the image until restart.  So: fail fast, build
nothing, and let a second call over the whole manifest succeed."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-BASE-NS")
    (graph-db:ensure-namespace "RS-KID-NS")
    (graph-db:create-vertex-type "RS-BASE-NS:PARENT" '((a :type string))
                                 :default-store :rs-store)
    (graph-db:create-vertex-type
     "RS-KID-NS:CHILD" '((b :type string))
     :parents (list (intern "PARENT" :rs-base-ns))
     :default-store :rs-store)
    ;; Fresh image for the classes; RS-BASE-NS the PACKAGE survives (a
    ;; source DEFPACKAGE would have created it), so the CHILD row still
    ;; READS -- its parent simply has no class and no selected row.
    (dolist (name '(:rs-kid-ns :rs-base-ns))
      (let ((pkg (find-package name)))
        (when pkg
          (do-symbols (sym pkg) (when (find-class sym nil)
                                  (setf (find-class sym) nil))))))
    (let ((pkg (find-package :rs-kid-ns)))
      (when pkg (delete-package pkg)))
    (%rs-drop-orphaned-metas)
    (signals graph-db:materialize-unresolved-parents
      (graph-db:materialize-schema graph-db::*system-directory*
                                   :namespaces '("RS-KID-NS")))
    ;; Nothing built, and no stub left behind for PARENT.
    (is (null (find-class (intern "CHILD" :rs-kid-ns) nil)))
    (is (null (find-class (intern "PARENT" :rs-base-ns) nil)))
    ;; Not poisoned: the call over the whole manifest succeeds.
    (graph-db:materialize-schema graph-db::*system-directory*)
    (is-true (find-class (intern "PARENT" :rs-base-ns) nil))
    (is-true (find-class (intern "CHILD" :rs-kid-ns) nil))
    (is-true (subtypep (intern "CHILD" :rs-kid-ns)
                       (intern "PARENT" :rs-base-ns)))))

(test a-forward-referenced-class-does-not-count-as-existing
  "I-2 pin: the skip test is %MATERIALIZED-CLASS-PRESENT-P, not
FIND-CLASS -- a stub must be rebuilt, not mistaken for source."
  ;; Portable way to get one: name an undefined superclass.  The MOP
  ;; leaves a stub for RS-FWD-ABSENT, and FIND-CLASS answers it.
  (let ((sym (intern "RS-FWD-ABSENT" :graph-db/test)))
    (unwind-protect
         (progn
           (eval `(defclass ,(intern "RS-FWD-CHILD" :graph-db/test)
                      (,sym) ()))
           (is-true (find-class sym nil))
           (is (null (graph-db::%materialized-class-present-p sym))))
      (setf (find-class (intern "RS-FWD-CHILD" :graph-db/test)) nil)
      (setf (find-class sym) nil))))

(test materialize-does-not-grow-the-manifest-per-restart
  "M-1: the rows materialize installs came OUT of the manifest, so a
second boot must not append an identical row (with a fresh :TIME) for
every type it rebuilds."
  (with-rs-store (g)
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:BOOTROW" '((x :type string))
                                 :default-store :rs-store)
    (let ((name (intern "BOOTROW" :rs-tlm)))
      (flet ((row-count (sym)
               (with-open-file (s (graph-db::%schema-manifest-file))
                 (loop for line = (read-line s nil :eof)
                       until (eq line :eof)
                       count (let ((rec
                                     (graph-db::%parse-schema-manifest-line
                                      line)))
                               (and rec (eq (getf rec :type) sym)))))))
        (let ((before (row-count name)))
          (is (plusp before))
          (%rs-wipe-runtime-state g)
          (graph-db:materialize-schema graph-db::*system-directory*)
          (is (= before (row-count (intern "BOOTROW" :rs-tlm))))
          (%rs-wipe-runtime-state g)
          (graph-db:materialize-schema graph-db::*system-directory*)
          (is (= before (row-count (intern "BOOTROW" :rs-tlm)))))))))

;;; ---------------------------------------------------------------------
;;; Task 4 (R6): DESCRIBE-SCHEMA / EXPORT-SCHEMA-SOURCE.
;;; ---------------------------------------------------------------------

(test describe-schema-shows-provenance-and-slots
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (let ((text (with-output-to-string (s)
                  (graph-db:describe-schema :stream s))))
      (is (search "RS-TLM" text))
      (is (search "READING" text))
      (is (search "[runtime" text))
      (is (search "RS-STATIC" text :test #'char-equal))
      (is (search "[source]" text)))))

(test export-schema-source-round-trips
  "The promotion path: export the runtime namespace, wipe the image
state, LOAD the exported file (the ordinary source path -- the ENGINE
never does this, the developer's build does), and the type is back
with the SAME registry id."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-TLM")
    (graph-db:create-vertex-type "RS-TLM:READING"
                                 '((value :type double-float))
                                 :default-store :rs-store)
    (let ((id-before (graph-db::node-type-id
                      (graph-db::%find-registered-node-type
                       (intern "READING" :rs-tlm) :vertex)))
          (path (merge-pathnames
                 "exported-schema.lisp"
                 (uiop:ensure-directory-pathname
                  graph-db::*system-directory*))))
      (graph-db:export-schema-source path :namespace :rs-tlm)
      (%rs-wipe-runtime-state)
      (load path)
      (let ((meta (graph-db::%find-registered-node-type
                   (intern "READING" :rs-tlm) :vertex)))
        (is-true meta)
        (is (= id-before (graph-db::node-type-id meta)))))))
