;;;; Schema retraction: a spec can be RE-DECLARED or withdrawn (GH #139, #140).
;;;;
;;;; Both registries were append-only for the life of the image, de-duped by
;;;; (owner . slot-names), newest-wins.  That resolves an IDENTICAL re-evaluation
;;;; correctly and nothing else: change the slot list and the two specs no longer
;;;; collide, so both stay live.  #139 then rejects writes the current schema
;;;; permits; #140 keeps building and maintaining an index no declaration asks for.
;;;;
;;;; The case that forces NAMED declarations rather than an unregister keyed by
;;;; slot-names: a macro that emits specs on a caller's behalf cannot name what a
;;;; PREVIOUS VERSION OF ITSELF emitted.  DEF-CLAIM-CLASSES is exactly that, and
;;;; GH #138 changes what it emits.

(in-package #:graph-db/test)

(def-suite schema-retraction-suite
  :description "Named schema declarations, withdrawal, and sidecar
reconciliation (GH #139, #140)."
  :in graph-db-suite)

(in-suite schema-retraction-suite)

(defparameter *sr-graph-name* :graph-db-schema-retraction-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *sr-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex sr-rec ()
  ((k1 :initarg :k1 :accessor sr-k1)
   (k2 :initarg :k2 :accessor sr-k2))
  :graph-db-schema-retraction-test)

(defmacro with-sr-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *sr-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %sr-clear-registries ()
  "Drop every spec this suite registered, so each test starts from a known
registry rather than from whatever ran before it."
  (setf (gethash *sr-graph-name* graph-db::*schema-index-metadata*) nil
        (gethash *sr-graph-name* graph-db::*schema-unique-metadata*) nil))

(defun %sr-index-specs ()
  (with-sr-graph (g) (graph-db::%registered-index-specs g)))

(defun %sr-unique-specs ()
  (with-sr-graph (g) (graph-db::%registered-unique-tuple-specs g)))


;;; --- the defect itself: a re-declaration leaves both specs live -------------

(test a-renamed-index-key-replaces-the-old-one
  "GH #140.  Two DEF-INDEX declarations on one owner with DIFFERENT slot lists
are two different identities under (owner . slot-names), so both survive and
the old index keeps being built and maintained on every write."
  (%sr-clear-registries)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test :name sr-key)
  (def-index sr-rec (k2) :graph-db-schema-retraction-test :name sr-key)
  (let ((specs (%sr-index-specs)))
    (is (= 1 (length specs)) "the re-declared name must leave ONE spec live")
    (is (equal '(k2) (graph-db::index-spec-slot-names (first specs))))))

(test a-renamed-unique-key-replaces-the-old-one
  "GH #139, and the worse half: the stale constraint REJECTS WRITES the current
schema permits, with no retraction path anywhere in the image."
  (%sr-clear-registries)
  (def-unique sr-rec (k1) :graph-db-schema-retraction-test :name sr-ukey)
  (def-unique sr-rec (k2) :graph-db-schema-retraction-test :name sr-ukey)
  (let ((specs (%sr-unique-specs)))
    (is (= 1 (length specs)) "the re-declared name must leave ONE spec live")
    (is (equal '(k2)
               (graph-db::unique-tuple-spec-slot-names (first specs))))))

(test the-139-repro-no-longer-rejects-a-valid-write
  "⚠ The issue's own repro, end to end.  K1 was the key, then K2 became the key;
two records sharing K1 are valid under the CURRENT schema and were rejected by
the stale one."
  (%sr-clear-registries)
  (def-unique sr-rec (k1) :graph-db-schema-retraction-test :name sr-ukey)
  (def-unique sr-rec (k2) :graph-db-schema-retraction-test :name sr-ukey)
  (with-sr-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction () (make-sr-rec :k1 "A" :k2 "X"))
      (with-transaction () (make-sr-rec :k1 "A" :k2 "Y")))))


;;; --- an UNNAMED declaration must behave exactly as it does today ------------

(test unnamed-declarations-keep-slot-name-identity
  "⚠ The compatibility guarantee.  Every declaration in the tree today is
unnamed, so identity must stay (owner . slot-names) for them -- two unnamed
declarations with different slot lists are two DIFFERENT indexes and both
must live, which is the legitimate multi-index case."
  (%sr-clear-registries)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test)
  (def-index sr-rec (k2) :graph-db-schema-retraction-test)
  (is (= 2 (length (%sr-index-specs)))
      "two distinct unnamed indexes must both survive"))

(test an-identical-unnamed-re-declaration-is-still-a-no-op
  (%sr-clear-registries)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test)
  (is (= 1 (length (%sr-index-specs)))))


;;; --- the registries must stop growing without bound -------------------------

(test re-evaluation-does-not-grow-the-registry
  "Both issues flag it: the tables accumulate one entry per EVALUATION and are
scanned linearly, so a long-lived development image that reloads schema pays
for every reload forever.  REGISTER now replaces in place."
  (%sr-clear-registries)
  (dotimes (i 5)
    (def-index sr-rec (k1) :graph-db-schema-retraction-test)
    (def-unique sr-rec (k1) :graph-db-schema-retraction-test :name sr-ukey))
  (is (= 1 (length (gethash *sr-graph-name*
                            graph-db::*schema-index-metadata*)))
      "the raw index table must hold one entry per logical declaration")
  (is (= 1 (length (gethash *sr-graph-name*
                            graph-db::*schema-unique-metadata*)))
      "the raw unique table must hold one entry per logical declaration"))


;;; --- explicit withdrawal ----------------------------------------------------

(test undef-index-withdraws-a-named-spec
  (%sr-clear-registries)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test :name sr-key)
  (is (= 1 (length (%sr-index-specs))))
  (undef-index sr-rec :graph-db-schema-retraction-test :name sr-key)
  (is (null (%sr-index-specs))))

(test undef-index-withdraws-an-unnamed-spec-by-slots
  (%sr-clear-registries)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test)
  (undef-index sr-rec :graph-db-schema-retraction-test :slots (k1))
  (is (null (%sr-index-specs))))

(test undef-unique-withdraws-a-spec
  (%sr-clear-registries)
  (def-unique sr-rec (k1) :graph-db-schema-retraction-test :name sr-ukey)
  (undef-unique sr-rec :graph-db-schema-retraction-test :name sr-ukey)
  (is (null (%sr-unique-specs))))

(test withdrawing-something-never-declared-is-a-no-op-not-an-error
  "A macro that clears before declaring must not have to know whether it ran
before."
  (%sr-clear-registries)
  (finishes (undef-index sr-rec :graph-db-schema-retraction-test :name nope))
  (finishes (undef-unique sr-rec :graph-db-schema-retraction-test :name nope)))


;;; --- the half that makes retraction SAFE rather than merely possible --------

(test a-withdrawn-index-is-not-reopened-from-the-sidecar
  "⚠ Maintenance is SPEC-driven (CLASS-SECONDARY-INDEX-DESCRIPTORS, \"the single
input to maintenance\") while reopen is SIDECAR-driven (RESTORE-SECONDARY-INDEX-
ROOTS iterates the saved records and populates the registry for every one,
consulting no spec).  Those agree only while specs never go away.

Retraction is exactly what makes them disagree, and the failure is worse than
the one it fixes: the index is reopened, no longer maintained, re-saved at
close -- and INDEX-LOOKUP takes its slot names from the CALLER, so it can still
be READ.  A stale index that answers queries beats a useless one that does not.

So the sidecar must be reconciled against the live specs at open."
  (%sr-clear-registries)
  (def-index sr-rec (k1) :graph-db-schema-retraction-test :name sr-key)
  (with-temp-directory (dir)
    (let ((g (make-graph *sr-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (make-sr-rec :k1 "A" :k2 "X")))
        (close-graph g))
      ;; Withdraw it, then reopen: the sidecar still names it.
      (undef-index sr-rec :graph-db-schema-retraction-test :name sr-key)
      (let ((g2 (open-graph *sr-graph-name* (namestring dir)
                            :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g2))
               (is (null (gethash (cons 'sr-rec '(k1))
                                  (graph-db::secondary-indexes g2)))
                   "a withdrawn index must not be reopened from the sidecar"))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))
