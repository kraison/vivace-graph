;;;; Tests for the NODE-CLASS metaclass's slot categorization (node-class.lisp).
;;;;
;;;; These need no graph and no schema registration: a bare NODE-CLASS is
;;;; enough to exercise how slots are sorted into persistent / ephemeral / meta
;;;; and how that categorization is cached (GH #87).

(in-package #:graph-db/test)

(def-suite node-class-suite
  :description "NODE-CLASS slot categorization and its per-class cache."
  :in graph-db-suite)

(in-suite node-class-suite)

;;; A bare node-class -- not a vertex/edge, because none of this depends on the
;;; node protocol.  Slots deliberately cover all three categories plus the
;;; default (a slot with no options is ephemeral; see
;;; COMPUTE-EFFECTIVE-SLOT-DEFINITION).
(defclass nc-probe ()
  ((p1 :persistent t)
   (p2 :persistent t)
   (e1 :ephemeral t)
   (m1 :meta t)
   (plain))
  (:metaclass graph-db::node-class))

;; CLASS-SLOTS requires a finalized class, and nothing here instantiates
;; NC-PROBE, so finalize it explicitly (DEF-VERTEX does the same for real node
;; types -- see schema.lisp).
(eval-when (:load-toplevel :execute)
  (graph-db::finalize-inheritance (find-class 'nc-probe)))

(defun %nc-names (fn class-name)
  (sort (copy-list (funcall fn (find-class class-name))) #'string< :key #'symbol-name))

(test slot-categories-are-exclusive-and-complete
  "Every effective slot lands in exactly one of persistent / ephemeral / meta."
  (let* ((class (find-class 'nc-probe))
         (all (mapcar #'graph-db::slot-definition-name (graph-db::class-slots class)))
         (p (graph-db::persistent-slot-names class))
         (e (graph-db::ephemeral-slot-names class))
         (m (graph-db::meta-slot-names class)))
    (is (null (intersection p e)) "persistent/ephemeral overlap: ~a" (intersection p e))
    (is (null (intersection p m)) "persistent/meta overlap: ~a" (intersection p m))
    (is (null (intersection e m)) "ephemeral/meta overlap: ~a" (intersection e m))
    (is (null (set-difference all (append p e m)))
        "uncategorized slots: ~a" (set-difference all (append p e m)))))

(test slot-categories-match-declarations
  "Persistent is the default: a slot is persistent unless declared :META T.
See EPHEMERAL-DECLARATION-IS-CURRENTLY-INERT for why E1 is in the list."
  (is (equal '(e1 p1 p2 plain) (%nc-names #'graph-db::persistent-slot-names 'nc-probe)))
  (is (equal '(m1) (%nc-names #'graph-db::meta-slot-names 'nc-probe)))
  ;; DATA-SLOTS is persistent + ephemeral, and excludes meta.
  (is (equal '(e1 p1 p2 plain) (%nc-names #'graph-db::data-slots 'nc-probe))))

(test ephemeral-declaration-is-currently-inert
  "CHARACTERIZATION, not endorsement: :EPHEMERAL T on a direct slot has no
effect on the effective slot, so EPHEMERAL-SLOT-NAMES is empty for every node
class and an :EPHEMERAL slot is in fact stored persistently.

NODE-SLOT-DEFINITION declares PERSISTENT with :INITFORM T, and
COMPUTE-EFFECTIVE-SLOT-DEFINITION does not propagate :EPHEMERAL from the direct
slots the way it propagates :INDEX / :UNIQUE / :VECTOR-INDEX / :SPATIAL-*.  So
by the time its COND runs (PERSISTENT-P SLOT) is always true, the second branch
always wins, and the third branch -- the only one that ever sets EPHEMERAL -- is
unreachable.

Recorded here so the behaviour is known rather than rediscovered, and so that
deliberately fixing it fails loudly at this test instead of silently changing
what gets written to disk."
  (let ((class (find-class 'nc-probe)))
    (is (null (graph-db::ephemeral-slot-names class))
        "ephemeral-slot-names is no longer empty: ~a"
        (graph-db::ephemeral-slot-names class))
    (is (member 'e1 (graph-db::persistent-slot-names class))
        "an :EPHEMERAL slot is no longer persistent -- storage behaviour changed")))

(test persistent-slot-keyword-agrees-with-persistent-slot-names
  "The hot path asks for a slot's DATA-alist keyword; NIL means \"not
persistent\", which is what routes an access to the standard method.  It must
agree exactly with PERSISTENT-SLOT-NAMES, and the keyword must be the one
NODE-SLOT-VALUE would have interned."
  (let ((class (find-class 'nc-probe)))
    (dolist (slot (graph-db::class-slots class))
      (let* ((name (graph-db::slot-definition-name slot))
             (kw (graph-db::%persistent-slot-keyword class name))
             (persistent (and (member name (graph-db::persistent-slot-names class)) t)))
        (is (eq persistent (and kw t))
            "~a: keyword ~a but persistent-slot-names says ~a" name kw persistent)
        (when kw
          (is (eq kw (intern (symbol-name name) :keyword))
              "~a: cached keyword ~s is not the interned one" name kw))))))

;;; ---- the cache (GH #87) -------------------------------------------------

#+sbcl
(test slot-name-lookups-do-not-rebuild-per-call
  "PERSISTENT-P / EPHEMERAL-P / META-P answers are fixed once the class is
finalized, so the slot-name lists must be computed once per class -- not walked
and freshly consed on every call.  GH #87: this ran on every slot access,
rebuilding up to three lists per access (~28 rebuilds per node materialized,
26.4 MB of throwaway lists in one profiler workload)."
  (let ((class (find-class 'nc-probe)))
    ;; warm: first call is allowed to compute and cache
    (graph-db::persistent-slot-names class)
    (graph-db::ephemeral-slot-names class)
    (graph-db::meta-slot-names class)
    (graph-db::%persistent-slot-keyword class 'p1)
    (sb-ext:gc :full t)
    (let ((before (sb-ext:get-bytes-consed))
          (n 2000))
      (dotimes (i n)
        (graph-db::persistent-slot-names class)
        (graph-db::ephemeral-slot-names class)
        (graph-db::meta-slot-names class)
        (graph-db::%persistent-slot-keyword class 'p1))
      (let ((per-iteration (/ (- (sb-ext:get-bytes-consed) before) n)))
        (is (< per-iteration 16)
            "one round of the three slot-name lookups consed ~a bytes"
            per-iteration)))))

(test slot-categorization-follows-class-redefinition
  "The cache is only sound if it dies when the class definition changes.
Redefine NC-REDEF so a persistent slot becomes meta, and the categorization --
and the hot path's keyword lookup -- must both follow."
  (eval '(defclass nc-redef ()
          ((a :persistent t) (b :meta t))
          (:metaclass graph-db::node-class)))
  (let ((class (find-class 'nc-redef)))
    (graph-db::finalize-inheritance class)
    (is (equal '(a) (graph-db::persistent-slot-names class)))
    (is (equal '(b) (graph-db::meta-slot-names class)))
    (is (eq :a (graph-db::%persistent-slot-keyword class 'a)))
    ;; A becomes meta, B becomes persistent -- a full swap.
    (eval '(defclass nc-redef ()
            ((a :meta t) (b :persistent t))
            (:metaclass graph-db::node-class)))
    (let ((class (find-class 'nc-redef)))
      (graph-db::finalize-inheritance class)
      (is (equal '(b) (graph-db::persistent-slot-names class))
          "persistent-slot-names still stale after redefinition: ~a"
          (graph-db::persistent-slot-names class))
      (is (equal '(a) (graph-db::meta-slot-names class)))
      (is (null (graph-db::%persistent-slot-keyword class 'a))
          "A is meta now, but the keyword cache still calls it persistent")
      (is (eq :b (graph-db::%persistent-slot-keyword class 'b))))))

(test slot-categorization-follows-superclass-redefinition
  "A subclass's effective slots change when its SUPERCLASS is redefined, so the
subclass's cache must be invalidated too -- not just the class that was
redefined."
  (eval '(defclass nc-super () ((s :persistent t)) (:metaclass graph-db::node-class)))
  (eval '(defclass nc-sub (nc-super) ((own :persistent t))
          (:metaclass graph-db::node-class)))
  (let ((sub (find-class 'nc-sub)))
    (graph-db::finalize-inheritance sub)
    (is (eq :s (graph-db::%persistent-slot-keyword sub 's)))
    ;; Redefine the PARENT so its slot is now meta.
    (eval '(defclass nc-super () ((s :meta t)) (:metaclass graph-db::node-class)))
    (let ((sub (find-class 'nc-sub)))
      (graph-db::finalize-inheritance sub)
      (is (null (graph-db::%persistent-slot-keyword sub 's))
          "subclass cache survived a superclass redefinition")
      (is (eq :own (graph-db::%persistent-slot-keyword sub 'own))))))

;;; ---- runtime schema mutation vs. MOP-derived caches ---------------------
;;;
;;; VG supports evaluating DEF-VERTEX / DEF-EDGE against a live image to add or
;;; redefine a type.  Several places memoize a CLASS-SLOTS-derived answer for
;;; speed, and each is only sound if it dies when the schema changes.  These
;;; pin the three ways that went wrong.

(defun %nc-fresh-name (stem)
  (intern (format nil "~:@(~a~)-~36R" stem (random (expt 36 8))) :graph-db/test))

(test runtime-redefinition-reaches-subclasses
  "A subclass's effective slots change when its SUPERCLASS is redefined, so every
per-class cache must be dropped for the subclass too.  Invalidating only the
class that was redefined left NODE-GEOMETRY-INDEX-SLOTS on the subclass stale,
which means a geometry slot added to a parent was silently never spatially
indexed for subclass instances."
  (let ((parent (%nc-fresh-name "rt-parent"))
        (child (%nc-fresh-name "rt-child")))
    (eval `(defclass ,parent () ((pgeom :index t)) (:metaclass graph-db::node-class)))
    (eval `(defclass ,child (,parent) ((cname)) (:metaclass graph-db::node-class)))
    (graph-db::finalize-inheritance (find-class child))
    (is (equal '(pgeom)
               (graph-db::node-geometry-index-slots (find-class child)))
        "baseline: subclass inherits the parent's indexed slot")
    ;; add a SECOND indexed slot to the PARENT
    (eval `(defclass ,parent () ((pgeom :index t) (pgeom2 :index t))
             (:metaclass graph-db::node-class)))
    (graph-db::finalize-inheritance (find-class parent))
    (graph-db::finalize-inheritance (find-class child))
    (is (equal '(pgeom pgeom2)
               (graph-db::node-geometry-index-slots (find-class parent)))
        "the redefined class itself must see the new slot")
    (is (equal '(pgeom pgeom2)
               (graph-db::node-geometry-index-slots (find-class child)))
        "the SUBCLASS must see the parent's new indexed slot, got ~a"
        (graph-db::node-geometry-index-slots (find-class child)))))

(test runtime-redefinition-reaches-vector-index-slots
  ":VECTOR-INDEX slots are memoized the same way and were never invalidated at
all, so a slot added at runtime never got a vector segment."
  (let ((name (%nc-fresh-name "rt-vi")))
    (eval `(defclass ,name () ((other)) (:metaclass graph-db::node-class)))
    (graph-db::finalize-inheritance (find-class name))
    (is (null (graph-db::node-vector-index-slots (find-class name)))
        "baseline: no vector-index slots")
    (eval `(defclass ,name () ((other) (emb :vector-index t))
             (:metaclass graph-db::node-class)))
    (graph-db::finalize-inheritance (find-class name))
    (is (equal '(emb) (graph-db::node-vector-index-slots (find-class name)))
        "a :VECTOR-INDEX slot added at runtime must be seen, got ~a"
        (graph-db::node-vector-index-slots (find-class name)))))

(test unfinalized-class-does-not-poison-the-slot-caches
  "Asking before the class is finalized must not CACHE the empty answer.
CLASS-SLOTS is unavailable then, and storing the resulting NIL made it permanent
-- the class could never be spatially indexed again for the life of the image.

NOTE the implementation difference: SBCL leaves a window between DEFCLASS and
finalization in which the premature call can happen, and that window is what
this test exploits.  ECL finalizes eagerly, so there is usually no window to
exploit there and this degenerates to checking the answers are right.  The
assertions below therefore do not require the window to exist -- requiring it
made this test fail on ECL against perfectly correct code."
  (let ((name (%nc-fresh-name "rt-unfin")))
    (eval `(defclass ,name () ((g :index t) (emb :vector-index t))
             (:metaclass graph-db::node-class)))
    ;; Ask too early WHERE THAT IS POSSIBLE -- this is what used to poison the
    ;; cache.  Harmless (and simply a warm read) where the class is already
    ;; finalized.
    (graph-db::node-geometry-index-slots (find-class name))
    (graph-db::node-vector-index-slots (find-class name))
    (graph-db::finalize-inheritance (find-class name))
    (is (equal '(g) (graph-db::node-geometry-index-slots (find-class name)))
        "geometry slots after finalizing, got ~a"
        (graph-db::node-geometry-index-slots (find-class name)))
    (is (equal '(emb) (graph-db::node-vector-index-slots (find-class name)))
        "vector-index slots after finalizing, got ~a"
        (graph-db::node-vector-index-slots (find-class name)))))
