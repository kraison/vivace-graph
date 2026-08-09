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
  "Persistent is the default: a slot is persistent unless declared :META T or
:EPHEMERAL T."
  (is (equal '(p1 p2 plain) (%nc-names #'graph-db::persistent-slot-names 'nc-probe)))
  (is (equal '(e1) (%nc-names #'graph-db::ephemeral-slot-names 'nc-probe)))
  (is (equal '(m1) (%nc-names #'graph-db::meta-slot-names 'nc-probe)))
  ;; DATA-SLOTS is persistent + ephemeral, and excludes meta.
  (is (equal '(e1 p1 p2 plain) (%nc-names #'graph-db::data-slots 'nc-probe))))

(test ephemeral-slots-are-not-persisted
  "GH #90: :EPHEMERAL T on a direct slot had no effect -- EPHEMERAL-SLOT-NAMES
was empty for every node class and such a slot was written to disk like any
other.  CALL-NEXT-METHOD builds a fresh effective slot without carrying custom
slot-definition slots over, so PERSISTENT-P was always true there and the clause
that set EPHEMERAL was unreachable; the propagation now reads the DIRECT slots,
as :INDEX / :UNIQUE / :VECTOR-INDEX / :SPATIAL-* already did."
  (let ((class (find-class 'nc-probe)))
    (is (equal '(e1) (graph-db::ephemeral-slot-names class))
        "the :EPHEMERAL slot is not categorized as ephemeral: ~a"
        (graph-db::ephemeral-slot-names class))
    (is (not (member 'e1 (graph-db::persistent-slot-names class)))
        "an :EPHEMERAL slot is still persistent, so it is still written to disk")
    ;; The gate the write path actually consults: NIL routes the access to the
    ;; standard method, i.e. ordinary CLOS storage rather than the DATA alist.
    (is (null (graph-db::%persistent-slot-keyword class 'e1))
        "e1 still has a DATA-alist keyword, so it would still be serialized")
    (is (eq :p1 (graph-db::%persistent-slot-keyword class 'p1))
        "a normal slot must be unaffected")
    ;; Ephemeral is NOT meta: it still holds a value in the instance.  Losing
    ;; that would turn the option from inert into actively broken.
    (is (not (member 'e1 (graph-db::meta-slot-names class)))
        "an :EPHEMERAL slot must not be categorized as meta")
    (is (member 'e1 (graph-db::data-slots class))
        "an :EPHEMERAL slot is still a data slot (persistent + ephemeral)")))

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
    ;; GET-BYTES-CONSED is process-wide, so a background thread allocating inside
    ;; the window inflates the reading.  Take the MINIMUM over several rounds:
    ;; interference can only ADD, so the min converges on the true cost.  A single
    ;; round against a budget of 16 measured 16.384 on a loaded host -- a false
    ;; failure 2.4% over, from ~768 stray bytes across the whole loop.
    (let* ((n 2000)
           (per-iteration
             (loop repeat 5
                   minimize (progn
                              (sb-ext:gc :full t)
                              (let ((before (sb-ext:get-bytes-consed)))
                                (dotimes (i n)
                                  (graph-db::persistent-slot-names class)
                                  (graph-db::ephemeral-slot-names class)
                                  (graph-db::meta-slot-names class)
                                  (graph-db::%persistent-slot-keyword class 'p1))
                                (/ (- (sb-ext:get-bytes-consed) before) n))))))
      ;; Headroom over the ~16 B/iteration steady state.  The regression this
      ;; guards rebuilt up to three lists per call (hundreds of bytes), so 64
      ;; still fails decisively on it while not sitting on the true value.
      (is (< per-iteration 64)
          "one round of the three slot-name lookups consed ~a bytes (~,1F)"
          per-iteration (float per-iteration)))))

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

;;; One CL class namespace, per-graph schemas (GH #53): DEF-VERTEX/DEF-EDGE
;;; share the CL class namespace across graphs, so a second graph reusing a
;;; name used to silently clobber the first class's slots.

(test duplicate-class-name-across-graphs-errors
  "One CL class namespace, per-graph schemas: a second graph reusing a name
silently clobbered the first class's slots (GH #53)."
  (eval '(def-vertex dupchk-thing () ((alpha :type string)) :dupchk-one))
  (signals duplicate-node-class-error
    (eval '(def-vertex dupchk-thing () ((beta)) :dupchk-two)))
  (is (member 'alpha (mapcar #'graph-db::slot-definition-name
                             (graph-db::class-slots (find-class 'dupchk-thing))))
      "the original class must be untouched -- the guard runs before DEFCLASS"))

(test same-graph-redefinition-still-allowed
  "Runtime schema evolution must keep working; the check is on graph-name
identity, not on presence (GH #53)."
  (eval '(def-vertex samechk-thing () ((alpha :type string)) :samechk-one))
  (finishes (eval '(def-vertex samechk-thing () ((alpha :type string) (beta))
                    :samechk-one)))
  (is (member 'beta (mapcar #'graph-db::slot-definition-name
                            (graph-db::class-slots (find-class 'samechk-thing))))))

(test same-graph-redefinition-with-string-graph-name-still-allowed
  "GRAPH-NAME need not be a keyword -- *SCHEMA-NODE-METADATA* is EQUAL-keyed, so
a string name is a legal key too. The uniqueness check must compare with EQUAL,
not EQ: two distinct string objects with the same contents are not EQ, so an EQ
check would misidentify a same-graph redefinition as a cross-graph collision and
wrongly signal DUPLICATE-NODE-CLASS-ERROR (GH #53).
The two graph-name strings are built at RUNTIME via COPY-SEQ, spliced in with
backquote rather than written as quoted literals: two literal \"strchk-one\"
strings in source are similar literals, which a compiler is free to (and SBCL
does) coalesce into one EQ object in the fasl -- silently defeating this test's
whole premise."
  (let ((gn1 (copy-seq "strchk-one"))
        (gn2 (copy-seq "strchk-one")))
    (is (not (eq gn1 gn2))
        "test setup is broken: the two graph-name objects must not be EQ")
    (eval `(def-vertex strchk-thing ()
            ((alpha :type string)) ,gn1))
    (finishes
      (eval `(def-vertex strchk-thing ()
              ((alpha :type string) (beta)) ,gn2))))
  (is (member 'beta (mapcar #'graph-db::slot-definition-name
                            (graph-db::class-slots (find-class 'strchk-thing))))))

(test redefinition-replaces-its-registry-entry
  "UPDATE-SCHEMA replays every meta in the list on graph open, so accumulating
duplicates costs an instantiation per historical version, forever (GH #53)."
  (setf (gethash :regchk-one *schema-node-metadata*) nil)
  (eval '(def-vertex regchk-thing () ((alpha :type string)) :regchk-one))
  (eval '(def-vertex regchk-thing () ((alpha :type string) (beta)) :regchk-one))
  (eval '(def-vertex regchk-thing () ((alpha :type string) (beta) (gamma))
          :regchk-one))
  (is (= 1 (count 'regchk-thing
                  (gethash :regchk-one graph-db::*schema-node-metadata*)
                  :key #'graph-db::node-type-name))
      "three definitions must leave exactly one registry entry")
  (is (member 'gamma (mapcar #'graph-db::slot-definition-name
                             (graph-db::class-slots (find-class 'regchk-thing))))
      "and the surviving entry must be the NEWEST definition"))

(test redefinition-keeps-type-id-stable
  "Registry position drives type-id assignment: UPDATE-SCHEMA replays the list in
order on graph open and INSTANTIATE-NODE-TYPE hands out ids in that order, so a
redefined type that moved would get a DIFFERENT type-id on a fresh graph -- and
type-ids travel on the peer wire (GH #53)."
  (setf (gethash :tidchk-graph *schema-node-metadata*) nil)
  (eval '(def-vertex tidchk-a () ((x)) :tidchk-graph))
  (eval '(def-vertex tidchk-b () ((y)) :tidchk-graph))
  ;; Redefine the FIRST-declared type last; it must not drift to the end.
  (eval '(def-vertex tidchk-a () ((x) (z)) :tidchk-graph))
  (with-temp-directory (dir)
    (let ((g (make-graph :tidchk-graph (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (let ((id-a (graph-db::node-type-id
                        (graph-db::lookup-node-type-by-name 'tidchk-a :vertex
                                                            :graph g)))
                 (id-b (graph-db::node-type-id
                        (graph-db::lookup-node-type-by-name 'tidchk-b :vertex
                                                            :graph g))))
             (is (= 1 id-a)
                 "the first-declared type keeps type-id 1 across a redefinition, got ~a"
                 id-a)
             (is (= 2 id-b) "the second-declared type keeps type-id 2, got ~a" id-b))
        (ignore-errors (close-graph g :snapshot-p nil))
        (collect-garbage)))))

(test registry-replay-order-survives-reopen
  "OPEN-GRAPH runs UPDATE-SCHEMA again over the same registry list; an inverted
replay order would only surface here (GH #53)."
  (setf (gethash :ropenchk-graph *schema-node-metadata*) nil)
  (eval '(def-vertex ropenchk-a () ((x)) :ropenchk-graph))
  (eval '(def-vertex ropenchk-b () ((y)) :ropenchk-graph))
  (eval '(def-vertex ropenchk-a () ((x) (z)) :ropenchk-graph))
  (with-temp-directory (dir)
    (let (id-a id-b tid-a tid-b)
      (let ((g (make-graph :ropenchk-graph (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (setq id-a (id (eval '(make-ropenchk-a :x "a")))
                       id-b (id (eval '(make-ropenchk-b :y "b")))))
               (setq tid-a (graph-db::node-type-id
                            (graph-db::lookup-node-type-by-name 'ropenchk-a
                                                                :vertex :graph g))
                     tid-b (graph-db::node-type-id
                            (graph-db::lookup-node-type-by-name 'ropenchk-b
                                                                :vertex :graph g))))
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph :ropenchk-graph (namestring dir))))
        (unwind-protect
             (let ((*graph* g))
               (is (= tid-a (graph-db::node-type-id
                             (graph-db::lookup-node-type-by-name 'ropenchk-a
                                                                 :vertex :graph g)))
                   "type-id of the redefined type is unchanged by reopen")
               (is (= tid-b (graph-db::node-type-id
                             (graph-db::lookup-node-type-by-name 'ropenchk-b
                                                                 :vertex :graph g))))
               (is (typep (lookup-vertex id-a) 'ropenchk-a)
                   "a stored node still resolves to its own class after reopen")
               (is (typep (lookup-vertex id-b) 'ropenchk-b)))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))
