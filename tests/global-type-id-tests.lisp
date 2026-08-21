;;;; Type-ids are assigned from the image-level registry (GH #186).
(in-package #:graph-db/test)

(def-suite global-type-id-suite :in graph-db-suite
  :description "Type-id assignment through the system-wide registry.")
(in-suite global-type-id-suite)

;;; Declaration ORDER is chosen so that BOTH tests below fail against the
;;; per-graph counters #186 replaces -- neither may pass vacuously against
;;; the implementation it exists to reject.  Under those counters the ids
;;; would be:
;;;
;;;   store 1: SHARED-TYPE 1, TYPE-IN-ONE 2, DUAL-TYPE 3
;;;   store 2: REG-FILLER 1, TYPE-IN-TWO 2, SHARED-TYPE 3, DUAL-TYPE 4
;;;
;;; so SHARED-TYPE differs across the stores (1 vs 3) AND TYPE-IN-ONE collides
;;; with TYPE-IN-TWO (both 2).  REG-FILLER exists only to offset store 2's
;;; counter; the two properties cannot both be violated without it.
(def-vertex shared-type () ((label :type string)) :reg-store-1)
(def-vertex type-in-one () ((label :type string)) :reg-store-1)
(def-vertex dual-type   () ((label :type string)) :reg-store-1)

(def-vertex reg-filler  () ((label :type string)) :reg-store-2)
(def-vertex type-in-two () ((label :type string)) :reg-store-2)
(def-vertex shared-type () ((label :type string)) :reg-store-2)
(def-vertex dual-type   () ((label :type string)) :reg-store-2)

(defmacro with-two-test-graphs ((g1 g2 sysdir) &body body)
  "Two stores in ONE image, sharing SYSDIR as their system directory.  The
existing WITH-TEST-GRAPH binds a single hardcoded graph name and cannot
express this, which is why #186 needs its own helper."
  (let ((s (gensym)) (d1 (gensym)) (d2 (gensym)))
    ;; IGNORABLE, not the caller's (DECLARE (IGNORE ...)): BODY lands inside
    ;; an UNWIND-PROTECT's PROGN, which is not a declaration context.
    `(with-temp-directory (,s)
       (with-temp-directory (,d1)
         (with-temp-directory (,d2)
           (let* ((,sysdir (namestring ,s))
                  (graph-db::*system-directory* ,sysdir))
             (declare (ignorable ,sysdir))
             (let ((,g1 (make-graph :reg-store-1 (namestring ,d1)
                                    :buffer-pool-size 1000))
                   (,g2 (make-graph :reg-store-2 (namestring ,d2)
                                    :buffer-pool-size 1000)))
               (declare (ignorable ,g1 ,g2))
               (unwind-protect (progn ,@body)
                 (ignore-errors (close-graph ,g1 :snapshot-p nil))
                 (ignore-errors (close-graph ,g2 :snapshot-p nil))
                 (collect-garbage)))))))))

(defun %type-id-of (sym parent graph)
  (graph-db::node-type-id
   (graph-db::lookup-node-type-by-name sym parent :graph graph)))

(test two-graphs-in-one-image-share-a-symbol-s-type-id
  "The unit's entire purpose.  Before #186 each graph counted from 1, so the
same symbol got different ids in different stores and different symbols
collided on one id."
  (with-two-test-graphs (g1 g2 sysdir)
    (is (= (%type-id-of 'shared-type :vertex g1)
           (%type-id-of 'shared-type :vertex g2))
        "one symbol, one id, both stores")))

(test distinct-symbols-never-collide-across-graphs
  (with-two-test-graphs (g1 g2 sysdir)
    (is (/= (%type-id-of 'type-in-one :vertex g1)
            (%type-id-of 'type-in-two :vertex g2))
        "two symbols never share an id, even in different stores")))

(test opening-a-graph-without-a-system-directory-signals
  "The directory is mandatory as of #186: the registry has nowhere to live
without one, and a graph opened outside a system would mint ids that mean
nothing to anyone else.  Refuse rather than silently fall back to per-graph
counters -- a silent fallback is how two id regimes diverge unnoticed."
  (with-temp-directory (dir)
    (let ((graph-db::*system-directory* nil))
      (signals graph-db:system-directory-required
        (make-graph :reg-nodir (namestring dir))))))

(test opening-an-existing-graph-without-a-system-directory-signals
  "The refusal covers OPEN-GRAPH, not only MAKE-GRAPH: reopening replays the
schema and assigns ids to any type added since (#186)."
  (with-temp-directory (sysdir)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sysdir)))
        (close-graph (make-graph :reg-store-1 (namestring dir)
                                 :buffer-pool-size 1000)
                     :snapshot-p nil))
      (collect-garbage)
      (let ((graph-db::*system-directory* nil))
        (signals graph-db:system-directory-required
          (open-graph :reg-store-1 (namestring dir)))))))

(test ensure-type-registry-signals-without-a-system-directory
  "The accessor every assignment path funnels through refuses on its own.
The checks in MAKE-GRAPH/OPEN-GRAPH are early, legible ones; this is what
makes a silent per-graph fallback impossible rather than merely absent from
those two entry points."
  (let ((graph-db::*system-directory* nil))
    (signals graph-db:system-directory-required
      (graph-db::ensure-type-registry))))

(test a-class-may-be-instantiated-in-more-than-one-store
  "Closes cl-llm#20.  %CHECK-NODE-CLASS-GRAPH-UNIQUE refused this and existed
only because ids were per-graph."
  (with-two-test-graphs (g1 g2 sysdir)
    (is-true (graph-db::lookup-node-type-by-name
              'dual-type :vertex :graph g1))
    (is-true (graph-db::lookup-node-type-by-name
              'dual-type :vertex :graph g2))))

(test the-registry-is-what-the-schema-records
  "The id in a store's schema is the registry's, not a store-local number:
otherwise the two could agree by coincidence of declaration order alone."
  (with-two-test-graphs (g1 g2 sysdir)
    (let ((r (graph-db::ensure-type-registry)))
      (is (= (graph-db::registry-id-for r 'shared-type :vertex)
             (%type-id-of 'shared-type :vertex g1))
          "the store's id for a symbol is the one the registry holds"))
    (is (probe-file (merge-pathnames "type-registry.log" sysdir))
        "assignment is persisted, not merely in-memory")))

;;; ---------------------------------------------------------------------------
;;; A store's persisted ids and the image registry must agree (GH #186).
;;;
;;; INSTANTIATE-NODE-TYPE has two branches and only one of them touches the
;;; registry: a type already in the store's schema keeps its persisted id and
;;; the registry is never told, while a type new to the store mints from a
;;; counter that knows nothing of those ids.  Nothing in between establishes
;;; that the two agree, so the ordinary upgrade -- open an existing store
;;; under a fresh system directory, then ship one more DEF-VERTEX -- lets a
;;; minted id land on an id the store already uses.
;;; ---------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (setf (gethash :reg-upgrade *schema-node-metadata*) nil))

;; What release 1 shipped.  Both id spaces start at 1, so the ids that a
;; later release's fresh registry mints are exactly these.
(def-vertex ru-shipped-a    () ((label :type string)) :reg-upgrade)
(def-edge   ru-shipped-link () ()                     :reg-upgrade)

;; What release 2 adds.  Declared here so the classes exist; kept out of the
;; store by %AS-RELEASE-1 while release 1's store is built.
(def-vertex ru-added-later  () ((label :type string)) :reg-upgrade)
(def-edge   ru-added-link   () ()                     :reg-upgrade)

(defparameter *ru-release-2-types* '(ru-added-later ru-added-link))

(defmacro %as-release-1 (&body body)
  "Run BODY with only release 1's types registered for :REG-UPGRADE, so a
store built inside it has never heard of the ones release 2 adds."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (gethash :reg-upgrade *schema-node-metadata*)))
       (unwind-protect
            (progn
              (setf (gethash :reg-upgrade *schema-node-metadata*)
                    (remove-if (lambda (m)
                                 (member (graph-db::node-type-name m)
                                         *ru-release-2-types*))
                               ,saved))
              ,@body)
         (setf (gethash :reg-upgrade *schema-node-metadata*) ,saved)))))

(defun %ru-build-release-1-store (location sysdir)
  "Build the store release 1 leaves behind: two types under their own system
directory, and one node of each so the persisted ids are in node heads."
  (let ((graph-db::*system-directory* (namestring sysdir))
        (graph-db::*type-registry* nil))
    (%as-release-1
      (let ((g (make-graph :reg-upgrade (namestring location)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db::*graph* g))
               (with-transaction ()
                 (let ((v1 (graph-db::make-vertex
                            'ru-shipped-a (list (cons :label "one"))))
                       (v2 (graph-db::make-vertex
                            'ru-shipped-a (list (cons :label "two")))))
                   (graph-db::make-edge 'ru-shipped-link
                                        (graph-db::id v1) (graph-db::id v2)
                                        1.0 nil))))
          (close-graph g :snapshot-p nil))
        (collect-garbage)))))

(test a-minted-id-never-lands-on-an-id-the-store-already-uses
  "The ordinary upgrade, one store, no multi-store system required.  A v3
deployment opens an existing store under a FRESH system directory -- what the
manual tells an operator to do -- so every persisted type takes the branch
that keeps its own id and the registry stays EMPTY.  The next release adds one
DEF-VERTEX, which mints 1 from that empty registry; UPDATE-NODE-TYPE then
overwrites id 1 -> meta unconditionally and the store's first type is gone
from its own schema.

Every persisted node of that type now materialises as the type added later,
new writes of both types go out under one id, and the type-index list for 1
mixes them.  Silent, and unrecoverable without the pre-upgrade backup.

Both parent kinds are checked because the registry counts them separately and
both start at 1, so the edge half fails the same way and independently."
  (with-temp-directory (dir)
    (with-temp-directory (sys1)
      (%ru-build-release-1-store dir sys1))
    (with-temp-directory (sys2)
      (let ((graph-db::*system-directory* (namestring sys2))
            (graph-db::*type-registry* nil))
        (let ((g (open-graph :reg-upgrade (namestring dir))))
          (unwind-protect
               (let ((graph-db::*graph* g))
                 (flet ((id-of (sym parent) (%type-id-of sym parent g))
                        (name-at (id parent)
                          (let ((meta (graph-db::lookup-node-type-by-id
                                       id parent :graph g)))
                            (and meta (graph-db::node-type-name meta)))))
                   (is (/= (id-of 'ru-shipped-a :vertex)
                           (id-of 'ru-added-later :vertex))
                       "the vertex type added later took id ~D, which ~
RU-SHIPPED-A already uses"
                       (id-of 'ru-added-later :vertex))
                   (is (/= (id-of 'ru-shipped-link :edge)
                           (id-of 'ru-added-link :edge))
                       "the edge type added later took id ~D, which ~
RU-SHIPPED-LINK already uses"
                       (id-of 'ru-added-link :edge))
                   ;; The id -> meta direction is the one UPDATE-NODE-TYPE
                   ;; clobbers, and it is what every node head resolves
                   ;; through.
                   (is (eq 'ru-shipped-a
                           (name-at (id-of 'ru-shipped-a :vertex) :vertex))
                       "vertex id ~D must still name RU-SHIPPED-A, it names ~S"
                       (id-of 'ru-shipped-a :vertex)
                       (name-at (id-of 'ru-shipped-a :vertex) :vertex))
                   (is (eq 'ru-shipped-link
                           (name-at (id-of 'ru-shipped-link :edge) :edge))
                       "edge id ~D must still name RU-SHIPPED-LINK, it ~
names ~S"
                       (id-of 'ru-shipped-link :edge)
                       (name-at (id-of 'ru-shipped-link :edge) :edge)))
                 ;; And the data: two persisted nodes, both still their own
                 ;; class.  This is the assertion an operator would notice.
                 (let ((classes (sort (graph-db::map-vertices
                                       (lambda (v) (type-of v))
                                       g :collect-p t)
                                      #'string< :key #'symbol-name)))
                   (is (equal '(ru-shipped-a ru-shipped-a) classes)
                       "the persisted nodes must still materialise as ~
RU-SHIPPED-A; they came back as ~S" classes)))
            (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test opening-a-store-the-registry-contradicts-is-refused
  "The other half of reconciliation, and what makes the peer type table
honest: the table is the registry while the wire carries STORE ids, so a
store whose ids the registry contradicts must never be opened for use.

Built the only way a real system builds one -- two stores numbered from 1
under system directories of their own, then brought under a single registry,
which is the pre-#186 estate the adoption procedure exists for.  The refusal
names both sides and points at the seeding run; it is not a retryable error."
  (with-temp-directory (root)
    (let ((first-dir (namestring (merge-pathnames "one/" root)))
          (second-dir (namestring (merge-pathnames "two/" root))))
      ;; Two stores, each numbered from 1 in its own system.
      (dolist (spec (list (list first-dir "sys-one/" :reg-store-1)
                          (list second-dir "sys-two/" :reg-store-2)))
        (destructuring-bind (dir sub name) spec
          (let ((graph-db::*system-directory*
                  (namestring (ensure-directories-exist
                               (merge-pathnames sub root))))
                (graph-db::*type-registry* nil))
            (close-graph (make-graph name dir :buffer-pool-size 1000)
                         :snapshot-p nil)
            (collect-garbage))))
      ;; Now one registry for both.  The first store opened adopts; the
      ;; second contradicts it, because SHARED-TYPE and REG-FILLER both hold
      ;; vertex id 1 (see the declaration order at the top of this file).
      (with-temp-directory (shared)
        (let ((graph-db::*system-directory* (namestring shared))
              (graph-db::*type-registry* nil))
          (close-graph (open-graph :reg-store-1 first-dir) :snapshot-p nil)
          (collect-garbage)
          (let ((registry (graph-db::ensure-type-registry)))
            (is (eql 1 (graph-db::registry-id-for registry 'shared-type
                                                  :vertex))
                "fixture sanity: the first store's id 1 was adopted"))
          (signals store-registry-conflict
            (open-graph :reg-store-2 second-dir))
          ;; ...and it is readable, so an operator can still get at it.
          (let ((g (with-schema-frozen ()
                     (open-graph :reg-store-2 second-dir))))
            (unwind-protect
                 (is (eql 1 (%type-id-of 'reg-filler :vertex g))
                     "a frozen open still sees the store's own id")
              (close-graph g :snapshot-p nil))
            (collect-garbage)))))))
