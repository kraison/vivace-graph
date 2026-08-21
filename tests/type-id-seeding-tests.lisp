;;;; Adopting global type-ids on a system that already has stores (GH #186).
;;;; The policy under test is spec §10.1's, and so is the shape of the
;;;; fixture: several stores, the low ids contested by nearly all of them,
;;;; and the store with the MOST types deliberately the SMALLEST on disk.
;;;; See docs/superpowers/specs/2026-08-20-namespaces-design.md.
(in-package #:graph-db/test)

(def-suite type-id-seeding-suite :in graph-db-suite
  :description "Seeding the type registry from populated stores, and the
renumbering migration that follows.")
(in-suite type-id-seeding-suite)

;;; Four stores' worth of types.  TS-SHARED-THING is declared under all four
;;; graph names -- one class instantiated in several stores, which is the
;;; case #186 makes legal -- and everything else is local to one store.
;;; Declaration order is what makes the ids collide: each store counts from
;;; 1, so vertex id 1 names a different symbol in every one of them.
(eval-when (:load-toplevel :execute)
  (dolist (name '(:ts-alpha :ts-beta :ts-gamma :ts-delta))
    (setf (gethash name *schema-node-metadata*) nil)))

(def-vertex ts-shared-thing () ((label :type string)) :ts-alpha)
(def-vertex ts-alpha-thing  () ((label :type string)) :ts-alpha)
(def-edge   ts-shared-link  () () :ts-alpha)

(def-vertex ts-beta-thing   () ((label :type string)) :ts-beta)
(def-vertex ts-shared-thing () ((label :type string)) :ts-beta)
(def-edge   ts-beta-link    () () :ts-beta)

(def-vertex ts-gamma-a      () ((label :type string)) :ts-gamma)
(def-vertex ts-gamma-b      () ((label :type string)) :ts-gamma)
(def-vertex ts-gamma-c      () ((label :type string)) :ts-gamma)
(def-vertex ts-shared-thing () ((label :type string)) :ts-gamma)
(def-edge   ts-gamma-link   () () :ts-gamma)

(def-vertex ts-delta-thing  () ((label :type string)) :ts-delta)
(def-vertex ts-shared-thing () ((label :type string)) :ts-delta)

(defun %fill-store (graph vertices edge-type extra-types)
  "Put VERTICES TS-SHARED-THINGs in GRAPH, chained by EDGE-TYPE edges when
one is given, plus one vertex of each of EXTRA-TYPES.  Nodes are made
through MAKE-VERTEX by type NAME so the store's own type-ids -- whatever
they are -- go into the heads."
  (let ((graph-db::*graph* graph)
        (previous nil))
    (with-transaction ()
      (dotimes (i vertices)
        (let ((v (graph-db::make-vertex
                  'ts-shared-thing
                  (list (cons :label (format nil "n~36R" i))))))
          (when (and previous edge-type)
            (graph-db::make-edge edge-type (graph-db::id previous)
                                 (graph-db::id v) 1.0 nil))
          (setq previous v)))
      (dolist (type extra-types)
        (graph-db::make-vertex type (list (cons :label "x")))))))

(defun %build-legacy-store (name root vertices edge-type extra-types)
  "Create the store NAME under ROOT, fill it, close it, return its location.

The store gets its OWN system directory, which is the whole point: that is
the pre-#186 shape this task exists for, where every store's type-ids count
from 1 and so the low ids name different classes in different stores.  The
private directory is left under ROOT and never read again."
  (let ((sysdir (namestring (merge-pathnames (format nil "sys-~A/" name)
                                             root)))
        (location (namestring (merge-pathnames (format nil "~A/" name)
                                               root))))
    (ensure-directories-exist sysdir)
    (ensure-directories-exist location)
    (let ((graph-db::*system-directory* sysdir)
          (graph-db::*type-registry* nil))
      (let ((g (make-graph name location :buffer-pool-size 1000)))
        (unwind-protect (%fill-store g vertices edge-type extra-types)
          (close-graph g))))
    (collect-garbage)
    location))

(defmacro with-legacy-stores ((stores root) &body body)
  "Bind ROOT to a scratch directory and STORES to ((:alpha . location) ...)
for four closed, populated stores whose type-ids all count from 1.

Sizes are chosen to separate the two rankings §10.1 says must not be
confused: :GAMMA has the most types and the fewest bytes, :ALPHA the most
bytes.  A seeding policy that counted types would pick :GAMMA."
  `(with-temp-directory (,root)
     (let ((,stores
             (list (cons :alpha (%build-legacy-store
                                 :ts-alpha ,root 120 'ts-shared-link
                                 '(ts-alpha-thing)))
                   (cons :beta (%build-legacy-store
                                :ts-beta ,root 40 'ts-beta-link
                                '(ts-beta-thing)))
                   (cons :gamma (%build-legacy-store
                                 :ts-gamma ,root 3 'ts-gamma-link
                                 '(ts-gamma-a ts-gamma-b ts-gamma-c)))
                   (cons :delta (%build-legacy-store
                                 :ts-delta ,root 10 nil
                                 '(ts-delta-thing))))))
       ,@body)))

(defun %fresh-registry (root &optional (name "system/"))
  "(values REGISTRY SYSDIR) for an empty system directory under ROOT."
  (let ((sysdir (namestring (merge-pathnames name root))))
    (ensure-directories-exist sysdir)
    (values (graph-db::open-type-registry sysdir) sysdir)))

(defun %store-entries (location)
  "(SYMBOL PARENT ID) for every type in the store at LOCATION, from its
schema.dat."
  (graph-db::%store-type-entries (graph-db::%store-schema location)))

(defun %store-duplicates (location)
  "The symbols the store at LOCATION holds more than one id for."
  (nth-value 1 (%store-entries location)))

(defun %seed-locations (stores)
  (mapcar #'cdr stores))

(defun %store-of (key stores)
  (cdr (assoc key stores)))

(defun %renumbered-p (location report)
  (and (member location (graph-db::seeding-report-renumber report)
               :test #'equal)
       t))

(defun %type-id-in (graph name parent)
  (graph-db::node-type-id
   (graph-db::lookup-node-type-by-name name parent :graph graph)))

(defun %stale-type-id (location name parent stale-id)
  "Leave the store at LOCATION holding a SECOND id for NAME, by writing a
stale copy of its metadata into schema.dat at STALE-ID.  This is §10.1's
one case no seeding policy exempts, and the shape a store's own history
produces -- the measured system had three of them."
  (let* ((file (format nil "~Aschema.dat" (pathname location)))
         (schema (cl-store:restore file))
         (sub (gethash parent (graph-db::schema-type-table schema)))
         (stale (graph-db::copy-node-type
                 (gethash (gethash name sub) sub))))
    (setf (graph-db::node-type-id stale) stale-id)
    (setf (gethash stale-id sub) stale)
    (setf (graph-db::schema-class-locks schema) nil)
    (setf (graph-db::schema-lock schema) nil)
    (cl-store:store schema file)
    location))

;;; ---------------------------------------------------------------------------
;;; Seeding
;;; ---------------------------------------------------------------------------

(test the-fixture-really-does-collide-on-the-low-ids
  "Guard on the fixture itself.  Every test below is about resolving a
collision, so a fixture that stopped colliding would leave them all passing
vacuously.  Vertex id 1 must name four different symbols."
  (with-legacy-stores (stores root)
    (declare (ignore root))
    (let ((claimants
            (remove-duplicates
             (mapcar (lambda (store)
                       (first (find-if (lambda (e)
                                         (and (eq (second e) :vertex)
                                              (eql (third e) 1)))
                                       (%store-entries (cdr store)))))
                     stores))))
      (is (= 4 (length claimants))
          "vertex id 1 must name four DIFFERENT symbols across the four ~
stores, got ~S" claimants))))

(test seeding-favours-the-largest-store-not-the-type-richest
  "Spec §10.1's policy, and the measurement behind it: all but one store
renumbers whichever is favoured, so the cost is bytes replayed.  The
type-richest store here is the smallest, exactly as on the measured system
where it held 59 of 95 types -- a policy that counted types would pick it
and rewrite the largest store instead."
  (with-legacy-stores (stores root)
    (multiple-value-bind (registry sysdir) (%fresh-registry root)
      (declare (ignore sysdir))
      (let* ((report (graph-db::registry-seed-from-stores
                      registry (%seed-locations stores)))
             (sizes (graph-db::seeding-report-sizes report))
             (alpha (%store-of :alpha stores))
             (gamma (%store-of :gamma stores)))
        (is (> (length (%store-entries gamma))
               (length (%store-entries alpha)))
            "fixture sanity: the gamma store must hold MORE types than the ~
alpha store, or this test cannot tell the two rankings apart")
        (is (< (cdr (assoc gamma sizes :test #'equal))
               (cdr (assoc alpha sizes :test #'equal)))
            "fixture sanity: the type-richest store must be the smaller one")
        (is (equal alpha (graph-db::seeding-report-seed report))
            "the largest store on disk is the one the registry adopts")
        (is (not (%renumbered-p alpha report))
            "and it is therefore the store that does NOT get rewritten")))))

(test the-seed-store-keeps-every-type-id-it-had
  "Adoption is verbatim: the seed store's ids are already written into every
node head, ve-key and type-index it owns, so keeping them is the whole
saving."
  (with-legacy-stores (stores root)
    (multiple-value-bind (registry sysdir) (%fresh-registry root)
      (declare (ignore sysdir))
      (graph-db::registry-seed-from-stores registry (%seed-locations stores))
      (dolist (entry (%store-entries (%store-of :alpha stores)))
        (destructuring-bind (symbol parent id) entry
          (is (eql id (graph-db::registry-id-for registry symbol parent))
              "~A must keep the id ~A it already holds on disk, got ~A"
              symbol id
              (graph-db::registry-id-for registry symbol parent)))))))

(test every-store-that-cannot-keep-its-ids-is-listed-for-renumbering
  "The report is the operator's whole instruction sheet: a store missing
from it is a store left holding ids that mean something else system-wide."
  (with-legacy-stores (stores root)
    (multiple-value-bind (registry sysdir) (%fresh-registry root)
      (declare (ignore sysdir))
      (let ((report (graph-db::registry-seed-from-stores
                     registry (%seed-locations stores))))
        (dolist (key '(:beta :gamma :delta))
          (let ((location (%store-of key stores)))
            (is (%renumbered-p location report)
                "the ~A store contests the seed store's low ids and must be ~
listed for renumbering" key)
            (is (find location (graph-db::seeding-report-changes report)
                      :key #'first :test #'equal)
                "and the report must say which of its ids moved")))))))

(test seeding-gives-one-symbol-one-id-and-one-id-one-symbol
  "The property the whole unit exists for, asserted over every type in the
system rather than over a chosen pair."
  (with-legacy-stores (stores root)
    (multiple-value-bind (registry sysdir) (%fresh-registry root)
      (declare (ignore sysdir))
      (graph-db::registry-seed-from-stores registry (%seed-locations stores))
      (let ((entries (graph-db::registry-entries registry))
            (by-symbol (make-hash-table :test 'equal))
            (by-id (make-hash-table :test 'equal)))
        (dolist (entry entries)
          (destructuring-bind (symbol parent id) entry
            (push id (gethash (cons symbol parent) by-symbol))
            (push symbol (gethash (cons parent id) by-id))))
        (is (plusp (hash-table-count by-symbol))
            "fixture sanity: seeding recorded something at all")
        (maphash (lambda (key ids)
                   (is (= 1 (length ids))
                       "~S must hold exactly one id, holds ~S" key ids))
                 by-symbol)
        (maphash (lambda (key symbols)
                   (is (= 1 (length symbols))
                       "~S must name exactly one symbol, names ~S"
                       key symbols))
                 by-id)))))

(test a-symbol-holding-two-ids-in-one-store-is-reported-and-renumbered
  "§10.1: no seeding policy exempts this case.  The store here is the ONLY
one seeded, so it wins every contest and would otherwise be left alone --
it must still be named, because its two ids have to unify."
  (with-temp-directory (root)
    (let ((location (%stale-type-id
                     (%build-legacy-store :ts-delta root 10 nil
                                          '(ts-delta-thing))
                     'ts-delta-thing :vertex 9)))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore sysdir))
        (let ((report (graph-db::registry-seed-from-stores
                       registry (list location))))
          (is (equal location (graph-db::seeding-report-seed report))
              "fixture sanity: the only store is the seed store")
          (is (null (graph-db::seeding-report-changes report))
              "fixture sanity: nothing contests it, so no id moves")
          (is (equal (list (list location 'ts-delta-thing :vertex 1 9))
                     (graph-db::seeding-report-duplicates report))
              "the two ids one symbol holds must be reported, both of them")
          (is (%renumbered-p location report)
              "and the store must be in the migration set anyway"))))))

(test seeding-defers-to-ids-the-registry-already-holds
  "The registry is the authority and may already have been distributed to
peers, so entries in it outrank even the largest store -- which then has to
renumber after all.  This is also the state MIGRATE-GRAPH used to leave
behind (#186): entries at ids no store uses."
  (with-legacy-stores (stores root)
    (multiple-value-bind (registry sysdir) (%fresh-registry root)
      (declare (ignore sysdir))
      (graph-db::registry-intern registry 'ts-prior-claim :vertex)
      (let* ((prior (graph-db::registry-intern registry 'ts-shared-thing
                                               :vertex))
             (report (graph-db::registry-seed-from-stores
                      registry (%seed-locations stores)))
             (alpha (%store-of :alpha stores)))
        (is (eql prior (graph-db::registry-id-for registry 'ts-shared-thing
                                                  :vertex))
            "an id already in the registry is never reassigned by seeding")
        (is (eql 1 (third (find 'ts-shared-thing (%store-entries alpha)
                                :key #'first)))
            "fixture sanity: the seed store holds a different id on disk")
        (is (%renumbered-p alpha report)
            "so even the largest store is listed for renumbering")))))

;;; ---------------------------------------------------------------------------
;;; The renumbering migration
;;; ---------------------------------------------------------------------------

(defun %class-census (graph)
  "((class . count) ...) over GRAPH's live vertices, sorted by class name --
what must survive a renumbering, since the id under it does not."
  (let ((counts (make-hash-table :test 'eq))
        (graph-db::*graph* graph))   ; MAP-VERTICES' all-types branch reads it
    (graph-db::map-vertices (lambda (v) (incf (gethash (type-of v) counts 0)))
                            graph)
    (sort (alexandria:hash-table-alist counts) #'string< :key #'car)))

(test renumbering-migration-gives-colliding-stores-distinct-ids
  "The unit's payload.  Two stores that both called a type id 1 come out of
the migration agreeing: the shared symbol has ONE id in both, the two
formerly-colliding local symbols have different ones, and every node is
still an instance of the class it was."
  (with-legacy-stores (stores root)
    (multiple-value-bind (registry sysdir) (%fresh-registry root)
      (let ((beta (%store-of :beta stores)))
        (graph-db::registry-seed-from-stores registry
                                             (%seed-locations stores))
        (let ((before nil) (after nil))
          (let ((graph-db::*system-directory* sysdir)
                (graph-db::*type-registry* nil))
            (let ((old (open-graph :ts-beta beta :buffer-pool-p nil
                                   :gc-heap-p nil)))
              (unwind-protect (setq before (%class-census old))
                (close-graph old :snapshot-p nil)))
            (let ((g (graph-db::migrate-graph
                      :ts-beta beta
                      (namestring (merge-pathnames "beta-renumbered/" root))
                      :package :graph-db/test :renumber-p t)))
              (unwind-protect
                   (progn
                     (setq after (%class-census g))
                     (is (eql (graph-db::registry-id-for
                               registry 'ts-shared-thing :vertex)
                              (%type-id-in g 'ts-shared-thing :vertex))
                         "the migrated store's id for the shared symbol is ~
the registry's, which is the id the seed store already holds")
                     (is (not (eql (%type-id-in g 'ts-beta-thing :vertex)
                                   (graph-db::registry-id-for
                                    registry 'ts-shared-thing :vertex)))
                         "and the symbol that used to share id 1 with it no ~
longer does")
                     (is (eql (graph-db::registry-id-for
                               registry 'ts-beta-link :edge)
                              (%type-id-in g 'ts-beta-link :edge))
                         "edges are a separate id space and are renumbered ~
too"))
                (close-graph g)))
            (collect-garbage))
          (is (equal before after)
              "every node keeps its CLASS across a renumbering: ~S became ~S"
              before after)
          (is (< 0 (length after))
              "fixture sanity: the store is not empty"))))))

(test renumbering-migration-unifies-a-symbol-that-held-two-ids
  "The unification §10.1 requires, and MIGRATE-GRAPH says so in its second
value rather than picking one id and moving on."
  (with-temp-directory (root)
    (let ((location (%stale-type-id
                     (%build-legacy-store :ts-delta root 10 nil
                                          '(ts-delta-thing))
                     'ts-delta-thing :vertex 9)))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore registry))
        (let ((graph-db::*system-directory* sysdir)
              (graph-db::*type-registry* nil))
          (multiple-value-bind (g unified)
              (graph-db::migrate-graph
               :ts-delta location
               (namestring (merge-pathnames "delta-renumbered/" root))
               :package :graph-db/test :renumber-p t)
            (unwind-protect
                 ;; Both duplicate lists are bound BEFORE the checks rather
                 ;; than read inside one: FiveAM pulls a check's form apart
                 ;; to report it, and an NTH-VALUE at the top of one reads
                 ;; back NIL however many values the call really returned.
                 (let ((source (%store-duplicates location))
                       (migrated (nth-value 1 (graph-db::%store-type-entries
                                               (graph-db::schema g)))))
                   (is (equal '((ts-delta-thing :vertex (1 9) 1)) unified)
                       "the migration must name the symbol, both its old ~
ids and the one they unified to, got ~S" unified)
                   (is (consp source)
                       "fixture sanity: the SOURCE store still holds the ~
duplicate -- MIGRATE-GRAPH does not rewrite it")
                   (is (null migrated)
                       "the migrated store holds exactly one id for it, ~
holds ~S" migrated)
                   (is (= 10 (length (graph-db::map-vertices
                                      #'graph-db::id g :collect-p t
                                      :vertex-type 'ts-shared-thing)))
                       "and no node is lost to the unification"))
              (close-graph g))))))))

(test migrating-without-renumbering-leaves-the-registry-untouched
  "MIGRATE-GRAPH's default preserves the source's per-graph type-ids, so it
must not ALSO claim ids for those names in the system registry: the store
would then be using one set of ids while the registry published another,
which is the two-regime divergence #186 exists to remove -- arriving through
the migration path.  Before the fix, MAKE-GRAPH's UPDATE-SCHEMA interned
every type of the graph and the schema swap on the next form threw the ids
away, leaving the registry holding ids no store uses."
  (with-temp-directory (root)
    (let ((location (%build-legacy-store :ts-delta root 10 nil
                                         '(ts-delta-thing))))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore registry))
        (let ((graph-db::*system-directory* sysdir)
              (graph-db::*type-registry* nil))
          (close-graph
           (graph-db::migrate-graph
            :ts-delta location
            (namestring (merge-pathnames "delta-preserved/" root))
            :package :graph-db/test)))
        (collect-garbage)
        (is (null (graph-db::registry-entries
                   (graph-db::open-type-registry sysdir)))
            "a :RENUMBER-P NIL migration must leave the registry empty, ~
it holds ~S"
            (graph-db::registry-entries
             (graph-db::open-type-registry sysdir)))))))

(test migrating-without-renumbering-preserves-every-type-id
  "The other half of the mode-dependent guarantee (#166 asserts this of the
v1 and v2 fixtures; here it is on a current-format store, beside the
renumbering test it is the exact reverse of)."
  (with-temp-directory (root)
    (let ((location (%build-legacy-store :ts-delta root 10 nil
                                         '(ts-delta-thing))))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore registry))
        (let ((expected (%store-entries location)))
          (let ((graph-db::*system-directory* sysdir)
                (graph-db::*type-registry* nil))
            (let ((g (graph-db::migrate-graph
                      :ts-delta location
                      (namestring (merge-pathnames "delta-preserved/" root))
                      :package :graph-db/test :renumber-p nil)))
              (unwind-protect
                   (dolist (entry expected)
                     (destructuring-bind (symbol parent id) entry
                       (is (eql id (%type-id-in g symbol parent))
                           "~A's type-id must survive a :RENUMBER-P NIL ~
migration unchanged (expected ~A, got ~A)"
                           symbol id (%type-id-in g symbol parent))))
                (close-graph g))))
          (collect-garbage))))))

(test migration-does-not-rewrite-the-source-s-schema
  "The manual's rollback story says MIGRATE-GRAPH leaves the source alone;
until #186 that came with an exception -- every open rewrote schema.dat --
and the exception was load-bearing, because that same UPDATE-SCHEMA is what
minted the registry ids the migration then discarded.  Both opens now skip
the schema replay, so the file is not touched at all."
  (with-temp-directory (root)
    (let* ((location (%build-legacy-store :ts-delta root 10 nil
                                          '(ts-delta-thing)))
           (file (merge-pathnames "schema.dat" (pathname location)))
           (before-date (file-write-date file))
           (before-size (with-open-file (s file :element-type
                                             '(unsigned-byte 8))
                          (file-length s))))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore registry))
        (let ((graph-db::*system-directory* sysdir)
              (graph-db::*type-registry* nil))
          (close-graph (graph-db::migrate-graph
                        :ts-delta location
                        (namestring (merge-pathnames "out/" root))
                        :package :graph-db/test)))
        (collect-garbage)
        (is (eql before-date (file-write-date file))
            "the source's schema.dat must not be rewritten by a migration")
        (is (eql before-size
                 (with-open-file (s file :element-type '(unsigned-byte 8))
                   (file-length s)))
            "nor changed in size")))))
