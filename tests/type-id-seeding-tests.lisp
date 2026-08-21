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

;; Declared against a graph name nothing here ever creates, so no fixture
;; store holds it.  %WITH-LATE-TYPE splices a copy of its metadata into
;; another graph's list at RUN time, which is the shape MIGRATE-GRAPH's
;; source open must not act on (GH #186).
(eval-when (:load-toplevel :execute)
  (setf (gethash :ts-unbuilt *schema-node-metadata*) nil))
(def-vertex ts-late-thing () ((label :type string)) :ts-unbuilt)

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

(defun %store-sysdir (name root)
  "The private system directory a legacy fixture store is built under."
  (namestring (merge-pathnames (format nil "sys-~A/" name) root)))

(defun %build-legacy-store (name root vertices edge-type extra-types)
  "Create the store NAME under ROOT, fill it, close it, return its location.

The store gets its OWN system directory, which is the whole point: that is
the pre-#186 shape this task exists for, where every store's type-ids count
from 1 and so the low ids name different classes in different stores.  The
private directory is left under ROOT and read again only by
%SPLIT-HISTORY-STORE."
  (let ((sysdir (%store-sysdir name root))
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

(defun %add-vertices (graph type n)
  "Write N vertices of TYPE into GRAPH, under whatever type-id its schema
currently gives TYPE."
  (let ((graph-db::*graph* graph))
    (with-transaction ()
      (dotimes (i n)
        (graph-db::make-vertex type
                               (list (cons :label (format nil "m~D" i))))))))

(defun %move-current-type-id (location name parent new-id)
  "Rewrite LOCATION's schema.dat so NAME's CURRENT id under PARENT becomes
NEW-ID, leaving the metadata at the old id in place -- which is how a store
comes to hold two ids for one name across its history.  Returns the old id."
  (let* ((file (format nil "~Aschema.dat" (pathname location)))
         (schema (cl-store:restore file))
         (sub (gethash parent (graph-db::schema-type-table schema)))
         (old-id (gethash name sub))
         (moved (graph-db::copy-node-type (gethash old-id sub))))
    (setf (graph-db::node-type-id moved) new-id)
    (setf (gethash new-id sub) moved)
    (setf (gethash name sub) new-id)
    (setf (gethash (intern (symbol-name name) :keyword) sub) new-id)
    (setf (graph-db::schema-class-locks schema) nil)
    (setf (graph-db::schema-lock schema) nil)
    (cl-store:store schema file)
    old-id))

(defun %split-history-store (root before after new-id)
  "A :TS-DELTA store whose history really does hold TS-DELTA-THING nodes
under TWO type-ids: BEFORE of them written under the id it was assigned,
then the schema moved so the type's CURRENT id is NEW-ID, then AFTER more
written under that one.  Returns (values LOCATION OLD-ID).

Built by writing nodes on both sides of the move rather than by editing
metadata around nodes that never used the losing id: only then can a
migration that DROPS the nodes at the losing id be told from one that
carries them across (§10.1, the case no seeding policy exempts)."
  (let* ((location (%build-legacy-store :ts-delta root 10 nil nil))
         (sysdir (%store-sysdir :ts-delta root))
         (old-id nil))
    (let ((graph-db::*system-directory* sysdir)
          (graph-db::*type-registry* nil))
      (let ((g (open-graph :ts-delta location :buffer-pool-size 1000)))
        (unwind-protect (%add-vertices g 'ts-delta-thing before)
          (close-graph g)))
      (collect-garbage)
      (setq old-id (%move-current-type-id location 'ts-delta-thing :vertex
                                          new-id))
      (let ((g (open-graph :ts-delta location :buffer-pool-size 1000)))
        (unwind-protect (%add-vertices g 'ts-delta-thing after)
          (close-graph g)))
      (collect-garbage))
    (values location old-id)))

(defmacro with-legacy-stores ((stores root) &body body)
  "Bind ROOT to a scratch directory and STORES to ((:alpha . location) ...)
for four closed, populated stores whose type-ids all count from 1.

Sizes are chosen to separate the three rankings a seeding policy could be
confusing: :GAMMA has the most types and the fewest bytes, :ALPHA the most
bytes, and %SEED-LOCATIONS hands them over in an order that is neither.
A policy that counted types would pick :GAMMA; one that took the first (or
the last) location given would pick :DELTA (or :ALPHA) for the wrong
reason."
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
                                 :ts-delta ,root 25 nil
                                 '(ts-delta-thing))))))
       ,@body)))

(defmacro %with-late-type ((graph-name) &body body)
  "Run BODY with TS-LATE-THING registered as a type of GRAPH-NAME that no
store on disk has, restoring the metadata list afterwards.

This is the state MIGRATE-GRAPH's SOURCE open has to be inert against: the
schema replay would otherwise add the type to the source store and mint a
registry id for it, in a store whose every other id is per-graph (#186).
A store that simply reopens is supposed to pick the type up -- only the
migration's two opens are suppressed -- so the registration is undone here
rather than left for the next test."
  (let ((saved (gensym "SAVED")) (name (gensym "NAME")))
    `(let* ((,name ,graph-name)
            (,saved (gethash ,name *schema-node-metadata*)))
       (unwind-protect
            (progn
              (setf (gethash ,name *schema-node-metadata*)
                    (append ,saved
                            (list (graph-db::copy-node-type
                                   (first (gethash :ts-unbuilt
                                                   *schema-node-metadata*))))))
              ,@body)
         (setf (gethash ,name *schema-node-metadata*) ,saved)))))

(defun %knows-type-p (location name parent)
  "True when the store at LOCATION's own schema.dat holds NAME."
  (and (find-if (lambda (entry)
                  (and (eq (first entry) name) (eq (second entry) parent)))
                (%store-entries location))
       t))

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
  "The store locations to seed from, deliberately REVERSED -- delta, gamma,
beta, alpha.  That is neither the by-bytes ranking nor its reverse, so a
seeding policy that ranked by argument position rather than by size cannot
produce the expected answer by accident (spec §10.1)."
  (reverse (mapcar #'cdr stores)))

(defun %store-of (key stores)
  (cdr (assoc key stores)))

(defun %renumbered-p (location report)
  (and (member location (graph-db::seeding-report-renumber report)
               :test #'equal)
       t))

(defun %type-id-in (graph name parent)
  (graph-db::node-type-id
   (graph-db::lookup-node-type-by-name name parent :graph graph)))

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
        (is (not (equal (%seed-locations stores) (mapcar #'car sizes)))
            "fixture sanity: the order the locations were PASSED in must ~
differ from the ranking, or this test cannot tell size from position")
        (is (equal (list alpha (%store-of :beta stores)
                         (%store-of :delta stores) gamma)
                   (mapcar #'car sizes))
            "the whole ranking, in order, is by bytes on disk -- not by ~
type count and not by the order the locations were given: got ~S"
            (mapcar #'car sizes))
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
    (multiple-value-bind (location old-id) (%split-history-store root 4 6 9)
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore sysdir))
        (let ((report (graph-db::registry-seed-from-stores
                       registry (list location))))
          (is (equal location (graph-db::seeding-report-seed report))
              "fixture sanity: the only store is the seed store")
          (is (null (graph-db::seeding-report-changes report))
              "fixture sanity: nothing contests it, so no id moves")
          (is (equal (list (list location 'ts-delta-thing :vertex old-id 9))
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

(defun %census-count (census class)
  "How many nodes of CLASS the census counted, 0 if none."
  (or (cdr (assoc class census)) 0))

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
                     (is (eql (graph-db::registry-id-for
                               registry 'ts-beta-thing :vertex)
                              (%type-id-in g 'ts-beta-thing :vertex))
                         "the symbol that used to share id 1 with it takes ~
the registry's id too -- not merely SOME other id")
                     (is (not (eql (%type-id-in g 'ts-beta-thing :vertex)
                                   (graph-db::registry-id-for
                                    registry 'ts-shared-thing :vertex)))
                         "so the two no longer collide")
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
  "The unification §10.1 requires: the nodes written under BOTH of the
store's ids for one name come across under the single surviving id, and
MIGRATE-GRAPH says which name it did that to rather than picking an id and
moving on.  The fixture wrote 4 nodes under the losing id and 6 under the
winning one, so an implementation that carried only the current id's nodes
across loses 4 and fails the census."
  (with-temp-directory (root)
    (multiple-value-bind (location old-id) (%split-history-store root 4 6 9)
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore registry))
        (let ((before nil))
          (let ((graph-db::*system-directory* sysdir)
                (graph-db::*type-registry* nil))
            (let ((old (open-graph :ts-delta location :buffer-pool-p nil
                                   :gc-heap-p nil)))
              (unwind-protect (setq before (%class-census old))
                (close-graph old :snapshot-p nil)))
            (collect-garbage)
            (is (equal '(10 . 10) (cons (%census-count before 'ts-delta-thing)
                                        (%census-count before
                                                       'ts-shared-thing)))
                "fixture sanity: the source must hold all 10 TS-DELTA-THINGs ~
-- 4 under the losing id and 6 under the winning one -- and 10 ~
TS-SHARED-THINGs, got ~S" before)
            (multiple-value-bind (g unified)
                (graph-db::migrate-graph
                 :ts-delta location
                 (namestring (merge-pathnames "delta-renumbered/" root))
                 :package :graph-db/test :renumber-p t)
              (unwind-protect
                   ;; The duplicate list is bound BEFORE the checks rather
                   ;; than read inside one: FiveAM pulls a check's form
                   ;; apart to report it, and an NTH-VALUE at the top of one
                   ;; reads back NIL however many values the call returned.
                   (let ((source (%store-duplicates location))
                         (migrated (nth-value 1
                                    (graph-db::%store-type-entries
                                     (graph-db::schema g))))
                         (entry (first unified)))
                     (is (= 1 (length unified))
                         "exactly one name unified, got ~S" unified)
                     (is (equal (list 'ts-delta-thing :vertex
                                      (sort (list old-id 9) #'<))
                                (list (first entry) (second entry)
                                      (third entry)))
                         "the migration must name the symbol and BOTH its ~
old ids, got ~S" entry)
                     ;; The unified id is whatever the registry minted, so
                     ;; assert it against the store rather than a literal --
                     ;; a fresh registry's assignment order is not fixed.
                     (is (eql (fourth entry)
                              (%type-id-in g 'ts-delta-thing :vertex))
                         "and the id it reports must be the one the ~
migrated store actually uses")
                     (is (consp source)
                         "fixture sanity: the SOURCE store still holds the ~
duplicate -- MIGRATE-GRAPH does not rewrite it")
                     (is (null migrated)
                         "the migrated store holds exactly one id for it, ~
holds ~S" migrated)
                     (is (equal before (%class-census g))
                         "every node written under EITHER id comes across, ~
under the unified one: ~S became ~S" before (%class-census g)))
                (close-graph g)))
            (collect-garbage)))))))

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

(test migration-does-not-add-a-late-type-to-the-source
  "The SOURCE half of the suppression, which the two tests above cannot
reach: they declare only types the source already holds, so the schema
replay takes its already-known branch and never mints anything.  Register a
type the source has NEVER seen against its graph name, and an unsuppressed
source open would both write it into the source's schema.dat and take a
registry id for it -- into a store whose every other id is per-graph, and
into a registry the :RENUMBER-P NIL migration is supposed to leave alone
(GH #186)."
  (with-temp-directory (root)
    (let ((location (%build-legacy-store :ts-delta root 10 nil
                                         '(ts-delta-thing))))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (declare (ignore registry))
        (is (not (%knows-type-p location 'ts-late-thing :vertex))
            "fixture sanity: the source must not already know the type")
        (%with-late-type (:ts-delta)
          (let ((graph-db::*system-directory* sysdir)
                (graph-db::*type-registry* nil))
            (close-graph
             (graph-db::migrate-graph
              :ts-delta location
              (namestring (merge-pathnames "delta-late/" root))
              :package :graph-db/test)))
          (collect-garbage))
        (is (not (%knows-type-p location 'ts-late-thing :vertex))
            "the migration must not add the type to the SOURCE store")
        (is (null (graph-db::registry-entries
                   (graph-db::open-type-registry sysdir)))
            "nor take a registry id for it: the registry holds ~S"
            (graph-db::registry-entries
             (graph-db::open-type-registry sysdir)))))))

;;; ---------------------------------------------------------------------------
;;; The renumbering mode against a LEGACY source.  Everything above builds its
;;; stores in this image and in the current format; this is the combination
;;; the unit is named for -- #166's head-shim replay plus RENUMBER-SCHEMA over
;;; a schema cl-store'd by a pre-widening build.  Reuses the v2 fixture and
;;; its readers from tests/type-id-width-tests.lisp, whose own migration test
;;; pins the opposite mode.
;;; ---------------------------------------------------------------------------

(test renumbering-migration-carries-a-v2-source-to-registry-ids
  "A v2 (31-byte head, 2-byte type-id) source migrated with :RENUMBER-P T
keeps every node, revision, slot value and edge endpoint -- and takes its
type-ids from the registry, which is the exact reverse of what
MIGRATE-V2-GRAPH-TO-V3-WITHOUT-RENUMBERING pins.  The registry is primed
with one filler name per parent first, so NO source id can survive by
coincidence and 'the id moved' is a real observation rather than a lucky
draw."
  #+ecl
  (skip "v2 fixture was cl-store'd by SBCL; ECL's cl-store cannot restore it ~
(graph on-disk dirs are not portable across Lisp implementations).")
  #-ecl
  (with-temp-directory (root)
    (let ((old-dir (extract-v2-fixture (merge-pathnames "v2/" root)))
          (new-dir (namestring (merge-pathnames "v3-renumbered/" root))))
      (multiple-value-bind (registry sysdir) (%fresh-registry root)
        (let ((graph-db::*system-directory* sysdir)
              (graph-db::*type-registry* nil))
          (graph-db::registry-intern registry 'ts-filler-vertex :vertex)
          (graph-db::registry-intern registry 'ts-filler-edge :edge)
          (let ((source-ids (%store-entries old-dir)))
            (multiple-value-bind (expected-v expected-k expected-l)
                (%read-v2-graph old-dir)
              (is (= 12 (length expected-v))
                  "fixture sanity: 12 people expected before migration")
              (let ((g (graph-db::migrate-graph
                        :ti-migration-fixture old-dir new-dir
                        :package :graph-db/test :renumber-p t
                        :snapshot-file
                        (namestring
                         (merge-pathnames "migrate.snapshot" root)))))
                (unwind-protect
                     ;; Re-read the registry from disk: MIGRATE-GRAPH interns
                     ;; through ENSURE-TYPE-REGISTRY's own object, so the one
                     ;; opened above has no idea what it just assigned.
                     (let ((graph-db::*graph* g)
                           (registry (graph-db::open-type-registry sysdir)))
                       (is (equalp expected-v (%fixture-vertices g))
                           "vertices survive a renumbering intact")
                       (is (equalp expected-k
                                   (%fixture-edges g 'ti-mig-knows))
                           "knows edges survive a renumbering intact")
                       (is (equalp expected-l
                                   (%fixture-edges g 'ti-mig-likes))
                           "likes edges survive a renumbering intact")
                       (dolist (entry source-ids)
                         (destructuring-bind (symbol parent id) entry
                           (is (eql (graph-db::registry-id-for
                                     registry symbol parent)
                                    (%type-id-in g symbol parent))
                               "~A must take the registry's id, got ~A"
                               symbol (%type-id-in g symbol parent))
                           (is (not (eql id (%type-id-in g symbol parent)))
                               "~A's legacy id ~A must NOT survive a ~
:RENUMBER-P T migration" symbol id))))
                  (close-graph g))
                (collect-garbage)))))))))
