(in-package :graph-db)

;;; Adopting global type-ids on a system that already has stores (GH #186).
;;; The policy here is derived from the measurement in spec §10.1
;;; (docs/superpowers/specs/2026-08-20-namespaces-design.md), not invented:
;;; every store's schema counted from 1, so the low ids are contested
;;; system-wide and all but one store must be renumbered whichever one is
;;; favoured.  The cost of favouring wrongly is measured in BYTES REPLAYED,
;;; so the ranking is by store size and never by type count.
;;;
;;; This file loads after SCHEMA and ALLOCATOR because it reads a store's
;;; schema.dat and heap header directly; TYPE-REGISTRY cannot hold it, since
;;; SCHEMA depends on TYPE-REGISTRY for ASSIGN-TYPE-ID.

(defstruct (seeding-report (:conc-name seeding-report-))
  "What REGISTRY-SEED-FROM-STORES did, and what the operator must do next."
  ;; The store whose ids were offered first -- the largest on disk.
  (seed nil)
  ;; ((location . bytes) ...), largest first.
  (sizes nil)
  ;; Locations that must now be MIGRATE-GRAPHed with :RENUMBER-P T.
  (renumber nil)
  ;; (location symbol parent id ...) for a symbol one store's history left
  ;; holding more than one id.
  (duplicates nil)
  ;; (location symbol parent old-id new-id) for every id that moved.
  (changes nil))

(defun %store-heap-bytes (location)
  "Bytes the store at LOCATION has handed out of its heap: the allocation
high-water mark from heap.dat's header, read without opening the graph.

Deliberately not the file's length.  heap.dat is created at
*DEFAULT-HEAP-SIZE* and is sparse, so its length is the same number whether
the store holds ten nodes or ten million -- useless for the one thing §10.1
needs a size for, which is deciding which store is too expensive to rewrite."
  (let ((mf (mmap-file (format nil "~A/heap.dat" (pathname location))
                       :create-p nil)))
    (unwind-protect
         (max 0 (- (deserialize-uint64 mf +memory-memory-pointer-offset+)
                   +memory-usable-offset+))
      (munmap-file mf))))

(defun %store-schema (location)
  "The persisted SCHEMA of the store at LOCATION, restored without opening
the graph.  Every type name's package must exist in this image -- the same
requirement reading the registry itself carries (GH #195)."
  (let ((file (format nil "~A/schema.dat" (pathname location))))
    (unless (probe-file file)
      (error "No schema.dat at ~A: not a graph location." location))
    (cl-store:restore file)))

(defun %store-type-entries (schema)
  "(values ENTRIES DUPLICATES) for SCHEMA, read from its type-table.

ENTRIES is (SYMBOL PARENT ID) ascending by id, where ID is the one the
store's own name lookup resolves to today.  DUPLICATES is (SYMBOL PARENT
ID ...) for a symbol the store's history left holding more than one id --
§10.1's case that no seeding policy exempts, since those ids must unify
whichever store wins."
  (let ((entries nil) (duplicates nil))
    (dolist (parent '(:vertex :edge))
      (let ((sub (gethash parent (schema-type-table schema)))
            (by-name (make-hash-table :test 'eq)))
        (when sub
          ;; Integer keys map to the metadata; the symbol key is an alias
          ;; onto the current id.  A keyword key can only be pre-#190
          ;; residue -- UPDATE-NODE-TYPE no longer writes one (GH #190).
          (maphash (lambda (key meta)
                     (when (and (integerp key) (node-type-p meta))
                       (push key (gethash (node-type-name meta) by-name))))
                   sub)
          (maphash (lambda (name ids)
                     (let* ((sorted (sort (copy-list ids) #'<))
                            (named (gethash name sub))
                            (current (if (integerp named)
                                         named
                                         (car (last sorted)))))
                       (push (list name parent current) entries)
                       (when (rest sorted)
                         (push (list* name parent sorted) duplicates))))
                   by-name))))
    (values (sort entries #'< :key #'third) (nreverse duplicates))))

(defun %registry-claimed-ids (registry)
  "(values VERTEX EDGE): ID -> SYMBOL tables built from REGISTRY's entries.
Adoption needs the reverse of REGISTRY-ID-FOR -- an id that already means
another symbol cannot be adopted, however large the store asking for it."
  (let ((vertex (make-hash-table))
        (edge (make-hash-table)))
    (dolist (entry (registry-entries registry) (values vertex edge))
      (destructuring-bind (symbol parent id) entry
        (setf (gethash id (ecase parent (:vertex vertex) (:edge edge)))
              symbol)))))

(defun %seeding-location (location)
  "LOCATION as a namestring that compares with EQUAL.  Signals if it does
not exist, which is the right moment to find that out."
  (namestring (truename (merge-pathnames "" location))))

(defun registry-seed-from-stores (registry locations)
  "Seed REGISTRY from the stores at LOCATIONS and report which of them must
now be renumbered.  Opens no graph: each store is read from its schema.dat
and its heap header.  Returns a SEEDING-REPORT.

Stores are offered their ids LARGEST FIRST, and each keeps every id it can:
a symbol REGISTRY has not seen, at an id no other symbol has claimed, is
adopted verbatim.  So the store that costs most to rewrite wins every
contest.  That is the whole of spec §10.1's policy -- all but one store
renumbers whichever is favoured, so favour the one measured in bytes, not
the one with the most types (on the measured system the type-richest store
held 59 of 95 types and was among the smallest; seeding from it would have
replayed ~4.9 GB instead of ~1.1 GB).

A store lands in the report's RENUMBER list when any of its symbols ends up
at an id other than the one on its disk, or when its own history holds two
ids for one symbol.  Migrate each of those with
  (MIGRATE-GRAPH name location new-location :RENUMBER-P T)
and leave the rest alone.

REGISTRY need not be empty.  Entries already in it win over every store,
including the largest -- it is the authority, and it may already have been
distributed to peers."
  (let* ((sizes (sort (mapcar (lambda (location)
                                (let ((l (%seeding-location location)))
                                  (cons l (%store-heap-bytes l))))
                              locations)
                      ;; Ties broken on the location, because SORT is not
                      ;; required to be stable and equal high-water marks are
                      ;; ordinary (fresh or empty stores).  Two images ranking
                      ;; one store set differently would seed two different
                      ;; registries -- the hole %REGISTRY-MINT-ORDER closes a
                      ;; level up (GH #186).
                      (lambda (a b)
                        (if (= (cdr a) (cdr b))
                            (and (string< (car a) (car b)) t)
                            (> (cdr a) (cdr b))))))
         (report (make-seeding-report :sizes sizes
                                      :seed (car (first sizes)))))
    (with-registry-append-lock (registry)
      (multiple-value-bind (vertex-claims edge-claims)
          (%registry-claimed-ids registry)
        (loop for (location . nil) in sizes do
          (multiple-value-bind (entries duplicates)
              (%store-type-entries (%store-schema location))
            (dolist (duplicate duplicates)
              (push (cons location duplicate)
                    (seeding-report-duplicates report))
              (pushnew location (seeding-report-renumber report)
                       :test #'equal))
            (dolist (entry entries)
              (destructuring-bind (symbol parent id) entry
                (let* ((claims (ecase parent
                                 (:vertex vertex-claims)
                                 (:edge edge-claims)))
                       (known (registry-id-for registry symbol parent))
                       (new (cond
                              (known known)
                              ;; The id means another symbol already: this
                              ;; store cannot keep it and must renumber.
                              ((gethash id claims)
                               (%registry-assign registry symbol parent))
                              (t (%registry-adopt registry symbol parent
                                                  id)))))
                  (setf (gethash new claims) symbol)
                  (unless (eql new id)
                    (push (list location symbol parent id new)
                          (seeding-report-changes report))
                    (pushnew location (seeding-report-renumber report)
                             :test #'equal)))))))))
    (setf (seeding-report-changes report)
          (nreverse (seeding-report-changes report)))
    (setf (seeding-report-duplicates report)
          (nreverse (seeding-report-duplicates report)))
    (setf (seeding-report-renumber report)
          (remove-if-not (lambda (l)
                           (member l (seeding-report-renumber report)
                                   :test #'equal))
                         (mapcar #'car sizes)))
    report))

(defun %symbol-home-package-name (symbol)
  "SYMBOL's home package name, or \"\" when it is uninterned.  Uninterned
symbols cannot round-trip through the registry file anyway (GH #186 task 1),
so they only have to sort somewhere stable."
  (let ((package (symbol-package symbol)))
    (if package (package-name package) "")))

(defun %registry-mint-order (table)
  "TABLE's symbol keys ordered by package name, then symbol name.

MAPHASH order is unspecified and a fresh registry mints ids in the order it
is asked, so without this two images renumbering ONE store into two EMPTY
registries assign different ids -- which the replication handshake then
refuses (D15).  A sort is nearly free and a migration that is not
reproducible cannot be verified by re-running it.

This buys reproducibility of a SINGLE migration.  It is NOT a substitute for
distributing the registry (D14): two images that opened different stores, or
the same stores in a different order, still disagree, and that disagreement
is the operator event D15 exists to name.  GH #186."
  (sort (loop for name being the hash-keys in table collect name)
        (lambda (a b)
          (let ((pa (%symbol-home-package-name a))
                (pb (%symbol-home-package-name b)))
            (if (string= pa pb)
                (and (string< (symbol-name a) (symbol-name b)) t)
                (and (string< pa pb) t))))))

(defun renumber-schema (schema registry)
  "Replace every type-id in SCHEMA with the one REGISTRY holds for that
type's NAME, minting where it holds none.  Returns (values SCHEMA UNIFIED).

UNIFIED is (SYMBOL PARENT (OLD-ID ...) NEW-ID) for each symbol the store's
history left at more than one id.  Those collapse to one id here, which is
why §10.1 says such a store is always in the migration set; the surviving
metadata is the one the store's own name lookup resolved to.

SCHEMA is mutated in place.  MIGRATE-GRAPH passes the schema it restored
from the source store, which is closed by then and does not own it."
  (let ((unified nil))
    (dolist (parent '(:vertex :edge))
      (let ((sub (gethash parent (schema-type-table schema))))
        (when sub
          (let ((survivors (make-hash-table :test 'eq))
                (old-ids (make-hash-table :test 'eq)))
            (maphash
             (lambda (key meta)
               (when (and (integerp key) (node-type-p meta))
                 (let ((name (node-type-name meta)))
                   (push key (gethash name old-ids))
                   (when (or (null (gethash name survivors))
                             (eql key (gethash name sub)))
                     (setf (gethash name survivors) meta)))))
             sub)
            (clrhash sub)
            ;; %REGISTRY-MINT-ORDER, not MAPHASH: minting order decides the
            ;; ids a fresh registry hands out, and it must not vary between
            ;; images.  See that function (GH #186).
            (dolist (name (%registry-mint-order survivors))
              (let ((meta (gethash name survivors))
                    (new (registry-intern registry name parent))
                    (ids (sort (gethash name old-ids) #'<)))
                (when (rest ids)
                  (push (list name parent ids new) unified))
                (setf (node-type-id meta) new)
                (setf (gethash new sub) meta)
                (setf (gethash name sub) new)))))))
    (values schema (nreverse unified))))
