;;;; The image-level type-id registry (GH #186).
(in-package #:graph-db/test)

(def-suite type-registry-suite :in graph-db-suite
  :description "The persisted, image-level type-id registry.")
(in-suite type-registry-suite)

(test registry-assigns-distinct-ids-to-distinct-symbols
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (let ((a (graph-db::registry-intern r 'reg-alpha :vertex))
                 (b (graph-db::registry-intern r 'reg-beta :vertex)))
             (is (integerp a))
             (is (/= a b) "two symbols never share an id"))
        (graph-db::close-type-registry r)))))

(test registry-is-idempotent-for-one-symbol
  "The whole point of a registry: asking twice gives the same answer, and
asking in another image after a reopen still does."
  (with-temp-directory (dir)
    (let* ((r (graph-db::open-type-registry (namestring dir)))
           (first (graph-db::registry-intern r 'reg-gamma :vertex)))
      (is (= first (graph-db::registry-intern r 'reg-gamma :vertex)))
      (graph-db::close-type-registry r)
      (let ((r2 (graph-db::open-type-registry (namestring dir))))
        (unwind-protect
             (is (= first (graph-db::registry-intern r2 'reg-gamma :vertex))
                 "the assignment is durable, not in-memory")
          (graph-db::close-type-registry r2))))))

(test registry-separates-vertex-and-edge-spaces
  "Vertices and edges are distinct spaces, as they are per-graph today; the
same symbol may hold a different id in each."
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (let ((v (graph-db::registry-intern r 'reg-delta :vertex))
                 (e (graph-db::registry-intern r 'reg-delta :edge)))
             (is (integerp v)) (is (integerp e))
             (is (= v (graph-db::registry-id-for r 'reg-delta :vertex)))
             (is (= e (graph-db::registry-id-for r 'reg-delta :edge))))
        (graph-db::close-type-registry r)))))

(test registry-distinguishes-same-name-in-two-packages
  "The registry is keyed on the PACKAGE-QUALIFIED symbol.  Keying on the name
would collide two packages' types -- the defect #190 records in the per-graph
keyword alias, which this must not reproduce.

Each symbol is interned with its OWN home package bound as the ambient
*PACKAGE*, matching a real caller registering its own type, and each goes
through a fresh OPEN-TYPE-REGISTRY so the second interning re-parses the
first's persisted record rather than reusing an in-memory hit.  Printing
that omits the package prefix when the symbol happens to be accessible in
the current *PACKAGE* -- the ordinary printer behaviour -- would read the
first record back as the SECOND symbol here, since both are named
\"REG-SPECIES\"; this only passes if the record survives the change in
ambient package intact."
  (with-temp-directory (dir)
    (flet ((pkg (n) (or (find-package n) (make-package n :use '()))))
      (let* ((p1 (pkg "REG-TEST-1"))
             (p2 (pkg "REG-TEST-2"))
             (s1 (intern "REG-SPECIES" p1))
             (s2 (intern "REG-SPECIES" p2))
             (id1 (let ((*package* p1))
                    (let ((r (graph-db::open-type-registry (namestring dir))))
                      (unwind-protect
                           (graph-db::registry-intern r s1 :vertex)
                        (graph-db::close-type-registry r)))))
             (id2 (let ((*package* p2))
                    (let ((r (graph-db::open-type-registry (namestring dir))))
                      (unwind-protect
                           (graph-db::registry-intern r s2 :vertex)
                        (graph-db::close-type-registry r))))))
        (is (/= id1 id2) "same name, different packages, different ids")))))

(test registry-symbol-identity-survives-an-ambient-package-change
  "GH #186's whole point: two images (or two call sites) with different
*PACKAGE* bindings must agree on what a type-id means.  Writing a record
while the symbol's own home package is ambient, then reading it back with
an unrelated package ambient, must yield the SAME symbol object -- not a
different one the reader happened to intern into whatever package was
current at read time.  Reproduces the collision the reviewer found in
round 1: printing without an explicit *PACKAGE* binding depends on the
CALLER's ambient package, so this test fails against the unfixed code and
REGISTRY-DISTINGUISHES-SAME-NAME-IN-TWO-PACKAGES alone does not catch it,
since that test's packages are never accessible from the ambient package
either way."
  (with-temp-directory (dir)
    (let* ((home (or (find-package "REG-TEST-HOME")
                      (make-package "REG-TEST-HOME" :use '())))
           (sym (intern "REG-ALPHA" home)))
      (let ((*package* home))
        (let ((r (graph-db::open-type-registry (namestring dir))))
          (unwind-protect
               (graph-db::registry-intern r sym :vertex)
            (graph-db::close-type-registry r))))
      (let ((*package* (find-package "COMMON-LISP-USER")))
        (let ((r2 (graph-db::open-type-registry (namestring dir))))
          (unwind-protect
               (let ((got (first (first (graph-db::registry-entries r2)))))
                 (is (eq sym got)
                     "the re-read symbol must be EQ to the one written, not
a same-named symbol interned under the ambient package at read time"))
            (graph-db::close-type-registry r2)))))))

(test registry-tolerates-a-torn-final-record
  "GH #191: the lifecycle journal signals on a truncated tail and loses the
whole file.  The registry is the ONLY record of what a type-id means -- losing
it is worse -- so a torn FINAL record is dropped with a log, while a malformed
record earlier still signals."
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (graph-db::registry-intern r 'reg-keep-1 :vertex)
      (graph-db::registry-intern r 'reg-keep-2 :vertex)
      (graph-db::close-type-registry r))
    (let ((f (merge-pathnames "type-registry.log" dir)))
      (with-open-file (s f :direction :output :if-exists :append)
        (format s "(:SYMBOL REG-TORN :PARENT :VERT"))   ; truncated, no newline
      (let ((r2 (graph-db::open-type-registry (namestring dir))))
        (unwind-protect
             (is (= 2 (length (graph-db::registry-entries r2)))
                 "the two intact records survive a torn tail")
          (graph-db::close-type-registry r2))))))

(test registry-assignment-is-serialised-across-open-file-descriptions
  "Two registries on one directory in this image, used SEQUENTIALLY (both
calls run in one thread -- this is not a concurrency test; POSIX-TESTS
covers flock's cross-open-file-description semantics directly).  What this
proves: the second REGISTRY-INTERN re-reads the persisted file under its
own lock and sees the first's write, rather than assigning from a stale
idea of \"next id\" cached at open time."
  (with-temp-directory (dir)
    (let ((r1 (graph-db::open-type-registry (namestring dir)))
          (r2 (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (let ((a (graph-db::registry-intern r1 'reg-race-a :vertex))
                 (b (graph-db::registry-intern r2 'reg-race-b :vertex)))
             (is (/= a b)
                 "the second assigner re-read the tail under the lock and did
not reuse the first's id"))
        (graph-db::close-type-registry r1)
        (graph-db::close-type-registry r2)))))
