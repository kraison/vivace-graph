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
keyword alias, which this must not reproduce."
  (with-temp-directory (dir)
    (flet ((pkg (n) (or (find-package n) (make-package n :use '()))))
      (let* ((s1 (intern "SPECIES" (pkg "REG-TEST-1")))
             (s2 (intern "SPECIES" (pkg "REG-TEST-2")))
             (r (graph-db::open-type-registry (namestring dir))))
        (unwind-protect
             (is (/= (graph-db::registry-intern r s1 :vertex)
                     (graph-db::registry-intern r s2 :vertex))
                 "same name, different packages, different ids")
          (graph-db::close-type-registry r))))))

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
  "Two images assigning concurrently must not hand the same id to different
symbols.  flock attaches to the open file description, so two registries on one
directory in this image contend exactly as two processes would."
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
