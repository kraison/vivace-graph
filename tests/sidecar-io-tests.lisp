;;;; Shared sidecar print/read discipline (GH #226, #227).  Exercises
;;;; WITH-SIDECAR-OUTPUT/-INPUT indirectly through the real writer/reader
;;;; entry points (CREATE-VERTEX-TYPE, ENSURE-NAMESPACE, %NOTE-EDGE-
;;;; OCCUPANCY, READ-SCHEMA-MANIFEST, MATERIALIZE-SCHEMA) rather than the
;;;; macros directly -- those entry points are exactly what #226 says are
;;;; reachable from arbitrary caller code with a hostile dynamic
;;;; environment.  Reuses RUNTIME-SCHEMA-TESTS' WITH-RS-STORE fixture and
;;;; :RS-STORE type-registry setup; loaded after that file.
(in-package #:graph-db/test)

(def-suite sidecar-io-suite :in graph-db-suite
  :description "Shared sidecar print/read discipline (GH #226, #227).")
(in-suite sidecar-io-suite)

(test sidecar-io-hostile-dynamic-environment
  "GH #226: CREATE-VERTEX-TYPE/ENSURE-NAMESPACE/%NOTE-EDGE-OCCUPANCY must
write a record that round-trips even when the caller has *PRINT-LENGTH*,
*PRINT-LEVEL*, *PRINT-BASE* and *READ-BASE* rebound.  Without the fix: a
5-slot list truncates to 2 under *PRINT-LENGTH* 2, the nested slot specs
print as # under *PRINT-LEVEL* 1, and the :TIME integer prints in hex
under *PRINT-BASE* 16 -- unreadable back at *READ-BASE* 10."
  (with-rs-store (g)
    g
    (let ((*print-length* 2) (*print-level* 1) (*print-base* 16)
          (*read-base* 16))
      (graph-db:ensure-namespace "RS-HOSTILE")
      (graph-db:create-vertex-type
       "RS-HOSTILE:WIDGET"
       '((a :type string) (b :type string) (c :type string)
         (d :type string) (e :type string))
       :default-store :rs-store)
      (graph-db::%note-edge-occupancy 'rs-knows "rs-store"))
    (multiple-value-bind (ns types skipped)
        (graph-db::read-schema-manifest graph-db::*system-directory*)
      (is (= 0 skipped))
      (is-true (find "RS-HOSTILE" ns :key (lambda (r) (getf r :namespace))
                     :test #'string=))
      (let ((row (find (intern "WIDGET" :rs-hostile) types
                       :key (lambda (r) (getf r :type)))))
        (is-true row)
        (is (= 5 (length (getf row :slots))))))
    (is (member "rs-store" (graph-db:edge-type-stores 'rs-knows)
               :test 'equal))))

(test sidecar-io-multiline-string-round-trips
  "GH #226: a runtime slot-spec option carrying an embedded newline (a
legal CLOS slot option value -- e.g. :DOCUMENTATION) must not split its
manifest record.  READ-SCHEMA-MANIFEST reads FORMS via READ, which spans
the embedded newline naturally, instead of READ-LINE splitting on it."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-MULTILINE")
    (let ((doc (format nil "line one~%line two")))
      (graph-db:create-vertex-type
       "RS-MULTILINE:NOTED"
       (list (list 'x :type 'string :documentation doc))
       :default-store :rs-store)
      (multiple-value-bind (ns types skipped)
          (graph-db::read-schema-manifest graph-db::*system-directory*)
        ns
        (is (= 0 skipped))
        (let ((row (find (intern "NOTED" :rs-multiline) types
                         :key (lambda (r) (getf r :type)))))
          (is-true row)
          (is (string= doc
                       (getf (cdr (first (getf row :slots)))
                             :documentation))))))))

(test sidecar-io-227-absent-package-warns-and-counts
  "GH #227: a manifest type row naming a symbol in a package this image
does not have must be REPORTED, not silently dropped -- READ-SCHEMA-
MANIFEST warns SIDECAR-RECORDS-SKIPPED with the count, and MATERIALIZE-
SCHEMA's summary carries :SKIPPED-UNREADABLE.  A row's OWN package (the
:TYPE symbol's home) is self-healed by %MATERIALIZE-ORPHAN-PACKAGES, so
this hand-appended row is filtered out of that namespace by :NAMESPACES
below -- it stays genuinely unreadable, matching #227's \"interior
symbol in an unrelated missing package\" scenario."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-227")
    (graph-db:create-vertex-type
     "RS-227:GOODROW" '((v :type string)) :default-store :rs-store)
    (with-open-file (s (graph-db::%schema-manifest-file)
                       :direction :output :if-exists :append)
      (write-string "(:TYPE RS-227-MISSING-PKG::BADROW :KIND :VERTEX " s)
      (write-string ":PARENTS NIL :SLOTS NIL :DEFAULT-STORE :RS-STORE " s)
      (write-string ":KEEP-REVISIONS NIL :PROVENANCE :RUNTIME :TIME 0)" s)
      (terpri s))
    (let ((warned-count nil))
      (handler-bind ((graph-db::sidecar-records-skipped
                       (lambda (c)
                         (setq warned-count
                               (graph-db::sidecar-skipped-count c))
                         (muffle-warning c))))
        (multiple-value-bind (ns types skipped)
            (graph-db::read-schema-manifest graph-db::*system-directory*)
          ns
          (is (= 1 skipped))
          (is-true (find (intern "GOODROW" :rs-227) types
                         :key (lambda (r) (getf r :type))))))
      (is (eql 1 warned-count)))
    (let ((summary (graph-db:materialize-schema
                    graph-db::*system-directory* :namespaces "RS-227")))
      (is (eql 1 (getf summary :skipped-unreadable))))))

(test sidecar-io-torn-tail-tolerated
  "GH #226/#227: a torn tail (write interrupted mid-record, no closing
paren) is dropped and warned, never signalled as an error; every prior
record survives."
  (with-rs-store (g)
    g
    (graph-db:ensure-namespace "RS-SIDECAR-TORN")
    (with-open-file (s (graph-db::%schema-manifest-file)
                       :direction :output :if-exists :append)
      (write-string "(:NAMESPACE \"RS-TORN-NEVER-LANDS\" :NICKNAMES" s))
    (let ((warned-count nil))
      (handler-bind ((graph-db::sidecar-records-skipped
                       (lambda (c)
                         (setq warned-count
                               (graph-db::sidecar-skipped-count c))
                         (muffle-warning c))))
        (multiple-value-bind (ns types skipped)
            (graph-db::read-schema-manifest graph-db::*system-directory*)
          types
          (is (= 1 skipped))
          (is-true (find "RS-SIDECAR-TORN" ns
                         :key (lambda (r) (getf r :namespace))
                         :test #'string=))
          (is (null (find "RS-TORN-NEVER-LANDS" ns
                          :key (lambda (r) (getf r :namespace))
                          :test #'string=)))))
      (is (eql 1 warned-count)))))
