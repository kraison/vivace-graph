;;;; Shared sidecar print/read discipline (GH #226, #227).  Exercises
;;;; WITH-SIDECAR-OUTPUT/-INPUT indirectly through the real writer/reader
;;;; entry points (CREATE-VERTEX-TYPE, ENSURE-NAMESPACE, %NOTE-EDGE-
;;;; OCCUPANCY, READ-SCHEMA-MANIFEST, MATERIALIZE-SCHEMA, the type/store
;;;; registries) rather than the macros directly -- those entry points
;;;; are exactly what #226 says are reachable from arbitrary caller code
;;;; with a hostile dynamic environment.  Reuses RUNTIME-SCHEMA-TESTS'
;;;; WITH-RS-STORE fixture and :RS-STORE type-registry setup; loaded
;;;; after that file.
(in-package #:graph-db/test)

(def-suite sidecar-io-suite :in graph-db-suite
  :description "Shared sidecar print/read discipline (GH #226, #227).")
(in-suite sidecar-io-suite)

(defun %sidecar-hostile-readtable ()
  "A readtable that would misparse a sidecar record if a reader ever
used it instead of WITH-SIDECAR-INPUT's own pristine copy: downcasing
input flips every interned symbol's name-case, so e.g. WIDGET on disk
would read back as the distinct symbol |widget| (GH #226)."
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :downcase)
    rt))

(defmacro with-hostile-read-env (&body body)
  "Every ambient binding a sidecar READER must survive: a *READ-BASE*
that would misparse a decimal :ID/:TIME integer as hex, and a
*READTABLE* that would fold case differently.  Reverting WITH-SIDECAR-
INPUT must fail any test that wraps its assertions in this."
  `(let ((*read-base* 16)
         (*readtable* (%sidecar-hostile-readtable)))
     ,@body))

(test sidecar-io-hostile-dynamic-environment
  "GH #226: CREATE-VERTEX-TYPE/ENSURE-NAMESPACE/%NOTE-EDGE-OCCUPANCY must
write a record that round-trips even when the caller has *PRINT-LENGTH*,
*PRINT-LEVEL*, *PRINT-BASE* and *READ-BASE* rebound.  Without the fix: a
5-slot list truncates to 2 under *PRINT-LENGTH* 2, the nested slot specs
print as # under *PRINT-LEVEL* 1, and the :TIME integer prints in hex
under *PRINT-BASE* 16 -- unreadable back at *READ-BASE* 10.  The READ
side is separately hostile (WITH-HOSTILE-READ-ENV) around every actual
read, including a forced disk reload of the edge-occupancy hint (GH
#226 review round 2, item 9) so an in-image cache hit can't pass this
without ever exercising WITH-SIDECAR-INPUT."
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
    (with-hostile-read-env
      (multiple-value-bind (ns types skipped)
          (graph-db::read-schema-manifest graph-db::*system-directory*)
        (is (= 0 skipped))
        (is-true (find "RS-HOSTILE" ns :key (lambda (r) (getf r :namespace))
                       :test #'string=))
        (let ((row (find (intern "WIDGET" :rs-hostile) types
                         :key (lambda (r) (getf r :type)))))
          (is-true row)
          (is (= 5 (length (getf row :slots))))))
      ;; Force a real disk read: the cache from %NOTE-EDGE-OCCUPANCY's
      ;; own write above would otherwise answer this without ever
      ;; calling READ-SIDECAR-FORMS.
      (graph-db::%clear-edge-occupancy-cache)
      (is (member "rs-store" (graph-db:edge-type-stores 'rs-knows)
                 :test 'equal)))))

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

(test sidecar-io-bad-record-before-good-record-survives
  "GH #226 review round 2: a bad record BEFORE a good one must not
consume the good one too.  An unterminated string's failed READ leaves
the stream at EOF (it scans past every following line looking for the
closing quote), so without repositioning back to where that READ
started, the resync-by-line recovery has nothing left to resync onto --
every good record after the bad one is lost, which is worse than the
old READ-LINE-based parser this replaced."
  (with-temp-directory (dir)
    (let ((file (merge-pathnames "raw-sidecar.dat" dir)))
      (with-open-file (s file :direction :output :if-exists :supersede)
        (write-line "(:BAD \"unterminated" s)
        (write-line "(:GOOD 1)" s))
      (multiple-value-bind (forms skipped first-skip)
          (graph-db::read-sidecar-file-forms file :warn-p nil)
        (is (= 1 skipped))
        (is (eql 0 first-skip))
        (is (equal '((:good 1)) forms))))))

(test type-registry-hostile-read-base
  "GH #226 review round 2, item 10: reopening the type registry
(%REGISTRY-LOAD -> %PARSE-REGISTRY-RECORD) must not misparse an :ID
under an ambient *READ-BASE* -- \"12\" read at base 16 is 18, not 12.
WITH-SIDECAR-INPUT pins *READ-BASE* 10 regardless of the caller's own
binding at reopen time."
  (with-temp-directory (dir)
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (dotimes (i 11)
        (graph-db::registry-intern
         r (intern (format nil "TR-HOSTILE-FILLER-~D" i) :graph-db/test)
         :vertex))
      (let ((target-id (graph-db::registry-intern r 'tr-hostile-target
                                                  :vertex)))
        (is (eql 12 target-id))
        (graph-db::close-type-registry r)
        (with-hostile-read-env
          (let ((r2 (graph-db::open-type-registry (namestring dir))))
            (unwind-protect
                 (is (eql target-id
                          (graph-db::registry-id-for r2 'tr-hostile-target
                                                     :vertex)))
              (graph-db::close-type-registry r2))))))))

(test store-registry-hostile-read-base
  "GH #226 review round 2, item 10: the same misparse hazard, for the
store-id registry's own reopen path (%STORE-REGISTRY-LOAD ->
%PARSE-STORE-REGISTRY-RECORD)."
  (with-temp-directory (sys)
    (let ((graph-db::*system-directory* (namestring sys))
          (graph-db::*store-registry* nil))
      (dotimes (i 11)
        (store-registry-intern
         (intern (format nil "SR-HOSTILE-FILLER-~D" i) :graph-db/test)))
      (let ((target-id (store-registry-intern 'sr-hostile-target)))
        (is (eql 12 target-id))
        (setq graph-db::*store-registry* nil)
        (with-hostile-read-env
          (is (eql target-id (store-registry-id-for 'sr-hostile-target))))))))

;;; GH #234: shadow-store's policy.dat / lease.dat and system-restore's
;;; restore manifest now go through WITH-SIDECAR-OUTPUT/-INPUT instead
;;; of partial *PRINT-READABLY*/*READ-EVAL* bindings.

(defmacro with-hostile-print-env (&body body)
  "Every ambient binding a sidecar WRITER must survive: hex *PRINT-BASE*
(with *PRINT-RADIX* noise), truncating *PRINT-LENGTH*/*PRINT-LEVEL*,
and -- the #234-specific hazard for the previously package-blind
shadow-store writers -- *PACKAGE* bound to KEYWORD, under which an
unfixed PRIN1 drops every keyword's leading colon."
  `(let ((*print-base* 16) (*print-radix* t)
         (*print-length* 1) (*print-level* 1)
         (*print-case* :downcase)
         (*package* (find-package :keyword)))
     ,@body))

(test shadow-policy-survives-a-hostile-dynamic-environment
  "GH #234: SET-STORE-RECOVERY-POLICY under hostile printer bindings
still writes a line STORE-RECOVERY-POLICY (itself under hostile reader
bindings) reads back as the same keyword -- pre-fix, *PACKAGE* KEYWORD
made the policy print colon-less and fail the strict member check."
  (with-temp-directory (dir)
    (with-hostile-print-env
      (graph-db::set-store-recovery-policy dir :derivable))
    (with-hostile-read-env
      (is (eq :derivable (graph-db::store-recovery-policy dir))))))

(test shadow-lease-survives-a-hostile-dynamic-environment
  "GH #234: %PERSIST-LEASE/%READ-LEASE round-trip decimal integers and
keywords under hostile bindings on both sides -- pre-fix, *PRINT-BASE*
16 wrote the lease bounds in hex and *PRINT-LENGTH* 1 truncated the
plist to (:LEASE-START ...)."
  (with-temp-directory (dir)
    (with-hostile-print-env
      (graph-db::%persist-lease dir 100 2000000))
    (with-hostile-read-env
      (let ((lease (graph-db::%read-lease dir)))
        (is (eql 100 (getf lease :lease-start)))
        (is (eql 2000000 (getf lease :lease-end)))))))

(test restore-manifest-survives-a-hostile-dynamic-environment
  "GH #234: %WRITE-MANIFEST / READ-RESTORE-MANIFEST under hostile
bindings on both sides; the decimal :STATE-AT and the keyword actions
must survive exactly."
  (with-temp-directory (dir)
    (let ((clock (graph-db::%make-system-clock
                  :location (namestring dir)))
          (manifest (list :restore t :requested 5 :at 100
                          :clock (namestring dir)
                          :stores (list (list :store :m234-store
                                              :action :rewound
                                              :state-at 100
                                              :exact t)))))
      (with-hostile-print-env
        (graph-db::%write-manifest clock manifest))
      (with-hostile-read-env
        (let* ((back (graph-db::read-restore-manifest
                      (graph-db::%manifest-file clock 100)))
               (entry (first (getf back :stores))))
          (is (eql 5 (getf back :requested)))
          (is (eql 100 (getf back :at)))
          (is (eq :m234-store (getf entry :store)))
          (is (eq :rewound (getf entry :action)))
          (is (eql 100 (getf entry :state-at)))
          (is (eq t (getf entry :exact))))))))
