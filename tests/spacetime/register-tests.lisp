;;;; REGISTER-GEOMETRY: binding a geometry to a registry's regions.
;;;; Design: docs/superpowers/specs/2026-08-19-registration-design.md
;;;; §4-§6 (GH #138).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *region-graph-name* :graph-db-register-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *region-graph-name* graph-db::*schema-node-metadata*) nil))

;; A geometry slot marked :INDEX T is what makes a type spatially indexed,
;; and so scopeable by class name (spatial-query.lisp's SCOPE argument).
(def-vertex ct-region ()
  ((name :type string)
   (geom :type geometry :index t))
  :graph-db-register-test)

(defmacro with-region-graph ((g) &body body)
  "A fresh on-disk graph named *REGION-GRAPH-NAME* in a temp dir.
Modelled on WITH-CLAIM-GRAPH (claim-tests.lisp).  :SPATIAL-PRECISION 5
because these regions are whole degrees across: at the default 7 their
cover hits the cell cap, which coarsens the index and warns (the cover
cap in spatial-index.lisp)."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *region-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000
                             :spatial-precision 5)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %make-region (graph name ring)
  "A CT-REGION called NAME covering RING, a closed list of (lon lat)."
  (let ((graph-db:*graph* graph))
    (with-transaction ()
      (make-ct-region :name name :geom (make-polygon (list ring))))))

(defun %fraction-of (registration)
  (getf registration :fraction))

(defun %near (expected actual)
  "Fractions are compared to 1d-6, never by equality (plan constraint)."
  (< (abs (- expected actual)) 1d-6))

(test a-point-registers-to-one-region-at-fraction-one
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-point 1d0 1d0) 'ct-region
                           :registry-graph g)
      (is-true evaluated)
      (is (= 1 (length regs)))
      (is (%near 1d0 (%fraction-of (first regs)))))))

(test a-polygon-registers-fractionally-to-every-region-it-overlaps
  "⚠ Registration is PARTIAL AND FRACTIONAL, not boolean (design §1)."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-region g "west" '((0d0 0d0) (1d0 0d0) (1d0 2d0) (0d0 2d0)
                                 (0d0 0d0)))
        (%make-region g "east" '((1d0 0d0) (2d0 0d0) (2d0 2d0) (1d0 2d0)
                                 (1d0 0d0)))
        (multiple-value-bind (regs evaluated)
            (register-geometry
             (make-polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0) (1.5d0 1.5d0)
                              (0.5d0 1.5d0) (0.5d0 0.5d0))))
             'ct-region :registry-graph g)
          (is-true evaluated)
          (is (= 2 (length regs)))
          (is (%near 1d0 (reduce #'+ regs :key #'%fraction-of
                                          :initial-value 0d0))
              "the halves partition the subject, so the fractions sum to 1")
          (dolist (r regs)
            (is (< 0.4d0 (%fraction-of r) 0.6d0)))))))

(test a-subject-outside-every-region-registers-to-nothing
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 1d0)
                          (0d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-point 50d0 50d0) 'ct-region
                           :registry-graph g)
      (is-true evaluated "an empty result is an ANSWER, not a failed scan")
      (is (null regs)))))

(test without-geos-a-polygon-refuses-and-a-point-still-registers
  "⚠ A bounding box is OVER-inclusive, so approximating here would bind
records to regions they never touch.  The point is the control: without
it this cannot tell 'refused correctly' from 'broken everywhere'
(design §6)."
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    ;; *GEOS-AVAILABLE-P* is a DEFVAR (globals.lisp), so a LET binding is
    ;; a special binding here as it is in tests/geos/suite.lisp.
    (let ((graph-db::*geos-available-p* nil))
      (multiple-value-bind (regs evaluated)
          (register-geometry
           (make-polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0) (1.5d0 1.5d0)
                            (0.5d0 1.5d0) (0.5d0 0.5d0))))
           'ct-region :registry-graph g)
        (is-false evaluated)
        (is (null regs)))
      (multiple-value-bind (regs evaluated)
          (register-geometry (make-point 1d0 1d0) 'ct-region
                           :registry-graph g)
        (is-true evaluated
                 "a point's candidates are exact with or without GEOS")
        (is (= 1 (length regs)))))))

(test a-line-registers-by-length-not-by-area
  "⚠ A line's AREA is zero, so an area ratio gives it 1.0 in EVERY
region it crosses -- three regions summing to 3.0.  Its fraction is a
LENGTH ratio (design §13).  The regions are stacked north/south and the
line runs along a meridian, where great-circle segments are exactly
additive, so the halves really do sum to the whole."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-region g "south" '((0d0 0d0) (2d0 0d0) (2d0 1d0) (0d0 1d0)
                                  (0d0 0d0)))
        (%make-region g "north" '((0d0 1d0) (2d0 1d0) (2d0 2d0) (0d0 2d0)
                                  (0d0 1d0)))
        (multiple-value-bind (regs evaluated)
            (register-geometry
             (make-linestring '((1d0 0.5d0) (1d0 1.5d0)))
             'ct-region :registry-graph g)
          (is-true evaluated)
          (is (= 2 (length regs)))
          (let ((fs (mapcar #'%fraction-of regs)))
            (is (%near 1d0 (reduce #'+ fs :initial-value 0d0))
                "the halves partition the line, so the fractions sum to ~
1, but they are ~S" fs)
            (dolist (f fs)
              (is (< 0.4d0 f 0.6d0)
                  "each half is about half the line, not ~A" f)))))))

(test a-region-the-subject-only-touches-is-not-registered
  "GEOS `intersects' is true for boundary contact, so an abutting region
comes back as a candidate with a zero-measure intersection.  Registering
it would bind a record to a region it does not overlap -- the mild form
of the false positive design §6 exists to prevent (design §13).  The
second assertion is the control: without it this passes when
registration is broken entirely."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-region g "overlapped" '((0d0 0d0) (1d0 0d0) (1d0 2d0)
                                       (0d0 2d0) (0d0 0d0)))
        (%make-region g "touched" '((1d0 0d0) (2d0 0d0) (2d0 2d0)
                                    (1d0 2d0) (1d0 0d0)))
        (multiple-value-bind (regs evaluated)
            (register-geometry
             (make-polygon '(((0.25d0 0.5d0) (1d0 0.5d0) (1d0 1.5d0)
                              (0.25d0 1.5d0) (0.25d0 0.5d0))))
             'ct-region :registry-graph g)
          (is-true evaluated)
          (is (= 1 (length regs))
              "only the overlapped region registers, not ~S"
              (mapcar (lambda (r) (name (getf r :region))) regs))
          (is (string= "overlapped" (name (getf (first regs) :region)))
              "it still registers to the region it genuinely overlaps")
          (is (%near 1d0 (%fraction-of (first regs))))))))

;;; --- The GEOS refusal, and the handler's NARROWNESS (design §6) ---------
;;;
;;; Which polygons GEOS calls invalid depends on the host's version -- four
;;; sites killed a whole backfill on 3.10.2 that 3.14.1 ran clean -- so a
;;; real invalid polygon makes a poor unit test.  CTR-TRAP-REGION signals
;;; from NODE-GEOMETRY instead, which REGISTER-GEOMETRY calls inside the
;;; very HANDLER-CASE under test, deterministically on any host.

(defparameter *ctr-region-trap* nil
  "What CTR-TRAP-REGION's NODE-GEOMETRY signals: :GEOS, :OTHER, or NIL to
read the slot normally.  NIL while the fixture is written, so the region
indexes like any other and is a genuine candidate; set only around the
scan under test.")

(def-vertex ctr-trap-region ()
  ((name :type string)
   (geom :type geometry :index t))
  :graph-db-register-test)

(defmethod graph-db:node-geometry ((n ctr-trap-region))
  (declare (ignorable n))
  (case *ctr-region-trap*
    (:geos (error 'graph-db:geos-error
                  :message "TopologyException (fixture, no GEOS call made)"))
    (:other (error "a non-GEOS failure raised inside the scan (fixture)"))
    (t (call-next-method))))

(defun %make-trap-region (graph name ring)
  (let ((graph-db:*graph* graph))
    (with-transaction ()
      (make-ctr-trap-region :name name
                            :geom (make-polygon (list ring))))))

(test a-geos-error-anywhere-in-the-scan-refuses-the-whole-scan
  "⚠ The refusal with production history: four sites killed a backfill on
GEOS 3.10.2 that 3.14.1 ran clean, so a scan that meets one is UNANSWERED,
not empty.  A valid region sits in the same scan, so this cannot pass by
finding nothing, and the first scan is the control -- same run, same
fixture, trap off."
  (with-region-graph (g)
    (%make-region g "valid" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                              (0d0 0d0)))
    (%make-trap-region g "trap" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                                  (0d0 0d0)))
    (let ((scope '(ct-region ctr-trap-region))
          (subject (make-point 1d0 1d0)))
      (multiple-value-bind (regs evaluated)
          (register-geometry subject scope :registry-graph g)
        (is-true evaluated "the control scan is evaluated")
        (is (= 2 (length regs))
            "with the trap off BOTH regions register, so the refusal below ~
is not just an empty registry"))
      (let ((*ctr-region-trap* :geos))
        (multiple-value-bind (regs evaluated)
            (register-geometry subject scope :registry-graph g)
          (is-false evaluated "a GEOS-ERROR is a REFUSAL, never a signal")
          (is (null regs)
              "the VALID region's registration goes too: the scan was ~
never answered, and a partial answer would be the false positive design ~
§6 exists to prevent"))))))

(test a-non-geos-error-in-the-scan-propagates-rather-than-being-caught
  "⚠ The handler catches GEOS-ERROR and NOTHING WIDER.  Widening it to
ERROR leaves every other test in this file green while silently swallowing
the multi-graph node-escape class (GH #53) the narrow catch exists to
protect.  The second scan is the control: without it a fixture that never
reaches the handler at all would look the same."
  (with-region-graph (g)
    (%make-trap-region g "trap" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                                  (0d0 0d0)))
    (let ((*ctr-region-trap* :other))
      (signals simple-error
        (register-geometry (make-point 1d0 1d0) 'ctr-trap-region
                           :registry-graph g)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-point 1d0 1d0) 'ctr-trap-region
                           :registry-graph g)
      (is-true evaluated "the same scan answers with the trap off")
      (is (= 1 (length regs))))))

;;; --- REGISTER-NODE: the registration, written as claims (Task 5) --------
;;;
;;; A second graph NAME, so the ambient/registry distinction can be
;;; exercised with two graphs that genuinely differ (design §7).

(defparameter *subject-graph-name* :graph-db-register-subject-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *subject-graph-name* graph-db::*schema-node-metadata*)
        nil))

(def-claim-classes ctr-claim :graph-db-register-test)

;; The registry is a SOURCE, not a bare vertex like CT-REGION above: a
;; claim names its object by EXTERNAL KEY, and OBJECT-KEY is part of
;; DEF-UNIQUE's binary identity tuple, so a registry with no declared key
;; cannot be registered against at all.
(def-source ctr-place :graph-db-register-test
    ((place-key :type string)
     (extent :type geometry :index t))
  :identity     (:namespace :ctr-places :key-slot place-key)
  :space        (:geometry-slot extent :kind :polygon :precision :exact)
  :time         :none
  :attribution  :none
  :sensitivity  (:class :internal)
  :registration :none
  :indexed-text :none)

(defparameter *ctr-precision-m* 25.0d0
  "What CTR-PRECISION reports.  A special, so a re-registration can change
what the facet PRODUCES without redefining the facet -- which is what
tells the upsert's update branch apart from its insert branch.")

(defparameter *ctr-confidence* 0.75d0
  "What CTR-CONFIDENCE reports; see *CTR-PRECISION-M*.")

(defparameter *ctr-method* "geometry-overlap"
  "What CTR-METHOD reports.  Defaults to CTR-RECORD's own static :METHOD
string, so tests that never bind this special see the same value either
way; a test that binds it to something else is the only kind that can
tell the :METHOD-FN path apart from the facet's static string.")

(defun ctr-precision (node)
  "The same value for every record, so a test can assert the facet's
:PRECISION-FN was consulted at all."
  (declare (ignore node))
  *ctr-precision-m*)

(defun ctr-confidence (node)
  (declare (ignore node))
  *ctr-confidence*)

(defun ctr-method (node)
  (declare (ignore node))
  *ctr-method*)

(def-source ctr-record :graph-db-register-test
    ((record-key :type string)
     (loc :type geometry :index t))
  :identity     (:namespace :ctr-records :key-slot record-key)
  :space        (:geometry-slot loc :kind :point :precision :exact)
  :time         :none
  :attribution  :none
  :sensitivity  (:class :internal)
  :registration (:registry ctr-place :registry-namespace :ctr-places
                 :claim-class ctr-claim
                 :producer "graph-db/spacetime-test/register"
                 :relation "registered-at" :method "geometry-overlap"
                 :rule-version "r/1"
                 :precision-fn ctr-precision
                 :confidence-fn ctr-confidence
                 :method-fn ctr-method)
  :indexed-text :none)

;; An extended subject, for the refusal test: a polygon's overlap needs
;; GEOS, a point's does not (design §6).
(def-source ctr-area :graph-db-register-test
    ((area-key :type string)
     (shape :type geometry :index t))
  :identity     (:namespace :ctr-areas :key-slot area-key)
  :space        (:geometry-slot shape :kind :polygon :precision :exact)
  :time         :none
  :attribution  :none
  :sensitivity  (:class :internal)
  :registration (:registry ctr-place :registry-namespace :ctr-places
                 :claim-class ctr-claim
                 :producer "graph-db/spacetime-test/register"
                 :relation "registered-at" :method "geometry-overlap"
                 :rule-version "r/1"
                 :precision-fn nil :confidence-fn nil
                 :method-fn nil)
  :indexed-text :none)

;; The map-less tenant's shape: everything else declared, registration not.
(def-source ctr-plain :graph-db-register-test
    ((plain-key :type string)
     (spot :type geometry :index t))
  :identity     (:namespace :ctr-plains :key-slot plain-key)
  :space        (:geometry-slot spot :kind :point :precision :exact)
  :time         :none
  :attribution  :none
  :sensitivity  (:class :internal)
  :registration :none
  :indexed-text :none)

;; Lives in the OTHER graph, so its registry is genuinely foreign.
(def-source ctr-remote :graph-db-register-subject-test
    ((remote-key :type string)
     (where :type geometry :index t))
  :identity     (:namespace :ctr-remotes :key-slot remote-key)
  :space        (:geometry-slot where :kind :point :precision :exact)
  :time         :none
  :attribution  :none
  :sensitivity  (:class :internal)
  :registration (:registry ctr-place :registry-namespace :ctr-places
                 :claim-class ctr-claim
                 :producer "graph-db/spacetime-test/register"
                 :relation "registered-at" :method "geometry-overlap"
                 :rule-version "r/1"
                 :precision-fn nil :confidence-fn nil
                 :method-fn nil)
  :indexed-text :none)

(defmacro with-two-graphs ((subject registry) &body body)
  "A fresh SUBJECT graph and a fresh REGISTRY graph, both on disk, with the
ambient GRAPH-DB:*GRAPH* bound to the SUBJECT's -- so the two are never
accidentally the same graph, which every Task 4 test left them."
  (let ((d1 (gensym "D1")) (d2 (gensym "D2")))
    `(with-temp-directory (,d1)
       (with-temp-directory (,d2)
         (let* ((,registry (make-graph *region-graph-name* (namestring ,d1)
                                       :buffer-pool-size 1000
                                       :spatial-precision 5))
                (,subject (make-graph *subject-graph-name* (namestring ,d2)
                                      :buffer-pool-size 1000
                                      :spatial-precision 5)))
           (unwind-protect (let ((graph-db:*graph* ,subject)) ,@body)
             (ignore-errors (close-graph ,subject))
             (ignore-errors (close-graph ,registry))
             (collect-garbage)))))))

(defparameter +ctr-square+
  '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0) (0d0 0d0))
  "The one region every REGISTER-NODE test binds to.")

(defun %make-place (graph key ring)
  (let ((graph-db:*graph* graph))
    (with-transaction ()
      (make-ctr-place :place-key key :extent (make-polygon (list ring))))))

(defun %make-record (graph key point)
  (let ((graph-db:*graph* graph))
    (with-transaction () (make-ctr-record :record-key key :loc point))))

(defun %make-area (graph key polygon)
  (let ((graph-db:*graph* graph))
    (with-transaction () (make-ctr-area :area-key key :shape polygon))))

(defun %make-plain (graph key point)
  (let ((graph-db:*graph* graph))
    (with-transaction () (make-ctr-plain :plain-key key :spot point))))

(defun %make-remote (graph key point)
  (let ((graph-db:*graph* graph))
    (with-transaction () (make-ctr-remote :remote-key key :where point))))

(defun %refind (namespace key)
  "The source record (NAMESPACE . KEY), read FRESH through the substrate's
own resolver.  A node object held across a SAVE still reads the OLD
serialized bytes, so a mutation test must re-read rather than re-use."
  (resolve-endpoint namespace key))

(defun %move-area (graph key polygon)
  "Give the CTR-AREA called KEY a new geometry -- a record whose position
was corrected between two ingests.  The COPY is INSIDE the transaction, or
SAVE signals MODIFYING-NON-COPY."
  ;; RESOLVE-ENDPOINT refuses to run inside a read-write transaction
  ;; (resolve.lisp), so the re-read is OUTSIDE it and only the COPY is in.
  (let* ((graph-db:*graph* graph)
         (n (%refind :ctr-areas key)))
    (with-transaction ()
      (let ((c (graph-db:copy n)))
        (setf (shape c) polygon)
        (graph-db:save c)))))

(defun %subject-claims (graph namespace key)
  "Claims in GRAPH naming (NAMESPACE . KEY) as subject.  Goes through the
substrate's own CLAIMS-TOUCHING rather than INDEX-LOOKUP: the index slot
names are DEF-CLAIM-CLASSES's, interned in GRAPH-DB.SPACETIME whoever
invokes the macro, so a bare SUBJECT-KEY here would name another symbol."
  (claims-touching graph 'ctr-claim namespace key :role :subject))

(test registering-a-node-writes-one-claim-per-region
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-record g "s-1" (make-point 1d0 1d0))))
      (multiple-value-bind (written evaluated) (register-node n :graph g)
        (is-true evaluated)
        (is (= 1 written))))
    (let ((c (first (%subject-claims g :ctr-records "s-1"))))
      (is-true c "one claim was written for the subject")
      (is (= 1.0d0 (claim-fraction c)))
      (is (string= "registered-at" (claim-relation c)))
      (is (string= "graph-db/spacetime-test/register" (claim-producer c)))
      (is (eq :ctr-places (claim-object-namespace c)))
      (is (string= "p-a" (claim-object-key c))
          "the region's EXTERNAL key, from its own :IDENTITY facet")
      (is (eq :inferred (claim-standing c))
          "a registration is derived by computation (design §3)")
      (is (= 25.0d0 (claim-precision-m c))
          "the facet's :PRECISION-FN was consulted")
      (is (= 0.75d0 (claim-confidence c)))
      (is (string= "geometry-overlap" (claim-method c))
          "*CTR-METHOD* defaults to the facet's own :METHOD string, so ~
this holds whether :METHOD-FN is consulted or the facet's static ~
:METHOD is written -- the control for the Task 2 ablation."))))

(test registering-a-node-writes-the-method-fns-result-not-the-static-string
  "⚠ CTR-RECORD's facet carries both a static :METHOD, \"geometry-overlap\",
and a :METHOD-FN.  Binding *CTR-METHOD* away from that string is the only
way to tell which one was written (design §3, plan Task 2)."
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-record g "s-1b" (make-point 1d0 1d0)))
          (*ctr-method* "special-method-1"))
      (register-node n :graph g))
    (let ((c (first (%subject-claims g :ctr-records "s-1b"))))
      (is (string= "special-method-1" (claim-method c))
          "the :METHOD-FN's result was written, not the facet's static ~
:METHOD string"))))

(test re-registering-a-node-writes-the-new-method-not-the-old-one
  "⚠ The important case: SITE's re-ingest hits exactly this branch on
every pass, and the facet's :METHOD does not vary but *CTR-METHOD* does
-- so a re-registration must overwrite the STORED method, not just count
a claim (plan Task 2, brief Step 1)."
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-record g "s-1c" (make-point 1d0 1d0))))
      (let ((*ctr-method* "special-method-a"))
        (register-node n :graph g))
      (let ((c (first (%subject-claims g :ctr-records "s-1c"))))
        (is (string= "special-method-a" (claim-method c))
            "the first pass's method"))
      (let ((*ctr-method* "special-method-b"))
        (register-node n :graph g)))
    (is (= 1 (length (%subject-claims g :ctr-records "s-1c")))
        "still ONE claim -- the UPDATE branch, not a second insert")
    (let ((c (first (%subject-claims g :ctr-records "s-1c"))))
      (is (string= "special-method-b" (claim-method c))
          "the second pass's method was written over the first's"))))

(test registering-the-same-node-twice-writes-one-claim
  "⚠ Idempotent on DEF-UNIQUE's binary tuple -- PRODUCER, the subject
pair, the object pair and RELATION.  A re-run of an ingest must not
double a corpus (design §4)."
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-record g "s-2" (make-point 1d0 1d0))))
      (register-node n :graph g)
      (multiple-value-bind (written evaluated) (register-node n :graph g)
        (is-true evaluated)
        (is (= 1 written) "the second pass still reports the claim")))
    (is (= 1 (length (%subject-claims g :ctr-records "s-2")))
        "the second pass UPDATED the claim rather than adding one")))

(test a-source-declaring-registration-none-writes-nothing
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-plain g "u-1" (make-point 1d0 1d0))))
      (multiple-value-bind (written evaluated) (register-node n :graph g)
        (is (zerop written))
        (is-true evaluated ":NONE is an answer, not an unevaluated scan")))
    (is (null (%subject-claims g :ctr-plains "u-1")))))

(test an-unevaluated-scan-writes-nothing-and-says-so
  "⚠ A refusal is never converted into 'wrote 0 claims, all fine'
(design §6).  The point is the control, in the same binding: without it
this cannot tell 'refused correctly' from 'broken everywhere'."
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((a (%make-area g "a-1" (make-polygon
                                  '(((0.5d0 0.5d0) (1.5d0 0.5d0)
                                     (1.5d0 1.5d0) (0.5d0 1.5d0)
                                     (0.5d0 0.5d0))))))
          (p (%make-record g "s-4" (make-point 1d0 1d0))))
      (let ((graph-db::*geos-available-p* nil))
        (multiple-value-bind (written evaluated) (register-node a :graph g)
          (is (zerop written))
          (is-false evaluated))
        (multiple-value-bind (written evaluated) (register-node p :graph g)
          (is-true evaluated "a point's candidates are exact without GEOS")
          (is (= 1 written)))))
    (is (null (%subject-claims g :ctr-areas "a-1"))
        "the refused scan wrote nothing at all")))

(test a-subject-with-no-geometry-is-not-answered
  "Where the record is, is unknown -- which is not the same as its being
in no region (design §6)."
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-record g "s-5" nil)))
      (multiple-value-bind (written evaluated) (register-node n :graph g)
        (is (zerop written))
        (is-false evaluated)))))

(test the-registry-is-read-under-its-own-graph-not-the-ambient-one
  "⚠ Here the subject's graph and the registry's genuinely differ, and the
ambient GRAPH-DB:*GRAPH* is the SUBJECT's.  A region read under that
binding comes back NIL through NODE-GEOMETRY's IGNORE-ERRORS, so the
region is dropped and the caller sees 'no regions here' with EVALUATED-P
true -- a wrong-graph read wearing an answer's clothes.  Every Task 4 test
bound *GRAPH* and passed :GRAPH to the same graph, so this was untested
until now (design §7, GH #53)."
  (with-two-graphs (subject registry)
    (is-false (eq subject registry) "the two graphs must genuinely differ")
    (is (eq subject graph-db:*graph*) "the ambient graph is the subject's")
    (%make-place registry "p-b" +ctr-square+)
    (let ((n (%make-remote subject "r-1" (make-point 1d0 1d0))))
      (multiple-value-bind (written evaluated)
          (register-node n :graph subject :registry-graph registry)
        (is-true evaluated)
        (is (= 1 written) "the region is found under the REGISTRY's graph")))
    (let ((c (first (claims-touching registry 'ctr-claim :ctr-remotes "r-1"
                                     :role :subject))))
      (is-true c "the claim is written in the REGISTRY graph")
      (is (string= "p-b" (claim-object-key c))
          "the region's key was read under the registry graph too"))))

(test re-registering-a-node-writes-the-new-values-not-the-old-ones
  "⚠ Counting claims does NOT test the upsert's UPDATE branch: deleting
its SETF block, or its SAVE, leaves a count-only test green.  What the
facet produces is changed between the two passes, and what is STORED must
follow (design §4).  This branch runs on every re-ingest."
  (with-region-graph (g)
    (%make-place g "p-a" +ctr-square+)
    (let ((n (%make-record g "s-6" (make-point 1d0 1d0))))
      (register-node n :graph g)
      (let ((c (first (%subject-claims g :ctr-records "s-6"))))
        (is (= 25.0d0 (claim-precision-m c)) "the first pass's value")
        (is (= 0.75d0 (claim-confidence c))))
      (let ((*ctr-precision-m* 40.0d0)
            (*ctr-confidence* 0.9d0))
        (register-node n :graph g)))
    (is (= 1 (length (%subject-claims g :ctr-records "s-6")))
        "still ONE claim -- the UPDATE branch, not a second insert")
    (let ((c (first (%subject-claims g :ctr-records "s-6"))))
      (is (= 40.0d0 (claim-precision-m c))
          "the second pass's PRECISION-M was written over the first's")
      (is (= 0.9d0 (claim-confidence c))
          "and its CONFIDENCE with it"))))

(test a-moved-subject-re-registers-with-the-new-fraction
  "⚠ FRACTION is what the update branch exists for: a re-ingest whose
geometry was corrected must overwrite the stored share, not keep the one
the first pass computed."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-place g "p-a" +ctr-square+)
        (register-node
         (%make-area g "a-2"
                     (make-polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0)
                                      (1.5d0 1.5d0) (0.5d0 1.5d0)
                                      (0.5d0 0.5d0)))))
         :graph g)
        (is (%near 1d0 (claim-fraction
                        (first (%subject-claims g :ctr-areas "a-2"))))
            "wholly inside the region to begin with")
        ;; Half of it now lies east of the region's 2° edge.
        (%move-area g "a-2"
                    (make-polygon '(((1d0 0.5d0) (3d0 0.5d0) (3d0 1.5d0)
                                     (1d0 1.5d0) (1d0 0.5d0)))))
        (register-node (%refind :ctr-areas "a-2") :graph g)
        (is (= 1 (length (%subject-claims g :ctr-areas "a-2")))
            "still ONE claim -- the UPDATE branch, not a second insert")
        (is (%near 0.5d0 (claim-fraction
                          (first (%subject-claims g :ctr-areas "a-2"))))
            "the NEW share was written over the old 1.0"))))

;;; --- invalid subject geometry (#138 task 6b task 5, finding I1) --------
;;; The tenant rule this API replaced repaired BOTH geometries with
;;; GEOMETRY-MAKE-VALID before intersecting and clamped the result to 1.0.
;;; Dropping either is not a simplification: an invalid ring can clear the
;;; index's INTERSECTS refinement and then throw inside GEOSIntersection,
;;; which refuses the WHOLE subject -- and its raw abs-summed spherical
;;; excess is not the area an intersection is a share of, so an unclamped
;;; ratio can exceed 1 and be written to a claim.

(defun %bowtie ()
  "A self-intersecting ring with two UNEQUAL lobes: (0,0)-(10,10) crosses
 (10,0)-(0,2) at about (1.67, 1.67).  Unequal deliberately -- a symmetric
bow-tie's two lobes cancel in the signed excess sum, which the ABS in
%RING-GEODESIC-AREA-M2 then reports as ~0, and a zero-measure subject
takes the 1.0 shortcut instead of exercising the ratio at all."
  (make-polygon '(((0d0 0d0) (10d0 10d0) (10d0 0d0) (0d0 2d0)
                   (0d0 0d0)))))

(test an-invalid-subject-polygon-is-repaired-not-refused
  "⚠ AN INVALID EXTENT MUST NOT COST THE SUBJECT ITS OTHER REGIONS.
GEOS validity is host- and version-dependent -- 4 of 341 site extents
that GEOS 3.14.1 tolerates were rejected by 3.10.2 -- so a self-
intersecting ring is a real deployed population, not a contrived input.
The scan must still be EVALUATED, and the fraction must still be a
SHARE: in [0,1], never above it (design §1, §6)."
  (cond
    ((not graph-db::*geos-available-p*) (skip "GEOS not available"))
    ((not graph-db::*geos-makevalid-available-p*)
     (skip "GEOS < 3.8: no makeValid"))
    (t
     (with-region-graph (g)
       ;; A region comfortably containing the whole bow-tie, so the only
       ;; thing under test is the subject's own validity.
       (%make-region g "cover" '((-1d0 -1d0) (11d0 -1d0) (11d0 11d0)
                                 (-1d0 11d0) (-1d0 -1d0)))
       (is-false (graph-db:geometry-valid-p (%bowtie))
                 "fixture sanity: the bow-tie really is invalid")
       (multiple-value-bind (regs evaluated)
           (register-geometry (%bowtie) 'ct-region :registry-graph g)
         (is-true evaluated
                  "an invalid ring must be repaired, not refused")
         (is (= 1 (length regs)))
         (let ((f (%fraction-of (first regs))))
           (is (<= f 1.0d0)
               "FRACTION is a share in [0,1]; got ~S" f)
           (is (plusp f)
               "a repaired bow-tie inside the region is not a touch")))))))

(test the-clamp-is-what-holds-the-upper-bound
  "⚠ A UNIT TEST ON %OVERLAP-FRACTION, DELIBERATELY.  The test above
does NOT pin the clamp: with subject and region both repaired the
intersection is a subset of the subject, so the ratio never exceeds 1 by
more than float noise and (IS (<= F 1.0D0)) passes with the MIN deleted.
Neither the implementer nor two reviewers could construct an end-to-end
input that overruns it.

Calling the function directly does what no fixture can: SUBJECT-MEASURE
is the caller's, so understating it makes the ratio genuinely 2.0 and
only the clamp brings it back to 1.0.  Verified RED with the MIN removed
(#138 task 6b, final review M-1; design §2's [0,1] range)."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available: GEOMETRY-INTERSECTION needs it")
      (let* ((square (make-polygon '(((0d0 0d0) (1d0 0d0) (1d0 1d0)
                                      (0d0 1d0) (0d0 0d0)))))
             (measure #'graph-db:geometry-geodesic-area)
             (whole (funcall measure square)))
        (is (plusp whole) "fixture sanity: the square has area")
        (is (= 1.0d0 (graph-db.spacetime::%overlap-fraction
                      square square measure (/ whole 2)))
            "an understated denominator must still not exceed 1.0"))))

;;; --- the repair that came back a GEOMETRYCOLLECTION (GH #163) ----------
;;; %REPAIRED's fallback to the original is for a repair that CANNOT
;;; happen (no add-on, GEOS < 3.8).  A mixed-dimension repair can happen
;;; -- GEOSMakeValid just answers it with a GEOMETRYCOLLECTION -- and
;;; taking that for "cannot" cost seven deployed subjects every region
;;; they overlapped.

(defun %tailed-square ()
  "A square whose ring runs on past its start out to 20°E and back: the
tail has no width, so the repair is a POLYGON plus a LINESTRING."
  (make-polygon '(((0d0 0d0) (10d0 0d0) (10d0 10d0) (0d0 10d0) (0d0 0d0)
                   (20d0 0d0) (0d0 0d0)))))

(test a-subject-whose-repair-is-a-collection-still-registers
  "⚠ THE WHOLE SUBJECT WAS LOST, not one region: GEOMETRY-MAKE-VALID
signalled on the GEOMETRYCOLLECTION, %REPAIRED handed back the
unrepaired ring, GEOMETRY-INTERSECTION threw on it and the GEOS-ERROR
handler returned (VALUES NIL NIL).  Measured on 7 of 4,196 deployed
subjects (GH #163)."
  (cond
    ((not graph-db::*geos-available-p*) (skip "GEOS not available"))
    ((not graph-db::*geos-makevalid-available-p*)
     (skip "GEOS < 3.8: no makeValid"))
    (t
     (with-region-graph (g)
       ;; Covers the square but NOT the tail, so a repair that kept the
       ;; tail as area would not measure 1.0 either.
       (%make-region g "cover" '((-1d0 -1d0) (11d0 -1d0) (11d0 11d0)
                                 (-1d0 11d0) (-1d0 -1d0)))
       (is-false (graph-db:geometry-valid-p (%tailed-square))
                 "fixture sanity: the tailed square really is invalid")
       (multiple-value-bind (regs evaluated)
           (register-geometry (%tailed-square) 'ct-region
                              :registry-graph g)
         (is-true evaluated
                  "a mixed-dimension repair is a repair, not a refusal")
         (is (= 1 (length regs)))
         (is (%near 1d0 (%fraction-of (first regs)))
             "the repaired square lies wholly inside the region"))))))
