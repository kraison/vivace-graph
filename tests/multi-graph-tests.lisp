;;;; Multi-graph coexistence: three graphs, one image.
;;;;
;;;; mine-action runs an ops graph, a knowledge graph and a forensics graph in a
;;;; single Lisp image, each with its own vertex classes, vector segments and
;;;; lifecycle.  These tests pin the properties that arrangement depends on.
;;;; They are engine-level and have no mine-action dependency.
(in-package #:graph-db/test)

(def-suite multi-graph-suite :in graph-db-suite
  :description "Three graphs coexisting in one image.")
(in-suite multi-graph-suite)

;; Declared once at load time, like the other integration schema in this
;; file's neighbours.  Three graph names -- :MG-ALPHA / :MG-BETA / :MG-GAMMA --
;; stand in for mine-action's ops / knowledge / forensics graphs.  MG-BOTH and
;; MG-GEO sharing :MG-GAMMA is deliberate: it is the same "text-indexed and
;; geo-indexed mixins compose, and a geo-only class contributes no vector
;; segment" property the forensics-graph design rests on (ACLED vs FIRMS).
(def-vertex mg-plain () ((label :type string)) :mg-alpha)
(def-vertex mg-text  () ((label :type string) (embedding :vector-index t)) :mg-beta)
(def-vertex mg-geo   () ((label :type string) (lat) (lon)) :mg-gamma)
(def-vertex mg-both  () ((label :type string) (lat) (lon) (embedding :vector-index t)) :mg-gamma)

(defun %mg-embedding (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v) (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(defmacro with-three-graphs ((ga gb gc) &body body)
  "Bind GA, GB, GC to freshly created graphs named :MG-ALPHA, :MG-BETA and
:MG-GAMMA respectively, each in its own scratch directory, run BODY, then
close and clean up all three regardless of how BODY exits.  Mirrors
WITH-TEST-GRAPH (suite.lisp), but for three independently-named graphs open
at once -- which is the whole point of this suite."
  (let ((dir-a (gensym "DIR-A")) (dir-b (gensym "DIR-B")) (dir-c (gensym "DIR-C")))
    `(with-temp-directory (,dir-a)
       (with-temp-directory (,dir-b)
         (with-temp-directory (,dir-c)
           (let ((,ga (make-graph :mg-alpha (namestring ,dir-a) :buffer-pool-size 1000))
                 (,gb (make-graph :mg-beta (namestring ,dir-b) :buffer-pool-size 1000))
                 (,gc (make-graph :mg-gamma (namestring ,dir-c) :buffer-pool-size 1000)))
             (unwind-protect
                  (progn ,@body)
               (ignore-errors (close-graph ,ga :snapshot-p nil))
               (ignore-errors (close-graph ,gb :snapshot-p nil))
               (ignore-errors (close-graph ,gc :snapshot-p nil))
               (collect-garbage))))))))

(test three-graphs-open-as-distinct-objects
  "Opening all three graphs in one image yields three distinct GRAPH objects
with three distinct GRAPH-NAMEs -- the property every downstream forensics-
graph decision (a third graph alongside ops and knowledge) assumes."
  (with-three-graphs (ga gb gc)
    (is (not (eq ga gb)) "mg-alpha and mg-beta must not be the same object")
    (is (not (eq gb gc)) "mg-beta and mg-gamma must not be the same object")
    (is (not (eq ga gc)) "mg-alpha and mg-gamma must not be the same object")
    (let ((names (list (graph-db:graph-name ga) (graph-db:graph-name gb) (graph-db:graph-name gc))))
      (is (equal '(:mg-alpha :mg-beta :mg-gamma) names)
          "expected graph-names (:MG-ALPHA :MG-BETA :MG-GAMMA), got ~S" names)
      (is (= 3 (length (remove-duplicates names)))
          "expected three DISTINCT graph-names, got ~S" names))))

(test writes-land-only-in-their-own-graph
  "Writing to each graph under its own *GRAPH* binding must land only in
that graph's own type index.  The cross probes below -- every FOREIGN type
reading back 0 in a graph it was never written to -- are the assertion that
actually discriminates: a test that only counted each graph's OWN type would
pass even against a badly broken type registry that let every class write
into every graph, because MAP-VERTICES on the right graph with the right type
would still return the right count regardless of what else leaked in."
  (with-three-graphs (ga gb gc)
    (let ((*graph* ga))
      (with-transaction () (make-mg-plain :label "a1"))
      (with-transaction () (make-mg-plain :label "a2")))
    (let ((*graph* gb))
      (with-transaction () (make-mg-text :label "b1" :embedding (%mg-embedding 4 1.0))))
    (let ((*graph* gc))
      (with-transaction () (make-mg-geo :label "g1" :lat 1.0d0 :lon 2.0d0))
      (with-transaction () (make-mg-both :label "g2" :lat 3.0d0 :lon 4.0d0
                                          :embedding (%mg-embedding 4 2.0))))
    (flet ((count-of (graph type)
             (length (map-vertices #'identity graph :collect-p t :vertex-type type))))
      ;; each graph's own type(s) -- sanity, not the discriminating part
      (is (= 2 (count-of ga 'mg-plain)) "mg-alpha should hold its own 2 mg-plain vertices")
      (is (= 1 (count-of gb 'mg-text)) "mg-beta should hold its own 1 mg-text vertex")
      (is (= 1 (count-of gc 'mg-geo)) "mg-gamma should hold its own 1 mg-geo vertex")
      (is (= 1 (count-of gc 'mg-both)) "mg-gamma should hold its own 1 mg-both vertex")
      ;; every FOREIGN type must read back 0 in every OTHER graph -- THE POINT
      (is (= 0 (count-of ga 'mg-text)) "mg-text must not appear in mg-alpha")
      (is (= 0 (count-of ga 'mg-geo)) "mg-geo must not appear in mg-alpha")
      (is (= 0 (count-of ga 'mg-both)) "mg-both must not appear in mg-alpha")
      (is (= 0 (count-of gb 'mg-plain)) "mg-plain must not appear in mg-beta")
      (is (= 0 (count-of gb 'mg-geo)) "mg-geo must not appear in mg-beta")
      (is (= 0 (count-of gb 'mg-both)) "mg-both must not appear in mg-beta")
      (is (= 0 (count-of gc 'mg-plain)) "mg-plain must not appear in mg-gamma")
      (is (= 0 (count-of gc 'mg-text)) "mg-text must not appear in mg-gamma"))))

(test vector-segments-are-per-graph-and-per-class
  "VECTOR-SEGMENTS is a per-graph hash keyed (CLASS . SLOT).  Creating
mg-text vertices in mg-beta and mg-both vertices in mg-gamma must establish
exactly the segments the mixin design promises: one in mg-beta keyed
(MG-TEXT . EMBEDDING), one in mg-gamma keyed (MG-BOTH . EMBEDDING), and
mg-alpha -- which indexes nothing -- zero."
  (with-three-graphs (ga gb gc)
    (let ((*graph* ga))
      (with-transaction () (make-mg-plain :label "a1")))
    (let ((*graph* gb))
      (with-transaction () (make-mg-text :label "b1" :embedding (%mg-embedding 4 1.0))))
    (let ((*graph* gc))
      (with-transaction () (make-mg-geo :label "g1" :lat 1.0d0 :lon 2.0d0))
      (with-transaction () (make-mg-both :label "g2" :lat 3.0d0 :lon 4.0d0
                                          :embedding (%mg-embedding 4 2.0))))
    (let ((alpha-segs (graph-db::vector-segments ga))
          (beta-segs (graph-db::vector-segments gb))
          (gamma-segs (graph-db::vector-segments gc)))
      (is (= 0 (hash-table-count alpha-segs))
          "mg-alpha declares no :vector-index slot anywhere and must have zero segments, got ~S"
          (alexandria:hash-table-keys alpha-segs))
      (is (= 1 (hash-table-count beta-segs))
          "mg-beta must have exactly one segment, got ~S" (alexandria:hash-table-keys beta-segs))
      (is (not (null (gethash (cons 'mg-text 'embedding) beta-segs)))
          "mg-beta's one segment must be keyed (MG-TEXT . EMBEDDING), got ~S"
          (alexandria:hash-table-keys beta-segs))
      (is (= 1 (hash-table-count gamma-segs))
          "mg-gamma must have exactly one segment despite holding TWO indexed-capable ~
classes (mg-geo, mg-both), got ~S" (alexandria:hash-table-keys gamma-segs))
      (is (not (null (gethash (cons 'mg-both 'embedding) gamma-segs)))
          "mg-gamma's one segment must be keyed (MG-BOTH . EMBEDDING), got ~S"
          (alexandria:hash-table-keys gamma-segs)))))

(test geo-only-class-creates-no-segment
  "mg-geo shares a graph (:mg-gamma) with mg-both, which DOES get a vector
segment -- so this is not merely \"the graph has no segments\", it is the
composition property the mixin design actually rests on: adding a
non-indexed (geo-only) class alongside an indexed one must not spill a
segment onto the non-indexed class.  mg-geo has no :vector-index slot at
all, so no key with car MG-GEO may ever appear in mg-gamma's
vector-segments table.  This is the engine-level version of the spike's
'fx-hotspot got none' observation (ACLED = both mixins; FIRMS = geo mixin
alone, the negative case)."
  (with-three-graphs (ga gb gc)
    ;; Keep all three graphs genuinely alive and taking writes, not just gc --
    ;; the property under test is about mg-gamma's own composition, but it
    ;; should hold with the other two graphs open alongside it, exactly as
    ;; mine-action's three graphs will be.
    (let ((*graph* ga)) (with-transaction () (make-mg-plain :label "a1")))
    (let ((*graph* gb))
      (with-transaction () (make-mg-text :label "b1" :embedding (%mg-embedding 4 9.0))))
    (let ((*graph* gc))
      (with-transaction () (make-mg-geo :label "g1" :lat 1.0d0 :lon 2.0d0))
      (with-transaction () (make-mg-both :label "g2" :lat 3.0d0 :lon 4.0d0
                                          :embedding (%mg-embedding 4 2.0))))
    (let ((gamma-segs (graph-db::vector-segments gc)))
      (is (null (find 'mg-geo (alexandria:hash-table-keys gamma-segs) :key #'car))
          "mg-geo must not have contributed any vector segment; keys present: ~S"
          (alexandria:hash-table-keys gamma-segs))
      (is (= 1 (hash-table-count gamma-segs))
          "mg-gamma must have exactly the mg-both segment and nothing contributed by ~
mg-geo, got ~S" (alexandria:hash-table-keys gamma-segs)))))

(test all-three-close-clean-and-reopen
  "A clean close of all three graphs together leaves no .dirty marker in any
of the three directories, and reopening restores every vertex count and both
established vector segments (mg-beta's and mg-gamma's)."
  (with-temp-directory (dir-a)
    (with-temp-directory (dir-b)
      (with-temp-directory (dir-c)
        (let (ga gb gc beta-id gamma-both-id
              alpha-count beta-count gamma-geo-count gamma-both-count
              beta-vec gamma-vec)
          (setf ga (make-graph :mg-alpha (namestring dir-a) :buffer-pool-size 1000))
          (setf gb (make-graph :mg-beta (namestring dir-b) :buffer-pool-size 1000))
          (setf gc (make-graph :mg-gamma (namestring dir-c) :buffer-pool-size 1000))
          (let ((*graph* ga))
            (with-transaction () (make-mg-plain :label "a1"))
            (with-transaction () (make-mg-plain :label "a2")))
          (let ((*graph* gb))
            (with-transaction ()
              (setf beta-id (id (make-mg-text :label "b1" :embedding (%mg-embedding 4 1.0))))))
          (let ((*graph* gc))
            (with-transaction () (make-mg-geo :label "g1" :lat 1.0d0 :lon 2.0d0))
            (with-transaction ()
              (setf gamma-both-id
                    (id (make-mg-both :label "g2" :lat 3.0d0 :lon 4.0d0
                                       :embedding (%mg-embedding 4 5.0))))))
          (setf alpha-count (length (map-vertices #'identity ga :collect-p t :vertex-type 'mg-plain))
                beta-count (length (map-vertices #'identity gb :collect-p t :vertex-type 'mg-text))
                gamma-geo-count (length (map-vertices #'identity gc :collect-p t :vertex-type 'mg-geo))
                gamma-both-count (length (map-vertices #'identity gc :collect-p t :vertex-type 'mg-both))
                beta-vec (graph-db::segment-get
                          (gethash (cons 'mg-text 'embedding) (graph-db::vector-segments gb))
                          beta-id)
                gamma-vec (graph-db::segment-get
                           (gethash (cons 'mg-both 'embedding) (graph-db::vector-segments gc))
                           gamma-both-id))
          (is (= 2 alpha-count))
          (is (= 1 beta-count))
          (is (= 1 gamma-geo-count))
          (is (= 1 gamma-both-count))
          (close-graph ga :snapshot-p t)
          (close-graph gb :snapshot-p t)
          (close-graph gc :snapshot-p t)
          (is (null (probe-file (format nil "~A/.dirty" (namestring dir-a))))
              "mg-alpha's .dirty must be absent after a clean close")
          (is (null (probe-file (format nil "~A/.dirty" (namestring dir-b))))
              "mg-beta's .dirty must be absent after a clean close")
          (is (null (probe-file (format nil "~A/.dirty" (namestring dir-c))))
              "mg-gamma's .dirty must be absent after a clean close")
          (let ((ga2 (open-graph :mg-alpha (namestring dir-a)))
                (gb2 (open-graph :mg-beta (namestring dir-b)))
                (gc2 (open-graph :mg-gamma (namestring dir-c))))
            (unwind-protect
                 (progn
                   (is (= alpha-count
                          (length (map-vertices #'identity ga2 :collect-p t :vertex-type 'mg-plain)))
                       "mg-alpha's vertex count must survive close/reopen")
                   (is (= beta-count
                          (length (map-vertices #'identity gb2 :collect-p t :vertex-type 'mg-text)))
                       "mg-beta's vertex count must survive close/reopen")
                   (is (= gamma-geo-count
                          (length (map-vertices #'identity gc2 :collect-p t :vertex-type 'mg-geo)))
                       "mg-gamma's mg-geo count must survive close/reopen")
                   (is (= gamma-both-count
                          (length (map-vertices #'identity gc2 :collect-p t :vertex-type 'mg-both)))
                       "mg-gamma's mg-both count must survive close/reopen")
                   (let ((beta-seg2 (gethash (cons 'mg-text 'embedding) (graph-db::vector-segments gb2)))
                         (gamma-seg2 (gethash (cons 'mg-both 'embedding) (graph-db::vector-segments gc2))))
                     (is (not (null beta-seg2)) "mg-beta's vector segment must be restored on reopen")
                     (is (not (null gamma-seg2)) "mg-gamma's vector segment must be restored on reopen")
                     (when beta-seg2
                       (is (every #'= beta-vec (graph-db::segment-get beta-seg2 beta-id))
                           "mg-beta's restored embedding must match what was written"))
                     (when gamma-seg2
                       (is (every #'= gamma-vec (graph-db::segment-get gamma-seg2 gamma-both-id))
                           "mg-gamma's restored embedding must match what was written"))))
              (ignore-errors (close-graph ga2 :snapshot-p nil))
              (ignore-errors (close-graph gb2 :snapshot-p nil))
              (ignore-errors (close-graph gc2 :snapshot-p nil)))))
        (collect-garbage)))))

;;; ---------------------------------------------------------------------------
;;; keep-revisions retention-cost bound
;;;
;;; mine-action's forensics graph adopts KEEP-REVISIONS = 100 on the strength of
;;; a single measurement (spec 2026-07-22-forensics-graph-design.md, S5):
;;; REAP-OLD-VERSIONS runs inside the transaction-manager lock, so its cost is
;;; paid by every commit in the graph, not only commits touching a deep version
;;; chain.  Without a guard, a future engine change could make retention
;;; expensive again -- linear in total chain length rather than in
;;; KEEP-REVISIONS -- and nothing would notice; the failure mode is a slow
;;; ingest, not a failing test.  Two more graph names/classes (distinct from
;;; MG-ALPHA/BETA/GAMMA above; a vertex type is bound to one graph name for its
;;; life in this image) stand in for a control graph and a bounded-retention
;;; graph.
;;; ---------------------------------------------------------------------------

(def-vertex mg-ctl-counter () ((counter :type integer)) :mg-delta)
(def-vertex mg-bnd-counter () ((counter :type integer)) :mg-epsilon)

(defparameter *mg-retain-update-count* 2000
  "Number of update commits timed against each of the control and bounded
graphs in KEEP-REVISIONS-BOUNDS-COMMIT-LATENCY.  2,000 matches the measurement
recorded in the spec and in this file's docstrings, and is large enough that
the bounded graph's chain (capped at its KEEP-REVISIONS window) reaches and
holds steady state for the great majority of the run.")

(defparameter *mg-epsilon-keep-revisions* 100
  "KEEP-REVISIONS for the bounded graph in the test below -- mine-action's
chosen forensics-graph policy.  Left as a DEFPARAMETER, not inlined, because
proving the test discriminates (see the task report) means temporarily setting
this to 4,000,000,000 -- effectively unbounded within the run -- and confirming
the assertion fails, then restoring it to 100 before committing.")

(defun %mg-commit-latencies-ms (graph vertex-id n)
  "Update the vertex at VERTEX-ID in GRAPH N times, using the mine-action
update idiom -- (COPY -> SETF -> SAVE) inside its own WITH-TRANSACTION -- and
return a list of N per-commit wall-clock latencies in milliseconds.  Uses
LOOKUP-VERTEX and SLOT-VALUE rather than a type-specific accessor, so this one
helper drives both MG-CTL-COUNTER and MG-BND-COUNTER."
  (let ((*graph* graph))
    (loop repeat n
          collect (let ((start (get-internal-real-time)))
                    (with-transaction ()
                      (let ((v (copy (lookup-vertex vertex-id))))
                        (setf (slot-value v 'counter) (random 1000000))
                        (save v)))
                    (/ (* 1000d0 (- (get-internal-real-time) start))
                       internal-time-units-per-second)))))

(defun %mg-mean (numbers)
  (/ (reduce #'+ numbers) (float (length numbers) 1.0d0)))

(test keep-revisions-bounds-commit-latency
  "KEEP-REVISIONS retention cost is paid by every commit in the graph, not only
commits touching a deep version chain: REAP-OLD-VERSIONS runs inside the
transaction-manager lock (see APPLY-TRANSACTION, transactions.lisp).

Measured 2026-07-22 (the number this bound and mine-action's policy both rest
on): with KEEP-REVISIONS at 4,000,000,000 -- i.e. effectively never reaping --
2,000 updates to one vertex degraded mean commit latency 0.378ms -> 1.378ms,
approximately 3.65x, roughly linearly in chain length (about +0.0005ms per
retained revision), against a flat ~0.31ms/commit control at KEEP-REVISIONS=0.

mine-action's forensics graph adopts KEEP-REVISIONS=100: the degradation is
linear in chain length, and its chains are short (FIRMS detections are written
once; ACLED events see a handful of corrections), so a window of 100 gives
complete audit history at negligible cost. This test pins that a *bounded*
window of 100 stays cheap relative to a KEEP-REVISIONS=0 control over the same
number of updates to a single vertex -- so a future regression that made the
reaper cost scale with total chain length again, rather than with
KEEP-REVISIONS, would fail this test rather than silently slow every commit in
production.

Bound chosen: mean commit latency at KEEP-REVISIONS=100 must stay within 3x of
the KEEP-REVISIONS=0 control's mean, both measured over *MG-RETAIN-UPDATE-COUNT*
(2,000) updates to a single vertex.

RECALIBRATED 2026-07-29 (GH #87), from 2x to 3x.  This is a ratio, so it moves
when the DENOMINATOR moves: #87 removed the per-slot-access rebuilding of the
persistent/ephemeral/meta name lists, which sped ordinary commits up far more
than it sped up reaping.  The reaper's ABSOLUTE cost -- the thing this test
exists to protect -- did not change:

                     control    bounded    ratio    reap overhead
    SBCL  before     0.4805ms   0.5395ms   1.123x   0.059ms
    SBCL  after      0.1810ms   0.2360ms   1.304x   0.055ms
    ECL   before     5.6066ms   7.1893ms   1.282x   1.583ms
    ECL   after      1.3497ms   3.0719ms   2.276x   1.722ms

so the ECL ratio crossed 2x purely because its baseline got 4.15x cheaper while
a fixed ~1.6ms of reaping stayed put.  (ECL's reaping is ~27x more expensive in
absolute terms than SBCL's, which is why ECL is the implementation that trips
this and SBCL is not.)

Discrimination is re-proved at the new bound, and is in fact WIDER than before:
setting *MG-EPSILON-KEEP-REVISIONS* to 4,000,000,000 -- effectively never
reaping within the run -- now measures 4.506x on SBCL and 12.119x on ECL,
against healthy ratios of 1.304x (SBCL) and 2.17x-2.28x over five runs (ECL).
A 3x bound sits with ~1.3x margin above the worst healthy observation and ~1.5x
below the closest pathological one.

Note that no single ratio ever separated healthy from pathological across BOTH
implementations before #87: the pre-#87 SBCL pathological case measured 2.342x
while the post-#87 ECL healthy case measures 2.28x.  The original 2x was
calibrated on SBCL alone (six runs, 0.953x-1.021x) and was already marginal on
ECL.  If this test trips again, check the reap OVERHEAD column above before
touching the bound -- a real regression moves that number, a baseline
improvement does not.

Note also that this mean-over-the-whole-run ratio is smaller than the 3.65x the
spec quotes for KEEP-REVISIONS=4e9 at update 2,000: the spec's figure compares
the first and the very last commit of the ramp, while this test averages every
commit across it, including the cheap early ones before the chain has grown --
both are real measurements of the same degradation, taken differently."
  (with-temp-directory (dir-ctl)
    (with-temp-directory (dir-bnd)
      (let (gctl gbnd id-ctl id-bnd)
        (unwind-protect
             (progn
               (setf gctl (make-graph :mg-delta (namestring dir-ctl)
                                       :buffer-pool-size 1000 :keep-revisions 0))
               (setf gbnd (make-graph :mg-epsilon (namestring dir-bnd)
                                       :buffer-pool-size 1000
                                       :keep-revisions *mg-epsilon-keep-revisions*))
               (let ((*graph* gctl))
                 (with-transaction () (setf id-ctl (id (make-mg-ctl-counter :counter 0)))))
               (let ((*graph* gbnd))
                 (with-transaction () (setf id-bnd (id (make-mg-bnd-counter :counter 0)))))
               ;; Warm both graphs identically before timing, so JIT/file-cache
               ;; warm-up is not mistaken for retention cost.
               (%mg-commit-latencies-ms gctl id-ctl 20)
               (%mg-commit-latencies-ms gbnd id-bnd 20)
               (let* ((n *mg-retain-update-count*)
                      (ctl-latencies (%mg-commit-latencies-ms gctl id-ctl n))
                      (bnd-latencies (%mg-commit-latencies-ms gbnd id-bnd n))
                      (ctl-mean (%mg-mean ctl-latencies))
                      (bnd-mean (%mg-mean bnd-latencies))
                      (ratio (/ bnd-mean ctl-mean)))
                 (format t "~&;; keep-revisions=0 control mean commit latency over ~D updates: ~,4Fms~%"
                         n ctl-mean)
                 (format t "~&;; keep-revisions=~D bounded mean commit latency over ~D updates: ~,4Fms (ratio ~,3Fx)~%"
                         *mg-epsilon-keep-revisions* n bnd-mean ratio)
                 (is (<= bnd-mean (* 3.0d0 ctl-mean))
                     "keep-revisions=~D mean commit latency ~,4Fms must stay within 3x the ~
keep-revisions=0 control's ~,4Fms mean over ~D updates (observed ratio ~,3Fx)"
                     *mg-epsilon-keep-revisions* bnd-mean ctl-mean n ratio)))
          (ignore-errors (close-graph gctl :snapshot-p nil))
          (ignore-errors (close-graph gbnd :snapshot-p nil))
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; VERTEX-HISTORY: the public read path over the retained MVCC chain
;;;
;;; Retention (above) without a read path is storage we pay for and cannot use.
;;; mine-action's forensics graph adopts KEEP-REVISIONS=100 specifically to keep
;;; an audit trail -- what a source told us about an event, and when -- so the
;;; chain-walk that until now existed only as an internal snapshot-isolation
;;; mechanism (RESOLVE-VERSION-AT-EPOCH) needs a supported public form.
;;; ---------------------------------------------------------------------------

(def-vertex mg-hist-note () ((note :type string)) :mg-zeta)

(defparameter *mg-history-update-count* 5
  "Number of in-place updates applied to the single vertex in
VERTEX-HISTORY-WALKS-THE-MVCC-CHAIN.  The expected history length is this plus
one, for the original create.")

(test vertex-history-walks-the-mvcc-chain
  "VERTEX-HISTORY (GRAPH ID &KEY LIMIT) returns the retained MVCC versions of a
vertex as (VERSION . COMMIT-EPOCH) pairs, newest first, the live version
included and first.

Create one vertex and update it *MG-HISTORY-UPDATE-COUNT* (5) times with
distinguishable NOTE values in a KEEP-REVISIONS=100 graph -- a window far wider
than the 6 versions produced, so the reaper cannot truncate what we assert on.
Then check every property a consumer reading this as an audit trail depends on:
the entry count, newest-first ordering, STRICTLY decreasing commit-epochs, the
right slot value at every position (not merely at the ends -- a walk that
returned the live version six times, or that skipped a link, would satisfy a
count-and-endpoints test), that the head of the list really is the live version
(same commit-epoch and revision as a plain LOOKUP-VERTEX returns), and that
:LIMIT truncates from the NEW end rather than the old.

The version objects are also read back with *GRAPH* bound to NIL, deliberately:
each returned version must be self-contained (bytes and data materialized while
the read pin was held), because an audit consumer will hold these objects well
past the call and often outside any *GRAPH* binding."
  (with-temp-directory (dir)
    (let (g id)
      (unwind-protect
           (progn
             (setf g (make-graph :mg-zeta (namestring dir)
                                 :buffer-pool-size 1000 :keep-revisions 100))
             (let ((*graph* g))
               (with-transaction () (setf id (id (make-mg-hist-note :note "v0"))))
               (dotimes (i *mg-history-update-count*)
                 (with-transaction ()
                   (let ((v (copy (lookup-vertex id))))
                     (setf (slot-value v 'note) (format nil "v~D" (1+ i)))
                     (save v)))))
             (let* ((expected-length (1+ *mg-history-update-count*))
                    (expected-notes (loop for i from *mg-history-update-count* downto 0
                                          collect (format nil "v~D" i)))
                    (history (vertex-history g id)))
               (is (= expected-length (length history))
                   "expected ~D history entries (1 create + ~D updates), got ~D"
                   expected-length *mg-history-update-count* (length history))
               (when (= expected-length (length history))
                 (let ((notes (let ((*graph* nil))
                                (mapcar (lambda (entry) (slot-value (car entry) 'note))
                                        history)))
                       (epochs (mapcar #'cdr history)))
                   (is (equal expected-notes notes)
                       "expected notes newest-first ~S, got ~S" expected-notes notes)
                   (is (every #'> epochs (rest epochs))
                       "commit-epochs must be STRICTLY decreasing newest-first, got ~S" epochs)))
               ;; The head of the list must be the live version, not merely the
               ;; newest archived one.
               (let ((live (let ((*graph* g)) (lookup-vertex id))))
                 (is (= (graph-db::commit-epoch live) (cdr (first history)))
                     "the first entry's commit-epoch ~D must be the LIVE version's ~D"
                     (cdr (first history)) (graph-db::commit-epoch live))
                 (is (= (graph-db:revision live) (graph-db:revision (car (first history))))
                     "the first entry's revision ~D must be the LIVE version's ~D"
                     (graph-db:revision (car (first history))) (graph-db:revision live)))
               ;; :LIMIT truncates from the old end -- it returns the N NEWEST.
               (let* ((limited (vertex-history g id :limit 3))
                      (limited-notes (let ((*graph* nil))
                                       (mapcar (lambda (entry) (slot-value (car entry) 'note))
                                               limited))))
                 (is (= 3 (length limited))
                     ":limit 3 must return exactly 3 entries, got ~D" (length limited))
                 (is (equal '("v5" "v4" "v3") limited-notes)
                     ":limit 3 must return the three NEWEST versions, got ~S" limited-notes))))
        (ignore-errors (close-graph g :snapshot-p nil))
        (collect-garbage)))))

(test cross-graph-lookup-materializes-the-owning-graphs-class
  "LOOKUP-VERTEX with an explicit :GRAPH must materialize the vertex's CLASS
against THAT graph's schema, even when the ambient *GRAPH* is a different graph
whose schema maps the same type-id to a different class.

Type-ids are per-graph -- assigned by class-registration order -- so the SAME
integer names different classes in different graphs.  MG-PLAIN is the only user
class in :MG-ALPHA and MG-TEXT the only one in :MG-BETA, so both are type-id 1
(the test asserts that collision as a precondition, so a future registration-order
change reports itself rather than silently no longer exercising the bug).

This is the reachable form of the wrong-graph materialization class in issue #53.
It was demonstrated in production: a forensics ACLED-EVENT (type-id 3) read while
*GRAPH* was the ops graph (type-id 3 = ADMIN-RAION) materialized as ADMIN-RAION,
its ACLED accessors returning NIL, while the raw type-id and type-index were both
correct.  MAP-VERTICES already binds *GRAPH* around its scan (edge.lisp / the note
in MAP-VERTICES) so it was never affected; LOOKUP-OBJECT did not, so a cross-graph
LOOKUP-VERTEX resolved the type-id against the wrong graph's schema.  The close +
reopen below forces a fresh disk deserialization: the node written under ga is
cached with its correct class, so only a from-disk read exercises the deserializer."
  (with-temp-directory (dir-a)
    (with-temp-directory (dir-b)
      (let ((ga (make-graph :mg-alpha (namestring dir-a) :buffer-pool-size 1000))
            (gb (make-graph :mg-beta  (namestring dir-b) :buffer-pool-size 1000))
            (id nil) (tid nil))
        (unwind-protect
             (progn
               (let ((*graph* ga))
                 (with-transaction () (setf id (id (make-mg-plain :label "a1")))))
               ;; precondition: one type-id, two different classes across the graphs
               (setf tid (graph-db::node-type-id
                          (graph-db::lookup-node-type-by-name 'mg-plain :vertex :graph ga)))
               (is (eq 'mg-plain (graph-db::node-type-name
                                  (graph-db::lookup-node-type-by-id tid :vertex :graph ga)))
                   "precondition: type-id ~D must be MG-PLAIN in ga" tid)
               (is (eq 'mg-text (graph-db::node-type-name
                                 (graph-db::lookup-node-type-by-id tid :vertex :graph gb)))
                   "PRECONDITION FAILED: type-id ~D is not MG-TEXT in gb (~S); registration ~
                    order changed and this test no longer collides"
                   tid (let ((m (graph-db::lookup-node-type-by-id tid :vertex :graph gb)))
                         (and m (graph-db::node-type-name m))))
               ;; force a from-disk read so the cache can't hand back the correctly
               ;; classed object written above
               (close-graph ga :snapshot-p t)
               (setf ga (open-graph :mg-alpha (namestring dir-a)))
               ;; the reproducing read: explicit :GRAPH ga, ambient *GRAPH* is gb
               (let ((*graph* gb))
                 (let ((node (lookup-vertex id :graph ga)))
                   (is (not (null node))
                       "the MG-PLAIN node must still be found in ga after reopen")
                   (is (eq 'mg-plain (type-of node))
                       "cross-graph lookup must materialize ga's class MG-PLAIN, got ~S ~
                        -- type-id ~D was resolved against gb's schema, not ga's"
                       (type-of node) tid))))
          (ignore-errors (close-graph ga :snapshot-p nil))
          (ignore-errors (close-graph gb :snapshot-p nil))
          (collect-garbage))))))

;;;; ---------------------------------------------------------------------------
;;;; GH #53: every node carries its home graph.
;;;;
;;;; An unstamped node leaves NODE-GRAPH NIL, and NIL silently falls back to
;;;; *GRAPH* -- reintroducing the cross-graph read the slot exists to stop.  So
;;;; each materialization path gets its own test, and each test is written to
;;;; fail if ITS stamp alone is removed: where two paths would otherwise cover
;;;; each other, the test clears the slot first to model the real path that
;;;; delivers an unstamped node there (peer apply, journal replay, image
;;;; restore -- all of which build nodes off the wire/disk, not via MAKE-<type>).
;;;; ---------------------------------------------------------------------------

;; An edge type on :MG-ALPHA, so the on-disk stamps can be asserted for edges
;; as well as vertices.
(def-edge mg-link () () :mg-alpha)

;; An in-memory graph for the memory-backend stamps.  Its own name, so the
;; on-disk :MG-* schemas stay untouched.
(def-vertex mg-mem-note () ((note :type string)) :mg-mem)
(def-edge mg-mem-link () () :mg-mem)

(defmacro with-alpha-graph ((g) &body body)
  "Fresh on-disk graph named :MG-ALPHA in a scratch dir, *GRAPH* bound to it."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph :mg-alpha (namestring ,d) :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defmacro with-mem-graph ((g) &body body)
  "Fresh in-memory graph named :MG-MEM in a scratch dir; closed afterward."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (graph-db::make-memory-graph :mg-mem (namestring ,d))))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defun %unstamp (node)
  "Clear NODE's home graph, modelling a node that arrived off the wire / off
disk rather than through MAKE-<type>.  Returns NODE."
  (setf (graph-db::node-graph node) nil)
  node)

(test nodes-record-their-home-graph
  "A node read out of its own graph reports THAT graph, whatever *GRAPH* is
bound to at the time (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id b-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "in-a")))))
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "in-b")))))
      (let ((*graph* gc))
        (is (eq ga (graph-db::node-graph (lookup-vertex a-id :graph ga)))
            "home graph must come from the lookup, not *GRAPH*")
        (is (eq gb (graph-db::node-graph (lookup-vertex b-id :graph gb)))
            "home graph must come from the lookup, not *GRAPH*")))))

(test node-graph-stamped-at-creation
  "MAKE-VERTEX / MAKE-EDGE stamp the node they build, so it is stamped for the
WHOLE body of the creating transaction -- not just from commit (GH #53).
Asserted INSIDE the transaction, the only place COMMIT's stamp cannot have run
yet."
  (with-alpha-graph (g)
    (with-transaction ()
      (let* ((v1 (make-mg-plain :label "v1"))
             (v2 (make-mg-plain :label "v2"))
             (e (make-mg-link :from v1 :to v2)))
        (is (eq g (graph-db::node-graph v1))
            "a vertex must be stamped before its transaction commits")
        (is (eq g (graph-db::node-graph e))
            "an edge must be stamped before its transaction commits")))))

(test node-graph-stamped-at-commit
  "FINALIZE-NODE stamps the node it writes.  Load-bearing for nodes that enter
APPLY-TX-WRITE without ever passing through MAKE-<type> -- peer apply and
journal replay build them off the wire; the UNSTAMP models that (GH #53)."
  (with-alpha-graph (g)
    (let ((v nil))
      (with-transaction ()
        (setq v (%unstamp (make-mg-plain :label "off-the-wire"))))
      (is (eq g (graph-db::node-graph v))
          "commit must stamp the node it writes, even when it arrived unstamped"))))

(test node-home-graph-defaults-on-unbound-slot
  "NODE-HOME-GRAPH must fall back to DEFAULT when the GRAPH slot is UNBOUND, not
just when it is NIL.  CL-STORE excludes the slot from serialization, so a node
fresh out of CL-STORE:RESTORE (before any downstream stamp runs) has it UNBOUND
-- and a bare NODE-GRAPH read on an unbound slot SIGNALS rather than falling
back (GH #53).  Built directly via ALLOCATE-INSTANCE so the slot is never
touched, rather than routing through CL-STORE."
  (with-alpha-graph (g)
    (let ((n (allocate-instance (find-class 'mg-plain))))
      (is (not (slot-boundp n 'graph-db::graph))
          "precondition: a freshly allocated instance must have an unbound GRAPH slot")
      (is (eq g (graph-db::node-home-graph n g))
          "node-home-graph must fall back to DEFAULT on an unbound slot, not signal"))))

(test node-graph-stamped-on-update
  "APPLY-TX-WRITE for TX-UPDATE stamps both the new node it writes and the
archived OLD-NODE, symmetric with TX-CREATE's FINALIZE-NODE call above.  Journal
replay, replication apply and peer push all build TX-UPDATE via
DESERIALIZE-TRANSACTION-NODE-VECTOR, which never runs COPY-NODE's own stamp --
the UNSTAMP models that on both sides here (GH #53)."
  (with-alpha-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-mg-plain :label "before"))))
      (with-transaction ()
        (let* ((old (%unstamp (copy (lookup-vertex id :graph g))))
               (new (%unstamp (copy (lookup-vertex id :graph g)))))
          (setf (slot-value new 'label) "after")
          (graph-db::apply-tx-write
           (make-instance 'graph-db::tx-update :node new :old-node old)
           g)
          (is (eq g (graph-db::node-graph new))
              "tx-update must stamp the new node it writes")
          (is (eq g (graph-db::node-graph old))
              "tx-update must stamp the archived old node too"))))))

(test node-graph-stamped-on-cache-hit
  "LOOKUP-NODE stamps on the CACHE-HIT branch too, so \"every node LOOKUP-NODE
returns is stamped\" holds unconditionally.  Nodes reach the cache unstamped via
APPLY-TX-WRITE :AFTER, which the UNSTAMP models.  Read through a TYPED
side-effect scan: inside its read pin LOOKUP-OBJECT skips ENSURE-NODE-BYTES, so
the cache-hit branch is the only stamp on that path (GH #53)."
  (with-alpha-graph (g)
    (let (id (seen '()))
      (with-transaction () (setq id (id (make-mg-plain :label "cached"))))
      (let ((cached (gethash id (graph-db::cache g))))
        (is (not (null cached))
            "precondition: the node must be in the graph cache, or this tests nothing")
        (when cached (%unstamp cached)))
      (map-vertices (lambda (v) (push (graph-db::node-graph v) seen)) g
                    :vertex-type 'mg-plain)
      (is (equal (list g) seen)
          "a cache HIT must stamp the node it returns, got ~S" seen))))

(test node-graph-stamped-on-cold-read
  "LOOKUP-NODE's miss branch stamps the node the deserializer just built.  Read
inside a transaction with the caches cleared: that path never calls
ENSURE-NODE-BYTES, so the miss branch is the only stamp (GH #53)."
  (with-alpha-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-mg-plain :label "cold"))))
      (clrhash (graph-db::cache g))
      (let ((graph-db::*cache-enabled* nil))
        (with-transaction ()
          (let ((v (lookup-vertex id :graph g)))
            (is (eq g (graph-db::node-graph v))
                "a cold transactional read must stamp the node it materializes")))))))

(test node-graph-stamped-on-untyped-scan
  "The fully-untyped MAP-VERTICES / MAP-EDGES scans hand out nodes straight from
the lhash deserializer, and a SIDE-EFFECT scan (no :COLLECT-P) never runs
ENSURE-NODE-BYTES -- so backup, GC, COMPACT-VERTICES and reindex would all see
NIL without this stamp (GH #53)."
  (with-alpha-graph (g)
    (let ((seen '()) (n 0))
      (with-transaction ()
        (let ((v1 (make-mg-plain :label "s1"))
              (v2 (make-mg-plain :label "s2")))
          (make-mg-link :from v1 :to v2)))
      (clrhash (graph-db::cache g))
      (map-vertices (lambda (v) (incf n) (pushnew (graph-db::node-graph v) seen)) g)
      (map-edges (lambda (e) (incf n) (pushnew (graph-db::node-graph e) seen)) g)
      (is (= 3 n) "precondition: the untyped scans must have visited 3 nodes, got ~D" n)
      (is (equal (list g) seen)
          "every node an untyped side-effect scan yields must be stamped with its ~
graph, got ~S" seen))))

(test node-graph-survives-copy
  "COPY builds a fresh instance for the update path; it must carry the original's
home graph (GH #53)."
  (with-alpha-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-mg-plain :label "orig"))))
      (with-transaction ()
        (let ((c (copy (lookup-vertex id :graph g))))
          (is (eq g (graph-db::node-graph c))
              "a COPY must inherit the original's home graph"))))))

(test node-graph-stamped-on-archived-versions
  "ENSURE-NODE-BYTES stamps the versions it materializes.  The ARCHIVED entries
VERTEX-HISTORY returns come straight off the heap and reach the caller through
no other stamping path (GH #53)."
  (with-temp-directory (dir)
    (let (g id)
      (unwind-protect
           (progn
             (setf g (make-graph :mg-zeta (namestring dir)
                                 :buffer-pool-size 1000 :keep-revisions 100))
             (let ((*graph* g))
               (with-transaction () (setf id (id (make-mg-hist-note :note "v0"))))
               (dotimes (i 3)
                 (with-transaction ()
                   (let ((v (copy (lookup-vertex id))))
                     (setf (slot-value v 'note) (format nil "v~D" (1+ i)))
                     (save v)))))
             (let* ((history (vertex-history g id))
                    (archived (rest history))
                    (graphs (mapcar (lambda (e) (graph-db::node-graph (car e))) archived)))
               (is (= 3 (length archived))
                   "precondition: expected 3 archived versions, got ~D" (length archived))
               (is (equal (list g g g) graphs)
                   "every archived version must be stamped with its graph, got ~S" graphs)))
        (ignore-errors (close-graph g :snapshot-p nil))
        (collect-garbage)))))

;;; --- the memory backend ------------------------------------------------------

(test memory-node-graph-stamped-at-commit
  "The memory backend overrides APPLY-TX-WRITE for TX-CREATE, so FINALIZE-NODE
never runs for it; the override must stamp instead.  The UNSTAMP models a node
applied from a peer pull rather than built by MAKE-<type> (GH #53)."
  (with-mem-graph (g)
    (let ((v nil))
      (with-transaction () (setq v (%unstamp (make-mg-mem-note :note "off-the-wire"))))
      (is (eq g (graph-db::node-graph v))
          "a memory-graph commit must stamp the node it publishes"))))

(test memory-node-graph-stamped-on-update
  "The memory backend overrides APPLY-TX-WRITE for TX-UPDATE too; the override
must stamp both the published new node and the archived OLD-NODE, symmetric with
TX-CREATE's override above.  Peer apply and journal replay build TX-UPDATE off
the wire, never through COPY-NODE's own stamp -- the UNSTAMP models that on both
sides (GH #53)."
  (with-mem-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-mg-mem-note :note "before"))))
      (with-transaction ()
        (let* ((old (%unstamp (copy (lookup-vertex id :graph g))))
               (new (%unstamp (copy (lookup-vertex id :graph g)))))
          (setf (slot-value new 'note) "after")
          (graph-db::apply-tx-write
           (make-instance 'graph-db::tx-update :node new :old-node old)
           g)
          (is (eq g (graph-db::node-graph new))
              "a memory-graph tx-update must stamp the new node it publishes")
          (is (eq g (graph-db::node-graph old))
              "a memory-graph tx-update must stamp the old node too"))))))

(test memory-node-graph-stamped-on-lookup
  "LOOKUP-NODE on a MEM-TABLE stamps what it returns.  Nodes land in a mem-table
unstamped via peer apply and image restore, which the UNSTAMP models.  Read
through a TYPED side-effect scan, whose read pin makes LOOKUP-OBJECT skip
ENSURE-NODE-BYTES, leaving the mem-table lookup as the only stamp (GH #53)."
  (with-mem-graph (g)
    (let (id (seen '()))
      (with-transaction () (setq id (id (make-mg-mem-note :note "in-ram"))))
      (%unstamp (graph-db::mem-table-get (graph-db::vertex-table g) id))
      (map-vertices (lambda (v) (push (graph-db::node-graph v) seen)) g
                    :vertex-type 'mg-mem-note)
      (is (equal (list g) seen)
          "a mem-table lookup must stamp the node it returns, got ~S" seen))))

(test memory-lazy-materialization-stamps-the-node
  "%LZNODE->NODE builds a live node from a deferred blob on first touch; it must
stamp it (GH #53).  Called directly, because LOOKUP-NODE would otherwise stamp
the materialized node on its way out and hide a missing stamp here."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) id)
      (let ((g (graph-db::make-memory-graph :mg-mem loc :lazy t)))
        (let ((*graph* g))
          (with-transaction () (setq id (id (make-mg-mem-note :note "lazy")))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph :mg-mem loc :lazy t)))
        (unwind-protect
             (let* ((table (graph-db::vertex-table g2))
                    (lz (graph-db::mem-table-get table id)))
               (is-true (graph-db::lznode-p lz)
                        "precondition: a lazy reopen must hold the node as an LZNODE")
               (when (graph-db::lznode-p lz)
                 (let ((node (graph-db::%lznode->node table id lz)))
                   (is (eq g2 (graph-db::node-graph node))
                       "fault-in materialization must stamp the node it builds"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test memory-image-never-stores-the-node-graph-slot
  "The cl-store memory image stores the node CLOS objects themselves, and
cl-store does not honour :PERSISTENT NIL -- so without an explicit exclusion the
GRAPH slot would drag the live graph (schema, mem-tables and hence every node,
transaction-manager, locks, threads) into the image, and restore would hand back
a phantom graph (GH #53)."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) id)
      (let ((g (graph-db::make-memory-graph :mg-mem loc)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (let ((a (make-mg-mem-note :note "a"))
                       (b (make-mg-mem-note :note "b")))
                   (setq id (id a))
                   (make-mg-mem-link :from a :to b)))
               ;; The slot must not even be offered to cl-store.
               (let ((slots (mapcar #'graph-db::slot-definition-name
                                    (cl-store:serializable-slots (lookup-vertex id :graph g)))))
                 (is (null (member 'graph-db::graph slots))
                     "cl-store must not see a node's GRAPH slot; serializable slots were ~S"
                     slots))
               (graph-db::checkpoint-memory-graph g))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage)))
      ;; ... and nothing graph-shaped may come back out of the image file.
      (let* ((image (graph-db::memory-image-file (pathname loc)))
             (blob (cl-store:restore image))
             (stored (append (getf blob :vertices) (getf blob :edges))))
        (is (= 3 (length stored))
            "precondition: the image must hold the 2 vertices + 1 edge, got ~D"
            (length stored))
        (is (every (lambda (n) (not (and (slot-boundp n 'graph-db::graph)
                                         (slot-value n 'graph-db::graph))))
                   stored)
            "no node restored straight from the image file may carry a graph"))
      ;; ... and the restored nodes are usable, stamped with the REOPENED graph.
      (let ((g2 (graph-db::open-memory-graph :mg-mem loc)))
        (unwind-protect
             (progn
               ;; Straight out of the mem-table, before anything looks it up:
               ;; restore must leave the slot BOUND and right, or NODE-GRAPH
               ;; signals instead of falling back.
               (let ((raw (graph-db::mem-table-get (graph-db::vertex-table g2) id)))
                 ;; IS-TRUE, not IS: FiveAM's IS evaluates a two-argument form's
                 ;; arguments eagerly, which would defeat the boundp guard.
                 (is-true (and (slot-boundp raw 'graph-db::graph)
                               (eq g2 (slot-value raw 'graph-db::graph)))
                          "restore must stamp the nodes it puts in the mem-table"))
               (let ((v (lookup-vertex id :graph g2)))
                 (is (string= "a" (slot-value v 'note))
                     "a restored node must still carry its data")
                 (is (eq g2 (graph-db::node-graph v))
                     "a restored node must be stamped with the graph that reopened it")))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; --- cross-graph reads under a foreign *GRAPH* (GH #53) ---------------------
;;;
;;; The stamps above are load-bearing, not decorative: these three tests pin
;;; the actual consumer-visible behaviour they enable -- reading a node found
;;; via an explicit :GRAPH stays correct no matter what *GRAPH* is ambiently
;;; bound to.  Lookups are deliberately OUTSIDE any transaction (reading a
;;; foreign graph inside a read-write transaction becomes an error in Task 4).

;; Both tests below close and reopen GA/GB before the cross-graph reads.
;; Without that, A-ID/B-ID are still the live, correctly-classed objects
;; CACHE-HIT hands back from their own creation moments earlier -- the read
;; never touches the deserializer, and passes no matter what *GRAPH* is bound
;; to.  Reopening forces a genuine disk materialization through
;; DESERIALIZE-VERTEX-HEAD's per-graph type-id -> class resolution, the actual
;; cross-graph hazard (MG-PLAIN and MG-TEXT collide on type-id 1; see
;; CROSS-GRAPH-LOOKUP-MATERIALIZES-THE-OWNING-GRAPHS-CLASS above).  TYPE-OF is
;; asserted alongside the slot value because MG-PLAIN and MG-TEXT both declare
;; a slot named LABEL -- a wrongly-classed node would still return the right
;; string, so the slot check alone does not discriminate.

(test slot-reads-resolve-through-the-nodes-own-graph
  "Read a node's slots with *GRAPH* bound to a different graph (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id b-id loc-a loc-b)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "alpha-value")))))
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "beta-value")))))
      (setq loc-a (graph-db:location ga) loc-b (graph-db:location gb))
      (close-graph ga :snapshot-p t)
      (close-graph gb :snapshot-p t)
      (setq ga (open-graph :mg-alpha loc-a))
      (setq gb (open-graph :mg-beta loc-b))
      (let ((*graph* gb))
        (let ((node (lookup-vertex a-id :graph ga)))
          (is (eq 'mg-plain (type-of node))
              "a-id must materialize as MG-PLAIN with *GRAPH* bound to gb, got ~S"
              (type-of node))
          (is (string= "alpha-value" (slot-value node 'label)))))
      (let ((*graph* ga))
        (let ((node (lookup-vertex b-id :graph gb)))
          (is (eq 'mg-text (type-of node))
              "b-id must materialize as MG-TEXT with *GRAPH* bound to ga, got ~S"
              (type-of node))
          (is (string= "beta-value" (slot-value node 'label)))))
      (let ((*graph* gc))
        (is (string= "alpha-value"
                     (slot-value (lookup-vertex a-id :graph ga) 'label)))
        (is (string= "beta-value"
                     (slot-value (lookup-vertex b-id :graph gb) 'label)))))))

(test lazy-slot-reads-resolve-through-the-nodes-own-graph
  "A node whose data is still LAZY, read with *GRAPH* bound elsewhere, must
materialize from its OWN heap (GH #53).

The test above does not cover this: LOOKUP-OBJECT's standalone branch binds
*GRAPH* to the requested graph AND calls ENSURE-NODE-BYTES, so by the time the
slot is read the bytes are already in the image and no heap is touched.  A
side-effect MAP-VERTICES scan is the opposite: it runs FN inside the read pin,
so the nodes it hands out keep BYTES = :INIT.  Stash one, let the scan return
-- dropping its *GRAPH* binding -- and read the slot under a foreign *GRAPH*."
  (with-three-graphs (ga gb gc)
    (let (loc-a)
      (let ((*graph* ga))
        (with-transaction () (make-mg-plain :label "lazy-alpha"))
        (with-transaction () (make-mg-plain :label "lazy-beta")))
      (setq loc-a (graph-db:location ga))
      (close-graph ga :snapshot-p t)
      (setq ga (open-graph :mg-alpha loc-a))
      ;; Two lazy nodes: SLOT-VALUE and SLOT-BOUNDP each need one that has not
      ;; already been materialized by the other's read.
      (let (nodes)
        (map-vertices (lambda (v) (push v nodes)) ga :vertex-type 'mg-plain)
        (is (= 2 (length nodes))
            "precondition: the scan must have yielded 2 vertices, got ~D" (length nodes))
        (dolist (n nodes)
          (is (or (eq :init (graph-db::bytes n)) (null (graph-db::bytes n)))
              "precondition: the node's bytes must still be lazy, got ~S"
              (graph-db::bytes n))
          (is (null (graph-db::data n))
              "precondition: the node's data must still be lazy, got ~S"
              (graph-db::data n))
          (is (plusp (graph-db::data-pointer n))
              "precondition: the node must have a heap data-pointer to resolve"))
        (when (= 2 (length nodes))
          (let ((value (let ((*graph* gb))
                         (handler-case (slot-value (first nodes) 'label)
                           (error (e) (format nil "<error: ~A>" (type-of e))))))
                (bound (let ((*graph* gb))
                         (handler-case (slot-boundp (second nodes) 'label)
                           (error (e) (format nil "<error: ~A>" (type-of e)))))))
            (is (member value '("lazy-alpha" "lazy-beta") :test #'equal)
                "a lazy slot must materialize from GA's heap, not *GRAPH*'s; got ~S"
                value)
            (is (eq t bound)
                "SLOT-BOUNDP must resolve through the node's own graph too; got ~S"
                bound))))
      gc)))

(test node-to-alist-resolves-through-the-nodes-own-graph
  "NODE-TO-ALIST omits :GRAPH and fell back to *GRAPH* (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id loc-a)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "alist-alpha")))))
      (setq loc-a (graph-db:location ga))
      (close-graph ga :snapshot-p t)
      (setq ga (open-graph :mg-alpha loc-a))
      (let ((*graph* gb))
        (is (string= "alist-alpha"
                     (cdr (assoc :label
                                 (graph-db::node-to-alist
                                  (lookup-vertex a-id :graph ga)))))))
      gc)))

(test copies-carry-the-home-graph
  "COPY-NODE enumerates the slots it copies (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "orig")))))
      (let ((node (let ((*graph* gb)) (lookup-vertex a-id :graph ga))))
        (let ((*graph* ga))
          (with-transaction ()
            (let ((copy (copy node)))
              (is (eq ga (graph-db::node-graph copy)))
              (is (string= "orig" (slot-value copy 'label)))))))
      gc)))

;;; --- read-write transactions are single-graph (GH #53) ----------------------

(test read-write-transaction-rejects-a-foreign-read
  "A read-write transaction is single-graph; reading another graph signals
rather than silently returning NIL (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "in-a")))))
      (signals graph-db:cross-graph-transaction-error
        (let ((*graph* gb))
          (with-transaction () (lookup-vertex a-id :graph ga))))
      (is (not (null (let ((*graph* ga))
                       (with-transaction () (lookup-vertex a-id :graph ga)))))
          "a same-graph transactional read is unaffected")
      gc)))

(test read-write-transaction-rejects-a-foreign-write
  "Saving a node whose home is another graph signals (GH #53).  The transaction
is opened on GB's manager directly rather than by rebinding *GRAPH*, which keeps
this test isolated to the write check it is about."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "orig")))))
      (let ((*graph* ga))
        (let ((node (lookup-vertex a-id :graph ga)))
          (signals graph-db:cross-graph-transaction-error
            (with-transaction ((graph-db::transaction-manager gb))
              (let ((copy (copy node)))
                (setf (slot-value copy 'label) "nope")
                (save copy :graph gb))))))
      gc)))

(test read-write-transaction-rejects-a-foreign-delete
  "DELETE-NODE bypasses UPDATE-NODE, so it needs its own check (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "doomed")))))
      (let ((node (let ((*graph* gb)) (lookup-vertex a-id :graph ga))))
        (signals graph-db:cross-graph-transaction-error
          (let ((*graph* gb))
            (with-transaction () (mark-deleted node)))))
      ;; and it must still be there
      (is (not (null (lookup-vertex a-id :graph ga)))
          "the foreign delete must not have landed")
      gc)))

(test read-write-transaction-rejects-a-foreign-create
  "CREATE-NODE ignored its GRAPH argument and ENSURE-TRANSACTION reused the
ambient *TRANSACTION*, so a node stamped :GRAPH GA inside a transaction opened
on GB was written into GB silently -- the create-path hole closed by GH #96."
  (with-three-graphs (ga gb gc)
    (signals graph-db:cross-graph-transaction-error
      (with-transaction ((graph-db::transaction-manager gb))
        (make-mg-plain :label "misrouted" :graph ga)))
    gc))

(test cross-graph-error-report-is-readable-on-the-read-path
  "The read path stores a raw id byte vector in the NODE slot, not a node
object (see the TRANSACTION method of LOOKUP-OBJECT); the report must print it
via STRING-ID like the write path does, not the raw vector (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "in-a")))))
      (let ((condition
              (handler-case
                  (let ((*graph* gb))
                    (with-transaction () (lookup-vertex a-id :graph ga)))
                (graph-db:cross-graph-transaction-error (e) e))))
        (is (typep condition 'graph-db:cross-graph-transaction-error))
        (let ((report (format nil "~A" condition)))
          (is (search (string-id a-id) report)
              "report must contain the readable hex id, got ~A" report)))
      gc)))

;;; --- read-only snapshots are per graph and compose (GH #53) -----------------

(defun %mg-count (graph type)
  (length (map-vertices #'identity graph :collect-p t :vertex-type type)))

(test read-only-snapshots-compose-across-graphs
  "A read-only snapshot may span graphs; each graph is internally consistent,
with no single instant across graphs (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id b-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "a1")))))
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "b1")))))
      (graph-db:with-read-snapshot (ga)
        (graph-db:with-read-snapshot (gb)
          (is (string= "a1" (slot-value (lookup-vertex a-id :graph ga) 'label)))
          (is (string= "b1" (slot-value (lookup-vertex b-id :graph gb) 'label)))))
      gc)))

(test composed-snapshots-each-hide-their-own-graphs-later-commits
  "THE discriminating test for the registry.  Reading the right value under two
composed snapshots (above) proves nothing on its own: LOOKUP-OBJECT's
NON-transactional method binds *GRAPH* to the requested graph, so a cross-graph
read returns the right value even with no snapshot in force at all.  What only a
per-graph registry can do is make BOTH reads resolve through their OWN graph's
snapshot -- so a commit made to either graph after the snapshots were taken is
invisible in that graph, while both remain visible outside.  If either graph's
entry were missing (or a single snapshot covered only one of them), that graph's
read would fall through to a live non-transactional read and see 2 (GH #53)."
  (with-three-graphs (ga gb gc)
    (let ((a-inside 0) (b-inside 0))
      (let ((*graph* ga)) (with-transaction () (make-mg-plain :label "a1")))
      (let ((*graph* gb)) (with-transaction () (make-mg-text :label "b1")))
      (graph-db:with-read-snapshot (ga)
        (graph-db:with-read-snapshot (gb)
          ;; commit into BOTH graphs after both snapshots were taken
          (let ((*graph* ga)) (with-transaction () (make-mg-plain :label "a2")))
          (let ((*graph* gb)) (with-transaction () (make-mg-text :label "b2")))
          (setq a-inside (%mg-count ga 'mg-plain)
                b-inside (%mg-count gb 'mg-text))))
      (is (= 1 a-inside)
          "ga's snapshot must hide the mg-plain committed after it started, saw ~D" a-inside)
      (is (= 1 b-inside)
          "gb's snapshot must hide the mg-text committed after it started, saw ~D" b-inside)
      (is (= 2 (%mg-count ga 'mg-plain)) "outside the snapshot ga has both vertices")
      (is (= 2 (%mg-count gb 'mg-text)) "outside the snapshot gb has both vertices")
      gc)))

(test read-write-transaction-blocks-a-foreign-read-even-under-a-snapshot
  "Step 1 of the read-resolution rule is exhaustive: an active read-write
transaction forbids cross-graph access outright and does NOT fall through to
*READ-SNAPSHOTS*, even when a snapshot for the other graph is active.  Answering
such a read from a different consistency domain is exactly the works-until-it-
doesn't behaviour the contract removes (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (b-id)
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "b1")))))
      (graph-db:with-read-snapshot (gb)
        ;; a snapshot of GB is active, and the read below still must signal
        (is (not (null (lookup-vertex b-id :graph gb)))
            "the gb snapshot really is in force outside the rw transaction")
        (signals graph-db:cross-graph-transaction-error
          (with-transaction ((graph-db::transaction-manager ga))
            (lookup-vertex b-id :graph gb))))
      gc)))

(test read-snapshots-do-not-outlive-their-dynamic-extent
  "A snapshot left registered pins the MVCC reaper's floor and retains versions
forever, so both the registry entry and the transaction-manager registration
must be undone on unwind -- including a non-local exit (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (ta tb)
      (ignore-errors
        (graph-db:with-read-snapshot (ga)
          (graph-db:with-read-snapshot (gb)
            (setq ta (gethash ga graph-db:*read-snapshots*)
                  tb (gethash gb graph-db:*read-snapshots*))
            (error "unwind"))))
      (is (not (null ta)) "ga's snapshot must be registered under ga while active")
      (is (not (null tb)) "gb's snapshot must be registered under gb while active")
      (is (null graph-db:*read-snapshots*)
          "the registry must not outlive the snapshots' extent")
      (is (null (gethash (graph-db::sequence-number ta)
                         (graph-db::transactions (graph-db::transaction-manager ga))))
          "ga's snapshot transaction must be deregistered from its manager")
      (is (null (gethash (graph-db::sequence-number tb)
                         (graph-db::transactions (graph-db::transaction-manager gb))))
          "gb's snapshot transaction must be deregistered from its manager")
      ;; nested snapshots share one table, so an inner entry must be gone while
      ;; the OUTER extent is still running -- otherwise later reads of gb inside
      ;; it would resolve through a snapshot that has already been discarded
      (graph-db:with-read-snapshot (ga)
        (graph-db:with-read-snapshot (gb) nil)
        (is (null (gethash gb graph-db:*read-snapshots*))
            "the inner snapshot's entry must be removed from the shared table")
        (is (not (null (gethash ga graph-db:*read-snapshots*)))
            "the outer snapshot's own entry must survive its inner one"))
      gc)))
