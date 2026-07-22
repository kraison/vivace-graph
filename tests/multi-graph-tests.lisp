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
