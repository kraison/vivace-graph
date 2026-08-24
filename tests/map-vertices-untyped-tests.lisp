;;;; Untyped MAP-VERTICES vs the per-type walks (GH #219).
;;;;
;;;; The untyped scan walks the raw vertex lhash; a typed scan walks the type
;;;; index.  These tests pin the two against each other on a multi-type store
;;;; across a close/reopen, including the inheritance case that made #219 look
;;;; like a lost-vertex bug: :VERTEX-TYPE defaults to :INCLUDE-SUBCLASSES-P T,
;;;; so a parent's walk already contains every subtype and summing parent +
;;;; subtypes double-counts.

(in-package #:graph-db/test)

(def-suite map-vertices-untyped-suite
  :description "Untyped MAP-VERTICES agrees with the per-type walks."
  :in graph-db-suite)

(in-suite map-vertices-untyped-suite)

(def-vertex mv-record ()
  ((n :type integer))
  :graph-db-integration-test)

(def-vertex mv-evidence (mv-record)
  ()
  :graph-db-integration-test)

(def-vertex mv-turning (mv-record)
  ()
  :graph-db-integration-test)

(def-vertex mv-unrelated ()
  ((n :type integer))
  :graph-db-integration-test)

(defun mv-count (graph &rest args)
  "Number of vertices GRAPH's MAP-VERTICES visits under ARGS."
  (let ((n 0))
    (apply #'map-vertices (lambda (v) (declare (ignore v)) (incf n)) graph args)
    n))

(defmacro with-mv-store ((g &key (bare 5) (evidence 7) (turning 11)) &body body)
  "Populate a fresh store with BARE MV-RECORDs, EVIDENCE MV-EVIDENCEs and
TURNING MV-TURNINGs (no MV-UNRELATEDs), close it, reopen it from disk and
bind G to the reopened graph."
  (let ((dir (gensym "DIR")) (path (gensym "PATH")) (g1 (gensym "G1")))
    `(with-temp-directory (,dir)
       (let ((,path (namestring ,dir)))
         (let ((,g1 (make-graph *integration-graph-name* ,path
                                :buffer-pool-size 1000)))
           (let ((*graph* ,g1))
             (with-transaction ()
               (dotimes (i ,bare) (make-mv-record :n i))
               (dotimes (i ,evidence) (make-mv-evidence :n i))
               (dotimes (i ,turning) (make-mv-turning :n i))))
           (close-graph ,g1 :snapshot-p nil))
         (let ((,g (open-graph *integration-graph-name* ,path)))
           (unwind-protect (let ((*graph* ,g)) ,@body)
             (ignore-errors (close-graph ,g :snapshot-p nil))
             (collect-garbage)))))))

(test untyped-map-vertices-equals-disjoint-per-type-sum
  "GH #219: after a reopen the untyped walk sees every vertex -- it equals the
sum over a DISJOINT type partition (the parent without subclasses, plus each
subtype), and equals the raw vertex-table count."
  (with-mv-store (g :bare 5 :evidence 7 :turning 11)
    (let ((untyped (mv-count g))
          (bare (mv-count g :vertex-type 'mv-record :include-subclasses-p nil))
          (evidence (mv-count g :vertex-type 'mv-evidence))
          (turning (mv-count g :vertex-type 'mv-turning))
          (unrelated (mv-count g :vertex-type 'mv-unrelated)))
      (is (= 5 bare))
      (is (= 7 evidence))
      (is (= 11 turning))
      (is (= 0 unrelated))
      (is (= 23 untyped))
      (is (= untyped (+ bare evidence turning unrelated)))
      (is (= untyped (graph-db::read-lhash-count
                      (graph-db::vertex-table g)))))))

(test typed-parent-walk-includes-subclasses-by-default
  "The shape that made GH #219 look like a defect: MV-RECORD's default walk
already contains both subtypes, so parent + subtypes over-counts the store.
:INCLUDE-SUBCLASSES-P NIL is what makes a per-type sum comparable."
  (with-mv-store (g :bare 5 :evidence 7 :turning 11)
    (let ((untyped (mv-count g))
          (parent (mv-count g :vertex-type 'mv-record))
          (bare (mv-count g :vertex-type 'mv-record :include-subclasses-p nil))
          (evidence (mv-count g :vertex-type 'mv-evidence))
          (turning (mv-count g :vertex-type 'mv-turning)))
      (is (= parent untyped))
      (is (= parent (+ bare evidence turning)))
      ;; the naive sum a caller reaches for is strictly larger
      (is (> (+ parent evidence turning) untyped)))))

(test untyped-map-vertices-scales-past-one-bucket-load
  "The same equality at a size with real lhash bucket collisions, so a
truncated bucket read or a missed overflow chain would show up."
  (with-mv-store (g :bare 300 :evidence 900 :turning 1500)
    (let ((untyped (mv-count g)))
      (is (= 2700 untyped))
      (is (= untyped
             (+ (mv-count g :vertex-type 'mv-record :include-subclasses-p nil)
                (mv-count g :vertex-type 'mv-evidence)
                (mv-count g :vertex-type 'mv-turning)))))))
