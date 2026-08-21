;;;; Type-ids are assigned from the image-level registry (GH #186).
(in-package #:graph-db/test)

(def-suite global-type-id-suite :in graph-db-suite
  :description "Type-id assignment through the system-wide registry.")
(in-suite global-type-id-suite)

;;; Declaration ORDER is chosen so that BOTH tests below fail against the
;;; per-graph counters #186 replaces -- neither may pass vacuously against
;;; the implementation it exists to reject.  Under those counters the ids
;;; would be:
;;;
;;;   store 1: SHARED-TYPE 1, TYPE-IN-ONE 2, DUAL-TYPE 3
;;;   store 2: REG-FILLER 1, TYPE-IN-TWO 2, SHARED-TYPE 3, DUAL-TYPE 4
;;;
;;; so SHARED-TYPE differs across the stores (1 vs 3) AND TYPE-IN-ONE collides
;;; with TYPE-IN-TWO (both 2).  REG-FILLER exists only to offset store 2's
;;; counter; the two properties cannot both be violated without it.
(def-vertex shared-type () ((label :type string)) :reg-store-1)
(def-vertex type-in-one () ((label :type string)) :reg-store-1)
(def-vertex dual-type   () ((label :type string)) :reg-store-1)

(def-vertex reg-filler  () ((label :type string)) :reg-store-2)
(def-vertex type-in-two () ((label :type string)) :reg-store-2)
(def-vertex shared-type () ((label :type string)) :reg-store-2)
(def-vertex dual-type   () ((label :type string)) :reg-store-2)

(defmacro with-two-test-graphs ((g1 g2 sysdir) &body body)
  "Two stores in ONE image, sharing SYSDIR as their system directory.  The
existing WITH-TEST-GRAPH binds a single hardcoded graph name and cannot
express this, which is why #186 needs its own helper."
  (let ((s (gensym)) (d1 (gensym)) (d2 (gensym)))
    ;; IGNORABLE, not the caller's (DECLARE (IGNORE ...)): BODY lands inside
    ;; an UNWIND-PROTECT's PROGN, which is not a declaration context.
    `(with-temp-directory (,s)
       (with-temp-directory (,d1)
         (with-temp-directory (,d2)
           (let* ((,sysdir (namestring ,s))
                  (graph-db::*system-directory* ,sysdir))
             (declare (ignorable ,sysdir))
             (let ((,g1 (make-graph :reg-store-1 (namestring ,d1)
                                    :buffer-pool-size 1000))
                   (,g2 (make-graph :reg-store-2 (namestring ,d2)
                                    :buffer-pool-size 1000)))
               (declare (ignorable ,g1 ,g2))
               (unwind-protect (progn ,@body)
                 (ignore-errors (close-graph ,g1 :snapshot-p nil))
                 (ignore-errors (close-graph ,g2 :snapshot-p nil))
                 (collect-garbage)))))))))

(defun %type-id-of (sym parent graph)
  (graph-db::node-type-id
   (graph-db::lookup-node-type-by-name sym parent :graph graph)))

(test two-graphs-in-one-image-share-a-symbol-s-type-id
  "The unit's entire purpose.  Before #186 each graph counted from 1, so the
same symbol got different ids in different stores and different symbols
collided on one id."
  (with-two-test-graphs (g1 g2 sysdir)
    (is (= (%type-id-of 'shared-type :vertex g1)
           (%type-id-of 'shared-type :vertex g2))
        "one symbol, one id, both stores")))

(test distinct-symbols-never-collide-across-graphs
  (with-two-test-graphs (g1 g2 sysdir)
    (is (/= (%type-id-of 'type-in-one :vertex g1)
            (%type-id-of 'type-in-two :vertex g2))
        "two symbols never share an id, even in different stores")))

(test opening-a-graph-without-a-system-directory-signals
  "The directory is mandatory as of #186: the registry has nowhere to live
without one, and a graph opened outside a system would mint ids that mean
nothing to anyone else.  Refuse rather than silently fall back to per-graph
counters -- a silent fallback is how two id regimes diverge unnoticed."
  (with-temp-directory (dir)
    (let ((graph-db::*system-directory* nil))
      (signals graph-db:system-directory-required
        (make-graph :reg-nodir (namestring dir))))))

(test opening-an-existing-graph-without-a-system-directory-signals
  "The refusal covers OPEN-GRAPH, not only MAKE-GRAPH: reopening replays the
schema and assigns ids to any type added since (#186)."
  (with-temp-directory (sysdir)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sysdir)))
        (close-graph (make-graph :reg-store-1 (namestring dir)
                                 :buffer-pool-size 1000)
                     :snapshot-p nil))
      (collect-garbage)
      (let ((graph-db::*system-directory* nil))
        (signals graph-db:system-directory-required
          (open-graph :reg-store-1 (namestring dir)))))))

(test ensure-type-registry-signals-without-a-system-directory
  "The accessor every assignment path funnels through refuses on its own.
The checks in MAKE-GRAPH/OPEN-GRAPH are early, legible ones; this is what
makes a silent per-graph fallback impossible rather than merely absent from
those two entry points."
  (let ((graph-db::*system-directory* nil))
    (signals graph-db:system-directory-required
      (graph-db::ensure-type-registry))))

(test a-class-may-be-instantiated-in-more-than-one-store
  "Closes cl-llm#20.  %CHECK-NODE-CLASS-GRAPH-UNIQUE refused this and existed
only because ids were per-graph."
  (with-two-test-graphs (g1 g2 sysdir)
    (is-true (graph-db::lookup-node-type-by-name
              'dual-type :vertex :graph g1))
    (is-true (graph-db::lookup-node-type-by-name
              'dual-type :vertex :graph g2))))

(test the-registry-is-what-the-schema-records
  "The id in a store's schema is the registry's, not a store-local number:
otherwise the two could agree by coincidence of declaration order alone."
  (with-two-test-graphs (g1 g2 sysdir)
    (let ((r (graph-db::ensure-type-registry)))
      (is (= (graph-db::registry-id-for r 'shared-type :vertex)
             (%type-id-of 'shared-type :vertex g1))
          "the store's id for a symbol is the one the registry holds"))
    (is (probe-file (merge-pathnames "type-registry.log" sysdir))
        "assignment is persisted, not merely in-memory")))
