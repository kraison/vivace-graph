;;;; Stable numeric store-ids and the open-store vector (GH #169).
(in-package #:graph-db/test)

(def-suite store-registry-suite :in graph-db-suite
  :description "Store-id registry: stable, persistent, never reused.")
(in-suite store-registry-suite)

(test store-ids-are-stable-and-distinct
  "One name, one id, forever; two names never share.  Nearest wrong
implementation: a per-image counter that restarts at 1 (ids would
collide across sessions -- the persistence test below catches that
half; distinctness catches the other)."
  (with-temp-directory (sys)
    (let ((graph-db::*system-directory* (namestring sys))
          (graph-db::*store-registry* nil))
      (let ((a (store-registry-intern :sr-store-a))
            (b (store-registry-intern :sr-store-b)))
        (is (integerp a))
        (is (/= a b))
        (is (= a (store-registry-intern :sr-store-a))
            "re-interning returns the same id")
        (is (eq :sr-store-b (store-registry-name-for b)))))))

(test store-ids-survive-a-fresh-registry-open
  "The registry is persisted in the system directory; a fresh open (a
new image, simulated by clearing the cached registry) reads the same
assignments back."
  (with-temp-directory (sys)
    (let ((graph-db::*system-directory* (namestring sys))
          (graph-db::*store-registry* nil))
      (let ((id (store-registry-intern :sr-persist)))
        (setq graph-db::*store-registry* nil)
        (is (= id (store-registry-id-for :sr-persist)))
        (is (= id (store-registry-intern :sr-persist)))))))

(test store-registry-accepts-string-names
  "Graph names may be strings (cf. the strchk fixtures); the registry
must key them by content, not identity."
  (with-temp-directory (sys)
    (let ((graph-db::*system-directory* (namestring sys))
          (graph-db::*store-registry* nil))
      (let ((id (store-registry-intern (copy-seq "sr-stringly"))))
        (is (= id (store-registry-intern (copy-seq "sr-stringly"))))))))

(test store-registry-requires-a-system-directory
  (let ((graph-db::*system-directory* nil)
        (graph-db::*store-registry* nil))
    (signals graph-db:system-directory-required
      (store-registry-intern :sr-nodir))))

(test open-graph-carries-its-store-id
  "MAKE-GRAPH interns the graph-name and exposes the id on the graph;
the open-store vector maps it back; CLOSE-GRAPH clears the vector slot
but the registry keeps the assignment.  Nearest wrong implementation:
a vector slot that survives close (a stale graph object would be
returned for a closed store)."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil))
        (let* ((g (make-graph :sr-open-store (namestring dir)
                              :buffer-pool-size 1000))
               (sid (graph-db::store-id g)))
          (unwind-protect
               (progn
                 (is (integerp sid))
                 (is (eq g (svref graph-db::*store-id->graph* sid))))
            (close-graph g :snapshot-p nil))
          (is (null (svref graph-db::*store-id->graph* sid)))
          (is (= sid (store-registry-id-for :sr-open-store))
              "the assignment outlives the open"))))))

(test v8-ids-carry-the-store-tag
  "Layout pin: version nibble 8, RFC variant, and the tag readable back
from bytes 14-15.  Nearest wrong implementation: tag written but
version left 5 (ID-STORE-TAG must return NIL for it)."
  (let ((id (graph-db::gen-v8-uuid 2749)))
    (is (= 16 (length id)))
    (is (graph-db:uuid-v8-p id))
    (is (= #b10 (ldb (byte 2 6) (aref id 8))) "RFC 9562 variant")
    (is (= 2749 (graph-db:id-store-tag id)))))

(test v5-ids-have-no-store-tag
  "Legacy ids answer NIL, never a garbage tag read out of hash bytes."
  (let ((id (graph-db::gen-v5-uuid graph-db::*vertex-namespace*)))
    (is (not (graph-db:uuid-v8-p id)))
    (is (null (graph-db:id-store-tag id)))))

(test gen-ids-fall-back-to-v5-without-a-tag
  "GEN-VERTEX-ID/GEN-EDGE-ID with no (or nil) tag are byte-layout v5 --
the memory-graph/legacy path is unchanged (GH #169)."
  (is (not (graph-db:uuid-v8-p (graph-db::gen-vertex-id))))
  (is (not (graph-db:uuid-v8-p (graph-db::gen-edge-id nil)))))

(test new-nodes-in-a-store-get-tagged-ids
  "End to end: a vertex and an edge created in an open store carry ids
whose tag is that store's id.  Nearest wrong implementation: generation
still v5 everywhere (the whole point of the unit)."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil))
        (let ((g (make-graph :sr-tagged-store (namestring dir)
                             :buffer-pool-size 1000)))
          (unwind-protect
               (let (v1 v2 e)
                 (with-transaction ((graph-db::transaction-manager g))
                   (setq v1 (graph-db:make-vertex :generic nil :graph g))
                   (setq v2 (graph-db:make-vertex :generic nil :graph g))
                   (setq e (graph-db:make-edge :generic (id v1) (id v2) 1.0 nil
                                               :graph g)))
                 (is (= (graph-db::store-id g)
                        (graph-db:id-store-tag (id v1))))
                 (is (= (graph-db::store-id g)
                        (graph-db:id-store-tag (id e)))))
            (close-graph g :snapshot-p nil)))))))
