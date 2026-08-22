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
