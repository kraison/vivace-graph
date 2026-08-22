;;;; The tagged-id resolver and the detached-read semantics (GH #169).
(in-package #:graph-db/test)

(def-suite store-resolver-suite :in graph-db-suite
  :description "O(1) v8 resolution; v5 scan; markers for the detached.")
(in-suite store-resolver-suite)

;; A typed edge for the cross-store traversal test.  :GENERIC edges are
;; type-id 0, and MAP-EDGES' vertex-adjacency scan always excludes type 0
;; (edge.lisp: "Generic, type-0 edges appear only in the untyped scan") --
;; so TRAVERSE never sees a :GENERIC edge at all.  A real edge type is
;; required to exercise TRAVERSE (task-3 correction to the brief, which
;; used :GENERIC and would see zero edges regardless of the resolver).
(eval-when (:load-toplevel :execute)
  (setf (gethash :rsv-store-1 *schema-node-metadata*) nil))

(def-edge rsv-edge () () :rsv-store-1)

(defmacro with-two-stores ((g1 g2 sys) &body body)
  "Two open disk stores under one system directory."
  (let ((d1 (gensym)) (d2 (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,d1)
         (with-temp-directory (,d2)
           (let ((graph-db::*system-directory* (namestring ,sys))
                 (graph-db::*store-registry* nil))
             (let ((,g1 (make-graph :rsv-store-1 (namestring ,d1)
                                    :buffer-pool-size 1000))
                   (,g2 (make-graph :rsv-store-2 (namestring ,d2)
                                    :buffer-pool-size 1000)))
               (unwind-protect (progn ,@body)
                 (ignore-errors (close-graph ,g1 :snapshot-p nil))
                 (ignore-errors (close-graph ,g2 :snapshot-p nil))
                 (collect-garbage)))))))))

(test v8-resolution-is-by-tag
  "A v8 id resolves to its OWN open store without consulting the store
it was found in.  Nearest wrong implementation: resolve everything via
*GRAPH* (the pre-#169 assumption)."
  (with-two-stores (g1 g2 sys)
    (let (v)
      (with-transaction ((graph-db::transaction-manager g2))
        (setq v (graph-db:make-vertex :generic nil :graph g2)))
      (multiple-value-bind (graph status sid)
          (graph-db:resolve-node-graph (id v))
        (is (eq g2 graph))
        (is (eq :resolved status))
        (is (= (graph-db::store-id g2) sid))))))

(test v5-resolution-scans-open-stores
  "A legacy v5 id resolves by trying each open store's vertex table --
slower, correct, and the reason there is no flag day (GH #169)."
  (with-two-stores (g1 g2 sys)
    (let ((vid (graph-db::gen-vertex-id)))   ; untagged v5
      (with-transaction ((graph-db::transaction-manager g2))
        (graph-db:make-vertex :generic nil :id vid :graph g2))
      (multiple-value-bind (graph status sid)
          (graph-db:resolve-node-graph vid)
        (declare (ignore sid))
        (is (eq g2 graph))
        (is (eq :resolved status))))))

(test detached-store-yields-a-marker
  "A v8 id whose store is REGISTERED but not OPEN is detached: known,
findable, offline.  Incidental access gets the marker, never a raw NIL
(indistinguishable from 'no such node') and never an error.  Nearest
wrong implementation: return NIL (silent skipping, rejected by D8)."
  (with-two-stores (g1 g2 sys)
    (let (v)
      (with-transaction ((graph-db::transaction-manager g2))
        (setq v (graph-db:make-vertex :generic nil :graph g2)))
      (close-graph g2 :snapshot-p nil)
      (let ((r (graph-db:lookup-vertex-anywhere (id v))))
        (is (graph-db:unresolved-node-p r))
        (is (equalp (id v) (graph-db:unresolved-node-id r)))
        (is (eq :rsv-store-2 (graph-db:unresolved-node-store-name r)))))))

(test explicit-access-to-a-detached-store-signals
  "The caller who ASKS for the detached store gets the error, per D8's
explicit-versus-incidental split."
  (with-two-stores (g1 g2 sys)
    (let (v)
      (with-transaction ((graph-db::transaction-manager g2))
        (setq v (graph-db:make-vertex :generic nil :graph g2)))
      (close-graph g2 :snapshot-p nil)
      (signals graph-db:store-detached-error
        (graph-db:lookup-vertex-anywhere (id v) :if-detached :error)))))

(test unknown-tag-is-unknown-not-detached
  "A v8 id with a tag the registry never assigned is :UNKNOWN -- the
resolver must not fabricate a detached store out of it."
  (with-two-stores (g1 g2 sys)
    (let ((id (graph-db::gen-v8-uuid 4000)))
      (multiple-value-bind (graph status sid)
          (graph-db:resolve-node-graph id)
        (declare (ignore sid))
        (is (null graph))
        (is (eq :unknown status))))))

(test traversal-reaches-across-open-stores-and-marks-detached
  "A cross-store edge: with both stores open, TRAVERSE surfaces the
far-end vertex in its results (it is not walked further -- cross-store
continuation is #170+ work); with the far store closed, the results
carry the unresolved marker instead.  Nearest wrong implementation:
LOOKUP-VERTEX in the local store only -- the far end silently vanishes
from results in BOTH cases.

CORRECTED (task-3), two brief defects:
1. The brief's TRAVERSE calls omitted :EDGE-TYPE.  TRAVERSE's established
   contract (tests/traverse-tests.lisp,
   TRAVERSE-WITHOUT-EDGE-TYPE-RETURNS-NIL) is that with no :EDGE-TYPE
   nothing is ever collected -- (TYPEP edge NIL) is always false.
2. The brief used a :GENERIC edge (MAKE-EDGE :GENERIC ...), but generic
   edges are type-id 0, and MAP-EDGES' vertex-adjacency scan always
   excludes type 0 (edge.lisp: \"Generic, type-0 edges appear only in the
   untyped scan\") -- TRAVERSE would see zero edges regardless of
   :EDGE-TYPE.  RSV-EDGE (defined above) is a real registered type."
  (with-two-stores (g1 g2 sys)
    (let (v1 v2)
      (with-transaction ((graph-db::transaction-manager g1))
        (setq v1 (graph-db:make-vertex :generic nil :graph g1)))
      (with-transaction ((graph-db::transaction-manager g2))
        (setq v2 (graph-db:make-vertex :generic nil :graph g2)))
      (with-transaction ((graph-db::transaction-manager g1))
        (make-rsv-edge :from v1 :to v2 :graph g1))
      (let ((results (traverse v1 :graph g1 :direction :out
                                :edge-type 'rsv-edge)))
        (is (find-if (lambda (r)
                       (and (graph-db::vertex-p r) (equalp (id v2) (id r))))
                     results)
            "open far store: the vertex itself is in the results"))
      (close-graph g2 :snapshot-p nil)
      (let ((results (traverse v1 :graph g1 :direction :out
                                :edge-type 'rsv-edge)))
        (is (find-if (lambda (r)
                       (and (graph-db:unresolved-node-p r)
                            (equalp (id v2)
                                    (graph-db:unresolved-node-id r))))
                     results)
            "closed far store: the marker is in the results")))))

(test backup-includes-and-warns-on-a-dangling-cross-store-edge
  "D12/sec.7: BACKUP writes the edge (connectivity survives) and signals
DANGLING-EDGE-WARNING naming the offline endpoint.  Nearest wrong
implementations: omit the edge silently, or refuse the backup.

CORRECTED (task-4), controller notes:
1. WITH-TRANSACTION takes the transaction manager, not :GRAPH.
2. The substring-search assertion from the brief is replaced by
   reading the backup file's forms back and matching a :E record's
   :ID against E's id with EQUALP -- stronger than a printed-token
   search, and avoids depending on STRING-ID's exact format."
  (with-two-stores (g1 g2 sys)
    (with-temp-directory (bdir)
      (let (v1 v2 e)
        (with-transaction ((graph-db::transaction-manager g1))
          (setq v1 (graph-db:make-vertex :generic nil :graph g1)))
        (with-transaction ((graph-db::transaction-manager g2))
          (setq v2 (graph-db:make-vertex :generic nil :graph g2)))
        (with-transaction ((graph-db::transaction-manager g1))
          (setq e (graph-db:make-edge :generic (id v1) (id v2) 1.0 nil
                                       :graph g1)))
        (close-graph g2 :snapshot-p nil)
        (let ((file (merge-pathnames "dangle.backup" bdir))
              (warned nil))
          (handler-bind ((graph-db:dangling-edge-warning
                           (lambda (w)
                             (setq warned w)
                             (muffle-warning w))))
            (graph-db::backup g1 (namestring file)))
          (is-true warned "the backup must warn")
          (is (equalp (id e) (graph-db:dangling-edge-id warned)))
          (is-true (probe-file file))
          (let* ((*readtable* graph-db::*restore-readtable*)
                 (forms (with-open-file (in file)
                          (loop for form = (read in nil :eof)
                                until (eq form :eof)
                                collect form))))
            (is-true (find-if (lambda (f)
                                 (and (eq :e (first f))
                                      (equalp (id e) (getf (cddr f) :id))))
                               forms)
                      "the dangling edge is IN the backup")))))))

(test traverse-gate-is-node-graph-not-tag
  "TRAVERSE's same-store enqueue gate must trust NODE-GRAPH (stamped by
every read), not a tag-vs-STORE-ID comparison: a store reopened under a
DIFFERENT system directory gets a different registry id than the one
baked into its own already-minted v8 ids, so the tag mismatches even
though every vertex IS local.  The tag-based gate (pre-#209) truncates
BFS after depth 1 here; the fix walks the whole chain.  Nearest wrong
implementation: the reverted tag-equality gate (see the ablation notes
in the fix-wave report)."
  (with-temp-directory (sys1)
    (with-temp-directory (d)
      (let (v1id v2id v3id)
        (let ((graph-db::*system-directory* (namestring sys1))
              (graph-db::*store-registry* nil))
          (let ((g (make-graph :rsv-store-1 (namestring d)
                                :buffer-pool-size 1000)))
            (unwind-protect
                 (let (v1 v2 v3)
                   (with-transaction ((graph-db::transaction-manager g))
                     (setq v1 (graph-db:make-vertex :generic nil :graph g))
                     (setq v2 (graph-db:make-vertex :generic nil :graph g))
                     (setq v3 (graph-db:make-vertex :generic nil :graph g))
                     (make-rsv-edge :from v1 :to v2 :graph g)
                     (make-rsv-edge :from v2 :to v3 :graph g))
                   (setq v1id (id v1) v2id (id v2) v3id (id v3)))
              (ignore-errors (close-graph g :snapshot-p nil))
              (collect-garbage))))
        ;; Reopen under a DIFFERENT, otherwise-unrelated system directory.
        ;; Consume id 1 with a dummy name first so :RSV-STORE-1 mints id 2
        ;; here -- deliberately mismatching the tag (1) already baked into
        ;; v1id/v2id/v3id, rather than relying on both registries handing
        ;; out 1 by coincidence.
        (with-temp-directory (sys2)
          (let ((graph-db::*system-directory* (namestring sys2))
                (graph-db::*store-registry* nil))
            (graph-db::store-registry-intern "rsv-dummy-consumer")
            (let ((g2 (open-graph :rsv-store-1 (namestring d)
                                  :buffer-pool-size 1000)))
              (unwind-protect
                   (let* ((v1 (lookup-vertex v1id :graph g2))
                          (results (traverse v1 :graph g2 :direction :out
                                             :edge-type 'rsv-edge)))
                     (is (find-if (lambda (r) (and (graph-db::vertex-p r)
                                                    (equalp v2id (id r))))
                                  results)
                         "depth 1 (v2) is reached")
                     (is (find-if (lambda (r) (and (graph-db::vertex-p r)
                                                    (equalp v3id (id r))))
                                  results)
                         "depth 2 (v3) is reached -- fails under the tag gate"))
                (ignore-errors (close-graph g2 :snapshot-p nil))
                (collect-garbage)))))))))

(test register-open-store-signals-on-slot-collision
  "%REGISTER-OPEN-STORE must not silently overwrite an occupied
open-store-vector slot: two system directories opened in the same image
can each mint id 1 for their own first store.  Nearest wrong
implementation: the pre-#209 unconditional (SETF SVREF ...), which would
make RESOLVE-NODE-GRAPH answer id 1 with the wrong graph from then on."
  (with-temp-directory (sys1)
    (with-temp-directory (d1)
      (let ((graph-db::*system-directory* (namestring sys1))
            (graph-db::*store-registry* nil))
        (let ((g1 (make-graph :rsv-collide-1 (namestring d1)
                              :buffer-pool-size 1000)))
          (unwind-protect
               (with-temp-directory (sys2)
                 (let ((graph-db::*system-directory* (namestring sys2))
                       (graph-db::*store-registry* nil))
                   ;; A hand-built second GRAPH avoids driving a full
                   ;; MAKE-GRAPH into the collision mid-open (which would
                   ;; leave heap/lhash files open with no clean unwind path).
                   (let ((g2 (make-instance 'graph-db::graph
                                           :graph-name :rsv-collide-2)))
                     (signals error (graph-db::%register-open-store g2)))))
            (ignore-errors (close-graph g1 :snapshot-p nil))
            (collect-garbage)))))))

(test resolve-node-graph-accepts-a-string-id
  "RESOLVE-NODE-GRAPH accepts a 32-hex-digit string id, like LOOKUP-VERTEX
does, and resolves it the same as the raw array (GH #209)."
  (with-two-stores (g1 g2 sys)
    g1
    (let (v)
      (with-transaction ((graph-db::transaction-manager g2))
        (setq v (graph-db:make-vertex :generic nil :graph g2)))
      (multiple-value-bind (graph status)
          (graph-db:resolve-node-graph (string-id (id v)))
        (is (eq g2 graph))
        (is (eq :resolved status))))))

(test reattach-after-reopen-resolves-to-the-new-graph
  "The detached->reattached transition: once the store is OPEN again
(a fresh GRAPH object, same directory), RESOLVE-NODE-GRAPH must answer
:RESOLVED with the NEW graph, and LOOKUP-VERTEX-ANYWHERE must return the
live vertex again -- not a marker, not the old (now-closed) graph."
  (with-two-stores (g1 g2 sys)
    g1
    (let (v vid loc)
      (with-transaction ((graph-db::transaction-manager g2))
        (setq v (graph-db:make-vertex :generic nil :graph g2)))
      (setq vid (id v))
      (setq loc (namestring (graph-db::location g2)))
      (close-graph g2 :snapshot-p nil)
      (is (eq :detached (nth-value 1 (graph-db:resolve-node-graph vid))))
      (let ((g2b (open-graph :rsv-store-2 loc :buffer-pool-size 1000)))
        (unwind-protect
             (multiple-value-bind (graph status)
                 (graph-db:resolve-node-graph vid)
               (is (eq :resolved status))
               (is (eq g2b graph))
               (is-true (graph-db::vertex-p
                         (graph-db:lookup-vertex-anywhere vid))))
          (ignore-errors (close-graph g2b :snapshot-p nil)))))))

(test clean-backups-never-warn-dangling
  "No-false-positive canary for DANGLING-EDGE-WARNING (review fix,
GH #169): one graph, no cross-store edges anywhere -- a live-live
edge, and an edge whose far endpoint is soft-deleted (but still in
THIS graph's own vertex table) -- must never warn.  Pins the exact
regression the risk names: filtering an endpoint out by DELETED-P, or
treating ANY RESOLVE-NODE-GRAPH miss as dangling instead of only
:DETACHED / :RESOLVED-elsewhere, would both turn this green.  Collects
DANGLING-EDGE-WARNING specifically, not WARNING generally -- other
machinery (e.g. spatial-index coarsening) may legitimately warn during
backup."
  (with-temp-directory (sys)
    (with-temp-directory (d)
      (with-temp-directory (bdir)
        (let ((graph-db::*system-directory* (namestring sys))
              (graph-db::*store-registry* nil))
          (let ((g (make-graph :rsv-store-1 (namestring d)
                                :buffer-pool-size 1000)))
            (unwind-protect
                 (let (v1 v2 v3)
                   (with-transaction ((graph-db::transaction-manager g))
                     (setq v1 (graph-db:make-vertex :generic nil :graph g))
                     (setq v2 (graph-db:make-vertex :generic nil :graph g))
                     (setq v3 (graph-db:make-vertex :generic nil :graph g))
                     (graph-db:make-edge :generic (id v1) (id v2) 1.0 nil
                                          :graph g)
                     (graph-db:make-edge :generic (id v1) (id v3) 1.0 nil
                                          :graph g))
                   (with-transaction ((graph-db::transaction-manager g))
                     (mark-deleted (lookup-vertex (id v3) :graph g)))
                   (let ((file (merge-pathnames "clean.backup" bdir))
                         (seen nil))
                     (handler-bind ((graph-db:dangling-edge-warning
                                      (lambda (w)
                                        (push w seen)
                                        (muffle-warning w))))
                       (graph-db::backup g (namestring file)
                                         :include-deleted-p t))
                     (is (null seen)
                         "a clean single-store backup must never warn")))
              (ignore-errors (close-graph g :snapshot-p nil))
              (collect-garbage))))))))
