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
