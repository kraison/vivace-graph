;;;; Endpoint resolution: (namespace, external key) -> node.
;;;;
;;;; The forward direction of the pair #131 started -- CLAIMS-TOUCHING is the
;;;; inverse and needs no cross-graph read.  This one does, which is why it
;;;; refuses to run inside a read-write transaction (GH #132, design §4).

(in-package #:graph-db.spacetime)

(defun resolve-endpoint (namespace key)
  "The node in NAMESPACE whose external key is KEY, or NIL.

Signals UNKNOWN-NAMESPACE when nothing is registered under NAMESPACE, and
AMBIGUOUS-ENDPOINT when two classes both answer -- an external key must be
unique within its namespace, and returning the first would make the answer
depend on class-definition order (design §4.2).

Must NOT be called inside a read-write transaction: resolution can cross
graphs, and cross-graph reads are legal only from a read-only snapshot or
outside a transaction (design §4.1)."
  ;; *TRANSACTION* IS the read-write transaction; read-only snapshots live
  ;; in *READ-SNAPSHOTS*, keyed by graph, and never bind this.  So a bound
  ;; *TRANSACTION* is exactly the illegal case (design §4.1).
  (when graph-db:*transaction*
    (error 'resolution-in-transaction :namespace namespace :key key))
  (let ((hits '())
        (classes '()))
    (dolist (class (namespace-sources namespace))
      (let* ((facets (source-contract class))
             ;; Each class names its own graph (SOURCE-FACETS-GRAPH); the
             ;; namespace keyword is not a graph name, so it cannot be
             ;; looked up once outside this loop (design §4).
             (graph (graph-db:lookup-graph (source-facets-graph facets)))
             (slot (getf (source-facets-identity facets) :key-slot))
             (found (graph-db:index-lookup graph class (list slot) key)))
        (when found
          (push class classes)
          (setf hits (append hits found)))))
    (when (cdr classes)
      (error 'ambiguous-endpoint :namespace namespace :key key
                                 :classes classes))
    (first hits)))

(defparameter +disclosure-classes+
  '(:public :internal :restricted :secret)
  "Least to most restricted.  A class outside this list -- including :NONE --
is treated as more restricted than every member (design §3.2).")

(defun %disclosure-rank (class)
  "CLASS's position, or NIL when unrecognised."
  (position class +disclosure-classes+))

(defun source-disclosable-p (class clearance)
  "True when a source's declared disclosure CLASS may be disclosed at
CLEARANCE.  FAIL-CLOSED: an unrecognised CLASS or CLEARANCE yields NIL, so
the unknown case withholds rather than releases.  The substrate never calls
this itself -- enforcement belongs to whoever reads or exports (design
§3.2).  Not to be confused with replication.lisp's DISCLOSABLE-P, the
peer-replication export filter, which is fail-OPEN and shaped differently
(graph, vertex, device-scope)."
  (let ((c (%disclosure-rank class))
        (k (%disclosure-rank clearance)))
    (and c k (<= c k))))
