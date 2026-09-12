;;;; Edges under claims: the spec sec.9 measurement (GH #372).  Own graph
;;;; name and own schema so no existing bench's work changes (the
;;;; generation rule, suite.lisp).  Reported, not gated: the numbers go
;;;; to the unit's issue.

(in-package #:graph-db/perf-test)

(defparameter *perf-claim-graph-name* :graph-db-perf-claims)

(eval-when (:load-toplevel :execute)
  (setf (gethash *perf-claim-graph-name* *schema-node-metadata*) nil))

(graph-db.spacetime:def-source pb-thing :graph-db-perf-claims
    ((thing-id :initarg :thing-id :accessor pb-thing-id))
  :identity     (:namespace :pb-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "perf fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(graph-db.spacetime:def-claim-classes pb-claim :graph-db-perf-claims)

(defmacro with-perf-claim-graph ((g) &body body)
  "A fresh on-disk graph holding PB-THING sources and PB-CLAIM claims."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph *perf-claim-graph-name* (namestring ,d)
                             :buffer-pool-size 4000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defun pb-key (i) (format nil "t-~D" i))

(defun pb-insert-things (n &key (batch 500))
  (let ((i 0))
    (loop while (< i n) do
      (graph-db:with-transaction ()
        (dotimes (k (min batch (- n i)))
          (make-pb-thing :thing-id (pb-key i))
          (incf i))))))

(defun pb-insert-claims (m n &key (batch 500))
  "M binary claims over N things: claim j relates thing j mod N to
thing (j*7+1) mod N with one of three relations."
  (let ((j 0))
    (loop while (< j m) do
      (graph-db:with-transaction ()
        (dotimes (k (min batch (- m j)))
          (make-pb-claim-binary
           :subject-namespace :pb-things :subject-key (pb-key (mod j n))
           :object-namespace :pb-things
           :object-key (pb-key (mod (1+ (* 7 j)) n))
           :relation (aref #("likes" "knows" "cites") (mod j 3))
           :producer "perf" :standing :inferred)
          (incf j))))))

(defun pb-two-hop-related (g key)
  "Objects two current hops from KEY through RELATED/3, under the
guarded runner (the consumer's read path)."
  (length
   (nth-value 1
     (graph-db.query:run-guarded-prolog
      (format nil "(is-a ?s pb-thing) (node-slot-value ?s thing-id ~S)
                   (related ?s ?r ?o) (related ?o ?r2 ?o2)" key)
      g :format :raw :limit 500))))

(defun pb-two-hop-touching (g key)
  "The same neighbourhood through CLAIMS-TOUCHING plus RESOLVE-ENDPOINT
per hop -- the first implementation's read path."
  (let ((count 0))
    (dolist (c (graph-db.spacetime:claims-touching
                g 'pb-claim :pb-things key :role :subject :current t))
      (let ((o (graph-db.spacetime:resolve-endpoint
                (graph-db.spacetime:claim-object-namespace c)
                (graph-db.spacetime:claim-object-key c))))
        (when o
          (dolist (c2 (graph-db.spacetime:claims-touching
                       g 'pb-claim :pb-things (pb-thing-id o)
                       :role :subject :current t))
            (when (graph-db.spacetime:resolve-endpoint
                   (graph-db.spacetime:claim-object-namespace c2)
                   (graph-db.spacetime:claim-object-key c2))
              (incf count))))))
    count))

(defun bench-claim-linking ()
  "Spec sec.9 (GH #372): claim write cost with linking off vs on, the
backfill sweep's cost, and a two-hop read through RELATED/3 versus
CLAIMS-TOUCHING + RESOLVE-ENDPOINT.  Same N/M/K on both sides."
  (let ((n (scale 2000)) (m (scale 4000)) (k (scale 200)))
    ;; 1. write cost, linking OFF, then the backfill on that store
    (with-perf-claim-graph (g)
      (pb-insert-things n)
      (let ((graph-db.spacetime:*link-claims-at-write* nil))
        (timed-ops ("claim-writes-unlinked" m)
          (pb-insert-claims m n)))
      (let (linked)
        (timed-seconds ("claim-sweep-backfill")
          (setf linked (graph-db.spacetime:link-claim-endpoints g)))
        (record "claim-sweep-backfill-linked" :edges linked)))
    ;; 2. write cost, linking ON, then the two read paths on that store
    (with-perf-claim-graph (g)
      (pb-insert-things n)
      (timed-ops ("claim-writes-linked" m)
        (pb-insert-claims m n))
      (let ((keys (loop for i below k collect (pb-key (* i 3))))
            t-related t-touching)
        (setf t-related
              (timed-ops ("two-hop-related" k)
                (dolist (key keys) (pb-two-hop-related g key))))
        (setf t-touching
              (timed-ops ("two-hop-claims-touching" k)
                (dolist (key keys) (pb-two-hop-touching g key))))
        (record "two-hop-speedup"
                :ratio (if (zerop t-related)
                           0
                           (float-3 (/ t-touching t-related))))))))
