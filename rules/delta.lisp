;;;; rules/delta.lisp -- a committed transaction's claim writes as a
;;;; weighted delta (forward-rules design §3, #456).

(in-package #:graph-db.rules)

(defun %claim-node-p (node)
  "NODE is a claim of some registered family."
  (and (graph-db::node-p node)
       (loop for family being the hash-values
               of graph-db.spacetime::*claim-families*
             thereis (typep node (graph-db.spacetime:claim-family-parent
                                  family)))))

(defun claim-delta (writes graph)
  "WRITES, a committed transaction's (GRAPH-DB::WRITES TX), as an EQUAL
hash relation -> list of (claim . weight), spec §3's table: a create
is +1; a delete -1 (the old version); an update that closes the
transaction period is -1, one that re-opens it +1, any other -1 of the
old version and +1 of the new.  A claim created and deleted in one
transaction nets to nothing and is absent.  GRAPH is the store the
writes belong to, unused for now."
  (declare (ignore graph))
  (let ((creates (make-hash-table :test 'equalp))
        (updates (make-hash-table :test 'equalp))
        (delta (make-hash-table :test 'equal)))
    (dolist (w writes)
      (let ((node (graph-db::node w)))
        (when (%claim-node-p node)
          (if (typep w 'graph-db::tx-create)
              (setf (gethash (graph-db:id node) creates) node)
              (setf (gethash (graph-db:id node) updates) w)))))
    (flet ((note (claim weight)
             (push (cons claim weight)
                   (gethash (graph-db.spacetime:claim-relation claim)
                            delta))))
      (maphash (lambda (id node)
                 (let ((u (gethash id updates)))
                   (cond ((null u) (note node 1))
                         ((typep u 'graph-db::tx-delete))
                         (t (note (graph-db::node u) 1)))))
               creates)
      (maphash (lambda (id u)
                 (unless (gethash id creates)
                   (let ((old (graph-db::old-node u))
                         (new (graph-db::node u)))
                     (cond ((typep u 'graph-db::tx-delete)
                            (note old -1))
                           (t
                            (let ((was (graph-db.spacetime:claim-current-p
                                        old))
                                  (now (graph-db.spacetime:claim-current-p
                                        new)))
                              (cond ((and was (not now)) (note old -1))
                                    ((and now (not was)) (note new 1))
                                    (t (note old -1) (note new 1)))))))))
               updates))
    delta))

(defun delta-touches-p (compiled delta)
  "DELTA names a relation COMPILED reads, positively or under a NOT."
  (let ((reads (compiled-rule-reads compiled))
        (negative (compiled-rule-negative-reads compiled)))
    (loop for rel being the hash-keys of delta
          thereis (or (eq reads :any)
                      (member rel reads :test #'string=)
                      (eq negative :any)
                      (member rel negative :test #'string=)))))
