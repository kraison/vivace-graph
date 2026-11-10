;;;; Graph predicates built on the engine (GH #123, design
;;;; docs/superpowers/specs/2026-11-10-aggregation-graph-predicates-design.md
;;;; D2, D3): reachable/3, a tabled closure over one edge type, and
;;;; k-hop/4, the vertices exactly K hops out.  shortest-path/4 and
;;;; its family live in graph-db/algorithms (manual Chapter 14).

(in-package #:graph-db)

(def-global-prolog-functor %closure-args/2 (from type cont)
  "Internal: reachable/3's first goal.  Succeeds when FROM is a vertex
and TYPE an edge-type symbol; otherwise an instantiation error naming
reachable/3, since outgoing-edges/4 would error on an unbound start
and match any type with both ends bound (design D2)."
  (let ((from (var-deref from)) (type (var-deref type)))
    (if (and (vertex-p from) (symbolp type) type)
        (funcall cont)
        (error 'prolog-error
               :reason (format nil "reachable/3 needs a bound start ~
vertex and a bound edge type; got ~S and ~S" from type)
               :ball (instantiation-error-ball)))))

(def-global-prolog-functor k-hop/4 (from k to type cont)
  "(k-hop From K To Type): the distinct vertices exactly K TYPE-edges
out from the vertex FROM, by a level breadth-first expansion with a
visited set; K = 0 is FROM itself.  FROM and K must be bound (an
instantiation error), K a non-negative integer (a type error); an
unbound TYPE fails (design D3)."
  (let ((from (var-deref from)) (k (var-deref k)) (type (var-deref type)))
    (cond ((or (var-p from) (var-p k))
           (error 'prolog-error
                  :reason "k-hop/4 needs a bound start vertex and a bound K"
                  :ball (instantiation-error-ball)))
          ((not (vertex-p from))
           (error 'prolog-error
                  :reason (format nil "k-hop/4: ~S is not a vertex" from)
                  :ball (type-error-ball :vertex from)))
          ((not (and (integerp k) (>= k 0)))
           (error 'prolog-error
                  :reason (format nil "k-hop/4: K must be a non-negative ~
integer, not ~S" k)
                  :ball (type-error-ball :integer k)))
          ((not (and (symbolp type) type)) nil)
          (t
           (let ((seen (make-hash-table :test 'equalp))
                 (frontier (list from)))
             (setf (gethash (id from) seen) t)
             (dotimes (level k)
               (declare (ignorable level))
               (let ((next '()))
                 (dolist (v frontier)
                   (map-edges (lambda (edge)
                                (let ((id (to edge)))
                                  (unless (gethash id seen)
                                    (setf (gethash id seen) t)
                                    (push (lookup-vertex id) next))))
                              *graph* :vertex v :direction :out
                              :edge-type type))
                 (setf frontier (nreverse next))))
             (dolist (v frontier)
               (let ((old-trail (fill-pointer *trail*)))
                 (when (unify to v) (funcall cont))
                 (undo-bindings old-trail))))))))

;;; reachable/3: two clauses in the GRAPH-DB package, left-recursive
;;; (one table per start, constant stack: tabling design D8), tabled
;;; at load.  A query in any package resolves the name here, as it
;;; resolves is-a.
(let ((*package* (find-package '#:graph-db)))
  (<- (reachable ?a ?b ?type) (%closure-args ?a ?type)
                              (outgoing-edges ?a ?type ?e ?b))
  (<- (reachable ?a ?c ?type) (reachable ?a ?b ?type)
                              (outgoing-edges ?b ?type ?e ?c))
  (table-functor 'reachable 3))
