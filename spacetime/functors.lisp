;;;; Prolog goals over claim topology (GH #369, spec sec.6.3).
;;;;
;;;; Both resolve endpoints with LOOKUP-VERTEX on the CLAIM's graph
;;;; (*GRAPH*, which the guarded runner binds): a caller-resolved
;;;; cross-store endpoint is linked but does not unify here; #368 is
;;;; the unit that changes that (spec sec.8).  Unary claims have no
;;;; object and are not solutions of either goal; SUBJECT-OF/2 reaches
;;;; them.

(in-package #:graph-db.spacetime)

(defun %claim-linked-endpoint (claim type graph)
  "The vertex CLAIM's TYPE edge points at, in GRAPH only; NIL when
unlinked or when the endpoint lives in another store."
  (let ((e (first (graph-db:outgoing-edges claim :graph graph
                                                 :edge-type type))))
    (when e (graph-db:lookup-vertex (graph-db:to e) :graph graph))))

(defun %solve-claims (claim subject relation object require-current cont)
  "The engine behind RELATED/3 and CLAIMED/4.  Arguments are already
VAR-DEREFed; CLAIM is NIL for RELATED/3.  The bound endpoint drives the
scan; with only RELATION bound, every family's CLAIM-RELATION index;
with nothing bound, QUERY-PRECONDITION-ERROR (the guard renders it as a
refusal), as an unbounded generator is refused elsewhere."
  (let ((graph graph-db:*graph*))
    (labels ((try (c)
               (when (and (%binary-claim-p c)
                          (or (not require-current) (claim-current-p c)))
                 (let ((s (%claim-linked-endpoint c 'subject-of graph))
                       (o (%claim-linked-endpoint c 'object-of graph)))
                   (when (and s o)
                     (let ((old-trail (fill-pointer graph-db:*trail*)))
                       (when (and (graph-db:unify subject s)
                                  (graph-db:unify relation
                                                  (claim-relation c))
                                  (graph-db:unify object o)
                                  (or (null claim)
                                      (graph-db:unify claim c)))
                         (funcall cont))
                       (graph-db:undo-bindings old-trail))))))
             (claims-into (vertex type)
               (graph-db:map-edges
                (lambda (e)
                  (let ((c (graph-db:lookup-vertex (graph-db:from e)
                                                    :graph graph)))
                    (when c (try c))))
                graph :vertex vertex :direction :in :edge-type type)))
      (cond ((not (graph-db::var-p subject))
             (when (graph-db::vertex-p subject)
               (claims-into subject 'subject-of)))
            ((not (graph-db::var-p object))
             (when (graph-db::vertex-p object)
               (claims-into object 'object-of)))
            ((stringp relation)
             (maphash (lambda (parent family)
                        (declare (ignore family))
                        (dolist (c (handler-case
                                       (graph-db:index-lookup
                                        graph parent '(relation) relation)
                                     (graph-db:query-precondition-error ()
                                       nil)))
                          (try c)))
                      *claim-families*))
            (t (error 'graph-db:query-precondition-error
                      :reason
                      (format nil "related/claimed: bind the subject, ~
the object, or the relation (a string)")))))))

(graph-db:def-global-prolog-functor related/3 (subject relation object cont)
  "(related ?subject ?relation ?object): SUBJECT and OBJECT are the
endpoint nodes of a CURRENT binary claim, RELATION its canonical string.
Any argument may be unbound; the bound endpoint drives the scan, a bound
relation alone rides the CLAIM-RELATION index, nothing bound is refused.
Linked claims only; endpoints are looked up in the claim's graph, so a
cross-store endpoint does not unify (GH #369, spec sec.6.3, sec.8)."
  (%solve-claims nil (graph-db:var-deref subject)
                 (graph-db:var-deref relation)
                 (graph-db:var-deref object) t cont))

(graph-db:def-global-prolog-functor claimed/4 (claim subject relation object
                                                cont)
  "(claimed ?claim ?subject ?relation ?object): as RELATED/3 with the
claim node exposed and NO currency filter -- retracted claims answer,
for history and for reading provenance off the claim in the same query.
Same scan rules and the same cross-store caveat (GH #369, spec sec.6.3)."
  (%solve-claims (graph-db:var-deref claim) (graph-db:var-deref subject)
                 (graph-db:var-deref relation)
                 (graph-db:var-deref object) nil cont))
