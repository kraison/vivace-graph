;;;; graph-db/algorithms -- Prolog predicate wrappers.
;;;;
;;;; Exposes the native (Mode B) algorithms as Prolog query predicates, so graph
;;;; analysis composes with the rest of the query language.  This is the #45
;;;; roadmap's "graph-native" surface (shortest_path/4 et al.).
;;;;
;;;; Each predicate is a global functor: it VAR-DEREFs its inputs, runs the
;;;; algorithm under the algorithm snapshot (these are pure reads -- untagged, so
;;;; allowed even under :effects nil), and for each solution saves the trail,
;;;; UNIFYs the output vars, FUNCALLs the continuation, then UNDO-BINDINGS.  The
;;;; "iterate all and unify" predicates (page-rank/2, degree/2, ...) work whether
;;;; the node arg is bound (acts as a filter) or unbound (enumerates).
;;;;
;;;; Nodes are vertex objects in the query, exactly as the engine's other node
;;;; predicates (is-a, outgoing-edges, ...) produce and consume them.

(in-package :graph-db)

(def-global-prolog-functor shortest-path/4 (?from ?to ?path ?cost cont)
  "(shortest-path ?From ?To ?Path ?Cost): weighted Dijkstra shortest path.  ?From
and ?To must be bound vertices; binds ?Path to the vertex list and ?Cost to the
total weight.  Fails when ?To is unreachable."
  (let ((from (var-deref ?from)) (to (var-deref ?to)))
    (when (and (typep from 'vertex) (typep to 'vertex))
      (multiple-value-bind (path cost) (shortest-path from to :graph *graph*)
        (when path
          (let ((old-trail (fill-pointer *trail*)))
            (when (and (unify ?path path) (unify ?cost cost))
              (funcall cont))
            (undo-bindings old-trail)))))))

(def-global-prolog-functor distance/3 (?from ?to ?distance cont)
  "(distance ?From ?To ?Distance): the weighted shortest-path distance from the
bound vertex ?From to ?To.  Fails when unreachable."
  (let ((from (var-deref ?from)) (to (var-deref ?to)))
    (when (and (typep from 'vertex) (typep to 'vertex))
      (multiple-value-bind (path cost) (shortest-path from to :graph *graph*)
        (when path
          (let ((old-trail (fill-pointer *trail*)))
            (when (unify ?distance cost) (funcall cont))
            (undo-bindings old-trail)))))))

(def-global-prolog-functor reachable/2 (?from ?to cont)
  "(reachable ?From ?To): succeeds once if the bound vertex ?To is reachable from
?From by a directed path."
  (let ((from (var-deref ?from)) (to (var-deref ?to)))
    (when (and (typep from 'vertex) (typep to 'vertex)
               (shortest-path from to :graph *graph* :unweighted t))
      (funcall cont))))

(def-global-prolog-functor connected-component/2 (?node ?member cont)
  "(connected-component ?Node ?Member): enumerates each ?Member in the weakly-
connected component of the bound vertex ?Node (including ?Node itself)."
  (let ((node (var-deref ?node)))
    (when (typep node 'vertex)
      (dolist (cell (distance-map node :graph *graph* :direction :both))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify ?member (car cell)) (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor degree/2 (?node ?degree cont)
  "(degree ?Node ?Degree): the total degree (in+out) of each vertex.  ?Node bound
filters to that vertex; unbound enumerates all."
  (dolist (v (all-vertices *graph*))
    (let ((old-trail (fill-pointer *trail*)))
      (when (and (unify ?node v) (unify ?degree (degree v :graph *graph*)))
        (funcall cont))
      (undo-bindings old-trail))))

(def-global-prolog-functor page-rank/2 (?node ?rank cont)
  "(page-rank ?Node ?Rank): the PageRank of each vertex.  ?Node bound filters to
that vertex; unbound enumerates all.  Recomputes PageRank per call."
  (dolist (cell (page-rank :graph *graph*))
    (let ((old-trail (fill-pointer *trail*)))
      (when (and (unify ?node (car cell)) (unify ?rank (cdr cell)))
        (funcall cont))
      (undo-bindings old-trail))))

(def-global-prolog-functor authority-value/2 (?node ?value cont)
  "(authority-value ?Node ?Value): the HITS authority score of each vertex.
?Node bound filters; unbound enumerates.  Recomputes HITS per call."
  (multiple-value-bind (auth hub) (hub-authority-values :graph *graph*)
    (declare (ignore hub))
    (dolist (cell auth)
      (let ((old-trail (fill-pointer *trail*)))
        (when (and (unify ?node (car cell)) (unify ?value (cdr cell)))
          (funcall cont))
        (undo-bindings old-trail)))))

(def-global-prolog-functor hub-value/2 (?node ?value cont)
  "(hub-value ?Node ?Value): the HITS hub score of each vertex.  ?Node bound
filters; unbound enumerates.  Recomputes HITS per call."
  (multiple-value-bind (auth hub) (hub-authority-values :graph *graph*)
    (declare (ignore auth))
    (dolist (cell hub)
      (let ((old-trail (fill-pointer *trail*)))
        (when (and (unify ?node (car cell)) (unify ?value (cdr cell)))
          (funcall cont))
        (undo-bindings old-trail)))))
