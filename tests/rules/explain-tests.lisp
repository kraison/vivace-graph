;;;; tests/rules/explain-tests.lisp -- explain-goals' account of claim/7's
;;;; routes, and that it agrees with the route the goal then takes
;;;; (GH #124, tracer/explain design D3).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defun ter-path (goal &optional before)
  "EXPLAIN-GOALS' path for GOAL after the goals BEFORE."
  (getf (car (last (graph-db:explain-goals '(?c) (append before (list goal)))))
        :path))

(defparameter *ter-routes*
  '(("subject-relation index" (claim ?c rt-claim "host" "h1" "runs" "app" ?o))
    ("subject index" (claim ?c rt-claim "host" "h1" ?r ?ons ?o))
    ("object index" (claim ?c rt-claim "host" ?s "runs" "app" "web"))
    ("names no keyword" (claim ?c rt-claim "never-recorded" "x" ?r ?ons ?o))
    ("vocabulary route, subject" (claim ?c rt-claim "host" ?k ?r ?ons ?o))
    ("vocabulary route, object" (claim ?c rt-claim ?sns ?k ?r "app" ?o))
    ("family walk" (claim ?c rt-claim ?sns ?k ?r ?ons ?o)))
  "docs/rules.md's route table, as explain-goals words it.")

(test ter-explain-names-every-claim-route
  (with-rules-graph (g)
    (seed g)
    (loop for (words goal) in *ter-routes*
          do (is (search words (ter-path goal)) "~A for ~S" words goal))
    (is (search "that claim"
                (ter-path '(claim ?c rt-claim ?s ?k ?r ?ons ?o)
                          '((claim ?c rt-claim "host" "h1" "runs" "app" ?o)))))
    (is (search "by value at run time"
                (ter-path '(claim ?c rt-claim ?ns "h1" ?r ?ons ?o)
                          '((= ?ns "host")))))))

(test ter-explain-and-the-goal-agree-on-the-route
  "The route explain-goals names is the one %CLAIM-GOAL switches on
when the goal runs (design D3)."
  (with-rules-graph (g)
    (seed g)
    (loop for (words goal) in *ter-routes*
          do (setf graph-db::*last-claim-route* nil)
             (graph-db:with-prolog-ports ((port f args depth)
                                          (declare (ignore port f args depth))
                                          nil)
               (eval `(select () (?c) ,goal)))
             (is (search (graph-db::%claim-route-string
                          graph-db::*last-claim-route*)
                         (ter-path goal))
                 "~S: ran ~S, explained ~S" goal
                 graph-db::*last-claim-route* (ter-path goal)))))
