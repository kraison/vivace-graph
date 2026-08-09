;;;; SCOPE-NODE-SET: reference-class vertices are TERMINAL in the walk.
;;;;
;;;; The reference-set ships global reference data BY CLASS (e.g. the whole ordnance
;;;; catalogue) so a device can classify finds locally, while that data's hub-only
;;;; neighbours (ordnance-detail, country) stay behind.  It did that by adding those
;;;; vertices to the result WITHOUT enqueueing them -- so "don't walk out of the
;;;; catalogue" was a property of HOW they were added, not of WHAT they are.
;;;;
;;;; That distinction is invisible until an edge points INTO the catalogue.  The moment
;;;; a device's edge-type bound includes one (mine-action's FIND-OF-TYPE, which carries a
;;;; find's ordnance classification to the field), the walk reaches the catalogue vertex
;;;; through the front door, enqueues it, and fans back out along its INCOMING edges --
;;;; touching EVERY find in the database that shares that ordnance type, paying a
;;;; DISCLOSABLE-P (and its survey/project traversal) on each before rejecting it.  The
;;;; result is still correct; it is just O(all finds) per device sync.
;;;;
;;;; So the rule is now a property of the vertex: a reference-class vertex is shipped and
;;;; never traversed out of, however it was reached.  These tests pin that, because
;;;; nothing else in the unit suite covers SCOPE-NODE-SET at all.

(in-package #:graph-db/test)

(def-suite peer-scope-suite
  :description "SCOPE-NODE-SET: the closed disclosable subgraph; reference vertices are terminal."
  :in graph-db-suite)

(in-suite peer-scope-suite)

(defparameter *scope-origin*
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 11)
  "A fixed device origin for the scope tests.")

(defmacro with-scope-graph ((g &key reference-classes) &body body)
  "A peer-graph (device role: no socket, no accept loop) carrying the g-* schema.
REFERENCE-CLASSES is the ship-by-class set SCOPE-NODE-SET treats as terminal."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :device :origin-id *scope-origin*
                           :peer-host "localhost" :replication-port 0
                           :reference-classes ,reference-classes
                           :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (ignore-errors (close-graph ,g :snapshot-p nil))))))

(test reference-vertices-are-terminal-in-the-walk
  "An edge INTO a reference-class vertex ships, but the walk must NOT continue out of that
vertex -- not even to ask whether its other neighbours are disclosable.  Without this, one
in-scope edge into a shared catalogue entry drags the entire corpus of that entry's other
neighbours through DISCLOSABLE-P on every sync."
  (with-scope-graph (g :reference-classes '(g-employee))
    (let (root catalogue stranger asked)
      (with-transaction ()
        (setq root      (make-g-person :name "root")           ; the scope root
              stranger  (make-g-person :name "stranger")       ; OUT of scope
              catalogue (make-g-employee :name "catalogue" :title "reference")))
      (with-transaction ()
        (make-g-knows :from root :to catalogue)                ; the edge INTO the catalogue
        (make-g-knows :from stranger :to catalogue))           ; ...and someone else's
      ;; Ship everything except the stranger, and record every vertex we are asked about.
      (setf (graph-db::export-predicate g)
            (lambda (vertex graph scope)
              (declare (ignore graph scope))
              (push (name vertex) asked)
              (not (string= (name vertex) "stranger"))))
      (multiple-value-bind (vset eset)
          (graph-db::scope-node-set g (list (id root)) '("any") :edge-types '(g-knows))
        ;; the root, the catalogue entry, and the edge between them all ship
        (is (gethash (id root) vset)      "the scope root ships")
        (is (gethash (id catalogue) vset) "the reference vertex ships")
        (is (= 1 (hash-table-count eset)) "exactly the root->catalogue edge ships")
        ;; the stranger does not
        (is (not (gethash (id stranger) vset)) "an out-of-scope vertex never ships")
        ;; ...AND WAS NEVER EVEN CONSIDERED.  This is the actual regression guard: without
        ;; the terminal rule the walk enqueues the catalogue vertex, follows its INCOMING
        ;; g-knows edge, and calls DISCLOSABLE-P on the stranger to reject it.  Correct, but
        ;; it is the O(all finds) blow-up.
        (is (not (member "stranger" asked :test #'string=))
            "the walk must not traverse OUT of a reference vertex (it asked about the stranger)")))))

(test a-non-reference-vertex-is-still-traversed
  "The terminal rule must apply ONLY to reference classes -- an ordinary vertex still
propagates the walk, or the closed subgraph would collapse to the roots."
  (with-scope-graph (g :reference-classes '(g-employee))
    (let (a b c)
      (with-transaction ()
        (setq a (make-g-person :name "a")
              b (make-g-person :name "b")
              c (make-g-person :name "c")))
      (with-transaction ()
        (make-g-knows :from a :to b)
        (make-g-knows :from b :to c))                    ; two hops from the root
      (multiple-value-bind (vset eset)
          (graph-db::scope-node-set g (list (id a)) '("any") :edge-types '(g-knows))
        (is (gethash (id b) vset) "one hop out")
        (is (gethash (id c) vset) "TWO hops out -- ordinary vertices still propagate")
        (is (= 2 (hash-table-count eset)) "both edges ship")))))

(test the-reference-set-still-ships-by-class
  "A reference-class vertex ships even when NOTHING points at it -- that is the whole point
of the reference set (the device gets the entire catalogue so it can classify locally)."
  (with-scope-graph (g :reference-classes '(g-employee))
    (let (root orphan)
      (with-transaction ()
        (setq root   (make-g-person :name "root")
              orphan (make-g-employee :name "orphan" :title "unreferenced")))
      (multiple-value-bind (vset eset)
          (graph-db::scope-node-set g (list (id root)) '("any") :edge-types '(g-knows))
        (declare (ignore eset))
        (is (gethash (id orphan) vset)
            "an unreferenced reference-class vertex still ships (by class, not by reachability)")))))
