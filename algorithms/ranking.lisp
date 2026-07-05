;;;; graph-db/algorithms -- ranking algorithms (Mode B, native).
;;;;
;;;; PageRank (+ a binned distribution), HITS hubs & authorities, and a pairwise
;;;; SimRank.  All run under a read snapshot, keyed by node UUID, and never
;;;; mutate the graph.  Adjacency is snapshotted once up front so the iterative
;;;; algorithms don't re-probe the indexes every round.
;;;;
;;;; Improvements over the graph-utils originals (per the port plan):
;;;;   * PageRank iterates to an L1-delta convergence tolerance (not a fixed k),
;;;;     redistributes dangling-node mass across all nodes (so ranks sum to 1),
;;;;     and caps iterations.
;;;;   * HITS normalizes (L2) EVERY iteration -- graph-utils normalized only
;;;;     optionally at the end, letting values blow up -- plus convergence.
;;;;   * SimRank: graph-utils' was an empty stub; here it is a real recursive,
;;;;     depth-bounded, memoized pairwise similarity (no O(V^2) materialization).

(in-package :graph-db)

(defun %l2-normalize! (table keys)
  "Divide each TABLE value (over KEYS, a list of vertices) by the L2 norm of the
vector.  No-op if the norm is 0.  Destructive; returns TABLE."
  (let ((norm (sqrt (reduce (lambda (acc v) (+ acc (expt (gethash (id v) table) 2)))
                            keys :initial-value 0.0d0))))
    (unless (zerop norm)
      (dolist (v keys)
        (setf (gethash (id v) table) (/ (gethash (id v) table) norm))))
    table))

(defun %kv->sorted-list (table vtable)
  "Materialize TABLE (node-key -> value) as a (VERTEX . VALUE) list, descending."
  (let ((r nil))
    (maphash (lambda (k v) (push (cons (gethash k vtable) v) r)) table)
    (sort r #'> :key #'cdr)))

;;; ------------------------------------------------------------------
;;; PageRank
;;; ------------------------------------------------------------------

(defun page-rank (&key (graph *graph*) (damping 0.85d0) (max-iterations 100)
                       (tolerance 1d-6) edge-type use-weights-p vertex-type)
  "PageRank of GRAPH.  Returns (values RANKED ITERATIONS) where RANKED is a list
of (VERTEX . RANK) descending and ITERATIONS the number of rounds run.

Iterates until the total absolute change (L1) between rounds falls below
TOLERANCE or MAX-ITERATIONS is reached.  DAMPING is the usual teleport factor.
Dangling nodes (no out-edges) have their mass redistributed across all nodes, so
ranks sum to 1.  With USE-WEIGHTS-P, a node's rank is split among its out-edges
in proportion to edge WEIGHT (otherwise uniformly).  VERTEX-TYPE / EDGE-TYPE
narrow the population / edges; each may be a single type or a list of types."
  (with-algorithm-snapshot (graph)
    (let* ((vertices (all-vertices graph vertex-type))
           (n (length vertices)))
      (if (zerop n)
          (values nil 0)
          (let ((out (make-hash-table :test 'equalp))   ; key -> (strength . ((nbr-key . w)...))
                (vtable (make-hash-table :test 'equalp))
                (pr (make-hash-table :test 'equalp))
                (init (/ 1.0d0 n)))
            (dolist (v vertices)
              (let* ((cells (adjacent-vertices v :graph graph :direction :out
                                                 :edge-type edge-type
                                                 :unweighted (not use-weights-p)))
                     (strength (if use-weights-p
                                   (reduce #'+ cells :key #'cdr :initial-value 0)
                                   (length cells))))
                (setf (gethash (id v) out)
                      (cons strength (mapcar (lambda (c) (cons (id (car c)) (cdr c))) cells))
                      (gethash (id v) vtable) v
                      (gethash (id v) pr) init)))
            (let ((iterations 0))
              (block converge
                (dotimes (i max-iterations)
                  (incf iterations)
                  (let* ((dangling 0.0d0)
                         (new (make-hash-table :test 'equalp)))
                    (maphash (lambda (k val)
                               (when (zerop (car (gethash k out)))
                                 (incf dangling val)))
                             pr)
                    (let ((baseval (+ (/ (- 1.0d0 damping) n)
                                      (/ (* damping dangling) n))))
                      (dolist (v vertices) (setf (gethash (id v) new) baseval)))
                    (maphash (lambda (k val)
                               (destructuring-bind (strength . nbrs) (gethash k out)
                                 (when (> strength 0)
                                   (let ((share (/ val strength)))
                                     (dolist (cell nbrs)
                                       (incf (gethash (car cell) new)
                                             (* damping share
                                                (if use-weights-p (cdr cell) 1))))))))
                             pr)
                    (let ((delta 0.0d0))
                      (maphash (lambda (k val) (incf delta (abs (- val (gethash k pr))))) new)
                      (setf pr new)
                      (when (< delta tolerance) (return-from converge))))))
              (values (%kv->sorted-list pr vtable) iterations)))))))

(defun page-rank-distribution (&key (graph *graph*) page-rank (bin-count 10)
                                    (damping 0.85d0) (max-iterations 100)
                                    (tolerance 1d-6))
  "Histogram of PageRank values as a list of (LOWER UPPER COUNT) bins, ascending.
PAGE-RANK may be a precomputed (VERTEX . RANK) list (as PAGE-RANK returns);
otherwise it is computed with the given parameters.  BIN-COUNT equal-width bins
span [min,max]."
  (let* ((ranked (or page-rank
                     (page-rank :graph graph :damping damping
                                :max-iterations max-iterations :tolerance tolerance)))
         (values (mapcar #'cdr ranked)))
    (cond ((null values) nil)
          (t (let ((mn (reduce #'min values))
                   (mx (reduce #'max values)))
               (if (= mn mx)
                   (list (list mn mx (length values)))
                   (let ((width (/ (- mx mn) bin-count))
                         (bins (make-array bin-count :initial-element 0)))
                     (dolist (val values)
                       (incf (aref bins (min (1- bin-count)
                                             (floor (- val mn) width)))))
                     (loop for i below bin-count
                           collect (list (+ mn (* i width))
                                         (+ mn (* (1+ i) width))
                                         (aref bins i))))))))))

;;; ------------------------------------------------------------------
;;; HITS (hubs & authorities)
;;; ------------------------------------------------------------------

(defun hub-authority-values (&key (graph *graph*) (max-iterations 100)
                                  (tolerance 1d-6) edge-type vertex-type)
  "Kleinberg HITS on GRAPH.  Returns (values AUTHORITIES HUBS ITERATIONS), the
first two being (VERTEX . SCORE) lists descending.

Each round recomputes authority(v) = sum of hub(u) over in-neighbours u, then
hub(v) = sum of authority(w) over out-neighbours w, and L2-normalizes both
vectors (every iteration -- the fix for graph-utils' value blow-up).  Iterates to
an L1 convergence TOLERANCE or MAX-ITERATIONS.  EDGE-TYPE / VERTEX-TYPE may each
be a single type or a list of types."
  (with-algorithm-snapshot (graph)
    (let* ((vertices (all-vertices graph vertex-type))
           (n (length vertices)))
      (if (zerop n)
          (values nil nil 0)
          (let ((inadj (make-hash-table :test 'equalp))
                (outadj (make-hash-table :test 'equalp))
                (vtable (make-hash-table :test 'equalp))
                (hub (make-hash-table :test 'equalp))
                (auth (make-hash-table :test 'equalp)))
            (dolist (v vertices)
              (setf (gethash (id v) inadj)
                    (mapcar (lambda (c) (id (car c)))
                            (adjacent-vertices v :graph graph :direction :in
                                                 :edge-type edge-type :unweighted t))
                    (gethash (id v) outadj)
                    (mapcar (lambda (c) (id (car c)))
                            (adjacent-vertices v :graph graph :direction :out
                                                 :edge-type edge-type :unweighted t))
                    (gethash (id v) vtable) v
                    (gethash (id v) hub) 1.0d0
                    (gethash (id v) auth) 1.0d0))
            (let ((iterations 0))
              (block converge
                (dotimes (i max-iterations)
                  (incf iterations)
                  (let ((na (make-hash-table :test 'equalp))
                        (nh (make-hash-table :test 'equalp)))
                    (dolist (v vertices)
                      (setf (gethash (id v) na)
                            (reduce (lambda (acc uk) (+ acc (gethash uk hub)))
                                    (gethash (id v) inadj) :initial-value 0.0d0)))
                    (%l2-normalize! na vertices)
                    (dolist (v vertices)
                      (setf (gethash (id v) nh)
                            (reduce (lambda (acc wk) (+ acc (gethash wk na)))
                                    (gethash (id v) outadj) :initial-value 0.0d0)))
                    (%l2-normalize! nh vertices)
                    (let ((delta 0.0d0))
                      (dolist (v vertices)
                        (incf delta (abs (- (gethash (id v) na) (gethash (id v) auth))))
                        (incf delta (abs (- (gethash (id v) nh) (gethash (id v) hub)))))
                      (setf auth na hub nh)
                      (when (< delta tolerance) (return-from converge))))))
              (values (%kv->sorted-list auth vtable)
                      (%kv->sorted-list hub vtable)
                      iterations)))))))

;;; ------------------------------------------------------------------
;;; SimRank (pairwise, depth-bounded)
;;; ------------------------------------------------------------------

(defun sim-rank (a b &key (graph *graph*) (max-depth 5) (decay 0.8d0) edge-type)
  "Pairwise SimRank similarity between nodes A and B: two nodes are similar to the
extent that their in-neighbours are similar.  Recursive, DECAY-weighted, and
bounded to MAX-DEPTH levels of recursion (with memoization), so it never
materializes the full O(V^2) similarity matrix.

s(a,a)=1; s(a,b)=0 when either has no in-neighbours or at depth 0; otherwise
s(a,b) = decay/(|In(a)||In(b)|) * sum over in-neighbour pairs of s(.,.).
EDGE-TYPE may be a single type or a list of types.  Returns a value in [0,1]."
  (with-algorithm-snapshot (graph)
    (let ((av (algorithm-vertex a graph))
          (bv (algorithm-vertex b graph))
          (memo (make-hash-table :test 'equal))
          (in-cache (make-hash-table :test 'equalp)))
      (unless (and av bv) (error "sim-rank: unknown node"))
      (labels ((ins (v)
                 (let ((k (id v)))
                   (multiple-value-bind (val present) (gethash k in-cache)
                     (if present val
                         (setf (gethash k in-cache)
                               (mapcar #'car
                                       (adjacent-vertices v :graph graph :direction :in
                                                            :edge-type edge-type
                                                            :unweighted t)))))))
               (sr (x y depth)
                 (cond ((equalp (id x) (id y)) 1.0d0)
                       ((<= depth 0) 0.0d0)
                       (t (let ((xs (ins x)) (ys (ins y)))
                            (if (or (null xs) (null ys))
                                0.0d0
                                (let* ((sx (string-id (id x)))
                                       (sy (string-id (id y)))
                                       (key (list (if (string< sx sy) sx sy)
                                                  (if (string< sx sy) sy sx)
                                                  depth)))
                                  (multiple-value-bind (val present) (gethash key memo)
                                    (if present val
                                        (setf (gethash key memo)
                                              (* (/ decay (* (length xs) (length ys)))
                                                 (loop for u in xs sum
                                                   (loop for v in ys sum
                                                     (sr u v (1- depth)))))))))))))))
        (sr av bv max-depth)))))
