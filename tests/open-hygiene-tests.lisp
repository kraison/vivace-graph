;;;; Coverage for two open-path hygiene fixes:
;;;;
;;;;  - GH #222: a slashless LOCATION namestring used to keep LOCATION as a
;;;;    FILE pathname, so every sidecar built with
;;;;    (MAKE-PATHNAME :defaults (LOCATION GRAPH)) landed in the store's
;;;;    PARENT directory instead of inside it.
;;;;  - GH #224: a MAKE-GRAPH/OPEN-GRAPH call that fails partway through used
;;;;    to leak every fd it had already opened, and could leave the graph
;;;;    half-registered in *GRAPHS*.
;;;;
;;;; Both fixes live in graph.lisp; see %ABORT-GRAPH-OPEN and the
;;;; MAKE-GRAPH/OPEN-GRAPH LOCATION-normalization comments there.

(in-package #:graph-db/test)

(def-suite open-hygiene-suite
  :description "LOCATION normalization and aborted-open fd hygiene."
  :in graph-db-suite)

(in-suite open-hygiene-suite)

;; A tiny schema of our own, so these tests don't share
;; *INTEGRATION-GRAPH-NAME*'s graph identity with the rest of the suite.
(def-vertex oh-thing () ((label :type string)) :oh-graph)

;;; ---------------------------------------------------------------------------
;;; fd-count helper
;;; ---------------------------------------------------------------------------

(defun %oh-fd-count ()
  "Open fd count for this process.  SBCL's DIRECTORY walks /proc/self/fd
directly and is internally consistent across calls (a fixed small offset
from the readdir fd itself), which is all a before/after delta needs."
  (length (directory "/proc/self/fd/*")))

;;; ---------------------------------------------------------------------------
;;; Aborted-open injection: force one internal opener to signal, so
;;; MAKE-GRAPH/OPEN-GRAPH fail after several other components are already
;;; open.  MAKE-VEV-INDEX/OPEN-VEV-INDEX are the LAST resource MAKE-INSTANCE
;;; opens in both functions (vertex-table, edge-table, heap, indexes,
;;; ve-index-in and ve-index-out all precede it), so injecting there proves
;;; the abort path runs with real partial state already open.
;;; ---------------------------------------------------------------------------

(defmacro %oh-with-injected-failure (fn-name &body body)
  "Redefine the internal GRAPH-DB function named by FN-NAME (a symbol) to
unconditionally signal an error, run BODY, then always restore the
original definition -- even if BODY itself signals."
  (let ((orig (gensym "ORIG")) (fn (gensym "FN")))
    `(let* ((,fn ,fn-name)
            (,orig (fdefinition ,fn)))
       (unwind-protect
            (progn
              (setf (fdefinition ,fn)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (error "GH #224 test: injected open failure")))
              ,@body)
         (setf (fdefinition ,fn) ,orig)))))

;;; ---------------------------------------------------------------------------
;;; GH #222
;;; ---------------------------------------------------------------------------

(test slashless-location-keeps-sidecars-inside-the-store
  "A slashless LOCATION namestring must not scatter .dirty/heap.dat/
schema.dat into the store's parent directory, and a graph created and
read that way must reopen and read back correctly (GH #222)."
  (with-temp-directory (dir)
    (let* ((parent (uiop:temporary-directory))
           (slashless (string-right-trim "/" (namestring dir)))
           id)
      (let ((g (make-graph :oh-graph slashless :buffer-pool-size 1000)))
        (is (probe-file (merge-pathnames
                          ".dirty" (uiop:ensure-directory-pathname
                                    slashless)))
            "~A/.dirty must exist while the graph is open" slashless)
        (is (not (probe-file (merge-pathnames ".dirty" parent)))
            "no .dirty must leak into the parent directory")
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-oh-thing :label "inside")))))
        (close-graph g :snapshot-p nil))
      (is (probe-file (merge-pathnames
                        "heap.dat"
                        (uiop:ensure-directory-pathname slashless)))
          "heap.dat must exist inside the store directory")
      (is (not (probe-file (merge-pathnames "heap.dat" parent)))
          "heap.dat must not leak into the parent directory")
      (is (not (probe-file (merge-pathnames "schema.dat" parent)))
          "schema.dat must not leak into the parent directory")
      (let ((g2 (open-graph :oh-graph slashless :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g2))
               (is (string= "inside" (slot-value (lookup-vertex id) 'label))
                   "data written under the slashless location must survive ~
                    a reopen through the same slashless string"))
          (close-graph g2 :snapshot-p nil))))
    (collect-garbage)))

;;; ---------------------------------------------------------------------------
;;; GH #224
;;; ---------------------------------------------------------------------------

(test aborted-make-graph-does-not-leak-fds
  "Ten aborted MAKE-GRAPH calls, each failing after most of its resources
are already open, must not leak fds and must not half-register the graph
(GH #224)."
  (with-temp-directory (dir)
    (let* ((path (namestring dir))
           (before (%oh-fd-count)))
      (%oh-with-injected-failure 'graph-db::make-vev-index
        (dotimes (i 10)
          (signals error
            (make-graph :oh-graph
                        (format nil "~A/g~D/" path i)
                        :buffer-pool-size 1000))))
      (let ((after (%oh-fd-count)))
        (is (<= after (+ before 3))
            "fd count grew from ~D to ~D across 10 aborted make-graph calls"
            before after))
      (is (null (graph-db:lookup-graph :oh-graph))
          "an aborted make-graph must not leave a half-registered graph"))
    (collect-garbage)))

(test aborted-open-graph-does-not-leak-fds
  "Ten aborted OPEN-GRAPH calls against a valid, cleanly-closed store must
not leak fds, must not half-register the graph, and must not corrupt the
store -- a subsequent un-injected OPEN-GRAPH still succeeds and reads the
data back (GH #224)."
  (with-temp-directory (dir)
    (let* ((path (namestring dir)) id)
      (let ((g (make-graph :oh-graph path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-oh-thing :label "survives")))))
        (close-graph g :snapshot-p nil))
      (let ((before (%oh-fd-count)))
        (%oh-with-injected-failure 'graph-db::open-vev-index
          (dotimes (i 10)
            (signals error
              (open-graph :oh-graph path :buffer-pool-size 1000))))
        (let ((after (%oh-fd-count)))
          (is (<= after (+ before 3))
              "fd count grew from ~D to ~D across 10 aborted open-graph ~
               calls"
              before after)))
      (is (null (graph-db:lookup-graph :oh-graph))
          "an aborted open-graph must not leave a half-registered graph")
      (let ((g2 (open-graph :oh-graph path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g2))
               (is (string= "survives" (slot-value (lookup-vertex id) 'label))
                   "a subsequent un-injected open-graph must still read ~
                    back data written before the aborted opens"))
          (close-graph g2 :snapshot-p nil))))
    (collect-garbage)))
