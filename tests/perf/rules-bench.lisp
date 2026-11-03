;;;; Paging a namespace by key through the guarded runner (GH #451): the
;;;; first page against the last.  Own graph name and own schema so no
;;;; existing bench's work changes (the generation rule, suite.lisp).
;;;; Reported, not gated: the numbers go to the unit's issue.

(in-package #:graph-db/perf-test)

(defparameter *perf-paging-graph-name* :graph-db-perf-paging)

(eval-when (:load-toplevel :execute)
  (setf (gethash *perf-paging-graph-name* *schema-node-metadata*) nil))

(graph-db.spacetime:def-source pp-thing :graph-db-perf-paging
    ((thing-id :initarg :thing-id :accessor pp-thing-id))
  :identity     (:namespace :pp-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "perf fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(graph-db.spacetime:def-claim-classes pp-claim :graph-db-perf-paging)

(defmacro with-perf-paging-graph ((g) &body body)
  "A fresh on-disk graph holding PP-CLAIM claims."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph *perf-paging-graph-name* (namestring ,d)
                             :buffer-pool-size 4000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defun pp-key (i) (format nil "k~5,'0D" i))

(defun pp-insert-claims (n &key (batch 500))
  "N unary claims under :PP-THINGS, one key each, one relation."
  (let ((i 0))
    (loop while (< i n) do
      (graph-db:with-transaction ()
        (dotimes (k (min batch (- n i)))
          (make-pp-claim-unary :subject-namespace :pp-things
                               :subject-key (pp-key i) :relation "rel"
                               :producer "perf" :standing :inferred)
          (incf i))))))

(defun pp-page (g after &key (limit 500))
  "One page of the namespace through the guard: the docs' paging text,
bounded by AFTER (a key) or unbounded for the first page.  Returns the
row count."
  (let ((text (if after
                  (format nil "(claim ?c pp-claim \"pp-things\" ?k ~
\"rel\" ?ons ?o) (> ?k ~S)" after)
                  "(claim ?c pp-claim \"pp-things\" ?k \"rel\" ?ons ?o)")))
    (multiple-value-bind (columns rows)
        (graph-db.query:run-guarded-prolog text g :limit limit)
      (declare (ignore columns))
      (length rows))))

(defun bench-claim-key-paging ()
  "GH #451: a 500-row page by key at the start of a namespace and at its
end, through RUN-GUARDED-PROLOG.  Before the key-bounded route the last
page cost every earlier key's claims; after, its own keys' claims plus
one comparison per earlier key.  KEY-PAGE-DEPTH-RATIO is last over
first."
  (let ((n (scale 20000)) (page 500))
    (with-perf-paging-graph (g)
      (pp-insert-claims n)
      (let ((graph-db::*query-default-max-inferences* 1000000)
            (graph-db::*query-default-timeout* 60)
            (last-bound (pp-key (- n page 1)))
            t-first t-last)
        (pp-page g nil :limit page)   ; warm-up, untimed
        (setf t-first (timed-seconds ("key-page-first-500")
                        (pp-page g nil :limit page)))
        (setf t-last (timed-seconds ("key-page-last-500")
                       (pp-page g last-bound :limit page)))
        (record "key-page-depth-ratio"
                :ratio (if (zerop t-first)
                           0
                           (float-3 (/ t-last t-first))))))))
