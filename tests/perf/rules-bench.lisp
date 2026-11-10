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

;;;; A forward step against two derived-set sizes (#457): the step for a
;;;; one-claim commit resolves what it derives per identity, so its time
;;;; should not grow with the rule's standing derivation.  Own graph name
;;;; and schema, as above.  Reported, not gated: the numbers go to #457.

(defparameter *perf-forward-graph-name* :graph-db-perf-forward)

(eval-when (:load-toplevel :execute)
  (setf (gethash *perf-forward-graph-name* *schema-node-metadata*) nil))

(graph-db.spacetime:def-source pf-thing :graph-db-perf-forward
    ((thing-id :initarg :thing-id :accessor pf-thing-id))
  :identity     (:namespace :pf-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "perf fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(graph-db.spacetime:def-claim-classes pf-claim :graph-db-perf-forward)
(graph-db.rules:def-rules-schema :graph-db-perf-forward)

(defmacro with-perf-forward-graph ((g) &body body)
  "A fresh on-disk graph holding PF-CLAIM claims and the rules schema."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph *perf-forward-graph-name* (namestring ,d)
                             :buffer-pool-size 4000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defun pf-insert-base (g from to &key (batch 500))
  "Unary \"base\" claims under :PF-THINGS for the keys FROM below TO."
  (let ((i from))
    (loop while (< i to) do
      (graph-db:with-transaction ((graph-db::transaction-manager g))
        (dotimes (k (min batch (- to i)))
          (make-pf-claim-unary :graph g :subject-namespace :pf-things
                               :subject-key (format nil "k~7,'0D" i)
                               :relation "base" :producer "perf"
                               :standing :observed)
          (incf i))))))

(defun pf-forward-step-seconds (n)
  "A store at its fixpoint with N claims derived by one rule from N
base claims, forward mode on; then one more base claim committed.  =>
the wall seconds of that commit, the step included (it runs on the
committing thread before WITH-TRANSACTION returns)."
  (with-perf-forward-graph (g)
    (pf-insert-base g 0 n)
    (graph-db:with-transaction ((graph-db::transaction-manager g))
      (graph-db.rules:make-rule
       :graph g :name "pf-derived" :version "1" :family "pf-claim"
       :head "(claim ?c pf-claim \"pf-things\" ?k \"derived\" nil nil)"
       :body "(claim ?p pf-claim \"pf-things\" ?k \"base\" nil nil)"))
    (let ((graph-db.rules::*rules-max-solutions* (* 2 n))
          (graph-db::*query-default-max-inferences* (* 100 n))
          (graph-db::*query-default-timeout* 600))
      (graph-db.rules:enable-forward-rules g))
    ;; The ratio is vacuous unless the store is at a fixpoint of N: a
    ;; refused RUN-RULES would leave nothing for the step to read past.
    (let ((standing (length (graph-db.spacetime:claims-by-producer
                             g 'pf-claim "rule/pf-derived"))))
      (unless (= n standing)
        (error "fixpoint at N=~D has ~D derived claims" n standing)))
    ;; One untimed commit first: the compiled-rule cache and first-use
    ;; costs are a constant that would dilute an O(N) residue.
    (pf-insert-base g n (1+ n))
    (let ((start (get-internal-real-time))
          (key (format nil "k~7,'0D" (1+ n))))
      (pf-insert-base g (1+ n) (+ n 2))
      (prog1 (/ (- (get-internal-real-time) start)
                (float internal-time-units-per-second 1.0d0))
        ;; A step that failed or derived nothing has no time worth
        ;; recording: say so instead of reporting a fast miss.
        (let ((status (graph-db.rules:forward-rules-status g))
              (derived (graph-db.spacetime:claims-touching
                        g 'pf-claim :pf-things key
                        :role :subject :relation "derived" :current t)))
          (when (or (getf status :failures) (/= 1 (length derived)))
            (error "forward step at N=~D derived ~D, status ~S"
                   n (length derived) status)))))))

(defun bench-forward-step-reads (&key (n (scale 20000)))
  "GH #457: one forward step on a store with N/10 derived claims under
the touched rule, and one on a store with N.  FORWARD-STEP-SCALE-RATIO
is large over small: near 1 when the step reads per identity, near 10
when it reads the producer's derivation.  The suite's N keeps RUN-PERF
short; the acceptance run is :N 100000 by hand (the numbers on #457)."
  (let ((small (pf-forward-step-seconds (max 1 (floor n 10))))
        (large (pf-forward-step-seconds n)))
    (record "forward-step-small" :ms (float-3 (* 1000 small)))
    (record "forward-step-large" :ms (float-3 (* 1000 large)))
    (record "forward-step-scale-ratio"
            :ratio (if (zerop small) 0 (float-3 (/ large small))))))

(defun bench-rules-rerun ()
  "Unit 6 (GH #376): one rule over N base claims run K times through
RUN-RULE, the same body every time: the shape a compiled-query cache
serves.  Before the cache every run interprets the body afresh; after,
the second run compiles it and the rest run compiled.  Runs per second."
  (let ((n (scale 2000)) (k (scale 20)))
    (with-perf-forward-graph (g)
      (pf-insert-base g 0 n)
      (let ((rule (graph-db:with-transaction
                      ((graph-db::transaction-manager g))
                    (graph-db.rules:make-rule
                     :graph g :name "pf-rerun" :version "1"
                     :family "pf-claim"
                     :head "(claim ?c pf-claim \"pf-things\" ?k ~
\"derived\" nil nil)"
                     :body "(claim ?p pf-claim \"pf-things\" ?k ~
\"base\" nil nil)")))
            (graph-db.rules::*rules-max-solutions* (* 2 n))
            (graph-db::*query-default-max-inferences* (* 100 n))
            (graph-db::*query-default-timeout* 600))
        (timed-ops ("rules-rerun" k)
          (dotimes (i k)
            (graph-db.rules:run-rule g rule)))))))
