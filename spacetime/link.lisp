;;;; Linking a claim to its endpoint nodes (GH #369, spec sec.4).
;;;;
;;;; Write-time here; the idempotent sweep (U2, spec sec.5) joins this
;;;; file.  Edges are derived: the (namespace, key) slots are the truth,
;;;; and nothing in this file may fail a claim write except a caller's
;;;; own mismatch (sec.4.2).

(in-package #:graph-db.spacetime)

(defun %binary-claim-p (claim)
  "True when CLAIM's class carries the binary arity's OBJECT-KEY slot."
  (slot-exists-p claim 'object-key))

(defun %identity-slot (class)
  "CLASS's identity key slot symbol (SOURCE-FACETS-IDENTITY :KEY-SLOT)."
  (getf (source-facets-identity (source-contract class)) :key-slot))

(defun %transaction-creates ()
  "Nodes GRAPH-DB::*TRANSACTION* is creating, computed once per claim
write rather than per candidate class per endpoint: a fresh
MAKE-COMMIT-VIEW is O(W) over the whole write set, quadratic over a bulk
transaction (GH #369).  NIL when no transaction is open."
  (let ((tx graph-db::*transaction*))
    (when tx
      (loop for w in (graph-db::writes tx)
            when (typep w 'graph-db::tx-create)
              collect (graph-db::node w)))))

(defun %uncommitted-creates (class slot key creates)
  "Nodes of CLASS in CREATES (see %TRANSACTION-CREATES) whose SLOT is
KEY -- what INDEX-LOOKUP cannot see, because the index is written at
commit apply (spec sec.4.1 'Same-transaction visibility'; the #324
overlay).  An unbound SLOT is 'no key', not a candidate."
  (loop for n in creates
        when (and (typep n class)
                  (slot-boundp n slot)
                  (let ((v (slot-value n slot)))
                    (and (stringp v) (string= v key))))
          collect n))

(defun %same-store-candidates (graph namespace key creates)
  "Distinct nodes in GRAPH that are registered sources of NAMESPACE with
identity key KEY: index hits unioned with CREATES, the open
transaction's own creates.  NIL, never a signal, for an unknown
namespace or a class with no index in GRAPH (spec sec.4.1 steps 1-2).
Reads *NAMESPACE-SOURCES* directly: an unregistered namespace is the
common case and must not cost a condition per write (GH #369)."
  (let ((hits '()))
    (dolist (class (gethash namespace *namespace-sources*))
      (let ((slot (%identity-slot class)))
        (dolist (n (handler-case
                       (graph-db:index-lookup graph class (list slot) key)
                     (graph-db:query-precondition-error () nil)))
          (push n hits))
        (dolist (n (%uncommitted-creates class slot key creates))
          (push n hits))))
    (remove-duplicates hits :key #'graph-db:id :test #'equalp)))

(defun %verify-endpoint-node (node namespace key)
  "Signal ENDPOINT-MISMATCH unless NODE is a registered source of
NAMESPACE whose identity key is KEY (spec sec.4.2 step 1).  An
UNRESOLVED-NODE marker is never a source, so it is refused too; an
unbound identity slot is a :KEY mismatch, not an error.  Reads
*NAMESPACE-SOURCES* directly -- see %SAME-STORE-CANDIDATES."
  (let* ((classes (gethash namespace *namespace-sources*))
         (class (find-if (lambda (c) (typep node c)) classes)))
    (unless class
      (error 'endpoint-mismatch :node node :namespace namespace :key key
                                :reason :not-a-source))
    (let ((slot (%identity-slot class)))
      (unless (and (slot-boundp node slot)
                   (let ((v (slot-value node slot)))
                     (and (stringp v) (string= v key))))
        (error 'endpoint-mismatch :node node :namespace namespace
                                  :key key :reason :key)))
    node))

(defun %single-same-store-candidate (claim graph namespace key creates)
  "The one same-store source of NAMESPACE with identity KEY, or NIL for
zero.  Several candidates is ambiguous: warns ENDPOINT-LINK-SKIPPED (a
WARNING, so it still reaches the caller though the write never fails)
and returns NIL (spec sec.4.1 step 3)."
  (let ((cands (%same-store-candidates graph namespace key creates)))
    (cond ((null cands) nil)
          ((cdr cands)
           (warn 'endpoint-link-skipped
                 :claim claim :namespace namespace :key key
                 :classes (remove-duplicates
                           (mapcar (lambda (n) (class-name (class-of n)))
                                   cands)))
           nil)
          (t (first cands)))))

(defun %link-endpoint (claim graph ctor namespace key given creates)
  "Never fail the claim write for a link failure: log and return NIL
(GH #161, #369)."
  (let ((target (when given (%verify-endpoint-node given namespace key))))
    (handler-case
        (let ((node (or target
                         (%single-same-store-candidate
                          claim graph namespace key creates))))
          (when node
            (funcall ctor :from claim :to (graph-db:id node) :graph graph)))
      (error (c)
        (log:warn "GH #369: endpoint (~S ~S) of claim ~A not linked: ~A"
                  namespace key (graph-db:id claim) c)
        nil))))

(defun %link-claim-at-write (claim &key subject-node object-node)
  "Link CLAIM's endpoints in its own store, inside the caller's open
transaction (spec sec.4.1-4.2).  Returns CLAIM.  Never signals an ERROR
for the derived edge; ENDPOINT-MISMATCH is the caller's, not the edge's."
  ;; NODE-GRAPH is internal to GRAPH-DB (not exported); GRAPH-DB::* is
  ;; the house idiom used elsewhere in this file for such symbols.
  (let ((graph (graph-db::node-graph claim))
        (creates (%transaction-creates)))
    (%link-endpoint claim graph #'make-subject-of
                    (claim-subject-namespace claim)
                    (claim-subject-key claim) subject-node creates)
    (when (%binary-claim-p claim)
      (%link-endpoint claim graph #'make-object-of
                      (claim-object-namespace claim)
                      (claim-object-key claim) object-node creates))
    claim))

(defun %strip-endpoint-node-args (args)
  "ARGS without :SUBJECT-NODE / :OBJECT-NODE, which the raw constructor
must never see."
  (let ((copy (copy-list args)))
    (remf copy :subject-node)
    (remf copy :object-node)
    copy))

;;; ---------------------------------------------------------------------------
;;; The sweep (GH #372, spec sec.5): read under a snapshot, resolve
;;; through RESOLVE-ENDPOINT (which may cross stores and refuses a
;;; write transaction), then one short write transaction.  Never prunes,
;;; never signals: the counts are the report.
;;; ---------------------------------------------------------------------------

(defun %family-parents-in (graph family)
  "Parent class names to sweep in GRAPH: FAMILY's alone
(UNKNOWN-CLAIM-FAMILY if unregistered), else every registered family
whose parent type is defined in GRAPH's schema -- a type defined but
never written costs one empty MAP-VERTICES."
  (if family
      (let ((parent (claim-family-parent (claim-family family))))
        (when (graph-db:lookup-node-type-by-name parent :vertex :graph graph)
          (list parent)))
      (let ((parents '()))
        (maphash (lambda (parent fam)
                   (declare (ignore fam))
                   (when (graph-db:lookup-node-type-by-name
                          parent :vertex :graph graph)
                     (push parent parents)))
                 *claim-families*)
        parents)))

(defun %missing-endpoints (claim graph)
  "The (CTOR TYPE NAMESPACE KEY) entries of CLAIM's endpoints that have
no edge in GRAPH; a unary claim has at most one."
  (let ((out '()))
    (unless (%linked-edge claim graph 'subject-of)
      (push (list #'make-subject-of 'subject-of
                  (claim-subject-namespace claim) (claim-subject-key claim))
            out))
    (when (and (%binary-claim-p claim)
               (null (%linked-edge claim graph 'object-of)))
      (push (list #'make-object-of 'object-of
                  (claim-object-namespace claim) (claim-object-key claim))
            out))
    (nreverse out)))

(defun %sweep-collect (graph parents since limit)
  "Claims of PARENTS in GRAPH with a missing edge, at or above commit
epoch SINCE, at most LIMIT of them: (VALUES ((CLAIM-ID . MISSING)...)
MORE-P).  MORE-P means a further such claim exists; the scan then STOPS
there, so LIMIT bounds the scan as well as the work (GH #372).  Runs
inside the caller's read snapshot: the slots are read here, and only
ids escape the pin."
  (let ((work '()) (n 0) (more nil))
    (block scan
      (dolist (parent parents)
        (graph-db:map-vertices
         (lambda (c)
           ;; No epoch = written before #347 or not yet committed:
           ;; :SINCE cannot place it, so the window excludes it.
           (when (or (null since)
                     (let ((e (claim-commit-epoch c))) (and e (>= e since))))
             (let ((missing (%missing-endpoints c graph)))
               (when missing
                 (when (and limit (>= n limit))
                   (setf more t)
                   (return-from scan))
                 (push (cons (graph-db:id c) missing) work)
                 (incf n)))))
         graph :vertex-type parent)))
    (values (nreverse work) more)))

(defun %sweep-resolution (namespace key seen)
  "RESOLVE-ENDPOINT for (NAMESPACE KEY), memoised in SEEN for the call:
a node id, :NONE, :AMBIGUOUS, or :SKIP for a namespace the call must
abandon (GH #372).  One resolution per endpoint value, however many
claims name it."
  (let ((cell (cons namespace key)))
    (multiple-value-bind (hit found) (gethash cell seen)
      (if found
          hit
          (setf (gethash cell seen)
                (handler-case
                    (let ((node (resolve-endpoint namespace key)))
                      (if node (graph-db:id node) :none))
                  ((or unknown-namespace unopened-source-graph) () :skip)
                  (ambiguous-endpoint () :ambiguous)))))))

(defun %sweep-resolve (work)
  "Resolve every missing endpoint in WORK: (VALUES PLAN UNRESOLVED
AMBIGUOUS SKIPPED), PLAN a list of (CLAIM-ID CTOR TYPE NODE-ID).  A
namespace that signals UNKNOWN-NAMESPACE or UNOPENED-SOURCE-GRAPH is
skipped for the rest of the call (spec sec.5 step 1).  The counts are
per ENDPOINT; the resolutions behind them are memoised per
(NAMESPACE . KEY)."
  (let ((plan '()) (unresolved 0) (ambiguous 0) (skipped '())
        (seen (make-hash-table :test 'equal)))
    (dolist (entry work)
      (destructuring-bind (claim-id . missing) entry
        (dolist (m missing)
          (destructuring-bind (ctor type namespace key) m
            (unless (member namespace skipped)
              (let ((hit (%sweep-resolution namespace key seen)))
                (case hit
                  (:none (incf unresolved))
                  (:ambiguous (incf ambiguous))
                  (:skip (push namespace skipped))
                  (t (push (list claim-id ctor type hit) plan)))))))))
    (values (nreverse plan) unresolved ambiguous (nreverse skipped))))

(defun %sweep-write (graph plan)
  "Create PLAN's edges in one transaction on GRAPH; the number committed.
Each claim is re-checked as still present, not deleted and still
unlinked for that edge type, which is what makes a repeated sweep
idempotent.  A construction failure is logged, never signalled."
  (if (null plan)
      0
      (handler-case
          (graph-db:with-transaction (:graph graph)
            ;; Counted INSIDE the body: a VALIDATION-CONFLICT retry
            ;; re-runs it, and an outer counter would double (GH #372).
            (let ((n 0))
              (dolist (step plan)
                (destructuring-bind (claim-id ctor type node-id) step
                  (let ((c (graph-db:lookup-vertex claim-id :graph graph)))
                    (when (and c
                               (not (graph-db:deleted-p c))
                               (null (%linked-edge c graph type)))
                      (funcall ctor :from claim-id :to node-id :graph graph)
                      (incf n)))))
              n))
        (error (c)
          (log:warn "GH #372: sweep on ~A wrote nothing: ~A"
                    (graph-db:graph-name graph) c)
          0))))

(defun link-claim-endpoints (graph &key family since limit)
  "Link every claim in GRAPH whose endpoint now resolves: the idempotent
sweep and backfill (GH #372, spec sec.5).  (VALUES LINKED UNRESOLVED
AMBIGUOUS SKIPPED-NAMESPACES MORE-P): edges committed; endpoints with no
node; endpoints with several; namespaces skipped for the whole call
(unregistered, or a source store not open); whether a claim with a
missing edge exists beyond this call's window.

FAMILY is one parent class name (default: every family defined in
GRAPH's schema); an unregistered one signals UNKNOWN-CLAIM-FAMILY, the
sweep's only caller-error signal.  SINCE is a commit epoch
(CLAIM-COMMIT-EPOCH): only claims at or above it are visited, so a
regeneration's writes can be swept alone.  LIMIT bounds both the work
and the scan: collection stops at the first claim with a missing edge
past the window.

MORE-P is NOT 'progress remains'.  A window of permanently unresolvable
claims -- an unregistered namespace, a key nobody holds -- returns
LINKED 0 with MORE-P T for ever, because each call re-collects them.
Loop (LOOP WHILE (AND MORE-P (PLUSP LINKED))) and stop when LINKED is
0: widen :LIMIT, fix the sources, or use :SINCE.  A large backfill wants
:LIMIT (a few thousand) so each write transaction stays short.

Never prunes -- ACTIVE-EDGE-P hides a dead endpoint's edge and
COMPACT-EDGES reclaims it -- and never signals for what it could not do.
A source deleted and re-created under the same key gains a FRESH edge at
the next sweep; the superseded one stays hidden until COMPACT-EDGES, so
run that after a source regeneration.

Trap: must not be called inside a read-write transaction
(RESOLUTION-IN-TRANSACTION); run it between transactions."
  (let ((parents (%family-parents-in graph family)))
    (if (null parents)
        (values 0 0 0 '() nil)
        (multiple-value-bind (work more)
            (graph-db:with-read-snapshot (graph)
              (%sweep-collect graph parents since limit))
          (multiple-value-bind (plan unresolved ambiguous skipped)
              (graph-db:with-read-snapshot (graph)
                (%sweep-resolve work))
            (values (%sweep-write graph plan)
                    unresolved ambiguous skipped more))))))
