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
  "Parent class names whose family has claims in GRAPH: FAMILY's alone
(UNKNOWN-CLAIM-FAMILY if unregistered), else every registered family
whose parent type is instantiated in GRAPH."
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
    (unless (graph-db:outgoing-edges claim :graph graph
                                           :edge-type 'subject-of)
      (push (list #'make-subject-of 'subject-of
                  (claim-subject-namespace claim) (claim-subject-key claim))
            out))
    (when (and (%binary-claim-p claim)
               (null (graph-db:outgoing-edges claim :graph graph
                                                    :edge-type 'object-of)))
      (push (list #'make-object-of 'object-of
                  (claim-object-namespace claim) (claim-object-key claim))
            out))
    (nreverse out)))

(defun %sweep-collect (graph parents since limit)
  "Claims of PARENTS in GRAPH with a missing edge, at or above commit
epoch SINCE, at most LIMIT of them: (VALUES ((CLAIM . MISSING)...) MORE-P).
Runs inside the caller's read snapshot."
  (let ((work '()) (n 0) (more nil))
    (dolist (parent parents)
      (graph-db:map-vertices
       (lambda (c)
         (when (or (null since)
                   (let ((e (claim-commit-epoch c))) (and e (>= e since))))
           (let ((missing (%missing-endpoints c graph)))
             (when missing
               (if (and limit (>= n limit))
                   (setf more t)
                   (progn (push (cons c missing) work) (incf n)))))))
       graph :vertex-type parent))
    (values (nreverse work) more)))

(defun %sweep-resolve (work)
  "Resolve every missing endpoint in WORK: (VALUES PLAN UNRESOLVED
AMBIGUOUS SKIPPED), PLAN a list of (CLAIM-ID CTOR TYPE NODE-ID).  A
namespace that signals UNKNOWN-NAMESPACE or UNOPENED-SOURCE-GRAPH is
skipped for the rest of the call (spec sec.5 step 1)."
  (let ((plan '()) (unresolved 0) (ambiguous 0) (skipped '()))
    (dolist (entry work)
      (destructuring-bind (claim . missing) entry
        (dolist (m missing)
          (destructuring-bind (ctor type namespace key) m
            (unless (member namespace skipped)
              (handler-case
                  (let ((node (resolve-endpoint namespace key)))
                    (if node
                        (push (list (graph-db:id claim) ctor type
                                    (graph-db:id node))
                              plan)
                        (incf unresolved)))
                ((or unknown-namespace unopened-source-graph) ()
                  (push namespace skipped))
                (ambiguous-endpoint () (incf ambiguous))))))))
    (values (nreverse plan) unresolved ambiguous (nreverse skipped))))

(defun %sweep-write (graph plan)
  "Create PLAN's edges in one transaction on GRAPH; the number committed.
Each claim is re-checked as still present and still unlinked for that
edge type, so two sweeps racing, or a sweep racing a write-time link,
stay idempotent.  A construction failure is logged, never signalled."
  (if (null plan)
      0
      (let ((n 0))
        (handler-case
            (graph-db:with-transaction ((graph-db::transaction-manager graph))
              (dolist (step plan)
                (destructuring-bind (claim-id ctor type node-id) step
                  (let ((c (graph-db:lookup-vertex claim-id :graph graph)))
                    (when (and c
                               (null (graph-db:outgoing-edges
                                      c :graph graph :edge-type type)))
                      (funcall ctor :from claim-id :to node-id :graph graph)
                      (incf n))))))
          (error (c)
            (log:warn "GH #372: sweep on ~A wrote nothing: ~A"
                      (graph-db:graph-name graph) c)
            (setf n 0)))
        n)))

(defun link-claim-endpoints (graph &key family since limit)
  "Link every claim in GRAPH whose endpoint now resolves: the idempotent
sweep and backfill (GH #372, spec sec.5).  (VALUES LINKED UNRESOLVED
AMBIGUOUS SKIPPED-NAMESPACES MORE-P): edges committed; endpoints with no
node; endpoints with several; namespaces skipped for the whole call
(unregistered, or a source store not open); whether LIMIT stopped the
visit with work remaining.

FAMILY is one parent class name (default: every family with claims in
GRAPH).  SINCE is a commit epoch (CLAIM-COMMIT-EPOCH): only claims at or
above it are visited, so a regeneration's writes can be swept alone.
LIMIT bounds the claims WITH A MISSING EDGE examined in one call; loop
while MORE-P, noting that endpoints that stay unresolved are examined
again each call.

Never prunes -- ACTIVE-EDGE-P hides a dead endpoint's edge and
COMPACT-EDGES reclaims it -- and never signals for what it could not do.
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
