;;;; rules/run.lisp -- derive, reconcile, record provenance, report
;;;; (spec §7-§9, GH #331).  Like compile.lisp this cold-compiles with
;;;; forward references: RULE's accessors and DERIVATION's constructor
;;;; are made per store by DEF-RULES-SCHEMA (schema.lisp).

(in-package #:graph-db.rules)

(defvar *rules-max-inferences* nil
  "Inference budget for one RUN-RULE, or NIL for the DSL's
*QUERY-DEFAULT-MAX-INFERENCES* (spec §7).  Effectively NIL on both this
and *RULES-TIMEOUT* is an operator error, not a report (ruling P4).")

(defvar *rules-timeout* nil
  "Wall-clock seconds for one RUN-RULE's body, or NIL for the DSL's
*QUERY-DEFAULT-TIMEOUT*.")

(defvar *rules-max-solutions* 100000
  "Solutions one RUN-RULE may collect before it is refused rather than
silently truncated.")

(define-condition rule-run-refusal (error)
  ((tag :initarg :tag :reader rule-run-refusal-tag)
   (text :initarg :text :reader rule-run-refusal-text))
  (:report (lambda (c s)
             (format s "~A: ~A" (rule-run-refusal-tag c)
                     (rule-run-refusal-text c))))
  (:documentation "RUN-RULE's own refusal, caught by RUN-RULE itself and
turned into a report entry; never escapes."))

(defstruct (rule-report (:constructor %make-rule-report))
  "What one RUN-RULE did (spec §7).  OUTCOME is :DERIVED or :REFUSED;
DERIVED counts claims constructed this run, KEPT the previous
derivation's claims whose identity was derived again (ruling P10), SWEPT
the ones that were not and are now deleted; on :REFUSED the transaction
unwound, so all three are 0 and the previous derivation stands.
REFUSALS is a list of (TAG . TEXT): TAG a claim family name for a commit
refusal, else :RULE (compile, effects, ill-typed head), :BUDGET (the
rails) or :SOLUTIONS (the cap).  INFERENCES is the count at the last
solution; ELAPSED is seconds."
  rule-name version (outcome :derived) (derived 0) (kept 0) (swept 0)
  (disjoint-premises 0) (refusals '()) (inferences 0) (elapsed 0))

(defun rule-producer (name)
  "The producer string rule NAME writes as: rule/NAME."
  (format nil "rule/~A" name))

(defun %resolve-rule (graph rule)
  "RULE as a RULE-SPEC: a record or spec passes through; a name finds the
stored rule, else the def-rule, else signals.  A spec is tested first so
the common path never names the RULE type, which is no type at all until
some store has run DEF-RULES-SCHEMA."
  (cond
    ((rule-spec-p rule) rule)
    ((stringp rule)
     (let ((stored (and (%graph-declares-p graph 'rule)
                        (first (graph-db:index-lookup
                                graph 'rule '(name) rule)))))
       (cond ((and stored (not (graph-db:deleted-p stored)))
              (rule-spec-of stored))
             ((find-def-rule rule))
             (t (error "No rule named ~S in ~(~S~) or the image."
                       rule (graph-db:graph-name graph))))))
    (t (rule-spec-of rule))))

;;; Evaluating the body

(defun %solutions (compiled graph report)
  "Every solution of COMPILED's body under RUN-QUERY-GOALS' rails (spec
§7.2, ruling P4): a list of rows aligned with COMPILED-RULE-VARS.
SELECT's :SNAPSHOT inherits the open transaction on the single-store
path and the composed read snapshots on the cross-store one (S3-P2).
Refuses past *RULES-MAX-SOLUTIONS*."
  (let ((max-inferences (or *rules-max-inferences*
                            graph-db::*query-default-max-inferences*))
        (timeout (or *rules-timeout* graph-db::*query-default-timeout*))
        (cap (1+ *rules-max-solutions*))
        (rows '()))
    (unless (or max-inferences timeout)
      (error "RUN-RULE needs a resource bound: *RULES-MAX-INFERENCES* ~
or *RULES-TIMEOUT*, or the DSL defaults they fall back to (spec §7, ~
ruling P4)."))
    (let ((graph-db::*query-default-max-inferences* max-inferences)
          (graph-db::*query-default-timeout* timeout)
          (graph-db::*query-default-limit* cap))
      (graph-db::run-query-goals
       (compiled-rule-vars compiled) (compiled-rule-goals compiled) graph
       :limit cap :format :raw
       :callback (lambda (row)
                   (setf (rule-report-inferences report)
                         graph-db::*inference-count*)
                   (push row rows))))
    (when (> (length rows) *rules-max-solutions*)
      (error 'rule-run-refusal :tag :solutions
             :text (format nil "more than ~D solutions"
                           *rules-max-solutions*)))
    (nreverse rows)))

(defun %term-value (term row vars)
  "TERM's value in this solution: a literal passes, a variable reads its
column."
  (if (%variable-p term)
      (nth (position term vars) row)
      term))

(defun %namespace (value what)
  "VALUE -- a keyword, or the wire string a CLAIM/7 goal bound -- as the
keyword a claim stores.  A string naming no keyword this image recorded
cannot come from a claim, so it is a refusal, not an intern."
  (cond ((keywordp value) value)
        ((graph-db::%namespace-keyword value))
        (t (error 'rule-run-refusal :tag :rule
                  :text (format nil "~A namespace ~S names no namespace ~
of this image" what value)))))

(defun %key (value what)
  "VALUE as a claim key: a string passes, an integer renders decimal."
  (cond ((stringp value) value)
        ((integerp value) (format nil "~D" value))
        (t (error 'rule-run-refusal :tag :rule
                  :text (format nil "~A key must be a string, not ~S"
                                what value)))))

(defun %premise-extent (premises policy)
  "The validity extent a derived claim gets from PREMISES (spec §8):
(VALUES EXTENT DISJOINT-P).  Under :NONE nothing; under :PREMISES the
intersection of the premises that have one, NIL when none has, and
DISJOINT-P when they never held at once."
  (if (eq policy :none)
      (values nil nil)
      (let ((extents (remove nil (mapcar #'graph-db.spacetime:claim-extent
                                         premises))))
        (if (null extents)
            (values nil nil)
            (let ((acc (first extents)))
              (dolist (e (rest extents) (values acc nil))
                (setf acc (temporal-extent:extent-intersection
                           acc e :semantics :validity
                                 :standing :inferred))
                (when (null acc)
                  (return (values nil t)))))))))

(defun %dedupe-key (family sns skey rel ons okey extent)
  "The derived claim's identity, as spec §7.3 collapses duplicates on:
the endpoints and relation, plus the extent START for a temporal family
-- what CLAIM-IDENTITY-KEY keys on, less the producer, which is constant
here.  The start alone, so two solutions differing only in extent kind
collapse (recon C5, ruling P11)."
  (list sns skey rel ons okey
        (and (graph-db.spacetime:claim-family-temporal-p family)
             extent
             (graph-db.spacetime:extent-sexp-start-key
              (graph-db.spacetime:extent->sexp extent)))))

(defun %existing-key (claim family)
  "An existing claim's dedupe key, the same shape %DEDUPE-KEY gives a
solution, so the two match by EQUAL."
  (let ((binary (typep claim (graph-db.spacetime:claim-family-binary
                              family)))
        (sexp (graph-db.spacetime:claim-extent-sexp claim)))
    (list (graph-db.spacetime:claim-subject-namespace claim)
          (graph-db.spacetime:claim-subject-key claim)
          (graph-db.spacetime:claim-relation claim)
          (and binary (graph-db.spacetime:claim-object-namespace claim))
          (and binary (graph-db.spacetime:claim-object-key claim))
          (and (graph-db.spacetime:claim-family-temporal-p family)
               sexp
               (graph-db.spacetime:extent-sexp-start-key sexp)))))

(defun %constructor (family unary-p)
  "The MAKE-<CLASS> function of FAMILY's unary or binary class, interned
where that class is.  Built from SYMBOL-NAME, not FORMAT: ~A on a symbol
follows *PRINT-CASE*, so an operator who set it would send this to a
name nothing defines."
  (let ((class (if unary-p
                   (graph-db.spacetime:claim-family-unary family)
                   (graph-db.spacetime:claim-family-binary family))))
    (fdefinition (intern (concatenate 'string "MAKE-"
                                      (symbol-name class))
                         (symbol-package class)))))

(defun %extent-sexp-key (sexp)
  "SEXP with its timestamps rendered as EQUAL-comparable triples: a
LOCAL-TIME:TIMESTAMP is a CLOS object, so two equivalent stored extents
are EQUAL -- and EQUALP -- only when EQ (%TIMESTAMP-KEY's reason,
spacetime/claim.lisp)."
  (cond ((typep sexp 'local-time:timestamp)
         (graph-db.spacetime::%timestamp-key sexp))
        ((consp sexp) (cons (%extent-sexp-key (car sexp))
                            (%extent-sexp-key (cdr sexp))))
        (t sexp)))

(defun %refresh-kept (claim version extent &optional (method nil methodp))
  "CLAIM brought to the current derivation's VERSION, EXTENT and -- when
METHOD is supplied -- METHOD, by ONE copy and save when any differs; the
saved copy, else CLAIM.  EXTENT is what this run would derive, NIL under
:EXTENT-POLICY :NONE and for a DERIVED-FROM record.  A kept temporal
claim's extent START cannot move -- the dedupe key carries it -- but its
END can, and a refreshed extent can then overlap a sibling run, which
%VALIDATE-EXTENT-DISJOINTNESS refuses at commit like any other (#331,
docs/rules.md).  METHOD is refreshed rather than the record swept and
rewritten: MARK-DELETED releases a unique key only post-durability while
VALIDATE-UNIQUE-CONSTRAINTS runs pre-durability, so a sweep and a
re-insert of one identity in a transaction always collide (ruling P10)."
  (let* ((sexp (and extent (graph-db.spacetime:extent->sexp extent)))
         (extent-moved
           (not (equal (%extent-sexp-key sexp)
                       (%extent-sexp-key
                        (graph-db.spacetime:claim-extent-sexp claim)))))
         (version-moved
           (not (equal version
                       (graph-db.spacetime:claim-rule-version claim))))
         (method-moved
           (and methodp
                (not (equal method
                            (graph-db.spacetime:claim-method claim))))))
    (if (not (or extent-moved version-moved method-moved))
        claim
        (let ((c (graph-db:copy claim)))
          (when version-moved
            (setf (graph-db.spacetime:claim-rule-version c) version))
          (when extent-moved
            (setf (graph-db.spacetime:claim-extent c) extent))
          (when method-moved
            (setf (graph-db.spacetime:claim-method c) method))
          (graph-db:save c)
          c))))

(defun %store-name (graph)
  "GRAPH's name as a DERIVED-FROM record's METHOD carries it: the
downcased graph name, cl-llm's STORE-NAME convention (spec §10).  A
store a rule reads must be keyword-named (recon B5); %NORMALIZE-SCOPE
is where a scope is refused for it, before the rule is resolved, and
this CHECK-TYPE is belt and braces for every other caller."
  (let ((name (graph-db:graph-name graph)))
    (check-type name keyword)
    (string-downcase (symbol-name name))))

(defun %normalize-scope (graph scope)
  "SCOPE as RUN-RULE reads it: the open stores a body may read, GRAPH
first and once (spec §10).  Signals on anything that is not an open
store, and on a store that is not keyword-named -- %STORE-NAME
downcases its SYMBOL-NAME (recon B5) -- so both are refused before the
rule is resolved.  Duplicates are dropped: a store named twice would
answer every route twice and so double every solution."
  (dolist (g scope)
    (unless (typep g 'graph-db::graph)
      (error "RUN-RULE :SCOPE holds ~S, which is not an open store."
             g))
    (unless (keywordp (graph-db:graph-name g))
      (error "A store a rule reads must be keyword-named; ~S is not."
             (graph-db:graph-name g))))
  (cons graph (remove graph (remove-duplicates scope :test #'eq
                                                     :from-end t)
                      :test #'eq)))

(defun %premise-ref (node graph)
  "A premise as the reconcile carries it: (IDENTITY-KEY . STORE-NAME),
STORE-NAME NIL for the rule's own GRAPH (S3-P3).  Call this INSIDE the
snapshot NODE was read under: a node an INDEX-LOOKUP returned under a
snapshot is not self-contained once the snapshot exits (recon C4).
The RESOLVE-NODE-GRAPH fallback scans every open store and would, on
the single-store path, meet the cross-graph refusal inside the
transaction (GH #53) -- unreachable today, since every lookup stamps
NODE-GRAPH (recon B3)."
  (let ((home (or (graph-db::node-graph node)
                  (graph-db:resolve-node-graph (graph-db:id node)))))
    (cons (graph-db.spacetime:claim-identity-key node)
          (and home (not (eq home graph)) (%store-name home)))))

(defun %merge-store-name (old new)
  "The store name one premise keeps when it is named twice: the own
store's NIL beats any name, and between two names the FIRST wins
(S3-P3).  One rule for both merge sites -- %MERGE-PREMISE-REFS within a
solution set, %RECONCILE-PROVENANCE across derived claims."
  (and old new old))

(defun %merge-premise-refs (refs more)
  "REFS with MORE added, one entry per identity key, the store name
merged by %MERGE-STORE-NAME.  Order is first-seen, so which records a
run writes does not depend on hash order."
  (let ((out refs))
    (dolist (r more out)
      (let ((seen (assoc (car r) out :test #'string=)))
        (if (null seen)
            (setf out (nconc out (list r)))
            (setf (cdr seen)
                  (%merge-store-name (cdr seen) (cdr r))))))))

(defun %desired (compiled graph report)
  "The derivation the body asks for (spec §7.3): (VALUES TABLE ORDER),
TABLE a dedupe key -> (ARGS . PREMISES) hash and ORDER its keys in the
order the solutions first named them; disjoint solutions counted on
REPORT and dropped.  A premise is (IDENTITY-KEY . STORE-NAME), read
here and never as a node (S3-P3, recon C4).  Owns REPORT's disjoint
count, hence the reset: %DERIVE cannot clear it, since a cross-store
run evaluates once and reconciles per attempt."
  (setf (rule-report-disjoint-premises report) 0)
  (let* ((spec (compiled-rule-spec compiled))
         (family (compiled-rule-family compiled))
         (vars (compiled-rule-vars compiled))
         (policy (rule-spec-extent-policy spec))
         (unary-p (compiled-rule-unary-p compiled))
         (claims (make-hash-table :test 'equal))
         (order '()))
    (dolist (row (%solutions compiled graph report))
      (let* ((premises
               (remove-if-not
                #'graph-db::node-p
                (mapcar (lambda (v) (%term-value v row vars))
                        (compiled-rule-premise-vars compiled))))
             (sns (%namespace
                   (%term-value (compiled-rule-head-sns compiled)
                                row vars)
                   "subject"))
             (skey (%key (%term-value (compiled-rule-head-skey compiled)
                                      row vars)
                         "subject"))
             (ons (and (not unary-p)
                       (%namespace
                        (%term-value (compiled-rule-head-ons compiled)
                                     row vars)
                        "object")))
             (okey (and (not unary-p)
                        (%key (%term-value
                               (compiled-rule-head-okey compiled)
                               row vars)
                              "object"))))
        (multiple-value-bind (extent disjoint-p)
            (%premise-extent premises policy)
          (if disjoint-p
              (incf (rule-report-disjoint-premises report))
              ;; The refs after %PREMISE-EXTENT and inside the
              ;; evaluation, both on the nodes (recon C4).
              (let* ((refs (mapcar (lambda (n) (%premise-ref n graph))
                                   premises))
                     (key (%dedupe-key family sns skey
                                       (compiled-rule-relation compiled)
                                       ons okey extent))
                     (entry (gethash key claims)))
                (if entry
                    (setf (cdr entry)
                          (%merge-premise-refs (cdr entry) refs))
                    (progn
                      (setf (gethash key claims)
                            (cons (list* :subject-namespace sns
                                         :subject-key skey
                                         :extent extent
                                         (unless unary-p
                                           (list :object-namespace ons
                                                 :object-key okey)))
                                  (%merge-premise-refs '() refs)))
                      (push key order))))))))
    (values claims (nreverse order))))

(defun %reconcile-claims (compiled graph report desired order)
  "Ruling P10, inside the transaction: the producer's existing claims
kept when re-derived -- with the version AND the extent this run
derives, since neither is part of every identity -- deleted when not;
new identities constructed.  => an alist of (dedupe key . claim) for
every claim of the derivation that now stands."
  ;; CLAIMS-BY-PRODUCER overlays the open transaction's writes (GH
  ;; #324), so the producer's claims must be read BEFORE this function
  ;; writes any: the reconcile compares against the committed set.
  (let* ((spec (compiled-rule-spec compiled))
         (family (compiled-rule-family compiled))
         (producer (rule-producer (rule-spec-name spec)))
         (version (rule-spec-version spec))
         (parent (graph-db.spacetime:claim-family-parent family))
         ;; SEEN, not an ASSOC over STANDING: *RULES-MAX-SOLUTIONS* is
         ;; 100000, and the alist scan would be quadratic in it.
         (seen (make-hash-table :test 'equal))
         (standing '()))
    (dolist (c (graph-db.spacetime:claims-by-producer graph parent
                                                      producer))
      (let ((key (%existing-key c family)))
        (cond ((and (nth-value 1 (gethash key desired))
                    (not (gethash key seen)))
               (incf (rule-report-kept report))
               (setf (gethash key seen) t)
               ;; The extent %DESIRED holds for this key, i.e. the FIRST
               ;; solution's where several collapsed (ruling T4-R3) --
               ;; what a fresh construction would have used.
               (push (cons key (%refresh-kept
                                c version
                                (getf (car (gethash key desired))
                                      :extent)))
                     standing))
              (t
               (graph-db:mark-deleted c)
               (incf (rule-report-swept report))))))
    (let ((ctor (%constructor family (compiled-rule-unary-p compiled))))
      (dolist (key order)
        (unless (gethash key seen)
          (let ((claim (apply ctor :graph graph
                              :relation (compiled-rule-relation compiled)
                              :producer producer :rule-version version
                              :standing :inferred
                              (car (gethash key desired)))))
            (incf (rule-report-derived report))
            (setf (gethash key seen) t)
            (push (cons key claim) standing)))))
    standing))

(defun %reconcile-provenance (compiled graph desired standing)
  "Spec §9 under ruling P10: one DERIVED-FROM record per (derived claim,
premise) the derivation now asks for, its METHOD the premise's store
name and NIL for the rule's own store (spec §10); records the producer
wrote that it no longer asks for are deleted, the rest kept with their
RULE-VERSION and METHOD refreshed -- a premise that moved store renames
its record rather than being swept and rewritten, which would collide
on the family's unique key (%REFRESH-KEPT)."
  (let* ((spec (compiled-rule-spec compiled))
         (producer (rule-producer (rule-spec-name spec)))
         (version (rule-spec-version spec))
         (wanted (make-hash-table :test 'equal))
         (kept (make-hash-table :test 'equal)))
    (loop for (key . claim) in standing
          for derived-key = (graph-db.spacetime:claim-identity-key claim)
          do (dolist (p (cdr (gethash key desired)))
               (let ((pair (cons derived-key (car p))))
                 (multiple-value-bind (name seen) (gethash pair wanted)
                   ;; Same rule as within a solution set (S3-P3).
                   (setf (gethash pair wanted)
                         (if seen
                             (%merge-store-name name (cdr p))
                             (cdr p)))))))
    ;; One record per pair: a pair already kept is a duplicate and goes
    ;; with the records the derivation no longer asks for, as does a
    ;; record under this producer that is not a DERIVED-FROM at all.
    (dolist (r (graph-db.spacetime:claims-by-producer graph 'derivation
                                                      producer))
      (let ((pair (cons (graph-db.spacetime:claim-subject-key r)
                        (graph-db.spacetime:claim-object-key r))))
        (multiple-value-bind (name wanted-p) (gethash pair wanted)
          (if (and wanted-p
                   (not (gethash pair kept))
                   (string= "derived-from"
                            (graph-db.spacetime:claim-relation r)))
              (progn (setf (gethash pair kept) t)
                     ;; No extent on a DERIVATION record, ever; NIL never
                     ;; differs from the NIL it stores.
                     (%refresh-kept r version nil name))
              (graph-db:mark-deleted r)))))
    (maphash (lambda (pair name)
               (unless (gethash pair kept)
                 (make-derivation-binary
                  :graph graph :subject-namespace :claim
                  :subject-key (car pair) :relation "derived-from"
                  :object-namespace :claim :object-key (cdr pair)
                  :producer producer :rule-version version
                  :method name :standing :inferred)))
             wanted)))

(defun %derive (compiled graph report desired order)
  "Spec §7 as ruling P10 has it, inside the transaction: reconcile the
claims DESIRED/ORDER name, and their provenance, against what stands.
The three counts are REPORT's, so an attempt starts by clearing them:
CALL-WITH-TRANSACTION re-invokes its thunk on VALIDATION-CONFLICT
(*MAXIMUM-TRANSACTION-ATTEMPTS*, transactions.lisp) and a retry must
report what it did, not what it and every earlier attempt did.  On a
retry only this runs: RUN-RULE's caller computed DESIRED/ORDER, and a
cross-store evaluation is never repeated (S3-P2).  The disjoint count
is %DESIRED's for the same reason."
  (setf (rule-report-derived report) 0
        (rule-report-kept report) 0
        (rule-report-swept report) 0)
  (let ((standing (%reconcile-claims compiled graph report desired
                                     order)))
    (%reconcile-provenance compiled graph desired standing)
    report))

;;; Refusals

(defun %parent-of (class-name)
  "CLASS-NAME's claim family parent when it is an arity subclass, else
itself."
  (let ((f (find-if
            (lambda (f)
              (subtypep class-name
                        (graph-db.spacetime:claim-family-parent f)))
            (alexandria:hash-table-values
             graph-db.spacetime::*claim-families*))))
    (if f (graph-db.spacetime:claim-family-parent f) class-name)))

(defun %violation-family (c)
  "The claim family a commit refusal names, as the report's tag, else
:RULE.  The report's tag vocabulary is closed (docs/rules.md), so a
CONSTRAINT-VIOLATION none of the three name is the rule's own fault
rather than a fourth kind of tag."
  (typecase c
    (graph-db.spacetime:extent-disjointness-violation
     (graph-db.spacetime:edv-claim-class c))
    (graph-db:unique-constraint-violation
     (%parent-of (graph-db:ucv-class-name c)))
    (graph-db:value-constraint-violation
     (%parent-of (graph-db:vcv-class-name c)))
    (t :rule)))

(defun %under-snapshots (graphs thunk)
  "THUNK under one composed read snapshot per graph in GRAPHS (GH #53).
Each store is internally consistent; under a shared system clock the
epochs come from one counter, so they are comparable and equal when
nothing commits between the acquisitions (#168, recon C2).  Without one
they are not comparable at all."
  (if (null graphs)
      (funcall thunk)
      (graph-db:call-with-read-snapshot
       (lambda () (%under-snapshots (rest graphs) thunk))
       (first graphs))))

(defun run-rule (graph rule &key scope)
  "Derive RULE afresh and reconcile the result with its previous
derivation (spec §7, ruling P10).  RULE is a RULE record, a RULE-SPEC,
or a name (stored first, then DEF-RULE).  SCOPE (spec §10) is the open
stores the body may read, GRAPH put first and once; the rule writes
GRAPH alone.  => RULE-REPORT.

NIL or (GRAPH) is S2 exactly: evaluation and reconcile in one
transaction.  With another store in SCOPE the body is evaluated BEFORE
the write transaction, under %UNDER-SNAPSHOTS, because a read-write
transaction refuses every read of another store (S3-P2, GH #53).  The
trap: that evaluation is not serialised against a premise committed
after its snapshots -- the next run sees such a premise, this one does
not.

A refusal of any kind -- compile, the rails, effects, a commit
constraint, a missing extent -- is reported, never signalled, and
unwinds the whole run so the previous derivation stands; an operator
error (no resource bound, no such rule, a SCOPE that is not open
stores) signals."
  (let* ((scope (%normalize-scope graph scope))
         (foreign (rest scope))
         (spec (%resolve-rule graph rule))
         (report (%make-rule-report :rule-name (rule-spec-name spec)
                                    :version (rule-spec-version spec)))
         (start (get-internal-real-time))
         (compiled nil))
    (flet ((refuse (tag text)
             (setf (rule-report-outcome report) :refused
                   (rule-report-derived report) 0
                   (rule-report-kept report) 0
                   (rule-report-swept report) 0
                   (rule-report-disjoint-premises report) 0)
             (setf (rule-report-refusals report)
                   (append (rule-report-refusals report)
                           (list (cons tag text))))))
      ;; Compiled outside the transaction and in its own HANDLER-CASE:
      ;; the family a commit refusal is tagged with is known only after.
      (handler-case
          (progn
            ;; DERIVATION too: without it there is nowhere to record
            ;; provenance, and the store never ran DEF-RULES-SCHEMA.
            (dolist (needed (list (rule-spec-family spec) 'derivation))
              (unless (%graph-declares-p graph needed)
                (error 'rule-run-refusal :tag :rule
                       :text (format nil "~(~S~) does not carry ~(~A~)"
                                     (graph-db:graph-name graph)
                                     needed))))
            (setf compiled (compile-rule graph spec)))
        (rule-compile-error (c)
          (refuse :rule (rule-compile-error-reason c)))
        (rule-run-refusal (c)
          (refuse (rule-run-refusal-tag c) (rule-run-refusal-text c))))
      (when compiled
        (let ((family (graph-db.spacetime:claim-family-parent
                       (compiled-rule-family compiled))))
          (handler-case
              (flet ((evaluate ()
                       (let ((graph-db::*claim-scope* scope))
                         (%desired compiled graph report))))
                (if foreign
                    (multiple-value-bind (desired order)
                        (%under-snapshots scope #'evaluate)
                      (graph-db:with-transaction (:graph graph)
                        (%derive compiled graph report desired order)))
                    (graph-db:with-transaction (:graph graph)
                      (multiple-value-bind (desired order) (evaluate)
                        (%derive compiled graph report desired
                                 order)))))
            (rule-run-refusal (c)
              (refuse (rule-run-refusal-tag c)
                      (rule-run-refusal-text c)))
            ;; Before PROLOG-ERROR, its superclass (recon A16, PF2).
            (graph-db:prolog-permission-error (c)
              (refuse :rule (princ-to-string c)))
            (graph-db:prolog-error (c)
              (refuse :budget (princ-to-string c)))
            (graph-db.spacetime:missing-claim-identity-component (c)
              (refuse family (princ-to-string c)))
            (graph-db:constraint-violation (c)
              (refuse (%violation-family c) (princ-to-string c)))
            (graph-db:query-precondition-error (c)
              (refuse :rule (princ-to-string c)))))))
    (setf (rule-report-elapsed report)
          (/ (- (get-internal-real-time) start)
             (float internal-time-units-per-second 1.0d0)))
    report))

;;; Every rule a store can run

(defun %runnable-spec-p (graph spec)
  "SPEC is one RUN-RULES considers: a stored rule always, a DEF-RULE only
where the store carries its family (ruling P8).  A def-rule the store
cannot run is skipped silently rather than reported -- it is not this
store's rule, and its text names types this schema does not have."
  (or (eq (rule-spec-source spec) :stored)
      (%graph-declares-p graph (rule-spec-family spec))))

(defun %dependency-order (compiled-rules)
  "COMPILED-RULES sorted so a rule runs after EVERY rule deriving a
relation it reads (spec §7).  Ready means no rule still PENDING derives
anything it reads: one producer of a relation having run is not enough,
since a reader scheduled between two producers of one relation sees
half its premises.  Cycles were refused at compile, so a pending set
always holds a ready rule; ties keep the input order."
  (let ((pending (copy-list compiled-rules))
        (done '()))
    (loop while pending do
      (let ((ready (find-if
                    (lambda (c)
                      (let ((reads (compiled-rule-reads c)))
                        (when (eq reads :any)
                          (error "RUN-RULES: compile-rule admits no ~
:any reads, but ~S has them."
                                 (rule-spec-name (compiled-rule-spec c))))
                        (every (lambda (r)
                                 (notany
                                  (lambda (o)
                                    (string=
                                     r (compiled-rule-relation o)))
                                  pending))
                               reads)))
                    pending)))
        (unless ready
          (error "RUN-RULES: no runnable rule among ~S -- a cycle the ~
compiler should have refused."
                 (mapcar (lambda (c)
                           (rule-spec-name (compiled-rule-spec c)))
                         pending)))
        (setf pending (remove ready pending))
        (push ready done)))
    (nreverse done)))

(defun run-rules (graph &key scope)
  "Every enabled rule GRAPH can run -- its stored rules, plus the
DEF-RULEs whose family it carries (ruling P8) -- each through RUN-RULE in
dependency order (spec §7), with SCOPE (spec §10) normalised once here
and passed to every one -- so a scope that is not open stores signals
even when no rule is runnable.  A rule that does not compile is
reported :REFUSED and skipped; the rest still run.  => the reports, the
refused ones first and then the rest in the order run.  Compile and the
dependency order stay single-store, so a cycle through another store's
rules is not detected (S3-P5)."
  (let ((scope (%normalize-scope graph scope))
        (reports '())
        (compiled '()))
    (dolist (spec (rules-in-scope graph))
      (when (%runnable-spec-p graph spec)
        (handler-case (push (compile-rule graph spec) compiled)
          (rule-compile-error (c)
            (push (%make-rule-report
                   :rule-name (rule-spec-name spec)
                   :version (rule-spec-version spec)
                   :outcome :refused
                   :refusals (list (cons :rule
                                         (rule-compile-error-reason c))))
                  reports)))))
    (dolist (c (%dependency-order (nreverse compiled)))
      (push (run-rule graph (compiled-rule-spec c) :scope scope)
            reports))
    (nreverse reports)))

;;; Provenance reads (spec §9)

(defun %claim-by-identity-key (graph key)
  "The claim KEY names, resolved through the subject-relation index of
every family this graph carries; a current one over a retracted one.  NIL
when nothing in the store has that identity now."
  (multiple-value-bind (producer sns skey rel)
      (graph-db.spacetime:split-claim-identity-key key)
    (declare (ignore producer))
    (let ((found '()))
      (dolist (family (alexandria:hash-table-values
                       graph-db.spacetime::*claim-families*))
        (handler-case
            (dolist (c (graph-db:index-lookup
                        graph
                        (graph-db.spacetime:claim-family-parent family)
                        graph-db::+claim-subject-relation-index-slots+
                        (list sns skey rel)))
              (when (string= key
                             (graph-db.spacetime:claim-identity-key c))
                (push c found)))
          ;; A family this graph does not index, as %PRODUCER-CANDIDATES
          ;; reads it (rules/facts.lisp): no candidates, not a fault.
          (graph-db:query-precondition-error () nil)))
      (or (find-if #'graph-db.spacetime:claim-current-p found)
          (first found)))))

(defun premises-of (graph claim &key (scope (list graph)))
  "The claims CLAIM was derived from (spec §9): its DERIVED-FROM records'
objects, resolved back to claims.  A record's METHOD names the store its
premise came from (spec §10): the premise resolves there when that store
is in SCOPE, in GRAPH when METHOD is NIL, and is DROPPED when the named
store is not in SCOPE -- fewer premises, never wrong ones (S3-P4,
cl-llm's :ABSENT convention).  A premise whose identity no longer exists
in its store is dropped too, and a DERIVATION record of any other
relation is not provenance and is not read.  Trap: a foreign store in
SCOPE is read here, so call this OUTSIDE a transaction (GH #53)."
  (let ((records (graph-db.spacetime:claims-touching
                  graph 'derivation :claim
                  (graph-db.spacetime:claim-identity-key claim)
                  :role :subject :relation "derived-from")))
    (remove nil
            (mapcar
             (lambda (r)
               (let* ((name (graph-db.spacetime:claim-method r))
                      (store (if name
                                 (find name scope :key #'%store-name
                                                  :test #'string=)
                                 graph)))
                 (when store
                   (%claim-by-identity-key
                    store (graph-db.spacetime:claim-object-key r)))))
             records))))

(defun dependents-of (graph claim &key current)
  "Every derived claim whose provenance names CLAIM (spec §9) -- one
CLAIMS-TOUCHING on the object endpoint, DERIVED-FROM records only, then
the subjects resolved.  With CURRENT, only dependents still believed.
Nothing is re-derived.  No scope: the records and the dependents are
GRAPH's whatever store CLAIM lives in, and a premise is named by its
identity key (spec §10)."
  (let* ((records (graph-db.spacetime:claims-touching
                   graph 'derivation :claim
                   (graph-db.spacetime:claim-identity-key claim)
                   :role :object :relation "derived-from"))
         (claims (remove nil
                         (mapcar
                          (lambda (r)
                            (%claim-by-identity-key
                             graph
                             (graph-db.spacetime:claim-subject-key r)))
                          records))))
    (if current
        (remove-if-not #'graph-db.spacetime:claim-current-p claims)
        claims)))
