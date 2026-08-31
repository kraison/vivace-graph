;;;; Membership disjointness -- unit 4b of the ontology epic (GH #157,
;;;; #109): at most one LIVE membership claim per subject within a
;;;; declared set of membership-object keys.
;;;;
;;;; Shaped by the tenant answer on #157: seen from the claim store, the
;;;; disjoint set is a set of VALUES -- class-designator object keys --
;;;; not classes in this store's schema, because a record's class is
;;;; derived per ingest and only expressible as a claim.  Membership is a
;;;; binary claim (subject = the record's external key, object = the
;;;; class designator, one canonical relation); reclassification is
;;;; retract-then-assert in ONE transaction (RETRACT-CLAIM joins it), and
;;;; the commit check evaluates the POST-commit state through the commit
;;;; view, so the retracted sibling does not count and the asserted one
;;;; does.  Runs via GRAPH-DB:*COMMIT-VALIDATORS* -- the substrate cannot
;;;; be named by core.  Design: the 4b addendum in
;;;; docs/superpowers/specs/2026-08-30-disjointness-design.md.

(in-package #:graph-db.spacetime)

(define-condition membership-disjointness-violation (error)
  ((name        :initarg :name        :reader mdv-name)
   (subject-namespace :initarg :subject-namespace
                      :reader mdv-subject-namespace)
   (subject-key  :initarg :subject-key :reader mdv-subject-key)
   (members      :initarg :members     :reader mdv-members))
  (:report
   (lambda (c s)
     (format s "Membership disjointness ~S violated: subject (~S ~S) ~
                would be left with ~D live membership claims:~{ ~S~}.  ~
                Reclassify with RETRACT-CLAIM then the new claim, in one ~
                transaction."
             (mdv-name c) (mdv-subject-namespace c) (mdv-subject-key c)
             (length (mdv-members c)) (mdv-members c)))))

(defvar *membership-disjointness-metadata* (make-hash-table)
  "graph-name (symbol) -> list of MEMBERSHIP-DISJOINTNESS-SPECs.")

(defstruct (membership-disjointness-spec
            (:conc-name mds-)
            (:constructor make-membership-disjointness-spec))
  claim-class relation object-namespace object-keys graph-name name)

(defun %mds-identity (spec)
  "(CLAIM-CLASS . NAME); :NAME is mandatory, as 4a's specs (note §2)."
  (graph-db::%spec-identity (mds-claim-class spec) nil (mds-name spec)))

(defun register-membership-disjointness-spec (spec)
  "Validate and record SPEC, REPLACING any spec of the same identity."
  (unless (mds-name spec)
    (error "DEF-DISJOINT-MEMBERSHIP on ~S has no :NAME; the name IS the ~
            identity (GH #139, #157)."
           (mds-claim-class spec)))
  (unless (canonical-relation-p (mds-relation spec))
    (error "DEF-DISJOINT-MEMBERSHIP ~S: :RELATION ~S is not a canonical ~
            relation string (GH #160)."
           (mds-name spec) (mds-relation spec)))
  (setf (mds-object-keys spec)
        (sort (remove-duplicates (copy-list (mds-object-keys spec))
                                 :test #'equal)
              #'string<))
  (when (< (length (mds-object-keys spec)) 2)
    (error "DEF-DISJOINT-MEMBERSHIP ~S names fewer than two distinct ~
            object keys." (mds-name spec)))
  (claim-family (mds-claim-class spec))   ; signals on an unknown family
  (let* ((g (mds-graph-name spec))
         (id (%mds-identity spec))
         (existing (gethash g *membership-disjointness-metadata*))
         (hit (find id existing :key #'%mds-identity :test #'equal)))
    (setf (gethash g *membership-disjointness-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-membership-disjointness-spec (claim-class graph-name name)
  (let* ((id (graph-db::%spec-identity claim-class nil name))
         (existing (gethash graph-name *membership-disjointness-metadata*))
         (hit (find id existing :key #'%mds-identity :test #'equal)))
    (when hit
      (setf (gethash graph-name *membership-disjointness-metadata*)
            (remove hit existing))
      t)))

(defmacro def-disjoint-membership (claim-class graph-name
                                   &key relation object-namespace
                                        object-keys name)
  "Declare that a subject holds at most ONE live membership claim of
CLAIM-CLASS's family in GRAPH-NAME whose RELATION and OBJECT-NAMESPACE
match and whose object key is one of OBJECT-KEYS (strings; the set is
canonicalised, so either order is one declaration; :NAME is required and
is the identity).  Enforced at commit through the commit view: a second
live membership is refused with MEMBERSHIP-DISJOINTNESS-VIOLATION, and a
sibling retracted in the SAME transaction does not count -- RETRACT-CLAIM
joins the transaction, so reclassification is one atomic
retract-then-assert (GH #157 4b)."
  `(register-membership-disjointness-spec
    (make-membership-disjointness-spec
     :claim-class ',claim-class
     :relation ,relation
     :object-namespace ,object-namespace
     :object-keys (list ,@object-keys)
     :graph-name ',graph-name
     :name ',name)))

(defmacro undef-disjoint-membership (claim-class graph-name &key name)
  "Withdraw a DEF-DISJOINT-MEMBERSHIP declaration by :NAME.  Warns
SCHEMA-WITHDRAWAL-MATCHED-NOTHING when nothing matches (GH #152)."
  `(graph-db::%withdrawn-p
    (unregister-membership-disjointness-spec ',claim-class ',graph-name
                                             ',name)
    :disjoint-membership ',claim-class ',graph-name ',name nil))

;;; --- Evaluation -----------------------------------------------------------

(defun %membership-claim-p (c spec)
  "True when claim C is a LIVE membership claim of SPEC's set."
  (let ((family (claim-family (mds-claim-class spec))))
    (and (typep c (claim-family-binary family))
         (not (graph-db:deleted-p c))
         (equal (mds-relation spec) (claim-relation c))
         (eq (mds-object-namespace spec) (claim-object-namespace c))
         (member (claim-object-key c) (mds-object-keys spec)
                 :test #'equal)
         (claim-current-p c))))

(defun %post-commit-subject-claims (view graph family subject-ns
                                    subject-key)
  "The subject's claims as the committing transaction will leave them:
the store's subject-index rows each mapped through the view (an updated
or retracted row in its post-commit version, a deleted one absent), plus
the claims this transaction CREATES for the subject, which the index
does not hold yet.  The view-lookup shape the evaluator note deferred to
the first unit needing it -- this one."
  (let* ((store (graph-db:index-lookup graph (claim-family-parent family)
                                       '(subject-namespace subject-key)
                                       (list subject-ns subject-key)))
         (post (loop for c in store
                     for v = (graph-db:view-node view (graph-db:id c))
                     when v collect v)))
    (dolist (w (graph-db:view-writes view) post)
      (let ((node (graph-db::node w)))
        ;; There is NO shared CLAIM class -- each family's PARENT holds
        ;; the spliced slots -- so membership in the family is the test.
        (when (and (typep w 'graph-db::tx-create)
                   (typep node (claim-family-parent family))
                   (eq (claim-subject-namespace node) subject-ns)
                   (equal (claim-subject-key node) subject-key)
                   (not (member (graph-db:id node) post
                                :key #'graph-db:id :test #'equalp)))
          (push node post))))))

(defun %validate-membership-disjointness (tx graph)
  "The commit validator (GRAPH-DB:*COMMIT-VALIDATORS*).  For every
written live membership claim of a declared set, the subject must be
left with at most one -- counted over post-commit state (GH #157 4b)."
  (let ((specs (gethash (graph-db:graph-name graph)
                        *membership-disjointness-metadata*)))
    (when specs
      (let ((view (graph-db:make-commit-view graph tx))
            (seen (make-hash-table :test 'equal)))
        (dolist (w (graph-db::writes tx))
          (let ((node (graph-db::node w)))
            (when (typep node 'graph-db:vertex)  ; claims are vertices;
              ;; %MEMBERSHIP-CLAIM-P checks the family class itself --
              ;; there is no shared CLAIM class to test against.
              (dolist (spec specs)
                (when (%membership-claim-p node spec)
                  (let* ((ns (claim-subject-namespace node))
                         (key (claim-subject-key node))
                         (dedupe (list (mds-name spec) ns key)))
                    (unless (gethash dedupe seen)
                      (setf (gethash dedupe seen) t)
                      (let* ((family (claim-family
                                      (mds-claim-class spec)))
                             (members
                               (remove-if-not
                                (lambda (c) (%membership-claim-p c spec))
                                (%post-commit-subject-claims
                                 view graph family ns key))))
                        (when (> (length members) 1)
                          (error 'membership-disjointness-violation
                                 :name (mds-name spec)
                                 :subject-namespace ns
                                 :subject-key key
                                 :members
                                 (mapcar #'claim-object-key
                                         members)))))))))))))))

(pushnew '%validate-membership-disjointness graph-db:*commit-validators*)

(defun check-disjoint-memberships (graph)
  "Survey GRAPH's declared membership sets without signalling.  Returns
 (values VIOLATIONS CHECKED SPEC-COUNT); VIOLATIONS are
 (NAME SUBJECT-NS SUBJECT-KEY MEMBER-KEYS) lists.  Zero violations over
zero specs is an unchecked graph, as every audit here."
  (let ((specs (gethash (graph-db:graph-name graph)
                        *membership-disjointness-metadata*))
        (violations '())
        (checked 0))
    (dolist (spec specs)
      (let ((family (claim-family (mds-claim-class spec)))
            (by-subject (make-hash-table :test 'equal)))
        (graph-db:map-vertices
         (lambda (c)
           (incf checked)
           (when (%membership-claim-p c spec)
             (push (claim-object-key c)
                   (gethash (list (claim-subject-namespace c)
                                  (claim-subject-key c))
                            by-subject))))
         graph :vertex-type (claim-family-binary family))
        (maphash (lambda (subject keys)
                   (when (> (length keys) 1)
                     (push (list (mds-name spec)
                                 (first subject) (second subject) keys)
                           violations)))
                 by-subject)))
    (values (nreverse violations) checked (length specs))))
