;;;; Temporal claim families (GH #296): live claims sharing a BASE TUPLE
;;;; -- producer, subject, object, relation -- must have pairwise disjoint
;;;; validity extents, so a state series (A -> B -> A) is several claims
;;;; and no instant sees two.  The identity half (the extent start in the
;;;; DEF-UNIQUE tuple) is in DEF-CLAIM-CLASSES; the reads are CLAIMS-
;;;; TOUCHING's :AT / :DURING.  Runs via GRAPH-DB:*COMMIT-VALIDATORS*
;;;; through the commit view, as membership does.  Design:
;;;; docs/superpowers/specs/2026-09-01-temporal-claim-families-design.md.

(in-package #:graph-db.spacetime)

(define-condition extent-disjointness-violation (error)
  ((claim-class :initarg :claim-class :reader edv-claim-class)
   (subject-namespace :initarg :subject-namespace
                      :reader edv-subject-namespace)
   (subject-key :initarg :subject-key :reader edv-subject-key)
   (object-namespace :initarg :object-namespace :initform nil
                     :reader edv-object-namespace)
   (object-key :initarg :object-key :initform nil :reader edv-object-key)
   (relation :initarg :relation :reader edv-relation)
   (conflicting-ids :initarg :conflicting-ids :reader edv-conflicting-ids))
  (:report
   (lambda (c s)
     (format s "Temporal family ~S: (~S ~S) ~S~@[ (~S ~S)~] would have ~
                ~D live claims with overlapping validity (~{~A~^, ~}).  ~
                Runs of one base tuple must be pairwise disjoint; close ~
                the old run with RETRACT-CLAIM, or extend it (COPY, SETF ~
                CLAIM-EXTENT, SAVE) instead of adding one."
             (edv-claim-class c)
             (edv-subject-namespace c) (edv-subject-key c)
             (edv-relation c)
             (edv-object-namespace c) (edv-object-key c)
             (length (edv-conflicting-ids c))
             (mapcar #'graph-db::string-id (edv-conflicting-ids c))))))

(defun %temporal-families ()
  (loop for f being the hash-values of *claim-families*
        when (claim-family-temporal-p f) collect f))

(defun %live-claim-p (c)
  (and (not (graph-db:deleted-p c)) (claim-current-p c)))

(defun %same-base-tuple-p (a b binary)
  "True when claims A and B share everything in the identity tuple but
the extent.  Arity is part of it: a unary and a binary claim never share
a tuple, whatever else agrees."
  (and (equal (claim-producer a) (claim-producer b))
       (equal (claim-relation a) (claim-relation b))
       (eq (claim-subject-namespace a) (claim-subject-namespace b))
       (equal (claim-subject-key a) (claim-subject-key b))
       (if (typep a binary)
           (and (typep b binary)
                (eq (claim-object-namespace a) (claim-object-namespace b))
                (equal (claim-object-key a) (claim-object-key b)))
           (not (typep b binary)))))

(defun %overlapping-siblings (node candidates binary)
  "The live CANDIDATES sharing NODE's base tuple whose validity is not
disjoint from NODE's; NODE itself excluded by id."
  (let ((e (claim-extent node)))
    (remove-if-not (lambda (c)
                     (and (not (equalp (graph-db:id c) (graph-db:id node)))
                          (%live-claim-p c)
                          (%same-base-tuple-p node c binary)
                          (not (extents-disjoint-p e (claim-extent c)))))
                   candidates)))

(defun %validate-extent-disjointness (tx graph)
  "The commit validator (GRAPH-DB:*COMMIT-VALIDATORS*).  Every written
live claim of a temporal family must be disjoint in validity from every
other live claim of its base tuple, counted over post-commit state: a
run retracted in this transaction does not count, one created in it
does (GH #296, design §2.3)."
  (let ((families (%temporal-families)))
    (when families
      (let ((view nil)
            (seen (make-hash-table :test 'equalp)))
        (dolist (w (graph-db::writes tx))
          (let* ((node (graph-db::node w))
                 (family (and (typep node 'graph-db:vertex)
                              (find-if (lambda (f)
                                         (typep node
                                                (claim-family-parent f)))
                                       families))))
            (when (and family
                       (%live-claim-p node)
                       (not (gethash (graph-db:id node) seen)))
              (setf (gethash (graph-db:id node) seen) t)
              (unless view
                (setf view (graph-db:make-commit-view graph tx)))
              (let* ((binary (claim-family-binary family))
                     (others (%overlapping-siblings
                              node
                              (%post-commit-subject-claims
                               view graph family
                               (claim-subject-namespace node)
                               (claim-subject-key node))
                              binary)))
                (when others
                  (error 'extent-disjointness-violation
                         :claim-class (claim-family-parent family)
                         :subject-namespace (claim-subject-namespace node)
                         :subject-key (claim-subject-key node)
                         :object-namespace
                         (and (typep node binary)
                              (claim-object-namespace node))
                         :object-key (and (typep node binary)
                                          (claim-object-key node))
                         :relation (claim-relation node)
                         :conflicting-ids
                         (cons (graph-db:id node)
                               (mapcar #'graph-db:id others))))))))))))

(pushnew '%validate-extent-disjointness graph-db:*commit-validators*)

(defun %base-tuple-key (c binary)
  (list (claim-producer c) (claim-subject-namespace c)
        (claim-subject-key c) (claim-relation c)
        (and (typep c binary) (claim-object-namespace c))
        (and (typep c binary) (claim-object-key c))
        (and (typep c binary) t)))

(defun check-extent-disjointness (graph claim-class)
  "Survey CLAIM-CLASS's family in GRAPH for live claims of one base tuple
whose validity overlaps -- runs written before the family was temporal,
or past the check.  Returns (values VIOLATIONS CHECKED); each violation
is a plist (:PRODUCER :SUBJECT-NAMESPACE :SUBJECT-KEY :RELATION
:OBJECT-NAMESPACE :OBJECT-KEY :IDS), IDS the overlapping claims.  A
non-temporal family returns (VALUES NIL 0): nothing to audit, which is
not the same as a clean audit."
  (let ((family (claim-family claim-class)))
    (if (not (claim-family-temporal-p family))
        (values nil 0)
        (let ((binary (claim-family-binary family))
              (groups (make-hash-table :test 'equal))
              (checked 0)
              (violations '()))
          (graph-db:map-vertices
           (lambda (c)
             (incf checked)
             (when (%live-claim-p c)
               (push c (gethash (%base-tuple-key c binary) groups))))
           graph :vertex-type (claim-family-parent family))
          (maphash
           (lambda (key claims)
             (let ((bad (remove-if-not
                         (lambda (a)
                           (some (lambda (b)
                                   (and (not (eq a b))
                                        (not (extents-disjoint-p
                                              (claim-extent a)
                                              (claim-extent b)))))
                                 claims))
                         claims)))
               (when bad
                 (destructuring-bind (producer ns key relation ons okey
                                      binary-p)
                     key
                   (declare (ignore binary-p))
                   (push (list :producer producer
                               :subject-namespace ns :subject-key key
                               :relation relation
                               :object-namespace ons :object-key okey
                               :ids (mapcar #'graph-db:id bad))
                         violations)))))
           groups)
          (values (nreverse violations) checked)))))
