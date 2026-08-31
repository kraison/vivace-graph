;;;; Disjointness over vertex types -- unit 4a of the ontology epic
;;;; (GH #157, #109): a declaration that no node may instantiate two of a
;;;; set of classes.
;;;;
;;;; A SCHEMA LINT, not a constraint family.  A node's class is fixed at
;;;; creation and nothing retypes it, so the set of classes a node
;;;; instantiates is exactly its class's ancestors -- fixed by the class
;;;; graph at definition time.  Either no defined class has two of the set
;;;; among its ancestors, and the declaration is vacuously true forever, or
;;;; one does, and EVERY instance of it violates: the declaration
;;;; contradicts the schema.  No commit can create or repair that, so the
;;;; check runs at DEF-DISJOINT time, at every later DEF-VERTEX / DEF-EDGE
;;;; (through *NODE-TYPE-DEFINITION-HOOKS*), and in an audit pass -- never
;;;; on the commit path.  Claim-asserted membership (4b) is a different
;;;; feature and is not here.  Design:
;;;; docs/superpowers/specs/2026-08-30-disjointness-design.md.
;;;;
;;;; Sixth schema registry, sharing %SPEC-IDENTITY (GH #139/#140).  :NAME
;;;; is MANDATORY: a set of classes has no natural owner, so identity is
;;;; (OWNER . NAME) with OWNER derived from the canonicalised set, and
;;;; (disjoint a b) / (disjoint b a) are one declaration.

(in-package :graph-db)

(define-condition disjointness-violation (error)
  ((classes   :initarg :classes   :reader djv-classes)
   (offender  :initarg :offender  :reader djv-offender)
   (name      :initarg :name      :reader djv-name))
  (:report
   (lambda (c s)
     (format s "Disjointness ~S over~{ ~S~} violated: ~S instantiates ~
                more than one of them."
             (djv-name c) (djv-classes c) (djv-offender c)))))

(defvar *schema-disjointness-metadata* (make-hash-table)
  "graph-name (symbol) -> list of DISJOINTNESS-SPECs (newest first).")

(defstruct (disjointness-spec (:constructor make-disjointness-spec))
  classes graph-name name)

(defun %canonical-class-set (classes)
  "CLASSES sorted by symbol name, duplicates removed: the same set written
in any order is one declaration.  Sorting by name, not by symbol, so two
same-named symbols from different packages stay distinct."
  (sort (remove-duplicates (copy-list classes))
        #'string< :key #'symbol-name))

(defun disjointness-spec-identity (spec)
  "(OWNER . NAME), OWNER the first class of the canonicalised set.  See
%SPEC-IDENTITY (index.lisp) and the disjointness note §2."
  (%spec-identity (first (disjointness-spec-classes spec)) nil
                  (disjointness-spec-name spec)))

;;; --- The class graph ------------------------------------------------------

(defun %class-ancestors (class)
  "CLASS and every class above it, by direct superclasses -- needs no
finalization, so a class mid-definition can be asked."
  (let ((seen '()))
    (labels ((walk (c)
               (unless (member c seen)
                 (push c seen)
                 (mapc #'walk (class-direct-superclasses c)))))
      (walk class))
    seen))

(defun %node-classes ()
  "Every CURRENT node class in the image: the transitive subclasses of
VERTEX and EDGE whose name still resolves to them.  A class object
orphaned by a redefinition or an (SETF FIND-CLASS NIL) stays linked from
its old superclasses' direct-subclass lists and is not a class anything
can instantiate, so it must not count."
  (let ((seen '()))
    (labels ((walk (c)
               (unless (member c seen)
                 (push c seen)
                 (mapc #'walk (class-direct-subclasses c)))))
      (walk (find-class 'vertex))
      (walk (find-class 'edge)))
    (remove-if-not (lambda (c) (eq (find-class (class-name c) nil) c))
                   seen)))

(defun %disjointness-offender-p (class spec)
  "True when CLASS instantiates two or more of SPEC's classes -- itself
included, so a declared class that is a subtype of another declared class
offends by itself."
  (let ((ancestors (mapcar #'class-name (%class-ancestors class))))
    (>= (count-if (lambda (c) (member c ancestors))
                  (disjointness-spec-classes spec))
        2)))

(defun %check-class-against-spec (class spec)
  (when (%disjointness-offender-p class spec)
    (error 'disjointness-violation
           :classes (disjointness-spec-classes spec)
           :offender (class-name class)
           :name (disjointness-spec-name spec))))

;;; --- Registry --------------------------------------------------------------

(defun register-disjointness-spec (spec)
  "Lint SPEC against the class graph as it stands, then record it,
REPLACING any spec of the same identity (GH #139).  Signals when SPEC is
unnamed, names fewer than two classes, or is already contradicted by a
defined class -- a contradictory declaration is an error at definition,
never a silent no-op.  A class in the set that is not yet defined is
allowed; it is checked when it is defined."
  (unless (disjointness-spec-name spec)
    (error "DEF-DISJOINT over~{ ~S~} has no :NAME.  A set of classes has ~
            no natural owner, so the name IS the identity (GH #139)."
           (disjointness-spec-classes spec)))
  (setf (disjointness-spec-classes spec)
        (%canonical-class-set (disjointness-spec-classes spec)))
  (when (< (length (disjointness-spec-classes spec)) 2)
    (error "DEF-DISJOINT ~S names fewer than two distinct classes."
           (disjointness-spec-name spec)))
  (dolist (class (%node-classes))
    (%check-class-against-spec class spec))
  (let* ((g (disjointness-spec-graph-name spec))
         (id (disjointness-spec-identity spec))
         (existing (gethash g *schema-disjointness-metadata*))
         (hit (find id existing :key #'disjointness-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-disjointness-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-disjointness-spec (graph-name name)
  "Withdraw the declaration called NAME in GRAPH-NAME.  T if one was.
⚠ A NAME interned in another package withdraws nothing (GH #152)."
  (let* ((existing (gethash graph-name *schema-disjointness-metadata*))
         (hit (find name existing :key #'disjointness-spec-name)))
    (when hit
      (setf (gethash graph-name *schema-disjointness-metadata*)
            (remove hit existing))
      t)))

(defmacro def-disjoint ((&rest classes) graph-name &key name)
  "Declare that no node in GRAPH-NAME instantiates two of CLASSES: none is
a subtype of another, and no defined class inherits from two.  Checked
NOW against every defined node class, and again whenever a node type is
defined later -- never at commit, since no write can change the answer
(GH #157, 4a).  Order does not matter; :NAME is required and is the
declaration's identity, so re-declaring the same name replaces.

⚠ A DEF-VERTEX the hook refuses is refused AFTER its DEFCLASS: the class
exists in the image, unregistered and without helpers, and any later
declaration over the same pair sees it.  Fix the definition and
re-evaluate; the leftover cannot be un-defined portably."
  `(register-disjointness-spec
    (make-disjointness-spec :classes ',classes
                            :graph-name ',graph-name
                            :name ',name)))

(defmacro undef-disjoint (graph-name &key name)
  "Withdraw a DEF-DISJOINT declaration by :NAME.  Warns
SCHEMA-WITHDRAWAL-MATCHED-NOTHING when nothing matches (GH #152)."
  `(%withdrawn-p (unregister-disjointness-spec ',graph-name ',name)
                 :disjointness nil ',graph-name ',name nil))

;;; --- The definition-time hook and the audit ---------------------------

(defun %check-disjointness-on-definition (class-name)
  "Run for every node type as it is defined (%INSTALL-NODE-TYPE): a class
that inherits from two declared-disjoint classes, in any store's
declarations, is refused at definition."
  (let ((class (find-class class-name nil)))
    (when class
      (maphash (lambda (graph-name specs)
                 (declare (ignore graph-name))
                 (dolist (spec specs)
                   (%check-class-against-spec class spec)))
               *schema-disjointness-metadata*))))

(pushnew '%check-disjointness-on-definition *node-type-definition-hooks*)

(defun check-disjointness (graph)
  "Survey the image's node classes against GRAPH's disjointness
declarations without signalling.  Returns (values VIOLATIONS
SPEC-COUNT), VIOLATIONS a list of (SPEC . OFFENDING-CLASS-NAME) -- read
the count: zero violations over zero specs is an unchecked schema.  A
declaration that is contradicted cannot normally be registered, so a
non-empty answer means a class was defined past the definition-time
hook (an image that loaded the class before this file, say)."
  (let ((specs (gethash (graph-name graph) *schema-disjointness-metadata*))
        (violations '()))
    (dolist (spec specs)
      (dolist (class (%node-classes))
        (when (%disjointness-offender-p class spec)
          (push (cons spec (class-name class)) violations))))
    (values (nreverse violations) (length specs))))
