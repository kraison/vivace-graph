;;;; The source onboarding contract: what a source declares about itself.
;;;;
;;;; Enforcement is STRUCTURAL -- DEF-SOURCE defines the class and will not
;;;; expand without all seven facets, so a non-conforming source cannot be
;;;; defined at all (GH #132, design §2).  A class defined with plain
;;;; DEF-VERTEX is simply not a source, which is correct rather than a gap.

(in-package #:graph-db.spacetime)

(defparameter +source-facets+
  '(:identity :space :time :attribution :sensitivity :registration
    :indexed-text)
  "The seven facets.  Every one is required and every one accepts :NONE --
the uniform rule is what makes the contract learnable (design §1).")

(defstruct (source-facets (:conc-name source-facets-) (:copier nil))
  "A class's declared facets, read back through SOURCE-CONTRACT."
  (class nil :read-only t)
  (graph nil :read-only t)
  (identity nil :read-only t)
  (space nil :read-only t)
  (time nil :read-only t)
  (attribution nil :read-only t)
  (sensitivity nil :read-only t)
  (registration nil :read-only t)
  (indexed-text nil :read-only t))

(defvar *source-contracts* (make-hash-table :test 'eq)
  "Class name -> SOURCE-FACETS.")

(defun source-contract (class)
  "CLASS's declared facets.  Signals NOT-A-SOURCE when CLASS was not defined
with DEF-SOURCE -- \"declared nothing\" and \"is not a source\" are different
facts (design §5)."
  (or (gethash class *source-contracts*)
      (error 'not-a-source :class class)))

(defvar *namespace-sources* (make-hash-table :test 'eq)
  "Namespace keyword -> list of class names declaring it.  Populated by the
:IDENTITY facet, and read by RESOLVE-ENDPOINT (design §4).")

(defun namespace-sources (namespace)
  "Class names registered under NAMESPACE.  Signals UNKNOWN-NAMESPACE when
none are -- a typo or an unloaded system, distinct from a key that matches
nothing (design §4)."
  (or (gethash namespace *namespace-sources*)
      (error 'unknown-namespace :namespace namespace)))

(defun %unregister-old-identity (class)
  "Remove CLASS from whatever namespace it was PREVIOUSLY registered under
in *SOURCE-CONTRACTS*, if any.  Called before a (re-)registration, so
re-evaluating DEF-SOURCE with a changed :IDENTITY -- ordinary practice --
leaves *NAMESPACE-SOURCES* in step with *SOURCE-CONTRACTS* instead of
accumulating a stale entry forever (Finding 2, GH #132 review)."
  (let ((old (gethash class *source-contracts*)))
    (when old
      (let ((old-identity (source-facets-identity old)))
        (unless (eq old-identity :none)
          (let ((old-ns (getf old-identity :namespace)))
            (setf (gethash old-ns *namespace-sources*)
                  (remove class (gethash old-ns *namespace-sources*)))))))))

(defun %register-identity (class identity)
  "Register CLASS under its namespace.  :NONE registers nothing: such a
class is never an endpoint target (plan clarification).  Always drops
CLASS's prior registration first (Finding 2, GH #132 review), so this is
correct whether the new IDENTITY names the same namespace, a different
one, or :NONE."
  (%unregister-old-identity class)
  (unless (eq identity :none)
    (let ((ns (getf identity :namespace)))
      (pushnew class (gethash ns *namespace-sources*)))))

(defparameter +facet-absent+ (list :absent)
  "Sentinel for GETF's default argument, to distinguish a sub-key that is
genuinely absent from one explicitly bound to NIL -- the same NIL-vs-
omitted distinction DEF-SOURCE itself draws for facets, one level down for
a facet's own sub-keys.")

(defun %facet-value (plist key)
  "KEY's value in PLIST, or +FACET-ABSENT+ if KEY is not present.  GETF
looks only at even (key) positions, so unlike MEMBER over the whole list
it cannot mistake a key appearing as someone else's VALUE for the key
itself being present (Finding 1, GH #132 review)."
  (getf plist key +facet-absent+))

(defun %proper-plist-p (x)
  "True when X is a proper list of even length.  GETF assumes this and
signals a raw SIMPLE-TYPE-ERROR when it does not hold; checking first lets
a malformed facet -- a dropped value, a dotted list -- signal
INVALID-SOURCE-FACET like every other malformed declaration (Fix 3, GH
#132 review)."
  (loop (cond ((null x) (return t))
              ((not (consp x)) (return nil))
              ((not (consp (cdr x))) (return nil))
              (t (setf x (cddr x))))))

(defun %check-facet (facet value)
  "Return VALUE if it is a well-formed FACET, else signal.  :NONE is always
well-formed (design §1)."
  (labels ((bad (reason)
             (error 'invalid-source-facet :facet facet :value value
                                          :reason reason))
           (req (key)
             "VALUE's KEY sub-value, or signal that it is missing."
             (let ((v (%facet-value value key)))
               (when (eq v +facet-absent+)
                 (bad (format nil "missing ~S" key)))
               v))
           (req-symbol (key)
             "A required sub-key naming a slot or function: a non-NIL
symbol that is not itself a keyword.  A keyword like :PID never matches
slot name PID, so RESOLVE-ENDPOINT would validate cleanly and then
resolve nothing, forever (Fix 1, GH #132 review)."
             (let ((v (req key)))
               (cond
                 ((keywordp v)
                  (bad (format nil "~S must be a slot or function name, ~
not the keyword ~S" key v)))
                 ((not (and (symbolp v) v))
                  (bad (format nil "~S must be a symbol, not ~S" key v))))
               v))
           (req-keyword (key)
             "A required sub-key whose value must be a keyword."
             (let ((v (req key)))
               (unless (keywordp v)
                 (bad (format nil "~S must be a keyword, not ~S" key v)))
               v))
           (req-string (key)
             "A required sub-key whose value must be a string."
             (let ((v (req key)))
               (unless (stringp v)
                 (bad (format nil "~S must be a string, not ~S" key v)))
               v)))
    (unless (eq value :none)
      (unless (%proper-plist-p value)
        (bad "expected a well-formed plist or :NONE"))
      (ecase facet
        (:identity
         (req-keyword :namespace)
         (req-symbol :key-slot))
        (:space
         (req-symbol :geometry-slot)
         (req :kind)
         (req :precision))
        (:time
         (req-symbol :extent-fn))
        (:attribution
         (req-string :licence)
         (req-string :citation))
        (:sensitivity
         (req-keyword :class))
        ;; Uninterpreted here; #138 defines its shape (design §3.3).
        (:registration value)
        (:indexed-text
         (req-symbol :text-fn))))
    value))

(defmacro def-source (name graph-name slots
                      &key (parent-types nil)
                           (identity nil identity-p)
                           (space nil space-p)
                           (time nil time-p)
                           (attribution nil attribution-p)
                           (sensitivity nil sensitivity-p)
                           (registration nil registration-p)
                           (indexed-text nil indexed-text-p))
  "Define NAME as a source vertex in GRAPH-NAME with SLOTS, declaring all
seven facets.  Omitting any signals MISSING-SOURCE-FACET at macroexpansion;
use :NONE to say a facet does not apply (design §2).  PARENT-TYPES is
DEF-VERTEX's own parent-types list (default (), Finding 5, GH #132
review) -- a source class inherits exactly as any other vertex does."
  (let ((missing (append (unless identity-p '(:identity))
                         (unless space-p '(:space))
                         (unless time-p '(:time))
                         (unless attribution-p '(:attribution))
                         (unless sensitivity-p '(:sensitivity))
                         (unless registration-p '(:registration))
                         (unless indexed-text-p '(:indexed-text)))))
    (when missing
      (error 'missing-source-facet :name name :facets missing)))
  (%check-facet :identity identity)
  (%check-facet :space space)
  (%check-facet :time time)
  (%check-facet :attribution attribution)
  (%check-facet :sensitivity sensitivity)
  (%check-facet :registration registration)
  (%check-facet :indexed-text indexed-text)
  `(progn
     (graph-db:def-vertex ,name ,parent-types ,slots ,graph-name)
     ,@(unless (eq identity :none)
         `((graph-db:def-index ,name (,(getf identity :key-slot))
             ,graph-name)
           ;; A single class owns its key slot, so the engine's own
           ;; constraint makes a duplicate unrepresentable at write time,
           ;; going forward -- Finding 3 layer 1, GH #132 review.
           ;; RESOLVE-ENDPOINT's read-side guard (resolve.lisp) is layer
           ;; 2, for what this constraint cannot retroactively cover.
           ;; Re-defining with a changed :KEY-SLOT leaves the OLD slot's
           ;; DEF-UNIQUE spec live (registry keyed on owner+slots); DEF-
           ;; INDEX has the identical leak.  Parked -- fixing needs an
           ;; unregister API in unique-constraint.lisp, off limits as
           ;; graph-db/core (GH #132).
           (graph-db:def-unique ,name (,(getf identity :key-slot))
             ,graph-name)))
     (%register-identity ',name ',identity)
     (setf (gethash ',name *source-contracts*)
           (make-source-facets :class ',name :graph ',graph-name
                               :identity ',identity :space ',space
                               :time ',time :attribution ',attribution
                               :sensitivity ',sensitivity
                               :registration ',registration
                               :indexed-text ',indexed-text))
     ',name))
