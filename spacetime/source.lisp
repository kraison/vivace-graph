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

(defun %register-identity (class identity)
  "Register CLASS under its namespace.  :NONE registers nothing: such a
class is never an endpoint target (plan clarification)."
  (unless (eq identity :none)
    (let ((ns (getf identity :namespace)))
      (pushnew class (gethash ns *namespace-sources*)))))

(defun %plist-has-p (plist &rest keys)
  (every (lambda (k) (member k plist)) keys))

(defun %check-facet (facet value)
  "Return VALUE if it is a well-formed FACET, else signal.  :NONE is always
well-formed (design §1)."
  (flet ((bad (reason)
           (error 'invalid-source-facet :facet facet :value value
                                        :reason reason)))
    (unless (eq value :none)
      (unless (listp value) (bad "expected a plist or :NONE"))
      (ecase facet
        (:identity
         (unless (%plist-has-p value :namespace :key-slot)
           (bad "expected (:NAMESPACE <keyword> :KEY-SLOT <slot>)")))
        (:space
         (unless (%plist-has-p value :geometry-slot :kind :precision)
           (bad "expected (:GEOMETRY-SLOT <slot> :KIND k :PRECISION p)")))
        (:time
         (unless (%plist-has-p value :extent-fn)
           (bad "expected (:EXTENT-FN <function-name>)")))
        (:attribution
         (unless (%plist-has-p value :licence :citation)
           (bad "expected (:LICENCE <string> :CITATION <string>)")))
        (:sensitivity
         (unless (%plist-has-p value :class)
           (bad "expected (:CLASS <keyword>)")))
        ;; Uninterpreted here; #138 defines its shape (design §3.3).
        (:registration value)
        (:indexed-text
         (unless (%plist-has-p value :text-fn)
           (bad "expected (:TEXT-FN <function-name>)")))))
    value))

(defmacro def-source (name graph-name slots
                      &key (identity nil identity-p)
                           (space nil space-p)
                           (time nil time-p)
                           (attribution nil attribution-p)
                           (sensitivity nil sensitivity-p)
                           (registration nil registration-p)
                           (indexed-text nil indexed-text-p))
  "Define NAME as a source vertex in GRAPH-NAME with SLOTS, declaring all
seven facets.  Omitting any signals MISSING-SOURCE-FACET at macroexpansion;
use :NONE to say a facet does not apply (design §2)."
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
     (graph-db:def-vertex ,name () ,slots ,graph-name)
     ,@(unless (eq identity :none)
         `((graph-db:def-index ,name (,(getf identity :key-slot))
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
