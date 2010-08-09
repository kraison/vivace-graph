(in-package #:vivace-graph)

(defparameter *conclusion-operators* '(assert trigger))

(defun print-rule (rule stream depth)
  (declare (ignore depth))
  (format stream "(rule ~A~%  if~%~{    ~a~^~%~}~%  then ~A~%~{    ~a~^~%~})" 
	  (rule-name rule) (rule-premises rule) (rule-cf rule) (rule-conclusions rule)))

(defstruct (rule (:print-function print-rule)
		 (:predicate rule?))
  name premises conclusions cf (lock (make-recursive-lock)))

(defun check-conditions (rule-name conditions kind)
  "Warn if any conditions are invalid."
  (when (null conditions)
    (error "Rule ~A: Missing ~A" rule-name kind))
  (dolist (condition conditions)
    (when (not (consp condition))
      (error "Rule ~A: Illegal ~A: ~A" rule-name kind condition))
    (when (not (symbolp (first condition)))
      ;; FIXME: this needs to walk the tree and check all cars
      (error "Rule ~A: Illegal functor ~A in ~A ~A" rule-name (first condition) kind condition))
    (let ((op (first condition)))
      (when (and (eq kind 'conclusion) (not (member op *conclusion-operators*)))
	(error "Rule ~A: Illegal operator (~A) in conclusion: ~A" rule-name op condition)))))

(defmethod deserialize-help ((become (eql +rule+)) bytes)
  "Decode a rule."
  (declare (optimize (speed 3)))
  (destructuring-bind (name premises conclusions cf) (extract-all-subseqs bytes)
    (let ((rule (make-rule :name (deserialize name)
			   :premises (deserialize premises)
			   :conclusions (deserialize conclusions)
			   :cf (deserialize cf))))
      (cache-rule rule))))

(defmethod serialize ((rule rule))
  "Encode a rule for storage."
  (serialize-multiple +rule+ 
		      (rule-name rule)
		      (rule-premises rule)
		      (rule-conclusions rule)
		      (rule-cf rule)))

(defun make-rule-key-from-name (name)
  (serialize-multiple +rule-key+ (princ-to-string name)))

(defmethod make-serialized-key ((rule rule))
  (make-rule-key-from-name (rule-name rule)))

(defmethod index-rule ((rule rule))
  "Pass the rule's premises into the Rete network."
  (let ((vars (variables-in (rule-premises rule))))
    (format t "Vars in ~A: ~A~%" (rule-name rule) vars)))

(defmethod save-rule ((rule rule))
  (store-object (rule-db *graph*) (make-serialized-key rule) (serialize rule))
  (index-rule rule)
  (cache-rule rule))

(defmethod cache-rule ((rule rule))
  (setf (gethash (rule-name rule) (rule-cache *graph*)) rule))

(defun get-rule (name)
  (or (gethash (cond ((or (symbolp name) (numberp name)) name)
		     ((stringp name)
		      (if (cl-ppcre:scan "^[0-9]+\.*[0-9]*$" name)
			  (parse-number:parse-number name)
			  (intern (string-upcase name))))
		     (t (error "Unknown type for rule name ~A: ~A" name (type-of name))))
	       (rule-cache *graph*))
      (let ((raw-rule (lookup-object (rule-db *graph*) (make-rule-key-from-name name))))
	(if (vectorp raw-rule)
	    (cache-rule (deserialize raw-rule))
	    nil))))

(defun retract-rule (name)
  (let ((rule (get-rule name)))
    (if (rule? rule)
	(sb-ext:with-locked-hash-table ((rule-cache *graph*))
	  ;; FIXME: delete all facts derived by this rule!
	  (remhash (rule-name rule) (rule-cache *graph*))
	  (delete-object (rule-db *graph*) (make-serialized-key rule)))
	(error "Rule ~A is undefined, cannot retract it." name))))

(defmacro defrule (name &body body)
  (assert (eq (first body) 'if))
  (let* ((name (or (and (symbolp name) (intern (string-upcase (symbol-name name))))
		   (and (stringp name) (intern (string-upcase name)))
		   (and (numberp name) name)
		   (error "Rule name must be a string, symbol or integer, not ~A" (type-of name))))
	 (then-part (member 'then body))
	 (premises (ldiff (rest body) then-part))
	 (conclusions (rest2 then-part)))
    (if (rule? (get-rule name)) (error "A rule named ~A already exists." name))
    (check-conditions name premises 'premise)
    (check-conditions name conclusions 'conclusion)
    (with-transaction ((rule-db *graph*))
      (save-rule
       (make-rule :name name :cf +cf-true+ :premises premises :conclusions conclusions)))))

(defmacro def-fuzzy-rule (name &body body)
  (assert (eq (first body) 'if))
  (let* ((name (or (and (symbolp name) (intern (string-upcase (symbol-name name))))
		   (and (stringp name) (intern (string-upcase name)))
		   (and (numberp name) name)
		   (error "Rule name must be a string, symbol or integer, not ~A" (type-of name))))
	 (then-part (member 'then body))
	 (premises (ldiff (rest body) then-part))
	 (conclusions (rest2 then-part))
	 (cf (second then-part)))
    (if (rule? (get-rule name)) (error "A rule named ~A already exists." name))
    (check-conditions name premises 'premise)
    (check-conditions name conclusions 'conclusion)
    (when (not (certainty-factor-p cf))
      (error "Rule ~A: Illegal certainty factor: ~A" name cf))
    (with-transaction ((rule-db *graph*))
      (save-rule
       (make-rule :name name :cf cf :premises premises :conclusions conclusions)))))

