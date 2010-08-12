(in-package #:vivace-graph)

(defconstant +wildcard+ '*)
(defparameter *conclusion-operators* '(assert trigger))

(defun print-rule (rule stream depth)
  (declare (ignore depth))
  (format stream "(rule ~A~%  if~%~{    ~a~^~%~}~%  then ~A~%~{    ~a~^~%~})" 
	  (rule-name rule) (rule-premises rule) (rule-cf rule) (rule-conclusions rule)))

(defstruct (rule (:print-function print-rule)
		 (:predicate rule?))
  name premises conclusions cf (lock (make-recursive-lock)) fn)

(defstruct (rule-execution (:predicate rule-execution?)
			   (:conc-name re-))
  rule substitution-list triple timestamp)

(defgeneric compile-rule (rule))
(defgeneric index-rule (rule))
(defgeneric deindex-rule (rule))
(defgeneric match-rules (triple))

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

(defun make-premise-idx (p)
  (mapcar #'(lambda (i) (if (variable-p i) +wildcard+ i)) p))

(defun map-premises (fn p)
  (cond ((atom p) nil)
	((and (consp p) (every #'atom p))
	 ;;(format t "Applying ~A to rule premise: ~A~%" fn p)
	 (funcall fn p))
	((and (consp p) (every #'consp p))
	 (dolist (p1 p) (map-premises fn p1)))
	((and (atom (first p)) (every #'consp (rest p)))
	 (dolist (p1 (rest p)) (map-premises fn p1)))))

(defun count-premises (p)
  (let ((count 0))
    (map-premises #'(lambda (p1) (declare (ignore p1)) (incf count)) p)
    count))

(defmethod index-rule ((rule rule))
  (map-premises #'(lambda (p) 
		    (pushnew rule (gethash (make-premise-idx p) (rule-idx *graph*))))
		(copy-tree (rule-premises rule))))

(defmethod deindex-rule ((rule rule))
  (map-premises #'(lambda (p) 
		    (setf (gethash (make-premise-idx p) (rule-idx *graph*))
			  (remove rule (gethash (make-premise-idx p) (rule-idx *graph*)))))
		(copy-tree (rule-premises rule))))

(defmethod compile-rule ((rule rule))
  rule)

(defmethod do-rule-substitution ((rule rule) (wme triple))
  (let ((result nil) (count 0))
    (map-premises #'(lambda (p)
		      (when (or (prolog-equal (nth 0 p) (predicate wme))
				(prolog-equal (nth 1 p) (subject wme))
				(prolog-equal (nth 2 p) (object wme)))
			(let ((r nil))
			  (if (variable-p (nth 0 p))
			      (push `(= ,(nth 0 p) ,(predicate wme)) r))
			  (if (variable-p (nth 1 p)) 
			      (push `(= ,(nth 1 p) ,(subject wme)) r))
			  (if (variable-p (nth 2 p)) 
			      (push `(= ,(nth 2 p) ,(object wme)) r))
			  (if r (pushnew r result :test 'equal)))))
		  (copy-tree (rule-premises rule)))
    (mapcar #'(lambda (r)
		(incf count)
		(make-rule-execution 
		 :rule rule 
		 :substitution-list r 
		 :triple wme 
		 :timestamp (triple-timestamp wme)))
	    result)))

(defmethod match-rules ((wme triple))
  (let ((r nil))
    (setq r (nconc r (gethash (list (predicate wme) (subject wme) (object wme)) (rule-idx *graph*)))
	  r (nconc r (gethash (list (predicate wme) (subject wme) +wildcard+) (rule-idx *graph*)))
	  r (nconc r (gethash (list (predicate wme) +wildcard+ (object wme)) (rule-idx *graph*)))
	  r (nconc r (gethash (list (predicate wme) +wildcard+ +wildcard+) (rule-idx *graph*)))
	  r (nconc r (gethash (list +wildcard+ (subject wme) (object wme)) (rule-idx *graph*)))
	  r (nconc r (gethash (list +wildcard+ (subject wme) +wildcard+) (rule-idx *graph*)))
	  r (nconc r (gethash (list +wildcard+ +wildcard+ (object wme)) (rule-idx *graph*)))
	  r (nconc r (gethash (list +wildcard+ +wildcard+ +wildcard+) (rule-idx *graph*))))
    (mapcar #'(lambda (rule)
		(do-rule-substitution rule wme))
	    (sort (remove-duplicates r) #'> 
		  :key #'(lambda (r) (count-premises (rule-premises r)))))))

(defmethod run-rules ((graph graph))
  (let ((*graph* graph))
    (let ((triggered-rules (make-hash-table)))
      (loop 
	 for triple = (second (delete-min (production-pq *graph*)))
	 while (triple? triple) do
	   (format t "Matching triple ~A~%" triple)
	   (dolist (l (match-rules triple))
	     (dolist (e l)
	       (format t "Got execution plan ~A~%" e)
	       (if (not (member (re-triple e) 
				(gethash (rule-name (re-rule e)) triggered-rules)
				:test 'triple-eql))
		   (progn
		     ;; FIXME: execute and add if execution is successful.
		     ;; FIXME: if bindings for triple are different, allow it to exec again?
		     (push (re-triple e) (gethash (rule-name (re-rule e)) triggered-rules))
		     (format t "Got rule execution ~A~%" (rule-name (re-rule e)))))))))))

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
	  (deindex-rule rule)
	  (delete-object (rule-db *graph*) (make-serialized-key rule)))
	(warn "Rule ~A is undefined, cannot retract it." name))))

(defmacro defrule (name &body body)
  (assert (eq (first body) 'if))
  (let* ((name (or (and (symbolp name) (intern (string-upcase (symbol-name name))))
		   (and (stringp name) (intern (string-upcase name)))
		   (and (numberp name) name)
		   (error "Rule name must be a string, symbol or integer, not ~A" (type-of name))))
	 (then-part (member 'then body))
	 (premises (ldiff (rest body) then-part))
	 (conclusions (rest then-part)))
    (if (rule? (get-rule name)) (error "A rule named ~A already exists." name))
    (check-conditions name premises 'premise)
    (check-conditions name conclusions 'conclusion)
    (let ((rule (make-rule :name name :cf +cf-true+ :premises premises :conclusions conclusions)))
      (with-transaction ((rule-db *graph*))
	(save-rule rule))
      (compile-rule rule))))

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
    (let ((rule (make-rule :name name :cf +cf-true+ :premises premises :conclusions conclusions)))
      (with-transaction ((rule-db *graph*))
	(save-rule rule))
      (compile-rule rule))))

(defmethod load-all-rules ((graph graph))
  (map-hash-objects (rule-db graph)
		    #'(lambda (key val)
			(declare (ignore key))
			(let ((rule (deserialize val)))
			  (when (rule? rule)
			    (cache-rule rule))))))

#|
(defrule t1
  if 
  (or (is-a ?x "dog") (is-a ?x "human"))
  (or (likes ?x "cats") (likes ?x "lizards"))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defrule t2
  if 
  (or
   (and (is-a ?x "dog") (likes ?x "cats"))
   (and (is-a ?x "dog") (likes ?x "lizards"))
   (and (is-a ?x "human") (likes ?x "lizards")))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defrule t3
  if 
  (or
   (and (is-a ?x "dog") (likes ?x "cats")
	(is-a ?y "dog") (likes ?y "cats"))
   (and (is-a ?x "human") (likes ?x "lizards")
	(is-a ?y "human") (likes ?y "lizards")))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defun test-rules ()
  (match-rules (lookup-triple "Kevin" 'likes "cats"))
  (run-rules *graph*)
  (format t "DONE. SKIP LIST SHOULD BE EMPTY, LENGTH IS ~A:~%" 
	  (skip-list-length (production-pq *graph*)))
  (map-skip-list #'(lambda (k v) (format t "~A: ~A~%" k (type-of v))) (production-pq *graph*)))
|#