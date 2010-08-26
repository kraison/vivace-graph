(in-package #:vivace-graph)

(defconstant +rete-wildcard+ :*)
(defconstant +beta-memory+ :beta)
(defconstant +join-node+ :join)
(defconstant +p-node+ :p-node)

(defstruct (rete-net
	     (:predicate rete-net?)
	     (:conc-name rn-))
  (alpha-memory (make-hash-table :synchronized t :test 'equal))
  (beta-memory nil)
  (beta-memory-index (make-hash-table :synchronized t :test 'equal)))

(defstruct (alpha-memory
	     (:predicate alpha-memory?)
	     (:conc-name alpha-))
  triples children)

(defstruct (token
	     (:predicate token?)
	     (:conc-name token-))
  parent triple)

(defstruct (rete-node
	     (:predicate rete-node?)
	     (:conc-name rete-node-))
  type children parent tokens alpha-memory tests)

(defstruct (join-node-test 
	     (:conc-name nil))
  arg1-field arg2-field levels-up)

(defgeneric add-rule (rule))

(defun join-test (tests token wme)
  )
;  (dolist (test tests)
;    (let ((arg1 (funcall (arg1-field test) wme)))
;      (dolist
	  
(defmethod left-activate ((node rete-node) (token token) &optional wme)
  (case (rete-node-type node)
    (+beta-memory+
     (let ((token (make-token :parent token :triple wme)))
       (push token (rete-node-tokens node))
       (dolist (child (rete-node-children node))
	 (left-activate child token))))
    (+join-node+
     (dolist (wme (alpha-triples (rete-node-alpha-memory node)))
       (when (join-test (rete-node-tests node) token wme)
	 (dolist (child (rete-node-children node))
	   (left-activate child token wme)))))
    (+p-node+ nil)))

(defmethod right-activate ((node rete-node) (wme triple))
  (case (rete-node-type node)
    (+beta-memory+ nil)
    (+join-node+
     (dolist (token (rete-node-tokens (rete-node-parent node)))
       (when (join-test (rete-node-tests node) token wme)
	 (dolist (child (rete-node-children node))
	   (left-activate child token wme)))))
    (+p-node+ nil)))

(defmethod activate-alpha-memory ((am alpha-memory) (triple triple))
  (push triple (alpha-triples am))
  (dolist (child (alpha-children am))
    (right-activate child triple)))

(defmethod add-wme ((triple triple))
  (flet ((add-wme1 (wme)
	   (let ((am (gethash wme (rete-net *graph*))))
	     (when (alpha-memory? am)
	       (activate-alpha-memory am triple)))))
    (let ((wme (as-list triple)))
      (add-wme1 wme)
      (add-wme1 (list (predicate wme) (subject wme) +rete-wildcard+))
      (add-wme1 (list (predicate wme) +rete-wildcard+ (object wme)))
      (add-wme1 (list (predicate wme) +rete-wildcard+ +rete-wildcard+))
      (add-wme1 (list +rete-wildcard+ (subject wme) (object wme)))
      (add-wme1 (list +rete-wildcard+ (subject wme) +rete-wildcard+))
      (add-wme1 (list +rete-wildcard+ +rete-wildcard+ (object wme)))
      (add-wme1 (list +rete-wildcard+ +rete-wildcard+ +rete-wildcard+)))
    ))

;(defmethod add-rule ((rule rule))
;  (dolist (premise (rule-premises rule))
;    ))
