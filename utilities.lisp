(in-package #:vivace-graph)

(defmacro logger (level msg &rest args)
  "Syslogger"
  `(funcall #'sb-posix:syslog (gethash ',level *syslog-priorities*) ,msg ,@args))

(defun ip-to-string (ip)
  (format nil "~A.~A.~A.~A" (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))

(defgeneric less-than (x y)
  (:documentation "Generic less-than operator.  Allows comparison of apples and oranges.")
  (:method ((x symbol)    (y symbol))    (string< (symbol-name x) (symbol-name y)))
  (:method ((x symbol)    (y string))    (string< (symbol-name x) y))
  (:method ((x symbol)    (y number))    (string< (symbol-name x) (write-to-string y)))
  (:method ((x symbol)    (y uuid:uuid)) (string< (symbol-name x) (uuid:print-bytes nil y)))
  (:method ((x number)    (y number))    (< x y))
  (:method ((x number)    (y symbol))    (string< (write-to-string x) (symbol-name y)))
  (:method ((x number)    (y string))    (string< (write-to-string x) y))
  (:method ((x number)    (y uuid:uuid)) (string< (write-to-string x) (uuid:print-bytes nil y)))
  (:method ((x string)    (y string))    (string< x y))
  (:method ((x string)    (y symbol))    (string< x (symbol-name y)))
  (:method ((x string)    (y number))    (string< x (write-to-string y)))
  (:method ((x string)    (y uuid:uuid)) (string< x (uuid:print-bytes nil y)))
  (:method ((x timestamp) (y timestamp)) (timestamp< x y))
  (:method ((x number)    (y timestamp)) (< (timestamp-to-universal x) y))
  (:method ((x timestamp) (y number))    (< x (timestamp-to-universal y)))
  (:method ((x uuid:uuid) (y uuid:uuid))
    (string< (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
  (:method ((x uuid:uuid) (y string)) (string< (uuid:print-bytes nil x) y))
  (:method ((x uuid:uuid) (y symbol)) (string< (uuid:print-bytes nil x) (symbol-name y)))
  (:method ((x uuid:uuid) (y number)) (string< (uuid:print-bytes nil x) (write-to-string y))))

(defgeneric greater-than (x y)
  (:documentation "Generic greater-than operator.  Allows comparison of apples and oranges.")
  (:method ((x symbol) (y symbol))    (string> (symbol-name x) (symbol-name y)))
  (:method ((x symbol) (y string))    (string> (symbol-name x) y))
  (:method ((x symbol) (y number))    (string> (symbol-name x) (write-to-string y)))
  (:method ((x symbol) (y uuid:uuid)) (string> (symbol-name x) (uuid:print-bytes nil y)))
  (:method ((x number) (y number))    (> x y))
  (:method ((x number) (y symbol))    (string> (write-to-string x) (symbol-name y)))
  (:method ((x number) (y string))    (string> (write-to-string x) y))
  (:method ((x number) (y uuid:uuid)) (string> (write-to-string x) (uuid:print-bytes nil y)))
  (:method ((x string) (y string))    (string> x y))
  (:method ((x string) (y symbol))    (string> x (symbol-name y)))
  (:method ((x string) (y number))    (string> x (write-to-string y)))
  (:method ((x string) (y uuid:uuid)) (string> x (uuid:print-bytes nil y)))
  (:method ((x timestamp) (y timestamp)) (timestamp> x y))
  (:method ((x number)    (y timestamp)) (> (timestamp-to-universal x) y))
  (:method ((x timestamp) (y number))    (> x (timestamp-to-universal y)))
  (:method ((x uuid:uuid) (y uuid:uuid)) 
    (string> (uuid:print-bytes nil x) (uuid:print-bytes nil y)))
  (:method ((x uuid:uuid) (y string)) (string> (uuid:print-bytes nil x) y))
  (:method ((x uuid:uuid) (y symbol)) (string> (uuid:print-bytes nil x) (symbol-name y)))
  (:method ((x uuid:uuid) (y number)) (string> (uuid:print-bytes nil x) (write-to-string y))))

(defun make-slot-key (id slot)
  (format nil "~A~A~A" id #\Nul slot))

;; Make compare-and-swap shorter to call
(defmacro cas (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

;; String split without regexes.
(defun split (string &optional (ws '(#\Space #\Tab)) max)
  "Split STRING along whitespace as defined by the sequence WS.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless MAX is provided, in which case the
string will be split into MAX tokens at most, the last one
containing the whole rest of the given STRING, if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
  (when (and max (>= words (1- max)))
    (return (cons (subseq string start) list)))
  (setf end (position-if #'is-ws string :start start))
  (push (subseq string start end) list)
  (incf words)
  (unless end (return list))
  (setf start (1+ end)))))))

(defun print-hash (ht)
  "Dump the k-v pairs of a hash table to stdout."
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht))

;; Plists
(defun get-prop (plist prop)
  "Return the value of a property in a property list."
  (cond ((null plist) nil)
	((eql (car plist) prop)
	 (cadr plist))
	(t (get-prop (cddr plist) prop))))

;; Norvig utilities
(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-p))))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun new-interned-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "return a list of leaves of tree satisfying predicate, with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;; Borrowed from On Lisp by Graham
(defmacro while (test &rest body)
  `(loop until (not ,test) do
	,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

