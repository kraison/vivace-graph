;; tokenizer.lisp
;; Author: Michael Weber <michaelw@foldr.org>, 2009

;; Modified by Kevin Raison <raison@chatsubo.net>, 2010, for use in vivace-graph

;; Erik Naggum, "Revisiting split-sequence and patterns"
;; <http://groups.google.com/group/comp.lang.lisp/msg/14c1561d3a33bea1>

;;; TODO:
;; * unbalanced (multi-)escapes, etc.
;; * docstring
;; * more tests
;; * optimize
;; * streams
;; * callback instead of returning list
;; * multi-character delimiters (e.g., CR/LF)? (Knuth-Morris-Pratt)
;; * signal END-OF-SEQUENCE, restart to continue with next sequence
(in-package #:vivace-graph)


(defun tokenize (sequence &key delimiters terminators punctuation
                 (escapes "\\") multi-escapes whitespace
                 (defaults '()) start end key test)
  (let ((start (or start 0))
        (end (or end (length sequence)))
        (key (or key #'identity))
        (test (or test #'eql))
        (whitespace (case whitespace
                      ((:whitespace t)
                       #(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page))
                      (t
                       whitespace)))
        (sequence-type (class-of sequence))
        (tokens '())
        (chunk '())
        (empty-chunk-p t)
        (empty-field-p t)
        (item)
        (i 0))
    (labels
        ((matchp (item* class)
           (let* ((item (funcall key item*))
                  (class
                   (cond ((null class)
                          (return-from matchp nil))
                         ((or (functionp class)
                              (symbolp class))
                          (return-from matchp (funcall class item)))
                         ((characterp class)
                          (string class))
                         (t
                          class))))
             (find item class :test test)))
         (next-default-token ()
           (etypecase defaults
             (list (pop defaults))
             (function (funcall defaults))))
         (next-item ()
           (prog1 (etypecase sequence
                    (list
                     (if (and (not (endp sequence))
                              (< i end))
                         (pop sequence)
                         (done)))
                    (t (if (< i end)
                           (elt sequence i)
                           (done))))
             (incf i)))
         (unread-item ()
           (when (listp sequence)
             (push item sequence))
           (decf i))
         (skip-whitespace ()
           (loop do (setf item (next-item))
                 while (matchp item whitespace)))
         (emit-token (&optional (visiblep t))
           (unless (and (null chunk)
                        empty-chunk-p)
             (push (coerce (nreverse (shiftf chunk '()))
                           sequence-type)
                   tokens)
             (setf empty-chunk-p t)
             (when visiblep
               (setf empty-field-p nil))))
         (emit-field ()
           (emit-token)
           (let ((default (next-default-token)))
             (if empty-field-p
                 (push default tokens)
                 (setf empty-field-p t))))
         (done ()
           (emit-field)
           (return-from tokenize
             (values (nreverse tokens) i)))
         (escape (delimiter)
           ;; similar to:
           ;; (tokenize original-sequence :start i
           ;;           :delimiters delimiter :escapes escapes)
           (loop
            (cond ((funcall test item delimiter)
                   (setf item (next-item))
                   (return))
                  ((matchp item escapes)
                   (push (next-item) chunk)
                   (skip-whitespace))
                  (t (push item chunk)
                     (setf item (next-item))))))
         (token ()
           (loop
            (cond ((matchp item delimiters)
                   (emit-field)
                   (skip-whitespace))
                  ((matchp item terminators)
                   (unread-item)
                   (done))
                  ((matchp item punctuation)
                   (emit-token)
                   (push item chunk)
                   (emit-token nil)
                   (skip-whitespace))
                  ((matchp item escapes)
                   (push (next-item) chunk)
                   (skip-whitespace))
                  ((matchp item multi-escapes)
                   (setf empty-chunk-p nil)
                   (escape (shiftf item (next-item))))
                  ((matchp item whitespace)
                   (emit-token)
                   (skip-whitespace))
                  (t (push item chunk)
                     (setf item (next-item)))))))
      (etypecase sequence
        (list (setf sequence (nthcdr start sequence)))
        (t (setf i start)))
      (skip-whitespace)
      (token))))


(defun test-tokenize (seq)
  (tokenize seq
            :escapes #\\
            :multi-escapes "\"|"
            :delimiters (format nil ",:~A" #\Tab)
            :terminators "}"
            :punctuation "[]()!."
            :whitespace :whitespace
            :defaults (let ((i 0))
                        (lambda () (incf i)))))

#+#.(cl:if (cl:find-package :utest) '(cl:and) '(cl:or))
(progn
  (utest:deftest tokenizer.1
    (utest:check
      (utest:is (test-tokenize "xxx yyy")
                '("xxx" "yyy") :test #'equal)
      (utest:is (test-tokenize "xxx   yyy} z")
                '("xxx" "yyy") :test #'equal)))

  (utest:deftest tokenizer.2
    (utest:check
      (utest:is (test-tokenize "xxx,yyy")
                '("xxx" "yyy") :test #'equal)
      (utest:is (test-tokenize "xxx,,,yyy")
                '("xxx" 2 3 "yyy") :test #'equal)
      (utest:is (test-tokenize "xxx,||,yyy")
                '("xxx" "" "yyy") :test #'equal)
      (utest:is (test-tokenize "xxx,  yyy")
                '("xxx" "yyy") :test #'equal)
      (utest:is (test-tokenize "xxx  ,yyy")
                '("xxx" "yyy") :test #'equal)
      (utest:is (test-tokenize "xxx|,|yyy")
                '("xxx,yyy") :test #'equal)
      (utest:is (test-tokenize "xxx|,| yyy")
                '("xxx," "yyy") :test #'equal)
      (utest:is (test-tokenize ",")
                '(1 2) :test #'equal)))

  (utest:deftest overlap.1
    (utest:check
      (utest:is '("one" nil nil "two")
                (tokenize (format nil "one~%~%~%two")
                          :delimiters #\newline)
                :test #'equal)
      (utest:is '("one" "two")
                (tokenize (format nil "one~%~%~%two")
                          :delimiters #\newline
                          :whitespace :whitespace)
                :test #'equal)))

  (utest:deftest lists.1
    (utest:check
      (utest:is '((x x) (y y) (z z))
                (tokenize '(x x * y y * z z) :delimiters '(*))
                :test #'equal)))

  (utest:deftest punctuation.1
    (utest:check
      (utest:is (test-tokenize "[]")
                '("[" "]" 1) :test #'equal)
      (utest:is (test-tokenize "[,]")
                '("[" 1 "]" 2) :test #'equal)
      (utest:is (test-tokenize "x[,]y")
                '("x" "[" "]" "y") :test #'equal)))

  (utest:deftest all-tests
    (utest:check
      (tokenizer.1)
      (tokenizer.2)
      (lists.1)
      (overlap.1)
      (punctuation.1))))
