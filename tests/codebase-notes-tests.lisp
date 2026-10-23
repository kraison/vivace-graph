;;;; docs/codebase-notes.md stays true: every name it gives resolves and
;;;; every path it gives exists.  A name is a code span such as
;;;; `graph-db:map-edges` or `graph-db/test::with-test-graph`.
(in-package #:graph-db/test)

(def-suite codebase-notes-suite :in graph-db-suite
  :description "The names and paths docs/codebase-notes.md gives.")
(in-suite codebase-notes-suite)

(defun %cn-note ()
  (uiop:read-file-string
   (asdf:system-relative-pathname :graph-db "docs/codebase-notes.md")))

(defun %cn-spans (text)
  "TEXT's inline `code` spans, in order, outside its ``` fenced blocks."
  (let ((spans '()) (fenced nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (if (uiop:string-prefix-p "```" (string-left-trim " " line))
          (setf fenced (not fenced))
          (unless fenced
            (loop with start = 0
                  for a = (position #\` line :start start)
                  for b = (and a (position #\` line :start (1+ a)))
                  while b
                  do (push (subseq line (1+ a) b) spans)
                     (setf start (1+ b))))))
    (nreverse spans)))

(defun %cn-name-p (span)
  "SPAN is pkg:name or pkg::name: no blank, not a URL, a package part."
  (let ((c (position #\: span)))
    (and c (plusp c)
         (< (1+ c) (length span))
         (not (search "://" span))
         (notany (lambda (ch) (find ch '(#\Space #\Tab))) span)
         (every (lambda (ch) (or (alphanumericp ch) (find ch "./-")))
                (subseq span 0 c))
         (string/= (string-left-trim ":" (subseq span c)) ""))))

(defun %cn-resolves-p (span)
  "SPAN names a function, macro, variable or class; NIL for an unknown
package or symbol, never an error."
  (let* ((c (position #\: span))
         (pkg (find-package (string-upcase (subseq span 0 c))))
         (sym (and pkg (find-symbol (string-upcase
                                     (string-left-trim ":" (subseq span c)))
                                    pkg))))
    (and sym (or (fboundp sym) (boundp sym) (find-class sym nil)) t)))

(defun %cn-path-p (span)
  "SPAN is a repo path: a slash, no colon, a known ending."
  (and (find #\/ span)
       (not (find #\: span))
       (some (lambda (end) (uiop:string-suffix-p span end))
             '(".lisp" ".md" ".asd" ".sh" "/"))))

(test every-name-resolves
  "Each name the note gives is still a function, macro, variable or class."
  (let ((unresolved (remove-if #'%cn-resolves-p
                               (remove-if-not #'%cn-name-p
                                              (%cn-spans (%cn-note))))))
    (is (null unresolved) "unresolved: ~{~A~^, ~}" unresolved)))

(test every-path-exists
  "Each repo path the note gives still exists."
  (let ((missing (remove-if
                  (lambda (p)
                    (probe-file (asdf:system-relative-pathname :graph-db p)))
                  (remove-if-not #'%cn-path-p (%cn-spans (%cn-note))))))
    (is (null missing) "missing: ~{~A~^, ~}" missing)))

(test note-names-enough
  "At least ten names: a broken span reader would pass the others empty."
  (is (<= 10 (count-if #'%cn-name-p (%cn-spans (%cn-note))))))

(test unknown-package-is-unresolved
  "An unknown package is reported, not a reader error."
  (is-false (%cn-resolves-p "no-such-pkg:foo"))
  (is-false (%cn-resolves-p "graph-db::no-such-function-anywhere")))
