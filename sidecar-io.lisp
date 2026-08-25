(in-package :graph-db)

;;; Shared print/read discipline for the image's line-oriented sidecar
;;; files (type registry, store registry, edge-occupancy hint, schema
;;; manifest, system journal): small, human-editable logs where each
;;; writer/reader used to bind only *PRINT-READABLY*/*PRINT-PRETTY*/
;;; *PACKAGE* and *READ-EVAL*, leaving *PRINT-LENGTH*, *PRINT-LEVEL*,
;;; *PRINT-BASE*, *PRINT-RADIX*, *PRINT-CIRCLE*, *PRINT-CASE*,
;;; *READ-BASE* and *READTABLE* to whatever the caller had ambient.
;;; Any of those rebound by app code (these writers are reachable from
;;; arbitrary code via CREATE-VERTEX-TYPE etc.) produces a truncated or
;;; misreadable line that the tolerant readers then silently drop
;;; (GH #226).

(defmacro with-sidecar-output ((&key (package :common-lisp)) &body body)
  "Bind the full printer-control set a sidecar writer needs so ambient
bindings (*PRINT-LENGTH* 2 from a caller's debugging session, etc.)
can never produce a truncated or non-standard line.  *READ-DEFAULT-
FLOAT-FORMAT* is bound too: the printer consults it to decide whether a
float needs an explicit exponent marker, so a writer and reader that
disagree on it can silently swap a float's type on round-trip even
though none of these sidecars currently store one (a runtime slot
spec's :INITFORM could).  PACKAGE defaults to :COMMON-LISP; pass
:PACKAGE :KEYWORD for a registry whose payload symbols must always
print qualified even when accessible in COMMON-LISP (GH #226)."
  `(let ((*print-readably* nil)
         (*print-pretty* nil)
         (*print-length* nil)
         (*print-level* nil)
         (*print-base* 10)
         (*print-radix* nil)
         (*print-circle* nil)
         (*print-case* :upcase)
         (*read-default-float-format* 'double-float)
         (*package* (find-package ,package)))
     ,@body))

(defmacro with-sidecar-input ((&key (package :common-lisp)) &body body)
  "Bind the full reader-control set a sidecar reader needs: *READ-EVAL*
NIL (the file is data, never code), a fixed *READ-BASE*, the matching
*READ-DEFAULT-FLOAT-FORMAT* (see WITH-SIDECAR-OUTPUT), and a pristine
copy of the standard readtable so an ambient #+ecl READTABLE-CASE or a
reader macro some other part of the image installed cannot change how
a line parses (GH #226)."
  `(let ((*read-eval* nil)
         (*read-base* 10)
         (*read-default-float-format* 'double-float)
         (*readtable* (copy-readtable nil))
         (*package* (find-package ,package)))
     ,@body))

(define-condition sidecar-records-skipped (warning)
  ((file :initarg :file :reader sidecar-skipped-file)
   (count :initarg :count :reader sidecar-skipped-count)
   (first-position :initarg :first-position
                    :reader sidecar-skipped-first-position))
  (:report
   (lambda (c s)
     (format s "~A: skipped ~D unreadable sidecar record~:P (first at ~
byte ~D) -- a torn tail, corrupt line, or a form naming a package this ~
image does not have.  Content before the first bad record was kept ~
(GH #227)."
             (sidecar-skipped-file c) (sidecar-skipped-count c)
             (sidecar-skipped-first-position c)))))

(defun read-sidecar-forms (stream &key (package :common-lisp))
  "Read successive top-level forms from STREAM under the sidecar reader
discipline (WITH-SIDECAR-INPUT), tolerating a bad form ANYWHERE in the
stream, not only at the tail: reading resumes at the next line boundary
after a failed READ, which is also what makes a legitimate multi-line
string inside a well-formed form safe -- READ spans the embedded
newline instead of a line-oriented reader tripping over it (GH #226).

Returns (VALUES FORMS SKIPPED FIRST-SKIP-POSITION).  SKIPPED is the
count of forms dropped; FIRST-SKIP-POSITION is the byte offset of the
first one, or NIL when SKIPPED is 0.  A stray unbalanced opener can
still swallow the remainder of the file into one erroring form -- the
same failure mode line-oriented parsing has for a torn opening paren --
so this is tolerance for the expected torn-tail/corrupt-row cases, not
a guarantee against adversarial input (GH #227)."
  (with-sidecar-input (:package package)
    (let ((forms nil) (skipped 0) (first-skip nil))
      (loop
        (let ((pos (file-position stream)))
          (let ((form (handler-case (read stream nil :eof)
                        (error ()
                          (unless first-skip (setf first-skip pos))
                          (incf skipped)
                          (read-line stream nil nil)
                          :sidecar-skip))))
            (cond ((eq form :eof) (return))
                  ((eq form :sidecar-skip))
                  (t (push form forms))))))
      (values (nreverse forms) skipped first-skip))))

(defun read-sidecar-file-forms (file &key (package :common-lisp))
  "READ-SIDECAR-FORMS over FILE, or (values nil 0 nil) when FILE does
not exist.  Signals SIDECAR-RECORDS-SKIPPED once when any record was
dropped (GH #227)."
  (if (probe-file file)
      (with-open-file (s file :direction :input)
        (multiple-value-bind (forms skipped first-skip)
            (read-sidecar-forms s :package package)
          (when (plusp skipped)
            (warn 'sidecar-records-skipped
                  :file file :count skipped :first-position first-skip))
          (values forms skipped first-skip)))
      (values nil 0 nil)))
