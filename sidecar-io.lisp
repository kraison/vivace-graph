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

(defmacro with-sidecar-output ((&key (package :common-lisp) (readably nil))
                                &body body)
  "Bind the full printer-control set a sidecar writer needs so ambient
bindings (*PRINT-LENGTH* 2 from a caller's debugging session, etc.)
can never produce a truncated or non-standard line.  *READ-DEFAULT-
FLOAT-FORMAT* is bound too: the printer consults it to decide whether a
float needs an explicit exponent marker, so a writer and reader that
disagree on it can silently swap a float's type on round-trip even
though none of these sidecars currently store one (a runtime slot
spec's :INITFORM could).  PACKAGE defaults to :COMMON-LISP; pass
:PACKAGE :KEYWORD for a registry whose payload symbols must always
print qualified even when accessible in COMMON-LISP.  READABLY
defaults to NIL; pass :READABLY T for a writer that wants *PRINT-
READABLY*'s stricter guarantee (store-registry.lisp's #191 strictness
predates #226 and is preserved this way) (GH #226)."
  `(let ((*print-readably* ,readably)
         (*print-pretty* nil)
         (*print-length* nil)
         (*print-level* nil)
         (*print-base* 10)
         (*print-radix* nil)
         (*print-circle* nil)
         (*print-case* :upcase)
         (*print-gensym* t)
         (*read-default-float-format* 'double-float)
         (*package* (find-package ,package)))
     ,@body))

(defmacro with-sidecar-input ((&key (package :common-lisp)) &body body)
  "Bind the full reader-control set a sidecar reader needs: *READ-EVAL*
NIL (the file is data, never code), *READ-SUPPRESS* NIL (an ambient T
here would read every form as NIL with no error and no warning -- total
silent data loss, worse than any parse failure this file guards
against), a fixed *READ-BASE*, the matching *READ-DEFAULT-FLOAT-
FORMAT* (see WITH-SIDECAR-OUTPUT), and a pristine copy of the standard
readtable so an ambient #+ecl READTABLE-CASE or a reader macro some
other part of the image installed cannot change how a line parses
(GH #226)."
  `(let ((*read-eval* nil)
         (*read-suppress* nil)
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
     (format s "~A: skipped ~D unreadable sidecar record~:P (first ~
failed READ started at byte ~D) -- a torn tail, corrupt bytes, or a ~
form naming a package this image does not have.  Content before the ~
first bad record was kept (GH #227)."
             (sidecar-skipped-file c) (sidecar-skipped-count c)
             (sidecar-skipped-first-position c)))))

(defun read-sidecar-forms (stream &key (package :common-lisp) (warn-p t))
  "Read successive top-level forms from STREAM under the sidecar reader
discipline (WITH-SIDECAR-INPUT), tolerating a bad form ANYWHERE in the
stream, not only at the tail: on a failed READ, the stream is
repositioned back to where that READ started (so an unterminated string
or unbalanced opener cannot swallow the rest of the file into one
erroring form) and reading resumes one line further on.  This is also
what makes a legitimate multi-line string inside a well-formed form
safe -- READ spans the embedded newline instead of a line-oriented
reader tripping over it (GH #226).

Returns (VALUES FORMS SKIPPED FIRST-SKIP-POSITION).  SKIPPED counts
failed READs, one per dropped line; FIRST-SKIP-POSITION is the byte
position the first failed READ started from, or NIL when SKIPPED is 0.
WARN-P (default T) controls whether SIDECAR-RECORDS-SKIPPED-worthy
information is even worth computing precisely for the caller -- see
READ-SIDECAR-FILE-FORMS, the only place that actually signals it."
  (declare (ignore warn-p))
  (with-sidecar-input (:package package)
    (let ((forms nil) (skipped 0) (first-skip nil))
      (loop
        (let ((pos (file-position stream)))
          (let ((form (handler-case (read stream nil :eof)
                        (error ()
                          (unless first-skip (setf first-skip pos))
                          (incf skipped)
                          ;; Reposition to where the failed READ began --
                          ;; without this, an unterminated string or an
                          ;; unbalanced opener has already consumed
                          ;; everything up to EOF, and every good record
                          ;; after the bad one is lost too (GH #226,
                          ;; review round 2).
                          (file-position stream pos)
                          (read-line stream nil nil)
                          :sidecar-skip))))
            (cond ((eq form :eof) (return))
                  ((eq form :sidecar-skip))
                  (t (push form forms))))))
      (values (nreverse forms) skipped first-skip))))

(defun read-sidecar-file-forms (file &key (package :common-lisp) (warn-p t))
  "READ-SIDECAR-FORMS over FILE, or (values nil 0 nil) when FILE does
not exist.  Signals SIDECAR-RECORDS-SKIPPED once when any record was
dropped, unless WARN-P is NIL -- the edge-occupancy sidecar's contract
is \"a lost hint, never a signal\" (type-occupancy.lisp, GH #167), so
its loader passes WARN-P NIL (GH #227)."
  (if (probe-file file)
      (with-open-file (s file :direction :input)
        (multiple-value-bind (forms skipped first-skip)
            (read-sidecar-forms s :package package)
          (when (and warn-p (plusp skipped))
            (warn 'sidecar-records-skipped
                  :file file :count skipped :first-position first-skip))
          (values forms skipped first-skip)))
      (values nil 0 nil)))
