;;;; Restoring a snapshot file as a set of recovery transactions

(in-package :graph-db)

(defvar *restore-objects-per-transaction* 10)

;;; ---------------------------------------------------------------------------
;;; Snapshot vector syntax:  #V(<element-type> e1 e2 ...)
;;;
;;; Snapshot files are plain s-expressions, so a vector's ELEMENT TYPE has to be
;;; written down explicitly -- CL's standard #(...) syntax always reads back a
;;; SIMPLE-VECTOR (element type T).  BACKUP therefore emits #V for every vector
;;; whose element type is not T (ids, byte vectors, float vectors, ...), and
;;; RESTORE-SHARP-V-READER rebuilds the array with that element type.
;;;
;;; #(...) itself is left alone: it reads as a plain simple-vector, losslessly.
;;; (Before the fix for issue #56, #(...) was overridden to coerce EVERYTHING to
;;; (unsigned-byte 8) so that ids came back as byte vectors -- which made any
;;; non-byte vector slot, e.g. a SINGLE-FLOAT embedding, a hard restore error.)
;;;
;;; Deliberately NOT using *PRINT-READABLY*: SBCL prints specialized arrays as
;;; #A((3) SINGLE-FLOAT ...), which is an SBCL extension, not portable syntax.
;;; graph-db snapshots must restore on SBCL, CCL, ECL and LispWorks alike.
;;; ---------------------------------------------------------------------------

(defun restore-sharp-v-reader (stream subcharacter arg)
  "Read #V(<element-type> e1 e2 ...) as a fresh vector of ELEMENT-TYPE.

ELEMENT-TYPE is an ordinary readable type specifier, e.g. SINGLE-FLOAT,
DOUBLE-FLOAT or (UNSIGNED-BYTE 8).  See BACKUP-VECTOR-LITERAL for the writer."
  (declare (ignore subcharacter arg))
  (let ((form (read stream t nil t)))
    (cond (*read-suppress* nil)
          ((not (consp form))
           (error "Malformed #V literal in snapshot: ~S" form))
          (t
           (let ((element-type (first form))
                 (elements (rest form)))
             (make-array (length elements)
                         :element-type element-type
                         :initial-contents
                         (mapcar (lambda (x)
                                   (if (typep x element-type)
                                       x
                                       (coerce x element-type)))
                                 elements)))))))

(defparameter *restore-readtable*
  (let ((*readtable* (copy-readtable)))
    (local-time:enable-read-macros)
    (set-dispatch-macro-character #\# #\V 'restore-sharp-v-reader)
    (set-dispatch-macro-character #\# #\v 'restore-sharp-v-reader)
    *readtable*))

(deftype id-array () '(simple-array (unsigned-byte 8) (*)))

(defun ensure-id-array (value)
  "Coerce VALUE back to an id byte vector if it plainly is one, else return it.

Needed for OLD-FORMAT snapshots (written before the #V syntax existed): there,
ids -- and an edge's FROM / TO -- were printed as bare #(...) and now read back
as simple-vectors of integers.  New snapshots carry the element type in the #V
literal, so this is a no-op on them.  Anything that is not a vector of
\(UNSIGNED-BYTE 8) values is passed through untouched."
  (cond ((typep value 'id-array) value)
        ((and (vectorp value)
              (plusp (length value))
              (every (lambda (x) (typep x '(unsigned-byte 8))) value))
         (coerce value 'id-array))
        (t value)))

(defun fix-restore-node-args (args positional-id-indices)
  "Return a copy of ARGS -- the argument list applied to MAKE-VERTEX / MAKE-EDGE
-- with every id-bearing value run through ENSURE-ID-ARRAY.

POSITIONAL-ID-INDICES lists the 0-based positions of id arguments (none for a
vertex; FROM and TO for an edge).  The :ID keyword argument is fixed in both
cases.  This is the one place that knows what each position MEANS, which is why
the id repair lives here and not in the reader."
  (let ((args (copy-list args)))
    (dolist (i positional-id-indices)
      (setf (nth i args) (ensure-id-array (nth i args))))
    (let ((cell (member :id args)))
      (when (cdr cell)
        (setf (second cell) (ensure-id-array (second cell)))))
    args))

(defun read-n-sexps (stream n)
  "Read N s-expressions from STREAM. Returns a list of s-expressions
as the primary value, and non-nil as the secondary value at EOF."
  (let ((sexps '()))
    (dotimes (i n (nreverse sexps))
      (let ((sexp (read stream nil stream)))
        (when (eq sexp stream)
          (return (values (nreverse sexps) :eof)))
        (push sexp sexps)))))

(defun call-for-snapshot-sexps (fun file sexp-count)
  "Call FUN with a list of SEXP-COUNT s-expressions from FILE, until
  EOF in the file. The final list may have fewer than SEXP-COUNT
  expressions."
  (with-open-file (stream file)
    (loop
       (multiple-value-bind (sexps eofp)
           (read-n-sexps stream sexp-count)
         (when sexps
           (funcall fun sexps))
         (when eofp
           (return))))))

(defmacro do-snapshot-sexps ((var file &optional (count 10)) &body body)
  `(call-for-snapshot-sexps (lambda (,var) ,@body)
                            ,file
                            ,count))

(defun recreate-graph (graph snapshot-file &key package-name)
  "Replay SNAPSHOT-FILE into GRAPH, recreating every node it holds through the
normal MAKE-VERTEX / MAKE-EDGE transaction path (so indexes, views, unique
constraints and vector segments are all repopulated).  Symbols in the file are
read in PACKAGE-NAME.

Both snapshot formats are accepted.  Current snapshots write specialized vectors
as #V(<element-type> ...), so a node's vector slots come back with their exact
element type.  Snapshots written before that syntax existed printed every vector
as bare #(...): their ids, and an edge's FROM / TO, are repaired here by
ENSURE-ID-ARRAY, but the element type of a node's own vector-valued SLOTS was
never recorded in those files and cannot be recovered -- such a slot restores as
a plain SIMPLE-VECTOR.  Re-snapshot after restoring to get the types back."
  (let ((*package* (find-package package-name))
        (*readtable* *restore-readtable*)
        (*graph* graph)
        (count 0)
        (tx-id (load-highest-transaction-id graph))
        (start-time (get-universal-time)))
    (do-snapshot-sexps (plists snapshot-file *restore-objects-per-transaction*)
      (let ((*transaction* (make-instance 'restore-transaction
                                          :transaction-id (incf tx-id))))
        (dolist (plist plists)
          (when (zerop (mod (incf count) 1000))
            (log:info "~A RESTORED ~A NODES" (current-thread) count))
          (ecase (car plist)
            (:v
             ;; (TYPE DATA :ID id :REVISION n :DELETED-P p)
             (apply 'make-vertex (fix-restore-node-args (rest plist) '())))
            (:e
             ;; (TYPE FROM TO WEIGHT DATA :ID id :REVISION n :DELETED-P p)
             (apply 'make-edge (fix-restore-node-args (rest plist) '(1 2))))
            (:last-txn-id)
            (otherwise
             (log:error "RESTORE: Unknown input: ~S" plist))))
        (apply-transaction *transaction* graph)))
    (persist-highest-transaction-id (incf tx-id) graph)
    (let ((elapsed-time (- (get-universal-time) start-time)))
      (log:info "RESTORE TOOK ~A SECONDS" elapsed-time)
      (values graph :count count :elapsed-time elapsed-time))))
