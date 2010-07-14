;;; N.B. this won't file-compile correctly. The SB-LOCATIVE stuff
;;; must be compiled and loaded before the rest.
;;;
;;; Multi-Word Compare-and-Swap (MCAS), an implementation of the
;;; algorithms described in "A Practical Multi-Word Compare-and-Swap
;;; Operation", by Harris, Fraser and Pratt (2002).
;;; 
;;; The authors use the RDCSS (restricted double compare-and-set)
;;; primitive to implement a n-way CAS.  A useful MCAS should allow one to
;;; CAS on a large set of places.  Thus we would like to be able to store
;;; references to places in data structures to use them later.  A simple
;;; way to achieve this is to use closures (one for each operation).  A
;;; slightly more complicated one would avoid most of the indirect-jump
;;; overhead by generating code on a larger scale to inline all the
;;; closures.  That's however fairly complex and generates a lot of
;;; nearly-duplicated code.  First-class places would lead to a much
;;; better solution than either of these alternatives.
;;; 
;;; There's no such thing in CL.  However, the few places that are CASable
;;; in SBCL are simple enough to easily make them first-class.  The cases
;;; I find interesting are implemented in SB-LOCATIVE.  A locative is a
;;; pair of values: a base value, which is the Lisp object that contains
;;; the cell we're interested in, and an offset (a signed machine integer)
;;; from the Lisp object's address (tag included) to the cell.  Since all
;;; these places accept arbitrary boxed objects (primitive type T), the
;;; error checking is much simplified.  Caveat utilisator: any semantic
;;; type check (e.g. a typed boxed slot in a structure) is completely
;;; weakened away.  That cannot confuse the runtime (directly), but could
;;; result in strange failures in conjunction with user code.
;;; 
;;; Finally, what's actually implemented is a variant of the paper's
;;; protocol.  RDCSS was specialised for the one use-case, as part of the
;;; MCAS algorithm.  The use of RDCSS in MCAS was also inlined and the nested
;;; loops flattened to simplify the code.  A similar loop flattening
;;; transformation was applied to the CASN read function.  A more dangerous
;;; modification is that RDCSS descriptors are allocated ahead of time.
;;; While I believe all the changes are safe, I haven't really proved it
;;; to my satisfaction yet.
;;; 
;;; I'm fairly certain the same technique can be used for more complex
;;; operations, e.g. ones that decide which locations to ``lock'' or the
;;; values with which to overwrite the locations dynamically.  Until I
;;; find a nice abstraction to safely describe such operations, MCAS is
;;; all there is.
;;; 
;;; INTERFACE
;;; 
;;; MCAS can only be used on a subset of the places supported by
;;; COMPARE-AND-SWAP.  For now, that subset includes CAR/FIRST, CDR/REST,
;;; SVREF, GLOBAL-SYMBOL-VALUE, and structure accessors for boxed (T)
;;; slots.
;;; 
;;; MCAS plans should never be shared between threads.  Ensuring exclusive
;;; access with a lock would be correct but contrary to the very spirit of
;;; the operation.
;;; 
;;; Macro READ
;;;  READ /place/
;;; 
;;;  MCAS:READ must be used to access the current value of any /place/
;;;  that may also be used in an MCAS operation.
;;;
;;; Macro SCAS (Single Compare-and-Swap)
;;;  SCAS /place/ /old-value/ /new-value/ => /previous-value/
;;;
;;;  MCAS:SCAS may be used to execute a regular compare-and-swap on a
;;;  /place/ that is also used by Multiple-CAS.  Atomically replaces
;;;  the value in /place/ with /new-value/ if the previous value in
;;;  /place/ is EQ to /old-value/.  Returns the previous value (the swap
;;;  was carried out successfully if /previous-value/ is EQ /old-value/).
;;;
;;; High-level API
;;; 
;;; Macro WITH-MCAS
;;;  WITH-MCAS (&key /initial-size/ /plan-name/) &body /body/ => /result*/
;;; 
;;;  Allocates a MCAS descriptor (a plan).  In /body/, the macro CAS and
;;;  the function RUN may be used to operate on the plan associated with
;;;  the closest enclosing WITH-MCAS (current plan). If /plan-name/ was
;;;  provided, it may be used to get the plan.
;;; 
;;; Macro CAS
;;;  CAS /place/ /old-value/ &optional /new-value/ => /plan/
;;; 
;;;  Appends a CAS sub-operation (set /place/ to /new-value/ if /place/ is
;;;  EQ /old-value/, do nothing otherwise) to the current plan.  If /new-value/
;;;  isn't given, it defaults to /old-value/, for a regular lock.
;;; 
;;; Function RUN
;;;  RUN => /success/
;;; 
;;;  Executes the current plan.  It is not safe to use CAS once RUN has
;;;  been called. RUN must be called at most once.  Returns T if the MCAS
;;;  operation was successful, NIL on failure.
;;;
;;; Atomic operation DSL
;;;
;;; Macro DO
;;;  DO (/variable/*) (/value/*) &body /statement/* => /success/ /value/*
;;;
;;;  Defines and executes a non-blocking process.  Each /variable/ is
;;;  initially unbound, and is bound by the operations defined in the
;;;  body.  The primary return value is T if the operation was success-
;;;  fully carried out and NIL otherwise.  The other values are the
;;;  values returned by each element of the second sublist; each such
;;;  value is evaluated in a lexical scope such that the value of vari-
;;;  ables bound in the body is available.
;;;
;;;  In the body of DO, only a restricted set of operations is allowed:
;;;
;;;  MCAS:EVAL /variable/ /form/
;;;
;;;   Evaluates /form/ in a lexical environment such that the variables
;;;   that have already been set evaluate to the set value, and others
;;;   to a form that errors out at compile-time.  Sets /variable/ to the
;;;   value of /form/ if it's unbound.
;;;
;;; MCAS:GET /variable/ /place/
;;;
;;;   Sets /variable/ to the value in /place/.  /place/ is evaluated in
;;;   a lexical environment similar to EVAL
;;;
;;; MCAS:CAS /place/ /old-value/ &optional /new-value/
;;;
;;;   Acquires a lock on /place/ if it's EQ to /old-value/.  If /new-value/
;;;   is given, it will be evaluated at the end of the sequence to find
;;;   the value to overwrite /place/ with.
;;;
;;; MCAS:GUARD /form/
;;;
;;;   Evaluates /form/, and aborts the machine if /form/ returns NIL.
;;;
;;; MCAS:CHECKPOINT
;;;
;;;   Used to insert checkpoint before expensive computations.  Does not
;;;   affect correctness, only avoid recomputing values when the status
;;;   of the machine has already been decided.
;;;
;;; Function MCAS:FAIL
;;;
;;;   Available in the lexical scope of the body. Calling that function
;;;   aborts the process (if it hasn't already succeeded).
;;;
;;; Low-level API
;;; Function MAKE-MCAS
;;;  MAKE-MCAS &optional /initial-size/ => /plan/
;;; 
;;;  Returns a MCAS descriptor (plan) that is used to accumulate a set of
;;;  CAS suboperations (place, old value, new value), before executing the
;;;  MCAS operation.  /plan/ is of type MCAS.
;;; 
;;; Macro PUSH-CAS
;;;  PUSH-CAS /plan/ /place/ /old-value/ /new-value/ => /plan/
;;; 
;;;  PUSH-CAS appends a CAS sub-operation on /place/ with /old-value/ and
;;;  /new-value/ to the MCAS descriptor /plan/.
;;; 
;;; Function MCAS
;;;  MCAS /plan/ => /success/
;;;
;;;  Executes the MCAS operation described by /plan/.  Returns T if the
;;;  MCAS operation was successful, NIL on failure.

(defpackage "MULTIPLE-COMPARE-AND-SWAP"
    (:use "CL" "SB-EXT" "SB-LOCATIVE")
  (:nicknames "MCAS")
  (:shadow "READ" "DO" "EVAL" "GET")
  (:export "MCAS-DESCRIPTOR" "MCAS-P"
           "READ" "SCAS"
           "MAKE-MCAS" "PUSH-CAS" "MCAS"
           "WITH-MCAS" "CAS" "RUN"
           "DO" "GET" "EVAL" "GUARD" "CHECKPOINT" "FAIL"))

(in-package "MULTIPLE-COMPARE-AND-SWAP")

(defconstant +undecided-status+ 2)
(defconstant +failed-status+    1)
(defconstant +succeeded-status+ 0)

(defstruct (process-descriptor
             (:conc-name #:descriptor.)
             (:constructor nil))
  (status +undecided-status+))

(declaim (maybe-inline %make-mcas %make-rdcss %make-concurrent-machine))
(defstruct (mcas-descriptor
             (:include process-descriptor)
             (:predicate mcas-p)
             (:conc-name #:mcas.)
             (:constructor %make-mcas (max-len bases offsets old-vals new-vals)))
  ;; all immutable once we're in multi-thread land
  (n        0   :type (integer 0 #.array-dimension-limit))
  (max-len  0   :type (and unsigned-byte fixnum))
  (bases    nil :type simple-vector)
  (offsets  nil :type (simple-array offset 1))
  (old-vals nil :type simple-vector)
  (new-vals nil :type simple-vector)
  (rdcsses  #() :type simple-vector))
(declaim (freeze-type mcas-descriptor))

(declaim (maybe-inline %make-concurrent-machine))
(defstruct (concurrent-machine
             (:include process-descriptor)
             (:predicate machine-p)
             (:constructor %make-concurrent-machine (complete))
             (:conc-name #:machine.))
  (complete nil
   :type (function (concurrent-machine) (values boolean &optional))
   :read-only t))
(declaim (freeze-type concurrent-machine))

(defstruct (rdcss-descriptor
             (:predicate rdcss-p)
             (:conc-name #:rdcss.)
             (:constructor %make-rdcss (parent base offset old-val)))
  (parent nil  :type process-descriptor :read-only t)
  base
  (offset   0  :type offset)
  old-val)
(declaim (freeze-type mcas-descriptor))

(declaim (maybe-inline make-mcas))
(defun make-mcas (&optional (size 8))
  (declare (inline %make-mcas))
  (%make-mcas (max size 1)
              (make-array size) (make-array size :element-type 'offset)
              (make-array size) (make-array size)))

(defun %grow-mcas (mcas)
  (declare (type mcas-descriptor mcas))
  (with-accessors ((max-len mcas.max-len)
                   (bases   mcas.bases)
                   (offsets mcas.offsets)
                   (olds    mcas.old-vals)
                   (news    mcas.new-vals))
      mcas
    (let ((new-len (* 2 max-len)))
      (setf max-len new-len
            bases   (replace (make-array new-len) bases)
            offsets (replace (make-array new-len :element-type 'offset) offsets)
            olds    (replace (make-array new-len) olds)
            news    (replace (make-array new-len) news))))
  mcas)

(declaim (inline %%push-cas))
(defun %%push-cas (mcas base offset old new)
  (declare (type mcas-descriptor mcas)
           (type offset offset))
  (let ((i   (mcas.n       mcas))
        (max (mcas.max-len mcas)))
    (when (= i max)
      (%grow-mcas mcas))
    (setf (mcas.n mcas)                 (1+ i)
          (aref (mcas.bases    mcas) i) base
          (aref (mcas.offsets  mcas) i) offset
          (aref (mcas.old-vals mcas) i) old
          (aref (mcas.new-vals mcas) i) new))
  mcas)

(declaim (maybe-inline %push-cas))
(defun %push-cas (mcas base offset old new)
  (%%push-cas mcas base offset old new))

(defmacro push-cas (mcas place old new)
  `(multiple-value-call #'%push-cas
     (values ,mcas) (make-locative ,place)
     (values ,old) (values ,new)))

(declaim (ftype (function (mcas-descriptor) (values mcas-descriptor &optional))
                setup-mcas)
         (ftype (function (mcas-descriptor) (values boolean &optional)) execute-mcas))

(declaim (inline mcas))
(defun mcas (mcas)
  (execute-mcas (setup-mcas mcas)))

;; Would probably make more sense to accumulate
;; rdcsses, and build a couple auxiliary arrays
;; later on.
(defun setup-mcas (descriptor)
  (declare (type mcas-descriptor descriptor))
  (when (zerop (mcas.max-len descriptor))
    (error "Attempting to use a ~S twice" 'mcas-descriptor))
  (setf (mcas.max-len descriptor) 0)
  (let* ((n       (mcas.n descriptor))
         (rdcsses (make-array n))
         (bases   (mcas.bases descriptor))
         (offsets (mcas.offsets descriptor))
         (olds    (mcas.old-vals descriptor)))
    (setf (mcas.rdcsses descriptor) rdcsses)
    (dotimes (i n descriptor)
      (setf (aref rdcsses i)
            (%make-rdcss descriptor
                         (aref bases i) (aref offsets i)
                         (aref olds i))))))

;; Compare and Compare-and-Swap
;; Saves some locked writes on contended locations
(declaim (inline ccas-locative))
(defun ccas-locative (base offset old new)
  (declare (type offset offset))
  (let ((contents (contents base offset)))
    (if (eq contents old)
        (compare-and-swap-locative base offset old new)
        contents)))

(declaim (maybe-inline complete-rdcss))
(defun complete-rdcss (descriptor)
  (declare (type rdcss-descriptor descriptor))
  (let ((parent  (rdcss.parent descriptor))
        (old-val (rdcss.old-val descriptor)))
    (ccas-locative (rdcss.base descriptor) (rdcss.offset descriptor)
                   descriptor
                   (if (eq +undecided-status+ (descriptor.status parent))
                       parent old-val))))

(defun execute-mcas (descriptor)
  (declare (type mcas-descriptor descriptor))
  (labels
      ((self (descriptor)
         (declare (type mcas-descriptor descriptor)
                  (optimize speed (sb-c::insert-array-bounds-checks 0))
                  (inline complete-rdcss))
         (let ((n       (mcas.n        descriptor))
               (bases   (mcas.bases    descriptor))
               (offsets (mcas.offsets  descriptor))
               (olds    (mcas.old-vals descriptor))
               (news    (mcas.new-vals descriptor))
               (rdcsses (mcas.rdcsses  descriptor))
               (status  +succeeded-status+))
           (when (eq +undecided-status+ (mcas.status descriptor))
             (dotimes (i n)
               (let ((base   (aref bases   i))
                     (offset (aref offsets i))
                     (old    (aref olds    i))
                     (rdcss  (aref rdcsses i)))
                 (loop named loop
                       do
                   (unless (eq (mcas.status descriptor) +undecided-status+)
                      (return))
                    (let ((r (ccas-locative base offset old rdcss)))
                      (cond ((eq r descriptor)) ; already locked that one
                            ((or (eq r old)
                                 (eq r rdcss)) ; success! complete the rdcss
                             (ccas-locative
                              base offset
                              rdcss
                              (if (eq +undecided-status+ (mcas.status descriptor))
                                  descriptor
                                  old))
                             (return-from loop))
                            ((rdcss-p r) ; help our quicker comrades
                             (complete-rdcss r))
                            ((mcas-p r)
                             (self r))
                            ((machine-p r)
                             (funcall (machine.complete r) r))
                            (t          ; unexpected value, fail
                             (setf status +failed-status+)
                             (return))))))))
           (when (eq (mcas.status descriptor) +undecided-status+)
             (compare-and-swap (mcas.status descriptor) +undecided-status+ status))
           (let* ((status (mcas.status descriptor))
                  (vals   (if (eq +succeeded-status+ status) news olds)))
             (dotimes (i n (eq +succeeded-status+ status))
               (ccas-locative (aref bases i) (aref offsets i)
                              descriptor (aref vals i)))))))
    (self descriptor)))

(declaim (maybe-inline mcas-read)
         (ftype (function (t offset) (values t &optional)) mcas-read))
(defun mcas-read (base offset)
  (declare (type offset offset))
  (loop
    (let ((r (contents base offset)))
      (cond ((rdcss-p r)
             (complete-rdcss r))
            ((mcas-p r)
             (execute-mcas r))
            ((machine-p r)
             (funcall (machine.complete r) r))
            (t
             (return r))))))

(defmacro read (place)
  `(multiple-value-call #'mcas-read (make-locative ,place)))

(defmacro cas (place old &optional new)
  (declare (ignore place old new))
  (error "~S must be called within the lexical scope of ~S or ~S" 'cas 'with-mcas 'do))

(defmacro run ()
  (error "~S must be called within the lexical scope of ~S" 'run 'with-mcas))

(defmacro with-mcas ((&key (size 8) plan-name (block-name nil block-p)) &body body)
  (let ((mcas-var (gensym "MCAS")))
    (append
     (when block-p
       `(block ,block-name))
     `(let* ((,mcas-var (make-mcas ,size))
             ,@(when plan-name
                 (list `(,plan-name ,mcas-var))))
        (macrolet ((cas (place old &optional (new nil newp))
                     (if newp
                         `(push-cas ,',mcas-var ,place ,old ,new)
                         (let ((_val (gensym "VAL")))
                           `(multiple-value-call #'%push-cas
                              ,',mcas-var (make-locative ,place)
                              (let ((,_val ,old))
                                (values ,_val ,_val)))))))
          (flet ((run ()
                   (mcas ,mcas-var))
                 (%push-cas (mcas base offset old new)
                   (declare (type offset offset))
                   (%%push-cas mcas base offset old new)))
            ,@body))))))

(declaim (maybe-inline %single-cas))
(defun %single-cas (base offset old new)
  (declare (type offset offset))
  (loop
    (let ((r (ccas-locative base offset old new)))
      (cond ((rdcss-p r)
             (complete-rdcss r))
            ((mcas-p r)
             (execute-mcas r))
            ((machine-p r)
             (funcall (machine.complete r) r))
            (t
             (return r))))))

(defmacro scas (place old new)
  `(multiple-value-call #'%single-cas
     (make-locative ,place) (values ,old) (values ,new)))

(defmacro eval (variable value)
  (declare (ignore variable value))
  (error "~S must be used in the lexical scope of ~S" 'eval 'do))
(defmacro get (variable place)
  (declare (ignore variable place))
  (error "~S must be used in the lexical scope of ~S" 'get 'do))
(defmacro guard (form)
  (declare (ignore form))
  (error "~S must be used in the lexical scope of ~S" 'guard 'do))
(defmacro fail ()
  (error "~S must be used in the lexical scope of ~S" 'fail 'do))
(defmacro checkpoint ()
  (error "~S must be used in the lexical scope of ~S" 'checkpoint 'do))

(defmacro do ((&rest variables) (&rest values) &body steps)
  (let* ((nvars 0)
         (vars  (mapcar (lambda (var)
                          (prog1 (cons var nvars)
                            (incf nvars)))
                        variables)))
    (multiple-value-bind (nrdcss body final-env)
        (compile-machine steps vars)
      `(macrolet ((empty (var)
                    (error "Using unset variable ~S in ~S" var 'do)))
         (let* ((.rdcsses. (make-array ,nrdcss))
                (.vars.    (make-array ,nvars))
                (complete  (lambda (.self.)
                             (flet ((var (index)
                                      (aref .vars. index)))
                               (declare (inline var)
                                        (ignorable #'var))
                               (macrolet ((read (place)
                                            (declare (ignore place))
                                            (error "~S used inside ~S" 'read 'do)))
                                 ,@body))))
                (.self.    (%make-concurrent-machine complete)))
           (fill .vars. .self.)
           (map-into .rdcsses. (lambda ()
                                 (%make-rdcss .self. 0 0 .self.)))
           ,(append final-env
                    `((flet ((var (index)
                               (aref .vars. index)))
                        (declare (inline var)
                                 (ignorable #'var))
                        (values (funcall complete .self.)
                                ,@values)))))))))

(defun %machine-read (base offset self var-base var-offset)
  (declare (type offset offset var-offset)
           (type concurrent-machine self))
  (symbol-macrolet ((var    (contents var-base var-offset))
                    (status (machine.status self)))
    (when (and (eq var self)
               (eq status +undecided-status+))
      (loop do (let ((r (contents base offset)))
                 (cond ((eq r self)
                        (return (not (eq var self))))
                       ((rdcss-p r)
                        (complete-rdcss r))
                       ((mcas-p r)
                        (execute-mcas r))
                       ((machine-p r)
                        (funcall (machine.complete r) r))
                       (t
                        (ccas-locative var-base var-offset self r)
                        (return t))))
            while (and (eq var self)
                       (eq status +undecided-status+))))))

(defun %machine-cas (rdcss self)
  (declare (type rdcss-descriptor rdcss)
           (type concurrent-machine self))
  (symbol-macrolet ((status (machine.status self)))
    (let ((base   (rdcss.base    rdcss))
          (offset (rdcss.offset  rdcss))
          (old    (rdcss.old-val rdcss)))
      (loop
         (when (eq status +undecided-status+)
           (let ((r (ccas-locative base offset old rdcss)))
             (cond ((eq r self))  ; already locked that one
                   ((or (eq r old)
                        (eq r rdcss))   ; success! complete the rdcss
                    (ccas-locative
                     base offset
                     rdcss (if (eq +undecided-status+ status) self old))
                    (return t))
                   ((rdcss-p r)         ; help our quicker comrades
                    (complete-rdcss r))
                   ((mcas-p r)
                    (execute-mcas r))
                   ((machine-p r)
                    (funcall (machine.complete r) r))
                   (t                   ; unexpected value, fail
                    (return nil)))))))))

(defvar *cas-count*)
(defvar *set-vars*)
(defvar *vars*)

(defun var (name)
  (or (cdr (assoc name *vars*))
      (error "Unknown variable ~S" name)))

(defun env (form)
  `(symbol-macrolet ,(loop for (var . index) in *vars*
                           collect `(,var ,(if (member var *set-vars*)
                                               `(var ,index)
                                               `(empty ,var))))
     ,form))

(defun note-set-var (var)
  (when (member var *set-vars*)
    (error "~S set multiple times in ~S" var 'do))
  (push var *set-vars*)
  (values))

(defun get-rdcss ()
  (prog1 *cas-count*
    (incf *cas-count*)))

(defun compile-op (op args)
  (ecase op
    (eval
       (destructuring-bind (var value) args
         (let ((index (var var))
               (form  (env value)))
           (note-set-var var)
           `(when (eq (svref .vars. ,index) .self.)
              (let ((val ,form))
                (when (eq (svref .vars. ,index) .self.)
                  (compare-and-swap (svref .vars. ,index) .self. val)))))))
    (get
       (destructuring-bind (var place) args
         (note-set-var var)
         (let ((index (var var)))
           `(when (eq (svref .vars. ,index) .self.)
              (unless (multiple-value-call #'%machine-read
                        ,(env `(make-locative ,place))
                        .self.
                        (make-locative (svref .vars. ,index)))
                (fail))))))
    (cas
       (destructuring-bind (place old &optional (new nil newp)) args
         (let ((rdcss (get-rdcss)))
           (values
             `(let ((.rdcss. (aref .rdcsses. ,rdcss)))
                (when (eq (rdcss.old-val .rdcss.) .self.)
                  (setf (values (rdcss.base   .rdcss.)
                                (rdcss.offset .rdcss.))
                        ,(env `(make-locative ,place))
                        (rdcss.old-val .rdcss.)
                        ,(env old)))
                (when (eq (machine.status .self.) +undecided-status+)
                  (unless (%machine-cas .rdcss. .self.)
                    (fail))))
             `(let* ((.rdcss.  (aref .rdcsses. ,rdcss))
                     (.old.    (rdcss.old-val  .rdcss.))
                     (.base.   (rdcss.base     .rdcss.))
                     (.offset. (rdcss.offset   .rdcss.)))
                (unless (or (eq .old. .self.)
                            (not (eq (contents .base. .offset.) .self.)))
                  (ccas-locative
                   .base. .offset.
                   .self. ,(if newp
                               `(if (eq .status. +succeeded-status+)
                                    ,new
                                    .old.)
                               '.old.))))))))
    (guard
       (destructuring-bind (form) args
         `(unless ,(env form)
            (fail))))
    (checkpoint
       (assert (null args))
       `(unless (eq (machine.status .self.) +undecided-status+)
          (go .end.)))))

(defun compile-machine (operations vars)
  (let ((*cas-count* 0)
        (*set-vars*   '())
        (*vars*       vars)
        (commit       '())
        (body         '()))
    (dolist (op operations)
      (destructuring-bind (op . args) op
        (multiple-value-bind (op clean-up) (compile-op op args)
          (push op body)
          (when clean-up
            (push clean-up commit)))))
    (setf body   (nreverse body)
          commit (nreverse commit))
    (values *cas-count*
            `((when (eq (machine.status .self.) +undecided-status+)
                (let ((.status. +succeeded-status+))
                  (tagbody
                     (flet ((fail ()
                              (setf .status. +failed-status+)
                              (go .end.)))
                       (declare (inline fail))
                       ,@body)
                   .end.
                     (when (eq (machine.status .self.) +undecided-status+)
                       (compare-and-swap (machine.status .self.)
                                         +undecided-status+ .status.))
                   .done.)))
              (let ((.status. (machine.status .self.)))
                ,(env `(progn
                         ,@commit))
                (eq .status. +succeeded-status+)))
            (env nil))))
