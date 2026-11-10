;;;; Tabled predicates (GH #122): the declaration, and the per-query
;;;; variant tables a tabled call is answered from.  Design:
;;;; docs/superpowers/specs/2026-11-10-tabling-design.md (D1 to D8).

(in-package #:graph-db)

;;; Keys (design D2).  The markers are uninterned symbols held in
;;; variables, never constants: a DEFCONSTANT of a fresh symbol does
;;; not survive a reload.

(defvar +var-tag+ (make-symbol "VAR"))
(defvar +node-tag+ (make-symbol "NODE"))
(defvar +ts-tag+ (make-symbol "TS"))
(defvar +array-tag+ (make-symbol "ARRAY"))

(defun %variant-key (term)
  "TERM's variant key under EQUAL (design D2's table), and whether it
holds no unbound variable.  Variables number by first occurrence."
  (let ((vars '()) (n -1))
    (labels ((key (x)
               (var-deref x)
               (cond ((var-p x)
                      (let ((seen (assoc x vars :test #'eq)))
                        (cons +var-tag+
                              (if seen
                                  (cdr seen)
                                  (progn (push (cons x (incf n)) vars)
                                         n)))))
                     ((consp x) (cons (key (car x)) (key (cdr x))))
                     ((realp x) (rational x))
                     ((stringp x) x)
                     ((typep x 'node) (cons +node-tag+ (coerce (id x) 'list)))
                     ((typep x 'timestamp)
                      (list +ts-tag+ (timestamp-to-universal x) (nsec-of x)))
                     ((arrayp x) (cons +array-tag+ (map 'list #'key x)))
                     (t x))))
      (let ((k (key term)))
        (values k (null vars))))))

(defun %answer-key (tuple)
  "TUPLE's key for a table's SEEN set: the variant numbering again."
  (%variant-key tuple))

;;; Tables (design D2, D3).

(defstruct (tbl (:constructor %make-tbl (key created-at)))
  "One variant's table.  ANSWERS holds (TUPLE . GROUND-P) in first
derivation order; SEEN its answer keys; INDEX the stack depth while it
evaluates; LEADER the lowest open table it depends on, or NIL."
  key
  (answers (make-array 4 :adjustable t :fill-pointer 0))
  (seen (make-hash-table :test 'equal))
  complete created-at (round 0) (index 0) leader)

(defun %lower-leader (tbl to)
  "TBL depends on TO, an open table: its LEADER becomes the lower of
the two by stack INDEX.  A TO at or above TBL is TBL itself (D3)."
  (when (and tbl to)
    (let ((to (if (<= (tbl-index to) (tbl-index tbl)) to tbl))
          (leader (tbl-leader tbl)))
      (when (or (null leader) (< (tbl-index to) (tbl-index leader)))
        (setf (tbl-leader tbl) to)))))

(defun %store-answer (tbl args)
  "The add-answer continuation's body: ARGS as they stand copied into
TBL unless SEEN has the variant.  Returns NIL so the clause backtracks."
  (let ((copy (deref-copy args)))
    (multiple-value-bind (key ground-p) (%answer-key copy)
      (unless (gethash key (tbl-seen tbl))
        (setf (gethash key (tbl-seen tbl)) t)
        (vector-push-extend (cons copy ground-p) (tbl-answers tbl))
        (incf *answers-added*))))
  nil)

(defun %consume-tbl (tbl args cont)
  "Deliver TBL's answers to CONT through ARGS, one tick each, by index
up to the vector's length at every step (so answers added meanwhile are
delivered too); bindings undone between answers."
  (let ((answers (tbl-answers tbl)) (i 0))
    (loop while (< i (fill-pointer answers))
          do (let ((answer (aref answers i))
                   (mark (fill-pointer *trail*)))
               (%tick)
               (when (unify args (if (cdr answer)
                                     (car answer)
                                     (deref-copy (car answer))))
                 (funcall cont))
               (undo-bindings mark)
               (incf i)))
    nil))

(defun %run-tbl (tbl functor args)
  "One pass of FUNCTOR's dispatcher over ARGS into TBL."
  (setf (tbl-round tbl) *round*
        (tbl-leader tbl) nil)
  ;; The last clause's bindings survive the dispatcher's return (the
  ;; caller's undo normally takes them): undo them here.
  (let ((dispatcher (functor-dispatcher functor))
        (mark (fill-pointer *trail*)))
    (when dispatcher
      (apply dispatcher
             (append args (list (lambda () (%store-answer tbl args))))))
    (undo-bindings mark))
  nil)

(defun %complete-from (tbl)
  "TBL and every incomplete table created inside its extent (a greater
CREATED-AT) are complete (design D3)."
  (let ((from (tbl-created-at tbl)))
    (maphash (lambda (key other)
               (declare (ignore key))
               (when (>= (tbl-created-at other) from)
                 (setf (tbl-complete other) t)))
             *tables*)))

(defun %drop-from (tbl)
  "TBL and every incomplete table created after it leave *TABLES*: an
evaluation left by a non-local exit built them half (design D5)."
  (let ((from (tbl-created-at tbl)))
    (remhash (tbl-key tbl) *tables*)
    (maphash (lambda (key other)
               (when (and (not (tbl-complete other))
                          (> (tbl-created-at other) from))
                 (remhash key *tables*)))
             *tables*)))

(defun %evaluate-tbl (tbl functor args)
  "Evaluate TBL per design D3: passes of the dispatcher with TBL on the
stack; a table that depends on nothing below itself leads, iterating
while a pass adds an answer anywhere, then completes its component; one
that depends on an ancestor stays incomplete and hands its leader to
the table below.  A non-local exit drops the half tables (D5).
Returns whether TBL completed."
  (let ((parent (first *table-stack*))
        (completed nil)
        (returned nil))
    (unwind-protect
         (let ((*table-stack* (cons tbl *table-stack*)))
           (setf (tbl-index tbl) (length *table-stack*))
           (loop
             (let ((before *answers-added*)
                   (leader nil))
               (%run-tbl tbl functor args)
               (setf leader (tbl-leader tbl))
               (cond ((null leader)
                      ;; Nothing read was open: one pass is the fixpoint.
                      (%complete-from tbl)
                      (setf completed t)
                      (return))
                     ((not (eq leader tbl))
                      (return))
                     ((= before *answers-added*)
                      (%complete-from tbl)
                      (setf completed t)
                      (return))
                     (t (incf *round*)))))
           (setf returned t))
      (unless returned
        (%drop-from tbl)))
    (unless completed
      (%lower-leader parent (tbl-leader tbl)))
    completed))

(defun %open-dependency (tbl)
  "The end of TBL's LEADER chain: the lowest table TBL's answers so far
depend on, TBL itself when it has no leader."
  (let ((at tbl) (steps 0))
    (loop while (and (tbl-leader at)
                     (not (eq (tbl-leader at) at))
                     (< (incf steps) 1000))
          do (setf at (tbl-leader at)))
    at))

(defun %check-floor (tbl functor)
  "Refuse to consume the incomplete TBL under a negation floor when the
table it depends on is open at or below the floor (design D4)."
  (when (plusp *negation-floor*)
    (let ((dependency (%open-dependency tbl)))
      (when (and (member dependency *table-stack* :test #'eq)
                 (<= (tbl-index dependency) *negation-floor*))
        (%unstratified (functor-name functor))))))

(defun %call-tabled (functor args cont)
  "A call of the tabled FUNCTOR with ARGS: the variant's table found or
made, evaluated when it is new or stale, then consumed into CONT (the
four paths of design D3)."
  ;; One hash serves every functor, so the functor heads the key.
  (let* ((key (cons (functor-name functor) (%variant-key args)))
         (tbl (gethash key *tables*))
         (top (first *table-stack*)))
    (cond ((null tbl)
           (setf tbl (%make-tbl key (incf *table-serial*))
                 (gethash key *tables*) tbl)
           (%evaluate-tbl tbl functor args))
          ((tbl-complete tbl))
          ((member tbl *table-stack* :test #'eq)
           ;; A looping consumer: the answers so far, the rest next round.
           (%check-floor tbl functor)
           (%lower-leader top tbl))
          ((= (tbl-round tbl) *round*)
           (%check-floor tbl functor)
           (%lower-leader top (tbl-leader tbl)))
          (t (%evaluate-tbl tbl functor args)))
    (%consume-tbl tbl args cont)))

(defun %make-tabled-entry (functor)
  "The FN a tabled FUNCTOR holds: a function of the call's arguments
and the continuation.  Outside a query root it binds the per-query
state for its own extent (design D2)."
  (lambda (&rest args)
    (let* ((n (1- (length args)))
           (cont (nth n args))
           (args (subseq args 0 n)))
      (if *tables*
          (%call-tabled functor args cont)
          (let ((*tables* (make-hash-table :test 'equal))
                (*table-stack* nil)
                (*round* 0)
                (*answers-added* 0)
                (*negation-floor* 0)
                (*table-serial* 0))
            (%call-tabled functor args cont))))))

(defun table-functor (name arity)
  "Declare the user functor NAME/ARITY tabled, registering it first if
no clause has; returns the functor.  NAME is interned in the run-time
*PACKAGE*, as <- interns a clause head.  A built-in functor is refused
with PROLOG-ERROR (design D1)."
  (let ((global (make-functor-symbol name arity)))
    (when (nth-value 1 (gethash global *prolog-global-functors*))
      (error 'prolog-error
             :reason (format nil "~A is a built-in functor; only a user ~
functor can be tabled" global))))
  (let ((functor (make-functor :name (make-functor-symbol name arity
                                                          :define t)
                               :clauses nil)))
    (with-recursive-lock-held ((functor-lock functor))
      (setf (functor-tabled functor) t)
      (%install-functor-fn functor))
    functor))

(defun untable-functor (name arity)
  "Undo TABLE-FUNCTOR: NAME/ARITY answers from its clauses again.
Returns the functor, or NIL when none is registered."
  (let ((functor (lookup-functor (make-functor-symbol name arity))))
    (when functor
      (with-recursive-lock-held ((functor-lock functor))
        (setf (functor-tabled functor) nil
              (functor-entry functor) nil)
        (%install-functor-fn functor))
      functor)))

(defun functor-tabled-p (name arity)
  "True when the registered user functor NAME/ARITY is tabled."
  (let ((functor (lookup-functor (make-functor-symbol name arity))))
    (and functor (functor-tabled functor) t)))
