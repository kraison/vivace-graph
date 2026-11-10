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
  "TBL depends on TO: when TO is on the stack (so its INDEX is live and
at or below TBL's), TBL's LEADER becomes the lower of the two by INDEX.
A TO off the stack is complete or dropped and records nothing; an
off-stack INDEX is stale and must not be compared (D3; the review's
finding 1 of 2026-11-10)."
  (when (and tbl to (member to *table-stack* :test #'eq))
    (let ((leader (tbl-leader tbl)))
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
CREATED-AT) are complete (design D3).  A table still on the stack is
mid-evaluation and is never marked from below it."
  (let ((from (tbl-created-at tbl)))
    (maphash (lambda (key other)
               (declare (ignore key))
               (when (and (>= (tbl-created-at other) from)
                          (or (eq other tbl)
                              (not (member other *table-stack*
                                           :test #'eq))))
                 (setf (tbl-complete other) t)))
             *tables*)))

(defun %drop-from (tbl)
  "TBL and every incomplete table not on the stack leave *TABLES*: an
evaluation left by a non-local exit built them half, and one that
survived could still name TBL as its leader (design D5).  The
ancestors still evaluating keep their answers, all consequences."
  (remhash (tbl-key tbl) *tables*)
  (maphash (lambda (key other)
             (when (and (not (tbl-complete other))
                        (not (member other *table-stack* :test #'eq)))
               (remhash key *tables*)))
           *tables*))

(defun %evaluate-tbl (tbl functor args)
  "Evaluate TBL per design D3: passes of the dispatcher with TBL on the
stack; a table that depends on nothing below itself leads, iterating
while a pass adds an answer anywhere, then completes its component; one
that depends on an ancestor stays incomplete and hands its leader to
the table below.  A non-local exit drops the half tables (D5).
Returns whether TBL completed, and the number of passes."
  (let ((parent (first *table-stack*))
        (completed nil)
        (returned nil)
        (passes 1))
    (unwind-protect
         (let ((*table-stack* (cons tbl *table-stack*)))
           (setf (tbl-index tbl) (length *table-stack*))
           (loop
             (let ((before *answers-added*)
                   (leader nil))
               (%run-tbl tbl functor args)
               (setf leader (tbl-leader tbl))
               (cond ((null leader)
                      ;; Nothing read was open: one pass is the fixpoint,
                      ;; and every table created inside completed on its
                      ;; own (else it would have lowered LEADER), so no
                      ;; walk of *TABLES* is needed: a lookup-shaped use
                      ;; with many variants stays linear.
                      (setf (tbl-complete tbl) t)
                      (setf completed t)
                      (return))
                     ((not (eq leader tbl))
                      (return))
                     ((= before *answers-added*)
                      (%complete-from tbl)
                      (setf completed t)
                      (return))
                     (t (incf *round*)))
             (incf passes)))
           (setf returned t))
      (unless returned
        (%drop-from tbl)))
    (unless completed
      (%lower-leader parent (tbl-leader tbl)))
    (values completed passes)))

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

(defun %table-port (functor tbl how passes)
  "Fire the :TABLE port for FUNCTOR (tracer design D1, tabling D7):
(:complete n), (:leader passes), (:follower n) or (:consume n)."
  (let ((hook *prolog-port-hook*))
    (when hook
      (funcall hook :table (functor-name functor)
               (list how (if (eq how :leader)
                             passes
                             (fill-pointer (tbl-answers tbl))))
               *prolog-port-depth*))))

(defun %call-tabled (functor args cont)
  "A call of the tabled FUNCTOR with ARGS: the variant's table found or
made, evaluated when it is new or stale, then consumed into CONT (the
four paths of design D3)."
  ;; One hash serves every functor, so the functor heads the key.
  (let* ((key (cons (functor-name functor) (%variant-key args)))
         (tbl (gethash key *tables*))
         (top (first *table-stack*)))
    (flet ((evaluate ()
             (multiple-value-bind (completed passes)
                 (%evaluate-tbl tbl functor args)
               (%table-port functor tbl (if completed :leader :follower)
                            passes))))
      (cond ((null tbl)
             (setf tbl (%make-tbl key (incf *table-serial*))
                   (gethash key *tables*) tbl)
             (evaluate))
            ((tbl-complete tbl)
             (%table-port functor tbl :complete nil))
            ((member tbl *table-stack* :test #'eq)
             ;; A looping consumer: the answers so far, the rest next round.
             (%check-floor tbl functor)
             (%lower-leader top tbl)
             (%table-port functor tbl :consume nil))
            ((= (tbl-round tbl) *round*)
             ;; Evaluated this round: the top depends on what it depends
             ;; on, the end of its chain, if that is still open.
             (%check-floor tbl functor)
             (%lower-leader top (%open-dependency tbl))
             (%table-port functor tbl :consume nil))
            (t (evaluate))))
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
