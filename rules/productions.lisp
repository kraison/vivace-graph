;;;; rules/productions.lisp -- the agenda: firing claims a step changed
;;;; become action calls (forward-rules design §7; #456).  The record,
;;;; DEF-PRODUCTION and the compile live in schema.lisp and compile.lisp.

(in-package #:graph-db.rules)

(defun %production-of (compiled-rules claim)
  "The compiled production whose FIRING claim CLAIM is, or NIL."
  (find-if (lambda (c)
             (and (compiled-production-p c)
                  (string= (rule-spec-name (compiled-rule-spec c))
                           (graph-db.spacetime:claim-relation claim))))
           compiled-rules))

(defun %match-bindings (graph compiled premises)
  "The body's variable bindings for the match PREMISES -- the body run
once with each premise variable bound to its own premise, body order
(#466) -- as an alist of (name . value) over COMPILED-RULE-BODY-VARS,
or NIL when it no longer matches (a :LOST event) or a premise is gone
(a NIL in PREMISES: the body is not run, so no other row is reported
as this one)."
  (when (some #'null premises)
    (return-from %match-bindings nil))
  (handler-case
      (let* ((vars (compiled-rule-body-vars compiled))
             (binders (loop for v in (compiled-rule-premise-vars compiled)
                            for p in premises
                            collect (list '= v p)))
             (row (first (let ((graph-db::*claim-scope* (list graph)))
                           (%solutions compiled graph
                                       (%make-rule-report)
                                       (append binders
                                               (compiled-rule-goals
                                                compiled))
                                       vars)))))
        (and row
             (loop for v in vars
                   for value in row
                   collect (cons (symbol-name v) value))))
    (error (c)
      (let ((state (%forward-state graph)))
        (when state
          (%note-failure state (rule-spec-name (compiled-rule-spec compiled))
                         (princ-to-string c) :tag :match :condition c)))
      nil)))

(defun %match-keys (claim)
  "The FIRING claim CLAIM's premise identity keys in body order, read
from its object key (#471) -- so a :LOST match whose premise is gone
still has them."
  (graph-db::%split-joined-keys
   (graph-db.spacetime:claim-object-key claim)))

(defun match-row (match)
  "MATCH, an action's second argument, as RUN-GUARDED-PROLOG :FORMAT
:DATA renders that solution of the body: (VALUES COLUMNS CELLS),
COLUMNS the body's variables as camelCase field names, CELLS
JSON-shaped (a node its id string).  Both NIL when the match carries
no bindings -- a :LOST whose body no longer matches (#466)."
  (let ((bindings (getf match :bindings)))
    (values (mapcar (lambda (b)
                      (graph-db::%query-var-field (make-symbol (car b))))
                    bindings)
            (mapcar (lambda (b) (graph-db::%query-value->json (cdr b)))
                    bindings))))

(defun %fire-production (graph state compiled event claim)
  "One agenda entry: the action, with the match's key, premises and
bindings; a signal is recorded on FAILURES and the agenda goes on."
  (let* ((spec (compiled-rule-spec compiled))
         (name (rule-spec-name spec))
         (action (gethash (production-spec-action spec) *production-actions*))
         (keys (%match-keys claim))
         (premises (mapcar (lambda (k) (%claim-by-identity-key graph k))
                           keys)))
    (if (null action)
        (%note-failure state name
                       (format nil "action ~S is not registered"
                               (production-spec-action spec))
                       :tag :action)
        (handler-case
            (let ((*agenda-production* compiled))
              (funcall action event
                       (list :key (format nil "~{~A~^|~}" keys)
                             :premises (remove nil premises)
                             :bindings (%match-bindings graph compiled
                                                        premises)
                             :firing claim)))
          (error (c)
            (log:error "production ~A on ~S: ~A" name
                       (graph-db:graph-name graph) c)
            (%note-failure state name (princ-to-string c)
                           :tag :action :condition c))))))

(defun run-agenda (graph compiled-rules changes)
  "*AGENDA-RUNNER*: the FIRING claims among CHANGES (a step's (claim .
weight) in arrival order), as agenda entries -- :ADDED for +1, :LOST
for -1 -- sorted by salience descending, then arrival, then key, each
fired through its production's action.  Returns nothing."
  (let ((state (%forward-state graph))
        (entries '())
        (epoch 0))
    (when state
      (dolist (ch changes)
        (let* ((claim (car ch))
               (compiled (and (typep claim 'firing)
                              (%production-of compiled-rules claim))))
          (when compiled
            (push (list compiled
                        (if (plusp (cdr ch)) :added :lost)
                        (incf epoch)
                        (graph-db.spacetime:claim-subject-key claim)
                        claim)
                  entries))))
      (setf entries
            (sort entries
                  (lambda (a b)
                    (let ((sa (production-spec-salience
                               (compiled-rule-spec (first a))))
                          (sb (production-spec-salience
                               (compiled-rule-spec (first b)))))
                      (cond ((/= sa sb) (> sa sb))
                            ((/= (third a) (third b)) (< (third a) (third b)))
                            (t (string< (fourth a) (fourth b))))))))
      (dolist (e entries)
        (%fire-production graph state (first e) (second e) (fifth e))))
    (values)))

(setf *agenda-runner* 'run-agenda)
