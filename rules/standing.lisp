;;;; rules/standing.lisp -- which production bodies deliver exactly their
;;;; query's rows, commit by commit (#470; forward-rules design §8).

(in-package #:graph-db.rules)

(defparameter *standing-query-functor-classes*
  '(;; the premises the delta sees
    ("claim/7" . :premise) ("claim-absent/7" . :premise)
    ;; a filter on a premise: its flip is the premise's own delta
    ("claim-current/1" . :filter)
    ;; readers of a slot no update changes
    ("claim-relation/2" . :reader)
    ;; the producer: a reader of a premise, a generator otherwise
    ("claim-producer/2" . :producer)
    ;; readers of a slot an update can change: a filter with a literal.
    ;; A re-open moves recorded-at, an in-place save or %REFRESH-KEPT
    ;; moves rule-version (#470 review).
    ("claim-recorded-at/2" . :mutable-reader)
    ("claim-rule-version/2" . :mutable-reader)
    ("claim-standing/2" . :mutable-reader)
    ("claim-valid-from/2" . :mutable-reader)
    ("claim-valid-to/2" . :mutable-reader)
    ("claim-valid-at/2" . :mutable-reader)
    ("claim-touched-at/2" . :mutable-reader)
    ("claim-retracted-at/2" . :mutable-reader)
    ;; tests over bound values
    ("=/2" . :test) ("/=/2" . :test) ("==/2" . :test) ("</2" . :test)
    ("<=/2" . :test) (">/2" . :test) (">=/2" . :test) ("is/2" . :test)
    ("atom/1" . :test) ("numberp/1" . :test) ("var/1" . :test)
    ("not-in-list/2" . :test) ("valid-date-p/1" . :test)
    ("regex-match/2" . :test) ("fail/0" . :test)
    ("instant</2" . :test) ("instant<=/2" . :test) ("instant>/2" . :test)
    ("instant>=/2" . :test) ("instant=/2" . :test) ("instant-offset/3" . :test)
    ;; negation as failure: over a premise form or a test
    ("not/1" . :not)
    ;; a row per true branch, unless once or not collapses them
    ("or/2" . :or)
    ;; control: a claim read under it is invisible to the delta
    ("once/1" . :control) ("if/2" . :control) ("if/3" . :control)
    ("forall/2" . :control)
    ;; aggregation and meta-call; unique/1 keeps a seen-table across
    ;; the query's solutions
    ("unique/1" . :meta)
    ("findall/3" . :meta) ("bagof/3" . :meta) ("setof/3" . :meta)
    ("call/1" . :meta) ("catch/3" . :meta) ("throw/1" . :meta)
    ("map-query/5" . :meta) ("select/2" . :meta)
    ("show-prolog-vars/2" . :meta)
    ;; effects, and goals that never belong in a standing body
    ("retract/1" . :effect) ("retract/3" . :effect) ("trigger/1" . :effect)
    ("write/1" . :effect) ("read/1" . :effect) ("nl/0" . :effect)
    ("repeat/0" . :effect) ("lisp/2" . :effect) ("lispp/1" . :effect)
    ("param/2" . :effect)
    ;; the clock
    ("now/1" . :clock)
    ;; the engine's own, withheld from text
    ("rule-delta/2" . :withheld) ("claim-after/9" . :withheld)
    ("instantiation-key/3" . :withheld) ("instantiation-key/4" . :withheld)
    ("instantiation-key/5" . :withheld) ("instantiation-key/6" . :withheld)
    ("instantiation-key/7" . :withheld) ("instantiation-key/8" . :withheld)
    ("instantiation-key/9" . :withheld) ("instantiation-key/10" . :withheld)
    ;; store reads outside the claim families: no delta reaches them
    ("is-a/2" . :store-read) ("node-slot-value/3" . :store-read)
    ("weight/2" . :store-read) ("find-by-slot/4" . :store-read)
    ("find-by-unique/4" . :store-read) ("find-slot-range/5" . :store-read)
    ("find-intersects/3" . :store-read) ("find-intersects/4" . :store-read)
    ("find-near/5" . :store-read) ("find-near/6" . :store-read)
    ("find-nearest/5" . :store-read) ("find-nearest/6" . :store-read)
    ("find-within/3" . :store-read) ("find-within/4" . :store-read)
    ("geo-distance/5" . :store-read) ("geo-near/5" . :store-read)
    ("geo-within/3" . :store-read)
    ("incoming-edges/2" . :store-read) ("incoming-edges/3" . :store-read)
    ("incoming-edges/4" . :store-read) ("outgoing-edges/2" . :store-read)
    ("outgoing-edges/3" . :store-read) ("outgoing-edges/4" . :store-read)
    ("invoke-view/4" . :store-read) ("invoke-view/5" . :store-read)
    ("invoke-reduced-view/4" . :store-read)
    ("related/3" . :store-read) ("claimed/4" . :store-read)
    ("subject-of/2" . :store-read) ("subject-of/3" . :store-read)
    ("object-of/2" . :store-read) ("object-of/3" . :store-read))
  "Every registered Prolog functor, name/arity as the inventory spells
it, with what a standing query may make of it (#470).  A functor with no
entry reads as :STORE-READ, and the rules suite insists every registered
one has an entry, so a new functor chooses here -- the third question
PROLOG-FUNCTOR-INVENTORY-IS-PINNED asks.  The classes:
:PREMISE a claim read the delta answers; :FILTER a currency filter on a
premise; :READER a premise slot no update changes; :PRODUCER a reader of
a premise or a generator; :MUTABLE-READER a slot an update can change,
a filter with a literal only; :TEST over bound values; :NOT; :OR, a row
per true branch unless under once or not; :CONTROL whose claim reads
the delta cannot see; :META aggregation, meta-call or a seen-table
across solutions; :EFFECT; :CLOCK; :WITHHELD; :STORE-READ.")

(defun %functor-key (goal)
  "GOAL's head as the inventory spells it: lowercase name/arity."
  (and (consp goal) (symbolp (first goal))
       (format nil "~(~A~)/~D" (symbol-name (first goal))
               (1- (length goal)))))

(defun %functor-class (goal)
  (let ((key (%functor-key goal)))
    (or (and key (cdr (assoc key *standing-query-functor-classes*
                             :test #'string=)))
        :store-read)))

(defun %goal-label (goal)
  "GOAL as the author wrote it: symbols bare and lowercased, strings
quoted, for a reason that names the goal."
  (with-output-to-string (s)
    (labels ((emit (x)
               (cond ((consp x)
                      (write-char #\( s)
                      (loop for (e . rest) on x
                            do (emit e)
                               (when rest (write-char #\Space s)))
                      (write-char #\) s))
                     ((null x) (write-string "nil" s))
                     ((symbolp x)
                      (write-string (string-downcase (symbol-name x)) s))
                     ((stringp x) (prin1 x s))
                     (t (princ x s)))))
      (emit goal))))

(defun %goal-variables (goal)
  (let ((vars '()))
    (labels ((walk (x)
               (cond ((%variable-p x) (pushnew x vars))
                     ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk goal))
    vars))

(defun %premise-form-p (goal)
  "GOAL is a claim read the delta answers: a variable ?c, a literal
family, and a literal relation or a family-wide read (#469)."
  (and (eq (%functor-class goal) :premise)
       (%variable-p (second goal))
       (symbolp (third goal)) (third goal) (not (%variable-p (third goal)))
       (or (stringp (sixth goal)) (%family-wide-goal-p goal))))

(defun %classify-goal (goal premises bound add &optional nested)
  "Classify GOAL for a standing query, calling ADD with each reason.
PREMISES are the body's premise variables, BOUND the variables earlier
goals bound; NESTED the control constructs GOAL sits under, innermost
first.  A claim read under NOT is a negative read at any depth, which
the full path handles; under anything else it is invisible to the
delta.  An OR is a row per true branch unless ONCE or NOT collapses
them (#470 review)."
  (let ((class (%functor-class goal))
        (label (%goal-label goal)))
    (flet ((premise-var-p (x) (member x premises))
           (under-p (name) (member name nested :test #'string=)))
      (ecase class
        (:premise
         (cond ((and nested (not (under-p "not")))
                (funcall add (format nil "~A is a claim read nested under ~
~A: the delta cannot see it" label (first nested))))
               ((not (%premise-form-p goal))
                (funcall add (format nil "~A leaves its family or relation ~
unbound without a literal family and namespace: the delta cannot route it"
                                     label)))))
        ((:filter :test))
        (:reader
         (unless (premise-var-p (second goal))
           (funcall add (format nil "~A is applied to ~A, which is not a ~
premise" label (%goal-label (second goal))))))
        (:producer
         (unless (premise-var-p (second goal))
           (funcall add (format nil "~A generates from the producer ~
index: a claim read the delta cannot see" label))))
        (:mutable-reader
         (cond ((not (premise-var-p (second goal)))
                (funcall add (format nil "~A is applied to ~A, which is ~
not a premise" label (%goal-label (second goal)))))
               ((and (%variable-p (third goal))
                     (not (member (third goal) bound)))
                (funcall add (format nil "~A reads a slot an update can ~
change into the row: the row would change with no delivery; compare ~
with a literal instead" label)))))
        (:not
         (let ((inner (second goal)))
           (cond ((eq (%functor-class inner) :premise)
                  ;; A negated read needs a literal relation: the
                  ;; compiler refuses it unbound, family-wide included.
                  (unless (and (%premise-form-p inner)
                               (stringp (sixth inner)))
                    (funcall add (format nil "~A negates a claim read ~
whose relation is unbound: compile-rule refuses it, bind the relation"
                                         label))))
                 ((member (%functor-class inner) '(:filter :test)))
                 (t (%classify-goal inner premises bound add
                                    (cons "not" nested))))))
        (:or
         (if (or (under-p "once") (under-p "not"))
             (dolist (sub (rest goal))
               (when (consp sub)
                 (%classify-goal sub premises bound add
                                 (cons "or" nested))))
             (funcall add (format nil "~A yields a row per true branch, ~
and a production holds one row per instantiation: put it under once, ~
or split the query" label))))
        (:control
         (let ((name (string-downcase (symbol-name (first goal)))))
           (dolist (sub (rest goal))
             (when (consp sub)
               (%classify-goal sub premises bound add
                               (cons name nested))))))
        (:meta
         (funcall add (format nil "~A aggregates or meta-calls: no delta ~
form" label)))
        (:effect
         (funcall add (format nil "~A is an effect or a goal no standing ~
body runs; it is refused at every run" label)))
        (:clock
         (funcall add (format nil "~A makes the rows depend on the clock: ~
a standing query has no threshold" label)))
        (:withheld
         (funcall add (format nil "~A is the engine's own" label)))
        (:store-read
         (funcall add (format nil "~A reads the store outside the claim ~
families: no delta reaches it" label)))))))

(defun standing-query-problems (text graph)
  "Why a production over TEXT would not deliver, commit by commit,
exactly the rows a full re-evaluation of TEXT adds and loses (#470):
a list of reasons, each naming its goal, or NIL when it would.  Admitted:
top-level claim/7 and claim-absent/7 with a variable ?c and a literal
relation or a family-wide read; NOT over those; tests over bound values;
a premise's slot read as a filter with a literal, or a slot no update
changes.  Everything else is a reason: another store read, a claim read
under once, if or forall, an or not under once or not, a mutable slot
read into the row, an effect, repeat, now, no premise, more than
eight.  The classes live in
*STANDING-QUERY-FUNCTOR-CLASSES*.  Text the guard refuses is one
reason, the guard's."
  (handler-case
      (multiple-value-bind (vars goals)
          (let ((graph-db.query:*guarding-rule-text* t))
            (graph-db.query:guard-query-text text graph))
        (declare (ignore vars))
        (let* ((problems '())
               (premises (%premise-vars goals))
               (bound '())
               (add (lambda (reason) (push reason problems))))
          (when (null premises)
            (funcall add "no claim/7 premise: a production body needs one"))
          (when (> (length premises) 8)
            (funcall add (format nil "~D premises; at most eight"
                                 (length premises))))
          (dolist (goal goals)
            (%classify-goal goal premises bound add)
            (setf bound (union bound (%goal-variables goal))))
          (nreverse problems)))
    (graph-db.query:prolog-guard-error (c)
      (list (graph-db.query:prolog-guard-error-reason c)))))
