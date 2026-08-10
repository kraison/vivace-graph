;;;; TEMPORAL-EXTENT: an interval or an instant, each endpoint a range, with
;;;; the precision that produced those ranges and the standing that says how
;;;; we came to know them (GH #130, design §3).

(in-package #:graph-db.spacetime)

(defparameter +precisions+
  '(:year :month :day :hour :minute :second :nsec)
  "Granularities a record may be stated at.  PRECISION never enters
comparison -- the bound width already encodes it (design §3.2).")

(defparameter +precision-units+
  '((:year . :year) (:month . :month) (:day . :day) (:hour . :hour)
    (:minute . :minute) (:second . :sec) (:nsec . :nsec))
  "Our precision names mapped to LOCAL-TIME's arithmetic unit names.")

(defstruct (temporal-extent
            (:conc-name extent-)
            (:constructor %make-extent
                (kind start end precision semantics standing))
            (:copier nil))
  "KIND is :INSTANT or :INTERVAL.  For an :INSTANT, START and END are the
SAME bound object -- that identity is the endpoint coupling (design §3.3)."
  (kind nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t)
  (precision nil :read-only t)
  (semantics nil :read-only t)
  (standing nil :read-only t))

(defun extent-instant-p (e)
  "True when E is a degenerate extent -- one timestamp, not a span."
  (eq (extent-kind e) :instant))

(defun %check-precision (p)
  (unless (member p +precisions+)
    (error 'invalid-extent
           :reason (format nil "~S is not a precision" p)))
  p)

(defun make-interval (start end &key (precision :nsec) (semantics :event)
                                     (standing :observed))
  "An extent spanning [START, END], both BOUNDs, whose endpoints move
independently.  Intervals are closed (design §3.2).  Signals INVALID-EXTENT
when START and END compare := -- that is a point in time; use MAKE-INSTANT."
  (when (eq (bound-compare start end) :=)
    (error 'invalid-extent
           :reason "START = END exactly -- a point in time; use MAKE-INSTANT"))
  (%make-extent :interval start end
                (%check-precision precision) semantics
                (check-standing standing)))

(defun make-instant (bound &key (precision :nsec) (semantics :event)
                                (standing :observed))
  "A degenerate extent: one timestamp, positioned somewhere in BOUND.  START
and END share the bound, so the two endpoints cannot move apart."
  (%make-extent :instant bound bound
                (%check-precision precision) semantics
                (check-standing standing)))

(defun granule-bounds (timestamp precision)
  "The first and last instants of the PRECISION granule containing
TIMESTAMP, as two values, computed in UTC (design §3.5)."
  (%check-precision precision)
  (let ((z local-time:+utc-zone+))
    (multiple-value-bind (nsec sec minute hour day month year)
        (local-time:decode-timestamp timestamp :timezone z)
      (let ((start (ecase precision
                     (:year (local-time:encode-timestamp
                             0 0 0 0 1 1 year :timezone z))
                     (:month (local-time:encode-timestamp
                              0 0 0 0 1 month year :timezone z))
                     (:day (local-time:encode-timestamp
                            0 0 0 0 day month year :timezone z))
                     (:hour (local-time:encode-timestamp
                             0 0 0 hour day month year :timezone z))
                     (:minute (local-time:encode-timestamp
                               0 0 minute hour day month year :timezone z))
                     (:second (local-time:encode-timestamp
                               0 sec minute hour day month year :timezone z))
                     (:nsec (local-time:encode-timestamp
                             nsec sec minute hour day month year
                             :timezone z)))))
        (values start
                (if (eq precision :nsec)
                    start
                    (local-time:timestamp-
                     (local-time:timestamp+
                      start 1 (cdr (assoc precision +precision-units+)))
                     1 :nsec)))))))

(defun make-granule-interval (timestamp precision &rest args)
  "The granule itself, as an interval with EXACT endpoints -- \"January
2026\".  Contrast MAKE-GRANULE-INSTANT (design §3.3)."
  (%check-precision precision)
  (multiple-value-bind (start end) (granule-bounds timestamp precision)
    (apply #'make-interval (exact-bound start) (exact-bound end)
           :precision precision args)))

(defun make-granule-instant (timestamp precision &rest args)
  "One timestamp known only to PRECISION -- \"sometime in January 2026\"."
  (%check-precision precision)
  (multiple-value-bind (start end) (granule-bounds timestamp precision)
    (apply #'make-instant (make-bound start end)
           :precision precision args)))

(defun %bound->sexp (b)
  (list (bound-earliest b) (bound-latest b)))

(defun %sexp->bound (s)
  (make-bound (first s) (second s)))

(defun extent->sexp (e)
  "A tree of values GRAPH-DB:SERIALIZE already handles -- keywords,
integers and LOCAL-TIME:TIMESTAMPs.  No core type byte is reserved
(design §6).  An :INSTANT writes ONE bound, so the codec cannot lose the
endpoint coupling."
  (list :temporal-extent 1
        (extent-kind e)
        (%bound->sexp (extent-start e))
        (if (extent-instant-p e) nil (%bound->sexp (extent-end e)))
        (extent-precision e)
        (extent-semantics e)
        (extent-standing e)))

(defun sexp->extent (s)
  "Inverse of EXTENT->SEXP.  Signals INVALID-EXTENT on an unknown tag or
version."
  (destructuring-bind (tag version kind start end precision semantics
                       standing)
      s
    (unless (and (eq tag :temporal-extent) (eql version 1))
      (error 'invalid-extent
             :reason (format nil "not a version-1 extent sexp: ~S ~S"
                             tag version)))
    (ecase kind
      (:instant (make-instant (%sexp->bound start) :precision precision
                              :semantics semantics :standing standing))
      (:interval (make-interval (%sexp->bound start) (%sexp->bound end)
                                :precision precision :semantics semantics
                                :standing standing)))))
