;;;; SB-PROFILE Deterministic Tracing Wrapper for VivaceGraph Profiler
(in-package #:graph-db/profiler)



(defstruct profile-entry
  (name "" :type string)
  (calls 0 :type fixnum)
  (seconds 0.0d0 :type double-float)
  (sec-per-call 0.0d0 :type double-float)
  (bytes 0 :type fixnum)
  (bytes-per-call 0.0d0 :type double-float))

(defstruct profile-result
  (entries '() :type list))

(defun parse-integer-clean (str)
  "Parse integer after removing commas or spaces."
  (let ((clean (ppcre:regex-replace-all "[,\\s]" str "")))
    (if (zerop (length clean)) 0 (parse-integer clean))))

(defun parse-sb-profile-report-string (report-str)
  "Parse text output from (sb-profile:report) into a PROFILE-RESULT object."
  (let ((entries '()))
    (with-input-from-string (s report-str)
      (loop for line = (read-line s nil nil) while line do
        (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
          ;; 6-column format: "seconds | gc | consed | calls | sec/call | name"
          (ppcre:register-groups-bind (sec gc consed calls spc fname)
              ("^\\s*([0-9.]+)\\s+\\|\\s*([0-9.]+)\\s+\\|\\s*([0-9,]+)\\s+\\|\\s*([0-9,]+)\\s+\\|\\s*([0-9.]+)\\s+\\|\\s*(.+)$" trimmed)
            (declare (ignore gc))
            (let* ((sec-val (parse-float:parse-float sec :type 'double-float))
                   (calls-val (parse-integer-clean calls))
                   (spc-val (parse-float:parse-float spc :type 'double-float))
                   (bytes-val (parse-integer-clean consed))
                   (bpc-val (if (plusp calls-val) (float (/ bytes-val calls-val) 1.0d0) 0.0d0)))
              (push (make-profile-entry
                     :name (string-trim '(#\Space) fname)
                     :calls calls-val
                     :seconds sec-val
                     :sec-per-call spc-val
                     :bytes bytes-val
                     :bytes-per-call bpc-val)
                    entries))))))
    (make-profile-result :entries (nreverse entries))))


(defmacro with-sb-profile-tracing ((function-symbols &key (reset t)) &body body)
  "Profile the specified FUNCTION-SYMBOLS with SB-PROFILE during BODY execution.
Returns (values RESULT PROFILE-RESULT)."
  #+sbcl
  `(let* ((valid-syms (remove-if-not (lambda (s) (and (symbolp s) (fboundp s)))
                                    ,function-symbols))
          (report-str nil)
          (body-result nil))
     (ignore-errors (sb-profile:unprofile))
     (when valid-syms
       (eval `(sb-profile:profile ,@valid-syms)))
     (when ,reset
       (sb-profile:reset))
     (unwind-protect
          (setf body-result (progn ,@body))
       (setf report-str (with-output-to-string (s)
                          (let ((*trace-output* s)
                                (*standard-output* s))
                            (sb-profile:report))))

       (ignore-errors (sb-profile:unprofile)))
     (values body-result (parse-sb-profile-report-string (or report-str ""))))
  #-sbcl
  `(values (progn ,@body) (make-profile-result :entries '())))
