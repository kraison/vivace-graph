;;;; GH #135 -- SAVE of a COPY of a node created in the SAME transaction
;;;; commits cleanly, closes cleanly, and then the graph cannot be opened.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive \
;;;;        --load repro-135-deserialization.lisp
;;;;
;;;; Expected on a broken build:
;;;;   commit: ok
;;;;   close:  ok
;;;;   open:   DESERIALIZATION-ERROR
;;;;
;;;; The contract being violated: UPDATE-NODE requires "a COPY (made with COPY
;;;; inside the current transaction) of an EXISTING node".  A node created in
;;;; this same transaction does not exist yet.  SAVE catches the neighbouring
;;;; misuse (a non-copy) with MODIFYING-NON-COPY at the point of error; this
;;;; one is unguarded and surfaces at the next OPEN-GRAPH.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(ql:quickload :graph-db :silent t)
(in-package :graph-db)

(setf (gethash :repro-135d *schema-node-metadata*) nil)

(def-vertex r135d ()
  ((a :initarg :a :accessor r135d-a)
   (b :initarg :b :initform nil :accessor r135d-b))
  :repro-135d)

(defun report (label thunk)
  (format t "~&~12A ~A~%" label
          (handler-case (progn (funcall thunk) "ok")
            (error (e) (format nil "~A~@[ -- ~A~]"
                               (type-of e)
                               (ignore-errors (princ-to-string e)))))))

(let ((dir "/tmp/repro-135d/") (id nil) (g nil))
  (when (probe-file dir)
    (uiop:delete-directory-tree (pathname dir) :validate t
                                              :if-does-not-exist :ignore))
  (ensure-directories-exist dir)
  (setq g (make-graph :repro-135d dir :buffer-pool-size 1000))

  (report "commit:"
          (lambda ()
            (let ((*graph* g))
              (with-transaction ()
                (let* ((n (make-r135d :a "A"))
                       ;; The whole bug: N was created in THIS transaction,
                       ;; so it does not exist yet and must not be copied.
                       ;; Nothing complains.
                       (c (copy n)))
                  (setf (r135d-b c) "B")
                  (save c)
                  (setq id (id c)))))))

  ;; The value is readable for the rest of the session, which is what makes
  ;; the mistake invisible until a restart.
  (let ((*graph* g))
    (format t "~12A a=~S b=~S~%" "in-session:"
            (r135d-a (lookup-vertex id)) (r135d-b (lookup-vertex id))))

  (report "close:" (lambda () (close-graph g)))

  ;; OPEN-GRAPH alone succeeds -- the damage is in the NODE, not the graph.
  (report "open:" (lambda ()
                    (let ((g2 (open-graph :repro-135d dir)))
                      (close-graph g2 :snapshot-p nil))))

  ;; Reading the node back is what signals.  This is the failing step; a
  ;; repro that opens and closes without touching the node sees nothing.
  (report "read back:"
          (lambda ()
            (let ((g2 (open-graph :repro-135d dir)))
              (unwind-protect
                   (let ((*graph* g2))
                     (r135d-b (lookup-vertex id)))
                (close-graph g2 :snapshot-p nil)))))

  (format t "~%The same sequence split across TWO transactions -- create and ~
commit,~%then copy, setf and save -- is correct and reads back fine.~%"))
