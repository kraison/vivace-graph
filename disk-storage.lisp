(in-package #:vivace-graph)

(defun open-store (file)
    (handler-case
	;;(let ((db (make-instance 'kc-dbm)))
	(let ((db (make-instance 'tc-bdb)))
	  ;;(dbm-cache db :non-leaf 2048 :leaf 10240)
	  (dbm-open db file :READ :WRITE :CREATE)
	  db)
      (error (condition)
	(error 'persistence-error :instance file :reason condition))))

(defun close-store (db)
  (handler-case
      (dbm-close db)
    (error (condition)
      (error 'persistence-error :instance db :reason condition))))

(defun open-hash (file)
  (handler-case
      ;;(let ((db (make-instance 'kc-dbm)))
      (let ((db (make-instance 'tc-hdb)))
	;;(dbm-cache db :non-leaf 2048 :leaf 10240)
	(dbm-open db file :READ :WRITE :CREATE)
	db)
      (error (condition)
	(error 'persistence-error :instance file :reason condition))))    

(defun close-hash (db)
  (handler-case
      (dbm-close db)
    (error (condition)
      (error 'persistence-error :instance db :reason condition))))

(defun lookup-object (db key)
  ;;(format t "lookup-object ~A: ~A~%" db key)
  (handler-case
      (dbm-get db key :octets)
    (error (condition)
      ;;(format t "ERROR: ~A~%" condition)
      (error 'persistence-error :instance (list :db db :key key) :reason condition))))    

(defun lookup-objects (db key)
  "FIXME: we need to implement TCLIST in cl-tokyo-cabinet on order to do this more efficiently."
  (handler-case
      (let ((cursor (iter-open db))
	    (result nil))
	(iter-jump cursor key)
	(do ((ikey (iter-key cursor :octets) (iter-key cursor :octets))
	     (ival (iter-get cursor :octets) (iter-get cursor :octets)))
	    ((not (equalp key ikey)))
	  (push ival result)
	  (iter-next cursor))
	(iter-close cursor)
	(reverse result))
    (error (condition)
      (error 'persistence-error :instance (list :db db :key key) :reason condition))))

(defun store-object (db key value &key (mode :keep))
  (handler-case
      (dbm-put db key value :mode mode)
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value :mode mode) 
	     :reason condition))))

(defun delete-object (db key &optional value)
  (handler-case
      (if (null value)
	  (dbm-rem db key)
	  (let ((cursor (iter-open db)) (done? nil))
	    (iter-jump cursor key)
	    (do ((ikey (iter-key cursor :octets) (iter-key cursor :octets))
		 (ival (iter-get cursor :octets) (iter-get cursor :octets)))
		((or done? (not (equalp key ikey))))
	      (when (equal ival value)
		(iter-rem cursor)
		(setq done? t))
	      (iter-next cursor))
	    (iter-close cursor)
	    t))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value) :reason condition))))

(defun map-hash-objects (db func &key collect?)
  (let ((cursor nil))
    (handler-case
	(unwind-protect
	     (let ((result nil))
	       (setq cursor (iter-open db))
	       (do ((ikey (iter-next cursor) (iter-next cursor)))
		   ((null ikey))
		 (let ((r (funcall func (iter-key cursor :octets))))
		   (when collect? (push r result))))
	       (nreverse result))
	  (iter-close cursor))
      (error (condition)
	(error 'persistence-error 
	       :instance (list :db db) :reason condition)))))

