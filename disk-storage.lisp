(in-package #:vivace-graph)

(defun open-store (file)
    (handler-case
	(let ((db (make-instance 'kc-dbm)))
	;;(let ((db (make-instance 'tc-bdb)))
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
      (let ((db (make-instance 'kc-dbm)))
      ;;(let ((db (make-instance 'tc-hdb)))
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
  (handler-case
      (dbm-get db key :octets)
    (error (condition)
      (error 'persistence-error :instance (list :db db :key key) :reason condition))))    

(defun lookup-objects (db key)
  (handler-case
      (let ((cursor (iter-open db)) (result nil))
	(iter-go-to cursor key)
	(loop
	   (multiple-value-bind (ikey ival) (iter-item cursor :key-type :octets) 
	     (when (or (not (equal key ikey)) (null ikey))
	       (return))
	     (push ival result)
	     (iter-next cursor)))
	(nreverse result))
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
	  (dbm-remove db key)
	  (let ((cursor (iter-open db)))
	    (iter-go-to cursor key)
	    (loop
	       (multiple-value-bind (ikey ival) (iter-item cursor :key-type :octets) 
		 (when (or (not (equal key ikey)) (null ikey))
		   (return))
		 (when (equal ival value)
		   (iter-rem cursor)
		   (return))
		 (iter-next cursor)))
	    t))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value) :reason condition))))

(defun map-hash-objects (db func &key collect?)
  (handler-case
      (let ((result nil) (cursor (iter-open db)))
	(iter-first cursor)
	(loop
	   (multiple-value-bind (key val) (iter-item cursor :key-type :octets) 
	     (when (null key) (return))
	     (let ((r (funcall func key val)))
	       (when collect? (push r result))))
	   (iter-next cursor))
	(nreverse result))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db) :reason condition))))

