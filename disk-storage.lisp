(in-package #:vivace-graph)

(defun open-store (file)
  (let ((db (make-instance 'tc-bdb)))
    ;;(dbm-cache db :non-leaf 2048 :leaf 10240)
    (dbm-open db file :READ :WRITE :CREATE)
    db))

(defun close-store (db)
  (dbm-close db))

(defun open-hash (file)
  (let ((db (make-instance 'tc-hdb)))
    ;;(dbm-cache db :non-leaf 2048 :leaf 10240)
    (dbm-open db file :READ :WRITE :CREATE)
    db))

(defun close-hash (db)
  (dbm-close db))

(defun lookup-object (db key)
  (dbm-get db key :octets))

(defun lookup-objects (db key)
  "FIXME: we need to implement TCLIST in cl-tokyo-cabinet on order to do this more efficiently."
  (let ((cursor (iter-open db))
	(result nil))
    (iter-jump cursor key)
    (do ((ikey (iter-key cursor :octets) (iter-key cursor :octets))
	 (ival (iter-get cursor :octets) (iter-get cursor :octets)))
	((not (equalp key ikey)))
      (push ival result)
      (iter-next cursor))
    (iter-close cursor)
    (reverse result)))

(defun store-object (db key value &key (mode :keep))
  (dbm-put db key value :mode mode))

(defun delete-object (db key &optional value)
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
	t)))
