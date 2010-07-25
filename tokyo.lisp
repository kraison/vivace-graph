(in-package #:vivace-graph)


(defun tokyo-test ()
  (with-database (db "db/test-db" 'tc-bdb :READ :WRITE :CREATE)
    (let ((thr1 (make-thread
		 #'(lambda ()
		     (with-transaction (db)
		       (dotimes (i 1000)
			 (dbm-put db 
				  (serialize (format nil "k~A" i)) 
				  (serialize (format nil "v~A" i)) :mode :duplicate))))))
	  (thr2 (make-thread
		 #'(lambda ()
		     (with-transaction (db)
		       (dotimes (i 1000)
			 (dbm-put db 
				  (serialize (format nil "k~A" i)) 
				  (serialize (format nil "V~A" i)) :mode :duplicate)))))))
      (join-thread thr1)
      (join-thread thr2)
      (let ((cursor (iter-open db))
	    (result nil))
	(iter-jump cursor (serialize "k50"))
	(do ((ikey (iter-key cursor :octets) (iter-key cursor :octets))
	     (ival (iter-get cursor :octets) (iter-get cursor :octets))
	     (count 0 (incf count)))
	    ((or (not (equalp (serialize "k50") ikey))
		 (> count 10)))
	  (push ival result)
	  (iter-next cursor))
	(dolist (r (reverse result))
	  (format t "~A~%" (deserialize r)))
	(iter-close cursor)))))

	(format t "~A: ~A, ~A: ~A~%" 
		(iter-key cursor :octets)
		(deserialize (iter-get cursor :octets))
		(progn
		  (iter-next cursor)
		  (iter-key cursor :octets))
		(deserialize (iter-get cursor :octets)))
;		(deserialize (dbm-get db (serialize "k1") :octets))
;		(deserialize (dbm-get db (serialize "k1") :octets)))))))
