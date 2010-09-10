(require 'asdf)
(asdf:oos 'asdf:load-op 'vivace-graph)

(in-package #:vivace-graph)


(defun triple-test-1 ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")) (p1 nil))
    (unwind-protect
	 (progn
	   (with-transaction ((triple-db *graph*))
	     (dotimes (i 100)
	       (let ((p (format nil "P~A" (random 3))))
		 (when (equal p "P1") (push i p1))
		 (format t "~A: ~A~%" i
			 (triple-uuid
			  (add-triple (format nil "S~A" i) 
				      p
				      (format nil "O~A" i)))))))
	   (let ((r (get-triples :p "P1")))
	     (format t "Got triples for P1: ~A~%" r)
	     (when (klist? r) (klist-free r)))
	   (format t "Should have ~A P1 triples.~%" p1))
      (progn
	(if (graph? *graph*) (shutdown-graph *graph*))
	(ignore-errors 
	  (cl-fad:delete-directory-and-files "/var/tmp/full-text-idx")
	  (delete-file "/var/tmp/functors.kch")
	  (delete-file "/var/tmp/rules.kch")
	  (delete-file "/var/tmp/deleted-triples.kct")
	  (delete-file "/var/tmp/triples.kct")
	  (delete-file "/var/tmp/config.ini"))))))

(defun triple-test-2 ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")))
    (unwind-protect
	 (let ((tuples nil))
	   (dotimes (i 1000)
	     (push (list (format nil "S~A" i) (format nil "P~A" i) (format nil "O~A" i)) tuples))
	   (time (bulk-add-triples *graph* tuples)))
      (progn
	(if (graph? *graph*) (shutdown-graph *graph*))
	(ignore-errors 
	  (cl-fad:delete-directory-and-files "/var/tmp/full-text-idx")
	  (delete-file "/var/tmp/functors.kch")
	  (delete-file "/var/tmp/deleted-triples.kct")
	  (delete-file "/var/tmp/rules.kch")
	  (delete-file "/var/tmp/triples.kct")
	  (delete-file "/var/tmp/config.ini"))))))

(defun triple-test-3 ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")))
    (unwind-protect
	 (progn
;	   (make-new-node :value "Kevin")
;	   (make-new-node :value "Dustie")
;	   (make-new-node :value "Echo")
;	   (make-new-node :value "cat nip")
;	   (make-new-node :value "cat")
;	   (make-new-node :value "Homo Sapien")
;	   (make-new-predicate :name 'loves) 
;	   (make-new-predicate :name 'is-a) 
	   (add-triple "Kevin" "loves" "Dustie")
	   (add-triple "Kevin" "loves" "Echo")
	   (add-triple "Dustie" "loves" "Kevin")
	   (add-triple "Echo" "loves" "cat nip")
	   (add-triple "Echo" "is-a" "cat")
	   (add-triple "Kevin" "is-a" "Homo Sapien")
	   (add-triple "Dustie" "is-a" "Homo Sapien")
	   (format t "Who loves whom? -> ~A~%" (get-triples :p 'loves))
	   (format t "What species? -> ~A~%" (get-triples :p 'is-a))
	   (format t "~A~%" (map-klist #'(lambda (i) (lookup-triple-by-id i))
				       (get-subjects "Kevin") :collect? t)))
      (progn
	(if (graph? *graph*) (shutdown-graph *graph*))
	(ignore-errors 
	  (cl-fad:delete-directory-and-files "/var/tmp/full-text-idx")
	  (delete-file "/var/tmp/functors.kch")
	  (delete-file "/var/tmp/deleted-triples.kct")
	  (delete-file "/var/tmp/rules.kch")
	  (delete-file "/var/tmp/triples.kct")
	  (delete-file "/var/tmp/config.ini"))))))

(defun triple-test-4 ()
  (let ((*graph* (make-new-graph :name "test graph 4" :location "/var/tmp")))
    (unwind-protect
	 (let ((predicates (make-array 0 :adjustable t :fill-pointer t)))
	   (time
	    (dotimes (i 2000)
	      (vector-push-extend (make-new-predicate :name (format nil "P~A" i)) predicates)))
;	   (time (dotimes (i 100000)
;	     (make-new-node-unsafe :value i)))
;	   (format t "Added 100k nodes.  Adding triples...~%")
	   (let ((c 0))
	     (declare (type fixnum c))
	     (time (dotimes (i 1000)
		     (loop for j from 99999 downto 99799 do
			  (incf c)
			  (when (= 0 (mod c 1000))
			    (format t "On triple ~A~%" c))
			  (add-triple-unsafe *graph* 
					     ;;(lookup-node i *graph*) 
					     i
					     (aref predicates (random 2000))
	                                     ;;(lookup-node j *graph*)))))
					     j 
					     :cache? nil
					     :index? t))))
	     (format t "Finished adding and indexing ~A triples.~%" c))
;	   (let ((c 0))
;	     (time (loop until (sb-concurrency:queue-empty-p (needs-indexing-q *graph*)) do
;			(incf c)
;			(when (= 0 (mod c 1000))
;			  (format t "Indexing triple ~A~%" c))
;			(index-triple-unsafe (sb-concurrency:dequeue (needs-indexing-q *graph*))))))
	   (format t "Added all triples~%"))
      (progn
	(if (graph? *graph*) (shutdown-graph *graph*))
	(ignore-errors 
	  (cl-fad:delete-directory-and-files "/var/tmp/full-text-idx")
	  (delete-file "/var/tmp/functors.kch")
	  (delete-file "/var/tmp/rules.kch")
	  (delete-file "/var/tmp/triples.kct")
	  (delete-file "/var/tmp/deleted-triples.kct")
	  (delete-file "/var/tmp/config.ini"))))))


(triple-test-4)

(defun serialize-test ()
  (dotimes (j 100)
    (let ((pointers nil))
      (dotimes (x 100)
	(let ((i (random 1000000000000000)))
	  (multiple-value-bind (ptr sz) (serialize (format nil "~A" i))
	    (format t "Serialized ~A to addr ~A: " i (pointer-address ptr))
	    (dump-pointer ptr sz)
	    (format t "deserialized to ~A~%" (deserialize ptr sz))
	    (push ptr pointers))))
      (dolist (ptr pointers)
	(foreign-free ptr)))))

#|

(defun reload-testdb ()
  (if (graph? *graph*) (shutdown-graph *graph*))
  (ignore-errors 
    (cl-fad:delete-directory-and-files "/home/raison/work/vivace-graph/db/full-text-idx")
    (delete-file "/home/raison/work/vivace-graph/db/functors.kch")
    (delete-file "/home/raison/work/vivace-graph/db/rules.kch")
    (delete-file "/home/raison/work/vivace-graph/db/deleted-triples.kct")
    (delete-file "/home/raison/work/vivace-graph/db/triples.kct"))
  (load-graph! "/home/raison/work/vivace-graph/db/config.ini")
  (<- (member ?item (?item . ?rest)))
  (<- (member ?item (?x . ?rest)) (member ?item ?rest))
  (<- (is-of-age ?x) (has-age ?x ?y) (>= ?y 21) !)
  (<- (lovers ?x ?y) (loves ?x ?y) (loves ?y ?x))
  (<- (has-age "Kevin" 35))
  (<- (has-age "Dustie" 35))
  (<- (has-age "Echo" 11))
  (<- (loves "Sonny" "Cher"))
  (<- (loves "Cher" "Sonny"))
  (<- (loves "Kevin" "Dustie"))
  (<- (loves "Dustie" "Kevin"))
  (<- (loves "Kevin" "Echo"))
  (<- (loves "Echo" "cat nip"))
  (<- (likes "Robin" "cats"))
  (<- (likes "Kevin" "cats"))
  (<- (likes "Sandy" ?x) (likes ?x "cats"))
  *graph*)

(defun who-likes (person)
  (declare (special person))
  (select-flat (?x) (lisp ?y (princ-to-string person)) (likes ?x ?y)))

(defun ptest1 ()
  (let ((*graph* (make-new-graph :name "test graph 1" :location "/var/tmp")))
    (unwind-protect
	 (progn
	   (<- (member ?item (?item . ?rest)))
	   (<- (member ?item (?x . ?rest)) (member ?item ?rest))
	   (format t "~%(?- (member 1 (4 3 2 1))) ->~%")
	   (?- (member 1 (4 3 2 1)))
	   (format t "~%(member ?x (1 2 3 4)) ->~%")
	   (?- (member ?x (1 2 3 4)))
	   (format t "~%Adding triples:~%")
	   (<- (loves "Kevin" "Dustie"))
	   (<- (loves "Dustie" "Kevin"))
	   (<- (loves "Kevin" "Echo"))
	   (<- (loves "Echo" "cat nip"))
	   (format t "(loves ?x ?y)) -> ~%")
	   (?- (loves ?x ?y))
	   (format t "(loves \"Kevin\" \"Dustie\")) -> ~%")
	   (?- (loves "Kevin" "Dustie"))
	   (format t "(loves \"Kevin\" ?y)) -> ~%")
	   (?- (loves "Kevin" ?y))
	   (<- (likes "Robin" "cats"))
	   (<- (likes "Kevin" "cats"))
	   (<- (likes "Sandy" ?x) (likes ?x "cats"))
	   (format t "(select-flat (?who) (likes \"Sandy\" ?who)) ->~%")
	   (format t "~A~%" (select-flat (?who) (likes "Sandy" ?who))))
      (progn
	(shutdown-graph *graph*)
	(cl-fad:delete-directory-and-files "/var/tmp/full-text-idx")
	(delete-file "/var/tmp/triples.kct")
	(delete-file "/var/tmp/deleted-triples.kct")
	(delete-file "/var/tmp/functors.kch")
	(delete-file "/var/tmp/rules.kch")
	(delete-file "/var/tmp/config.ini")))))

(defun ptest2 ()
  ;; 4.10 seconds in interpreted mode
  ;; 0.083 seconds in compiled mode
  (let ((*graph* (make-new-graph :name "test graph 2" :location "/var/tmp")))
    (unwind-protect
	 (progn
	   (<- (member ?item (?item . ?rest)))
	   (<- (member ?item (?x . ?rest)) (member ?item ?rest))
	   (<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
	   (<- (nextto ?x ?y ?list) (iright ?y ?x ?list))
	   (<- (iright ?left ?right (?left ?right . ?rest)))
	   (<- (iright ?left ?right (?x . ?rest))
	       (iright ?left ?right ?rest))
	   ;;(<- (= ?x ?x)) ;; Now a compiler macro
	   (<- (zebra ?h ?w ?z)
	       (= ?h ((house norwegian ? ? ? ?)
		      ?
		      (house ? ? ? milk ?) ? ?))
	       (member (house englishman ? ? ? red) ?h)
	       (member (house spaniard dog ? ? ?) ?h)
	       (member (house ? ? ? coffee green) ?h)
	       (member (house ukranian ? ? tea ?) ?h)
	       (iright (house ? ? ? ? ivory)
		       (house ? ? ? ? green) ?h)
	       (member (house ? snails winston ? ?) ?h)
	       (member (house ? ? kools ? yellow) ?h)
	       (nextto (house ? ? chesterfield ? ?)
		       (house ? fox ? ? ?) ?h)
	       (nextto (house ? ? kools ? ?)
		       (house ? horse ? ? ?) ?h)
	       (member (house ? ? luckystrike orange-juice ?) ?h)
	       (member (house japanese ? parliaments ? ?) ?h)
	       (nextto (house norwegian ? ? ? ?)
		       (house ? ? ? ? blue) ?h)
	       (member (house ?w ? ? water ?) ?h)
	       (member (house ?z zebra ? ? ?) ?h))
	   (time (select (?houses ?water-drinker ?zebra-owner) 
			 (zebra ?houses ?water-drinker ?zebra-owner))))
      (progn
	(shutdown-graph *graph*)
	(cl-fad:delete-directory-and-files "/var/tmp/full-text-idx")
	(delete-file "/var/tmp/triples.kct")
	(delete-file "/var/tmp/deleted-triples.kct")
	(delete-file "/var/tmp/functors.kch")
	(delete-file "/var/tmp/rules.kch")
	(delete-file "/var/tmp/config.ini")))))

(defrule t1
  if 
  (and
   (or (is-a ?x "dog") (is-a ?x "human"))
   (and (has-age ?x ?y) (> ?y 23)))
  (or (likes ?x "cats") (likes ?x "lizards"))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defrule t2
  if 
  (or
   (and (is-a ?x "dog") (likes ?x "cats"))
   (and (is-a ?x "dog") (likes ?x "lizards"))
   (and (is-a ?x "human") (likes ?x "lizards")))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defrule t3
  if 
  (or
   (and (is-a ?x "dog") (likes ?x "cats")
	(is-a ?y "dog") (likes ?y "cats"))
   (and (is-a ?x "human") (likes ?x "lizards")
	(is-a ?y "human") (likes ?y "lizards")))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defun test-rules ()
  (match-rules (lookup-triple "Kevin" 'likes "cats"))
  (run-rules *graph*)
  (format t "DONE. SKIP LIST SHOULD BE EMPTY, LENGTH IS ~A:~%" 
	  (skip-list-length (production-pq *graph*)))
  (map-skip-list #'(lambda (k v) (format t "~A: ~A~%" k (type-of v))) (production-pq *graph*)))
|#