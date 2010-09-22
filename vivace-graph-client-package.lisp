(in-package #:cl-user)

(defpackage #:vivace-graph-client
  (:use #:cl 
	#:cffi 
	#:bordeaux-threads 
	#:kyoto-persistence
	#:local-time
	#:usocket))

