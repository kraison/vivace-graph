(defpackage "SB-LOCATIVE"
    (:use "CL" "SB-EXT" "SB-VM" "SB-KERNEL" "SB-INT")
  (:export "MAKE-LOCATIVE" "COMPARE-AND-SWAP-LOCATIVE" "CONTENTS" "OFFSET"))

(in-package "SB-VM")
(defknown sb-locative:compare-and-swap-locative (t (signed-byte #.n-word-bits) t t) (values t &optional)
    (always-translatable))
(defknown sb-locative:contents (t (signed-byte #.n-word-bits)) (values t &optional)
    (always-translatable))
(defknown sb-locative::%set-contents (t t (signed-byte #.n-word-bits)) (values t &optional)
    (always-translatable))

(define-vop (sb-locative:compare-and-swap-locative)
  (:translate sb-locative:compare-and-swap-locative)
  (:policy :fast-safe)
  (:args (base :scs (descriptor-reg) :to :eval)
         (offs :scs (signed-reg) :to :result)
         (old  :scs (descriptor-reg) :target rax)
         (new  :scs (descriptor-reg)))
  (:arg-types t signed-num t t)
  (:temporary (:sc descriptor-reg :offset rax-offset
                   :from (:argument 2) :to :result :target res)
              rax)
  (:results (res :scs (descriptor-reg)))
  (:result-types t)
  (:generator 5
    (move rax old)
    (inst cmpxchg (make-ea :qword :base base :index offs)
          new :lock)
    (move res rax)))

(define-vop (sb-locative:contents)
  (:translate sb-locative:contents)
  (:policy :fast-safe)
  (:args (base :scs (descriptor-reg))
         (offs :scs (signed-reg)))
  (:arg-types t signed-num)
  (:results (res :scs (descriptor-reg)))
  (:result-types t)
  (:generator 2
    (inst mov res (make-ea :qword :base base :index offs))))

(define-vop (sb-locative::%set-contents)
  (:translate sb-locative::%set-contents)
  (:policy :fast-safe)
  (:args (value :scs (descriptor-reg) :to :result :target res)
         (base :scs (descriptor-reg))
         (offs :scs (signed-reg)))
  (:arg-types t t signed-num)
  (:results (res :scs (descriptor-reg)))
  (:result-types t)
  (:generator 4
    (inst mov (make-ea :qword :base base :index offs) value)
    (move res value)))

(in-package "SB-LOCATIVE")

(defmacro make-locative (place &environment env)
  (destructuring-bind (op . args) (macroexpand place env)
    (case op
      ((car first)
         `(values (the cons ,@args) ,(- (* n-word-bytes cons-car-slot) list-pointer-lowtag)))
      ((cdr rest)
         `(values (the cons ,@args) ,(- (* n-word-bytes cons-cdr-slot) list-pointer-lowtag)))
      (svref
         (destructuring-bind (vector index) args
           (let ((v (gensym "VECTOR")))
             `(let ((,v ,vector))
                (declare (type simple-vector ,v))
                (values ,v (truly-the offset
                                      (+ (* n-word-bytes (%check-bound ,v (length ,v) ,index))
                                         ,(- (* n-word-bytes vector-data-offset) other-pointer-lowtag))))))))
      (symbol-global-value
         `(let ((symbol ,@args))
            (declare (type symbol symbol))
            (about-to-modify-symbol-value symbol 'compare-and-swap)
            (values symbol ,(- (* n-word-bytes symbol-value-slot)
                               other-pointer-lowtag))))
      (t
         (let ((dd (info :function :structure-accessor op)))
           (unless dd
             (error "Foo"))
           (let* ((structure (dd-name dd))
                  (slotd (find op (dd-slots dd) :key #'dsd-accessor-name))
                  (index (dsd-index slotd))
                  #+nil(type (dsd-type slotd)))
             (unless (eq t (dsd-raw-type slotd))
               (error "Cannot use COMPARE-AND-SWAP with structure accessor for a typed slot: ~S"
                      place))
             (when (dsd-read-only slotd)
               (error "Cannot use COMPARE-AND-SWAP with structure accessor for a read-only slot: ~S"
                      place))
             `(values (the ,structure ,@args) ,(- (* n-word-bytes (+ instance-slots-offset index))
                                                  instance-pointer-lowtag))))))))
(deftype offset ()
  `(signed-byte ,n-word-bits))

(defun compare-and-swap-locative (base offset old new)
  (declare (type offset offset))
  (compare-and-swap-locative base offset old new))

(defun contents (base offset)
  (declare (type offset offset))
  (contents base offset))

(defun %set-contents (value base offset)
  (declare (type offset offset))
  (%set-contents value base offset))

(declaim (inline (setf contents)))
(defun (setf contents) (value base offset)
  (%set-contents value base offset))
