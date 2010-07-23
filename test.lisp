(defstruct ts a b c)

(defun set-ts-a (place value)
  (sb-ext:compare-and-swap place place value))

(defsetf ts-a set-ts-a)

