(defpackage #:hyperbolic
  (:use #:cl))

#+clisp(SETF (EXT:LONG-FLOAT-DIGITS) 80)
;;;long-float is double-float in sbcl
(setf *read-default-float-format* 'long-float)

