;;; -*- Mode: Lisp; Syntax: CLtL; Package: CL-USER; lowercase: yes; -*-

(DEFPACKAGE #:VECTOR3
  (:use #:cl)
  (:nicknames "V3")
  (:shadow :+ :- :/ :*)
  (:export :make
	   :with-v3s
	   :from-complex
	   :vx :vy :vz
	   :incv3 :decv3
	   :dot
	   :normalize :norm
	   :+ :- :* :/
	   :copy :v3
	   :m*))

#+CLISP(SETF (EXT:LONG-FLOAT-DIGITS) 80)
;;;LONG-FLOAT IS DOUBLE-FLOAT IN SBCL
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)

(IN-PACKAGE :VECTOR3)


(deftype V3 () '(simple-array double-float (3)))
;;think of 3x3 matrix as 3 v3 row vectors
(deftype M3x3 () '(simple-array v3 (3)))

(defmacro D! (d)
  `(the double-float ,d))

(defmacro I! (i)
  `(the fixnum ,i))

(defmacro V3! (v)
  `(the (simple-array double-float (3)) ,v))


(defmacro VX (v) 
  `(the double-float (aref (v3! ,v) (i! 0))))

(defmacro VY (v)
  `(the double-float (aref (v3! ,v) (i! 1))))

(defmacro VZ (v)
  `(the double-float (aref (v3! ,v) (i! 2))))

;;(locally
;;    (declare (optimize speed (safety 0) (debug )))
(defun MAKE (&optional (x 0.0d0) (y 0.0d0) (z 0.0d0))
  (let ((v (make-array 3 :element-type 'double-float)))
    (declare (type v3 v)
	     ;;(dynamic-extent v)
	     (double-float x y z))
    (setf (vx v) x)
    (setf (vy v) y)
    (setf (vz v) z)
    v))
;;)

(defun COPY (v0 &optional (into (make)))
  "Copy v0 either into a supplied v3 or a new v3"
  (setf (vx into) (vx v0))
  (setf (vy into) (vy v0))
  (setf (vz into) (vz v0))
  into)

(defmacro WITH-V3S (vectors &body body)
  `(let ,(loop for v in vectors
            collect (list v '(make-array 3 :element-type 'double-float)))
     ,@(loop for v in vectors
	  collect 
	    `(declare (type v3 ,v)))  ;;(dynamic-extent ,v)))
     ,@body))


  
(defun FROM-COMPLEX (z)
  (let ((v (make-array 3 
		       :element-type 'double-float 
		       :initial-contents (list (realpart z) (imagpart z) 0.0d0))))
    (declare (type v3 v) 
	     ;;(dynamic-extent v)
             (type (complex double-float) z))
    v))


(defmacro SETV (v x y z)
  (declare (type v3 v)
	   (double-float x y z))
  `(progn (setf (aref (v3! ,v) (i! 0)) (d! ,x))
          (setf (aref (v3! ,v) (i! 1)) (d! ,y))
          (setf (aref (v3! ,v) (i! 2)) (d! ,z))
          ,v))



(defmacro D+ (a b)
  `(the double-float (cl:+ (d! ,a) (d! ,b))))

(defmacro D- (a b)
  `(the double-float (cl:- (d! ,a) (d! ,b))))

(defmacro D* (a b)
  `(the double-float (cl:* (d! ,a) (d! ,b))))

(defmacro D/ (a b)
  `(the double-float (cl:/ (d! ,a) (d! ,b))))



(defun + (v0 v1 &optional (into (make)))
  (declare (type v3 v0 v1))
  (setf (vx into) (d+ (vx v0) (vx v1)))
  (setf (vy into) (d+ (vy v0) (vy v1)))
  (setf (vz into) (d+ (vz v0) (vz v1)))
  into)

(defun - (v0 v1 &optional (into (make)))  
  (declare (type (simple-array double-float) v0 v1 into))
  (setf (vx into) (d- (vx v0) (vx v1)))
  (setf (vy into) (d- (vy v0) (vy v1)))
  (setf (vz into) (d- (vz v0) (vz v1)))
  into)

(defun / (v0 s &optional (into (make)))
  "scalar division"
  (declare (type (simple-array double-float) v0)
           (double-float s))
  ;;(setv into (d/ (vx v0) s)
  ;;           (d/ (vy v0) s)
  ;;	     (d/ (vz v0) s))
  (setf (vx into) (d/ (vx v0) s))
  (setf (vy into) (d/ (vy v0) s))
  (setf (vz into) (d/ (vz v0) s))
  into)

(defun * (v0 s &optional (into (make)))
  "scalar multiplication"
  (declare (type v3 v0)
           (type double-float s))
  (setf (vx into) (d* (vx v0) s))
  (setf (vy into) (d* (vy v0) s))
  (setf (vz into) (d* (vz v0) s))
  into)

(defun DOT (v0 v1)
  (declare (type v3 v0 v1))
  (the double-float 
    (cl:+ (d* (vx v0) (vx v1))
	  (d* (vy v0) (vy v1))
	  (d* (vz v0) (vz v1)))))

(defun CROSS (v0 v1 &optional (into (make)))  
  (declare (type v3 v0 v1 into))
  (setf (vx into) (d- (d* (vy v0) (vz v1)) (d* (vz v0) (vy v1))))
  (setf (vy into) (d- (d* (vz v0) (vx v1)) (d* (vx v0) (vz v1))))
  (setf (vz into) (d- (d* (vx v0) (vy v1)) (d* (vy v0) (vx v1))))
  into)

(defun NORM (v) 
  (declare (type v3 v))
  (the double-float (sqrt (the double-float (dot v v)))))

(defun NORMALIZE (v &optional scale)
  (declare (type v3 v))
  (if scale
      (* (/ v (norm v)) scale)
      (/ v (norm v))))

(defun M* (m v &optional (into (make)))
  (declare (type v3 v)
           (type m3x3 m))
  (setf (vx into) (dot v (aref m 0)))
  (setf (vy into) (dot v (aref m 1)))
  (setf (vz into) (dot v (aref m 2)))
  into) 
    
