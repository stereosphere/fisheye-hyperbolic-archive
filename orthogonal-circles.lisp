;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)
 
;;;http://math.stackexchange.com/questions/603872/find-a-circle-orthogonal-to-two-other-circles

;;;finds the e-center of the given h-circle and returns the e-coords of the h-center
;;;and the ...
(defun H-CIRCLE-CENTER (e-center e-radius h-radius)
  (let* ((dir (/ e-center (abs e-center))))
    (multiple-value-bind (i0 i1) (line-circle e-center e-radius #c(0.0 0.0) dir)
      (let* ((h-i0 (e-h i0))
	     (h-i1 (e-h i1))
	     (h-center (/ (+ h-i0 h-i1) 2.0))
	     (h-point-on-circle-a (+ h-center (* dir h-radius)))
	     (h-point-on-circle-b (- h-center (* dir h-radius)))
	     (e-a (h-e h-point-on-circle-a))
	     (e-b (h-e h-point-on-circle-b))
	     (center (/ (+ e-a e-b) 2.0))
	     (radius (abs (/ (- e-a e-b) 2.0))))
	(values center radius)))))
	

