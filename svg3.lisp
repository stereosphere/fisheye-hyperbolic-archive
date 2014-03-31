;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-


(in-package :hyperbolic)

;; the size should be the maximum contemplated. The bitmap output from 
;;inkscape can be scaled down to the desired size without compromising
;;quality
(defparameter *ssize* 4096)
(defparameter *ssize/2* (/ *ssize* 2))

;;;-------------------------------------------------------------------------
(defun PRINT-TAG (stream name attrs)
  (format stream "<~a" (string-downcase name))
  (loop for (attr-name attr-val) on attrs by #'cddr
      for name = (string-downcase attr-name)
      for val  = (unless (eql attr-name 'path)
                   (if (or (stringp attr-val) (symbolp attr-val))
                     (string-downcase attr-val))
                   ;;else
                   attr-val)
      do
        (format stream " ~a=\"~a\"~%" name val))
  nil)

;;;-------------------------------------------------------------------------
(defun PRINT-TAG-CLOSE (stream name)
  (format stream "</~a>~%" (string-downcase name)))

;;;-------------------------------------------------------------------------
(defmacro TAG (stream name attrs &body body)
  (cond (body
         `(progn (print-tag ,stream ',name
                            (list ,@(loop for (a b) on attrs by #'cddr
                                 append `(',a ,b))))

                 (format ,stream ">~%")
                 ,@body
                 (format ,stream "</~a>~%" (string-downcase ',name))))
        (t
         `(progn (print-tag ,stream ',name
                            (list ,@(loop for (a b) on attrs by #'cddr
                                        append `(',a ,b))))
                 (format ,stream "/>~%")))))

;;;-------------------------------------------------------------------------
;;;returns the attribute string for black filled paths
(defun STYLE-1 ()    
   "fill:#000000;fill-rule:evenodd")

;;;--------------------------------------------------------------------------
;;;header copied from inkscape plain svg
(defmacro SVG (stream &body body)
  `(progn
     (format ,stream "<?xml version=\"1.0\" standalone=\"no\"?>~%")
     (format ,stream "<!-- Created by Michael Fleming -->~%")
     (tag ,stream svg 
          ("xmlns:dc"  "http://purl.org/dc/elements/1.1/"
           "xmlns:cc"  "http://creativecommons.org/ns#"
           "xmlns:rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
           "xmlns:svg" "http://www.w3.org/2000/svg"
           "xmlns"     "http://www.w3.org/2000/svg"
            version     "1.1"
            width       (format nil "~dpx" *ssize*);;"3200px" ;;"800"
            height      (format nil "~dpx" *ssize*);;"3200px" ;;"800"
            id          "svg2")
          ,@body)))


;;;-------------------------------------------------------------------------
(defun BRIGHTNESS (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))

;;;---------------------------------------------------------------------
(defun SVG-COLOR (r g b)
  (format nil "fill:rgb(~a,~a,~a)" r g b))


;;--------------------------------------------------------------------------
;;(defun GRADIENT (stream id stop-info-list fx fy r)
;;)


;;--------------------------------------------------------------------------
;; (defun DEFS (stream def-list)
;;   (tag stream defs (gradient (loop for (attr val) on def-list by #'cddr
;; 				do
;; 				  (tag stream aaa ("test" attr val))))))

	     


;;;-------------------------------------------------------------------------
;;(format nil "~{~&~a ~a ~a~}" '(m 1 2 l 3 4 l 5 6 l 7 8))
;;(loop with  x = 'm repeat 5 do (print x) (setf x 'l))
(defun POLYGON (stream point-list &key (r 0) (g 0) (b 0))
  (let ((sty (format nil "fill:rgb(~a, ~a, ~a)" r g b)))
    (tag stream polygon (points  (format nil "~{~a,~a ~}" point-list)         
				 style sty))))
;;;-----------------------------------------------------------------------------
;;; 
(defun POLYLINE (stream point-list &key (r 0) (g 0) (b 0))
  (let* ((str (make-array '(0) :element-type 'base-char
			  :fill-pointer 0 :adjustable t))
	 (style (princ-to-string (format nil "stroke:rgb(~d,~d,~d); fill:none; stroke-width:8" r g b))))
    (with-output-to-string (s str)    
      (loop for (xx yy eol) on point-list by #'cddr
	 for x = (* *ssize/2* xx)
	 for y = (* *ssize/2* yy)              
	 do
	   (format s "~&~8,1f ~8,1f" x y)
	   (when eol
	     (format s ","))))    
    (tag stream polyline (points str style style))
    (setf str nil)))

;;;-----------------------------------------------------------------------------
(defun BACKGROUND (stream r g b)
  (let* ((s (princ-to-string *ssize*))
	 (style (princ-to-string (format nil "fill:rgb(~d,~d,~d)" r g b))))
    (tag stream rect (width s height s style style))))


;;;------------------------------------------------------------------------------
(defun SVG-PATH (stream point-list style)
  (let* ((str (make-array '(0) :element-type 'base-char
			  :fill-pointer 0 :adjustable t))) 
    (with-output-to-string (s str)    
      (loop for (xx yy) on point-list by #'cddr
	 for c = "M" then "L"
	 for x = (* *ssize/2* xx)
	 for y = (* *ssize/2* yy)              
	 do
	   (format s "~a ~8,1f ~8,1f~%" c x y)))
    (tag stream path (d str style style))
    (setf str nil))) 


;;;-----------------------------------------------------------------------
(defun SVG-TEXT (stream xx yy string)
  (let ((x (* *ssize/2* xx))
	(y (* *ssize/2* yy))
	(sty "fill:rgb(255, 0, 0);font-size 24px"))
    (tag stream text (x x y y style sty) (format stream "~a" string))))

;;;-----------------------------------------------------------------------
(defun DOME-MATTE (stream)
  (let ((str (make-array '(0) :element-type 'base-char
			  :fill-pointer 0 :adjustable t))
	(sty (svg-color 64 64 64))
	(circle-points (append (loop for angle from 0.0 to (/ pi 2.0) by (/ pi 72.0)
				  collect 
				    (list (cos angle) (sin angle)))
			       (list (list 0.0 1.0)))))
    (loop for (xsign ysign) in '((1.0 1.0) (-1.0 1.0) (-1.0 -1.0) (1.0 -1.0))
       for cornerx = (* (+ 1.0 xsign) *ssize/2*)
       for cornery = (* (+ 1.0 ysign) *ssize/2*)
       do
	 (adjust-array str '(0) :element-type 'base-char :fill-pointer 0) 
	 (with-output-to-string (s str)  
	   (format s "M ~8,1f ~8,1f~%" cornerx cornery)

	   (loop for (xx yy) in circle-points
	      for x = (* *ssize/2* (+ 1.0 (* xsign xx)))
	      for y = (* *ssize/2* (+ 1.0 (* ysign yy)))
	      do
		(format s "L ~8,1f ~8,1f~%" x y))
	   ;;(format s "L ~8,1f ~8,1f~%" (/ cornerx 2.0) cornery)
	   (format s "L ~8,1f ~8,1f~%" cornerx cornery)
	   (tag stream path (d str style sty))))))
