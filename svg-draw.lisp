
;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;--------------------------------------------------------------------
(defun ARC-POINTS-SVG (hl)
  (with-slots (e-center e-a e-b swept-angle) hl
    (let ((pts (when e-center
		 (arc-points e-center e-a swept-angle 8))))
      (unless pts
	(multiple-value-bind (a b)
	    (line-circle #c(0.0 0.0) 1.0 e-a (- e-a e-b))
	  (setf pts (line-points a b 3))))
      (loop for p in pts
	 append (list (realpart p) (imagpart p))))))
	   

	  
;;     (list (realpart res) (imagpart res))))
;;  (list (realpart ez) (imagpart ez) on-sphere)))) 
 

;;;-------------------------------------------------------------------------
(defmethod GET-POLYGON-POINTS (hp)
 (let* ((points (loop for el across (equi-lines hp)
		  append (arc-points-svg el)))
	(coords (loop for (x y) on points by #'cddr
		   append (list (+ x 1.0)
				(+ y 1.0)))))
   coords))
   
;;-------------------------------------------------------------------------
(defun TO-EQUI-AZ1 (z)
  (let* ((on-dome (stereographic-project z))
	 (equi-az (from-dome-to-equi-az on-dome)))
    equi-az))
	 

;;;-------------------------------------------------------------------------
(defmethod GET-POLYGON-EQUI-AZ-POINTS (hp)
  (let* ((points (loop for el across (equi-lines hp)
		   append (arc-points-svg el)))
	 (coords (loop for (x y) on points by #'cddr
		    append (list x y)))
	 (ea-points (loop for (x y) on coords by #'cddr
		       for z = (to-equi-az1 (complex x y))
		       append (list (+ (realpart z) 1.0) 
				    (+ (imagpart z) 1.0)))))
    ea-points))

;;;-------------------------------------------------------------------------
;;; works on equi-line or h-line
(defun GET-H-LINE-POINTS (hl)
   (let* ((points (arc-points-svg hl))
	  (coords (loop for (x y) on points by #'cddr
		     append (list x y)))
	  (hl-points (loop for (x y) on coords by #'cddr
			for z = (to-equi-az1 (complex x y))
			append (list (+ (realpart z) 1.0) 
				     (+ (imagpart z) 1.0)))))
    hl-points))

;;;-------------------------------------------------------------------------
(defmethod SVG-HYPERBOLIC-TILING (stream hp-list)
  (let* ((cw-style (svg-color 96 96 96))
	 (ccw-style (svg-color 128 0 0)))
    (svg stream
      (background stream 255 255 255)
      (loop for hp in hp-list
	 ;;for line-coords = (get-h-line-points (get-line hp (first-edge hp)))
	 for coords = (get-polygon-points hp)
	 ;;for coords = (get-polygon-equi-az-points hp) 
	 ;;for center = (to-equi-az (center hp))
	 for style-property = (getf (properties hp) :style)
	 for sty = (cond (style-property
			  style-property)
			 ((cw hp) 
			  cw-style)
			 ((ccw hp)
			  ccw-style))
	 do
	   (svg-path stream coords sty)
	   
	   ;;(polyline stream line-coords :r 255)
	   ;; (svg-text stream 
	   ;; 	     (+ (realpart center) 1.0)
	   ;; 	     (+ (imagpart center) 1.0)
	   ;; 	     (serial-number hp))
		     
	   ;; (loop for pt in (get-points hp)
	   ;;    for ea = (to-equi-az1 pt)
	   ;;    for x = (+ 1.0 (realpart ea))
	   ;;    for y = (+ 1.0 (imagpart ea))
	   ;;    for i from 0
	   ;;    do
	   ;; 	(svg-text stream x y (format nil "~d" i)))
	   ))))
;;	 (fhp (make-fundamental-hp p q)))





;;;-------------------------------------------------------------------------------------
(defun ANIM-SVG (p q n &optional (start-frame 1))
  (let* ((root-name (format nil "f_~d~d~d" p q n))
	 (avconv-filename (format nil "~a_%04d.png" root-name))
	 (step 0.01)
	 (start-d (+ (* step (- start-frame 1)) 0.001)))
    start-d
    (loop for d from -2.005 to 2.005 by step ;; by 0.01 ;;1.151 for y
       for fnum from start-frame
       ;;repeat 2
       do  
	 (when t ;;(= (mod fnum 1) 0)
	   (format t "~%~%")
	   (cl-user::gc :full t)
	   (room nil)
	   ;;(cl-user::gc :full t) 
	   ;;(format t "~%~%")
	   ;;(room nil)
	   (format t "~%~%"))
	 (format t "~%~%doing frame ~d d=~5,3f ..." fnum d)
	 (multiple-value-bind (hla hlb) 
	     (make-translating-h-lines (complex 0.0 0.101) d) ;;(complex (/ (sqrt 2.0) 2.0) (/ (sqrt 2.0) 2.0)) d)
	   (let* ((fl (first-layer-anim0 p q hla hlb))
		  (hps (do-layers-anim fl n))
		  (path (format nil "/home/michael/SVG-FRAMES/Anim0/~a_~4,'0d.svg" root-name fnum)))
	     (format t " calculated ")
	     (with-open-file (stream path
				     :direction :output
				     :if-exists :supersede)
	       (svg-hyperbolic-tiling stream hps))))
	 (format t " done~%~%"))
    (sb-posix:chdir #p"/home/michael/SVG-FRAMES/Anim0")
    (convert-to-png root-name)
    (sb-ext:run-program "/usr/bin/avconv" (list "-r" "30"
						"-i" avconv-filename 
						"-b:v" "3000k" 
						(concatenate 'string "../" root-name ".avi")))))
 





;;;-------------------------------------------------------------------------         
(defun FILE (hp-list)
  (unless (consp hp-list)
    (list hp-list))
  (let* ((name (cond ((= (p (first hp-list)) 4)
		      "test46")
		     ((= (p (first hp-list)) 6)
		      "test64")
		     (t
		      "test")))
	(path (format nil 
		      "/home/michael/SVG-FRAMES/~a.svg" name)))
    (format t "~&writing svg file")
    (with-open-file (out path
			 :direction :output 
			 :if-exists :supersede 
			 :if-does-not-exist :create)
      (svg-hyperbolic-tiling out hp-list)))) 

;;;-------------------------------------------------------------------------
(defmethod SVG-DRAW-POINT-LISTS (stream point-lists)
  (let* (;;(cw-style (svg-color 96 96 96))
	 (ccw-style (svg-color 128 0 0)))
    (svg stream
      (background stream 255 255 255)
      (loop for pt-list in point-lists
	 for coords = (loop for pt in pt-list
			   append (list (realpart pt) (imagpart pt)))
	 do
	   (svg-path stream coords ccw-style))
      (dome-matte stream))))

;;;-------------------------------------------------------------------------
;; (defun SVG-DRAW-MULTI-POINT-LISTS+ (stream &rest style-point-lists)
;;   (svg stream
;;     (background stream 0 0 0) ;;255 255 255)
;;     (loop for (style pt-list) in style-point-lists
;;        for coords = (loop for pt in pt-list
;; 		       append (list (realpart pt) (imagpart pt)))
;;        do
;; 	 ;;(print (list coords style))
;; 	 (svg-path stream coords style))
;;     (when (and hla hlb)
;;       (let ((a-coords (get-h-line-points hla))
;; 	    (b-coords (get-h-line-points hlb)))
;; 	(polyline stream a-coords :g 128)
;; 	(polyline stream b-coords :b 128)))
;;     (dome-matte stream)))


;;;-------------------------------------------------------------------------
(defun SVG-DRAW-POINT-LISTS+ (stream style-point-lists &optional hla hlb)
  (svg stream
    (background stream 0 0 0) ;;255 255 255)
    (loop for (style pt-list) in style-point-lists
       for coords = (loop for pt in pt-list
		       append (list (realpart pt) (imagpart pt)))
       do
	 ;;(print (list coords style))
	 (svg-path stream coords style))
    (when (and hla hlb)
      (let ((a-coords (get-h-line-points hla))
	    (b-coords (get-h-line-points hlb)))
	(polyline stream a-coords :g 128)
	(polyline stream b-coords :b 128)))
    (dome-matte stream)))



;;;-------------------------------------------------------------------------
;;; abandon this
(defun SVG-DRAW-POINT-LISTS+old (stream point-lists style-list &optional hla hlb)
  (svg stream
    (background stream 0 0 0) ;;255 255 255)
    (loop for pt-list in point-lists
       for style in style-list
       for coords = (loop for pt in pt-list
		       append (list (realpart pt) (imagpart pt)))
       do
	 ;;(print (list coords style))
	 (svg-path stream coords style))
    (when (and hla hlb)
      (let ((a-coords (get-h-line-points hla))
	    (b-coords (get-h-line-points hlb)))
	(polyline stream a-coords :g 128)
	(polyline stream b-coords :b 128)))
    (dome-matte stream)))


;;;-------------------------------------------------------------------------         
(defun FILE-3D (point-lists)
  (let ((path (format nil "/home/michael/SVG-FRAMES/3d-test.svg")))
    (format t "~&writing svg file")
    (with-open-file (out path
			 :direction :output 
			 :if-exists :supersede 
			 :if-does-not-exist :create)
      (svg-draw-point-lists out point-lists))))
