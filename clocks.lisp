;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)
 

;;;----------------------------------------------------------------------------------
(defun FIND-E-RADIUS-OF-INSCRIBED-CIRCLE (centered-hp)
  (let* ((hl        (aref (h-lines centered-hp) 0))
	 (hl-center (/ (+ (e-a hl) (e-b hl)) 2.0)) ;;any h-line will do
	 (dir (/ hl-center (abs hl-center))) ;;normalize center to make dir
	 (p-intersect      (line-circle (e-center hl) (e-radius hl) #c(0.0 0.0) dir)))
    ;;distance from origin to intersection is the Euclidean radius
    (abs p-intersect)))


;;;-------------------------------------------------------------------------------
;;TRASH?
(defun HYPERBOLIC-CENTER-AND-RADIUS (e-center e-radius)
  (destructuring-bind (a b) (dismember e-center)
    (let ((h-center (complex a (- (* b b) (* e-radius e-radius))))
	  (h-radius (* 0.1 e-radius))) ;;later(* b (tanh 
      (values h-center h-radius))))

;;;-------------------------------------------------------------------------
(defmethod GET-CLOCK-STYLE-POINTS-DOMEx (hp &optional (scale 1.0) use-h-center)
 (let* (;;(center-dist (abs (center hp)))
	(color (list 192 192 192)));;(rainbow-color center-dist)))
   (destructuring-bind (a b c)
       (coerce (circ-pts hp) 'list)
     (multiple-value-bind (center radius)
	 (circle-through-3-points a b c)
       ;;(PRINT (LIST CENTER (center hp)))
       (multiple-value-bind (h-center h-radius) (h-circle-center center radius 0.5)
	 (declare (ignorable h-center h-radius)) 
	 (destructuring-bind (r g b) color
	   ;;(print (list (abs (center hp)) (center hp) r g b))
	   (let* ((style (svg-color r g b))
		  (step (/ pi (max 12
				   (truncate (* 72.0 radius)))))
		  (c-center (if use-h-center h-center center))
		  (c-radius (if use-h-center h-radius radius))
		  (pts (loop for theta from 0.0 to (* 2.0 pi) by step
			  for p = (+ c-center (* scale c-radius (cis theta))) ;; or (center hp)
			  collect p)))
	     (list style pts))))))))

;;--------------------------------------------------------------------------------
;;top-mat and bot-mat rotate the horizon, y-refl mirrors the top pts
(defun PROJECT-CLOCKS (hps x-rot z-rot)
  (let* ((top-mat (make-x-rotate (- x-rot)))
	 (bot-mat (make-x-rotate (- (+ x-rot pi))))
	 (z-rotater (cis z-rot))
	 (y-refl  (make-y-reflect))
	 (sty-pts (loop for hp in hps 
		     for style-pts = (get-clock-style-points-domex hp)
		     for style = (first style-pts)
		     for zpts = (loop for p in (second style-pts) collect (* z-rotater p))
		     for inner-circle-style-pts = (get-clock-style-points-domex hp 1.0 t)
		     for inner-circle-zpts = (loop for p in (second inner-circle-style-pts) collect (* z-rotater p))
		     ;;collect the top pts
		     collect (list style (loop for z in zpts 
					    for coords = (stereographic-project z)
					    for rot-coords = (v3:m* top-mat coords)
					    for on-plane = (from-dome-to-equi-az-v3 rot-coords)
					    ;;for on-plane =  (unstereographic rot-coords)
					    collect
					      (+ #c(1.0 1.0) (* (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*))))
		     into top-pts
		      
		     collect (list (svg-color 0 0 0) 
				   (loop for z in inner-circle-zpts 
				      for coords = (stereographic-project z)
				      for rot-coords = (v3:m* top-mat coords)
				      for on-plane = (from-dome-to-equi-az-v3 rot-coords) 
				      ;;for on-plane =  (unstereographic rot-coords)
				      collect
					(+ #c(1.0 1.0) (* (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*))))
		     into inner-top-pts
		      
		     ;;collect the bottom pts
		     when (/= x-rot 0.0)
		     collect (list style (loop for z in zpts 
					    for coords = (stereographic-project z)
					    for yrefl-coords  = (v3:m* y-refl coords)
					    for xyrefl-coords = (v3:m* bot-mat yrefl-coords)
					    ;;for on-plane      = (unstereographic xyrefl-coords)
					    for on-plane      = (from-dome-to-equi-az-v3 xyrefl-coords)
					    collect
					      (+ #c(1.0 1.0) (* (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*))))
		     into bot-pts
		     finally (return (append top-pts inner-top-pts bot-pts)))))
    ;;cull pts outside circle
    (setf sty-pts (remove-if 
		   (lambda (sty-pt-list) 
		     (every (lambda (z) 
			      (> (abs (- z #c(1.0 1.0))) 1.0)) 
			    (second sty-pt-list)))
		   sty-pts))
    ;;return pts
    sty-pts))



;;;----------------------------------------------------------------------------------
(defun CLOCK-DOME (p q n &optional (name "a") (start-frame 1))
  (let* ((root-name (format nil "~a_~d~d~d" name p q n))
	 (avconv-filename (format nil "/home/michael/SVG-FRAMES/Anim0/~a_%04d.png" root-name))
	 (step 0.011);;0.005)
	 (start-d (+ (* step (- start-frame 1)) 0.001)))
    start-d

    (loop for d from -2.01 to 2.0 by step ;;-2.01
    ;;(loop for d from 0.0 to (* 2.0 pi) by (/ pi 360.0)
       for fnum from start-frame
       ;;repeat 1
       do  
	 (when (= (mod fnum 3) 0)
	   ;;(format t "~%~%")
	   (cl-user::gc :full t)
	   (room-report)
	   ;;(cl-user::gc :full t) 
	   ;;(format t "~%~%")
	   ;;(room nil)
	   (format t "~%~%"))
	 (format t "~%~%doing frame ~d d=~5,3f ..." fnum d)
	 (multiple-value-bind (hla hlb) 
	     ;;(make-rotating-h-lines (complex 0.0 0.2) (- d))
	     (if (> (abs d) 1.0e-6)
		 (make-translating-h-lines (complex 1.0 0.0) (- d))
		 (values (make-h-line #c(0.0 0.0) #c(1.0 0.0))
			 (make-h-line #c(0.0 0.0) #c(1.0 0.0))))
	   (let* ((fl (first-layer-anim0 p q hla hlb))
		  ;;(fl (first-layer-anim-center p q hla hlb));;doesn't work
		  (hps (do-layers-anim fl n))
		  ;;(point-lists (project-to-dome hps (make-x-rotate 0.0))) ;; no x rotate
		  (style-point-lists (project-clocks hps 0.0 0.0));;(- (/ pi 4.0)) 0.0));;(- (/ pi 4.0))))
		  (path (format nil "/home/michael/SVG-FRAMES/Anim0/~a_~4,'0d.svg" root-name fnum)))
	     (format t " calculated ")
	     (with-open-file (stream path
				     :direction :output
				     :if-exists :supersede)
	       (svg-draw-point-lists+ stream style-point-lists nil nil)))) ;;hla hlb))))
	 (format t " done~%~%"))
    (sb-posix:chdir #p"/home/michael/SVG-FRAMES/Anim0")
    (convert-to-png root-name) 
    (print (concatenate 'string "/usr/bin/avconv" 
			"-r" "30"
			"-i" avconv-filename 
			"-b:v" "3000k" 
			(concatenate 'string "../" root-name ".avi")))
    (when (probe-file (format nil "/home/michael/SVG-FRAMES/~a.avi" root-name))
      (print 'deleting-old-avi)
      (delete-file (format nil "/home/michael/SVG-FRAMES/~a.avi" root-name))) 
   (sb-ext:run-program "/usr/bin/avconv" 
			(list "-r" "30"
			      "-i" avconv-filename 
			      "-b:v" "3000k" 
			      (concatenate 'string "../" root-name ".avi")))))

