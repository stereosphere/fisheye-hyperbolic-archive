;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

(defparameter *ISIZE* 800)
(defparameter *ISIZE/2* (/ *isize* 2))

;;;--------------------------------------------------------------------------------------O
(defun DRAW-LINES (ls)
  (loop for l across ls
     for points = (if (e-center l)
		      (arc-points (e-center l) (e-a l) (swept-angle l) 8)
		      (list (e-a l) (e-b l)))
     do  
       ;;(print points)
       (loop for (aa bb) on points 
	  for i from 0
	  while bb
	  do
	    (let* ((a (* *isize/2* aa))
		   (b (* *isize/2* bb))
		   (xa (+ *isize/2* (truncate (realpart a))))
		   (ya (+ *isize/2* (truncate (imagpart a))))
		   (xb (+ *isize/2* (truncate (realpart b))))
		   (yb (+ *isize/2* (truncate (imagpart b)))))
	      ;;(print (list xa ya xb yb))
	      (sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil)))))

;;;--------------------------------------------------------------------------------------
(defmethod DRAW-H-POLYS (hp-list &optional fnum)
  (when (not (consp hp-list))
    (setf hp-list (list hp-list)))
  (sdl:with-init ()
    (sdl:window *isize* *isize* :title-caption "Line Drawing" :icon-caption "Line Drawing")
    (setf (sdl:frame-rate) 5)
    (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))    
    (sdl:with-surface (surf sdl:*default-display*)
      (loop for hp in hp-list
	 do
	   (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	     (draw-lines (h-lines hp)))
	   (sdl:with-color (col (sdl:color :r 96 :g 96 :b 0))
	     (draw-lines (equi-lines hp))))
      (sdl:update-display)      
      (when fnum
	(sdl:save-image sdl:*default-display* (format nil "/home/michael/Frames/sdl-~4,'0d.bmp" fnum))))
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :SDL-KEY-ESCAPE)
			   (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))

;;;--------------------------------------------------------------------------------------O
(defun DRAW-POINT (c &optional big)
  (let* ((a (* *isize/2* c))
	 (b (* *isize/2* c))
	 (xa (+ *isize/2* (truncate (realpart a))))
	 (ya (+ *isize/2* (truncate (imagpart a))))
	 (xb (+ *isize/2* (truncate (realpart b))))
	 (yb (+ *isize/2* (truncate (imagpart b)))))
    (when big
      (sdl-gfx:draw-line-*  (1+ xa)  ya  (1- xb)  yb :aa nil)
      (sdl-gfx:draw-line-*  xa  (1+ ya)  xb  (1- yb) :aa nil))
      (sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil)))

;;;--------------------------------------------------------------------------------------
(defun DRAW-H-LINE (hl)
  (let ((points (if (and (e-center hl) (e-a hl)) 
		    (arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
		    (list (e-a hl) (e-b hl)))))
    (loop for (aa bb) on points 
       for i from 0
       while bb
       do
	 (let* ((a (* *isize/2* aa))
		(b (* *isize/2* bb))
		(xa (+ *isize/2* (truncate (realpart a))))
		(ya (+ *isize/2* (truncate (imagpart a))))
		(xb (+ *isize/2* (truncate (realpart b))))
		(yb (+ *isize/2* (truncate (imagpart b)))))
	   (sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil)))))

;;;--------------------------------------------------------------------------------------
(defun DRAW-CIRCLE (hl)
  (let ((r (e-radius hl))
	(c (e-center hl)))
    (draw-circ c r)))

;;;-------------------------------------------------------------------------------------
(defun DRAW-CIRC (c r)
  (let ((step (/ +pi+ 60))
	(two-pi (* 2.0 +pi+)))
  (destructuring-bind (cx cy) (dismember c)
    (loop for aa from 0.0 to two-pi by step
       for bb from step to (+ two-pi step) by step
       for ccx = (* *isize/2* cx)
       for ccy = (* *isize/2* cy)
       do
	    (let* ((cosa (* r *isize/2* (cos aa)))
		   (sina (* r *isize/2* (sin aa)))
		   (cosb (* r *isize/2* (cos bb)))
		   (sinb (* r *isize/2* (sin bb)))
		   (xa (+ *isize/2* (truncate (+ ccx cosa))))
		   (ya (+ *isize/2* (truncate (+ ccy sina))))
		   (xb (+ *isize/2* (truncate (+ ccx cosb))))
		   (yb (+ *isize/2* (truncate (+ ccy sinb)))))
	      ;;(print (list xa ya xb yb))
	      (if (and (< (abs xa) 30000)
		       (< (abs ya) 30000)
		       (< (abs xb) 30000)
		       (< (abs yb) 30000))
		  (sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil)))))))

;;;--------------------------------------------------------------------------------------
(defun DRAW-TEXT (text pos)
  (let ((x (+ *isize/2* (truncate (* *isize/2* (realpart pos)))))
	(y (+ *isize/2* (truncate (* *isize/2* (imagpart pos))))))
    (sdl:draw-string-solid-* text x y :font sdl:*default-font*)))
 

;;;--------------------------------------------------------------------------------------
(defun DRAW-CIRCLES% (hp-list)
  (loop for hp in hp-list
     for r = 100
     do
       (sdl:with-color (col (sdl:color :r r :g 255 :b 255))
	 (loop for hl across (h-lines hp)
	    do
	      (draw-circle hl)))
       (sdl:with-color (col (sdl:color :r 64 :g 64 :b 0))
	 (loop for el across (equi-lines hp)
	    do
	      (draw-circle el)))
       (sdl:with-color (col (sdl:color :r 255 :g 0 :b 0))
	 (draw-circ #c(0.0 0.0) 1.0))
       (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	 (loop for el across (equi-lines hp)
	    for i from 0
	    do
	      (draw-text (format nil "~d" i) (e-a el))
	    ;;(draw-text (format nil "~d" i) (e-b el))
	      (draw-point (e-a el) t)
	      (draw-point (e-b el) t)))))

;;;--------------------------------------------------------------------------------------
(defun DRAW-CIRCLES (hp-list &optional fnum)   
  (when (not (consp hp-list))
    (setf hp-list (list hp-list)))
  (sdl:with-init ()
    (setf sdl:*default-font* (sdl:initialise-font sdl:*font-10x20*))
    (sdl:window *isize* *isize* :title-caption "Line Drawing" :icon-caption "Line Drawing")
    (setf (sdl:frame-rate) 5)
    (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
    (sdl:with-surface (surf sdl:*default-display*)
      (loop for hp in hp-list
	 for r = 100
	 do
	
	   (sdl:with-color (col (sdl:color :r r :g 255 :b 255))
	     (loop for hl across (h-lines hp)
		do
		  (draw-circle hl)))
	   ;; (sdl:with-color (col (sdl:color :r 64 :g 64 :b 0))
	   ;;   (loop for el across (equi-lines hp)
	   ;; 	do
	   ;; 	  (draw-circle el)))
	   (sdl:with-color (col (sdl:color :r 255 :g 0 :b 0))
	     (draw-circ #c(0.0 0.0) 1.0))
	   (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	     (loop for el across (h-lines hp) ;;;;;(equi-lines hp)
		for i from 0
		do
		  (draw-text (format nil "~d" i) (e-a el))
		  (if (= i (first-edge hp))
		      (draw-text "x" (e-a el)))
		  ;;(draw-text (format nil "~d" i) (e-b el))
		  (draw-point (e-a el) t)
		  (draw-point (e-b el) t))))
      (sdl:update-display)  
      (when fnum
	(sdl:save-image sdl:*default-display* 
			(format nil "/home/michael/Frames/sdl0-~4,'0d.bmp" fnum))))
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :SDL-KEY-ESCAPE)
			   (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))

(defun DRAW-HP (hp r g b)
  (sdl:with-color (col (sdl:color :r r :g g :b b))
    (draw-lines (h-lines hp)))
 (sdl:with-color (col (sdl:color :r (truncate r 2) :g (truncate g 2) :b b))
    (draw-lines (equi-lines hp))))

    
;;;--------------------------------------------------------------------------------------
(setf fhp (make-fundamental-hp 6 4))
(setf rhp (make-reflected-hp-x fhp 0))
(setf rhp1 (make-reflected-hp-x rhp 1))

(defun DRAW-LINE-REFLECTION (hp index)
  (with-slots (h-lines equi-lines) hp
    (let ((el (aref equi-lines index))
	  (hl (aref h-lines index))
	  (rhp (make-reflected-hp hp index)))
      (sdl:with-init ()
	(setf sdl:*default-font* (sdl:initialise-font sdl:*font-10x20*))
	(sdl:window *isize* *isize* :title-caption "Line Drawing" :icon-caption "Line Drawing")
	(setf (sdl:frame-rate) 5)
	(sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
	(sdl:with-surface (surf sdl:*default-display*)

	  (draw-hp hp 255 255 255)

	  (draw-hp rhp 128 128 128)

	  (sdl:with-color (col (sdl:color :r 255 :g 0 :b 0))
	    (draw-circ #c(0.0 0.0) 1.0))
	  (sdl:with-color (col (sdl:color :r 0 :g 255 :b 255))
	    (draw-circle hl))
	  (sdl:with-color (col (sdl:color :r 255 :g 255 :b 0))
	    (draw-circle el))
	  (sdl:with-color (col (sdl:color :r 0 :g 0 :b 255))
	    (draw-h-line hl))
	  (sdl:with-color (col (sdl:color :r 0 :g 255 :b 255))
	    (draw-h-line el))

	  (reflect-equi-line hl el hl)	 
	  ;;(sdl:with-color (col (sdl:color :r 255 :g 255 :b 0))
	   ;; (draw-circle el))	 
	  (sdl:with-color (col (sdl:color :r 255 :g 255 :b 0))
	    (draw-h-line el))
	  (reflect-equi-line hl el hl);;put it back
	  (draw-hp hp 255 0 0)
	  (draw-hp rhp 0 255 0) 
	  (sdl:update-display))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event (:key key)
			   (if (sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event)))
	  (:video-expose-event () (sdl:update-display)))))))

;;;--------------------------------------------------------------------------------------
;;;avconv -r 10 -i ~/Frames/sdl-%04d.bmp -b:v 1000k test.mp4
(defmethod ANIM-SDL0 (&optional (start-frame 1))
  (let* ((p 6)
	 (q 4)
	 (n 4)
	 ;;(fhp (make-fundamental-hp p q))
	 (start-d (+ (* 0.005 (- start-frame 1)) 0.001)))
    (sdl:with-init () 
    (setf sdl:*default-font* (sdl:initialise-font sdl:*font-10x20*))
    (sdl:window *isize* *isize* :title-caption "Line Drawing" :icon-caption "Line Drawing")
    (setf (sdl:frame-rate) 5)
    (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))    
    (sdl:with-surface (surf sdl:*default-display*)
      (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	(loop for d from start-d to 0.881 by 0.005 ;;1.151 for y
	   for fnum from start-frame
	   do  	   
	     (print fnum)
	     (multiple-value-bind (hla hlb) 
		 (make-translating-h-lines #c(0.0 1.0) d)
	       (let* ((fl (first-layer-anim0 p q hla hlb))
		      (hps (do-layers-anim fl n))) 
		 (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))  
		 ;;(sdl:with-color (col (sdl:color :r 255 :g 0 :b 0))
		 ;;  (draw-text (format nil "~f" d) #c(0.1 0.1))) 

		 (loop for hp in hps ;; (cons hps)
		    do
		    ;;(draw-circles% (list (first hps)))
		      (when t
			(sdl:with-color (col (sdl:color :r 0 :g 96 :b 255))
			  (loop for hl across (h-lines hp)
			     for points = (if (e-center hl)
					      (arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
					      (list (e-a hl) (e-b hl)))
			     do  
			     ;;(draw-circle hl)
			       (loop for (aa bb) on points 
				  for i from 0
				  while bb
				  do
				    (let* ((a (* *isize/2* aa))
					   (b (* *isize/2* bb))
					   (xa (+ *isize/2* (truncate (realpart a))))
					   (ya (+ *isize/2* (truncate (imagpart a))))
					   (xb (+ *isize/2* (truncate (realpart b))))
					   (yb (+ *isize/2* (truncate (imagpart b)))))
				      (sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil))))))
		      
		      (when t
			(sdl:with-color (col (sdl:color :r 64 :g 64 :b 0))
			  (loop for hl across (equi-lines hp)
			     for points = (if (e-center hl)
					      (arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
					      (list (e-a hl) (e-b hl)))
			     do  
			     ;;(draw-circle hl)
			       (loop for (aa bb) on points 
				  for i from 0
				  while bb
				  do
				    (let* ((a (* *isize/2* aa))
					   (b (* *isize/2* bb))
					   (xa (+ *isize/2* (truncate (realpart a))))
					   (ya (+ *isize/2* (truncate (imagpart a))))
					   (xb (+ *isize/2* (truncate (realpart b))))
					   (yb (+ *isize/2* (truncate (imagpart b)))))
				      ;;(when (= (mod i 10000))
				      ;;  (sdl:update-display))
				      (sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil)))))))
		 
		 (sdl:update-display) 
		 ;;(break)     
		 (sdl:save-image sdl:*default-display* 
				 (format nil "/home/michael/Frames/sdlY-~4,'0d.bmp" fnum))
		 ))))
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
			 (if (sdl:key= key :SDL-KEY-ESCAPE)
			     (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display)))))))


#|
	      (if nil ;;*DEBUG-HL-TO-DRAW*
			  ;;(print *DEBUG-HL-TO-DRAW*)
			  (sdl:with-color (col (sdl:color :r 255 :g 96 :b 138))	
			    (loop for hl in *debug-hl-to-draw*
			       for points = (if (and (e-center hl) (e-a hl)) 
						(arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
						(list (e-a hl) (e-b hl)))
			       do  
				 (loop for (aa bb) on points 
				    for i from 0
				    while bb
				    do
				      (let* ((a (* *isize/2* aa))
					     (b (* *isize/2* bb))
					     (xa (+ *isize/2* (truncate (realpart a))))
					     (ya (+ *isize/2* (truncate (imagpart a))))
					     (xb (+ *isize/2* (truncate (realpart b))))
					     (yb (+ *isize/2* (truncate (imagpart b)))))
					
					(sdl-gfx:draw-line-*  xa  ya  xb  yb :aa nil)))
				 (when T
				   (format t "~&***~%in-draw ~d" (incf counter)) 
				   (describe hl)
				   (finish-output)
				   (sdl:update-display)
				   ;;(break)
				   ))))
		      (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
			(loop for el across (h-lines hp)
			   for i from 0
			   do
			     (draw-text (format nil "~d" i) (e-a el))
			   ;;(draw-text (format nil "~d" i) (e-b el))
			     (draw-point (e-a el) t)
			     (draw-point (e-b el) t)))
		      (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
			(loop for el across (equi-lines hp)
			   for i from 0
			   do
			     (draw-text (format nil "~d" i) (e-a el))
			   ;;(draw-text (format nil "~d" i) (e-b el))
			     (draw-point (e-a el) t)
			     (draw-point (e-b el) t))))
|#
