;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;-----------------------------------------------------------------------
(defclass SCENE-DRAW (glut:window)
  ((renderbuffer :accessor renderbuffer)
   (framebuffer :accessor framebuffer)
   (hps :accessor hps)
   (sz  :accessor sz)
   (side :accessor side)
   (pixels :accessor pixels))
  (:default-initargs :width 800 :height 800
		     :title "Hyperbolic Tile View"
                     :mode '(:double :rgba)))

;;;------------------------------------------------------------------------
(defun ARC-POINTS-GL (hl)
  (with-slots (e-center e-a e-b swept-angle) hl
    (let ((pts (if e-center
		   (arc-points e-center e-a swept-angle 8)
		   (list e-a e-b))))
      (loop for p in pts
	   append (list (realpart p) (imagpart p) 0.0)))))
	   

;;;-----------------------------------------------------------------------
(defmethod glut:display-window :before ((sd SCENE-DRAW))  
  (with-slots (sz side framebuffer renderbuffer) sd
      (let ((fbuffer (first (gl:gen-framebuffers-ext  1)))
	    (rbuffer (first (gl:gen-renderbuffers-ext 1))))

	(setf side  512)
	(setf sz (* side side))

	(gl:bind-framebuffer-ext  :framebuffer-ext fbuffer)
	(gl:bind-renderbuffer-ext :renderbuffer-ext rbuffer)

	(gl:renderbuffer-storage-ext :renderbuffer-ext :rgb8 side side)
	
	(gl:framebuffer-renderbuffer-ext :framebuffer-ext
					 :color-attachment0-ext
					 :renderbuffer-ext
					 rbuffer)

	;;validate framebuffer
	(let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
	  (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
	    (error "Framebuffer not complete: ~A." framebuffer-status)))

	(setf renderbuffer rbuffer
	      framebuffer  fbuffer))

    (gl:clear-color 0 0 0 0)
    (gl:clear :color-buffer)
    (gl:shade-model :flat)))
  


;;;-----------------------------------------------------------------------
(defmethod glut:display ((sd SCENE-DRAW))    
  (with-slots (side framebuffer renderbuffer hps pixels) sd
    (gl:load-identity)
    (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)
    (gl:viewport 0 0 side side)
    ;; (gl:matrix-mode :projection)
    (gl:load-identity)
    ;; (glu:perspective 50 1 0.5 20)
    ;; (gl:matrix-mode :modelview)

    (gl:clear-color 0 0 0 0)
    (gl:clear :color-buffer)
    (gl:color 1 1 1)
    (gl:line-width 1)

    (print 'start-drawing-offscreen)
    (loop for hp in hps
       for points = (loop for hl across (h-lines hp)			  
		       append (arc-points-gl hl))
       for i from 0
       do   
	 (when (= (mod i 10000) 0)
	   (print i))
	 (gl:with-primitives :line-strip
	   (loop for (x y z) on points by #'cdddr
	      while x
	      do
		(gl:vertex x y z))))
    (gl:flush) 
    (print 'done-with-offscreen)
  
    (print 'reading-buffer)   
    (gl:read-buffer :color-attachment0-ext)
    (setf pixels 
	 (gl:read-pixels 0 0 side side
			 :rgba :UNSIGNED-INT-8-8-8-8-REV))

   (print 'start-writing-tga)
   (let ((displaced (make-array (list side side) :displaced-to pixels)))
     (imago:write-tga 
      (make-instance 'imago:rgb-image :pixels displaced) "~/hyper.tga"))
   (print 'end-writing-tga)

   (print 'start-drawing-on-screen)
   (gl:bind-framebuffer-ext :framebuffer-ext 0)
   (gl:load-identity) 
   (gl:viewport 0 0 800 800)
   (gl:clear :color-buffer)
   (gl:color 1 0.5 0) 
   (gl:line-width 2)
   (loop for hp in hps
      for points = (loop for hl across (h-lines hp)			  
		      append (arc-points-gl hl))
      for r = (+ 0.25 (random 0.75))
      for g = (+ 0.25 (random 0.75))
      for b = (+ 0.25 (random 0.75))
      do   
	(gl:with-primitives :line-strip
	  (gl:color r g b)
	  (loop for (x y z) on points by #'cdddr
	     while x
	     do
	       (gl:vertex x y z))))
  (gl:flush)
  (glut:swap-buffers)))

;;;-----------------------------------------------------------------------
(defmethod glut:reshape ((w scene-draw) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height))

;;;-----------------------------------------------------------------------
(defmethod glut:idle ((window scene-draw))
  ;; (declare (special mhp mhp hla hlb))
  ;; (setf (hps window) mhp)
  ;; (anim-1 mhp mhp hla hlb)
  ;; (glut:post-redisplay)
  ;; (sleep 1)
 )

;;;-----------------------------------------------------------------------
(defun rb (p q n)
  (let ((rbuf (make-instance 'scene-draw)))
    (setf (hps rbuf) (do-layers p q n))
    (glut:display-window rbuf)))

;;;-----------------------------------------------------------------------
(defun rb0 (hps)
  (let ((rbuf (make-instance 'scene-draw)))
    (setf (hps rbuf) hps)
    (glut:display-window rbuf)))

;;;-------------------------------------------------------------------------
;;(defmethod glut:close ((w scene-draw))
;;  (gl:free-gl-array (pixarray w)))
