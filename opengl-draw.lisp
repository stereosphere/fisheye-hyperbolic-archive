;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;---------------------------------------------------------------------
(gl:define-gl-array-format pixelarray
  (gl:color :type :unsigned-char :components (r g b a)))

;;;-----------------------------------------------------------------------
(defclass RENDER-BUFFER (glut:window)
  ((renderbuffer :accessor renderbuffer)
   (framebuffer :accessor framebuffer)
   (pixelbuffer :accessor pixelbuffer)
   (pixarray :accessor pixarray)
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
(defmethod glut:display-window :before ((w RENDER-BUFFER))  

  (let ((framebuffer  (first (gl:gen-framebuffers-ext 1)))
	(renderbuffer (first (gl:gen-renderbuffers-ext 1))))
    (setf (side w) 1024)
    (setf (sz w) (* (side w) (side w)))
    (gl:bind-framebuffer-ext  :framebuffer-ext framebuffer)
    (gl:bind-renderbuffer-ext :renderbuffer-ext renderbuffer)


    (let ((pixelbuffer (first (cl-opengl-bindings:gen-buffers-ARB 1 (pixelbuffer w)))))
      (setf (pixarray w)
	    (gl:alloc-gl-array 'pixelarray (sz w)));;remember free
      (cl-opengl-bindings:bind-buffer-ARB :pixel-pack-buffer-ARB pixelbuffer)
      (cl-opengl-bindings:buffer-data-ARB :pixel-pack-buffer :dynamic-read (pixarray w))
      ;;(:bind-buffer :pixel-pack-buffer 0)
      (setf (pixelbuffer w) pixelbuffer))



    (gl:renderbuffer-storage-ext :renderbuffer-ext :rgb8 (side w) (side w))
    
    (gl:framebuffer-renderbuffer-ext :framebuffer-ext
				     :color-attachment0-ext
				     :renderbuffer-ext
				     renderbuffer)

    ;;validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
	(error "Framebuffer not complete: ~A." framebuffer-status)))

    (setf (renderbuffer w) renderbuffer
	  (framebuffer  w) framebuffer))

   (gl:clear-color 0 0 0 0)
   (gl:clear :color-buffer)
   (gl:shade-model :flat))
  


;;;-----------------------------------------------------------------------
(defmethod glut:display ((window RENDER-BUFFER))
  (gl:load-identity)


   (gl:bind-framebuffer-ext :framebuffer-ext (framebuffer window))
   (gl:viewport 0 0 (side window) (side window))
   ;; (gl:matrix-mode :projection)
   (gl:load-identity)
   ;; (glu:perspective 50 1 0.5 20)
   ;; (gl:matrix-mode :modelview)

   (gl:clear-color 0 0 0 0)
   (gl:clear :color-buffer)
   (gl:color 1 1 1)
   (print 'start-drawing-offscreen)
   (gl:line-width 1)

   (loop for hp in (hps window)
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
   (print 'done-with-offscreen)
   (gl:flush) 

   (print 'reading-pixels)


   (cl-opengl-bindings:bind-buffer-ARB :pixel-pack-buffer-ARB (pixelbuffer window))
   (cl-opengl-bindings:buffer-data-ARB :pixel-pack-buffer-ARB (* (sz window) 4) (pixarray window) :stream-read-ARB)
   ;;(cl-opengl-bindings:read-buffer-ARB :front)
   ;;(gl:bind-buffer :pixel-pack-buffer (pixelbuffer window))
   (gl:read-pixels 0 0 (side window) (side window)
   		   :rgba :UNSIGNED-INT-8-8-8-8)
;;   (let ((xx (cl-opengl-bindings:map-buffer-range :pixel-pack-buffer 
;;						  0 (sz window) :map-read-bit)))
;;     (print xx))
   (break)

#|
   (print 'reading-buffer)   
   (gl:read-buffer :color-attachment0-ext)
   
   (setf (pixels window) 
	 (gl:read-pixels 0 0 (side window) (side window)
			 :rgba :UNSIGNED-INT-8-8-8-8-REV))
|#
#|
   (print 'writing-sequence)
   (with-open-file (stream "~/hyper2.bin" 
			   :direction :output 
			   :element-type '(unsigned-byte 32)
			   :if-exists :supersede
			   :if-does-not-exist :create)
     (write-sequence (pixels window) stream))
   (print 'end-writing)
|#

   (print 'start-writing-tga)
   (let ((displaced (make-array (list (side window) (side window)) :displaced-to (pixels window))))
     (imago:write-tga 
      (make-instance 'imago:rgb-image :pixels displaced) "~/hyper.tga"))
   (print 'end-reading-tga)

   (print 'start-drawing-on-screen)
   (gl:bind-framebuffer-ext :framebuffer-ext 0)
   ;; (gl:viewport 0 0 (glut:width window) (glut:height window))
   ;; (gl:matrix-mode :projection)
   (gl:load-identity)
   ;; (glu:perspective 50 (/ (glut:width window) (glut:height window)) 0.5 20)
   ;; (gl:matrix-mode :modelview)
  
  (gl:viewport 0 0 800 800)
  (gl:clear :color-buffer)
  (gl:color 1 0.5 0) 
  (gl:line-width 2)
  (loop for hp in (hps window)
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
  (glut:swap-buffers)
  )

;;;-----------------------------------------------------------------------
(defmethod glut:reshape ((w render-buffer) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height))

;;;-----------------------------------------------------------------------
(defmethod glut:idle ((window render-buffer))
  ;; (declare (special mhp mhp hla hlb))
  ;; (setf (hps window) mhp)
  ;; (anim-1 mhp mhp hla hlb)
  ;; (glut:post-redisplay)
  ;; (sleep 1)
 )

;;;-----------------------------------------------------------------------
(defun rb (p q n)
  (let ((sd (make-instance 'scene-draw)))
    (setf (hps sd) (do-layers p q n))
    (glut:display-window sd)))

;;;-----------------------------------------------------------------------
(defun rb0 (hps)
  (let ((sd (make-instance 'scene-draw)))
    (setf (hps sd) hps)
    (glut:display-window sd)))

;;;-------------------------------------------------------------------------
;;(defmethod glut:close ((w render-buffer))
;;  (gl:free-gl-array (pixarray w)))
