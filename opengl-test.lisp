;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;-----------------------------------------------------------------------
(defclass RENDER-BUFFER (glut:window)
  ((renderbuffer :accessor renderbuffer)
   (framebuffer :accessor framebuffer))
  (:default-initargs :width 800 :height 800 
		     :title "Hyperbolic Tile View"
                     :mode '(:single :rgb)))


;;;-----------------------------------------------------------------------
(defmethod glut:display-window :before ((w RENDER-BUFFER))  
#|
  (let ((framebuffer  (first (gl:gen-framebuffers-ext 1)))
	(renderbuffer (first (gl:gen-renderbuffers-ext 1))))

    (gl:bind-framebuffer-ext  :framebuffer-ext framebuffer)
    (gl:bind-renderbuffer-ext :renderbuffer-ext renderbuffer)

    (gl:renderbuffer-storage-ext :renderbuffer-ext :rgba8 512 512)
    
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
|#
    (gl:clear-color 0 0 0 0)
    (gl:shade-model :flat))
  
;;;----------------------------------------------------------------------- 
(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

;;;-----------------------------------------------------------------------
(defmethod glut:display ((window RENDER-BUFFER))
  (gl:load-identity)

  ;; We render the teapot in the first pass. To do this, we switch to our
  ;; custom framebuffer, set the viewport to the texture size and render it
  ;; normally.
   ;; (gl:bind-framebuffer-ext :framebuffer-ext (framebuffer window))
   ;; (gl:viewport 0 0 512 512)
   ;; (gl:matrix-mode :projection)
   ;; (gl:load-identity)
   ;; (glu:perspective 50 1 0.5 20)
   ;; (gl:matrix-mode :modelview)

   ;; (gl:bind-framebuffer-ext :framebuffer-ext 0)
   ;; (gl:viewport 0 0 (glut:width window) (glut:height window))
   ;; (gl:matrix-mode :projection)
   ;; (gl:load-identity)
   ;; (glu:perspective 50 (/ (glut:width window) (glut:height window)) 0.5 20)
   ;; (gl:matrix-mode :modelview)
  

  (gl:clear :color-buffer)
  (gl:color 1 1 1) 
  (gl:line-width 5)
  (gl:with-primitives :lines
    (gl:vertex 0 0)
    (gl:vertex 0.5 0.5)
    (gl:vertex 0.5 0.0)
    (gl:vertex 0.0 0.0))
 
  (gl:flush))

;;;-----------------------------------------------------------------------
(defmethod glut:reshape ((w render-buffer) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height))

;;;-----------------------------------------------------------------------
(defun rb ()
  (glut:display-window (make-instance 'render-buffer)))
