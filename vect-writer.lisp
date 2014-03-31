;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
  
;;;-----------------------------------------------------------------------------
(defun SUBDIVIDE-LINE0 (a b num-points)
  (let ((step (/ (- b a) (coerce (1- num-points) 'double-float))))
    (loop for d from 0 below num-points
       collect
	 (+ a (* d step)))))


;;;----------------------------------------------------------------------------
;;; need to put more info here
(defun WRITE-VECT (stream hps)    
  (let* ((dome          (v3:make))
	 (vert-count    0)
	 (num-polylines (length hps))
	 (num-edges     (p (first hps)))
	 (num-vertices  (* num-polylines num-edges 8))
	 (num-colors    1))
    (format stream "~&VECT")
    (format stream "~&~d ~d ~d" num-polylines num-vertices num-colors) 
    (format stream "~&")

    (loop for hp in hps
       do
	 (format stream "~d " (* num-edges 8)))
    (format stream "~&")

    (loop for hp in hps
       for color = 1 then 0
       do
	 (format stream "~d " color))

    (format stream "~&")
    (loop for hp in hps
       for pts = (get-points hp)
       do 
	 (loop for hl across (h-lines hp)
	    for points = (if (e-center hl)
			     (arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
			     (subdivide-line (e-a hl) (e-b hl) 8))
	    do  
	      (loop for pt in points
		 for coords = (stereographic-project pt dome)
		 ;;for coords = (v3:from-complex pt) ;;doesn't work??
		 do
		   ;;rotate so that y is up
		   (format stream "~&~9,6f ~9,6f ~9,6f" (v3:vx dome) (v3:vz dome) (- (v3:vy dome)))
		   (incf vert-count))))
    (format stream "~&1 1 1 1")))


;;;---------------------------------------------------------------------------
(defun FILE-VECT (hps path)
  (with-open-file (str path
		       :direction :output
		       :if-exists :supersede)
    (write-vect str hps)))


