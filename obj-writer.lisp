;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)   
  
;;;----------------------------------------------------------------------------
;;; need to put more info here
(defun WRITE-OBJ-HEADER (stream num-faces num-vertices)
  (format stream "~&####                             ")
  (format stream "~&#                                ")
  (format stream "~&# Vertices ~d                    " num-vertices)
  (format stream "~&# Faces    ~d                    " num-faces)
  (format stream "~&#                                ")
  (format stream "~&####                             "))

;;;-----------------------------------------------------------------------------
(defun SUBDIVIDE-LINE (a b num-points)
  (let ((step (/ (- b a) (coerce (1- num-points) 'double-float))))
    (loop for d from 0 below num-points
       collect
	 (+ a (* d step)))))
  


;;;-----------------------------------------------------------------------------
(defun WRITE-OBJ (str hps)
    (let ((dome (v3:make))
	  (vert-count 0)
	  (face-vert-count 0))
    (loop for hp in hps
       for pts = (get-points hp)
       do 
	 (loop for hl across (h-lines hp)
	    for points = (if (e-center hl)
			     (arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
			     (subdivide-line (e-a hl) (e-b hl) 8))
	    do  
	      ;;(print (length points))
	      (loop for pt in points
		 for coords = (stereographic-project pt dome)
		 do

		   (format str "~&v ~9,6f ~9,6f ~9,6f ~t# ~d" (v3:vx dome) (v3:vy dome) (v3:vz dome) vert-count)
		   (incf vert-count)
		   (incf face-vert-count)))
	 (setf (getf (properties hp) :count) face-vert-count)
	 (setf face-vert-count 0))
    (format str "~&# ~d vertices, 0 vertices normals" vert-count)
       ;;now do the face list
    (setf vert-count 1)
    (format str "~&s off")
    (loop for hp in hps
       for face-vert-count =  (getf (properties hp) :COUNT)

       do
	 (format str "~&f ")
	 (loop repeat face-vert-count
	    do
	      (format str " ~d" vert-count)
	      (incf vert-count))
	 (format str "~%"))
    (format str "~&# ~d faces, 0 coords texture" (length hps))
    (values vert-count (length hps))))

;;;---------------------------------------------------------------------------
(defun FILE-OBJ (hps objname path)
  (with-open-file (str path
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (write-obj-header str 0 0)
    (format str "~&o ~a" objname)
    (write-obj str hps)))

#|
    (with-open-file (str "~/hps.ply"
			 :direction :io
			 :if-exists :overwrite
			 :if-does-not-exist :error)
      (loop repeat 3
	 do
	   (print (read-line str)))
      (format str "element vertex ~6,'0d~%" vert-count)
      (file-position str 0)
      (loop repeat 13
	 do
	   (print (read-line str)))
      (format str "element face ~6,'0d~%" face-count))))
|#
