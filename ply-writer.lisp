;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)   
  

;;;------------------------------------------------------------------
(defun WRITE-PLY-HEADER (str num-verts num-faces)
  (format str "ply~%~
               format ascii 1.0~%~
               comment Created by MLF~%~
               element vertex ~6,'0d~%~
               property float x~%~
               property float y~%~
               property float z~%~
               property float nx~%~
               property float ny~%~
               property float nz~%~
               property uchar red~%~
               property uchar green~%~
               property uchar blue~%~
               element face ~6,'0d~%~
               property list uchar uint vertex_indices~%~
               end_header~%" num-verts num-faces))

;;;------------------------------------------------------------------
(defun WRITE-HPS-ASCII (str hps)
  (let ((dome (v3:make))
	(vert-count 0)
	(face-vert-count 0))
    (loop for hp in hps
       for pts = (get-points hp)
       do 
	 (loop for hl across (h-lines hp)
	    for points = (if (e-center hl)
			     (arc-points (e-center hl) (e-a hl) (swept-angle hl) 8)
			     (list (e-a hl) (e-b hl)))
	    do  
	      (loop for pt in points
		 for coords = (stereographic-project pt dome)
		 do
		   (format str "~&~f ~f ~f" (v3:vx dome) (v3:vy dome) (v3:vz dome))
		   (format str "  ~f ~f ~f" 0.0 1.0 0.0)
		   (format str "  ~d ~d ~d" 255 255 face-vert-count)
		   (incf face-vert-count)))
	 (setf (getf (properties hp) :count) face-vert-count)
	 (setf face-vert-count 0))
   
       ;;now do the face list
    (loop for hp in hps
       for face-vert-count =  (getf (properties hp) :COUNT)
       do
	 (format str "~&~d " face-vert-count)
	 (loop repeat face-vert-count
	    do
	      (format str " ~d" vert-count)
	      (incf vert-count))
	 (format str "~%"))
    (values vert-count (length hps))))

(defun FILE-PLY (hps)
  (let ((vert-count 0)
	(face-count 0))
    (with-open-file (str "~/hps.ply"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (write-ply-header str 0 0)
      (multiple-value-setq (vert-count face-count)
	(write-hps-ascii str hps)))
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
