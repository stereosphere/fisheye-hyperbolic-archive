;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)
 
;;;--------------------------------------------------------------------
(defun ARC-POINTS-DOME (hl)
  (with-slots (e-center e-a e-b swept-angle) hl
    (let ((pts (if e-center
		   (arc-points e-center e-a swept-angle 8)
		   (line-points e-a e-b 8))))
      pts)))

;;;-------------------------------------------------------------------------
(defun MAKE-Y-REFLECT ()
  (let ((m (make-array 3 :element-type 'v3:v3 
		       :initial-contents  (list (v3:make) (v3:make) (v3:make)))))
    (setf (v3:vx (aref m 0))  1.0)
    (setf (v3:vy (aref m 1)) -1.0)
    (setf (v3:vz (aref m 2))  1.0)
    m))

;;;-------------------------------------------------------------------------
(defmethod GET-POLYGON-POINTS-DOME (hp)
 (let* ((pts (loop for el across (equi-lines hp)
		  append (arc-points-dome el))))
   pts))

;;;-------------------------------------------------------------------------
(defmethod GET-POLYGON-STYLE-POINTS-DOME (hp)
 (let* ((center-dist (abs (center hp)))
	(color (rainbow-color center-dist)))
   (destructuring-bind (r g b) color
     ;;(print (list (abs (center hp)) (center hp) r g b))
     (let ((style (svg-color r g b))
	   (pts (loop for el across (equi-lines hp)
		  append (arc-points-dome el))))
       (list style pts)))))
	

;;;-----------------------------------------------------------------
(defun MAKE-X-ROTATE (radians)
  (let ((m (make-array 3 :element-type 'v3:v3 
		       :initial-contents  (list (v3:make) (v3:make) (v3:make))))
	(sina (sin radians))
	(cosa (cos radians)))
    (setf (v3:vx (aref m 0)) 1.0)
    (setf (v3:vy (aref m 1)) cosa)
    (setf (v3:vz (aref m 1)) (- sina))
    (setf (v3:vy (aref m 2)) sina)
    (setf (v3:vz (aref m 2)) cosa)
    m))

;;;-----------------------------------------------------------------
(defun MAKE-Y-ROTATE (radians)
  (let ((m (make-array 3 :element-type 'v3:v3 
		       :initial-contents  (list (v3:make) (v3:make) (v3:make))))
	(sina (sin radians))
	(cosa (cos radians)))
    (setf (v3:vx (aref m 0)) cosa)
    (setf (v3:vy (aref m 0)) (- sina))
    (setf (v3:vy (aref m 1)) 1.0)
    (setf (v3:vy (aref m 2)) sina)
    (setf (v3:vz (aref m 2)) cosa)
    m))

;;;-----------------------------------------------------------------
(defun MAKE-Z-ROTATE (radians)
  (let ((m (make-array 3 :element-type 'v3:v3 
		       :initial-contents  (list (v3:make) (v3:make) (v3:make))))
	(sina (sin radians))
	(cosa (cos radians)))
    (setf (v3:vx (aref m 0)) cosa)
    (setf (v3:vy (aref m 0)) (- sina))
    (setf (v3:vy (aref m 1)) sina)
    (setf (v3:vy (aref m 1)) cosa)
    (setf (v3:vz (aref m 2)) 1.0)
    m))

;;;-----------------------------------------------------------------
;;;
(defun PROJECT-TO-DOME (hps m3x3)
  (loop for hp in hps
     for pts = (get-polygon-points-dome hp)
     collect
       (loop for z in pts
	  for coords = (stereographic-project z)
	  for rot-coords = (v3:m* m3x3 coords)
	  for on-plane = (unstereographic rot-coords)
	  collect
	   ;; (+ #c(1.0 1.0) (from-dome-to-equi-az rot-coords)))))
	    (+ #c(1.0 1.0) 
	       (/ (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*)))))



;;;-----------------------------------------------------------------
;;;
(defun PROJECT-BOTTOM-TO-DOME (hps m3x3)
  (loop for hp in hps
     for pts = (get-polygon-points-dome hp)
     collect
       (loop for z in pts
	  for coords = (let ((c (stereographic-project z)))
			 (setf (v3:vz c) (- (v3:vz c)))
			 ;;(setf (v3:vx c) (- (v3:vx c)))
			 c)
	  for rot-coords = (v3:m* m3x3 coords)
	  for on-plane = (unstereographic rot-coords)
	  when (> (v3:vz rot-coords) 0.0)
	  collect
	   ;; (+ #c(1.0 1.0) (from-dome-to-equi-az rot-coords)))))
	    (+ #c(1.0 1.0) (/ (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*))
	  do 
	    (when (> (v3:vz rot-coords) 0.0)
	      (format t "~&~4,3f ~4,3f ~4,3f" 
		      (v3:vx rot-coords) (v3:vy rot-coords) (v3:vz rot-coords))))))

(defparameter *CIRCLE-SCALE* 1.0)

;;--------------------------------------------------------------------------------
;;top-mat and bot-mat rotate the horizon, y-refl mirrors the top pts
(defun PROJECT-BOTH (hps x-rot z-rot)
  (let* ((top-mat (make-x-rotate (- x-rot)))
	 (bot-mat (make-x-rotate (- (+ x-rot pi))))
	 (z-rotater (cis z-rot))
	 (y-refl  (make-y-reflect))
	 (sty-pts (loop for hp in hps 
		 for style-pts = (get-polygon-style-points-dome hp)
		 for style = (first style-pts)
		 for zpts = (loop for p in (second style-pts) collect (* z-rotater p))
		 ;;collect the top pts
		 collect (list style (loop for z in zpts 
					for coords = (stereographic-project z)
					for rot-coords = (v3:m* top-mat coords)
					for on-plane = (unstereographic rot-coords)
					collect
					  (+ #c(1.0 1.0) (* (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*))))
			       into top-pts
		 ;;collect the bottom pts
		 when (/= x-rot 0.0)
		 collect (list style (loop for z in zpts 
					for coords = (stereographic-project z)
					for yrefl-coords  = (v3:m* y-refl coords)
					for xyrefl-coords = (v3:m* bot-mat yrefl-coords)
					for on-plane     = (unstereographic xyrefl-coords)
					collect
					  (+ #c(1.0 1.0) (* (complex (v3:vx on-plane) (v3:vy on-plane)) *circle-scale*))))
		 into bot-pts
		 finally (return (append top-pts bot-pts)))))
    ;;cull pts outside circle
    (setf sty-pts (remove-if 
	       (lambda (sty-pt-list) 
		 (every (lambda (z) 
			  (> (abs (- z #c(1.0 1.0))) 1.0)) (second sty-pt-list)))
	       sty-pts))
    ;;return pts
    sty-pts))

;;;(file-3d (append (project-to-dome (first-layer 4 6) (make-x-rotate (- (/ pi 6)))) 
;;;(project-bottom-to-dome (first-layer 4 6) (make-x-rotate (- (/ pi 6))))))

;;;---------------------------------------------------------------------------------
(defun RUN-AVCONV (avconv-filename root-name)
  (let ((rate "-r 30")
	(input (format nil "-i ~a" avconv-filename))
	(bv "b:v 3000k")
	(out (concatenate 'string root-name ".avi")))
    (format nil "~a ~a ~a ~a" rate input bv out)))
;; (print (concatenate 'string 
;; 		    "/usr/bin/avconv "
;; 		    "-r" " 30 "
;; 		    "-i " avconv-filename 
;; 		    " -b:v" " 3000k " 
;; 		    (concatenate 'string "../" root-name ".avi")))

  
;;;---------------------------------------------------------------------------------
;;;(loop for x in '(1 2 3 4 5) collect (1+ x) into a collect x into b finally (return (list a b)))
(defun ANIM-DOME (p q n &optional (name "a") (start-frame 1))
  (let* ((root-name (format nil "~a_~d~d~d" name p q n))
	 (avconv-filename (format nil "/home/michael/SVG-FRAMES/Anim0/~a_%04d.png" root-name))
	 (step 0.005)
	 (start-d (+ (* step (- start-frame 1)) 0.001)))
    start-d

    (loop for d from 0.01 to 3.0 by step 
    ;;(loop for d from 0.0 to (* 2.0 pi) by (/ pi 360.0)
       for fnum from start-frame
       repeat 1
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
		  (style-point-lists (project-both hps 0.0 0.0));;(- (/ pi 4.0)) 0.0));;(- (/ pi 4.0))))
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

;;;---------------------------------------------------------------------------------
(defclass DIR-STRUCTURE ()
  ((root :initform (make-pathname :directory '(:absolute "home" "michael" "SVG-FRAMES")) :accessor root)
   (filename :initarg :filename :accessor filename)
   (anim-dir :initarg :top-dir :accessor anim-dir)
   (svg-dir :initarg :svg-dir :accessor svg-dir)
   (png-dir :initarg :png-dir :accessor png-dir)))

;;;---------------------------------------------------------------------------------
(defun MAKE-DIR-STRUCTURE (anim-name)
  (let ((dirs (make-instance 'dir-structure :filename anim-name)))
    (with-slots (root filename anim-dir svg-dir png-dir) dirs
      (setf filename anim-name)
      (setf anim-dir (merge-pathnames (make-pathname :directory (list :relative filename)) root))
      (setf svg-dir (merge-pathnames #p"svg/" anim-dir))
      (setf png-dir (merge-pathnames #p"png/" anim-dir))
      (unless (probe-file anim-dir)
	(sb-posix:mkdir anim-dir #o777)
	(sb-posix:mkdir svg-dir #o777)
	(sb-posix:mkdir png-dir #o777))
      dirs)))

;;;---------------------------------------------------------------------------------
(defclass SVG-ON-DOME ()
  ((name        :initarg :name       :accessor name)
   (p           :initarg :p          :reader   p)
   (q           :initarg :q          :reader   q)
   (max-layers  :initarg :max-layers :reader   max-layers)
   (horizon     :initarg :horizon    :reader   horizon)
   (hla         :initarg :hla        :reader   hla)
   (hlb         :initarg :hlb        :reader   hlb)
   (png-spec                         :accessor png-spec)))


;;;--------------------------------------------------------------------------------- 
(defmethod SETUP-ANIM ((anim svg-on-dome))
  (with-slots (name p q max-layers png-spec) anim
    (let* ((dirs (make-dir-structure (name anim))))
      (setf (png-spec anim) (pathname (format nil "~a/~a_%04d.png" (namestring (png-dir dirs)) (filename dirs))))
      (sb-posix:chdir  (anim-dir dirs))
      anim)))

;;;---------------------------------------------------------------------------------
(defun MAKE-SVG-ON-DOME (name p q n &optional 
				      (horizon 0.0) 
				      (hla (make-h-line #c(0.0 0.0) #c(1.0 0.0)))
				      (hlb (make-h-line #c(0.0 0.0) #c(0.0 1.0))))
  (setup-anim (make-instance 'svg-on-dome :name name :p p :q q :max-layers n :horizon horizon :hla hla :hlb hlb)))


