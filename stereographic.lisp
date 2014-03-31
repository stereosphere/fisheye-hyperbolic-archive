;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)   
  

;;;===================================================================
;;;CAN'T PROJECT STRAIGHT UP LIKE THAT! ORTHOGRAPHIC IS
;;;FOR KLEIN THIS IS WRONG
;;;---------------------------------------------------------------
(defun TO-EQUI-AZ (ez)
  (let* ((abs-ez (abs ez))
         (on-sphere (sqrt (- 1.0 (* abs-ez abs-ez))))
         (angle (- (/ pi 2.0) (atan on-sphere abs-ez))))
    (values (* angle (cis (phase ez)))
            (list (realpart ez) (imagpart ez) on-sphere))))

;;;----------------------------------------------------------------
;; (defun TEST-TO-EQUI-AZ (thing)
;;   (let ((sphere-points nil)
;;         (flat-points nil))
;;    (loop for p in (get-points thing) 
;;        do
;;          (multiple-value-bind (flat sphere) (to-equi-az (complex-coords p))
;;            (push flat    flat-points)
;;            (push sphere  sphere-points)))))

;;;=========================================================================
      ;;;  
(defun STEREOGRAPHIC-PROJECT (z &optional (res (v3:make)))
  "stereographic projects from -1 on the negative z axis"
  (let* ((south-pole (v3:make 0.0d0 0.0 -1.0d0))
	 (dir        (v3:- south-pole (v3:from-complex z)))
	 (a          (v3:dot dir dir))
	 (b          (* 2.0 (v3:dot dir south-pole)))
	 (c          (- (v3:dot south-pole south-pole) 1.0))
	 (discrim    (- (* b b) (* 4.0 a c)))
	 (t1         (/ (- (- b) (sqrt discrim)) 
			(* 2.0 a)))) 
    (v3:* dir t1 res)
    (v3:+ res south-pole res)
    res))

;;;----------------------------------------------------------------------------
(defun FROM-DOME-TO-EQUI-AZ (coords)
  (let* ((dir (v3:make (v3:vx coords) (v3:vy coords) 0.0))
         (l (sqrt (v3:dot dir dir)))
         ;;(angle (atan (v3:vz coords) l))) 
         (angle (- (/ pi 2.0) (atan (v3:vz coords) l))))
    ;;(print (list 'coords coords 'dir dir 'l l 'angle angle))
    (if (> l 0.0)
	(setf dir (v3:normalize dir angle)))
    (/ (complex (v3:vx dir) (v3:vy dir)) (/ pi 2.0))))
    ;;(v3:/ dir (/ pi 2.0) dir)))

;;;----------------------------------------------------------------------------
(defun FROM-DOME-TO-EQUI-AZ-v3 (coords)
  (let* ((dir (v3:make (v3:vx coords) (v3:vy coords) 0.0))
         (l (sqrt (v3:dot dir dir)))
         ;;(angle (atan (v3:vz coords) l))) 
         (angle (- (/ pi 2.0) (atan (v3:vz coords) l))))
    ;;(print (list 'coords coords 'dir dir 'l l 'angle angle))
    (if (> l 0.0)
	(setf dir (v3:normalize dir angle)))
    (v3:/ dir (/ pi 2.0) dir)))


;;;--------------------------------------------------------------------
(defmethod UNSTEREOGRAPHIC (coords &optional (res (v3:make)))
  (let* ((south-pole (v3:make 0.0 0.0 -1.0))
	 (scale (/ 1.0 (+ 1.0 (v3:vz coords)))))
    (v3:- coords south-pole res)
    (v3:* res scale res)
    (v3:+ res south-pole res)))


#|


(defun FROM-DOME-TO-EQUI-AZ (coords)
  (let* ((dir (make-ftriplet (ftriplet-x coords) (ftriplet-y coords) 0.0))
         (l (sqrt (dot dir dir)))
         (angle (- (/ pi 2.0) (atan (ftriplet-z coords) l))))
    (print (list 'coords coords 'dir dir 'l l 'angle angle))
    (normalize-ftriplet dir (coerce angle 'single-float))))

(defun test-dome (thing)
  (let ((pts (loop for p in (collect-my-points thing)
                 collect (from-dome-to-equi-az (locus-coordinates p)))))
    (add-object-to-view (3d-i::make-wire-from-coordinate-alist pts))))

;;;--------------------------------------------------------------
(defun test-lots ()
  (loop for obj in (get-body (object-named "x"))
      for bod = (find-body obj)
      do
        (let ((o (test-stereo-proj bod)))
          (test-dome (find-body o)))))

;;;------------------------------------------------------------
;;;a1 is a point on circle
;;;find circle through a1 and center perpendicular to circle
(defmethod CIRCLE-3 (circ p)
  (let* ((e-center (get-e-center circ))
         (h-center (get-h-center circ))
         (a1 (complex-coords p))
         (xx (- a1 e-center))
         (a2 (+ a1 (complex (- (imagpart xx)) (realpart xx)))))
    (multiple-value-bind (c1 c2) (perpendicular-bisector h-center a1)
      (let* ((center (intersect c1 c2 a1 a2))
             ;;(center (intersect a1 a2 c1 c2))
             (radius (abs (- a1 center))))
        (values center radius)))))

 


;;;-----------------------------------------------------------------------
;;; only projects endpoints
(defmethod STEREO-PROJECT-H-POLYGON ((hp h-polygon))
  (let ((points nil))
    (using-ftriplet ((r1 r2) nil)
      (loop for v in (vertices hp)
          do
            (stereographic-project (e-pos v) r1 r2)
            (push (copy-ftriplet r1) points))
      points)))

;;;-----------------------------------------------------------------------
(defmethod H-STEREOGRAPHIC-FISHEYE ((w simple-wire))
  (using-ftriplet ((r1 r2) nil)
    (do-wire-nodes (n w)
      (let* ((p (locus-coordinates n))
             (z (complex (ftriplet-x p) (ftriplet-y p))))
        (stereographic-project z r1 r2)
                                        ;(print (list p z r1))
        (translate-to n r1)))))

;;;--------------------------------------------------------------------
(defmethod UNSTEREOGRAPHIC ((phd polyhedron))
  (using-ftriplet ((scaler3f center3f) nil)
    (fill-ftriplet center3f 0.0f0 -1.0f0 0.0f0)
    (do-vertices (v phd)
      (let* ((p (locus-coordinates v))
             (scale (/ 1.0f0 (+ 1.0f0 (ftriplet-y p)))))
        (fill-ftriplet scaler3f scale scale scale)
        (scale v scaler3f center3f)))))

;;;--------------------------------------------------------------------
(defmethod H-STEREOGRAPHIC-FISHEYE ((phd polyhedron))
  (using-ftriplet ((r1 r2) nil)
    (do-vertices (v phd)
      (let* ((p (locus-coordinates v))
             (z (complex (ftriplet-x p) (ftriplet-y p))))
        (stereographic-project z r1 r2)
                                        ;(print (list p z r1))
        (translate-to v r1)))))

;;;-----------------------------------------------------------------------
(defun STEREOGRAPHIC-MULTI (multi-obj)
  (loop for obj in (get-body multi-obj)
      do
        (h-stereographic-fisheye (find-body obj))))
    
 ;  (let* ((new-hp (copy-h-polygon hp))
;          (verts (rotxate-list (tail he) (vertices hp)))
;          (reflect-verts (nthcdr 2 verts)))
;     new-hp reflect-verts))
    



;;;-----------------------------------------------------------------
;;;Takes a point and makes two h-lines that will translate twice the
;;;distance magnitude of p in the  direction of p
(defmethod MAKE-TRANSLATER ((a complex))
  (let* ((dir90 (complex (- (imagpart a)) (realpart a)))
         (hla (make-h-line #c(0.0 0.0) dir90));;;!!!!!!make-h-line may be nil
         (hlb (make-h-line-through-point a)))
     (mirai-draw hla "wtf")
     (mirai-draw hlb)
     (values hla hlb)))

;;;-----------------------------------------------------------------
(defmethod MAKE-ROTATER ((ecenter complex) angle)
  (let* ((a (+ ecenter (complex 1.0d0 0.0d0)))
         (b (+ ecenter (cis angle)))
         (hla (make-h-line ecenter a))
         (hlb (make-h-line ecenter b)))
    (mirai-draw hla)
    (mirai-draw hlb)
    (values hla hlb)))

|#
