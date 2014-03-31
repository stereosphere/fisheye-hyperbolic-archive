;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

(defparameter *SERIAL-NUMBER* 0)

;;;----------------------------------------------------------------
(defclass EQUI-LINE ()
  ((e-a :initform nil :initarg :e-a :accessor e-a)
   (e-b :initform nil :initarg :e-b :accessor e-b)
   (swept-angle :initform nil :initarg :swept-angle :accessor swept-angle)
  ;;for the straight line case
   (is-straight :initform nil :accessor is-straight)
   (e-point  :initform nil :initarg :e-point :accessor e-point)
   (e-direction :initform nil :initarg :e-direction :accessor e-direction)
   (e-center :initform nil :initarg :e-center :accessor e-center)
   (e-radius :initform nil :initarg :e-radius :accessor e-radius)))

;;;----------------------------------------------------------------
(defclass H-LINE ()
  ((e-a :initform nil :initarg :a :accessor e-a)
   (e-b :initform nil :initarg :b :accessor e-b)
   (swept-angle :initform nil :initarg :swept-angle :accessor swept-angle)
   ;;for the straight line case
   (is-straight :initform t :accessor is-straight)
   (e-point  :initform nil :initarg :e-point :accessor e-point)
   (e-direction :initform nil :initarg :e-direction :accessor e-direction)
   ;;for the circular case
   (e-center :initform nil :initarg :e-center :accessor e-center)
   (e-radius :initform nil :initarg :e-radius :accessor e-radius)))

;;;-----------------------------------------------------------------
(defmethod FIND-SWEPT-ANGLE (center radius a b)
  (if (and (> radius 0.0 ) (/= a b) (/= center a) (/= center b))
      (let* ((a-c (- a center))
	     (b-c (- b center))
	     (dp  (dot-product a-c b-c)))
	;;(print (list (abs a-c) (abs b-c) radius))
	(destructuring-bind (ax ay bx by) (dismember a-c b-c)
	  (let* ((dir (- (* ax by) (* ay bx))) ;cross product
		 (dir1 (/ dir (abs dir))));either 1 or -1
	    (* dir1 (acos (/ dp (* radius radius)))))))
      ;;else sweep = 0
      0.0))

;;;----------------------------------------------------------------
(defun RANDOM-PERTURB (&optional (mag 0.1))
  (complex (random mag) (random mag)))

;;;----------------------------------------------------------------
(defmethod CALCULATE-H-LINE ((hl h-line) (a complex) (b complex))
  (when (/= a b)
    (with-slots (is-straight e-a e-b e-center e-radius e-point e-direction) hl
      (destructuring-bind (ax ay bx by) (dismember a b)
        ;;dot90 is dot product of b reflected in 45 degree line and a
        (let ((dot90 (- (* ax by) (* ay bx)))) 
          ;;(print dot90)
          (setf is-straight (< (abs dot90) 1e-6)) 
          (if is-straight
            (let ((dif (- b a)))
	      ;;(break)
              (setf e-point a)
              (setf e-direction (/ dif (abs dif)))
	      (setf e-center nil)
	      (setf e-radius nil))
            ;;else it's a circle
            (let* ((s1 (/ (+ 1.0 (* ax ax) (* ay ay)) 2.0))
                   (s2 (/ (+ 1.0 (* bx bx) (* by by)) 2.0))
                   (center  (complex (/ (- (* s1 by) (* s2 ay)) dot90)
                                     (/ (- (* s2 ax) (* s1 bx)) dot90)))
                   (radius (sqrt (- (abs-squared center) 1.0))))
              ;;(print (list dot90 e-a e-b e-center e-radius))
              (setf e-center  center)
              (setf e-radius (realpart radius)))))
        (setf (e-a hl) a)
        (setf (e-b hl) b)))))


;;;-----------------------------------------------------------------
(defmethod MAKE-H-LINE ((a complex) (b complex))
  (let ((hl (make-instance 'h-line)))
    (with-slots (e-center e-radius e-a e-b) hl
      (calculate-h-line hl a b)
      (when e-center
	(if (= (e-radius hl) 0.0)
	    (setf (swept-angle hl) 0.0)
	    (let ((sw-ang (find-swept-angle e-center e-radius e-a e-b)))
	      (if (complexp sw-ang)
		  (setf (swept-angle hl) 0.0)
		  (setf (swept-angle hl) sw-ang))))))
    hl))

;;;-----------------------------------------------------------------
(defmethod COPY-H-LINE (hl)
  (let ((new-hl (make-instance 'h-line)))
    (with-slots (swept-angle is-straight e-center e-radius e-a e-b e-point e-direction) new-hl
      (setf e-a         (e-a hl))
      (setf e-b         (e-b hl))
      (setf swept-angle (swept-angle hl))
      (setf is-straight (is-straight hl))
      (setf e-point     (e-point hl))
      (setf e-direction (e-direction hl))
      (setf e-center    (e-center hl))
      (setf e-radius    (e-radius hl)))
    new-hl))

;;;-----------------------------------------------------------------
(defmethod MODIFY-H-LINE (hl (a complex) (b complex))
  (with-slots (e-center e-radius e-a e-b) hl
    (calculate-h-line hl a b)
    (when e-center
      (if (= (e-radius hl) 0.0)
	  (setf (swept-angle hl) 0.0)
	  (let ((sw-ang (find-swept-angle e-center e-radius e-a e-b)))
	    (if (complexp sw-ang)
		(setf (swept-angle hl) 0.0)
		(setf (swept-angle hl) sw-ang))))))
  hl)



;;;-----------------------------------------------------------------
(defmethod GET-HEAD ((hl h-line))
  (e-b hl))

;;;-----------------------------------------------------------------
(defmethod GET-TAIL ((hl h-line))
  (e-a hl))


;;;----------------------------------------------------------------
;;; make the h-line through a given point that is symmetric with
;;;respect to the unit circle centered at the origin
;;; if p is at the origin, returns nil
(defmethod MAKE-H-LINE-THROUGH-POINT ((p complex))
  (when (/= p #c(0.0 0.0))
    (let* ((hl (make-instance 'h-line))
           (mag (abs p))
           (r (/ (- 1.0 (* mag mag)) (* 2.0 mag)))
           (center (* (+ mag r) (cis (phase-sf p)))))
      (multiple-value-bind (a b) (circle-circle #c(0.0 0.0) 1.0 center r)
        ;;(make-wire-complex (list a b) "circle-intersections")
        (calculate-h-line hl a b))  
      (with-slots (e-center e-radius e-a e-b) hl
	(setf (swept-angle hl) (find-swept-angle e-center e-radius e-a e-b)))
      hl)))


;;;-----------------------------------------------------------------
(defmethod POINTS-SENSE (h-point-list)
  (let* ((a (first  h-point-list))
         (b (second h-point-list))
         (c (third  h-point-list))
         (v0 (- a b))
         (v1 (- c b))
         (res (- (* (realpart v0) (imagpart v1))
                 (* (realpart v1) (imagpart v0)))))
    (cond ((> res 0.0)
           -1)
          ((< res 0.0)
           1)
          (t ;; a and b equal
           0))))



;;;-----------------------------------------------------------------
(defclass H-POLY ()
  ((p          :initform 0   :initarg :p :reader p)
   (q          :initform 0   :initarg :q :reader q)
   ;;center is the hyperbolic center, but it is expressed in Euclidean coords
   (center    :initform (complex 0.0 0.0)  :initarg :center :accessor center)
   ;;radius is in hyperbolic coordinates -- not using
   (radius    :initform 0.0 :initarg :radius :accessor radius)
   (sense      :initform 1   :initarg :sense :reader sense) ;;counter-clockwise = 1, clockwise = -1
   (first-edge :initform nil :initarg :first-edge :accessor first-edge) ;;low number exposed edge, to be reflected about
   (num-edges  :initform nil  :initarg :num-edges :accessor num-edges);;number of exposed edges to do
   (h-lines    :initform nil :initarg :h-lines :reader h-lines)
   (equi-lines :initform nil :initarg :equi-lines :reader equi-lines)
   (circ-pts   :initform nil :initarg :circ-pts :accessor circ-pts) ;;a list of 3 edge midpoints to define the inscribed circle
   (serial-number :initform (incf *serial-number*) :accessor serial-number);;not working
   (properties :initform nil :initarg :properties :accessor properties)))


;;;------------------------------------------------------------------
;;;Make a smaller interior region based on the h-lines
;;;scale is how much border to leave, e.g. 0.95 is inside by 5%
;;;This is used when the polygon is centered.
(defun MAKE-INTERIOR-REGION-LINES (h-lines scale)
  (let ((e-lines (loop for hl in h-lines
		    collect
		      (MAKE-INTERIOR-REGION-LINE hl scale))))
    e-lines))

;;;------------------------------------------------------------------
(defun MAKE-INTERIOR-REGION-LINE (hl scale)
  (let* ((a (* scale (get-tail hl)))
	 (b (* scale (get-head hl)))
	 (e-line (make-instance 'equi-line
				:e-a a
				:e-b b)))
    (with-slots (e-a e-b swept-angle e-center e-radius) e-line
      (multiple-value-bind (i0 i1) 
	  (circle-circle (e-center hl) (e-radius hl) #c(0.0 0.0) 1.0)
	(multiple-value-setq (e-center e-radius)
	  (circle-through-3-points i0 i1 e-a))
	(setf swept-angle (find-swept-angle e-center e-radius e-a e-b))))
    e-line))

;;;------------------------------------------------------------------
(defun MODIFY-INTERIOR-REGION-LINE (hl el)
  (with-slots (e-a e-b is-straight swept-angle e-center e-radius) el
    (destructuring-bind (ax ay bx by) (dismember e-a e-b)
      ;;dot90 is dot product of b reflected in 45 degree line and a
      (let ((dot90 (- (* ax by) (* ay bx)))) 
	;;(print dot90)
	(setf is-straight (< (abs dot90) 1e-6)) 
	(if (is-straight hl)
	    (multiple-value-bind (i0 i1)
		(line-circle #c(0.0 0.0) 1.0 (e-a hl) (e-b hl))
	      (multiple-value-setq (e-center e-radius)
		(circle-through-3-points i0 i1 e-a))
	      (setf e-center nil))
	    (multiple-value-bind (i0 i1) 
		(circle-circle (e-center hl) (e-radius hl) #c(0.0 0.0) 1.0)
	      (declare (ignore i1))
	      (if i0
		  (progn 
		    (multiple-value-setq (e-center e-radius)
		      (circle-through-3-points e-a i0 e-b))
		    (setf swept-angle (find-swept-angle e-center e-radius e-a e-b)))
		  (progn
		    (setf swept-angle 0.0))))))))
  el)
	      ;;(when (complexp swept-angle)
		;;(describe hl)
		;;(describe el))))))))

;;;-------------------------------------------------------------------
;;;find the center of an hp's edge
(defun FIND-H-EDGE-CENTER (hl)
  (let* ((hl-center  (/ (+ (e-a hl) (e-b hl)) 2.0)) ;;any h-line will do
	 (dir (/ hl-center (abs hl-center)))) ;;normalize center to make dir
    (line-circle (e-center hl) (e-radius hl) #c(0.0 0.0) dir)))

;;;------------------------------------------------------------------
;;;find the circ-pts, need three
(defun SET-CIRC-PTS (hp)
  (setf (circ-pts hp) (make-array 3))
  (loop for hl across (h-lines hp)
     for i from 0
     repeat 3
     do
       (setf (aref (circ-pts hp) i) (find-h-edge-center hl))))


;;;------------------------------------------------------------------
(defmethod MAKE-FUNDAMENTAL-HP (p q)
  (let* ((fhp-points (make-fundamental-region-points p q))
	 (lines (loop for (a b) on fhp-points
		   collect (if b
			       (make-h-line a b)
			       (make-h-line a (first fhp-points)))))
	 (hp (make-instance 'h-poly 
			    :p p
			    :q q 
			    ;;center is the default, 0 0
			    :radius (e-h (abs (first fhp-points))) ;;the h-distance to any point is the radius
			    :sense (points-sense fhp-points)       
			    :h-lines (make-array (length lines)
						 :initial-contents lines)
			    :equi-lines (make-array (length lines)
						    :initial-contents (make-interior-region-lines lines 0.9)))))
    (set-circ-pts hp)
    ;;(print 'made-fundamental-hp)
    ;;(break)
    hp))
  
    

 ;;;-----------------------------------------------------------------
(defmethod COPY-H-POLY (hp)
  (let* ((lines (loop for (a b) on (get-points hp)
		   collect (if b
                               (make-h-line a b)
                               (make-h-line a (first (get-points hp))))))
	 (equi-lines (loop for hl in lines
			for el0 across (equi-lines hp)
			for el = (make-instance 'equi-line)
			collect
			  (progn 
			    (setf (e-a el) (e-a el0))
			    (setf (e-b el) (e-b el0))
			    (modify-interior-region-line hl el))))
	 (new-hp (make-instance 'h-poly 
				:p (p hp)
				:q (q hp)
				:sense (points-sense (get-points hp))
				:center (center hp)
				:radius (radius hp) 
				:h-lines (make-array (length lines)
						     :initial-contents lines)
				:equi-lines (make-array (length lines)
							:initial-contents equi-lines)
				:circ-pts (make-array 3 :initial-contents (circ-pts hp)))))
    ;;(print 'copied-h-poly)
    ;;(break)
    new-hp))
    


;;;-------------------------------------------------------------------
(defmethod GET-POINTS ((hp h-poly))
  (let ((pts (loop for hl across (h-lines hp)
		collect (get-tail hl))))
    pts))

;;;-----------------------------------------------------------------
(defmethod GET-LINE ((hp h-poly) i)
  (with-slots (p h-lines) hp
    (aref h-lines (mod i p))))

;;;-----------------------------------------------------------------
(defmethod REFLECT ((r complex) (hl h-line))
  (let ((rx (realpart r))
        (ry (imagpart r)))
    (if (is-straight hl)
      ;;is straight
      (destructuring-bind (px py dx dy) (dismember (e-point hl) (e-direction hl))
        (let* ((dif (- r (e-point hl)))
               (factor (* 2.0 (dot-product (e-direction hl) dif))))
          (complex (+ (* 2.0 px) (* factor dx) (- rx))
                   (+ (* 2.0 py) (* factor dy) (- ry)))))
      ;;else it's a circle
      (let* ((dif (- r (e-center hl)))
             (radius (e-radius hl))
             (factor (/ (* radius radius) (abs-squared dif)))
             (x (realpart (+ (realpart (e-center hl)) (* factor (realpart dif)))))
             (y (realpart (+ (imagpart (e-center hl)) (* factor (imagpart dif))))))
        ;;(describe hl)
        ;;(print (list 'dif dif 'rad radius 'factor factor 'x x 'y y))
        (complex x y)))))

(defun TESTREFLECT (c center radius)
  (let* ((dif (- c center))
	 (factor (/ (* radius radius) (abs-squared dif)))
	 (x (realpart (+ (realpart center) (* factor (realpart dif)))))
	 (y (realpart (+ (imagpart center) (* factor (imagpart dif))))))
    (complex x y)))

;;;-----------------------------------------------------------------
;;;Takes a point and makes two h-lines that will translate the
;;;distance magnitude of p in the  direction of p
(defmethod MAKE-TRANSLATER ((a complex))
  (let* ((dir90 (/ (complex (- (imagpart a)) (realpart a)) 2.0))
         (hla (make-h-line #c(0.0 0.0) dir90));;;!!!!!!make-h-line may be nil
         (hlb (make-h-line-through-point a)))
     (values hla hlb)))

;;;-----------------------------------------------------------------------------
(defun MAKE-FUNDAMENTAL-POINT (p q)
  ;;find radius r and center c of generating circle
  (multiple-value-bind (r c) (generating-circle p q)
    (let* ((angle (/ +pi+ p))
           (cosa  (cos angle))
           (sina  (sin angle))
           (b     (- (* cosa (- c)))) ;;extra work TAKE OUT
           (root  (sqrt (- (* b b) (- (* c c) (* r r)))))
           (t1    (- b root)) ;;use the closest intersection
           (x     (* t1 cosa))
           (y     (* t1 sina)))
      (complex x y))))

;;;----------------------------------------------------------------
(defmethod ROTATE-POINT (radians (pt complex))
  (let* ((rotater (cis radians)))
    (* rotater pt)))
                  

;;;----------------------------------------------------------------
(defun TRANSLATE-POINT (pt hla hlb)
  (reflect (reflect pt hla) hlb))
  
;;;----------------------------------------------------------------
(defun MAKE-FUNDAMENTAL-REGION-POINTS (p q)
  (let* ((fundamental-point (make-fundamental-point p q))
         (tiles (reverse (loop for ang from 0.0 below +2pi+ by (/ +2pi+ p)
			    repeat p         
			    collect (rotate-point ang fundamental-point)))))
    tiles))


	   
	 

;;;-------------------------------------------------------------------
;;;puts one point of the fundamental region at the origin
;;;doesn't work!
(defun CENTER-FUNDAMENTAL-REGION (p q)
  (let* ((fhp (make-fundamental-hp p q))
	 (hpt (first (get-points fhp)))
	 (dir (/ hpt (abs hpt)))
	 (dist (/ (abs (h-e (radius fhp))) 1.0)))
    (multiple-value-bind (hla hlb) 
	(make-translating-h-lines dir dist)
      (print dist)
      ;;(describe hla)
      (describe fhp)
      (loop for hl across (h-lines fhp)
	 for a = (translate-point (get-tail hl) hla hlb)
	 for b = (translate-point (get-head hl) hla hlb)
	 do
	   (modify-h-line hl a b)))
    fhp))


;;;----------------------------------------------------------------
(defun MAKE-HYPERBOLIC-STAR (p q)
  (let ((pts (make-fundamental-region-points p q)))
    (center-fundamental-region pts q)))

;;;----------------------------------------------------------------
;;; return list rotated so that elem is the first element of the list
(defun ROTATE-LIST (elem list)
  (let* ((pos (position elem list))
         (front (nthcdr pos list))
         (back (reverse (nthcdr (- (length list) pos) (reverse list)))))
    (append front back)))

;;;----------------------------------------------------------------
;;; return array rotated so that elem is the first element of the array
(defun ROTATE-ARRAY (elem arr)
  (let* ((idx (position elem arr))
         (n   (length arr))
         (tmp-arr (copy-seq arr)))
    (loop for i from idx
        for j from 0 below n
        do
          (print (setf (aref arr j) (aref tmp-arr (mod i n)))))
    arr))

;;;----------------------------------------------------------------
(defun TRANSLATE-HP (hp hla hlb)
  (with-slots (h-lines equi-lines) hp
    (loop for hl across h-lines
	 for el across equi-lines
	 for a = (translate-point (e-a hl) hla hlb)
	 for b = (translate-point (e-b hl) hla hlb)
	 for equi-a = (translate-point (e-a el) hla hlb)
	 for equi-b = (translate-point (e-b el) hla hlb)
       do
	 (modify-h-line hl a b)
	 (setf (e-a el) equi-a)
	 (setf (e-b el) equi-b)
	 (modify-interior-region-line hl el)))
  (setf (center hp) (reflect (reflect (center hp) hla) hlb))
  ;;the following may not be correct

  (reflect-circ-pts hp hla)
  (reflect-circ-pts hp hlb)
  hp)
	 

;;;--------------------------------------------------------------------
;;;reflect about the index point
(defmethod REFLECT-HP0 (hp-pt-list index)
  (let* ((hl (make-h-line (nth index hp-pt-list) 
                          (nth (1+ index) hp-pt-list)))
         (new-points (loop for p   in hp-pt-list ;;reflecting some points to themselves
                         collect (reflect p hl))))
    ;;(describe hl)
    new-points))

;;;--------------------------------------------------------------------
;;;reflect about the index point
(defmethod REFLECT-HP ((hp h-poly) (hl h-line))
  (let* ((new-points (loop for p in (get-points hp) ;;reflecting some points to themselves
                         collect (reflect p hl))))
    ;;(describe hl)
    (reverse new-points)))

;;;---------------------------------------------------------------------------------------
(defmethod REFLECT-HL ((hl h-line) (reflektor h-line))
  (with-slots (e-a e-b e-center e-radius swept-angle) hl
    (setf e-a (reflect e-a reflektor))
    (setf e-b (reflect e-b reflektor))
    (calculate-h-line hl e-a e-b)
    (if e-center
	(setf swept-angle (find-swept-angle e-center e-radius e-a e-b)))))
    


;;;------------------------------------------------------------------------
(defmethod REFLECT-HL0 ((in-hl h-line) (out-hl h-line) (reflektor h-line))
  (let* ((a (reflect (get-tail in-hl) reflektor))
         (b (reflect (get-head in-hl) reflektor)))
    (calculate-h-line out-hl a b)
    (setf (swept-angle out-hl) (find-swept-angle (e-center out-hl) (e-radius out-hl) a b))))

;;;---------------------------------------------------------------------------
(defun REFLECT-EQUI-LINE (hl el reflektor)
  (with-slots (e-a e-b e-center e-radius swept-angle) el
    (setf e-a (reflect e-a reflektor))
    (setf e-b (reflect e-b reflektor))
    ;(print (list 'a e-a 'b e-b 'r-a (e-a reflektor) 'r-b (e-b reflektor)))
    (modify-interior-region-line hl el)))
    
;;;------------------------------------------------------------------------------------
(defun REFLECT-CIRC-PTS (hp reflektor)
  (loop for pt across (circ-pts hp)
     for i from 0 below 3
     do
      (setf (aref (circ-pts hp) i) (reflect pt reflektor))))
       
           
;;;-------------------------------------------------------------------------------------------
(defmethod MAKE-REFLECTED-HP ((hp h-poly) (reflektor h-line))
  (let ((edge-number (position reflektor (h-lines hp)))) ;;position of reflektor in hp = position in new-hp
    (make-reflected-hp hp edge-number)))

(defgeneric MAKE-REFLECTED-HP (hp edge-number))
;;;-------------------------------------------------------------------------------------------
;;;These new h-polys have 2 internal edges and (- p 2) external edges
;; (defmethod MAKE-REFLECTED-HP ((hp h-poly) (edge-number t))
;;   (let* ((p (p hp))
;; 	 (reflektor (get-line hp edge-number))
;; 	 (new-hp (make-h-poly-from-h-lines (q hp)
;; 					   (loop for i from 0
;;                                               for hl = (get-line hp i)
;;                                               repeat (p hp)
;;                                               collect
;;                                                 (reflect-hl hl reflektor)))))
;;     (setf (center new-hp) (reflect (center hp) reflektor))
;;     (setf (num-edges new-hp) (- p 2))
;;     (if (cw new-hp)
;; 	(setf (first-edge new-hp) (mod (+ edge-number 2) p)) ;;next edge from the other interior edge
;; 	(setf (first-edge new-hp) (mod (+ edge-number 1) p)));;next edge
;;     new-hp))

;;;-------------------------------------------------------------------------------------------------
(defmethod MAKE-REFLECTED-HP ((hp h-poly) (edge-number t))
  (let* ((p (p hp))
	 (reflektor (COPY-H-LINE (get-line hp edge-number)))
	 (new-hp (copy-h-poly hp)))
    (with-slots (h-lines equi-lines center) new-hp
      (loop for i from 0 below (p hp)
	 for hl = (get-line new-hp i)
	 for el = (aref equi-lines i)
	 do   
	   (reflect-hl hl reflektor)
	   (reflect-equi-line hl el reflektor))

    (setf (center new-hp) (reflect (center hp) reflektor))
    (setf (num-edges new-hp) (- p 2))
    (if (cw new-hp)
	(setf (first-edge new-hp) (mod (+ edge-number 2) p)) ;;next edge from the other interior edge
	(setf (first-edge new-hp) (mod (+ edge-number 1) p)));;next edge
    new-hp)))

(defgeneric MAKE-REFLECTED-HP-X (hp edge-number))
;;;-------------------------------------------------------------------------------------------
;;;Can't think of a good name. This one is the same as the above except for the calculation of
;;;the first edge to start the next layer. These polygons have 3 internal edges and 
;;;(- p 3) external
(defmethod MAKE-REFLECTED-HP-X ((hp h-poly) (edge-number t))
  (let* ((p (p hp))
	 (reflektor (COPY-H-LINE (get-line hp edge-number)))
	 (new-hp (copy-h-poly hp)))
    (with-slots (h-lines equi-lines center) new-hp
      (loop for i from 0 below (p hp)
	 for hl = (get-line new-hp i)
	 for el = (aref equi-lines i)
	 do  
	   (reflect-hl hl reflektor)	
	   (reflect-equi-line hl el reflektor))
      (setf (center new-hp) (reflect (center hp) reflektor))
      (reflect-circ-pts new-hp reflektor)
      (setf (num-edges new-hp) (- p 3))
      (if (cw new-hp)
	  (setf (first-edge new-hp) (mod (+ edge-number 2) (p hp))) ;;next edge, 2 ccw from reflect edge
	  (setf (first-edge new-hp) (mod (+ edge-number 2) (p hp))));;next edge, 2 edges cw from reflect edge
      new-hp)))


;;;--------------------------------------------------------------------------------------
;;;make hp with 3 sides internal, as when the polygon is the initial reflection about the 
;;;edge of the previous layer polygon
(defmethod MAKE-REFLECTED-HP-3 ((hp h-poly) (edge-number t))
  ;;(PRINT (LIST 'EDGE-NUMBER EDGE-NUMBER))
  (let* ((p (p hp))
	 (reflektor (get-line hp edge-number))
	 (new-hp (copy-h-poly hp)))
    (with-slots (h-lines equi-lines center num-edges first-edge) new-hp
      (loop for i from 0 below (p hp)
	 for hl = (get-line new-hp i)
	 for el = (aref equi-lines i)
	 do  
	   (reflect-hl hl reflektor)	
	   (reflect-equi-line hl el reflektor))
      (setf center    (reflect (center hp) reflektor))
      (reflect-circ-pts new-hp reflektor)
      (setf num-edges (- p 3))
      (if (cw new-hp)
	  (setf first-edge (mod (- edge-number 2) p)) ;;next edge, 2 ccw from reflect edge
	  (setf first-edge (mod (+ edge-number 2) p)));;next edge, 2 edges cw from reflect edge
      ;;(print (list 'first-edge first-edge))
      new-hp)))

;;;--------------------------------------------------------------------------------------
;;;make hp with 2 sides internal, as when the polygon is reflected about a point
;;;of the previous polygon
;;;-------------------------------------------------------------------------------------------------
(defmethod MAKE-REFLECTED-HP-2 ((hp h-poly) (edge-number t))
  (let* ((p (p hp))
	 (reflektor (get-line hp edge-number))
	 (new-hp (copy-h-poly hp)))
    (with-slots (h-lines equi-lines center) new-hp
      (loop for i from 0 below (p hp)
	 for hl = (get-line new-hp i)
	 for el = (aref equi-lines i)
	 do   
	   (reflect-hl hl reflektor)
	   (reflect-equi-line hl el reflektor))

    (setf (center new-hp) (reflect (center hp) reflektor))
    (reflect-circ-pts new-hp reflektor)
    (setf (num-edges new-hp) (- p 2))
    (if (cw new-hp)
	(setf (first-edge new-hp) (mod (- edge-number 1) p)) ;;next edge from the other interior edge
	(setf (first-edge new-hp) (mod (+ edge-number 1) p)));;next edge
    new-hp)))
