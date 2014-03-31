;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;-------------------------------------------------------------------------------------------
(defmethod CW (hp) 
  (= -1 (sense hp)))

;;;-------------------------------------------------------------------------------------------
(defmethod CCW (hp) 
  (= 1 (sense hp)))


;;;-------------------------------------------------------------------------------------------
(defmethod REFLECT-ABOUT-EDGE (hp edge other-edge n polys)
  (cond ((= n 0)
         (reverse polys))
        (t 
         (let* ((new-hp (make-reflected-hp-2 hp edge)))
	   ;;(draw-h-polys (list hp new-hp))
	   (cond (new-hp
		  (setf (num-edges new-hp) (- (p new-hp) 2))
		  (reflect-about-edge new-hp 
				      other-edge edge (1- n) (cons new-hp polys)))
		 (t
		  (reverse polys)))))))


;;;----------------------------------------------------------------------------	   
(defun ARC-POINTS (center start-point swept-angle num-points)
  (let ((start (- start-point center))
	(step  (/ swept-angle (1- num-points))))
    (if (not (complexp step))
	(loop for i from 0.0 by 1.0
	   for r   = (cis (* i step));;this makes a complex number that rotates
	   repeat num-points
	   collect (+ center (* r start)))
	(list start-point))))

;;;------------------------------------------------------------------------------
(defun LINE-POINTS (start-point end-point num-points)
  (let* ((diff  (- end-point start-point))
	 (diffx (realpart diff))
	 (diffy (imagpart diff))
	 (stepx (/ diffx (1- num-points)))
	 (stepy (/ diffy (1- num-points))))
    (loop for i from 0.0 by 1.0
       for dx = (* i stepx)
       for dy = (* i stepy)
       repeat num-points
       collect (+ start-point (complex dx dy)))))



(defun ARC-POINTS0 (hl)
  (with-slots (e-center e-a e-b swept-angle) hl
    (arc-points e-center e-a swept-angle 8)))


;;;-------------------------------------------------------------------------------------------
(defmethod FIRST-LAYER (p q)
  (declare (special *serial-number*))
  (setf *serial-number* 0)
  (let* ((fhp (make-fundamental-hp p q))
         (hps (loop for fhp-edge from 0 below p
                  ;;for reflektor = (get-line fhp fhp-edge);;(- start-edge 1))
                  for new-hp = (make-reflected-hp-3 fhp fhp-edge) ;;reflektor)
                  for other-edge = (if (cw new-hp) 
                                     (1+ fhp-edge) 
                                     (1- fhp-edge))  
		 ;;  repeat 1
		 append
		   (cons new-hp
			 (reflect-about-edge new-hp other-edge fhp-edge (- q 3) nil)))))
    hps))

;;;-------------------------------------------------------------------------------------------
(defun ADD-Z (hp start-edge num-edges)
  (let* ((new-hp (make-reflected-hp-3 hp start-edge))
         (other-edge (if (cw new-hp)
                       (1+ start-edge) 
                       (1- start-edge))))
    ;;(print (list (sense hp) start-edge other-edge))
    ;;(file (list new-hp))
    ;;(sleep 5)
    (cons new-hp 
	  (reflect-about-edge new-hp other-edge start-edge num-edges nil))))

;;;-------------------------------------------------------------------------------------------
(defun ADD-Z0 (hp start-edge num-edges)
  (let* ((new-hp (make-reflected-hp-3 hp start-edge))
         (other-edge (if (ccw new-hp)
                       (1+ start-edge) 
                       (1- start-edge))))
    (print (list (sense hp) start-edge other-edge))
    ;;(file (list new-hp))
    ;;(sleep 5)
    (cons new-hp 
	  (reflect-about-edge new-hp other-edge start-edge num-edges nil))))


 
(defparameter *DO-CW* t)
(defparameter *DO-CCW* t)
;;;-------------------------------------------------------------------------------------------
(defun DO-LAYER0 (prev-layer) 
  (print (list 'num-prev (length prev-layer)))
  (if (= (q (first prev-layer)) 3)
      (do-layer0-q3 prev-layer)
      (let* ((discard-count 0)
	     (thresh 0.999) ;;;;;;;;0.995)
	     (hps (loop for hp in prev-layer
		     for start = (first-edge hp)
		     for q = (q hp)
 		     for n from 0
		     for n-edges = (num-edges hp)
		     for dist-from-center = (abs (center hp))
		     when (and n-edges (< dist-from-center thresh));;n-edges will be nil for seed hp
		     append
		       (if (cw hp)
			   (when *do-cw*
			     (Loop for i from 0 below n-edges
				for start-edge = (- start i)
				for num = (cond ((= q 3)
						 0)
						((= i (1- n-edges))
						 (- q 4))
						((= i 0)
						 (- q 3))
						(t
						 (- q 3)))
				append
				  (add-z hp start-edge num)))
			   ;;ELSE
			   (when *do-ccw*
			     (loop for i from 0 below n-edges
				for start-edge = (+ start i)
				for num = (cond ((= q 3)
						 0)
						((= i 0)
						 (- q 3))	     	   
						((= i (1- n-edges))
						 (- q 4))	
						(t
						 (- q 3)))
				append
				  (add-z hp start-edge num))))
		     do
		     ;; (when (= 0 (mod n 18000))
		     ;; 	 ;;(gc-and-report)
		     ;; 	 (cl-user::gc :full t))
		       (when (> dist-from-center thresh)
			 (incf discard-count)))))
	(print (list 'discarded discard-count 'len (length hps)))
	(unless hps
	  (print "hps nil"))
	hps)))

 
;;;-------------------------------------------------------------------------------------------
(defun DO-LAYER0-q3 (prev-layer) 
  (print (list 'do-layer0-q3))
  (let* ((discard-count 0)
	 (thresh 0.999) ;;;;;;;;0.995)
	 (hps (loop for hp in prev-layer
		 for new-hps = nil
		 for start = (first-edge hp)
		 for q = 3
		 for n from 0
		 for dist-from-center = (abs (center hp))
		 when (< dist-from-center thresh)
		 append
		   (if (cw hp)
		       (when *do-cw*
			 (Loop for i from 0 below (num-edges hp)
			    for start-edge = (- start i)
			    for new-hp = (first (add-z hp start-edge 0))
			    for num = (if (= (mod n 3) 0)
					  3
					  4)			    
			    do
			      (print (list n new-hp (num-edges new-hp)))
			      (setf (num-edges new-hp) num)
			      (push new-hp new-hps))
			 new-hps)
		       ;;ELSE
		       (when *do-ccw*
			 (loop for i from 0 below (num-edges hp)
			    for start-edge = (+ start i)
			    for new-hp = (first (add-z hp start-edge 0))
			    for num = (if (= (mod n 3) 0)
					  3
					  4)
			    do
			      (print (list n new-hp (num-edges new-hp)))
			      (setf (num-edges new-hp) num)
			      (push new-hp new-hps))
			 new-hps))
		 do
		 ;; (when (= 0 (mod n 18000))
		 ;; 	 ;;(gc-and-report)
		 ;; 	 (cl-user::gc :full t))
		   (when (> dist-from-center thresh)
		     (incf discard-count)))))
    (print (list 'discarded discard-count 'len (length hps)))
    (unless hps
      (print "hps nil"))
    hps))
		

;;;---------------------------------------------------------------------------  
(defun DO-LAYERS (p q n)
  (let* ((fhp (make-fundamental-hp p q))
	 (layers0 (first-layer p q))
	 (layers1 layers0))
    (when (= q 3)
      (loop for hp in layers0
	 do
	   (setf (num-edges hp) 3)))
    (loop for i from 0 below n
       for new-layers = (do-layer0 layers0)
       do
	 ;;(print (abs (center (first new-layers))))
	 (print (list 'layer i 'new-layers (length new-layers)))
	 (setf layers0 new-layers)
	 (setf layers1 (append layers1 new-layers))
	 (print (list 'so-far (length layers1)))
       while new-layers)
	 ;;(print (list 'new-layers (length new-layers) 'so-far (length layers1))))
    (print (list 'number-of-hps (1+ (length layers1))))
    (cons fhp layers1)))

;;;---------------------------------------------------------------------------  
;; (defun DO-LAYERS0 (p q n)
;;   (let* ((fhp (make-fundamental-hp p q))
;; 	 (layers0 (first-layer p q))
;; 	 (layers1 nil))
;;     (loop for i from 0 below n
;;        for new-layers = (do-layer layers0)
;;        while new-layers
;;        do
;; 	 ;;(print (abs (center (first new-layers))))
;; 	 (print (list 'layer i))
;; 	 (setf layers0 new-layers)
;; 	 (setf layers1 (append layers1 new-layers)))
;; 	 ;;(print (list 'new-layers (length new-layers) 'so-far (length layers1))))
;;     (cons fhp layers1)))
