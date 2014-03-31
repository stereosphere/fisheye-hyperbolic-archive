;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;---------------------------------------------------------------------------
(defclass H-ANIMATION ()
  ((tiles :accessor tiles)))
   
;;;---------------------------------------------------------------------------
(defmethod MAKE-TRANSLATING-H-LINES ((dir complex) (hdist double-float))
  (let* ((hla (make-h-line #c(0.0 0.0) dir)) 
         (dir90 (/ (dcomplex (- (imagpart dir)) (realpart dir)) (abs dir)))
         (edist (realpart (h-e hdist)))
         (hlb (make-h-line-through-point (* dir90 edist))))
    ;;(print edist)
    (values hla hlb)))


;;;-----------------------------------------------------
(defun ANIM-1 (tiles move-tiles hla hlb)
  (loop for hp in tiles
       for mhp in move-tiles
     do
       (loop for hl across (h-lines hp)
	  for mhl across (h-lines mhp)
	  for reflect1  = (reflect-hl0 hl mhl hlb)
	  do
	    (when reflect1
	      (reflect-hl0 mhl mhl hla)))))

;;;---------------------------------------------------------------------------
(defmethod MAKE-ROTATING-H-LINES ((ecenter complex) (angle double-float))
  (let* ((a (complex 1.0d0 0.0d0))
         (b (cis angle))
         (mag (coerce (abs ecenter) 'double-float))
         (dir (/ ecenter mag))
         (dir90 (dcomplex (- (imagpart dir)) (realpart dir))))
    (multiple-value-bind (hlc hld) 
	(make-translating-h-lines dir90 (coerce (realpart (e-h mag)) 'double-float))
      (setf a (reflect (reflect a hld) hlc))
      (setf b (reflect (reflect b hld) hlc))
      (setf ecenter (reflect (reflect ecenter hld) hlc))
      (values (make-h-line ecenter a)
              (make-h-line ecenter b)))))

;;;-------------------------------------------------------------------------------------------
(defmethod FIRST-LAYER-ANIM0 (p q hla hlb)
  (let* ((fhp (translate-hp (copy-h-poly (make-fundamental-hp p q)) hla hlb))
         (hps (loop for fhp-edge from 0 below p
		 for new-hp = (make-reflected-hp-3 fhp fhp-edge) ;;reflektor)
		 for other-edge = (if (cw new-hp) 
				      (1+ fhp-edge) 
				      (1- fhp-edge))
		;; repeat 2
		 append
		   (cons new-hp 
			 (reflect-about-edge new-hp 
					     other-edge fhp-edge (- q 3) nil)))))
    (when (= q 3)
      (loop for hp in hps
	 do
	   (setf (num-edges hp) 3)))
    (cons fhp hps)))

;;;-------------------------------------------------------------------------------------------
;;;doesn't work
(defmethod FIRST-LAYER-ANIM-CENTER (p q hla hlb)
  (let* ((fhp (copy-h-poly (center-fundamental-region p q)))
         (hps (loop for fhp-edge from 0 below p
		 for new-hp = (make-reflected-hp-3 fhp fhp-edge) ;;reflektor)
		 for other-edge = (if (cw new-hp) 
				      (1+ fhp-edge) 
				      (1- fhp-edge))
		;; repeat 2
		 append
		   (cons new-hp 
			 (reflect-about-edge new-hp 
					     other-edge fhp-edge (- q 3) nil)))))
    (when (= q 3)
      (loop for hp in hps
	 do
	   (setf (num-edges hp) 3)))
    (cons fhp hps)
    (list fhp)));;;

;;;---------------------------------------------------------------------------
(defun TRANSLATE-HPS (hps hla hlb)
  (loop for hp in hps
     do
       (translate-hp hp hla hlb)))

;;;---------------------------------------------------------------------------  
(defun DO-LAYERS-ANIM (fl n)
  (let* ((layers0 fl)
	 (layers1 layers0))
    (loop for i from 0 below n
       for new-layers = (progn (print (list 'LAYER i)) 
			       (do-layer0 layers0))
       do
	 (print (list 'layer-length (length new-layers)))
	 (setf layers0 new-layers)
	 (setf layers1 (append layers1 new-layers))
       while
	 new-layers)
    layers1)) 

;;can be better quality, but this works
;;avconv -f image2 -i anim-4-6-2-%04d.vect.ppm -r 12 -s 450x450 out.avi

;;;---------------------------------------------------------------------------
(defun DO-ANIM1 (p q n start end num-frames &optional do-snap)
  (let ((inc (/ (- end start) (1- num-frames)))
	(script-file (format nil "/home/michael/Frames/anim-~d-~d-~d.script" p q n)))
    (with-open-file (script script-file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      ;;write camera stuff here
      (loop for d from start to end by inc
	 for fnum from 1
	 do
	   (multiple-value-bind (hla hlb) 
	       (make-translating-h-lines #c(1.0 0.0) d)
	     (let* ((objname (format nil "anim-~d-~d-~d-~4,'0d" p q n fnum))
		    (path (format nil "/home/michael/Frames/~a" objname))
		    (fl (first-layer-anim0 p q hla hlb))
		    (hps (do-layers-anim fl n)))
	       (format script "~&(load ~a geometry)" objname)
	       (format script "~&(bbox-draw ~a no)" objname)
	       (format script "~&(look anim-7-3-2-0197.vect c0)")
	       (when do-snap
		 (format script "~&(snapshot c0 ~a.ppm)" objname))
	       (format script "~&(delete ~a)" objname)
	       (file-vect hps path)
	       (format t "~&finished ~a" path)))))))
	 
;;;---------------------------------------------------------------------------
(defun DO-ANIM-OBJ (p q n start end num-frames)
  (let ((inc (/ (- end start) (1- num-frames))))
      (loop for d from start to end by inc
	 for fnum from 1
	 do
	   (multiple-value-bind (hla hlb) 
	       (make-translating-h-lines #c(1.0 0.0) d)
	     (let* ((objname (format nil "anim-~d-~d-~d-~4,'0d" p q n fnum))
		    (path (format nil "/home/michael/obj/~a.obj" objname))
		    (fl (first-layer-anim0 p q hla hlb))
		    (hps (do-layers-anim fl n)))
	       (file-obj hps objname path)
	       (format t "~&finished ~a" path))))))	   
	
;;;-----------------------------------------------------
;;; 
;; (defun SETUP-TRANSLATION-ANIM (p q n dir)
;;   (multiple-value-bind (hla hlb) (make-translater dir)
;;     (setup-anim p q n hla hlb)))

;; ;;;-----------------------------------------------------
;; ;;;
;; (defun SETUP-ROTATION-ANIM (p q n ecenter rads)
;;   (let* ((a (+ ecenter (complex 1.0d0 0.0d0)))
;;          (b (+ ecenter (cis rads)))
;;          (hla (make-h-line ecenter a))
;;          (hlb (make-h-line ecenter b)))
;;     (setup-anim p q n hla hlb))) 

