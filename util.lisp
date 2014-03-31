;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

(defconstant  +pi+  3.1415927)
(defconstant +2pi+ (* 2.0 +pi+))
(defconstant +halfpi+ (* 0.5 +pi+))

;;;----------------------------------------------------------------
(defun TO-DEGREES (rad)
  (* rad (/ 180.0 +pi+)))

;;;----------------------------------------------------------------
(defun TO-RADIANS (deg)
  (* deg (/ +pi+ 180.0)))

;;;----------------------------------------------------------------
(defmethod ABS-SQUARED ((c complex))
  (realpart (* c (conjugate c))))

;;;----------------------------------------------------------------
(defmethod INNER-PRODUCT-SQUARED ((c complex))
  (realpart (* c (conjugate c))))

;;;-----------------------------------------------------------------
(defmethod DOT-PRODUCT ((a complex) (b complex))
  (destructuring-bind (ax ay bx by) (dismember a b)
    (+ (* ax bx) (* ay by))))

;;;----------------------------------------------------------------
(defmethod ANGLE-BETWEEN ((a complex) (b complex))
  (let ((cosa (/ (dot-product a b) (* (abs a) (abs b)))))
    (acos cosa)))

;;;----------------------------------------------------------------
(defmethod COS-SIN-BETWEEN ((a complex) (b complex))
  (let ((cosa (/ (dot-product a b) (* (abs a) (abs b)))))
    (values cosa (- 1.0 (* cosa cosa)))))

;;;
;;;----------------------------------------------------------------
;;;calculates e raised to the i theta power
(defun RCOS-ISIN (&optional (radians 0.0) (r 1.0))
  (* r (complex (cos radians) (sin radians))))

;;;----------------------------------------------------------------
;;;z is complex, a point in the 2d Argand plane
;;;need to think about this
(defmethod ROTATE-Z ((z complex) (radians t))
  (* z (rcos-isin radians)))

;;;-----------------------------------------------------------------
(defmethod ROTATE-Z ((z complex) (cossin complex))
  (* z cossin))

;;;---------------------------------------------------------------
;;;q is center of circle for inversion, r is radius
;;;WRONG??? 2013 17 06
(defmethod KINVERSION ((z complex) (q complex) (r t))
  (+ q (/ (* r r) (- z q ))))

;;;---------------------------------------------------------------
(defmethod CINVERSION ((z complex))
  (/ 1.0 (conjugate z)))

;;;---------------------------------------------------------------
;;; todo : explore using inversion for storing e-line
(defclass E-LINE ()
  ((a :accessor a :initarg :a) 
   (b :accessor b :initarg :b)))

;;;--------------------------------------------------------------
(defmethod PRINT-OBJECT ((l e-line) stream)
  (format stream "#<e-line ~a ~a>~%" (a l) (b l)))

;;;---------------------------------------------------------------
; (defun MAKE-E-LINE (a b)
;   (make-instance 'e-line :a a :b b))
    
; ;;;-------------------------------------------------------------
; ; (defclass H-LINE ()
; ;   ((a :accessor a :initarg :a) 
; ;    (b :accessor b :initarg :b) 
; ;    (center :accessor center) 
; ;    (r :accessor r)))

; ;;;-------------------------------------------------------------
; (defmethod E-PERPENDICULAR-BISECT ((a complex) (b complex))
;   (let* ((midpt (/ (+ a b) 2.0))
;          (perp  (rotate-z (- b a) +halfpi+)))
;     (make-e-line midpt (+ perp midpt))))

;;;--------------------------------------------------------------
(defmethod MAG-SQUARED ((a complex))
  (* a (conjugate a)))

;;;---------------------------------------------------------------
;;;(defun MAKE-H ()
;;;  (let ((coords (make-array 8 :initial-contents
;;;                            (list (make-ftriplet 0.0 ) (make-ftriplet 1.0 0.0) (make-ftriplet 1.0 1.0) (make-ftriplet 0.0 1.0)
;;;                                  (make-ftriplet 1.0 1.0) (make-ftriplet 2.0 1.0) (make-ftriplet 2.0 2.0) (make-ftriplet 1.0 2.0)))))
;;;    (3D-I::MAKE-POLYHEDRON-FROM-POINT-POLYS coords '((0 1 2 3) (4 5 6 7)))))
  
;;;----------------------------------------------------------------
;;; return list rotated so that elem is the first element of the list
(defun ROTATE-LIST (elem list)
  (let* ((pos (position elem list))
         (front (nthcdr pos list))
         (back (reverse (nthcdr (- (length list) pos) (reverse list)))))
    (append front back)))

;;;----------------------------------------------------------------
; (defun ROTATE-TO-FRONT (elem list)
;   (if (eql elem (first list))
;     list
;     (rotate-to-front (append

;;;---------------------------------------------------------------
(defun PHASE-SF (z)
  (coerce (phase z) 't))


;;;--------------------------------------------------------------
(defmethod DCOMPLEX ((x t) (y t))
  (complex (coerce x 'double-float)
           (coerce y 'double-float)))

;;;--------------------------------------------------------------
;;(defmethod DCOMPLEX ((x double-float) (y double-float))
;;  (complex x y))

;;;--------------------------------------------------------------
(defmethod TO-SF-COMPLEX (z)
  (complex (coerce (realpart z) 't) (coerce (imagpart z) 't)))

;;;--------------------------------------------------------------------------
;;; red to blue is 0.0 to 240.0. 240.0 to 360.0 goes back to red
;;; color is between 0.0 (red) and 1.0 (blue)
(defun RAINBOW-COLOR (color)
  (let* ((color-degree (* color 240.0))
	 (chroma 255)
	 (x (truncate (* 255.0 (- 1.0 (abs (- (mod (/ color-degree 60.0) 2.0) 1.0)))))))
    (cond ((<= color-degree 60.0)
	   (list chroma x 0))
	  ((<= color-degree 120.0)
	   (list x chroma 0))
	  ((<= color-degree 180.0)
	   (list 0 chroma x))
	  ((<= color-degree 240.0)
	   (list 0 x chroma))
	  ((<= color-degree 300.0)
	   (list x chroma 0))
	  ((<= color-degree 360.0)
	   (list chroma x 0))
	  (t
	   (list chroma x 0)))))





#|
;;;---------------------------------------------------------------
;;; use Cramer's rule
(defmethod E-INTERSECT ((el1 e-line) (el2 e-line))
  (let* ((direl1 (- (b el1) (a el1)))
         (direl2 (- (b el2) (a el2)))
         (c1     (mag-wq
;;;---------------------------------------------------------------
(defmethod MAKE-H-LINE ((a complex) (b complex))
  (let* ((ainv (cinversion a))
         (binv (cinversion b))
         (aperp (e-perpendicular-bisect a ainv))
         (bperp (e-perpendicular-bisect b binv)))
    |#
    

