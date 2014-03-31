;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

(defconstant +origin+ #c(0.0 0.0))

;;;-----------------------------------------------------------
;;;???
(defun Ma (z a)
  (/ (- z a) 
     (- (* (conjugate a) z) #c(1.0 0.0))))



;;------------------------------------------------------------
;;;inversion of z in arbitrary circle, radius r, center q
(defun k-invert (z q r)
  (+ (/ (* r r) (- (conjugate z) (conjugate q)))
     q))

;;;------------------------------------------------------------
;;;convenience
(defun c-invert(z)
  (k-invert z #c(0.0 0.0) 1.0))


;;;------------------------------------------------------------
(defmethod INTERSECT ((a1 complex) (a2 complex) (b1 complex) (b2 complex))
  (destructuring-bind (x1 y1 x2 y2 x3 y3 x4 y4) (dismember a1 a2 b1 b2)
    (let ((denom  (- (* (- y4 y3) (- x2 x1)) (* (- x4 x3) (- y2 y1))))
          (ua-num (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3)))))
      (if (< (abs denom) 0.00001)
        nil
        (let ((ua (/ ua-num denom)))
          (complex (+ x1 (* ua (- x2 x1)))
                   (+ y1 (* ua (- y2 y1)))))))))
            
;;;-----------------------------------------------------------------
;;;need better name
(defun DISMEMBER (&rest list-of-complex)
  (let ((result nil))
    (loop for c in list-of-complex
        do
          (push (realpart c) result)
          (push (imagpart c) result))
    (reverse result)))
         


;;;------------------------------------------------------------
;;;a1 and b1 are points on unit circle
;;;find tangent at a1 and b1, which are perpendicular to a1 and b1
;;;returns center and radius
(defmethod CIRCLE ((a1 complex) (b1 complex))
  (let* ((a2 (+ a1 (complex (- (imagpart a1)) (realpart a1))))
         (b2 (+ b1 (complex (- (imagpart b1)) (realpart b1))))
         (center (intersect a1 a2 b1 b2))
         (radius (when center (abs (- a1 center)))))
    (if center        
      (values center radius)
      (values nil nil))))

;;;------------------------------------------------------------
;;;a1 and b1 are points on circle
;;;find tangent at a1 and b1, which are perpendicular to a1 and b1
;;;returns center and radius
(defmethod CIRCLE-2 ((circ-center complex) (a1 complex) (b1 complex))
  (let* ((a1-c (- a1 circ-center))
         (b1-c (- b1 circ-center))
         (a2 (+ a1-c (complex (- (imagpart a1-c)) (realpart a1-c))))
         (b2 (+ b1-c (complex (- (imagpart b1-c)) (realpart b1-c))))
         (center (+ circ-center (intersect a1-c a2 b1-c b2)))
         (radius (abs (- a1 center))))
    ;;(print (list a1 a1-c center radius))
    (values center radius)))




;;;-------------------------------------------------------------
;;; given euclidean distance from origin edist, determine
;;; hyperbolic distance from origin
(defun E-H-DIST (edist)
  (log (/ (+ 1.0 edist) (- 1.0 edist))))

;;;-------------------------------------------------------------
;;; given euclidean ez, determine hyperbolic distance
;;; from origin
(defun H-DIST0 (ez)
  (e-h-dist (abs ez)))

;;;-------------------------------------------------------------
;;; hyperbolic distance from ea to ez, ea and ez euclidean
;;; points
(defun H-DIST (ea ez)
  (let ((az-1 (abs (- (* (conjugate ea) ez) #c(1.0 0.0))))
        (z-a  (abs (- ez ea))))
    ;;(print (/ (+ az-1 z-a) (- az-1 z-a)))
    (log (/ (+ az-1 z-a) (- az-1 z-a)))))

;;;-------------------------------------------------------------
;;; given hyperbolic distance from origin hdist, determine
;;; euclidean distance from origin
(defun H-E-DIST (hdist)
  (let* ((de (exp hdist))
         (d (/ (- de 1.0) (+ de 1.0))))
    d))


;;; given hyperbolic hz, determine euclidean distance
;;; from origin
(defun E-DIST0 (hz)
  (h-e-dist (abs hz)))

;;;--------------------------------------------------------------
(defun E-H (ez)
  (let ((z (* (cis (phase ez)) (e-h-dist (abs ez)))))
    (complex (realpart z) 
             (imagpart z))))

;;;--------------------------------------------------------------
(defun H-E-D (hz)
  (let ((z (* (cis (phase hz)) (h-e-dist (abs hz)))))
    z))
;;;--------------------------------------------------------------
(defun E-H-D (ez)
  (let ((z (* (cis (phase ez)) (e-h-dist (abs ez)))))
    z))

;;;--------------------------------------------------------------
(defun H-E (hz)
  (let ((z (* (cis (phase hz)) (h-e-dist (abs hz)))))
    (complex (realpart z) 
             (imagpart z))))


;;;--------------------------------------------------------------
(defun COORDS-FROM-COMPLEX (z)
  (list (realpart z) (imagpart z) 0.0d0))

;;;--------------------------------------------------------------
(defun xxxwire ()
  (let ((points (loop for hd from -17.0 to 17.0 by 0.1
                    for edist = (h-e-dist hd)
                    while (< edist 0.98)
                    collect (list 0.0 edist))))
    (print points)
    (print (length points))))
;;    (add-object-to-view (3d-i::make-wire-from-coordinate-alist points))))
          

;;;-------------------------------------------------------------
;;;
;;(defun GET-RADIUS (circ)
;;  (using-fdoublet ((xbounds) nil)
    ;;(3d-i::find-bounds-on-axis circ 'x xbounds)       
;;    (/ (- (fdoublet-v xbounds) (fdoublet-u xbounds)) 2.0)))



;;;-----------------------------------------------------------------------
;;; returns direction and centerpoint
(defun PERPENDICULAR-BISECTOR (a b)
  (let* ((ab-dif    (- a b))
         (ab-center (/ (+ a b) 2.0))
         (ab-perp   (+ ab-center 
                       (complex (- (imagpart ab-dif)) (realpart ab-dif)))))
    (values ab-center ab-perp)))

;;;-------------------------------------------------------------------------
;;(defun TANGENT-POINTS (
;;;-------------------------------------------------------------------------
;;;circle-through-point-and-point-on-circumference
;;(defun CIRCLE-THROUGH-POINTS (center point-on-circumference)
;;  (let ((chord (- center point-on-circumference))
        

;;;--------------------------------------------------------------------------
(defun CIRCLE-THROUGH-3-POINTS (a b c)
  (let* ((ab-dif    (- a b))
         (ab-center (/ (+ a b) 2.0))
         (ab-perp   (+ ab-center 
                       (complex (- (imagpart ab-dif)) (realpart ab-dif))))
         (bc-dif      (- b c))
         (bc-center (/ (+ c b) 2.0))
         (bc-perp   (+ bc-center 
                       (complex (- (imagpart bc-dif)) (realpart bc-dif))))
         (center    (intersect ab-center ab-perp bc-center bc-perp)))
    (if center
         (values center (abs (- a center))) ;return center and radius
	 (values a 0.0001)))) ;return position and small radius 

;;;-----------------------------------------------------------------------------
;;;intersection points of two circles
;;;http://stackoverflow.com/questions/3349125/circle-circle-intersection-points
;;;dif-vec - vector between circle centers
;;;a  - length of line from p0 to line between intersect points
;;;p2 - point of intersection on line between intersect points
;;;h  - half the distance between intersect points
;;; return intersections with the smallest magnitude first
;;;
(defmethod CIRCLE-CIRCLE ((pos0 complex) r0 (pos1 complex) r1)
  (let* ((dif-vec (- pos1 pos0))
         (d       (abs dif-vec)))
    (when (and (< d (+ r0 r1)) (> d (abs (- r0 r1))))
      (let* ((a       (/ (+ (* r0 r0) (- (* r1 r1)) (* d d)) (* 2.0 d )))
             (p2      (+ pos0 (* (/ a d) dif-vec)))
             (h       (sqrt (- (* r0 r0) (* a a))))
             (vert    (* (/ h d) (complex (- (imagpart dif-vec)) (realpart dif-vec))))
             (is0     (- p2 vert)) ;; intersection 0
             (is1     (+ p2 vert)));; intersection 1
        (if (> (abs is0) (abs is1)) 
          (rotatef is0 is1))
        ;;(print (list d a p2 h vert))
        ;;(make-wire-complex (list (+ p2 vert) (- p2 vert)))
        ;;(print (abs (+ p2 vert)))
        (values is0 is1)))))

;;;---------------------------------------------------------------------------
;;;line circle intersection  http://stackoverflow.com/questions/1073336/circle-line-collision-detection
(defmethod LINE-CIRCLE ((circle-pos complex) circle-r (line-pos complex) (line-dir complex))
  (let* ((center-to-pos (- line-pos circle-pos))
         (a 1.0)
         (b (* 2.0 (dot-product line-dir center-to-pos)))
         (c (-(dot-product center-to-pos center-to-pos) (* circle-r circle-r)))
         (discrim (- (* b b) (* 4 a c))))
    (if (>= discrim 0.0)
      (let* ((sqrt-discrim (sqrt discrim))
             (t1 (/ (- (- b) sqrt-discrim) (* 2.0 a)))
             (t2 (/ (+ (- b) sqrt-discrim) (* 2.0 a)))
             (i0 (+ line-pos (* t1 line-dir)))
             (i1 (+ line-pos (* t2 line-dir))))
        (when (> (abs i0) 1.0)
          (rotatef i0 i1))
        ;;(print (list i0 i1))
        (values i0 i1))
      ;;no intersection
      (progn
        ;;(print discrim)
        (values nil nil)))))
         
    

;;;---------------------------------------------------------------------------
;;;angle between circles --in euclidean
;;;this calculates unnescesary stuff.
;;;
(defmethod ANGLE-BETWEEN-CIRCLES ((pos0 complex) r0 (pos1 complex) r1)
  ;;(print (list pos0 r0 pos1 r1))
  (let ((intersection-point (circle-circle pos0 r0 pos1 r1)))
    (when intersection-point
      (let* ((circ0-dir (- pos0 intersection-point))
             (circ1-dir (- pos1 intersection-point))
             (tan0 (complex (- (imagpart circ0-dir)) (realpart circ0-dir)))
             (tan1 (complex (- (imagpart circ1-dir)) (realpart circ1-dir))))
        ;;(print (list 'using-radius (angle-between circ0-dir circ1-dir)))
        ;;(print (list 'using-tan (angle-between tan0 tan1)))
        (angle-between tan0 tan1)))))

#|
(defmethod H-CENTER ((center complex) r)
  (let* ((line-dir (abs center))
	 (perp (complex (- imagpart line-dir) (realpart line-dir)))
	 (intersect (line-circle center r center perp)))
    |#   
	
  
