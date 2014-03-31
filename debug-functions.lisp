;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
#|
;; (defparameter *DEBUG-HL-TO-DRAW* nil)

;; (defparameter fl (first-layer 4 6))
;; (defparameter l1 (do-layer fl))
;; (defparameter l2 (do-layer l1))
;; (defparameter l3 (do-layer l2))

;; (defparameter l4 (do-layer l3))
;; (defun CHECK-LAYERS ()
;;   (file fl "fl46")
;;   (file l1 "l146")
;;   (file l2 "l246")
;;   (file l3 "l346"))
|#

(defparameter fl64 (first-layer 6 4))
(defparameter fl46 (first-layer 4 6))
(defparameter hp0 (first fl64))
(defparameter hp1 (second fl64))
(defparameter hpa (first fl46))
(defparameter hpb (second fl46))


(defun VL (hp)
  (let ((hps  (cons hp (do-layer0 (list hp)))))
  ;;(draw-circles (cons hp (do-layer0 (list hp))))
    (file hps)
    hps))

(defun VADD (hp i num)
  (let ((hps  (cons hp (add-z hp (+ (first-edge hp) i) num))))
    (file hps)
  ;;(draw-circles (cons hp (add-z hp (+ (first-edge hp) i) num)))
    (print (list 'num (+ (first-edge hp) i) 'sense (sense hp)))
    hps))



(defun ROOM-REPORT ()  
  (let* ((str (make-array '(0) :element-type 'base-char
			  :fill-pointer 0 :adjustable t))) 
    (format t "~%")
    (with-output-to-string (s str)    
      (let* ((*standard-output* s)) 
	(room nil)))
    (print (subseq str 0 (position #\newline str)))))

(defun GC-AND-REPORT ()
  (room-report)
  (cl-user::gc :full t) 
  (room-report))

(defun LOOK (hps)
  (loop for hp in hps
     for pts = (get-points hp)
     do
       (loop for p in pts
	    do
	    (when (< (abs p) 0.1)
	      (print p)))))

(defun LOOK0 (hps)
  (loop for hp in hps
     for hls = (h-lines hp)
     do
       (loop for hl across hls
	  for aps = (arc-points-gl hl)
	  do
	    (when (and (swept-angle hl) (= 0.0 (swept-angle hl)))
	      (describe hl))
	    (describe hl)
	    (loop for (a b) on aps by #'cddr
	       while b
	       do
		 (format t "~&~7,4f ~4,4f" a b)))))

(defparameter fhp (make-fundamental-hp 6 4))
(defparameter rhp (make-reflected-hp-x fhp 0))
(defparameter rhp1 (make-reflected-hp-x rhp 1))

(defun TEST-EQUI ()
  (let ((current (copy-h-poly fhp))) ;;(make-reflected-hp-x rhp 0)))
    (draw-h-polys current)
    (multiple-value-bind (hla hlb) 
	(make-translating-h-lines #c(0.0 1.0) 0.2)
   
      (translate-hp current hla hlb)
      (draw-h-polys current)

      (loop for i from 0 below 20
	 for hp = current then (make-reflected-hp-x hp i)
	 do
	   (print (list hp i))
	   (draw-line-reflection hp (mod i 6))))))

(defun TEST-EXPONENTIAL ()
  (multiple-value-bind (hla hlb) 
      (make-translating-h-lines #c(0.0 1.0) 0.2)
    (multiple-value-bind (hl0 hl1) 
	(make-translating-h-lines #c(0.0 1.0) 0.1)
      (print (reflect (reflect #c(0.5 0.0) hla) hlb))
      (let ((a (reflect (reflect #c(0.5 0.0) hl0) hl1)))
	(print (reflect (reflect a hl0) hl1))))))


