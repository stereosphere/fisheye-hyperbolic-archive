;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)

;;;-----------------------------------------------------------------
(defun CONVERT-TO-PNG (root-name)
  ;;(sb-posix:chdir #p"/home/michael/SVG-FRAMES/Anim0") ;;must be in directory
  (loop for f from 1
     for input  = (format nil "/home/michael/SVG-FRAMES/Anim0/~a_~4,'0d.svg" root-name f)
     for export = (format nil "--export-png=~a_~4,'0d.png" root-name f)
     while (probe-file input)
     do
       (format t "~& ~d ~a ~a" f input export)
       ;;(print input) (print export)))
       (sb-ext:run-program "/usr/bin/inkscape" 
			   (list 
			    "--export-width"  "1024"
			    "--export-height" "1024"
			    export ;;"--export-png=f_0001.png" 
			    input))
       (sb-ext:run-program "/usr/bin/inkscape" (list "--version"))))
  
;; avconv -r 10 -i ~/SVG-FRAMES/Anim0/f_%04d.png -b:v 1000k test.mp4

