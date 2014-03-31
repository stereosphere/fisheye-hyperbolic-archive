;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic) 

(defvar cam "
camera {
  camtoworld transform {
             1             0             0             0
             0             1             0             0
             0             0             1             0
             0             0             3             1

  }
  perspective 1  stereo 0
  fov 40
  frameaspect 1
  focus 3
  near 0.1
  far 100
  bgcolor 0.333333 0.333333 0.333333 4.5559e-41
}
")


(defun CAMERA-SETUP ()
  (window c0 {position 200 599 100 499)
  (backcolor c0 0 0 0)
;;  (bbox-draw no)
  
)

#|
camera {
  camtoworld transform {
             1             0             0             0
             0             1             0             0
             0             0             1             0
             0             0             3             1

  }
  perspective 1  stereo 0
  fov 40
  frameaspect 1
  focus 3
  near 0.1
  far 100
  bgcolor 0.333333 0.333333 0.333333 4.5559e-41
}




<camera> ::=

   [ "camera" ]			(optional keyword)
    [ "{" ]			(opening brace, generally required)
	[ "define" <name> ]

	"<" <filename>
      |
	":" <name>
      |
				(or any number of the following,
				 in any order...)

	"perspective"  {"0" | "1"}		(default 1)
					(otherwise orthographic)

	"stereo"       {"0" | "1"}		(default 0)
					(otherwise mono)

	"worldtocam" <transform>	(see transform syntax above)

	"camtoworld" <transform>
				(no point in specifying both
				 camtoworld and worldtocam; one is
				 constrained to be the inverse of						 the other)

	"halfyfield" <half-linear-Y-field-at-unit-distance>
				(default tan 40/2 degrees)

	"fov"		(angular field-of-view if perspective,
			 linear field-of-view otherwise.
			 Measured in whichever direction is smaller,
			 given the aspect ratio.  When aspect ratio
			 changes -- e.g. when a window is reshaped --
			 "fov" is preserved.)

	"frameaspect" <aspect-ratio>	(X/Y) (default 1.333)

	"near"  <near-clipping-distance>	(default 0.1)
	
	"far"	<far-clipping-distance>		(default 10.0)

	"focus" <focus-distance>		(default 3.0)

	
     [ "}" ]				(matching closebrace)

|#
