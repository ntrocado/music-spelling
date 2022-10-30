(defpackage #:music-spelling
  (:use #:cl)
  (:export #:note
	   #:letter
	   #:accidental
	   #:octave
	   #:rest-p
	   #:make-penalties
	   #:*default-penalties*
	   #:*chord-penalties*
	   #:*default-parsimony*
   	   #:*natural-parsimony*
	   #:make-parsimony-ht-from-notes
	   #:pitch-spell
	   #:pitch-spell-chords

	   #:metric-subdivision
	   #:min-subdivision-quantize
	   #:rhythm-spell))
