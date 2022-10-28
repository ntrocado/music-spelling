(defpackage #:pitch-spelling
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
	   #:pitch-spell-chords))

(defpackage #:rhythm-spelling
  (:use #:cl)
  (:import-from #:pitch-spelling rest-p)
  (:export #:metric-subdivision
	   #:min-subdivision-quantize
	   #:rhythm-spell))
