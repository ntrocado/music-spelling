(defpackage #:pitch-spelling
  (:use #:cl)
  (:export #:note
	   #:letter
	   #:accidental
	   #:octave
	   #:rest-p
	   #:pitch-spell
	   #:pitch-spell-chords
	   #:make-penalties
	   #:*natural-parsimony*
	   #:*chord-penalties*
	   #:*default-parsimony*
	   #:make-parsimony-ht-from-notes))

(defpackage #:rhythm-spelling
  (:use #:cl)
  (:import-from #:pitch-spelling rest-p)
  (:export #:metric-subdivision
	   #:min-subdivision-quantize
	   #:rhythm-spell))
