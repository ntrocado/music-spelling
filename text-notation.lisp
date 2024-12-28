(in-package #:music-spelling)

;;; TODO add lilypond-style absolute and relative octaves

(defun char-pitch->value (pitch)
  "Parse the char PITCH, returning 0 for C, 2 for D, upto 11 for B."
  (case pitch
    (#\c 0)
    (#\d 2)
    (#\e 4)
    (#\f 5)
    (#\g 7)
    (#\a 9)
    (#\b 11)))

(defun text-accidental->value (accidental)
  "Parse the string ACCIDENTAL, returning the value to add or substract to the midi note."
  (alexandria:switch (accidental :test 'string=)
    ("ss" 2)
    ("s+" 1.5)
    ("s" 1)
    ("+" .5)
    ("-" -.5)
    ("b" -1)
    ("b-" -1.5)
    ("bb" -2)
    (t 0)))

(defparameter *default-octave* 4)

(defun text-octave->value (octave &optional (default *default-octave*))
  "Parse the string OCTAVE, counting quotes and commas to return the corresponding base number in 
midi notes."
  (* (if octave
	 (+ default (- (count #\' octave)
		       (count #\, octave)))
	 default)
     12))

(defun find-default-octave (pitches)
  (alexandria:when-let (first-note (find-if #'numberp pitches :from-end t))
    (truncate (/ first-note 12))))

(defparameter *input-regex*
  "^([cdefgabr])(ss|s\\+|s|\\+|-|b|b-|bb)?('+|,+)?(\\d*\\.?\\d*)?$")

(defun parse-to-note (pitch accidental octave previous-notes)
  "Make a note object from args in text notation."
  (if (char= pitch #\r)
      'rest
      (make-instance
       'note
       :letter pitch
       :accidental (alexandria:switch (accidental :test 'string=)
				      ("bb" :double-flat)
				      ("b" :flat)
				      ("s" :sharp)
				      ("ss" :double-sharp)
				      (t :natural))
       :octave (/ (text-octave->value octave
				      (octave (or (find-if (lambda (x)
							     (eql (type-of x) 'note))
							   previous-notes
							   :from-end t)
						  (make-instance 'note))))
		  12))))

(define-condition malformed-input-error (error)
  ((input :initarg :input :reader input)))

(defun parse-text (input &key (return-notes nil))
  "Parse INPUT text into a list of midi note values and a list of durations as fractions/multiples
of a beat, or, with RETURN-NOTES, return a list of note objects.

The syntax is as follows:

- Notes are separated by spaces.
- Each note starts with a pitch letter (a-g), or r for a rest.
- Add s for sharp, b for flat (eg, cs is C-sharp, bb is B-flat).
- Add one or more quotes (') to indicate octaves above middle C, commas (,) indicate octaves
  below middle C. For example: c' is the C above middle C (C5, as it is also usually called)
  ab,, is the A-flat two octaves below middle C (A3).
- Add the rhythmic value in number of beats. Examples: c1 is a quarter-note C; d.5 is an
  eighth-note D (.5=half a beat); e1.75 is a double-dotted
  quarter-note E, or a quarter-note tied to a dotted eight-note… (the exact notation is set
  by the computer); fs4 is a whole-note F-sharp.
- One more thing about octaves: they are all relative to the first note. If you type
  g,,1 a1 b1 c1' the first three notes are two octaves below middle-C, and the last one one
  octave below (G3 A3 B3 C4).
- One more thing about rhythms: If you don't indicate a rhythmic value, the preceding one is
  assumed. So you could write the last example like this: g,,1 a b c'.

Some more examples of valid phrases:

f1 r.5 f'1 d'.5 e'1 (Moose The Mooche)
r.5 g g g eb2 r.5 f f f d2 (Beethoven's fifth)
d2 a f d cs d1 e f2.5 g.5 f e d1 (Bach's Art of the Fugue)
cs'.5 e gs b as1.5 gs.5 fs ds b,1 (Coltrane's solo on Giant Steps)
d.5 e f g e1 c.5 d1.5 (The Lick)"
  
  (loop :with midi
	:with durations
	:for note :in (ppcre:split "\\s" input)
	:for first-octave := (or first-octave (find-default-octave midi))
	:do (or (ppcre:register-groups-bind (pitch accidental octave dur)
		    (*input-regex* note)
		  (push (alexandria:when-let (ch (character pitch))
			  (if return-notes
			      (parse-to-note ch accidental octave midi)
			      (if (char= ch #\r)
				  'rest
				  (+ (char-pitch->value ch)
				     (text-accidental->value accidental)
				     (text-octave->value octave
							 (or first-octave
							     *default-octave*))))))
			midi)
		  (push (or (parse-float:parse-float dur :junk-allowed t)
			    (or (first durations) 1))
			durations))
		(error 'malformed-input-error :input note))
	:finally (return (values (reverse midi) (reverse durations)))))
