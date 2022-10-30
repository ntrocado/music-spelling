(in-package :music-spelling)

(defun pitch-letter-p (letter)
  (find letter "abcdefg" :test 'char=))

(deftype pitch-letter ()
  `(and standard-char
	(satisfies pitch-letter-p)))

(defclass note ()
  ((letter
    :accessor letter
    :initarg :letter
    :initform #\c
    :type pitch-letter
    :documentation "One of c, d, e, f, g, a or b")
   (accidental
    :accessor accidental
    :initarg :accidental
    :initform :natural
    :type keyword
    :documentation "Accidentals are :natural, :double-flat, :flat, :sharp, :double-sharp")
   (octave
    :accessor octave
    :initarg :octave
    :initform 4
    :type integer
    :documentation "The octave. Middle C is on the fourth octave.")))

(defmethod print-object ((object note) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((letter letter) (accidental accidental) (octave octave))
	object
      (format stream
	      "~@(~a~)~a~a"
	      letter
	      (ecase accidental
		(:natural "")
		(:flat "b")
		(:sharp "#")
		(:double-sharp "x")
		(:double-flat "bb"))
	      octave))))

(defun rest-p (x)
  (eql x :rest))

(defun letter-value (letter)
  (case letter
    (#\a 5)
    (#\b 6)
    (#\c 0)
    (#\d 1)
    (#\e 2)
    (#\f 3)
    (#\g 4)))

(defun note->midi-note-number (note)
  (with-accessors ((letter letter) (accidental accidental) (octave octave))
      note
    (+ (ecase letter
	 (#\a 9)
	 (#\b 11)
	 (#\c 0)
	 (#\d 2)
	 (#\e 4)
	 (#\f 5)
	 (#\g 7))
       (ecase accidental
	 (:natural 0)
	 (:sharp 1)
	 (:double-sharp 2)
	 (:flat -1)
	 (:double-flat -2))
       (* 12 (1+ octave)))))

;; (defun interval-number (note1 note2)
;;   ;; Compound (> octave) intervals are wrapped
;;   (destructuring-bind (high low)
;;       (sort (list note1 note2) (lambda (a b)
;; 				 (> (note->midi-note-number a)
;; 				    (note->midi-note-number b))))
;;     (1+ (mod (- (letter-value (letter high))
;; 		(letter-value (letter low)))
;; 	     7))))

(defun common-enharmonics-p (note)
  (or (and (char= (letter note) #\c)
	   (eql (accidental note) :flat))
      (and (char= (letter note) #\b)
	   (eql (accidental note) :sharp))
      (and (char= (letter note) #\f)
	   (eql (accidental note) :flat))
      (and (char= (letter note) #\e)
	   (eql (accidental note) :sharp))))

(defun interval-number (note1 note2)
  (1+ (abs (- (+ (* (octave note2) 7) (letter-value (letter note2)))
	      (+ (* (octave note1) 7) (letter-value (letter note1)))))))

(defun interval-in-semitones (note1 note2)
  (- (note->midi-note-number note2)
     (note->midi-note-number note1)))

(defun abs-interval-in-semitones (note1 note2)
  (abs (interval-in-semitones note1 note2)))

(defun major-or-perfect-interval-size (interval-number)
  (nth (mod (1- interval-number) 7) '(0 2 4 5 7 9 11)))

(defun distance-from-major-or-perfect (note1 note2)
  (- (mod (abs-interval-in-semitones note1 note2) 12)
     (major-or-perfect-interval-size (interval-number note1 note2))))

(defun funny-interval-p (note1 note2)
  (when (or (and (eql (accidental note1) :double-flat)
		 (member (accidental note2) '(:sharp :double-sharp)))
	    (and (eql (accidental note1) :double-sharp)
		 (member (accidental note2) '(:flat :double-flat))))
    :other))

(let ((previous-results (make-hash-table :test 'equal)))
  (defun interval-quality (note1 note2)
    (or (gethash (list note1 note2) previous-results)
	(setf (gethash (list note1 note2) previous-results)
	      (or (funny-interval-p note1 note2)
		  (let ((distance (distance-from-major-or-perfect note1 note2)))
		    (if (member (mod (interval-number note1 note2) 7) '(1 4 5))
			(case distance
			  (-1 :diminished)
			  (0  :perfect)
			  (+1 :augmented))
			;; 2 3 6 7
			(case distance
			  (-2 :diminished)
			  (-1 :minor)
			  (0  :major)
			  (+1 :augmented))))
		  :other)))))

(defun diminished-interval-p (note1 note2)
  (eql (interval-quality note1 note2)
	 :diminished))

(defun augmented-interval-p (note1 note2)
  (eql (interval-quality note1 note2)
       :augmented))

;; 60 b# c dbb
;; 61 (bx) c# db
;; 62 cx d ebb
;; 63 d# eb (fbb)
;; 64 dx e fb
;; 65 e# f gbb
;; 66 (ex) f# gb
;; 67 fx g abb
;; 68 g# ab (bbbb)
;; 69 gx a bb
;; 70 a# bb (cbb)
;; 71 ax b cb

(defun possible-spellings (midi-note-number)
  (if (rest-p midi-note-number)
      (list :rest)
      (let ((octave (1- (floor (/ midi-note-number 12)))))
	(flet ((make-note (letter accidental)
		 (make-instance 'note :letter letter :octave octave :accidental accidental)))
	  (case (mod midi-note-number 12)
	    (0 (list (make-note #\c :natural)
		     (make-note #\b :sharp)
		     (make-note #\d :double-flat)))
	    (1 (list (make-note #\c :sharp)
		     (make-note #\d :flat)))
	    (2 (list (make-note #\d :natural)
		     (make-note #\c :double-sharp)
		     (make-note #\e :double-flat)))
	    (3 (list (make-note #\e :flat)
		     (make-note #\d :sharp)))
	    (4 (list (make-note #\e :natural)
		     (make-note #\f :flat)
		     (make-note #\d :double-sharp)))
	    (5 (list (make-note #\f :natural)
		     (make-note #\e :sharp)
		     (make-note #\g :double-flat)))
	    (6 (list (make-note #\f :sharp)
		     (make-note #\g :flat)))
	    (7 (list (make-note #\g :natural)
		     (make-note #\f :double-sharp)
		     (make-note #\a :double-flat)))
	    (8 (list (make-note #\a :flat)
		     (make-note #\g :sharp)))
	    (9 (list (make-note #\a :natural)
		     (make-note #\g :double-sharp)
		     (make-note #\b :double-flat)))
	    (10 (list (make-note #\b :flat)
		      (make-note #\a :sharp)))
	    (11 (list (make-note #\b :natural)
		      (make-note #\c :flat)
		      (make-note #\a :double-sharp))))))))

(defun count-accidentals (notes)
  (count-if-not (lambda (x) (eql (accidental x) :natural)) notes))

(defun count-double-accidentals (notes)
  (count-if (lambda (x) (or (eql (accidental x) :double-flat)
			    (eql (accidental x) :double-sharp)))
	    notes))

(defun count-diminished-intervals (notes)
  (loop :for (a b) :on notes
	:while b
	:when (diminished-interval-p a b)
	  :count :it))

(defun count-augmented-intervals (notes)
  (loop :for (a b) :on notes
	:while b
	:when (augmented-interval-p a b)
	  :count :it))

(defparameter *default-parsimony* (make-hash-table :size 7))

;;; TODO Still needs optimization...
(let ((ht (make-hash-table :size 7)))
  (defun parsimony (note)
    (if (hash-table-p note) ;reset by sending a new hash table
	(setf ht note)
	(if (gethash (letter note) ht)
	    (unless (eql (accidental note) (gethash (letter note) ht))
	      (setf (gethash (letter note) ht) (accidental note)))
	    ;; TODO Yuck... improve this
	    (progn (setf (gethash (letter note) ht) (accidental note))
		   nil)))))

(defun make-parsimony (&rest letter-accidental-pairs)
  "Make a parsimony hash table from LETTER-ACCIDENTAL-PAIRS, each a cons of a `pitch-letter' char and on of :natural, :flat, :sharp, :double-flat or :double-sharp."
  (mapc (lambda (pair) (assert (pitch-letter-p (car pair)))) letter-accidental-pairs)
  (alexandria:alist-hash-table letter-accidental-pairs))

(defun make-parsimony-ht-from-notes (notes)
  "Returns a parsimony hash table produced from the accidentals in the list of `note' objects NOTES."
  (let ((ht (make-hash-table :size 7)))
    (mapc (lambda (note)
	    (setf (gethash (letter note) ht) (accidental note)))
	  notes)
    ht))

(defun make-default-penalties ()
  (let ((ht (make-hash-table)))
    (setf (gethash :accidentals ht) 1.0
	  (gethash :double-accidentals ht) 2.5
	  (gethash :parsimony ht) 1.2
	  (gethash :direction ht) 1.5
	  (gethash :diminished ht) 1.5
	  (gethash :augmented ht) 1.4
	  (gethash :other-intervals ht) 8.0
	  (gethash :e#-fb-b#-cb ht) .4)
    ht))

(defparameter *default-penalties* (make-default-penalties))

(defun make-chord-penalties ()
  (let ((ht (alexandria:copy-hash-table *default-penalties*)))
    (setf (gethash :direction ht) 0.0
	  (gethash :diminished ht) 1.0
	  (gethash :augmented ht) 1.0
	  (gethash :parsimony ht) 1.1)
    ht))

(defparameter *chord-penalties* (make-chord-penalties)
  "Penalties used when spelling chords.")

(defun count-penalties (notes-vec best-score-so-far
			&optional (penalties *default-penalties*)
			  (parsimony-ht *default-parsimony*))
  (declare (optimize speed)
	   (type single-float best-score-so-far)
	   (type (simple-array note) notes-vec))
  
  (let ((accidentals (gethash :accidentals penalties))
	(double-accidentals (gethash :double-accidentals penalties))
	(parsimony (gethash :parsimony penalties))
	(direction (gethash :direction penalties))
	(diminished (gethash :diminished penalties))
	(augmented (gethash :augmented penalties))
	(other-intervals (gethash :other-intervals penalties))
	(e#-fb-b#-cb (gethash :e#-fb-b#-cb penalties)))
    
    (declare (type single-float accidentals double-accidentals parsimony direction diminished augmented other-intervals e#-fb-b#-cb))

    (parsimony (alexandria:copy-hash-table parsimony-ht))
    
    (loop :with penalty :of-type single-float
	  ;; penalties for the first note
	    := (let ((first-note (aref notes-vec 0)))
		 (+ (case (accidental first-note)
		      ((:sharp :flat) accidentals)
		      ((:double-sharp :double-flat) double-accidentals)
		      (t 0s0))
		    (or (when (parsimony first-note) parsimony)
			0s0)
		    (or (when (common-enharmonics-p first-note) e#-fb-b#-cb)
			0s0)))
	  
	  :for i :from 1 :below (length notes-vec)
	  :for a := (the note (aref notes-vec (1- i)))
	  :for b := (aref notes-vec i)
	  :for interval :of-type fixnum := (interval-in-semitones a b)
	  :do (incf penalty
		    (case (accidental b)
		      ((:sharp :flat) accidentals)
		      ((:double-sharp :double-flat) double-accidentals)
		      (t 0s0)))
	  :do (when (parsimony b) (incf penalty parsimony))
	  :do (cond ((or (and (= 1 interval)
			      (and (eql (accidental b) :flat)
				   (not (eql (accidental a) :flat))))
			 (and (= -1 interval)
			      (and (eql (accidental b) :sharp)
				   (not (eql (accidental a) :sharp)))))
		     (incf penalty direction))
		    ((funny-interval-p a b) (incf penalty other-intervals))
		    ((> (abs interval) 1)
		     (incf penalty
			   (case (interval-quality a b)
			     (:diminished diminished)
			     (:augmented augmented)
			     (:other other-intervals)
			     (t 0s0)))))
	  :do (when (common-enharmonics-p b)
		(incf penalty e#-fb-b#-cb))
	  :when (>= penalty best-score-so-far) :do (return penalty)
	    :finally (return penalty))))

;; TODO avoid mixing accidentals (maybe covered by other constrains already)
(defun score-spelling (notes best-score-so-far penalties parsimony)
  (count-penalties (coerce (delete :rest notes) 'vector)
		   best-score-so-far
		   penalties
		   parsimony))

;; TODO could be more efficient with vectors?
(defun map-product (function list &rest more-lists)
  "Non-collecting version of the function in Alexandria."
  (labels ((%map-product (f lists)
             (let ((more (cdr lists))
                   (one (car lists)))
               (if (not more)
                   (mapcar f one)
                   (mapc (lambda (x)
                              (%map-product (alexandria:curry f x) more))
                            one)))))
    (%map-product (alexandria:ensure-function function) (cons list more-lists))))

(defun %pitch-spell (midi-note-numbers penalties parsimony)
  (if (= 1 (length midi-note-numbers))
      (list (first (possible-spellings (car midi-note-numbers))))
      (let ((best-score 1000s0)
	    (best-solution nil))
	(apply #'map-product
	       (lambda (&rest vals)
		 (let ((score (score-spelling vals best-score
					      penalties
					      parsimony)))
		   (when (< score best-score)
		     (setf best-solution vals
			   best-score score))))
	       (mapcar #'possible-spellings midi-note-numbers))
	(values best-solution best-score))))


(defun make-natural-parsimony ()
    (let ((ht (make-hash-table :size 7)))
      (loop :for ch :across "abcdefg" :do (setf (gethash ch ht)	:natural))
      ht))

(defparameter *natural-parsimony* (make-natural-parsimony))


(defun make-penalties (&key accidentals augmented diminished direction double-accidentals
		       e#-fb-b#-cb other-intervals parsimony (default *default-penalties*))
  "Returns a hash table with the following cumulative penalties used in pitch scoring:

ACCIDENTALS: note is sharp or flat;
AUGMENTED: augmented interval between two consecutive notes;
DIMINISHED: diminished interval between two consecutive notes;
DIRECTION: ascending with flats or descending with sharps;
DOUBLE-ACCIDENTALS: note is double sharp or double flat;
E#-Fb-B#-Cb: specific penalty for these spellings;
OTHER-INTERVALS: intervals between two consecutive notes other than major, minor, perfect, augmented or diminished (e.g. doubly augmented interval);
PARSIMONY: occurrence of an accidental different from previous used ones (or pre-defined on a given parsimony table).

Unsupplied values are taken from DEFAULT.

Penalty values are summed when scoring each spelling try. A lower score is better."
  (macrolet ((supplied-or-take-default (val)
	       `(or (when ,val (coerce ,val 'single-float))
		    (gethash (alexandria:make-keyword (quote ,val)) default))))
   (let ((ht (make-hash-table :size 8)))
     (setf (gethash :accidentals ht) (supplied-or-take-default accidentals)
	   (gethash :double-accidentals ht) (supplied-or-take-default double-accidentals) 
	   (gethash :parsimony ht) (supplied-or-take-default parsimony)
	   (gethash :direction ht) (supplied-or-take-default direction)
	   (gethash :diminished ht) (supplied-or-take-default diminished)
	   (gethash :augmented ht) (supplied-or-take-default augmented)
	   (gethash :other-intervals ht) (supplied-or-take-default other-intervals)
	   (gethash :e#-fb-b#-cb ht) (supplied-or-take-default e#-fb-b#-cb))
     ht)))

(defun pitch-spell-chords (chord-seq
			   &key (penalties *chord-penalties*)
			     (parsimony *natural-parsimony*))
  "Spell chords in CHORD-SEQ, a list of chords, where each chord is a list of midi note values, returning a similarly structured list with the values replaced by `note' objects.
PENALTIES is a hash table with the penalty values for various situations, summed when scoring spelling tries (see `make-penalties'). PARSIMONY is a hash table with the chars for the letters from 'a' to 'g' as keys and their preferred initial spelling (:natural, :sharp, :flat, etc.) as values.

Example:
(pitch-spell-chords '((60 64 67) (61 65 68) (62 66 69)))

--> ((#<NOTE C4> #<NOTE E4> #<NOTE G4>) (#<NOTE Db4> #<NOTE F4> #<NOTE Ab4>)
 (#<NOTE D4> #<NOTE F#4> #<NOTE A4>))"
  
  (mapcar (alexandria:rcurry #'%pitch-spell
			     penalties
			     parsimony)
	  chord-seq))

(defun pitch-spell (midi-note-numbers
		    &key (split 9)
		      (penalties *default-penalties*)
		      (parsimony *default-parsimony*))
  "Spell notes in MIDI-NOTE-NUMBERS, a list of midi note values, Returning a list of `note' objects.

For longer sequences, computing the n-fold cartesian product of all spelling possibilites becomes dramatically time-intensive. Therefore, the sequence is split in blocks of SPLIT elements, which are calculated separately. However, maintaining previous choices of accidentals across blocks is preferred (divergences receive the 'parsimony' penalty). PENALTIES is a hash table with the penalties for various situations, summed when scoring spelling tries (see `make-penalties'). PARSIMONY is a hash table with the chars for the letters from 'a' to 'g' as keys and their preferred initial spelling (:natural, :sharp, :flat, etc.) as values.

Example:
(pitch-spell '(48 50 52 53 55 57 59 60 61 64 65 68 69))

--> (#<NOTE C3> #<NOTE D3> #<NOTE E3> #<NOTE F3> #<NOTE G3> #<NOTE A3> #<NOTE B3>
 #<NOTE C4> #<NOTE C#4> #<NOTE E4> #<NOTE F4> #<NOTE Ab4> #<NOTE A4>)"

 (cond ((null midi-note-numbers) nil)
	((< (length midi-note-numbers) split)
	 (%pitch-spell midi-note-numbers penalties parsimony))
	(t (loop :for i :below (length midi-note-numbers) :by split
		 :for split-seq := (subseq midi-note-numbers
					   i (min (+ i split)
						  (length midi-note-numbers)))
		 :for parsimony-ht := parsimony
		   :then (make-parsimony-ht-from-notes notes)
		 :for (notes score) := (multiple-value-list (%pitch-spell split-seq
									  penalties
									  parsimony-ht))
		 :nconc notes :into result-notes
		 :sum score :into result-score
		 :finally (return (values result-notes result-score))))))
