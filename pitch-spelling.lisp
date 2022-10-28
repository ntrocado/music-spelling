(in-package :pitch-spelling)

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

(let ((previous-results (make-hash-table :test 'equal)))
  (defun interval-quality (note1 note2)
    (or (gethash (list note1 note2) previous-results)
	(setf (gethash (list note1 note2) previous-results)
	      (or (let ((distance (distance-from-major-or-perfect note1 note2)))
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
  (defun parsimony (note &optional new-ht)
    (if new-ht
	(setf ht new-ht)
	(if (gethash (letter note) ht)
	    (unless (eql (accidental note) (gethash (letter note) ht))
	      (setf (gethash (letter note) ht) (accidental note)))
	    ;; TODO Yuck... improve this
	    (progn (setf (gethash (letter note) ht) (accidental note))
		   nil)))))

(defun make-parsimony-ht (notes)
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
	  (gethash :direction ht) 1.6
	  (gethash :diminished ht) 1.5
	  (gethash :augmented ht) 1.4
	  (gethash :other-intervals ht) 8.0
	  (gethash :e#-fb-b#-cb ht) .5)
    ht))

(defparameter *default-penalties* (make-default-penalties))

(defun make-chord-penalties ()
  (let ((ht (alexandria:copy-hash-table *default-penalties*)))
    (setf (gethash :direction ht) 0.0
	  (gethash :diminished ht) 1.0
	  (gethash :augmented ht) 1.0
	  (gethash :penalties ht) 1.1)
    ht))

(defparameter *chord-penalties* (make-chord-penalties)
  "Penalties used when spelling chords.")

(defun count-penalties (notes-vec best-score-so-far
			&optional (penalties *default-penalties*)
			  (parsimony-ht *default-parsimony*))
  (declare (optimize speed)
	   (type single-float best-score-so-far))
  
  (let ((accidentals (gethash :accidentals penalties))
	(double-accidentals (gethash :double-accidentals penalties))
	(parsimony (gethash :parsimony penalties))
	(direction (gethash :direction penalties))
	(diminished (gethash :diminished penalties))
	(augmented (gethash :augmented penalties))
	(other-intervals (gethash :other-intervals penalties))
	(e#-fb-b#-cb (gethash :e#-fb-b#-cb penalties)))
    
    (declare (type single-float accidentals double-accidentals parsimony direction diminished augmented other-intervals e#-fb-b#-cb))
    
    (loop :initially (parsimony (aref notes-vec 0)
				(alexandria:copy-hash-table parsimony-ht))
	  :with penalty :of-type single-float
	  ;; penalties for the first note
	    := (+ (case (accidental (aref notes-vec 0))
		    ((:sharp :flat) accidentals)
		    ((:double-sharp :double-flat) double-accidentals)
		    (t 0s0))
		  (or (when (parsimony (aref notes-vec 0)) parsimony)
		      0s0))
	  
	  :for i :from 1 :below (length notes-vec)
	  :for a := (aref notes-vec (1- i))
	  :for b := (aref notes-vec i)
	  :for interval :of-type fixnum := (interval-in-semitones a b)
	  :do (incf penalty
		    (case (accidental b)
		      ((:sharp :flat) accidentals)
		      ((:double-sharp :double-flat) double-accidentals)
		      (t 0)))
	  :do (when (parsimony b) (incf penalty parsimony))
	  :do (cond ((or (and (= 1 interval)
			      (eql (accidental b) :flat))
			 (and (= -1 interval)
			      (eql (accidental b) :sharp)))
		     (incf penalty direction))
		    ((> (abs interval) 1)
		     (incf penalty
			   (case (interval-quality a b)
			     (:diminished diminished)
			     (:augmented augmented)
			     (:other other-intervals)
			     (t 0)))))
	  :do (when (or (and (char= (letter b) #\c)
			     (eql (accidental b) :flat))
			(and (char= (letter b) #\b)
			     (eql (accidental b) :sharp))
			(and (char= (letter b) #\f)
			     (eql (accidental b) :flat))
			(and (char= (letter b) #\e)
			     (eql (accidental b) :sharp)))
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

(defun %pitch-spell (midi-note-numbers
		    &optional (penalties *default-penalties*)
		      (parsimony *default-parsimony*))
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

(defun pitch-spell (midi-note-numbers
		    &key (split 10)
		      (penalties *default-penalties*)
		      (parsimony *default-parsimony*))
  (cond ((null midi-note-numbers) nil)
	((< (length midi-note-numbers) split)
	 (%pitch-spell midi-note-numbers))
	(t (loop :for i :below (length midi-note-numbers) :by split
		 :for split-seq := (subseq midi-note-numbers
					   i (min (+ i split)
						  (length midi-note-numbers)))
		 :for parsimony-ht := parsimony :then (make-parsimony-ht notes)
		 :for notes := (%pitch-spell split-seq penalties parsimony-ht)
		 :nconc notes))))
