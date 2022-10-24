(in-package #:rhythm-spelling)

(defun ends-a-beat-p (end)
  (zerop (nth-value 1 (truncate end))))

(defun rest-of-beat (onset)
  (- 1 (nth-value 1 (truncate onset))))

(defun crosses-beats-p (duration onset)
  (> (+ duration onset)
     (truncate (1+ onset))))

(defun rest-of-bar (onset &optional (beats-per-bar 4))
  (- beats-per-bar (mod onset beats-per-bar)))

(defun crosses-bars-p (duration onset &optional (beats-per-bar 4))
  (> (+ (mod onset beats-per-bar) duration)
     beats-per-bar))

(defun post-half-bar (duration onset beats-per-bar)
  (when (< duration beats-per-bar)
    (let* ((onset-in-bar (mod onset beats-per-bar))
	   (r (- (+ duration onset-in-bar)
		 (/ beats-per-bar 2))))
      (when (and (< onset-in-bar (/ beats-per-bar 2))
		 (plusp r)
		 ;; some simple cases
		 ;; TODO add more?...
		 (not (or (and (= duration 2) (= onset-in-bar 1) (= beats-per-bar 4))
			  (and (= duration 3) (= onset-in-bar 0) (= beats-per-bar 4))
			  (and (= duration 3) (= onset-in-bar 1) (= beats-per-bar 4)))))
	r))))

(defun int-frac (d)
  (let* ((int-part (if (>= d 1)
		       (loop :for i := 1 :then (* i 2)
			     :while (<= i d)
			     :maximize i)
		       (loop :for i := 1 :then (/ i 2)
			     :minimize i
			     :until (<= i d))))
	 (frac-part (- d int-part)))
    (values int-part frac-part)))

(defun metric-subdivision (d)
  (loop :for i from 1
	:for sub := (/ 1 i)
	:minimize sub
	:until (zerop (mod d sub))))

(defun dots (int-part frac-part)
  (when (plusp frac-part)
    (loop :with d := (+ int-part frac-part)
	  :for i := int-part :then (/ i 2)
	  :sum i :into acc
	  :sum 1 :into dots
	  :when (= acc d) :return (1- dots)
	    :while (< acc d))))

(defun ties (p d tie-from beats-per-bar offset)
  (append (rhythm-spell (list p) (list tie-from) beats-per-bar offset)
	  (when (not (rest-p p)) (list :tie))
	  (rhythm-spell (list p) (list (- d tie-from)) beats-per-bar (+ offset tie-from))))

(defun min-subdivision-quantize (d subdivision)
  (loop :for i :by subdivision
	:while (< i d)
	:finally (return (if (< (- i d) (/ subdivision 2))
			     i
			     (- i subdivision)))))

(defun rhythm-spell (pitches durations
		     &optional (beats-per-bar 4) (offset 0) (min-subdivision 1/32))
  (loop :with result
	:for p :in pitches
	:for d :in (mapcar (lambda (x)
			     (min-subdivision-quantize x min-subdivision))
			   durations)
	:for end := d :then (+ end d)
	:for onset := offset :then (+ offset (- end d))
	:do (multiple-value-bind (int-part frac-part)
		(int-frac d)
	      
	      (alexandria:if-let (post-half-bar (post-half-bar d onset beats-per-bar))
		(push (ties p d (- d post-half-bar) beats-per-bar onset)
		      result)

		(cond
		  ;; crosses bars
		  ((crosses-bars-p d onset beats-per-bar)
		   (push (ties p d (rest-of-bar onset) beats-per-bar onset)
			 result))

		  ;; dots
		  ((and (dots int-part frac-part)
			(or (not (rest-p p))
			    (< d 1))
			(not (and (< d 1) (crosses-beats-p d onset))))
		   (push (append (rhythm-spell (list p) (list int-part) beats-per-bar)
				 (make-list (dots int-part frac-part)
					    :initial-element :dot))
			 result))

		  ;; ties across beats
		  ((and (crosses-beats-p d onset)
			(<= (metric-subdivision (nth-value 1 (truncate onset)))
			    (min (/ d 2) 1))
			(not (and (= d 1) (zerop (mod end .5))))
			(not (and (zerop frac-part) (> d 1))))
		   (push (ties p d (rest-of-beat onset) beats-per-bar onset)
			 result))

		  ;; other ties
		  ((plusp frac-part)
		   (push (ties p d int-part beats-per-bar offset)
			 result))

		  ;; print note
		  ((zerop frac-part)
		   (push (list p (/ 4 int-part))
			 result)
		  
		   (t (error "Can't spell ~a." d)))))
	      
	:finally (return (alexandria:flatten (nreverse result)))))
