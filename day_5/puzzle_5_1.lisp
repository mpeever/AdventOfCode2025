;; Advent of Code 2025
;;
;; Puzzle 1 Day 5

;; Require cl-ppcre
#-CL-PPCRE-UNICODE
(progn
  (ql:quickload 'CL-PPCRE)
  (ql:quickload 'CL-PPCRE-UNICODE))

;; Require alexandria
#-ALEXANDRIA
(ql:quickload 'alexandria)


(defun flatten (tree)
  "Save some keystrokes."
  (alexandria:flatten tree))

(defun string-to-list (str)
  "Return a list of elements in a string."
  (labels ((split (str acc)
	     (cond ((zerop (length str)) acc)
		   (T (split (subseq str 1) (cons (subseq str 0 1) acc))))))
    (nreverse (split str '()))))

(defun all-except (posn list)
  "Return a with the element in POSN removed."
  (labels ((skip (n list acc)
	     "Skip the nth element of a list, copying the rest into acc."
	     (cond ((equal n posn) (skip (+ 1 n) (cdr list) acc))
		   ((null list) acc)
		   (T (skip (+ 1 n) (cdr list) (cons (car list) acc))))))
    (nreverse (skip 0 list '()))))

(defmacro list-max (list)
    `(max ,@list))


(defvar test-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_5/test_input.txt"))

(defvar puzzle-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_5/input_5_1.txt"))

(defstruct range
  (min 0 :type number)
  (max 0 :type number))


(defun text-to-range (str)
  "Consume a string and produce a range."
  (let ((grps (cl-ppcre:split "-" str)))
    (make-range :min (parse-integer (car grps))
		:max (parse-integer (cadr grps)))))


(defgeneric in-range? (range id)
  (:documentation "Test whether ID is in RANGE"))

(defmethod in-range? ((range range) (id number))
  (and (>= id (range-min range))
       (<= id (range-max range))))

(defmethod in-range? ((range range) (id string))
  (in-range? range (parse-integer id)))

(defun range-size (range)
  (+ 1 (- (range-max range)
	  (range-min range))))


(defun puzzle-5-1 (text-lines &key (debug '()))
  (labels ((db (lines acc)
	     (when debug
	       (format T "lines: ~S~%acc: ~S~%" lines acc))
	     (cond ((string= "" (car lines)) (values (cdr lines) acc))
		   (T (db (cdr lines) (cons (text-to-range (car lines)) acc)))))) 
    (multiple-value-bind (ingredient-lines accumulator) (db text-lines '())
      (progn
	(when debug
	  (format T "~S~%~S" ingredient-lines accumulator))
	(let ((fresh (remove-if-not #'(lambda (line)
					(remove-if-not #'(lambda (range)
							   (in-range? range line))
						       accumulator))
				    ingredient-lines)))
	  (values (length fresh)
		  fresh))))))


(defun intersect-p (range1 range2)
  "Check whether two ranges intersect"
  (or (in-range? range1 (range-min range2))
      (in-range? range1 (range-max range2))
      (in-range? range2 (range-min range1))
      (in-range? range2 (range-max range1))))

(defun combine-ranges (range1 range2 &key (debug T))
  (when (intersect-p range1 range2)
    (when debug
      (format T "Combining ~S and ~S~%" range1 range2))
    (make-range :min (min (range-min range1)
			  (range-min range2))
		:max (max (range-max range1)
			  (range-max range2)))))

(defun all-disjunct-p (ranges)
  (if (null ranges)
      T
      (let* ((head (car ranges))
	     (tail (cdr ranges))
	     (intersections (remove-if-not #'(lambda (r)
					       (intersect-p head r))
					   tail)))
	(cond ((null intersections) (all-disjunct tail))
	      (T '())))))

(defun combine-all-ranges (ranges &key (debug T))
  "Combine all the ranges in RANGES that are possible to combine."
  (labels ((cmbn (list acc)
	     (cond ((null list) acc)
		   (T (let ((disjunctions (remove-if #'(lambda (r)
							 (intersect-p (car list) r))
						     (cdr list)))
			    (intersections (remove-if-not #'(lambda (r)
							      (intersect-p (car list) r))
							  (cdr list))))
			(when debug
			  (format T "range: ~S; ~S disjunctions, ~S intersections~%"
				  (car list)
				  (length disjunctions)
				  (length intersections)))
			(cmbn disjunctions
			      (cons (reduce #'combine-ranges
					    intersections
					    :initial-value (car list))
				    acc)))))))
    ;; I HATE HATE HATE that I have to do this, but it seems necessary
    ;; clearly there's a bug in cmbn I haven't found yet
    (loop for input = (cmbn ranges '()) then (cmbn input '())
	  until (all-disjunct-p input)
	  finally (return input))))
	
(defun puzzle-5-2 (text-lines &key (debug T))
  (labels ((db (lines acc)
	     (cond ((string= "" (car lines)) acc)
		   (T (db (cdr lines) (cons (text-to-range (car lines)) acc))))))
    (let ((ranges (combine-all-ranges (db text-lines '()))))
      (when debug
	(format T "ranges: ~S; all disjunct? ~S~%" ranges (all-disjunct ranges)))
      (reduce #'+
	      (mapcar #'range-size
		     ranges)))))
