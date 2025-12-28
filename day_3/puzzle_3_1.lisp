;; Advent of Code 2025
;;
;; Puzzle 1 Day 3

;; Require cl-ppcre
#-CL-PPCRE-UNICODE
(progn
  (ql:quickload 'CL-PPCRE)
  (ql:quickload 'CL-PPCRE-UNICODE))

;; Require alexandria
#-ALEXANDRIA
(ql:quickload 'alexandria)

;; Require split-sequence
#-SPLIT-SEQUENCE
(ql:quickload 'split-sequence)

(defvar test-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_3/test_input.txt"))

(defvar puzzle-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_3/input_3_1.txt"))

(defstruct pair
  (first 0 :type number)
  (second 0 :type number))

(defun strings-to-pair (first second)
  (make-pair :first (parse-integer first)
	     :second (parse-integer second)))

(defun ensure-integer (val)
  (if (integerp val)
      val
      (parse-integer val)))

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

(defun combinations (list)
  "Return all in-order combinations of elements in list."
  (labels ((cbns (list acc)
	     (cond ((null list)  acc)
		   (T (let* ((first (car list))
			     (rest (cdr list))
			     (combos (mapcar #'(lambda (n) (make-pair :first (ensure-integer first)
								      :second (ensure-integer n)))
					     rest)))
			(cbns (cdr list) (cons (nreverse combos) acc)))))))
    (nreverse (flatten (cbns list '())))))

(defun pair-to-int (pair)
  "Return a pair as an integer"
  (let* ((n-str (format nil "~D~D" (pair-first pair) (pair-second pair)))
	 (n-val (parse-integer n-str)))
    (values n-val n-str)))

(defmacro list-max (list)
    `(max ,@list))
				     
(defun puzzle-3-1 (input &key (debug '()))
  "Figure out which pair gives the highest integer value."
  (let* ((lists (mapcar #'string-to-list input)))
    (reduce #'+
	    (loop for list in lists
		  for numbers = (mapcar #'parse-integer list) then (mapcar #'parse-integer list)
		  for pairs = (combinations numbers) then (combinations numbers)
		  for ints = (mapcar #'pair-to-int pairs) then (mapcar #'pair-to-int pairs)
		  do (when debug
		       (format T "pairs: ~S;~%ints: ~S~%" pairs ints))
		  collecting (reduce #'(lambda (a b) (max a b)) ints)))))
    
			
