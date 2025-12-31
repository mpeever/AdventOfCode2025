;; Advent of Code 2025
;;
;; Puzzle 2 Day 3

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


(defun flatten (tree)
  "Save some keystrokes."
  (alexandria:flatten tree))


(defun string-to-list (str)
  "Return a list of elements in a string."
  (labels ((split (str acc)
	     (cond ((zerop (length str)) acc)
		   (T (split (subseq str 1) (cons (subseq str 0 1) acc))))))
    (nreverse (split str '()))))

(defun list-to-string (list)
  "Return a string of all the elements in a list."
  (let ((strings (mapcar #'(lambda (element) (format nil "~D" element)) list)))
    (apply 'concatenate (cons 'string strings))))

(defun list-to-integer (list)
  "Return an integer made of the elements of list."
  (parse-integer (list-to-string list)))

(defun all-except (posn list)
  "Return a with the element in POSN removed."
  (labels ((skip (n list acc)
	     "Skip the nth element of a list, copying the rest into acc."
	     (cond ((equal n posn) (skip (+ 1 n) (cdr list) acc))
		   ((null list) acc)
		   (T (skip (+ 1 n) (cdr list) (cons (car list) acc))))))
    (nreverse (skip 0 list '()))))

(defun possible-integers (list)
  "Given a list of digits, return all the possible integers by skipping one digit."
  (loop for i from 0 to (- (length list) 1)
	collecting (list-to-integer (all-except i list))))

(defun highest-integer (list &optional (length 12))
  "Given list of digits, pick the highest integer of length LENGTH without rearranging order."
  (cond ((stringp list) (highest-integer (string-to-list list) length))
	((integerp list) (highest-integer (string-to-list (format nil "~D" list)) length))
	((equal (length list) length) (list-to-integer list))
	(T (let* ((biggest (reduce  #'max (possible-integers list)))
		  (biggest-string (format nil "~D" biggest)))
	     (highest-integer (string-to-list biggest-string) length)))))

(defun puzzle-3-2 (input &key (debug '()))
  "Figure out which pair gives the highest integer value."
  (let* ((lists (mapcar #'string-to-list input)))
    (reduce #'+
	    (loop for list in lists
		  collecting (highest-integer list 12)))))
    
			
