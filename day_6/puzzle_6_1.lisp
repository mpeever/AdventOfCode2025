;; Advent of Code 2025
;;
;; Puzzle 1 Day 6

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
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_6/test_input.txt"))

(defvar puzzle-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_6/input_6_1.txt"))

(defun cols (text-line)
  "Split a line of text into columns"
  (let ((cols (cl-ppcre::split "\\s+" text-line)))
    (if (string= (car cols) "")
	(cdr cols)
	cols)))

(defun puzzle-6-1 (text-lines &key (debug T))
  "total up columns and apply the operation in the last place"
  (let* ((rows (mapcar #'cols
		       text-lines))
	 (operations (car (last rows)))) ;; last returns a cons, not the last element
    (let ((fns (loop for op in operations
		     for fn = (cond ((string= "+" op) #'+)
				    ((string= "*" op) #'*))
		     collecting fn)))
      (when debug
	(format T "rows: ~S~%operations:~S~%functions: ~S~%" rows operations fns))
      (reduce #'+
	      (loop for col from 0 to (- (length (car rows))
					 1)
		    for tally = (reduce #'(lambda (a b)
					    (funcall (nth col fns) a b))
					(remove-if #'null
						   (mapcar #'(lambda (row)
							       (when (cl-ppcre:scan "\\d+" (nth col row))
								 (parse-integer (nth col row))))
							   rows)))
		    collecting tally)))))
		     
    
  


