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



			
