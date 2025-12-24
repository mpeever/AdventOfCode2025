;; Advent of Code 2025
;;
;; Puzzle 1 Day 2

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


(defun find-pairs (start end)
  "Generate a list of integers between START and END that consist of pairs of integers."
  (labels ((half-string (n)
	     (let* ((str (format nil "~D" n))
		    (strlen (length str))
		    (end (if (evenp strlen)
			     (/ strlen 2)
			     (mod strlen 2))))
	       (subseq str 0 end)))
	   (count-up (n acc)
	     (let* ((n-str (format nil "~D~D" n n))
		    (n-val (parse-integer n-str)))
	       (cond ((> n-val end) acc)
		     (T (count-up (+ n 1) (if (>= n-val start)
					      (cons n-str acc)
					      acc)))))))
    (nreverse (count-up (parse-integer (half-string start)) '()))))      

(defun puzzle-2-1 (input-line)
  (let ((ranges (cl-ppcre:split "," input-line)))
    (format T "Ranges: ~S~%" ranges)
    (reduce #'+
	    (mapcar #'parse-integer
		    (alexandria:flatten
		     (mapcar #'(lambda (range)
				 (cl-ppcre:register-groups-bind (startstr endstr) ("(\\d+)-(\\d+)" range)
				   (let ((start (parse-integer startstr))
					 (end (parse-integer endstr)))
				     (find-pairs start end))))
			     ranges))))))

(defvar test-input
  (car (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_2/test_input.txt")))

(defvar puzzle-input
  (car (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_2/input_2_1.txt")))

