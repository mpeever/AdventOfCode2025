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

(defstruct range 
  (start 0 :type number)
  (end 0 :type number))


(defun uniq (list)
  "Return a unique list built from list. This definitely isn't the best implementation."
  (let ((ulist '()))
    (loop for element in list
	  do (pushnew element ulist))
    ulist))

(defun nth-string (string n)
  "Find the length of a string 1/n the length of string."
  (let ((strlen (length string)))
    (if (zerop (rem strlen n))
	(/ strlen n)
	'())))

(defun strstr (substr n)
  "Repeat a substring n times."
  (if (< n 2)
      substr
      (let ((strs (loop for i from 1 to n
			 collecting substr)))
	 (reduce #'(lambda (a b) (concatenate 'string a b)) strs))))

(defun int-to-str (n)
  "Convert an integer into a string."
  (format nil "~D" n))


(defun count-up (start end length &optional n (acc '()) (debug T))
  "Generate integers made of repeated integers of length LENGTH between START and END."
  (let* ((startstr (int-to-str start))
	 (repeat (nth-string startstr length)))
    (cond ((null n) (count-up start end length (parse-integer (subseq (int-to-str start)
								      0
								      (min length (length startstr))))
			      acc))
	  ((null repeat) acc)
	  ((zerop repeat) acc)
	  (T (let* ((n-str (strstr (int-to-str n) repeat))
		    (n-val (parse-integer n-str))
		    (accumulator (if (>= n-val start) (cons n-val acc) acc)))
	       (when debug
		 (format T "start: ~S; end: ~S; n: ~S; repeat: ~S;  n-str: ~S; n-val: ~S~%"
			 start end n repeat n-str n-val))
	       (cond ((> n-val end) acc)
		     ((= 1 repeat) acc)
		     (T (count-up start end length (+ n 1) accumulator))))))))

(defun segment-range (range)
  "Break down a range into subranges with equal length."
  (let* ((start (range-start range))
	 (end (range-end range))
	 (startlog (truncate (log start 10)))
	 (endlog (truncate (log end 10))))
    (if (> endlog startlog)
	(let* ((midstart (expt 10 endlog))
	       (midend (- midstart 1)))
	  (alexandria:flatten (list (segment-range (make-range :start start :end midend))
				    (segment-range (make-range :start midstart :end end)))))
	(list range))))


(defun find-n-tuples (start end)
  (let* ((startstr (int-to-str start))
	 (endstr (int-to-str end))
	 (integers (loop for i from 1 to (length endstr)
			 collecting (count-up start end i))))
    (uniq (remove-if #'null (alexandria:flatten integers)))))


(defun puzzle-2-2 (input-line)
  (let* ((rangestrs (cl-ppcre:split "," input-line))
	 (ranges (mapcar #'(lambda (range)
			     (cl-ppcre:register-groups-bind (startstr endstr) ("(\\d+)-(\\d+)" range)
			       (make-range :start (parse-integer startstr)
					   :end (parse-integer endstr))))
			 rangestrs))
	 (segments (mapcar #'segment-range ranges)))
    (format T "Ranges: ~S~%" ranges)
    (reduce #'+
	    (alexandria:flatten (mapcar #'(lambda (range) (find-n-tuples (range-start range)
									 (range-end range)))
					(alexandria:flatten segments))))))

(defvar test-input
  (car (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_2/test_input.txt")))

(defvar puzzle-input
  (car (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_2/input_2_1.txt")))


