;; Advent of Code 2025
;;
;; Puzzle 1 Day 1

;; Require cl-ppcre
#-CL-PPCRE-UNICODE
(progn
  (ql:quickload 'CL-PPCRE)
  (ql:quickload 'CL-PPCRE-UNICODE))

;; Require split-sequence
#-SPLIT-SEQUENCE
(ql:quicklisp 'split-sequence)

(defstruct turn
  (direction 'LEFT :type symbol)
  (clicks 0 :type number))

(defun parse-input-line (input-line)
  "Parse an input line consisting of a direction and a count."
  (cl-ppcre:register-groups-bind (direction clicks) ("^(\\D)(\\d+)$" input-line)
	   (make-turn :direction (cond ((equalp direction "R") 'RIGHT)
				       (T 'LEFT))
		      :clicks (parse-integer clicks))))

;; Test
(parse-input-line "L99") ; => #S(TURN :DIRECTION LEFT :CLICKS 99)

(defun turn-dial (turn &key (start 50)
			 (dial-length 99)
			 (debug '())
			 (on-pass-zero #'(lambda (turn) (if debug (format T "Passes zero!~%"))))
			 (on-zero #'(lambda (turn) (if debug (format T "Lands on zero!~%"))))) 
  "Execute a turn, returning the new location on the dial."
  (format T "Turning dial ~S ~D clicks from ~D~%" (turn-direction turn) (turn-clicks turn) start)
  (labels ((click-dial (n direction start-from)
	     "click the dial one time in a given direction start-from a start"
	     (when debug
	       (format T "Clicking dial one click ~S start-from ~S~%" direction start-from))
	     (cond ((zerop n)
		    ;; no more clicks left, just return our starrt
		    start-from)
		   (T (let ((end (cond ((equalp direction 'RIGHT)
						    (if (equal start-from dial-length)
							0
							(+ 1 start-from)))
				       (T (if (equal start-from 0)
					      dial-length
					      (- start-from 1))))))
			(when (zerop end)
			  (funcall on-pass-zero turn))
			(click-dial (- n 1) direction end))))))
    (let ((end (click-dial (turn-clicks turn) (turn-direction turn) start)))
      (when (zerop end)
	(funcall on-zero turn))
      end)))

  
(defun puzzle-1-1 (input-lines &key (start 50) (dial-length 99))
  "Calculate the number of times a dial of 'length' points to 0, given a start of 'start' and input directions 'input-lines'."
  (let ((turns (mapcar #'parse-input-line input-lines))
	(pass-zero-counter 0)
	(zero-counter 0))
    (values (reduce #'(lambda (start turn)
			(let ((end (turn-dial turn :start start
						   :dial-length dial-length
						   :on-pass-zero #'(lambda (turn)
								     (format T "Passed zero!~%")
								     (incf pass-zero-counter))
						   :on-zero #'(lambda (turn)
								(format T "Hit zero!~%")
								(incf zero-counter)))))
			  (format T "Ends at: ~D~%" end)
			  end))
		    turns :initial-value start)
	    pass-zero-counter
	    zero-counter)))

;; (puzzle-1-1 (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/input-1-1.txt"))    
