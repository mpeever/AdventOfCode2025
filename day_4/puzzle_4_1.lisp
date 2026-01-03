;; Advent of Code 2025
;;
;; Puzzle 1 Day 4

;; Require cl-ppcre
#-CL-PPCRE-UNICODE
(progn
  (ql:quickload 'CL-PPCRE)
  (ql:quickload 'CL-PPCRE-UNICODE))

;; Require alexandria
#-ALEXANDRIA
(ql:quickload 'alexandria)

(defvar test-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_4/test_input.txt"))

(defvar puzzle-input
  (uiop:read-file-lines "/Users/mark/Projects/AdventOfCode2025/day_4/input_4_1.txt"))


(defstruct point
  (x 0 :type number)
  (y 0 :type number))

(defgeneric point-add (point direction &optional steps)
  (:documentation "Step from a point in DIRECTION a set number of STEPS."))

(defmethod point-add ((point point) direction &optional (steps 1))
  (cond ((equal direction 'NORTH) (make-point :x (point-x point)
					      :y (- (point-y point) steps)))
	((equal direction 'SOUTH) (make-point :x (point-x point)
					      :y (+ (point-y point) steps)))
	((equal direction 'EAST) (make-point :x (+ (point-x point) steps)
							   :y (point-y point)))
	((equal direction 'WEST) (make-point :x (- (point-x point) steps)
					     :y (point-y point)))
	;; not quite correct, because Pythagorus, but this seems to be what a map user would think
	((equal direction 'NORTHEAST) (point-add (point-add point 'NORTH steps)
						'EAST
						steps))
	((equal direction 'NORTHWEST) (point-add (point-add point 'NORTH steps)
						'WEST
						steps))
	((equal direction 'SOUTHWEST) (point-add (point-add point 'SOUTH steps)
						'WEST
						steps))
	((equal direction 'SOUTHEAST) (point-add (point-add point 'SOUTH steps)
						'EAST
						steps))))

(defmethod point-add ((point list) direction &optional (steps 1))
  (point-add (make-point :x (car point)
			 :y (cadr point))
	     direction
	     steps))
  
(defstruct grid
  (width 0 :type number)
  (height 0 :type number)
  (array (make-array '(0 0))))

(defgeneric get-grid-value (grid position)
  (:documentation "Get a value from the grid."))
		  
(defmethod get-grid-value ((grid grid) (position point))
  (aref (grid-array grid) (point-y position) (point-x position)))

(defmethod get-grid-value ((grid grid) (position list))
  (aref (grid-array grid) (cadr position) (car position)))


(defgeneric set-grid-value (grid position value)
  (:documentation "Set the value of a point in the grid defined by position."))

(defmethod set-grid-value ((grid grid) (position point) value)
  (setf (aref (grid-array grid) (point-y position) (point-x position))
	value))

(defmethod set-grid-value ((grid grid) (position list) value)
  (set-grid-value grid (make-point :y (cadr position)
				   :x (car position))
		  value))

(defgeneric on-grid-p (grid position)
  (:documentation "Tell whether a position is on a grid."))

(defmethod on-grid-p ((grid grid) (position point))
  (and (and (>= (point-x position) 0)
	    (< (point-x position) (grid-width grid)))
       (and (>= (point-y position) 0)
	    (< (point-y position) (grid-height grid)))))

(defmethod on-grid-p ((grid grid) (position sequence))
  (on-grid-p grid (make-point :x (car position)
			      :y (cadr position))))

(defgeneric grid-step (grid point direction &optional steps)
  (:documentation "Find the point finite number of steps from a point on a grid in the specified direction."))

(defmethod grid-step ((grid grid) (point point) direction &optional (steps 1))
  (let ((point1 (point-add point direction steps)))
    (when (on-grid-p grid point1)
      point1)))
  
(defmethod grid-step ((grid grid) (point sequence) direction &optional (steps 1))
  (grid-step grid (make-point :x (car point)
			      :y (cadr point))
	     direction steps))
	

(defun create-grid (text)
  "Generate a Grid from lines of text."
  (let* ((lines (cl-ppcre:split "\\n" text))
	 (width (length (car lines)))
	 (height (length lines))
	 (content (loop for y from 0 to (- height 1)
			for row = (nth y lines)
			collecting (string-to-list row))))
    (make-grid :width width
	       :height height
	       :array (make-array (list height width)
				  :initial-contents content))))
    
(defconstant all-directions (list 'NORTH
				  'NORTHEAST
				  'EAST
				  'SOUTHEAST
				  'SOUTH
				  'SOUTHWEST
				  'WEST
				  'NORTHWEST))

(defun adjacent-points (grid point &key (debug '()))
  "Return the adjacent points to a given position on a grid."
  (loop for direction in all-directions
	for p1 = (grid-step grid point direction)
	when debug
	  do (format T "Point: ~S;~TDirection: ~D~T; P1: ~S~%" point direction p1)
	when (not (null p1))
	  collecting p1))

(defun occupied-point-p (grid point)
  "Check whether a point is occupied on a grid."
  (and (on-grid-p grid point)
       (not (string= (get-grid-value grid point) "."))))

(defun adjacent-rolls (grid point &optional (string "@"))
  "Find points around POINT on GRID which are occupied with STRING."
  (loop for p1 in (adjacent-points grid point)
	when (and (occupied-point-p grid p1)
		  (string= (get-grid-value grid p1) string))
	  collecting p1))

(defstruct decorated-point
  (point (make-point :x 0 :y 0) :type point)
  (meta '() :type list))

(defmethod set-grid-value ((grid grid) (position decorated-point) value)
  (set-grid-value grid (decorated-point-point position) value))
   

(defun all-adjacent-rolls (grid &optional (string "@"))
  "Find the adjacent rolls to every point on the grid."
  (flatten (loop for y  from 0 to (- (grid-height grid) 1)
		 collecting (loop for x from 0 to (- (grid-width grid) 1)
				  for point = (make-point :x x :y y)
				  for ar = (adjacent-rolls grid point string)
				  for dp = (make-decorated-point :point point
								 :meta ar)
				  when (and (string= (get-grid-value grid point) string)
					    (> 4 (length ar)))
				    collecting dp))))

(defun mark-adjacent-rolls (grid &optional (string "@"))
  "Find the adjacent rolls to every point on the grid."
  (loop for point in (all-adjacent-rolls grid string)
	for p0 = (decorated-point-point point)
	do (set-grid-value grid point "x")
	   finally (return grid)))

(defun remove-rolls (grid)
  "Remove all the removable rolls."
  (let ((updated-grid (mark-adjacent-rolls (copy-grid grid))))
    (loop for y from 0 to (- (grid-height updated-grid) 1)
	  with count = 0
	  do (loop for x from 0 to (- (grid-width updated-grid) 1)
		   for point = (make-point :x x :y y)
		   when (string= (get-grid-value updated-grid point) "x")
		     do (progn
			  (incf count)
			  (set-grid-value updated-grid point ".")))
	     finally (return (values updated-grid count)))))


(defun puzzle-4-1 (input)
  (length (all-adjacent-rolls (create-grid input))))

(defun puzzle-4-2 (input &key (debug '()))
  (let* ((grid (create-grid input))
	 (removals (loop for (updated-grid count) = (multiple-value-list (remove-rolls grid))
			 when debug do (format T "~S~%" grid)
			   until (zerop count)
			 collecting count)))
    (values
     removals
     (reduce #'+ removals))))
  
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


			
