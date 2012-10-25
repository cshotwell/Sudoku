#|

Carl shotwell.

This is a brute force solver for a sudoku puzzle. It will produce all possible
solutions for a particular puzzle.  There is much much room for optimization.

HOW TO USE:

define a 9x9 array, as can be seen in the two examples below. Then start
the algorithm at the upper leftmost corner of the array by calling (solve)
like this:

(solve 0 0 testboard#)

for some reason the program must then be run from the command line.

|#



(defvar testboard1 #2a((9 0 3 8 5 0 0 0 0)
		      (0 6 0 0 0 3 9 7 0)
		      (0 1 8 0 6 7 0 0 0)
		      (0 0 9 2 3 8 0 0 0)
		      (6 0 0 0 9 0 0 0 7)
		      (0 0 0 4 7 6 3 0 0)
		      (0 0 0 6 1 0 5 3 0)
		      (0 2 6 3 0 0 0 1 0)
		      (0 0 0 0 4 5 6 0 2)))

(defvar testboard2 #2a((0 0 0 4 0 6 0 2 0)
		       (9 0 0 0 0 7 6 0 0)
		       (6 8 0 2 9 1 0 0 0)
		       (0 0 8 1 0 0 2 7 6)
		       (0 0 7 0 0 0 9 0 0)
		       (2 6 1 0 0 5 4 0 0)
		       (0 0 0 6 2 9 0 3 4)
		       (0 0 9 3 0 0 0 0 1)
		       (0 3 0 7 0 8 0 0 0)))

(defvar testboard3 #2a((0 0 0 1 0 0 0 9 0)
		       (1 0 0 7 0 0 0 6 0)
		       (8 5 4 0 6 3 0 0 0)
		       (0 0 0 0 0 0 0 7 9)
		       (0 0 8 0 0 0 4 0 0)
		       (3 4 0 0 0 0 0 0 0)
		       (0 0 0 6 9 0 7 1 3)
		       (0 8 0 0 0 5 0 0 6)
		       (0 1 0 0 0 7 0 0 0)))

(setf possible-values (vector '1 '2 '3 '4 '5 '6 '7 '8 '9))

(defun next-coordinate (r c board)
"This advances us to the next open space in the board."
  (cond ((not (= (+ 1 c) 9)) (solve r (+ c 1) board))
	((not (= (+ 1 r) 9)) (solve (+ 1 r) 0 board))
	(t (print board)))) 

(defun in-rowp (num r board)
  "Checks if the number is present in the row"
  (loop for i from 0 to 8
	when (= num (aref board r i)) do (return i)))

(defun in-colp (num c board)
  "Checks if the number is present in the col"
  (loop for i from 0 to 8
	when (= num (aref board i c)) do (return i)))

(defun in-sqrp (num r c board)
  "Checks is a number is present in the particular square."
  (let ((squares-row (floor (/ r 3)))
	(squares-col (floor (/ c 3))))
    (let ((cstart (* 3 squares-col))
	  (rstart (* 3 squares-row)))
      (loop for row from rstart to (+ rstart 2) 
	    when (loop for col from cstart to (+ cstart 2) 
		  when (= num (aref board row col)) do (return num))
	    do (return row)))))

(defun solve (r c board)
  (cond ((= 0 (aref board r c))
	 (loop for num across possible-values do
	       (cond ((not (or  (in-rowp num r board)
				(in-colp num c board)
				(in-sqrp num r c board)))
		      (setf (aref board r c) num)
		      (next-coordinate r c board))
		     (t nil)))
	 (setf (aref board r c) 0))
	(t (next-coordinate r c board))))

(solve 0 0 testboard3)
