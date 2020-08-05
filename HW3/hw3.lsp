;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; Box-In-row (row)
; Go through each element to check whether there is box in current row.
; If current element is a box, then return True.
; Otherwise, go to check next element.
(defun Box-In-row (row)
	(cond ((NULL row) NIL) ; Finish all elements in this row.
		  ((isBox (car row)) t) ; current element is a box, return True.
		  (t (Box-In-row (cdr row))) ; Go to check the next element.
	)
)


; check-box (s)
; Go through each row in the state to check if there is any box inside.
; If there is box in current row, then return True.
; Otherwise, go to check next row.
(defun check-box (s)
	(cond ((NULL s) NIL) ; Finish all rows in the state, so return NIL to represent there is no box inside.
		  ((Box-In-row (car s)) t) ; If there is box in current row, then return true to represent there is box inside.
		  (t (check-box(cdr s))) ; Go to check the next row.
	)
)

; goal-test (s)
; Check whether there is box that not on star in current state.
; If yes, then the goal test fails, so return False.
; If no, then the goal test successes, return True.
(defun goal-test (s)
  (not (check-box s))
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; direction-coordinates (direction)
; Get the move coordinates of the direction
; 0: Up -> (-1, 0)
; 1: Right -> (0, 1)
; 2: Down -> (1, 0)
; 3: Left -> (0, -1)
(defun direction-coordinates (direction)
	(cond ((equal direction 0) (list -1 0)) ; Up
		  ((equal direction 1) (list 0 1)) ; Right
		  ((equal direction 2) (list 1 0)) ; Down
		  ((equal direction 3) (list 0 -1)) ; Left
	)
)

; get-square (s x y)
; Get the content of the element (x, y)
(defun get-square (s x y)
	(let* ( (row_length (list-length s)) ; Get boundary
		    (col_length (list-length (car s))) ; Get boundary
		  )
		  (cond ((< x 0) 1) ; Exceed the boundary, return wall (1)
		  		((< y 0) 1) ; Exceed the boundary, return wall (1)
		  		((>= x row_length) 1) ; Exceed the boundary, return wall (1)
		  		((>= y col_length) 1) ; Exceed the boundary, return wall (1)
		  		(t (car (nthcdr y (car (nthcdr x s))))) ; Get the content of the element (x, y)
		  )
	)
)

; original-get-square (s x y)
; Get the original content of the element after either keeper or box is moved
(defun original-get-square (s x y)
	(cond ((isKeeper (get-square s x y)) blank) ; Keeper -> Blank
		  ((isBox (get-square s x y)) blank) ; Box -> Blank
		  ((isKeeperStar (get-square s x y)) star) ; KeeperStar -> Star
		  ((isBoxStar (get-square s x y)) star) ; BoxStar -> Star
	)
)


; set-square(s x y new_content)
; Put the new_content in the coordinate (x, y) 
(defun set-square(s x y new_content)
	(let* ( (row_length (list-length s)) ; Get boundary
		    (col_length (list-length (car s))) ; Get boundary
		  )
		  (cond ((NULL s) Nil)
		  		(t (append (butlast s (- row_length x))
		  				   (cons (append (butlast (car (nthcdr x s)) (- col_length y))
		  				   				 (cons new_content Nil)
		  				   				 (nthcdr (+ y 1) (car (nthcdr x s)))) Nil)
		  				   (nthcdr (+ x 1) s)
		  		   ) 
		  		)
		  )
	)
)

; Move-box (s x y direction)
; Move the box in (x, y) according to the direction
(defun Move-box (s x y direction)
	(let* ( (move (direction-coordinates direction)) ; Get the moving coordinates
		    (next_x (+ x (car move))) ; next x coordinate
		    (next_y (+ y (cadr move))) ; next y coordinate
		    (next_element (get-square s next_x next_y)) ; Get the content of the next position
		    (original_element (original-get-square s x y)) ; Get the original content of the current position
		  )
		  (cond ((isBlank next_element) (set-square (set-square s next_x next_y box) x y original_element)) ; if next position's content is blank, then just move the box forward
		  		((isStar next_element) (set-square (set-square s next_x next_y boxstar) x y original_element)) ; if next position's content is star, then move the box forward and set the content of new position as boxstar
		  		(t Nil) ; Otherwise, return Nil
		  )
	)
)

; Move-keeper (s x y direction)
; Move the keeper in (x, y) according to the direction
(defun Move-keeper (s x y direction)
	(let* ( (move (direction-coordinates direction)) ; Get the moving coordinates
		    (next_x (+ x (car move))) ; next x coordinate
		    (next_y (+ y (cadr move))) ; next y coordinate
		    (next_element (get-square s next_x next_y)) ; Get the content of the next position
		    (original_element (original-get-square s x y)) ; Get the original content of the current position
		  )

	      (cond ((isBlank next_element) (set-square (set-square s next_x next_y keeper) x y original_element)) ; if next position's content is blank, then just move the keeper forward
	      		((isStar next_element) (set-square (set-square s next_x next_y keeperstar) x y original_element)) ; if next position's content is star, then move the keeper forward and set the content of new position as keeperstar
	      		((isBox next_element) (set-square (set-square (Move-box s next_x next_y direction) next_x next_y keeper) x y original_element)) ; if next position's content is star, then move the keeper forward and move the box
	      		((isBoxStar next_element) (set-square (set-square (Move-box s next_x next_y direction) next_x next_y keeperstar) x y original_element)) ; if next position's content is boxstar, then move the keeper forward and move the box and set the content of new position as keeperstar
	      		(t NIL)
	      )
	)
)


; next-states (s)
; Generate all possible next-states
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (y (car pos))
	 (x (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (Move-keeper s x y 0) (Move-keeper s x y 1) (Move-keeper s x y 2) (Move-keeper s x y 3)))
	 )
    (cleanUpList result);end
   );end let
  );

; (setq s1 '((1 1 1 1 1)
; 	       (1 0 0 4 1)
; 	       (1 0 2 0 1)
; 	       (1 0 3 0 1)
; 	       (1 0 0 0 1)
; 	       (1 1 1 1 1)
; 		  )
; )

; (print (next-states s1))

; (setq s2 '((1 1 1 1 1)
; 	       (1 0 0 4 1)
; 	       (1 0 2 3 1)
; 	       (1 0 0 0 1)
; 	       (1 0 0 0 1)
; 	       (1 1 1 1 1)
; 		  )
; )

; (print (next-states s2))

; (setq s3 '((1 1 1 1 1)
; 	       (1 0 0 6 1)
; 	       (1 0 2 0 1)
; 	       (1 0 0 0 1)
; 	       (1 0 0 0 1)
; 	       (1 1 1 1 1)
; 		  )
; )

; (print (next-states s3))

; (setq s4 '((1 1 1 1 1)
; 	       (1 4 2 0 1)
; 	       (1 0 0 0 1)
; 	       (1 0 0 0 1)
; 	       (1 0 5 3 1)
; 	       (1 1 1 1 1)
; 		  )
; )

; (print (next-states s4))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; count-box-col (col)
; check whether this element is Box or not
(defun count-box-col (col)
	(cond ((isBox col) 1) ; if it's a box, return 1
		  (t 0) ; Otherwise, return 0
	)
)

; count-box-row (row)
; Count the number of boxes in this row
(defun count-box-row (row)
	(cond ((Null row) 0) ; if it's null, return 0
		  (t (+ (count-box-col (car row)) ; check whether the first element is a box or not
		  		(count-box-row (cdr row)) ; Go check the rest elements
		  	 )
		  )
	)
)

; Go through every row to calculate the number of boxes that are not on goal.
; This heuristic function is admissible since we need to spend at least one action to move the box to the goals.
; So the heuristic will never be larger than the true cost.
(defun h1 (s)
	(cond ((Null s) 0) ; if the row is empty, then return 0
		  (t (+ (count-box-row (car s)) ; plus the number of boxes in this row and go to calculate the next row
		  		(h1 (cdr s))
		  	 )
		  )
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; find-position-element (row_now row_idx col_idx obj)
; Find the coordinates of the object in current row_now
(defun find-position-element (row_now row_idx col_idx obj)
	(cond ((Null row_now) Nil)
		  (t (cond ((equal obj (car row_now)) (cons (list row_idx col_idx) (find-position-element (cdr row_now) row_idx (+ col_idx 1) obj)))
		  		   (t (find-position-element (cdr row_now) row_idx (+ col_idx 1) obj)
		  		   )
		  	 )
		  )
	)
)

; find-position (s obj)
; Go through all rows to find the coordinates of the object
(defun find-position (s row_idx obj)
	(cond ((Null s) Nil)
		  (t (append (find-position-element (car s) row_idx 0 obj)
		  			 (find-position (cdr s) (+ row_idx 1) obj)
		  	 )
		  )
	)
)

; distance (pos1 pos2)
; Calculate the distance between the coordinates pos1 and pos
(defun distance (pos1 pos2)
	(+ (abs (- (first pos1) (first pos2)))
	   (abs (- (second pos1) (second pos2)))
	)
)

; Min-cus (a b)
; A customized min function, since Nil will regard as inf here
(defun Min-cus (a b)
	(cond ((Null a) b)
		  ((Null b) a)
		  (t (Min a b))
	)
)

; Max-cus (a b)
; A customized max function, since Nil will regard as -inf here
(defun Max-cus (a b)
	(cond ((Null a) b)
		  ((Null b) a)
		  (t (Max a b))
	)
)

; Min-distance-one-list (pos1 pos_2)
; Find the min distance between the coordinate pos1 with one of the coordinate in the coordinate list pos_2
(defun Min-distance-one-list (pos1 pos_2)
	(cond ((Null pos_2) Nil)
		  (t (Min-cus (distance pos1 (car pos_2)) (Min-distance-one-list pos1 (cdr pos_2)))
		  )
	)
)

; Min-distance-list-list (pos_1 pos_2)
; Find the min distance between one of the  coordinate in the coordinate list pos_1 with one of the coordinate in the coordinate list pos_2
(defun Min-distance-list-list (pos_1 pos_2)
	(cond ((Null pos_1) Nil)
		  (t (cons (Min-distance-one-list (car pos_1) pos_2)
		  		   (Min-distance-list-list (cdr pos_1) pos_2)
		  	 )
		  )
	)
)


; h705315195 (s)
; I use the sum of every boxes' minimal distance to star as the heuristic.
; since this heuristic will be the min number of boxes' moves for current state to become a goal state.
; And this function will be admissible because we will use at least this summed value to win this game.
; Thus, this heuristic will never be larger than the true cost.
(defun h705315195 (s)
	(let* ((box_pos (find-position s 0 box))
           (star_pos (find-position s 0 star))
           (keeperstar_pos (find-position s 0 keeperstar))
           (star_all_pos (append star_pos keeperstar_pos))
           (Min_box_star (Min-distance-list-list box_pos star_all_pos))
           (Min_box_star_sum (apply '+ Min_box_star))
          )
		  (cond ((Null Min_box_star_sum) 0)
		  	    (t Min_box_star_sum
		  	    )
		  )

	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
