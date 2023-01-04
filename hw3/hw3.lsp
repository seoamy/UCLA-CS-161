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
; The first (left) column is the zeroth column.
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
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
	(cond ((null s) t)
				((atom s)
					(cond ((OR (isKeeper s) (isBox s)) nil)
								(t t))
				)
				(t (AND (goal-test (car s)) (goal-test (cdr s))))
	)
 );end defun

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

; helper function for get-square
; input: State s and row r
; output: a list that represents row r
(defun get-row (S r)
	(cond ((OR (null S) (< r 0)) nil)
		((equal r 0) (car S))
		(t (get-row (cdr S) (- r 1)))
	)
)

; helper function for get-square
; input: row state rs and column c
; output: value at column c of rs
(defun get-value (rs c)
	(cond ((OR (null rs) (< c 0)) 1)
		((equal c 0) (car rs))
		(t (get-value (cdr rs) (- c 1))))
)

; input: State S, row r, column c
; returns integer content of state S at square (r, c)
; if square is out of bounds, return value of a wall (1)
(defun get-square (S r c)
		(get-value (get-row S r) c)
)

; input: row r, column c, value v
; output: row with value v in column c
(defun set-value (r c v)
	(cond ((null r) nil) 
		((equal c 0) (cons v (cdr r))) ; if at correct column, cons value v to rest of row r
		(t (cons (car r) (set-value (cdr r) (- c 1) v))) 	; cons first value of r to column c changed to v 
																											; in the rest of the row
	)
)

; input: State S, row r, column c, square value v
; returns new state S' obtained by setting square (r, c) to value v
(defun set-square (S r c v)
	(cond ((null S) nil)
		((equal r 0) (cons (set-value (car S) c v) (cdr S))) ; if at correct row, set element in that row
		(t (cons (car S) (set-square (cdr S) (- r 1) c v))) ; otherwise cons this row with setsquare called on next rows
	)
)

; input: state of square where keeper currently is
; output: state of square after the keeper moves
;					if the square is a keeper + star, then the resulting state will be star
;					if the square is just a keeper, then the resulting state will be blank
; note: the square the keeper is on cannot be anything else besides star or blank
(defun state-after-keeper-moves (square)
	(cond ((isKeeperStar square) star)
				((isKeeper square) blank)
	)
)

; input: state S, location of keeper at column x row y (x, y)
;				location of neighbor in given direction (nx, ny), location of neighbor 2 spaces away in given direction (nbx, nny)
; output state S' after moving the keeper in given direction, or nil if moving it is not possible
(defun try-move-helper (S x y next-state nx ny nnx nny)
	(let* ((nb (get-square S ny nx)) ; nb: state of neighbor in given direction
		(keeper-removed-state (set-square S y x next-state))) ; keeper-removed-state: state of current location of keeper after it moves one space in given direction
		(cond ((isWall nb) nil) ; if neighbor is a wall, invalid move
			((isBlank nb) (set-square keeper-removed-state ny nx keeper)) ; if neighbor is blank, update it to keeper
			((isStar nb) (set-square keeper-removed-state ny nx keeperstar)) ; if neighbor is a star, update it to keeperstar
			((isBox nb) ; if neighbor is a box, we have to check the box next to the neighbor in given direction
				(let* ((nb2 (get-square S nny nnx))) ; get state of square 2 units in given direction from the keeper
					(cond ((OR (isWall nb2) (isBox nb2) (isBoxStar nb2)) nil) ; if it is a wall, box, or boxstar, it is an invalid move
						  	((isBlank nb2) (set-square (set-square keeper-removed-state ny nx keeper) nny nnx box)) ; if it is blank, move keeper and box in given direction
						  	((isStar nb2) (set-square (set-square keeper-removed-state ny nx keeper) nny nnx boxstar)) ; if it is a star, move keeper and box in given direction
					)
				)
			)
			((isBoxStar nb) ; if neighbor is boxstar, we have to check the box next to the neighbor in given direction
				(let* ((nb2 (get-square S nny nnx))) ; get state of square 2 units away from keeper in given direction
					(cond ((OR (isWall nb2) (isBox nb2) (isBoxStar nb2)) nil) ; if it is a wall, box, or boxstar, it is an invalid move
								((isBlank nb2) (set-square (set-square keeper-removed-state ny nx keeperstar) nny nnx box)) ; if it is blank, move keeper and box in given direction
						  	((isStar nb2) (set-square (set-square keeper-removed-state ny nx keeperstar) nny nnx boxstar)) ; if it is a star, move keeper and box in given direction
					)
				)
			)
		)
	)
)

; input: state S, direction D, coordinate of keeper at (x, y)
; output: returns state after moving keeper in direction D from (x, y) in the current state S
(defun try-move (S D x y)
	(let* ((cur_keeper_state (get-square S y x))
				(next-state (state-after-keeper-moves cur_keeper_state)))
		(cond ((equal D 'LEFT) (try-move-helper S x y next-state (- x 1) y (- x 2) y))
			((equal D 'RIGHT) (try-move-helper S x y next-state (+ x 1) y (+ x 2) y))
			((equal D 'UP) (try-move-helper S x y next-state x (- y 1) x (- y 2)))
			((equal D 'DOWN) (try-move-helper S x y next-state x (+ y 1) x (+ y 2)))
			(t nil)
		)
	)
)

; input: state s of current 
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 	(x (car pos))
	 	(y (cadr pos))
	 	;x and y are now the coordinate of the keeper in s.
	 	(result (list (try-move s 'UP x y) 
		 							(try-move s 'RIGHT x y) 
		 							(try-move s 'DOWN x y)
		 							(try-move s 'LEFT x y))))
    (cleanUpList result))
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; Always return 0
;
(defun h0 (s)
 	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;	check if state is null, return 0
; if state is an atom and is a box, return 1 otherwise 0
; sum up h1 called recursively on all elements of the state
;
; This heuristic is admissiable because we are calculating the number of boxes that are not on a goal.
; In the best case, we will need to make 1 move for every box to be moved onto a goal state
; Therefore, it can never overestimate the cost of finishing the game as it calculates the least cost for the given scenario.
(defun h1 (s)
	(cond ((null s) 0)
		((AND (atom s) (isBox s)) 1)
		((atom s) 0)
		(t (+ (h1 (car s)) (h1 (cdr s))))
	)
 )

; helper function for get-all-boxes
; input: row state rs, row index r, column index c
; output: list of lists of ((r c)) locations of boxes in current row
(defun get-boxes-in-row (rs r c)
	(cond ((null rs) nil)
		((isBox (car rs)) (cons (list r c) (get-boxes-in-row (cdr rs) r (+ c 1))))
		(t (get-boxes-in-row (cdr rs) r (+ c 1)))
	)
)

; input: state s, starting row r
; output: list of lists of ((r c)) locations of boxes in entire state
(defun get-all-boxes (s r)
	(cond ((null s) nil)
				(t (append (get-boxes-in-row (car s) r 0) (get-all-boxes (cdr s) (+ r 1))))
	)
)

; helper function for get-all-stars
; input: row state rs, row index r, column index c
; output: list of lists of ((r c)) locations of stars in current row
(defun get-stars-in-row (rs r c)
	(cond ((null rs) nil)
		((isStar (car rs)) (cons (list r c) (get-stars-in-row (cdr rs) r (+ c 1))))
		(t (get-stars-in-row (cdr rs) r (+ c 1)))
	)
)

; input: state s, starting row r
; output: list of lists of ((r c)) locations of stars in entire state
(defun get-all-stars (s r)
	(cond ((null s) nil)
				(t (append (get-stars-in-row (car s) r 0) (get-all-stars (cdr s) (+ r 1))))
	)
)

; input: two points x1, x2
; output: manhattan distance between x1, x2 
(defun man-dist (x1 x2)
	(cond ((OR (null x1) (null x2)) 0)
		(t (+ (abs (- (car x1) (car x2))) (abs (- (cadr x1) (cadr x2)))))
	)
)

; input: point x, list of points points
; output: the shortest distance between x and one of the points in points
(defun shortest-dist (x points)
	(cond ((OR (null x) (null points)) nil)
				(t (let* ((cur_min (shortest-dist x (cdr points))))
						(cond ((null cur_min) (man-dist x (car points)))
									(t (min (man-dist x (car points)) cur_min))
						)
				))
	)
)

; input: list of box coordinates b, list of stars stars
; output: finds sum of distances from box b to all the shortest stars
(defun add-dist (b stars)
	(cond ((OR (null b) (null stars)) 0)
				(t (let* ((man-dist1 (shortest-dist (car b) stars))
									(man-dist-rest (add-dist (cdr b) stars)))
									(+ man-dist1 man-dist-rest)))
	)
)

; input: state s, row r, column c
; output: boolean of whether box at location c, r is in a corner 
; 				checks for walls up & left, up & right, down & left, down & right of current box
(defun in-corner (s r c)
	(let* ((left (get-square s r (- c 1)))
				(right (get-square s r (+ c 1)))
				(up (get-square s (- r 1) c))
				(down (get-square s (+ r 1) c)))
				(cond ((AND (isWall up) (isWall left)) t)
							((AND (isWall up) (isWall right)) t)
							((AND (isWall down) (isWall left)) t)
							((AND (isWall down) (isWall right)) t)
							(t nil)
				)
	)
)

; inputs: state s, b list of box coordinates
; output: boolean of whether any box in state s is in a 'dead' state where it cannot be moved (surrounded by walls)
(defun is-dead-state (s b)
	(cond ((OR (null s) (null b)) nil)
				((AND (listp b) (in-corner s (caar b) (cadar b))) t)
				(t (is-dead-state s (cdr b)))
	)
)

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; input: state s
; output: admissible heuristic based on dead state 
; 				checks if any boxes are in a dead state and returns a large number
; 				otherwise, uses shortest manhattan distance as a heuristic
(defun h2 (s)
	(let* ((b (get-all-boxes s 0)) 
				(stars (get-all-stars s 0)))
		(cond ((is-dead-state s b) 4900)
					(t (add-dist b stars))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)

(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

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
