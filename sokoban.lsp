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
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Before modification, it always returned NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

;@param s a state representatoin of the gam
;checks if any boxes are left still misplaced. Loops through each element of s
;to see if any are boxes (2, representing a box only without a goal)
;@return nil if a box is still not in a goal, true if all boxes are in goals
(defun goal-test (s)

	(cond
		    ((NULL s) T)
            ((atom s) 
          		; if ANY tile is a box, then we know it is not a goal state
               (cond 
               		((or (isKeeper s) (isBox s)) nil)
                    (t T)
               )
            )
            ;recursively check rest of s if any boxes are left
            (t (and (goal-test (first s)) (goal-test (rest s))))
    )
);end defun



;@param s: game state, row: row index, col: col index
;checks if the element at location (row, col) is within the game board
;@return True if element is outside game board, otherwise false
(defun outside-scope(s row col)
	(cond
		 ( (or 
  			(< col 0) 
  			(< row 0) 
  			(> col (- (length (first s)) 1) )
  			(> row (- (length s) 1)) 
  			
  		   )
		 T
		 )
		 (t nil)

	)
)


;helper function for get-square, returns the element at index in a list l
(defun get-element(l index)
	(cond
		( (NULL l) nil)
		( (= 0 index) (first l))
		( t ( get-element (rest l) (- index 1)) )
	)

)


;@param s: game state, row: row index, col: column index
;searches for location (row, col) and returns type of square
;@return return the value of the square at location (row, col)

(defun get-square (s row col)

	(cond
		( (outside-scope s row col) wall) 
		( (= 0 row) (get-element (first s) col) )
		( t 
			( get-square (rest s)  (- row 1) col ) 
		)
	)

	)


;helper function to change element of list l at index to object
(defun change-element(l index object)
	(cond
		( (NULL l) nil )
		( (= 0 index) 
			(cons object (rest l))  
		) 
		( t 
			(cons 
				(first l) 
				( change-element (rest l) (- index 1) object ) 
			) 
		)
	)
)


;@param s: game state, row: row index, col: column index, object: item to change square to
;searches for location (row, col) and returns type of square
;changes it to object
;@return the state of the game board after the change is made
(defun set-square (s row col object)

	(cond
		( (NULL s) nil )
		( (= 0 row) 
			( cons (change-element (first s) col object) (rest s)  ) 
		)
		( t
			( cons 
				(first s) 
				(set-square (rest s) (- row 1) col object ) 
			)
		)

	)

)






;@param s, a game state representation and delta, a direction to move in
;represented as a list (dx,dy)
;sorry for calling them boulders throughought the function, in Pokemon this puzzle
;was done using boulders
;tries to move in the direction specified by checking the tile the keeper is moving to,
; and the tile the boulder is being moved to if a boulder is being moved
;if possible, then update and return the state of the board
;after the move is made
;@return s, the state of game board after the move

(defun try-move (s delta)
	(let*
		(

			;gets row and column of keeper, keeper's destination, and the boulder that is being moved
			;if moving a boulder
			(pos (getKeeperPosition s 0))
			(keeperCol (first pos))
			(keeperRow (second pos))
			(destKeeperRow (+ keeperRow (second delta)) )
			(destKeeperCol (+ keeperCol (first delta)) )
			(destBoulderRow (+ destKeeperRow (second delta))  )
			(destBoulderCol (+ destKeeperCol  (first delta)) )

			(keeperSquare
				(get-square
					s
					keeperRow
					keeperCol
				)
			)
			(destKeeper   
				(get-square 
						s 
						destKeeperRow 
						destKeeperCol  
				)
			)
			(destBoulder
				(get-square 
						s 
						destBoulderRow   
						destBoulderCol
				)

			)

		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;case manager is moving into a blank space
		(cond
			;destination is a blank

			( (isBlank destKeeper) 

				(cond 
					((isKeeperStar keeperSquare)
						(set-square
							(set-square s keeperRow keeperCol star )
							destKeeperRow 
							destKeeperCol
							keeper
						)
					)
					((isKeeper keeperSquare)
						(set-square
							(set-square s keeperRow keeperCol blank )
							destKeeperRow 
							destKeeperCol
							keeper
						)
					)

				)

			)

			( (isStar destKeeper) 

				(cond 
					((isKeeperStar keeperSquare)
						(set-square
							(set-square s keeperRow keeperCol star )
							destKeeperRow 
							destKeeperCol
							keeperStar
						)
					)
					((isKeeper keeperSquare)
						(set-square
							(set-square s keeperRow keeperCol blank )
							destKeeperRow 
							destKeeperCol
							keeperStar
						)
					)

				)

			)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;case manager is moving a box
			( (isBox destKeeper)

				(cond
					((isBlank destBoulder)

						(cond 
							((isKeeperStar keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol star )
											destKeeperRow 
											destKeeperCol
											keeper
										)
										destBoulderRow
										destBoulderCol
										box
									)
							)
							((isKeeper keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol blank )
											destKeeperRow 
											destKeeperCol
											keeper
										)
										destBoulderRow
										destBoulderCol
										box

									)
							)

						)

					)

					((isStar destBoulder)

						(cond 
							((isKeeperStar keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol star )
											destKeeperRow 
											destKeeperCol
											keeper
										)
										destBoulderRow
										destBoulderCol
										boxStar
									)
							)
							((isKeeper keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol blank )
											destKeeperRow 
											destKeeperCol
											keeper
										)
										destBoulderRow
										destBoulderCol
										boxStar

									)
							)

						)

					)

					((isWall destBoulder) nil)

					((isBox destBoulder) nil)

					((isBoxStar destBoulder) nil)



				)
			)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; case manager is moving a box that was on a goal state
			( (isBoxStar destKeeper)

				(cond
					((isBlank destBoulder)

						(cond 
							((isKeeperStar keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol star )
											destKeeperRow 
											destKeeperCol
											keeperStar
										)
										destBoulderRow
										destBoulderCol
										box
									)
							)
							((isKeeper keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol blank )
											destKeeperRow 
											destKeeperCol
											keeperStar
										)
										destBoulderRow
										destBoulderCol
										box

									)
							)

						)

					)

					((isStar destBoulder)

						(cond 
							((isKeeperStar keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol star )
											destKeeperRow 
											destKeeperCol
											keeperStar
										)
										destBoulderRow
										destBoulderCol
										boxStar
									)
							)
							((isKeeper keeperSquare)

									(set-square
										(set-square
											(set-square s keeperRow keeperCol blank )
											destKeeperRow 
											destKeeperCol
											keeperStar
										)
										destBoulderRow
										destBoulderCol
										boxStar

									)
							)

						)

					)

					((isWall destBoulder) nil)

					((isBox destBoulder) nil)

					((isBoxStar destBoulder) nil)


				)
			)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;case the manager is trynna move into a wall

			( (isWall destKeeper)
					nil
			)



			(t nil)

		)

	)

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



; returns a list of all possible successor states of the game board at state s
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result nil)
	 )
    (cleanUpList   	
    	(list 
    		;moves manager up, left, right, down
	  		( try-move s '(0 -1) ) 
	  		( try-move s '(-1 0) )
	  		( try-move s '(1 0) ) 		
	  		( try-move s '(0 1) ) 	
	  			
	  		
  		)
  	);end
   );end let
  );











; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; always returns 0, so is admissible 
(defun h0 (s)
	0
  )

;helper function to count number of boxes in a row of s
(defun numboxes(row)
	(cond
		((NULL row) 0)
		((isBox (first row)) (+ 1 (numboxes (rest row))) )
		(t (numboxes (rest row)))
	)

)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; @param s a state representation of the game
; check for number of boxes (2, by itself) still remaining. Then we know that
; there must be at least that many steps still remaining to take, as we need one step
; at least to move the box into a goal tile. So, this heuristic is admissible 
; @return number of boxes still misplaced

(defun h1 (s)
	(cond
		((NULL s) 0)
		(t (+ (numboxes (first s)) (h1 (rest s))) )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEURISTIC STUFF;;;;;;

;@param box and goal, two coordinates in list form
;calculates manhattan distance between box and goal
;@return manhattan distance between box and goal

(defun manhattan-distance (box goal)
	(cond	
		((NULL box)	0)	
			((> (first box) (first goal))	
				(+ (- (first box) (first goal)) (manhattan-distance (rest box) (rest goal)))
			) 
			(t	
				(+ (- (first goal) (first box)) (manhattan-distance (rest box) (rest goal)))
			)
	)
)

;@param a list
;sums up the list
;@return sum
(defun sumList (l)
	(cond	
		((NULL l) 0)
		(t	
			(+ (first l) (sumList (rest l)))
		)
	)
)



;standard function takes in a list, returns minimum value in list
(defun listmin (l) 
	(cond	
		((NULL l) nil)
		((NULL (rest l)) (first l))
		((< (first l) (listmin (rest l))) (first l) )
		(t	
			(listmin(rest l)) 
		)
	)
)

;@param lists: a list of lists
;finds the minimum of each list in lists 
;@return a list of minimums of each list in lists
(defun listMins(lists) 
	(cond	
		((NULL lists) nil)
		(t	
			( cons (listmin (first lists)) (listMins (rest lists))  )
		)
	)
)


;@param box a coordinate , goals a list of goal coordinates
;calculates manhattan distance from box to each of the goals
;@return distances from box to each goal in goals
(defun boxDistances (box goals) 
	(cond	
		((NULL goals) nil)
		(t	
			(cons (manhattan-distance box (first goals)) (boxDistances box (rest goals))))
		)
)

;@param boxes, a list of box coordinates  goals, a list of goal coordinates
;calculates manhattan distance from each box to each goal
;returns a list of lists, a list for each box, its distances to each goal

(defun allBoxDistances (boxes goals) ;returns list of lists of number
	(cond	
		((NULL boxes) nil)
		(t	
			(cons (boxDistances (first boxes) goals) (allBoxDistances (rest boxes) goals))
		)
	)
)



;@param col a list of column values, rows a row value
;turns lists into a list of coordinates of (col[i],row)
;@return a list of coordinates with  ( col[i],row )
(defun coord (col row) 
	(cond	
		((NULL col) nil)
		(t	
			(cons (cons (first col) (list row)) (coord (rest col) row))
		)
	)
)

;@param row a  list, col a column to be inspected
;in row, if at index col there is a box, add it to a list
;@return a list of columns where there is a box in row

(defun getBoxCols (row col)
	(cond 
		((NULL row) nil)
		(t 	
			(cond	
				( (isBox (first row)) 
					(cons col (getBoxCols (rest row) (+ 1 col)))
				)

				(t 
					(getBoxCols (rest row) (+ 1 col))
				)
			)
		)
	)
)


;@param s state of game board, row: a row to be examined
;checks row by row to see where the boxes are, and creates coordinates of each
;@return list of coordinates of each box in s

(defun getBoxCoords (s row) 
	(cond	
		((NULL s) nil)
		(t	
			(let ((cols (getBoxCols (first s) 0)))

					(cond  
						(cols
							(append (coord cols row) (getBoxCoords (rest s) (+ row 1)))
						)
						(t 
							(getBoxCoords (rest s) (+ row 1))
						)
					)
			)
		)
	)
)



;@param row a  list, col a column to be inspected
;in row, if at index col there is a goal, add it to a list
;@return a list of columns where there is a goal in row

(defun getGoalCols (row col)
	(cond 
		((NULL row) nil)
		(t 	
			(cond	
				( (or (isStar (first row)) (isKeeperStar (first row)))
					(cons col (getGoalCols (rest row) (+ 1 col)))
				)

				(t 
					(getGoalCols (rest row) (+ 1 col))
				)
			)
		)
	)
)

;@param s state of game board, row: a row to be examined
;checks row by row to see where the goals are, and creates coordinates of each
;@return list of coordinates of each goal in s

(defun getGoalCoords (s row) ;returns list of (column, row)
	(cond	
		((NULL s) nil)
		(t	
			(let ((cols (getGoalCols (first s) 0)))

					(cond  
						(cols
							(append (coord cols row) (getGoalCoords (rest s) (+ row 1)))
						)
						(t 
							(getGoalCoords (rest s) (+ row 1))
						)
					)
			)
		)
	)
)

;@param s state of game board
;finds Coords of all boxes and goals
;calculates the distances between box and each goal
;finds minimum distance for each box
;@return returns the sum of all the minimum distances



;@param s state of game boards, pos a coordinate
;checks if tile at coordinate pos is a corner, meaning
;it is surriounded in at least 2 adjacent sides by walls
;@return true if corner, false if not
(Defun InCorner (s pos)
	(let* (
		(col (first pos))
		(row (second pos))

		( topRow (- row 1) )
		( topCol col )

		( rightRow row )
		( rightCol (+ col 1) )

		( leftRow row )
		( leftCol (- col 1) )

		( botRow (+ row 1) )
		( botCol col )

		( top (isWall  ( get-square s topRow topCol )  ))
		( right (isWall  ( get-square s rightRow rightCol )  ))
		( left (isWall  ( get-square s leftRow leftCol )  ))
		( bot (isWall  ( get-square s botRow botCol )  ))

		)
			(cond
				( (and top left)
					t
				)

				( (and left bot)
					t

				)

				((and bot right)
					t

				)

				((and right top)
					t
				)

				( t nil )

			)


	)



)


; checks if any boxes are in corners
(defun isCorner(s boxList)
	(cond
		( (NULL boxList) nil)

		(  (InCorner s (first boxList) )
			T
		)
		( t
			(isCorner s (rest boxlist))
		)

	)


)

;@param s state of game, boxcoords a list of coordinates of boxes
;checks whether any box is stuck in a corner
;@return True if a box is stuck in corner, false if no boxes are in corners
(defun hasBoxInCorner (s boxcoords)

	(cond
	((isCorner s boxcoords)
		T
	)
	(t nil)
	)

)

;@param s state of game board
;finds Coords of all boxes and goals
;calculates the distances between box and each goal
;finds minimum distance for each box
;checks if there is a box in a corner, if there is, then that means it's
;a dead end so to speak, and we shouldn't consider it, so add some large value to 
;the return, in this case 40 in addition to the sum of min distances
;@return heuristic value calculated described above

(defun h304576879(s)
	(let* (
		(boxcoords (getBoxCoords s 0)) ;initialize row parameter to 0 to loop through entire state s
		(goalcoords (getGoalCoords s 0))
		( dist ( cleanUpList (allBoxDistances boxcoords goalcoords)))
	 	( mins ( cleanUpList (listMins dist)))
	 	(result ( sumList mins)) )

	(cond 
		( (hasBoxInCorner s boxcoords)
			(+ result 40)
		)
	 	(t result)
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

;(?)j
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
