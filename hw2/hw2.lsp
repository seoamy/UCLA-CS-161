; Amy Seo
; UID: 505328863
; CS 161 Spring 2022

; Problem 1
; Input: TREE which is a list representation of a tree
; Output: list of BFS traversal of TREE
; Base case: If TREE is null, return NIL
; If first element of TREE is an atom, "visit" the node:
;    cons that element with BFS performed on the rest of the tree
; Otherwise, call BFS with the first element appended to the back of the TREE

; TEST CASES:
; (BFS 'a) returns (a)
; (BFS '(a)) returns (a)
; (BFS '(a b c d e f g)) returns (a b c d e f g)
; (BFS '((a b) (c d) e (f (g h)))) returns (e a b c d f g h)
; (BFS '((a b c) d (e f) (g h) ((i j k) l (m n o)))) 
;   returns (d a b c e f g h l i j k m n o)
(defun BFS (TREE)
  (cond ((null TREE) NIL)
    ((atom TREE) (list TREE))
    ((atom (car TREE)) (cons (car TREE) (BFS (cdr TREE))))
    (t (BFS (append (cdr TREE) (car TREE))))
  )
)

; Problem 2
; Input: TREE which is a list representation of a tree
; Output: list of DFS traversal of TREE
; Base case: If TREE is null, return NIL
; If TREE is an atom, "visit" the node:
;   return a list of that atom so it can be appended in its recursive call
; Otherwise, append the DFS call of car TREE with DFS call of cdr TREE 
;   since we go left to right order

; TEST CASES:
; (DFS 'a) returns (a)
; (DFS '(a)) returns (a)
; (DFS '(a b c d e f g)) returns (a b c d e f g)
; (DFS '((a b) (c d) e (f (g h)))) returns (a b c d e f g h)
; (DFS '((a b c) d (e f) (g h) ((i j k) l (m n o)))) 
;   returns (a b c d e f g h i j k l m n o)
(defun DFS (TREE)
  (cond ((null TREE) NIL)
    ((atom TREE) (list TREE))
    (t (append (DFS (car TREE)) (DFS (cdr TREE))))
  )
)

; Problem 3
; Input: TREE which is a list representation of a Tree
;        DEPTH which is the depth (number of levels) to traverse
; Output: returns a list representation of depth first limited search on
;         input TREE with depth limit DEPTH
; DFID Helper Function, similar to DFS but with limited depth
; Base case: If TREE is null or depth is < 0, return NIL as there is nothing to traverse
; If TREE is an atom, return a list of itself so it can be appended in its recursive call
; Otherwise, append DFID_HELPER call of cdr TREE with same depth since its on the same level
; with DFID_HELPER called on car TREE with one less depth since its the next level
; Appending the call on cdr TREE first makes the order from right to left

; TEST CASES
; (DFID_HELPER '(a) 1) returns (a)
; (DFID_HELPER '(a) 3) returns (a)
; (DFID_HELPER '(a b c d e f g) 2) returns (g f e d c b a)
; (DFID_HELPER '((a b) (c d) e (f (g h))) 2) returns (f e d c b a)
; (DFID_HELPER '((a b c) d (e f) (g h) ((i j k) l (m n o))) 3) 
;   returns (o n m l k j i h g f e d c b a)
(defun DFID_HELPER (TREE DEPTH)
  (cond ((OR (null TREE) (< depth 0)) NIL)
    ((atom TREE) (list TREE))
    (t (append (DFID_HELPER (cdr TREE) depth) (DFID_HELPER (car TREE) (- DEPTH 1))))
  )
)

; DFID
; Input: TREE which is a list representation of a Tree
;        DEPTH which is the depth (number of levels) to traverse
; Output: returns a list representation of depth first iterative deepening search
;         on a tree from right to left
; Base case: If TREE is null or depth is < 0, return NIL
; Otherwise, call DFID with one less depth and DFID_HELPER with current tree and depth
; Append these two together 

; TEST CASES
; (DFID '(a) 1) returns (a)
; (DFID '(a) 3) returns (a a a)
; (DFID '(a b c d e f g) 2) returns (g f e d c b a g f e d c b a)
; (DFID '((a b) (c d) e (f (g h))) 2) returns (e f e d c b a)
; (DFID '((a b c) d (e f) (g h) ((i j k) l (m n o))) 3) 
;   returns (d l h g f e d c b a o n m l k j i h g f e d c b a)
(defun DFID (TREE DEPTH)
  (cond ((OR (null TREE) (< depth 0)) NIL)
    (t (append (DFID TREE (- DEPTH 1)) (DFID_HELPER TREE DEPTH)))
  )
)


; These functions implement a depth-first solver for the River-Boat
; problem. In this problem, three members from Group-X, denoted XXX,
; and three members from Group-O, denoted OOO, are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more O's on one side of the river than X's.

; In this implementation, a state is represented by a single list
; (#X #O side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. #X and #O represent the number of X's and
; O's on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three X's, three O's, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((equal s '(3 3 NIL)) t)
    (t NIL)
  )  
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

; TEST CASES
; (next-state '(3 3 t) 5 2) -> nil
; (next-state '(3 3 t) 2 1) -> nil
; (next-state '(3 3 t) 1 2) -> nil
; (next-state '(3 3 t) 1 1) -> ((1 1 nil))
; (next-state '(3 3 t) 0 1) -> ((0 1 nil))
; (next-state '(3 3 t) 1 0) -> nil
; (next-state '(3 3 t) 0 2) -> ((0 2 nil))
; (next-state '(3 3 t) 2 0) -> nil
; (next-state '(1 1 nil) 1 1) -> ((3 3 t))
; (next-state '(1 1 nil) 1 0) -> ((3 2 t))
; (next-state '(0 1 nil) 1 1) -> nil
; (next-state '(0 2 nil) 0 1) -> ((3 2 t))
; (next-state '(0 2 nil) 0 2) -> ((3 3 t))
(defun next-state (s m c)
  (let* ((this_x (first s))
    (this_o (second s))
    (other_side (not (third s)))
    (new_x (- 3 (- this_x m))) ; number of Xs on other side
    (new_o (- 3 (- this_o c)))) ; number of Os on other side
    (cond ((OR (> m this_x) (> c this_o)) NIL) ; moving more Xs or Os than available
      ((OR (equal new_x 3) (equal new_x 0)) (list (list new_x new_o other_side))) ; if one side has all the Xs, it is legal
      ((not (equal new_x new_o)) NIL) ; if number of Xs and Os are unequal on one side, it can lead to illegal states
      (t (list (list new_x new_o other_side))) ; all illegal states have been checked
    )
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
; NOTE: all possible successor states are moving:
;; 1 X, 0 O
;; 0 X, 1 O
;; 2 X, 0 O
;; 0 X, 2 O
;; 1 X, 1 O

; TEST CASES
; (succ-fn '(3 3 t)) -> ((0 1 nil) (1 1 nil) (0 2 nil))
; (succ-fn '(1 1 nil)) -> ((3 2 t) (3 3 t))
; (succ-fn '(3 2 t)) -> ((1 1 nil) (0 2 nil) (0 3 nl))
; (succ-fn '(0 2 nil)) -> ((3 2 t) (3 3 t))
; (succ-fn '(0 3 nil)) -> ((3 1 t) (3 2 t))
(defun succ-fn (s)
  (append (next-state s 1 0)
          (next-state s 0 1)
          (next-state s 1 1)
          (next-state s 2 0)
          (next-state s 0 2)
  )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) NIL)
    ((equal (car states) s) t)
    (t (on-path s (cdr states)))
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond ((null states) NIL) ; no successor states
    (t (let ((result (mc-dfs (first states) path))) ; find path from current state to goal state
      (cond ((null result) (mult-dfs (cdr states) path)); no path -> go to next successor state
        (t result) ; valid path to goal state -> return path from initial to final state
      ))
    )
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

; TEST CASES
; (mc-dfs '(3 3 t) nil) -> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T)
;                           (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T))
; (mc-dfs '(3 2 t) '((1 1 NIL) (3 3 t))) -> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T)
;                                           (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T))
; (mc-dfs '(1 1 t) nil) -> ((3 3 NIL) (0 2 T) (3 2 NIL) (1 1 T))
; (mc-dfs '(3 2 nil) nil) -> ((3 3 NIL) (1 1 T) (3 2 NIL))
(defun mc-dfs (s path)
  (cond ((final-state s) (append (list s) path))
    ((on-path s path ) NIL)
    (t (mult-dfs (succ-fn s) (append (list s) path)))
  )
)

; Function execution examples

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
