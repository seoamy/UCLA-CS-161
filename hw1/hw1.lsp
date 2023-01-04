; Amy Seo
; UID: 505328863
; CS 161 Spring 2022


; #1
; Function TREE-CONTAINS takes in arguments N (number) and TREE (number or list)
; Returns a boolean of whether or not the TREE contains number N
; Recursively checks if head or tail of TREE contains N and returns T when it is found, F if never found.
(defun TREE-CONTAINS (N TREE)
	(cond ((null TREE) NIL)
		((atom TREE) (equal N TREE))
		(t (OR (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE))))
	)
)


; #2
; Function TREE-MAX takes in argument TREE (number or list)
; Returns max value in the tree
; Recursively break down the TREE into its tail (or head if tail is null) to find the last element in the tree.
; By definition of TREE, the max value should be at the end of the TREE.
(defun TREE-MAX (TREE)
	(cond ((null TREE) NIL)
		((atom TREE) TREE)
		((null (cdr TREE)) (TREE-MAX (car TREE)))
		(t (TREE-MAX (cdr TREE)))	
	)
)


; #3
; Function TREE-ORDER takes in argument TREE (number or list)
; Returns post-order traversal of TREE
; Recursively concatenates head of tree with tail of tree and then the middle node
; When TREE is one atom, return a list of itself
; When TREE is a list of one node, return itself
(defun TREE-ORDER (TREE)
	(cond ((null TREE) NIL)
		((atom TREE) (cons TREE NIL))
		((null (cdr TREE)) TREE)
		(t (append (TREE-ORDER (car TREE)) (TREE-ORDER(third TREE)) (TREE-ORDER(second TREE))))
	)
)


; #4
; Function SUB-LIST takes in list L and two non-negative integers START and LEN
; Returns sub-list of L starting at position START with length LEN
; base case: len is 0, return NIL
; if start is 0, then append first element to SUB-LIST of the rest of L with LEN decremented
; otherwise, recursivly call SUB-LIST on the rest of the list with START decremented
(defun SUB-LIST (L START LEN)
	(cond ((equal LEN 0) NIL)
		((equal START 0) (append (cons (car L) NIL) (SUB-LIST (cdr L) START (- LEN 1))))
		(t (SUB-LIST (cdr L) (- START 1) LEN))
	)
)

; #5
; Function SPLIT-LIST takes in list L
; Returns a list of two lists L1, L2 such that L is result of appending L1, L2 
; 	and length of L1 - length of L2 is 0 or 1
; When length is odd, first sublist has length of (length L - 1)/2 + 1, and second sublist starts at (length L - 1)/2 + 1 with length (length L - 1)/2
; When length is even, first sublit has length L/2, and second sublist starts at index (length L / 2) with same length (length L / 2)
(defun SPLIT-LIST (L)
	(cond ((oddp (length L)) (list (SUB-LIST L 0 (+ 1 (/ (- (length L) 1) 2))) (SUB-LIST L (+ 1 (/ (- (length L) 1) 2)) (/ (- (length L) 1) 2))))
		(t (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
	)
)

; #6
; Function BTREE-HEIGHT takes in a binary tree TREE
; Returns height of TREE (length of longest path from root to farthest leaf node)
; recursively computes height of left and right subtrees, then increment by 1 and sum with the longer subtree
(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0)
		((< (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE)) ) (+ 1 (BTREE-HEIGHT (cadr TREE))))
		(t (+ 1 (BTREE-HEIGHT(car TREE))))
	)
)

; #7
; Function LIST2BTREE takes a non-empty list of atoms LEAVES
; Returns binary tree where tree leaves are LEAVES and all internal nodes have a number of leaves
; 	where the number of leaves in left subtree - number of leaves in right subtree is 0 or 1
; Base case: if leaves is length 1, return first element
; Default: recursively create list of first part of split-list and second element (out of the list) of split-list
(defun LIST2BTREE (LEAVES)
	(cond ((equal (length LEAVES) 1) (car LEAVES))
		(t (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (cadr (SPLIT-LIST LEAVES)))))
	)
)

; #8
; Function BTREE2LIST (inverse of LIST2BTREE) takes a binary tree TREE as input
; Returns a list of atoms
; Base case: if TREE is an atom, return a list of TREE
; Default: recursively append BTREE2LIST of the first element of the TREE and the second element (out of the list) of TREE
(defun BTREE2LIST (TREE)
	(cond ((atom TREE) (cons TREE NIL))
		(t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
	)
)

; #9
; Function IS-SAME takes two expressions E1 and E2 whose atoms are all numbers
; Returns a boolean of whether the expressions are identical, only using '='
; Base cases:
; 	both E1 and E2 are null: t
;		one of the expressions are null and the other isnt: f
; 	both expressions are atoms and are equal using = : t
; 	one of the expressions are an atom and the other isnt: f
; Default: recursively call IS-SAME on car of both expressions ANDed with IS-SAME on cdr of both expressions
(defun IS-SAME (E1 E2)
	(cond ((AND (null E2) (null E1)) t)
		((OR (null E1) (null E2)) NIL)
		((AND (atom E1) (atom E2)) (= E1 E2)) 
		((OR (atom E1) (atom E1)) NIL)
		(t (AND (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
	)
)

; #10
; Function FLATTEN-APPEND takes two expressions E1 (cannot be an atom) and E2
; Returns a list of all atoms of E2 appended to E1 in left-to-right, 
; 	depth-first order of occurence of atoms in E2
; Base case: If E2 is null, simply return E1
; Base case 2: If E2 is an atom, append it to E1
; Default: Recursively call FLATTEN-APPEND with FLATTEN-APPEND of E1 with head of E2 and tail of E2
(defun FLATTEN-APPEND (E1 E2)
	(cond ((null E2) E1)
		((atom E2) (append E1 (cons E2 NIL)))
		(t (FLATTEN-APPEND (FLATTEN-APPEND E1 (car E2)) (cdr E2)))
	)
)




