;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
	  (backtrack-search n delta '())
)

; input: n, number of variables in delta
;        delta, a CNF representation (list of lists)
;        result, the resulting solution to the cnf if possible 
; output: result which is a list of variable assignments recursively found through this backtrack search
(defun backtrack-search (n delta result)
  (cond ((= (length result) n) result)
        (t (let* ((next-var (+ (length result) 1)) ; next variable is next element in list
                 (next-var-true (append result (list next-var))) ; next variable could be true
                 (next-var-false (append result (list (- 0 next-var)))) ; next variable could be false (negative)
                 )
                (check-next-var n delta next-var-true next-var-false)
            )
        )
  )
)

; satisfied clauses do not need to be checked again, so the delta can be pruned of them
(defun prune-clause (delta result)
  (cond ((null delta) delta)
        ((prune-clause-helper (car delta) result) (prune-clause (cdr delta) result))
        (t (cons (car delta) (prune-clause (cdr delta) result)))
  )
)

; helper function to prune satsified clauses
(defun prune-clause-helper (clause result)
  (cond ((null clause) clause)
        ((is-in-list (car clause) result) t)
        (t (prune-clause-helper (cdr clause) result))
  )
)

; helper function for backtrack search
; input: n, delta, possible true assignment of current variable, possible false assignment of current variable
; output: addition of current value to result, or nil if current variable assignment does not satisfy the cnf
(defun check-next-var (n delta true-var false-var)
  (cond ((is-satisfied-cnf n delta true-var)
        (let* ((p-delta (prune-clause delta true-var))
        ;;  test here
              (p-)
        ;;
              (true-var-valid (backtrack-search n p-delta true-var)))
              (cond (true-var-valid true-var-valid)
                    ((is-satisfied-cnf n delta false-var) (check-false-var n delta false-var))
                    (t nil)
              )
        )
      )
      ((is-satisfied-cnf n delta false-var) (check-false-var n delta false-var))
      (t nil)   
  )
)

(defun check-false-var (n delta false-var)
  (let* ((p-delta (prune-clause delta false-var))
        (false-var-valid (backtrack-search n p-delta false-var)))
        (cond (false-var-valid false-var-valid)
              (t nil)
        )
  )
)

(defun is-satisfied-cnf (n cnf result)
  (cond ((OR (= (length result) 0) (< n (length result)) (< n (abs (car result))) (= (car result) 0)) nil)
        ((null cnf) t)
        ((not (is-satisfied-clause (car cnf) result)) nil)
        (t (is-satisfied-cnf n (cdr cnf) result))
  )
)

(defun is-satisfied-clause (clause result)
  (cond ((null clause) nil)
        ((OR (is-in-list (car clause) result) (is-unsatisfied-literal (car clause) result)) t)
        (t (is-satisfied-clause (cdr clause) result))
  )
)

(defun is-unsatisfied-literal (literal result)
  (cond ((null literal) nil)
        ((AND (not (is-in-list literal result)) (not (is-in-list (- 0 literal) result))) t)
        (t nil)
  )
)

(defun is-in-list (item list)
  (cond  ((null list) nil)
         ((= item (car list)) t)
         (t (is-in-list item (cdr list)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

