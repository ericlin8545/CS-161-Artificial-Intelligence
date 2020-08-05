; Question 1. Write a single LISP function, called PAD, that takes a single integer argument N, and returns the Nth Padovan number.
(defun PAD (N)
    (cond ((or (= N 0) (= N 1) (= N 2)) 1)  ; if N equals to 0, 1, or 2, then resutns the value 1
          ; In other cases, we need to conduct the recursion to find PAD(n+1) = PAD(n-1) + PAD(n-2).
          ; To make it conveniently to handle this question, we transfer the furmula to PAD(n) = PAD(n-2) + PAD(n-3)
          (t (+ (PAD (- N 2)) (PAD (- N 3))
             )
          )
    )
)

; (print (PAD 0))
; (print (PAD 1))
; (print (PAD 2))
; (print (PAD 3))
; (print (PAD 4))
; (print (PAD 5))
; (print (PAD 6))
; (print (PAD 7))
; (print (PAD 8))
; (print (PAD 9))
; (print (PAD 10))
; (print (PAD 11))

; Question 2. Analyze the number of additions required of PAD function
(defun SUMS (N)
    (cond ((or (= N 0) (= N 1) (= N 2)) 0) ; Base Case: if N equals to 0, 1, or 2, then resutns the value 0
          ; In other cases, we need to conduct the recursion to find SUMS(n) = SUMS(n-2) + SUMS(n-3) + 1.
          (t (+ (SUMS (- N 2)) (SUMS (- N 3)) 1
             )
          )
    )
)

; (print (SUMS 0))
; (print (SUMS 1))
; (print (SUMS 2))
; (print (SUMS 3))
; (print (SUMS 4))
; (print (SUMS 5))
; (print (SUMS 6))
; (print (SUMS 7))
; (print (SUMS 8))
; (print (SUMS 9))
; (print (SUMS 10))
; (print (SUMS 11))

(defun ANON (TREE)
    (cond ((NULL TREE) '()) ; If the TREE is nil, then return an empty list.
          ((atom TREE) '?)  ; If the TREE is an atom, which means it's a basic element, then return a question mark ?.
          ; In other cases, conduct the recursion on the first item of TREE and the rest items of TREE.
          ; Then use the function cons to concatenate the results
          (t (cons (ANON (car TREE)) (ANON (cdr TREE))
             ) 
          )
    )
)

; (print (ANON '42))
; (print (ANON 'FOO))
; (print (ANON '(((L E) F) T)))
; (print (ANON '(5 FOO 3.1 -0.2)))
; (print (ANON '(1 (FOO 3.1) -0.2)))
; (print (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
; (print (ANON '(R (I (G (H T))))))
; (print (ANON nil))
; (print (ANON ()))
; (print (ANON '(nil nil nil (? ?))))
