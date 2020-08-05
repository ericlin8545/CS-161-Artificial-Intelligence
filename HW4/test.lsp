(defun node2var (n c k)
    (+ (* (- n 1) k) c)
)

(defun at-most-one-color (n c k)
    (cond
        ((> c k) '())
        (t (append (amo-helper n c k c) (at-most-one-color n (+ c 1) k)))
    )
)

; Helper function for at-most-one color
; We build up clauses one by one
; and there are O(n^2) clauses given n available color indexes
(defun amo-helper (n c k base)
    (cond 
        ((>= c k) '())
        (t (cons (list (* -1 (node2var n base k)) (* -1 (node2var n (+ c 1) k))) (amo-helper n (+ c 1) k base)))
    )
)

(print (at-most-one-color 1 1 4))