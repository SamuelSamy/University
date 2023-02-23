;; a) Write a function that inserts in a linear list a given atom A after the 2nd, 4th, 6th, ... element.
;; '(1 2 3 4 5 6 7 8) 'A -> (1 2 A 3 4 A 5 6 A 7 8 A)
(defun evenInsert (vector element &optional (length 0))
    (cond
        ((null vector) nil)
        ((= (mod length 2) 1) (cons (car vector) (cons element (evenInsert (cdr vector) element (+ 1 length)))))
        (t (cons (car vector) (evenInsert (cdr vector) element (+ 1 length))))
    )
)


;; b) Write a function to get from a given list the list of all atoms, on any level, but reverse order. 
;; Example: '(((A B) C) (D E)) ==> (E D C B A)

;; vector1 **must** be of type 'list'
;; vector2 can be of type 'list' or 'atom'
; (defun myAppend (vector1 vector2)
;     (cond
;         ((null vector1) vector2)
;         (t (cons (car vector1) (myAppend (cdr vector1) vector2)))
;     )
; )

; (defun explodeReversed (vector)
;     (cond
;         ((null vector) nil)
;         ((listp (car vector)) (myAppend (explodeReversed (cdr vector)) (explodeReversed (car vector))))
;         (t (myAppend (explodeReversed (cdr vector)) (list (car vector))))
;     )
; )


;; c) Write a function that returns the greatest common divisor of all numbers in a nonlinear list.

(defun _gcd (a b)
    (cond
        ((and (not (numberp a)) (not (numberp b)) nil))
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((= b 0) a)
        (t (_gcd b (mod a b)))
    )
)

(defun gcdForList(l)
    (cond
        ((null l) nil)
        ((listp (car l)) (_gcd (gcdForList (car l)) (gcdForList (cdr l))))
        (t (_gcd (car l) (gcdForList (cdr l))))    
    )
)


;; d) Write a function that determines the number of occurrences of a given atom in a nonlinear list.
;; '(1 2 (3 1 (4 1) (5 1) 1) (1) 1) 1 => 7

(defun countOccurrences (vector element)
    (cond
        ((null vector) 0)
        ((listp (car vector)) (+ (countOccurrences (car vector) element) (countOccurrences (cdr vector) element)))
        ((= (car vector) element) (+ 1 (countOccurrences (cdr vector) element)))
        (t (countOccurrences (cdr vector) element))
    )
)