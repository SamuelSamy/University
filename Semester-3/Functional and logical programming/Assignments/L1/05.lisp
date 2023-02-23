;; a) Write twice the n-th element of a linear list. 
;; Example: for (10 20 30 40 50) and n=3 will produce (10 20 30 30 40 50).

(defun doubleNth (vector n &optional (current 1))
    (cond
        ((null vector) nil)
        ((= n current) (cons (car vector) vector))
        (t (cons (car vector) (doubleNth (cdr vector) n (+ 1 current))))
    )
)


;; b) Write a function to return an association list with the two lists given as parameters. 
;; Example: (A B C) (X Y Z) --> ((A.X) (B.Y) (C.Z)).

(defun getAssociationList (vector1 vector2)
    (cond
        ((or (null (cdr vector1)) (null (cdr vector2))) (cons (car vector1) (car vector2)))
        (t (list (cons (car vector1) (car vector2)) (getAssociationList (cdr vector1) (cdr vector2))))
    )
)


;; c) Write a function to determine the number of all sublists of a given list, on any level. A sublist is either the list itself, or any element that is a list, at any level. 
;; Example: (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 5 
;; lists: (list itself, (3 ...), (4 5), (6 7), (9 10)).

(defun countLists (vector)
    (cond
        ((null vector) 1)
        ((listp (car vector)) (+ (countLists (car vector)) (countLists (cdr vector))))
        (t (countlists (cdr vector)))
    )
)


;; d) Write a function to return the number of all numerical atoms in a list at superficial level.
;; '(1 2 a (3 (4 (5 (6) 3) 2) 5) 0 b 4 c) -> 4

(defun countNumbers (vector)
    (cond
        ((null vector) 0)
        ((numberp (car vector)) (+ 1 (countNumbers (cdr vector))))
        (t (countNumbers (cdr vector)))
    )
)