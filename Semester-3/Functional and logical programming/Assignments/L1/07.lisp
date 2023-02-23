;; a) Write a function to eliminate the n-th element of a linear list.
;; '(1 2 3 4 5) 3 -> '(1 2 4 5)
(defun removeNth (vector n &optional (current 1))
    (cond
        ((null vector) nil)
        ((= n current) (cdr vector))
        (t (cons (car vector) (removeNth (cdr vector) n (+ 1 current))))
    )
)


;; b) Write a function to determine the successor of a number represented digit by digit as a list, 
;;    without transforming the representation of the number from list to number. 
;; Example: (1 9 3 5 9 9) --> (1 9 3 6 0 0)
;; reverse the list: (9 9 5 3 9 1)

(defun appendSimple (vector1 vector2)
    (cond
        ((and (null vector1) (listp vector2)) vector2)
        ((and (null vector1)) list(vector2))
        (t (cons (car vector1) (appendSimple (cdr vector1) vector2)))
    )
)

(defun myReverse (vector)
    (cond
        ((null vector) nil)
        (t (appendSimple (myReverse (cdr vector)) (list (car vector))))
    )
)

(defun successorHelper (number &optional (adder 1))
    (cond
        ((and (null number) (= adder 0)) nil)
        ((null number) (list adder))
        (t (cons (mod (+ adder (car number)) 10) (successorHelper (cdr number) (floor (+ adder (car number)) 10)) ))
    )
)

(defun successor (number)
    (myReverse (successorHelper (myReverse number)))
)


;; c) Write a function to return the set of all the atoms of a list.
;; Exemple: '(1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)

(defun simpleAppend (vector1 vector2)
    (cond
        ((and (null vector1) (listp vector2)) vector2)
        ((null vector1) (list vector2))
        (t (cons (car vector1) (simpleAppend (cdr vector1) vector2)))
    )
)

(defun explode (vector)
    (cond
        ((null vector) nil)
        ((atom (car vector)) (cons (car vector) (explode (cdr vector))))
        ((listp (car vector)) (simpleAppend (explode (car vector)) (explode (cdr vector))))    
    )
)

(defun contains (vector element)
    (cond
        ((null vector) nil)
        ((equal (car vector) element) t)
        (t (contains (cdr vector) element))
    )
)

(defun makeSetHelper (vector &optional (result nil))
    (cond
        ((null vector) result)
        ((contains result (car vector)) (makeSetHelper (cdr vector) result))
        (t (makeSetHelper (cdr vector) (simpleAppend result (car vector))))
    )
)

(defun makeSet (vector)
    (makeSetHelper (explode vector))
)

;; d) Write a function to return the sum of all numerical atoms in a list at superficial level.
;; '(1 A 2 (B 3 (C 4)) D 3) -> 6

(defun sum (vector)
    (cond
        ((null vector) 0)
        ((numberp (car vector)) (+ (car vector) (sum (cdr vector))))
        (t (sum (cdr vector)))
    )
)