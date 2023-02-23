;; a) Write a function to return the product of two vectors.
;; https://en.wikipedia.org/wiki/Dot_product

;; len(vector1) must be equal to len(vector2)
(defun dotProduct (vector1 vector2)
    (cond
        ((null vector1) 0)
        (t (+ (dotProduct (cdr vector1) (cdr vector2)) (* (car vector1) (car vector2))))
    )
)


;; b) Write a function to return the depth of a list. 
;; Example: the depth of a linear list is 1.

;; b - first solution:
(defun myMax (a b)
    (cond
        ((> a b) a)
        (t b)
    )
)

(defun maxInList (vector)
    (cond
        ((null (cdr vector)) (car vector))
        (t (myMax (car vector) (maxInList (cdr vector))))
    )
)

(defun findDepth (vector &optional (currentDepth 0))
    (cond 
        ((atom vector) currentDepth)
        (t (apply #'maxInList (list (mapcar #'(lambda (x) (findDepth x (+ currentDepth 1))) vector))))
    )
)

;; b - second solution
(defun findDepth2 (vector &optional (currentDepth 1))
    (cond
        ((null vector) currentDepth)
        ((listp (car vector)) (myMax (findDepth2 (car vector) (+ 1 currentDepth)) (findDepth (cdr vector) currentDepth)))
        (t (findDepth2 (cdr vector) currentDepth))
    )
)


;; c) Write a function to sort a linear list without keeping the double values.
;; '(10 5 8 9 11 12 10 13 10 14 13 12 100) -> '(5 8 9 10 11 12 13 14 100)


(defun contains (vector element)
    (cond
        ((null vector) nil)
        ((= (car vector) element) t)
        (t (contains (cdr vector) element))
    )
)

(defun compare (a b)
    (cond 
        ((= a b) 0)
        ((> a b) 1)
        (t -1)
    )
)

(defun sortedAppend (vector element)
    (cond
        ((null vector) (list element)) 
        ((= (compare (car vector) element)  0) vector)
        ((= (compare (car vector) element)  1) (cons element vector)) ;; insert it into answer
        ((= (compare (car vector) element) -1) (cons (car vector) (sortedappend (cdr vector) element)))
    )
)

(defun mySortHelper (vector (result nil))
    (cond
        ((null vector) result)
        (t (mySortHelper (cdr vector) (sortedAppend result (car vector))))
    )
)


(defun mySort (vector)
    (mySortHelper vector nil)
)



;; d) Write a function to return the intersection of two sets. 
;; '(1 2 3 4 5 6 7 8 9 10) '(7 8 9 10 11 12 13) -> '(7 8 9 10)
;; (sets!!!!! -> no duplicates)

(defun findIntersection (set1 set2)
    (cond
        ((null set1) nil)
        ((contains set2 (car set1)) (cons (car set1) (findIntersection (cdr set1) set2)))
        (t (findIntersection (cdr set1) set2))
    )
)