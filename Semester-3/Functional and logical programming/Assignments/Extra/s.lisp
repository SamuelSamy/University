;; '(A (A B (B) (A C C) C))

(defun myAppend(l1 l2)
    (cond
        ((and (null l1) (listp l2)) l2)
        ((and (null l1) (atom l2)) (list l2))
        (T (cons (car l1) (myAppend (cdr l1) l2)))
    )
)

(defun removeE(l e)
    (cond
        ((null l) nil)
        ((and (atom (car l)) (equal (car l) e)) (removeE (cdr l) e))
        ((atom (car l)) (cons (car l) (removeE (cdr l) e)))
        (T (cons (removeE (car l) e) (removeE (cdr l) e)))
    )
)

(defun countE(l e)
    (cond
        ((null l) 0)
        ((and (atom (car l)) (equal (car l) e)) (+ 1 (countE (cdr l) e)))
        ((atom (car l)) (countE (cdr l) e))
        (T (+ (countE (car l) e) (countE (cdr l) e)))
    )
)

(defun makePairs(l)
    (format t "~a~%" l)
    (cond
        ((null l) nil)
        ((null (car l)) (makePairs (cdr l)))
        ((atom (car l)) (myAppend (list (list (car l) (countE l (car l)))) (makePairs (removeE (cdr l) (car l)))))
        (T (myAppend (makePairs (car l)) (makePairs (cdr l))))
    )
)


(defun linearize (vector) 
    ((null vector) nil)
    ((atom (car vector)) (cons (car vector) (linearize (cdr vector))))
    (t (myAppend (linearize (car vector)) (linearize (cdr vector))))
)

(defun countElements (vector)
    
)