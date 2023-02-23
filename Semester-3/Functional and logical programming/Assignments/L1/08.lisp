;; a)cWrite a function to return the difference of two sets.
;; '(1 3 5 7) '(1 5 7 9) -> '(3)

(defun contains (vector element)
    (cond
        ((null vector) nil)
        ((equal (car vector) element) t)
        (t (contains (cdr vector) element))
    )
)

(defun differenceHelper (set1 set2)
    (cond
        ((null set1) nil)
        ((contains set2 (car set1)) (differenceHelper (cdr set1) set2))
        (t (cons (car set1) (differenceHelper (cdr set1) set2)))
    )
)

(defun difference (set1 set2)
    (format t "Set1 - Set2: ~%~a~%" (differenceHelper set1 set2))
    (format t "Set2 - Set1: ")
    (differenceHelper set2 set1)
)


;; 