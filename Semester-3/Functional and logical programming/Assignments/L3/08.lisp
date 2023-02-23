;; '(1 (2 (3) (4 (8))) 5 (6 7))

;; myMax(a, b) = b, if a is not a number
;;               a, if b is not a number
;;               a, if a and b are numbers and a >= b
;;               b, otherwise
(defun myMax (a b)
    (cond
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((>= a b) a)
        (t b)
    )
) 


;; maxInList(l1 l2 ... ln)  =   l1, if n = 1
;;                              myMax(l1, maxInList(l2 l3 ... ln)), otherwise
(defun maxInList (vector)
    (cond
        ((atom vector) vector)
        ((null (cdr vector)) (car vector))
        (t (myMax (car vector) (maxInList (cdr vector))))
    )
)

;; maxInList(l1 l2 ... ln)  = l1, if n = 1
;;                          = maxInList(maxMain(l1) U maxMain(l2) U ... U maxMain(ln)), otherwise
(defun maxMain (vector)
    (cond
        ((atom vector) vector)
        (t (apply #'maxInList (list (mapcar #'maxMain vector))))
    )
)