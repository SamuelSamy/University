;; '(A (B) (C (D) (E)))  -> T
;; '(A (B) (C (D) (E (F)))) -> NIL

(defun myMax (a b)
    (cond
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((>= a b) a)
        (t b)
    )
)

;; getDepth(parent leftTree rightTree, currentDepth)  =  currentDepth, if parent is null
;;                                                       =  max(getDepth(leftTree, currentDepth + 1), getDepth(rightTree, currentDepth + 1))
(defun getDepth (tree currentDepth)
    (cond
        ((null tree) currentDepth)
        (t (myMax (getDepth (cadr tree) (+ 1 currentDepth)) (getDepth (caddr tree) (+ 1 currentDepth))))
    )
)

;; myAbs(number1, number2)  = -(number1 - number2), if number1 < number2,
;;                          = number1 - number2, otherwise
(defun myAbs (number1 number2)
    (cond
        ((< number1 number2) (* -1 (- number1 number2)))
        (t (- number1 number2))
    )
)

;; main(parent leftTree rightTree)  = True, if parent is null
;;                                  = False, if |leftDepth - rightDepth| > 1,
;;                                  = main(leftTree) & main(rightTree) otherwise
(defun main (tree)
    (cond
        ((null tree) t)
        ((> (myAbs (getDepth (cadr tree) -1) (getDepth (caddr tree) -1)) 1) nil)
        (t (and (main (cadr tree)) (main (caddr tree))))
    )        
)