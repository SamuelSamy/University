;; 1.Write a function to check if an atom is member ofa list (the list is non-liniar)
;; '(A (B (C D) (E (F)) G) H) 'A -> T
;; '(A (B (C D) (E (F)) G) H) 'Z -> NIL


(defun countElement (vector element)
    (cond
        ((atom vector)
            (cond
                ((equal vector element) 1)
                (t 0)
            )
        )
        (t (apply #'+ (mapcar #'(lambda (x) (countElement x element)) vector)))
    )
)

(defun main (vector element)
    (cond
        ((equalp (countElement vector element) 0) nil)
        (t t)
    )
)