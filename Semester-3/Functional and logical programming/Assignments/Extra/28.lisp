;; '((2 3 4) (6 (7 8) ((7 9) 8)) (6 8) 9) -> ((2 3 4) (6) 9)

(defun getLen (vector)
    (cond
        ((null vector) 0)
        (t (+ 1 (getLen (cdr vector))))
    )
)

(defun specialRemove (vector)
    (cond
        ((null vector) nil)
        ((listp (car vector))
            (cond
                ((= (mod (getLen (car vector)) 2) 0) (specialRemove (cdr vector)))
                (t (cons (specialRemove (car vector)) (specialRemove (cdr vector))))
            )
        )
        (t (cons (car vector) (specialRemove (cdr vector))))
    )
)