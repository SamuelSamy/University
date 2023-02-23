;; (A B C (D (E F) G H I)) -> (C B A (D (F E) I H G))

(defun appendSimple (first second)
    (cond
        ((null first)
            (cond
                ((listp second) second)
                (t (list second))
            )
        )
        ((null second)
            (cond
                ((listp first) first)
                (t (list first))
            )
        )
        (t (cons (car first) (appendSimple (cdr first) second)))
    )
)

(defun specialReverse (vector &optional (result nil))
    (cond
        ((null vector) result)
        ((atom (car vector)) (specialReverse (cdr vector) (cons (car vector) result)))
        (t (appendSimple result (appendSimple (list (specialReverse (car vector))) (specialReverse (cdr vector)))))
    )
)