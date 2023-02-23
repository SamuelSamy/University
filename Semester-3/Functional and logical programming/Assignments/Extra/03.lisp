;; '(A 2 B 3 C D 1) -> ((A B) (A C) (A D) (B C) (B D) (C D))

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

(defun makePairs (element vector)
    (cond
        ((null vector) nil)
        ((numberp (car vector)) (makePairs element (cdr vector)))
        (t (appendSimple (list (list element (car vector))) (makePairs element (cdr vector))))
    )
)

(defun main (vector)
    (cond
        ((null (cdr vector)) nil)
        ((numberp (car vector)) (main (cdr vector)))
        (t (appendSimple (makePairs (car vector) (cdr vector)) (main (cdr vector))))
    )
)