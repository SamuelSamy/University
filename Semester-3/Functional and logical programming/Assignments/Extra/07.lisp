;; 1 '(2 3) -> ((1 2 3) (2 1 3) (2 3 1))

(defun insertOnPos (vector element position &optional (currentPosition 1))
    (cond
        ((null vector) (list element))
        ((= currentPosition position) (cons element vector))
        (t (cons (car vector) (insertOnPos (cdr vector) element position (+ 1 currentPosition))))
    )
)

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

(defun specialInsert (element vector len &optional (currentPosition 0))
    (cond
        ((> currentPosition len) nil)
        (t (appendSimple (list (insertOnPos vector element (+ 1 currentPosition))) (specialInsert element vector len (+ 1 currentPosition)) ))
    )
)

(defun getLength (vector)
    (cond
        ((null vector) 0)
        (t (+ 1 (getLength (cdr vector))))
    )
)

(defun main (element vector)
    (specialinsert element vector (getLength vector))
)