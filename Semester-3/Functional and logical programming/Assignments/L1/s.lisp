(defun myAppend (first second)
    (cond
        ((and (null first) (null second)) nil)
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
        (t (cons (car first) (myAppend (cdr first) second)))
    )
)

(defun evaluate-short (sign number1 number2)
    (cond
        ((equal sign '+) (+ number1 number2))
        ((equal sign '-) (- number1 number2))
        ((equal sign '/) (/ number1 number2))
        ((equal sign '*) (* number1 number2))
    )
)

(defun evaluate (vector number-stack)
    (format t "Vector: ~a~%Number-stack: ~a~%~%" vector number-stack)
    (cond
        ((null vector) (car number-stack))
        ((numberp (car vector)) (evaluate (cdr vector) (cons (car vector) number-stack)))
        (t 
            (
                evaluate
                (cdr vector)
                (cons (evaluate-short (car vector) (car number-stack) (cadr number-stack)) (cddr number-stack))
            )
        )
    )
)

(defun reverseVector (vector)
    (cond
        ((null vector) nil)
        (t (myAppend (reverseVector (cdr vector)) (car vector)))
    )
)

(defun main (vector)
    (evaluate (reverseVector vector) nil)
)
