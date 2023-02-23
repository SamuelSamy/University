;; (+ 1 3) ==> 4  (1 + 3)
;; (+ * 2 4 3) ==> 11  [((2 * 4) + 3)
;; (+ * 2 4 - 5 * 2 2) ==> 9  ((2 * 4) + (5 - (2 * 2))


(defun myLen (vector)
    (cond
        ((null vector) 0)
        (t (+ 1 (myLen (cdr vector))))
    )
)

; (defun myAppend (first second)
;     (cond
;         ((and (null first) (null second)) nil)
;         ((null first)
;             (cond
;                 ((listp second) second)
;                 (t (list second))
;             )
;         )
;         ((null second)
;             (cond
;                 ((listp first) first)
;                 (t (list first))
;             )
;         )
;         (t (cons (car first) (myAppend (cdr first) second)))
;     )
; )

(defun evaluate-short (sign number1 number2)
    ;;(format t "~a~a~a~%" number1 sign number2)
    (cond
        ((equal sign '+) (+ number1 number2))
        ((equal sign '-) (- number1 number2))
        ((equal sign '/) (/ number1 number2))
        ((equal sign '*) (* number1 number2))
    )
)

(defun myAppend (l1 l2)
    (cond
        ((and (null l1) (listp l2)) l2)
        ((and (null l1) (atom l2)) (list l2))
        (t (cons (car l1) (myAppend (cdr l1) l2)))
    )
)

(defun evaluate (vector number-stack)
    ;;(format t "Head: ~a; Tail: ~a~%Number stack: ~a~%~%" (car vector) (cdr vector) number-stack)
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
        (t (myAppend (reverseVector (cdr vector)) (car vector)))
    )
)

(defun main (vector)
    (evaluate (reverseVector vector) nil)
)