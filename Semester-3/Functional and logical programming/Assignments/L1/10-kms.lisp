;; (+ 1 3) ==> 4  (1 + 3)
;; (+ * 2 4 3) ==> 11  [((2 * 4) + 3)
;; (+ * 2 4 - 5 * 2 2) ==> 9  ((2 * 4) + (5 - (2 * 2))


(defun myLen (vector)
    (cond
        ((null vector) 0)
        (t (+ 1 (myLen (cdr vector))))
    )
)

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
    (format t "~a~a~a~%" number1 sign number2)
    (cond
        ((equal sign '+) (+ number1 number2))
        ((equal sign '-) (- number1 number2))
        ((equal sign '/) (/ number1 number2))
        ((equal sign '*) (* number1 number2))
    )
)

(defun evaluate (vector sign-stack number-stack)
    (
        (lambda (head tail last-sign last-number)
            (format t "Head: ~a; Tail: ~a~%Sign stack: ~a; Number stack: ~a~%~%" head tail sign-stack number-stack)
            (cond
                ((null vector) last-number)
                ((numberp head)
                    (cond
                        ;; if there is at least 1 number in the number-stack compute the result
                        (
                            (> (myLen number-stack) 0) 
                            ;; compute evaluate-short between last-sign, last-number, car number-stack
                            ;; pop element from number-stack and sign-stack
                            ;; add result to number-stack
                            (
                                evaluate 
                                tail 
                                (cdr sign-stack) 
                                (cons (evaluate-short last-sign last-number head) (cdr number-stack)) 
                            )
                        )
                        ;; else add it to the stack
                        (
                            t
                            (
                                evaluate
                                tail
                                sign-stack
                                (cons head number-stack)
                            )
                        )
                        
                    )
                )
                ;; it's a sign -> just add it to the sign stack
                (   
                    t 
                    (
                        evaluate
                        tail
                        (cons head sign-stack)
                        number-stack
                    )
                )
            )
        
        )
        (car vector) (cdr vector) (car sign-stack) (car number-stack)
    )
)

(defun main (vector)
    (evaluate vector nil nil)
)