;; '(1 2 3 4 5 6) -> '(3 5 6)

(defun specialRemove (vector &optional (nextToRemove 1) (current 1))
    (cond
        ((null vector) nil)
        ((= nextToRemove current) (specialRemove (cdr vector) (* 2 nextToRemove) (+ 1 current)))
        (t (cons (car vector) (specialRemove (cdr vector) nextToRemove (+ 1 current))))
    )
)