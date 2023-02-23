;; 2.Write a function that returns the sum of numeric atoms in a list, atany level.
;; '(1 (a (2 (b (3) (4) 5) 6) 7) 8) -> 36

(defun sum (vector)
    (cond        
        ((numberp vector) vector)
        ((atom vector) 0)
        (t (apply #'+ (mapcar #'sum vector)))
    )
)