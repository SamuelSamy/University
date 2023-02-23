;; 4. Write a function that returns the product of numeric atoms in a list, at any level.
;; '(1 (a (2 (b (3) (4) 5) 6) 7) 8) -> 40320

(defun product (vector)
    (cond
        ((numberp vector) vector)
        ((atom vector) 1)
        (t (apply #'* (mapcar #'product vector)))
    )
)