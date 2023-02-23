;; 5. Write a function that computes the sum of even numbers and the decrease the sum of odd numbers, at any level of a list.
;; '(1 (a (2 (b (3) (4) 5) 6) 7) 8) -> 4


(defun main (vector)
    (cond
        ((numberp vector) 
            (cond
                ((= (mod vector 2) 0) vector)
                (t (* -1 vector))
            )
        )
        ((atom vector) 0)
        (t (apply #'+ (mapcar #'main vector)))
    )
)