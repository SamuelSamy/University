;; '(a (b (g)) (c (d (e)) (f))) 
;; 'e -> (a c d e)
;; 'v -> ()

(defun get-good-two (l1 l2)
    (cond
        ((null l1) l2)
        (t l1)
    )
)

(defun get-good-list (vector)
    (cond
        ((null vector) nil)
        (t (get-good-two (car vector) (get-good-list (cdr vector))))
    )
)

(defun get-good (&rest args)
    (get-good-list args)
)

(defun myAppend (vector element)
    (cond
        ((null vector) element)
        (t (cons (car vector) (myAppend (cdr vector))))
    )
)

(defun reverseE (vector)
    (cond
        ((null vector) nil)
        (t (myAppend (car vector) (reverseE (cdr vector))))
    )
)


(defun find-path (tree node &optional (path nil)) 
    (cond
        ((atom tree)
            (cond
                ((equalp tree node) path)
                (t nil)
            )
        )
        (t (apply #'get-good (mapcar #'(lambda (x) (find-path x node (cons (car tree) path) )) tree)))
    )
)

(defun main (tree node)
    (reverseE (find-path tree node))
)