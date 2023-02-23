(defun F (L1 L2)
    (append (F (car L1) L2)
        (cond
            ((null l1) (cdr l2))
            (T (list (f (car l1) l2)))
        )
    )
)

(defun fct (f  l)
    (cond
        ((null l) nil)
        ((funcall f (car l)) (cons (funcall f (car l) (fct f (cdr l)))))
        (t nil)
    )
)

(defun fct2 (f l)

)

(defun G (l)
    (list (car l) (car l))
)

(defun test ()
    (setq q 'g)
    (setq p q)
    (funcall p '(a b c))
)