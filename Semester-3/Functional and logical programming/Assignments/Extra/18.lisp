;; '(10 9 8 9 10) -> T
;; '(10 9 8 9 8 10) -> NIL

(defun valley (vector &optional (flag 0))
    (
        (lambda (first second rest)
            (cond
                ((null rest) t)
                ((> first second)
                    (cond
                        ((= flag 1) nil)
                        (t (valley rest flag))
                    )
                )
                ((< first second)
                    (cond
                        ((= flag 0) (valley rest 1))
                        (t (valley rest flag))
                    )
                )
            )
        )
        (car vector) (cadr vector) (cdr vector)
    )
)


