;; 7. Write a function that substitutes an element E withall elements of alist L1 at alllevels of a given list L.

;; good luck lol

;; '(1 2 (3 2 4) (5 (2))) 2 '(0 0) -> (1 0 0 (3 0 0 4) (5 (0 0)))

;; easier function, it replaces the element with the whole list
; (defun replaceElement (vector element newElements)
;     (cond
;         ((atom vector)
;             (cond
;                 ((equal vector element) newElements)
;                 (t vector)
;             )
;         )
;         (t (mapcar #'(lambda (x) (replaceElement x element newElements)) vector))
;     )
; )

;; you can stop now, it's fine
(defun myAppendSimple (first second)
    (cond
        ((null first)
            (cond
                ((null second) nil)
                ((atom second) (list second))
                (t second)
            )
        )
        ((atom first)
            (cond
                ((null second) (list first))
                ((atom second) (list first second))
                (t (cons first second))
            )
        )
        (t (cons (car first) (myAppendSimple (cdr first) second)))
    )
)

(defun myAppend (&rest args)
    (cond
        ((null args) nil)
        (t (myAppendSimple (car args) (apply #'myAppend (cdr args))))
    )
)

(defun replaceElement (vector element newElements)
    (cond
        ((atom vector)
            (cond
                ((equal vector element) newElements)
                (t (list vector))
            )
        )
        (t (list (apply #'myAppend (mapcar #'(lambda (x) (replaceElement x element newElements)) vector))))
    )
)

(defun main (vector element newElements)
    (car (replaceElement vector element newElements))
)