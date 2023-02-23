;; '(1 2 (1) (2 (1))) '1
;;(2 (nil) (2 (nil))

; (defun removeNil(l)
;     (cond
;         ((null l) nil)
;         ((and (atom (car l)) (equal (car l) nil)) (removeNil (cdr l)))
;         ((atom (car l)) (cons (car l) (removeNil (cdr l))))
;         ((and (listp (car l)) (equal (car l) (list nil))) (cons (list (car l)) (removeNil (cdr l))))
;         (T (cons (removeNil (car l)) (removeNil (cdr l))))
;     )
; )


(defun removeE(l e)
    (cond
        ((and (atom l) (equal l e)) nil)
        ((atom l) (list l))
        (T (list (mapcan #'(lambda (l) (removeE l e)) l)))
    )
)

(defun main (l e)
    (car (removeE l e))
)


