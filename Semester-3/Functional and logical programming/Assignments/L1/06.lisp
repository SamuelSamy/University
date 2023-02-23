;; a) Write a function to test whether a list is linear.
;; '(1 2 3 4)  -> T
;; (1 (2 3 4)) -> nil

(defun testLiniarity (vector)
    (cond
        ((null vector) t)
        ((listp (car vector)) nil)
        (t (testLiniarity (cdr vector)))
    )
)


;; b) Write a function to replace the first occurence of an element E in a given listwith an other element O.
;;  '(1 2 3 (4) 5 4) 4 0 -> '(1 2 3 (0) 5 4)

(defun countOccurrences (vector element)
    (cond
        ((null vector) 0)
        ((equal (car vector) element) (+ 1 (countOccurrences (cdr vector) element)))
        (t (countOccurrences (cdr vector) element))
    )
)

(defun replaceFirst (vector element other &optional (found 0))
    (cond
        ((null vector) nil)
        ((listp (car vector)) (cons (replaceFirst (car vector) element other found) (replaceFirst (cdr vector) element other (+ found (countOccurrences (car vector) element)))))
        ((and (equal (car vector) element) (= 0 found)) (cons other (cdr vector)))
        (t (cons (car vector) (replaceFirst (cdr vector) element other found)))
    )
)


;; c) Write a function to replace each sublist of a list with its last element.
;; A sublist is an element from the first level, which is a list.
;; Example: (a (b c) (d (e (f)))) ==> (a c (e (f))) ==> (a c (f)) ==> (a c f)(a (b c) (d ((e) f))) ==> (a c ((e) f)) ==> (a c f)

; (defun getLastHelper (vector)
;     (cond
;         ((null (cdr vector)) (car vector))
;         (t (getLastHelper (cdr vector)))
;     )
; )

; (defun getLast (vector)
;     (cond
;         ((atom vector) vector)
;         (t (getLast (getLastHelper vector)))
;     )
; )

(defun getLast (vector)
    (cond
        ((and (null (cdr vector)) (atom vector)) vector)
        ((null (cdr vector)) (getLast (car vector)))
        (t (getLast (cdr vector)))
    )
)

(defun replaceSublists (vector)
    (format t "~a~%" vector)
    (cond
        ((null vector) nil)
        ;;((atom vector) vector)
        ((atom (car vector)) (cons (car vector) (replaceSublists (cdr vector))))
        ((listp (car vector)) (cons (getLast (car vector)) (replaceSublists (cdr vector))))
    )
)


;; d) Write a function to merge two sorted lists without keeping double values.
;; '(1 2 8 12) '(1 3 5 9 13) -> '(2 3 5 8 9 12 13)

(defun mergeSort (vector1 vector2)
    (cond
        ((and (null vector1) (null vector2)) nil)
        ((null vector1) vector2)
        ((null vector2) vector1)
        ((= (car vector1) (car vector2)) (mergeSort (cdr vector1) (cdr vector2)))
        ((< (car vector1) (car vector2)) (cons (car vector1) (mergeSort (cdr vector1) vector2)))
        ((> (car vector1) (car vector2)) (cons (car vector2) (mergeSort vector1 (cdr vector2))))
    )
)

