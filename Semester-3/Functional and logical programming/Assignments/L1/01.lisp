;; a) Write a function to return the n-th element of a list, or NIL if such an element does not exist.

(defun get-n (vector n)
    (cond
        ((equalp n 1) (car vector))
        (t (get-n (cdr vector) (- n 1)))
    )
)


;; b) Write a function to check whether an atom E is amember of a list which is not necessarily linear.

(defun contains (vector element)
    (cond
        ((null vector) nil)
        ((listp (car vector)) (or (contains (car vector) element) (contains (cdr vector) element)))
        ((equalp (car vector) element) t)
        (t (contains (cdr vector) element))
    )
)

;; Write a function to determine the listof all sublists of a given list, on any level. 
;; A sublist is either the list itself, or any element that is a list, at any level. 
;; Example: (1 2 (3 (4 5)(6 7)) 8 (9 10)) => 5 sublists: (  (1 2 (3 (4 5) (6 7)) 8 (9 10))    (3 (4 5) (6 7))     (4 5)   (6 7)   (9 10) )

(defun myAppendTwo (first second)
    (cond
        ((null first) second)
        ((listp first) (cons (car first) (myAppendTwo (cdr first) second)))
        ((listp second) (cons first second))
        (t (cons first second))        
    )
)

(defun myAppendUnder (args)
    (cond
        ((null args) nil)
        ((null (cdr args)) (car args))
        (t (myAppendTwo (myAppendTwo (car args) (cadr args)) (myAppendUnder (cddr args))))
    )
)


(defun myAppend(&rest args)
    (myAppendUnder args)
)


(defun getSublists (vector)
    (cond
        ((null vector) nil)
        ((atom vector) nil)
        ((list vector) (apply 'myAppend (list vector) (mapcar 'getSublists vector)))
    )
)


;; Write a function to transform a linear list into a set.

;; vector - linear list
(defun contains (vector element)
    (cond
        ((null vector) nil)
        ((= (car vector) element) t)
        (t (contains (cdr vector) element))
    )
)

(defun makeSetHelper (vector (result nil))
    (cond
        ((null vector) result)
        ((contains result (car vector)) (makeSetHelper (cdr vector) result))
        (t (makeSetHelper (cdr vector) (myAppend result (car vector) )))
    )
)

(defun makeSet (vector)
    (makeSetHelper vector nil)
)