(defun myList (&rest args)
    (return-from myList args)
)

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
