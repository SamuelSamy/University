;; 12. Determine the list of nodes accesed in preorder in a tree of type (2).
;; '(A (B) (C (D) (E)))


;; helper functions ;)

;; myList(l1 l2 ... ln) = {l1, l2, ..., ln}
(defun myList (&rest args)
    (return-from myList args)
)

;; myAppendTwo(l1 l2 ... ln, p1 p2 ... pm)  = {p1, p2, ..., pm}, if n = 0
;;                                          = {l1} U myAppendTwo(l2 l3 ... ln, p1 p2 ... pm), if (l1 l2 ... ln) is list and n != 0
;;                                          = {l1} U {p1, p2, ... pm}, if l1 is not a list and p2 is a list
;;                                          = {l1} U {p1}, if neither l1 and p1 are not a list
(defun myAppendTwo (first second)
    (cond
        ((null first) second)
        ((listp first) (cons (car first) (myAppendTwo (cdr first) second)))
        ((listp second) (cons first second))
        (t (cons first second))        
    )
)

;; myAppendUnder(l1 l2 ... ln)  = {}, if n = 0
;;                              = {l1}, if n = 1
;;                              = myAppendTwo(myAppendTwo(l1, l2), myAppendUnder(l3 l4 ... ln))
(defun myAppendUnder (args)
    (cond
        ((null args) nil)
        ((null (cdr args)) (car args))
        (t (myAppendTwo (myAppendTwo (car args) (cadr args)) (myAppendUnder (cddr args))))
    )
)

;; myAppend(l1 l2 ... ln) = myAppendUnder(l1 l2 ... ln)
(defun myAppend(&rest args)
    (myAppendUnder args)
)


;; inorder(l1 l2 l3)    = nil if l1 is empty
;;                      = myAppend(preroder(l2), l1, preorder(3)), otherwise
(defun inorder (vector)
    (cond
        ((null vector) nil)
        (t (myAppend (inorder (cadr vector)) (car vector) (inorder (caddr vector))))
    )

)

;; preorder(l1 l2 l3)   = nil if l1 is empty
;;                      = myAppend(l1, preorder(l2), preorder(l3)), otherwise
(defun preorder (vector)
    (cond
        ((null vector) nil)
        (t (myAppend (car vector) (preorder (cadr vector)) (preorder (caddr vector))))
    )

)

;; postorder(l1 l2 l3)  = nil if l1 is empty
;;                      = myAppend(postorder(l2), postorder(l3), l1), otherwise
(defun postorder (vector)
    (cond
        ((null vector) nil)
        (t (myAppend (preorder (cadr vector)) (preorder (caddr vector)) (car vector)))
    )
)
