;; 13.Define  a  function  that  returns  the  depth  of  a  tree  represented  as 
;; (root  list_of_nodes_subtree1  ... list_of_nodes_subtreen) 
;; Eg. the depth of the tree (a (b (c)) (d) (e (f))) is 3



;; myMax(a, b) = b, if a is not a number
;;               a, if b is not a number
;;               a, if a and b are numbers and a >= b
;;               b, otherwise
(defun myMax (a b)
    (cond
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((>= a b) a)
        (t b)
    )
) 


;; maxInList(l1 l2 ... ln)  =   l1, if n = 1
;;                              myMax(l1, maxInList(l2 l3 ... ln)), otherwise
(defun maxInList (vector)
    (cond
        ((null (cdr vector)) (car vector))
        (t (myMax (car vector) (maxInList (cdr vector))))
    )
)


;; findDepth(parent child1 child2 ... childn, currentDepth) = currentDepth, if n = 0 and parent is an atom
;;                                                            maxInList(list(findDepth(child1, currentDepth + 1), findDepth(child2, currentDepth + 1), ... findDepth(childn, currentDepth + 1)))
(defun findDepth (tree &optional (currentDepth 0))
    (cond
        ((atom tree) currentDepth)
        (t (apply #'maxInList (list (mapcar #'(lambda (x) (findDepth x (+ 1 currentDepth))) tree))))
    )
)
