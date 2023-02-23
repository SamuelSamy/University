;; '(A (B) (C (D) (E))) 'E
;; '(A (B) (C (D) (E (G (I (J))) (H)))) 'J
;; ;; '(A (B) (C (D) (E (G (I (J))) (H (K) (L (M)))))) 'H

;; myAppend(l1 l2 ... ln, list2)    = list2, if n = 0 AND list2 is a list
    ;;                              = {list2}, if n = 0 AND list2 is not a list
    ;;                              = l1 l2 ... ln, if list2 is null AND `l1 l2 ... ln` is a list
    ;;                              = {l1 l2 ... ln}, if list2 is null AND `l1 l2 ... ln` is not a list
    ;;                              = l1 U myAppend(l2 .. ln, list2), otherwise
(defun myAppend (vector1 vector2)
    (cond
        ((null vector1)
            (cond
                ((listp vector2) vector2)
                (t (list vector2))
            )
        )
        ((null vector2)
            (cond
                ((listp vector1) vector1)
                (t (list vector1))
            )
        )
        (t (cons (car vector1) (myAppend (cdr vector1) vector2)))
    )

)

;; getGood(l1, l2)  = l1, if l2 is null
;;                  = l2, if l1 is null
;;                  = nil, if both are null
(defun getGood (l1 l2)
    (cond
        ((null l1) l2)
        ((null l2) l1)
        (t nil)
    )
)


;; getPath(root left right, node, path) = path U node, if root == node
;;                                      = nil, if root is null
;;                                      = getGood(getPath(left, node, path U root), getPath(right, node, path U root))
(defun getPath (tree node &optional (path nil))
    (
        (lambda (root left right)
            (cond
                ((equal root node) (myAppend path node))
                ((null tree) nil)
                (t (getGood (getPath left node (myAppend path root)) (getPath right node (myAppend path root))))
            )
        )
        (car tree) (cadr tree) (caddr tree)
    )
)