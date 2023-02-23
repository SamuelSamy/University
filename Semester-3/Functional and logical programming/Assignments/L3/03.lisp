;; 3. Define a function totests the membership of a node in a n-tree represented as 
;; (root list_of_nodes_subtree1 ... list_of_nodes_subtreen) 
;; Eg. tree is (a (b (c)) (d) (E (f))) and the node is "b" => true


(defun countNode (tree node)
    (cond
        ((and (atom tree) (equal tree node)) 1)
        ((atom tree) 0)
        (t (apply '+ (mapcar #'(lambda (newTree) (countNode newTree node)) tree)))
    )
)

(defun main (tree node)
    (cond
        ((equalp (countNode tree node) 0) nil)
        (t t)
    )
)