;; '(A (B (C (X) (Y)) (D (P))) (E (O))) 2

;; getNodesOnLevel (parent leftChild rightChild, k, currentLevel) = {tree}, if k == currentLevel and parent is an atom (if tree is atom => leftChild and rightChild are NIL)
;;                                                                  {nil},  if k != currentLevel and parent is not an atom
;;                                                                  {getNodesOnLevel(parent, k, currentLevel + 1)} U {getNodesOnLevel(leftChild, k, currentLevel + 1)} U {getNodesOnLevel(rightChild, k, currentLevel + 1)}, otherwise

(defun getNodesOnLevel (tree k currentLevel)
    (cond
        ((and (= k currentLevel) (atom tree)) (list tree))
        ((atom tree) nil)
        (t (mapcan #'(lambda (x) (getNodesOnLevel x k (+ 1 currentLevel))) tree))
    )
)

;; main(tree k) = getNodesOnLevel(tree k -1)
(defun main (tree k)
    (getNodesOnLevel tree k -1)
)
