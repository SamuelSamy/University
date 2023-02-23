;; (A 2 B 0 C 2 D 0 E 0)

;; (B 0 C 2 D 0 E 0)
;; (A 2)

;; found B 0, do something with B, idk gl
;; (A 1 C 2 D 0 E 0)
;; ()

;; (C 2 D 0 E 0)
;; (A 1)

;; (D 0 E 0)
;; (C 2 A 1)

;; found D 0
;; (C 1 E 0)
;; (A 1)

;; (E 0)
;; (C 1 A 1)

;; found E
;; (C 0)
;; (A 1)


;; found C
;; (A 0)
;; ()

;; found A
;; ()
;; ()

;; poti sterge tot ce ii mai sus de asta, ii numa sa vezi cum functioneaza programu

;; myAppend(l1 l2 ... ln, v1 v2 ... vm) = v1 v2 ... vm, if n = 0
;;                                        l1 l2 ... ln, if m = 0
;;                                        l1 U myAppend(l2 ... ln, v1 v2 ... vm), if n > 1 (n is list)
;;                                        l1 U {v1 v2 ... vm}, if n = 1 and m > 1 (m is list, n is atom)
;;                                        l1 U v1, if n = 1, m = 1 (n and m are both atoms)
(defun myAppend (first second)
    (cond
        ((null first) second)
        ((null second) first)
        ((listp first) (cons (car first) (myAppend (cdr first) second)))
        ((listp second) (cons first second))
        (t (cons first second))        
    )
)

;; findNodesOnlevel(l1 v1 l2 v2 ... ln vn, k, s1 vs1 s2 vs2 ... sm vsm, currentLevel) = nil, if n = 0
;;                                                                                      {l1}, if n = 1 and m = 0 and k = currentLevel
;;                                                                                      nil, if n = 1 and m = 0 and k != currentLevel
;;                                                                                      {l1} U findNodesOnlevel(s1 (sv1 - 1) l2 v2 ... ln vn, k, s2 vs2 ... sm vsm, currentLevel - 1), if k = currentLevel and v1 = 0
;;                                                                                      findNodesOnlevel(s1 (sv1 - 1) l2 v2 ... ln vn, k, s2 vs2 ... sm vsm, currentLevel - 1), if k != currentLevel and v1 = 0
;;                                                                                      findNodesOnLevel(l2 v2 ... ln vn, k, {l1, v1} U s1 vs1 s2 vs2 ... sm vsm, currentLevel + 1), otherwise

(defun findNodesOnLevel (tree k stack currentLevel)
    (cond
        ((null tree) nil)
        ((and (null (cddr tree)) (null stack) (= k currentLevel)) (car tree))
        ((and (null (cddr tree)) (null stack)) nil)
        ((and (= k currentLevel) (= (cadr tree) 0)) (myAppend (list (car tree)) (findNodesOnLevel (myAppend (list (car stack) (- 1 (cadr stack))) (cddr tree)) k (cddr stack) (- currentLevel 1))))
        ((= (cadr tree) 0) (findNodesOnLevel (myAppend (list (car stack) (- (cadr stack) 1)) (cddr tree)) k (cddr stack) (- currentLevel 1)))
        (t (findNodesOnLevel (cddr tree) k (myAppend (list (car tree) (cadr tree)) stack) (+ 1 currentLevel)))
    )
)

;; main(tree, k) = findNodesOnLevel(tree, k, nil, 0)
(defun main (tree k)
    (findNodesOnLevel tree k nil 0)
)