
(defun myMax (a b)
    (cond
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((>= a b) a)
        (t b)
    )
) 

;; '(A (B (D (G) (H)) (E (I))) (C (F) (X)))
;; '(A (B (D) (E)) (C (F) (G)))

(defun countNodesOnLevel (vector level &optional (nodes 0) (currentLevel 0))
    (cond
        ((null vector) 0)
        ((equal level currentLevel) (+ 1 nodes))
        (t (+ (countNodesOnLevel (cadr vector) level 0 (+ 1 currentLevel)) (countNodesOnLevel  (caddr vector) level 0 (+ 1 currentLevel))))
    )
)

(defun getTotalLevels (vector &optional (totalLevels 0))
    (cond
        ((null vector) totalLevels)
        (t (myMax (getTotalLevels (cadr vector) (+ 1 totalLevels)) (getTotalLevels (caddr vector) (+ 1 totalLevels))))
    )
)

(defun getMaxChildren (vector maxChildren totalLevels &optional (currentLevel 0))
    (cond
        ((equal totalLevels currentLevel) maxchildren)
        ((> (countNodesOnLevel vector currentLevel) maxchildren) (getMaxLevel vector (countNodesOnLevel vector currentLevel) totalLevels (+ 1 currentLevel)))
        (t (getMaxLevel vector maxchildren totalLevels (+ 1 currentLevel)))
    )
)

(defun getMaxLevel (vector maxChildren totalLevels &optional (currentLevel 0))
    (cond
        ((equal totalLevels currentLevel) currentLevel)
        ((= (countNodesOnLevel vector currentLevel) maxchildren) currentLevel)
        (t (getMaxLevel vector maxchildren totalLevels (+ 1 currentLevel)))
    )
)

(defun getNodesOnLevel (vector level &optional (currentLevel 0))
    (cond
        ((null vector) nil)
        ((equal level currentLevel) (list (car vector)))
        (t (append (getNodesOnLevel (cadr vector) level (+ 1 currentLevel)) (getNodesOnLevel (caddr vector) level (+ 1 currentLevel))))
    )
)

(defun main (vector)
    (format t "Nivelul cu cele mai multe noduri: ~s~%" (getMaxLevel vector (getMaxChildren vector 0 (getTotalLevels vector)) (getTotalLevels vector)))
    (getNodesOnLevel vector (getMaxLevel vector (getMaxChildren vector 0 (getTotalLevels vector)) (getTotalLevels vector)))
)
