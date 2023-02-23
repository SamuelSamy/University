;; 1. Definiti o functie care obtine dintr-o lista data lista tutoror atomilor nenumerici care apar
;; pe orice nivel, dar in ordine inversa
;; '(((A B) 2 C) 3 (D 1 E)) -> (E D C B A)

(defun myAppendSimple (first second)
    (cond
        ((null first)
            (cond
                ((listp second) second)
                (t (list second))
            )
        )
        ((null second)
            (cond
                ((listp first) first)
                (t (list first))
            )
        )
        (t (cons (car first) (myAppendSimple (cdr first) second)))
    )
)


(defun getAtoms (vector)
    (
        (lambda (first rest)
            (cond
                ((null vector) nil)
                ((numberp first) (getAtoms rest))
                ((atom first) (myAppendSimple (getAtoms rest) first))
                (t (myAppendSimple (getAtoms rest) (getAtoms first)))
            )
        )
        (car vector) (cdr vector)
    )
)