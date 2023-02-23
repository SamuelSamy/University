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

;; myMax(a, b)  = a, if b is not a number or a >= b
;;              = b, otherwise
(defun myMax (a b)
    (cond
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((>= a b) a)
        (t b)
    )
) 


;; a) Write a recursive function to return the sum of two vectors.

;; sumVectors(l1 l2 ... ln, p1 p2 ... pm)   = {p1, p2, ..., pm} if n = 0
;;                                          = {l1, l2, ..., ln} if m = 0
;;                                          = {l1 + l2} U sumVectors(l2 l3 ... ln, p2 p3 ... pn), otherwise
(defun sumVectors (vector1 vector2)
    (cond
        ((null vector1) vector2)
        ((null vector2) vector1)
        (t (cons (+ (car vector1) (car vector2)) (sumvectors (cdr vector1) (cdr vector2)))) 
    )
)

;; b) Write a function to get from a given list the list of all atoms, on any level, but on the same order. 
;; Example: (((A B) C) (D E)) ==> (A B C D E)

;; exploseList(l1 l2 ... ln)    = {}, if n = 0
;;                              = {l1} U explodeList(l2 l3 ... ln), if l1 is an aton
;;                              = explodeList(l1) U explodeList(l2 l3 ... ln), if l1 is a list
(defun explodeList (vector)
    (cond
        ((null vector) nil)
        ((atom (car vector)) (cons (car vector) (explodelist (cdr vector))))
        ((listp (car vector)) (myAppend (explodeList (car vector)) (explodeList (cdr vector))))
    )
)


;; c) Write a function that, with a list given as parameter, inverts only continuoussequences of atoms. 
;; Example:(a b c (d (e f) g h i)) ==> (c b a (d (f e) i h g))

;; invertList(l1 l2 ... ln, result) = result, if n = 0
;;                                  = invertList(l2 l3 ... ln, {l1} U result), if l1 is an atom
;;                                  = result U invertList(l1, {}) U invertList(l2 l3 ... ln, {}), if l1 is a list
(defun invertList (vector &optional (result nil))
    (cond
        ((null vector) result)
        ((atom (car vector)) (invertList (cdr vector) (myAppend (myList (car vector)) result)))
        ((listp (car vector)) (myAppend result (myList (invertList (car vector) nil)) (invertList (cdr vector) nil)))
    )
)


;; d) Write a list to return the maximum value of the numerical atoms from a list, at superficial level.
;; '(1 2 (3 (4 (5 (6) 3) 2) 5) 0 4) -> 4

;; maxSuperficial(l1 l2 ... ln) = nil, if n = 0
;;                              = myMax(l1, maxSuperficial(l2 l3 ... ln)), if l1 is a number
;;                              = myMax(maxSuperficial(l1), maxSuperficial(l2 l3 ... ln)), if l1 is a list
;;                              = maxSuperficial(l2 l3 ... ln), otherwise (l1 is netiher a number nor a list)
(defun maxSuperficial (vector)
    (cond
        ((null vector) nil)
        ((numberp (car vector)) (myMax (car vector) (maxSuperficial (cdr vector))))
        ;;((listp (car vector)) (myMax (maxSuperficial (car vector)) (maxSuperficial (cdr vector))))
        (t (maxSuperficial (cdr vector))) ; it is neither a number nor a list
    )
)