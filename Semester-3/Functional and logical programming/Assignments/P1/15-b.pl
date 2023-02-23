% b.  Write  a  predicate  to  decompose  a  list  in  a  list  respecting  the  following:  
% - [list  of  even  numbers  list  of  odd numbers]
% - also return the number ofeven numbers and the numbers of odd numbers.


splitNumbers([], [], [], 0, 0).
splitNumbers([H | T], LO, [H | LE], CO, CE) :-
    H mod 2 =:= 0, !,
    splitNumbers(T, LO, LE, CO, CE1),
    CE is CE1 + 1.

splitNumbers([H | T], [H | LO], LE, CO, CE) :-
    splitNumbers(T, LO, LE, CO1, CE),
    CO is CO1 + 1.