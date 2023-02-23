insert(E, L, [E|L]).
insert(E, [H|T], [H|R]):-
    insert(E, T, R).

 

arr([E|_], 1, [E]).
arr([_|T], K, R):-
    arr(T, K, R).
arr([H|T], K, R1):-
    K > 1,
    K1 is K - 1,
    arr(T, K1, R),
    insert(H, R, R1).