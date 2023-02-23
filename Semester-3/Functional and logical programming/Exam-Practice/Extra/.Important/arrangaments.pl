insert(E, L, [E | L]).
insert(E, [H | T], [H | R]) :-
    insert(E, T, R).

arr([E | _], 1, [E]).

arr([_ | T], K, R) :-
    arr(T, K, R).

arr([H | T], K, R) :-
    K > 1,
    K1 is K - 1,
    arr(T, K1, R1),
    insert(H, R1, R).

getLength([], 0).
getLength([_ | T], N) :-
    getLength(T, N1),
    N is N1 + 1.

getSum([], 0).
getSum([H | T], S) :-
    getSum(T, S1),
    S is S1 + 1.


verify([]).
verify(L) :-
    getLength(L, N),
    N mod 2 =:= 0,
    getSum(L, S),
    S mod 2 =:= 1.


main(L, K, R) :-
    findall(R1, arr(L, K, R1), R).