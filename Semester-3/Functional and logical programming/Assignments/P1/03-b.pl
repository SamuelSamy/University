% b. Remove all occurrence of a maximum value from a list on integer numbers.

max(A, B, A) :- 
    A >= B.

max(A, B, B) :-
    A < B.

maxList([H], H).
maxList([H | T], R) :-
    maxList(T, R1),
    max(H, R1, R).


removeOccurrences([], _, []).

removeOccurrences([V | T], V, R) :-
    removeOccurrences(T, V, R).

removeOccurrences([H | T], V, [H | R]) :-
    removeOccurrences(T, V, R).


removeMax(L, R) :-
    maxList(L, M),
    removeOccurrences(L, M, R).
