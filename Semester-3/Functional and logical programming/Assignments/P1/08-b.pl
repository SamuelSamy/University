% b. Write a predicate to delete first occurrence of the minimum number from a list.

min(A, B, A) :- 
    A < B.

min(A, B, B) :-
    A >= B.

minList([H], H).
minList([H | T], R) :-
    minList(T, R1),
    min(R1, H, R).


removeFirstMinimMain([], _, []).
removeFirstMinimMain([H | T], H, T) :- !.
removeFirstMinimMain([H | T], V, [H | R]) :-
    removeFirstMinimMain(T, V, R).


removeFirstMinim([], []).
removeFirstMinim(L, R) :-
    minList(L, M),
    removeFirstMinimMain(L, M, R).