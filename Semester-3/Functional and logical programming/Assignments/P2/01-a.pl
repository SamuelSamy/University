% a. Sort a list with removing the double values. E.g.: [4 2 6 2 3 4] --> [2 3 4 6]

min(A, B, A) :- A < B, !.
min(_, B, B).

minList([E], E).
minList([H | T], R) :-
    minList(T, R1),
    min(H, R1, R).

removeOccurrences([], _, []).
removeOccurrences([H | T], H, R) :-
    !, removeOccurrences(T, H, R).

removeOccurrences([H | T], E, [H | R]) :-
    removeOccurrences(T, E, R).


mySort([], []).
mySort(L, [V | R]) :-
    minList(L, V),
    removeOccurrences(L, V, LR),
    mySort(LR, R).