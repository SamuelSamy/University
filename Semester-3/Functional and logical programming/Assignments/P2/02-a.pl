% a. Sort a list with keeping double values in resulted list. E.g.: [4 2 6 2 3 4] --> [2 2 3 4 4 6]


min(A, B, A) :- A < B, !.
min(_, B, B).

minList([E], E).
minList([H | T], R) :-
    minList(T, R1),
    min(H, R1, R).

removeFirstElement([], _, []).
removeFirstElement([H | T], H, T) :- !.
removeFirstElement([H | T], E, [H | R]) :-
    removeFirstElement(T, E, R).


mySort([], []) :- !.
mySort(L, [V | R]) :-
    minList(L, V),
    removeFirstElement(L, V, LR),
    mySort(LR, R).