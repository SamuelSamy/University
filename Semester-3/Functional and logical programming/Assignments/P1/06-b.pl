% b. Write a predicate to remove the first three occurrences of an element in a list.
% If the element occurs less than three times, all occurrenceswill be removed.

min(A, B, A) :-
    A < B.

min(A, B, B) :-
    A >= B.

countOccurrences([], _, 0).
countOccurrences([V | T], V, C) :-
    !, countOccurrences(T, V, C1),
    C is C1 + 1.

countOccurrences([_ | T], V, C) :-
    countOccurrences(T, V, C). 

removeMain(L, _, 0, L) :- !.

removeMain([E | T], E, C, R) :-
    C =\= 0, !,
    C1 is C - 1,
    removeMain(T, E, C1, R).

removeMain([H | T], E, C, [H | R]) :-
    removeMain(T, E, C, R).

remove(L, E, R) :-
    countOccurrences(L, E, C),
    min(C, 3, C1),
    removeMain(L, E, C1, R).