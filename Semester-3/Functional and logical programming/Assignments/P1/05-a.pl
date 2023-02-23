% a. Write a predicate to compute the union of two sets

isInList([], _) :- false.
isInList([E | _], E) :-
    true, !.
isInList([_ | T], E) :-
    isInList(T, E).


union([], S2, S2).
union([HS1 | TS1], S2, R) :-
    isInList(S2, HS1), !,
    union(TS1, S2, R).

union([HS1 | TS1], S2, [HS1 | R]) :-
    union(TS1, S2, R).
