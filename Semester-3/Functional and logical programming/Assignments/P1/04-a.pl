% a. Write a predicate to determine the difference of two sets

isInList([], _) :- false.
isInList([E | _], E) :-
    true, !.
isInList([_ | T], E) :-
    isInList(T, E).

difference([], _, []).
difference([HS1 | TS1], S2, R) :-
    isInList(S2, HS1), !,
    difference(TS1, S2, R).

difference([HS1 | TS1], S2, [HS1 | R]) :-
    difference(TS1, S2, R).