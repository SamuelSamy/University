% a. Write a predicate to compute the intersection of two sets.

isInList([], _) :- false, !.
isInList([E | _], E) :- true, !.
isInList([_ | T], E) :-
    isInList(T, E).

intersection([], _, []).
intersection([H | T], L, [H | R]) :-
    isInList(L, H), !,
    intersection(T, L, R).

intersection([_ | T], L, R) :-
    intersection(T, L, R).