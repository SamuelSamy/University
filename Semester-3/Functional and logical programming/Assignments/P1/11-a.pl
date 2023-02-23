% a. Write a predicate to substitute an element from a list with another element in the list.

substitute([], _, _, []).
substitute([H | T], H, E, [E | R]) :-
    !, substitute(T, H, E, R).

substitute([H | T], E1, E2, [H | R]) :-
    substitute(T, E1, E2, R).