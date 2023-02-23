% a. Replace all occurrences of an element from a list with another element e

replace([], _, _, []) :- !.
replace([H | T], H, E, [E | R]) :-
    !, replace(T, H, E, R).
replace([H | T], V, E, [H | R]) :-
    replace(T, V, E, R).