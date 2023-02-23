% a. For a list of integer number, write a predicate to add in list after 1-st, 3-rd, 7-th, 15-th element a given value e

addValue([], _, _, _, []).
addValue([H | T], E, I, I, [H, E | R]) :-
    !, NI is I * 2 + 1,
    I1 is I + 1,
    addValue(T, E, I1, NI, R).

addValue([H | T], E, I, NI, [H | R]) :-
    I1 is I + 1,
    addValue(T, E, I1, NI, R).

main(L, E, R) :-
    addValue(L, E, 1, 1, R).