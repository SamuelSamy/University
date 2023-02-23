% b. Write a predicate to add a value v after 1-st, 2-nd, 4-th, 8-th, ... element in a list.

addValueMain([], _, _, _, []).
addValueMain([H | T], I, N, V, [H | [V | R]]) :-
    I =:= N, !,
    N1 is N + 1,
    I1 is I + I,
    addValueMain(T, I1, N1, V, R).

addValueMain([H | T], I, N, V, [H | R]) :-
    N1 is N + 1,
    addValueMain(T, I, N1, V, R).


addValue(L, V, R) :-
    addValueMain(L, 1, 1, V, R).
