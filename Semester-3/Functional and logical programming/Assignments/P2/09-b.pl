% b. For a heterogeneous list, formed from integer numbers and list of numbers; add in every sublist after 1-st,
% 3-rd, 7-th, 15-th element the value found before the sublist in the heterogenous list. The list has the particularity
% that starts with a number and there aren’t two consecutive elements lists.

% [1, [2, 3], 7, [4, 1, 4], 3, 6, [7, 5, 1, 3, 9, 8, 2, 7], 5] =>
% [1, [2, 1, 3], 7, [4, 7, 1, 4, 7], 3, 6, [7, 6, 5, 1, 6, 3, 9, 8, 2, 6, 7], 5].


addValueMain([], _, _, _, []).
addValueMain([H | T], E, I, I, [H, E | R]) :-
    !, NI is I * 2 + 1,
    I1 is I + 1,
    addValueMain(T, E, I1, NI, R).

addValueMain([H | T], E, I, NI, [H | R]) :-
    I1 is I + 1,
    addValueMain(T, E, I1, NI, R).

addValue(L, E, R) :-
    addValueMain(L, E, 1, 1, R).


main([], []) :- !.
main([H], [H]) :- !.
main([H1, H2 | T], [H1, RR | R]) :-
    number(H1), is_list(H2), !,
    addValue(H2, H1, RR),
    main(T, R).

main([H1, H2 | T], [H1 | R]) :-
    main([H2 | T], R).