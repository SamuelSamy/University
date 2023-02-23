% b. For a heterogeneous list, formed from integer numbers and list of numbers, determine the successorMain of a
% sublist considered as a number.

% [1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>
% [1, [2, 4], 4, 5, [6, 8, 0], 10, 11, [1, 2, 1], 6]

append([], E, [E]) :- !.
append([H | T], E, [H | R]) :-
    append(T, E, R).

reverse([], []) :- !.
reverse([H | T], R) :-
    reverse(T, R1),
    append(R1, H, R).


sumDigits(D1, D2, PC, NC, R) :-
    S is D1 + D2 + PC,
    R is S mod 10,
    NC is S // 10.

successorMain([], 0, []) :- !.
successorMain([], 1, [1]) :- !.
successorMain(L, 0, L) :- !.
successorMain([H | T], C, [ND | R]) :-
    sumDigits(H, 0, C, NC, ND),
    successorMain(T, NC, R).


successor(L, R) :-
    reverse(L, NL),
    successorMain(NL, 1, RR),
    reverse(RR, R).


main([], []) :- !.
main([H | T], [SR | R]) :-
    is_list(H), !,
    successor(H, SR),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).