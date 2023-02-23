% b. For a heterogeneous list, formed from integer numbers and list of numbers, define a predicate to determine
% the predecessor of the every sublist considered as numbers.

% [1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>
% [1, [2, 2], 4, 5, [6, 7, 8], 10, 11, [1, 1, 9] 6]

predecessor([], 0, []) :- !.
predecessor([0], 1, [9]) :- !.
predecessor([X], 0, [X1]) :-
    X1 is X - 1, !.

predecessor([0 | T], 1, [9 | R]) :-
    predecessor(T, 1, R), !.

predecessor([H | T], 0, [H1 | R]) :-
    predecessor(T, C, R),
    H1 is H - C.



predecessor(L, R) :-
    predecessor(L, 0, R).

main([], []) :- !.
main([H | T], [RR | R]) :-
    is_list(H), !,
    predecessor(H, RR),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).