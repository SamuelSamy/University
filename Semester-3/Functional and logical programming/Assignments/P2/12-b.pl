% b. For a heterogeneous list, formed from integer numbers and list of numbers, define a predicate to add in
% every sublist the divisors of every element.

% [1, [2, 5, 7], 4, 5, [1, 4], 3, 2, [6, 2, 1], 4, [7, 2, 8, 1], 2] =>
% [1, [2, 5, 7], 4, 5, [1, 4, 2], 3, 2, [6, 2, 3, 2, 1], 4, [7, 2, 8, 2, 4, 1], 2]


concat([], [], []) :- !.
concat([], L, L) :- !.
concat(L, [], L) :- !.

concat([H | T], L, [H | R]) :-
    concat(T, L, R).


divisorsMain(N, D, []) :-
    D >= N, !.

divisorsMain(N, D, [D | R]) :-
    N mod D =:= 0, !,
    D1 is D + 1,
    divisorsMain(N, D1, R).

divisorsMain(N, D, R) :-
    D1 is D + 1,
    divisorsMain(N, D1, R).

divisors(N, R) :-
    divisorsMain(N, 2, R).


addDivisors([], []) :- !.
addDivisors([H | T], R) :-
    divisors(H, DL),
    addDivisors(T, R2),
    concat([H], DL, RR),
    concat(RR, R2, R).


main([], []) :- !.
main([H | T], [RR | R]) :-
    is_list(H), !,
    addDivisors(H, RR),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).