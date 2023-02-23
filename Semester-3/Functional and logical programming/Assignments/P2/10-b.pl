% b. For a heterogeneous list, formed from integer numbers and list of numbers, define a predicate to write in
% every sublist twice every prime number.

% [1, [2, 3], 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5] =>
% [1, [2, 2, 3, 3], 4, 5, [1, 4, 6], 3, [1, 3, 3, 7, 7, 9, 10], 5]

primeMain(N, D) :-
    N mod D =\= 0,
    D >= N // 2, 
    true, !.

primeMain(N, D) :-
    N mod D =\= 0,
    D =< N // 2,
    D1 is D + 2,
    primeMain(N, D1).


prime(2) :- true, !.
prime(3) :- true, !.
prime(N) :- N < 2, !, false.
prime(N) :- 
    N mod 2 =\= 0,
    primeMain(N, 3).

duplicatePrimes([], []).
duplicatePrimes([H | T], [H, H | R]) :-
    prime(H), !,
    duplicatePrimes(T, R).

duplicatePrimes([H | T], [H | R]) :-
    duplicatePrimes(T, R).

main([], []) :- !.
main([H | T], [RR | R]) :-
    is_list(H), !,
    duplicatePrimes(H, RR),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).