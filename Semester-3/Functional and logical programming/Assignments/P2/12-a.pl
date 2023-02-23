% a. Define a predicate to add after every element from a list, the divisors of that number.

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

