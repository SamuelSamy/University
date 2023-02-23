% a. For a list of integer numbers, define a predicate to write twice in list every prime number.


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