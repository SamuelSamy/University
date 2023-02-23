% a. Determine the successor of a number represented as digits in a list.
% [1 9 3 5 9 9] --> [1 9 3 6 0 0]


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

successor([], 0, []) :- !.
successor([], 1, [1]) :- !.
successor(L, 0, L) :- !.
successor([H | T], C, [ND | R]) :-
    sumDigits(H, 0, C, NC, ND),
    successor(T, NC, R).


main(L, R) :-
    reverse(L, NL),
    successor(NL, 1, RR),
    reverse(RR, R).