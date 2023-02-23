% a. Determine the product of a number represented as digits in a list to a given digit.
% [1 9 3 5 9 9] * 2 => [3 8 7 1 9 8]

append([], E, [E]) :- !.
append([H | T], E, [H | R]) :-
    append(T, E, R).

reverse([], []).
reverse([H | T], R) :-
    reverse(T, R1),
    append(R1, H, R).


productDigits(D1, D2, PC, R, NC) :-
    P is D1 * D2 + PC,
    R is P mod 10,
    NC is P // 10.

product([], _, _, []) :- !.
product([H | T], N, PC, [ND | R]) :-
    productDigits(H, N, PC, ND, NC),
    product(T, N, NC, R).


main(L, D, R) :-
    reverse(L, NL),
    product(NL, D, 0, RR),
    reverse(RR, R).