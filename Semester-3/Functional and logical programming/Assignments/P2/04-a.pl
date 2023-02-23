% a. Write a predicate to determine the sum of two numbers written in list representation


append([], E, [E]) :- !.
append([H | T], E, [H | R]) :-
    append(T, E, R).

reverse([], []) :- !.
reverse([H | T], R) :-
    reverse(T, R1),
    append(R1, H, R).


sumDigits(D1, D2, PC, R, C) :-
    S is D1 + D2 + PC,
    R is S mod 10,
    C is S // 10.


sum([], [], 0, []) :- !.
sum([], [], 1, [1]) :- !.
sum([], [H | T], C, [D | R]) :-
    sumDigits(0, H, C, D, NC),
    sum([], T, NC, R).

sum([H | T], [], C, [D | R]) :-
    sumDigits(H, 0, C, D, NC),
    sum(T, [], NC, R), !.

sum([H1 | T1], [H2 | T2], C, [D | R]) :-
    sumDigits(H1, H2, C, D, NC),
    sum(T1, T2, NC, R).


main(L1, L2, R) :-
    reverse(L1, NL1),
    reverse(L2, NL2),
    sum(NL1, NL2, 0, RR),
    reverse(RR, R).