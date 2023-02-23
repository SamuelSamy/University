% b. For a heterogeneous list, formed from integer numbers and list of digits, write a predicate to compute the
% sum of all numbers represented as sublists.
% [1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] => [8, 2, 2].


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


sumSublists([], []).
sumSublists([H | T], R) :-
    is_list(H), !,
    reverse(H, L),
    sumSublists(T, R1),
    sum(L, R1, 0, R).

sumSublists([_ | T], R) :- 
    sumSublists(T, R).

main(L, R) :-
    sumSublists(L, R1),
    reverse(R1, R).