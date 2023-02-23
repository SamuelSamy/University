% b. For a heterogeneous list, formed from integer numbers and list of numbers, define a predicate to replace
% every sublist with the longest sequences of even numbers from that sublist.

% [1, [2, 1, 4, 6, 7], 5, [1, 2, 3, 4], 2, [1, 4, 6, 8, 3], 2, [1, 5], 3] =>
% [1, [4, 6], 5, [2], 2, [4, 6, 8], 2, [], 3]



len([], 0) :- !.
len([_ | T], R) :-
    len(T, R1),
    R is R1 + 1.

append([], E, [E]) :- !.
append([H | T], E, [H | R]) :-
    append(T, E, R).

longestSequenceMain([], BR, CR, BR) :-
    len(BR, BRL),
    len(CR, CRL),
    BRL >= CRL, !.

longestSequenceMain([], BR, CR, CR) :-
    len(BR, BRL),
    len(CR, CRL),
    BRL < CRL, !.

longestSequenceMain([H | T], BR, CR, R) :-
    H mod 2 =:= 0, !,
    append(CR, H, CRR),
    longestSequenceMain(T, BR, CRR, R).


longestSequenceMain([_ | T], BR, CR, R) :-
    len(BR, BRL),
    len(CR, CRL),
    BRL >= CRL, !,
    longestSequenceMain(T, BR, [], R).

longestSequenceMain([_ | T], _, CR, R) :-
    longestSequenceMain(T, CR, [], R).


longestSequence(L, R) :-
    longestSequenceMain(L, [], [], R).


main([], []) :- !.
main([H | T], [RR | R]) :-
    is_list(H), !,
    longestSequence(H, RR),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).