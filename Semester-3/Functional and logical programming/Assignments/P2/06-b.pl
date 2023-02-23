% b. For a heterogeneous list, formed from integer numbers and list of numbers, write a predicate to replace
% every sublist with the position of the maximum element from that sublist.

% [1, [2, 3], [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, [2], [1, 3], 3, 6, [2], 5, [1, 2, 3], 7]


max(A, B, A) :- A > B, !.
max(_, B, B).

maxList([H], H) :- !.
maxList([H | T], R) :-
    maxList(T, RR),
    max(H, RR, R).

findMaxPosMain([], _, _, []).
findMaxPosMain([M | T], M, I, [I | R]) :-
    !, I1 is I + 1,
    findMaxPosMain(T, M, I1, R).

findMaxPosMain([_ | T], M, I, R) :-
    I1 is I + 1,
    findMaxPosMain(T, M, I1, R).

findMaxPos(L, R) :-
    maxList(L, M),
    findMaxPosMain(L, M, 1, R).   

main([], []) :- !.
main([H | T], [PL | R]) :-
    is_list(H), !,
    findMaxPos(H, PL),
    main(T, R).


main([H | T], [H | R]) :-
    main(T, R).