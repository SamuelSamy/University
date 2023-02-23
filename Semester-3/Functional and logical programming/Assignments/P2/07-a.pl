% a. Determine the position of the maximal element of a linear list.
% maxpos([10,14,12,13,14], L) produces L = [2,5].

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