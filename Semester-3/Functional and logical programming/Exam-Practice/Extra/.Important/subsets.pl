
% subsets(l1 l2 ... ln) = [], if n = 0
%                       = subsets(l2 l3 ... ln), if n > 0
%                       = l1 U subsets(l2 l3 ... ln), if n > 0

subsets([], []).
subsets([_ | T], R) :-
    subsets(T, R).

subsets([H | T], [H | R]) :-
    subsets(T, R).

main(L, R) :-
    findall(R1, subsets(L, R1), R).