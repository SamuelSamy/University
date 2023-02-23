% remove n-th element of a list 

% flow(i, i, o).
% remove(L: list, N: integer, R: list).

% Mathematical model:
%                               - 0, if n = 0
% remove(l1 l2 ... ln, P)   =   - l1 U remove(l2 l3 ... ln, P - 1), if P > 1
%                               - l2 l3 ... ln, if P = 1

remove([], X, []) :-
    X > 1.

remove([_ | T], 1, T).

remove([H | T], N, [H | R]) :-
    N > 1,
    N1 is N - 1,
    remove(T, N1, R).