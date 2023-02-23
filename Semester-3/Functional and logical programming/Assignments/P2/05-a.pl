% Write a predicate to substitute in a list a value with all the elements of another list.


% flow(i, i, o).
% concat(L1: list, L2: list, R: list).

% Mathematical model:
%                                               - [], n = 0, m = 0
% concat(l1 l2 l3 .. ln, k1 k2 k3 .. km)    =   - l1 l2 .. ln, n > 0, m = 0
%                                               - k1 k2 ... km, n = 0, m > 0
%                                               - l1 U concat(l2 l3 .. ln, k1 k2 ... km)
%

concat([], L2, L2).
concat([H | T], L2, [H | R]) :-
    concat(T, L2, R).


% flow(i, i, i, o)
% replace(L1: list, L2: list, X: integer, R: list).

% Mathematical model:
%                                               - [], n = 0
% replace(l1 l2 .. ln, k1 k2 ... km, X)     =   - l1 U repalce(l2 l3 .. ln, k1 k2 ... km, X), if l1 != X
%                                               - [k1 k2 ... km] U replace(l2 l3 ... ln, k1 k2 ... km, X), if l1 = X 
%

replace([], [], _, []).
replace([], [_ | _], _, []).

replace([H | T], L, H, R) :-
    replace(T, L, H, R1),
    concat(L, R1, R).


replace([H | T], L, X, R) :-
    H =\= X,
    replace(T, L, X, R1),
    concat([H], R1, R).


