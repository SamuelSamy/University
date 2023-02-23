
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


% flow(i, i, o)
% replace_sublists(L1: list, L2: list, R: List)

% Mathematical model:
%                                                       - [], n = 0
% replace_sublists(l1 l2 ... ln, k1 k2 ... km)      =   - l1 U replace_sublists(l2 l3 ... ln, k1 k2 ... kn), if l1 is a number
%                                                       - replace(l11 l12 ... l1m, k1 k2 ... kn, l11) U replace_sublists(l2 l3 ... ln, k1 k2 ... kn) if l1 is a list


replace_sublists([], [_ | _], []).

replace_sublists([[H | T0] | T1], L, [HR | R]) :-
    replace([H | T0], L, H, HR),
    replace_sublists(T1, L, R).

replace_sublists([H | T], L, [H | R]) :-
    number(H),
    replace_sublists(T, L, R).