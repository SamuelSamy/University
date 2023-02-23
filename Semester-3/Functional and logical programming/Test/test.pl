
% max(a, b) =   a, if a > b
%               b, otherwise

% flow max(i, i, o)
max(A, B, A) :- A > B, !.
max(_, B, B).

% maxList(l1 l2 ... ln) =   0, if n = 0
%                           ln if n = 1,
%                           max(l1, maxList(l2 l3 ... ln)), otherwise

% flow maxList(i, o)
maxList([], 0) :- !.
maxList([H], H) :- !.
maxList([H | T], R) :-
    maxList(T, R1),
    max(H, R1, R).


% removeOccurrences(l1 l2 ... ln, e)    =   [], if n = 0
%                                           removeOccurrences(l2 l3 ... ln), if l1 == e
%                                           l1 U removeOccurrences(l2 l3 ... ln), if l1 != e

%flow removOccurrences(i, i, o)
removeOccurrences([], _, []).
removeOccurrences([H | T], H, R) :-
    !, removeOccurrences(T, H, R).

removeOccurrences([H | T], E, [H | R]) :-
    removeOccurrences(T, E, R).


% main(l1 l2 ... ln) = removeOccurrences(l1 l2 ... ln, maxList(l1 l2 ... ln))

% flow main(i, o)
main(L, R) :-
    maxList(L, M),
    removeOccurrences(L, M, R).