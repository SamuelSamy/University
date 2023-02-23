% a. Define a predicate to determine the longest sequences of consecutive even numbers 
% (if exist more maximal sequences one of them).

%  [2, 1, 4, 6, 7] -> [4, 6]

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
