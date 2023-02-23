% a. Transform a list in a set, in the order of the last occurrences of elements.
% [1,2,3,1,2] is transformed in [3,1,2].

isInList([], _) :- false, !.
isInList([E | _], E) :- true, !.
isInList([_ | T], E) :- 
    isInList(T, E).


transform([], []).
transform([H | T], [H | R]) :-
    not(isInList(T, H)), !,
    transform(T, R).

transform([_ | T], R) :-
    transform(T, R).