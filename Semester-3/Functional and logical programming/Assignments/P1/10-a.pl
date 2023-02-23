% a. Define a predicate to test if a list of an integer elements has a "valley" aspect 
% (a set has a "valley" aspect if elements decreases up to a certain point, and then increases)
% 10 8 6 9 11 13 – has a “valley” aspect


% F = 1 -> increasing
% F = 0 -> decreasing

isValleyMain([_], 1) :- !. 
isValleyMain([H1, H2 | T], F) :-
    H1 > H2,
    F =:= 0,
    isValleyMain([H2 | T], 0).

isValleyMain([H1, H2 | T], _) :-
    H1 < H2,
    isValleyMain([H2 | T], 1).


isValley([H1, H2 | T]) :-
    H1 > H2,
    isValleyMain([H1, H2 | T], 0).