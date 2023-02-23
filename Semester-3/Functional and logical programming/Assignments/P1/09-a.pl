% a. Insert an element on the positionn in a list.

insertAtPosMain([], E, _, _, [E]). % inserts the element at the end if P > len(L)
insertAtPosMain(L, E, P, P, [E | L]).
insertAtPosMain([H | T], E, P, I, [H | R]) :-
    I1 is I + 1,
    insertAtPosMain(T, E, P, I1, R).

insertAtPos(L, E, P, R) :-
    insertAtPosMain(L, E, P, 1, R).