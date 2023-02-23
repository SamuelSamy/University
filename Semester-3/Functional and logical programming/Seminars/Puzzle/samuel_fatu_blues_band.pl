% Five musicians are side by side talking about starting a blues band. 
% Each one plays an instrument, is drinking a type of juice and suggested a name for the band. 
% Follow the clues to find out which band name the 32-year-old musician suggested.

/*
    01. The Harmonica player is exactly to the left of the Guitarist.
    02. The man wearing the Blue shirt is somewhere between the oldest musician and the man wearing the Red shirt, in that order.
    03. Larry is at one of the ends.
    04. The musician that plays Harmonica is next to the man that is drinking Apple juice.
    05. At the second position is the man drinking Strawberry juice.
    06. The Drummer is somewhere to the right of the man wearing the Blue shirt.
    07. Larry is next to the musician that suggested the band name Lazy Blues.
    08. Bryan is next to the Pianist.
    09. At one of the ends is the youngest musician.
    10. The Pianist suggested the band name Apocalypse Blues.
    11. The 28-year-old man is exactly to the left of the 30-year-old man.
    12. The musician wearing the Green shirt is somewhere to the left of the musician that suggested the band name Bear Blues.
    13. Joey is next to the man wearing the Blue shirt.
    14. The musician drinking Grapefruit is exactly to the right of the musician that suggested the band name The Blues Cathedral.
    15. The man drinking Grapefruit is wearing the Green shirt.
    16. The man drinking Lemon juice is somewhere between the man wearing the White shirt and the 30-year-old man, in that order.
    17. The musician that suggested the band name Blues Up is exactly to the right of the man wearing the Red shirt.
    18. At the first position is the Piano player.
    19. Dustin is next to the musician that suggested the band name The Blues Cathedral.
    20. The Harmonica player is exactly to the right of the man wearing the Red shirt.
*/

exactlyToLeft(A, B, R) :- append(_, [A, B | _], R).

exactlyToRight(A, B, R) :- exactlyToLeft(B, A, R).

nextTo(A, B, R) :- exactlyToLeft(A, B, R).
nextTo(A, B, R) :- exactlyToRight(A, B, R).

atAnyEnd(A, [A | _]).
atAnyEnd(A, [_, _, _, _, A]).

somewhereLeft(A, B, R):- append(_, [A, B | _], R).
somewhereLeft(A, B, R):- append(_, [A, _, B | _], R).
somewhereLeft(A, B, R):- append(_, [A, _, _, B | _], R).
somewhereLeft(A, B, R):- append(_, [A, _, _, _, B | _], R).

somewhereRight(A, B, R) :- somewhereLeft(B, A, R).


somewhereBetween(A, B, C, R):- somewhereLeft(A, B, R), somewhereLeft(B, C, R).


musicians(Musicians) :- 
    % each musician is represented as:
    %   -   musician('Shirt', 'Name', 'Instrument', 'Band name', 'Age', 'Juice')
    length(Musicians, 5),

    % 05. At the second position is the man drinking Strawberry juice.
    % 10. The Pianist suggested the band name Apocalypse Blues.
    % 18. At the first position is the Piano player.
    Musicians = [musician(_, _, piano, apocalypse, _, _), musician(_, _, _, _, _, strawberry), _, _, _],

    % 08. Bryan is next to the Pianist.
    nextTo(musician(_, bryan, _, _, _, _), musician(_, _, piano, _, _, _), Musicians),

    % 01. The Harmonica player is exactly to the left of the Guitarist.
    exactlyToLeft(musician(_, _, harmonica, _, _, _), musician(_, _, guitarist, _, _, _), Musicians), 
    
    % 02. The man wearing the Blue shirt is somewhere between the oldest musician and the man wearing the Red shirt, in that order.
    somewhereBetween(musician(_, _, _, _, 34, _), musician(blue, _, _, _, _, _), musician(red, _, _, _, _, _), Musicians),   

    % 03. Larry is at one of the ends.
    atAnyEnd(musician(_, larry, _, _, _, _), Musicians),        

    % 09. At one of the ends is the youngest musician.
    atAnyEnd(musician(_, _, _, _, 26, _), Musicians), 

    % 07. Larry is next to the musician that suggested the band name Lazy Blues.
    nextTo(musician(_, larry, _, _, _, _), musician(_, _, _, lazy, _, _), Musicians),

    % 04. The musician that plays Harmonica is next to the man that is drinking Apple juice.
    nextTo(musician(_, _, harmonica, _, _, _), musician(_, _, _, _, _, apple), Musicians),

    % 06. The Drummer is somewhere to the right of the man wearing the Blue shirt.
    somewhereRight(musician(_, _, drums, _, _, _), musician(blue, _, _, _, _, _), Musicians),

    % 11. The 28-year-old man is exactly to the left of the 30-year-old man.
    exactlyToLeft(musician(_, _, _, _, 28, _), musician(_, _, _, _, 30, _), Musicians),

    % 13. Joey is next to the man wearing the Blue shirt.
    nextTo(musician(_, joey, _, _, _, _), musician(blue, _, _, _, _, _), Musicians),
    
    % 14. The musician drinking Grapefruit is exactly to the right of the musician that suggested the band name The Blues Cathedral.
    % 15. The man drinking Grapefruit is wearing the Green shirt.
    exactlyToRight(musician(green, _, _, _, _, grapefruit), musician(_, _, _, cathedral, _, _), Musicians),

    % 19. Dustin is next to the musician that suggested the band name The Blues Cathedral.
    nextTo(musician(_, dustin, _, _, _, _), musician(_, _, _, cathedral, _, _), Musicians),

    % 12. The musician wearing the Green shirt is somewhere to the left of the musician that suggested the band name Bear Blues.
    somewhereLeft(musician(green, _, _, _, _, _), musician(_, _, _, bear, _, _), Musicians),

    % 17. The musician that suggested the band name Blues Up is exactly to the right of the man wearing the Red shirt.
    % 20. The Harmonica player is exactly to the right of the man wearing the Red shirt.
    exactlyToRight(musician(_, _, harmonica, up, _, _), musician(red, _, _, _, _, _), Musicians),

    % 16. The man drinking Lemon juice is somewhere between the man wearing the White shirt and the 30-year-old man, in that order.
    somewhereBetween(musician(white, _, _, _, _, _), musician(_, _, _, _, _, lemon), musician(_, _, _, _, 30, _), Musicians),

    % one musician has a black shirt
    member(musician(black, _, _, _, _, _), Musicians),

    % one man is named Elbert
    member(musician(_, elbert, _, _, _, _), Musicians),

    % one member is playing the bass
    member(musician(_, _, bass, _, _, _), Musicians),

    % one member is 32 years old
    member(musician(_, _, _, _, 32, _), Musicians),

    % one musicians drinks orange juice
    member(musician(_, _, _, _, _, orange), Musicians), !.


bandName32(Musician, BandName) :-
    musicians(Musicians),
    member(musician(_, Musician, _, BandName, 32, _), Musicians), !.