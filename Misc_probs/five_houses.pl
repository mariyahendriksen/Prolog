/*
The five houses puzzle

Five men with different nationalities live in the first five houses of a street.
They smoke five distinct kinds of cigarettes, and each of them has a favorite 
animal and a favorite drink, all of them different.  The five houses are  
painted different colors.  The following facts are know:
1. The Englishman lives in a red house.
2. The Spaniard owns a dog.
3. The Japanese is a painter.
4. The Italian drinks tea.
5. The Norwegian lives in the first house on the left.
6. The owner of the green house drinks coffee.
7. The green house is on the right of the white one.
8. The sculptor breeds snails.
9.  The diplomat lives in the yellow house.
10. Milk is drunk in the middle house.
11. The Norwegian's house is next to the blue one.
12. The violinist drinks fruit juice.
13.  The fox is in the house next to that of the doctor.
14. The horse is in the house next to that of the diplomat.

 Where does the zebra live? And who drinks water?  

*/
% house(Color,Nationality,Animal,Drink,Profession)

main :-
        houses(Houses),
        member(house(red, english, _, _, _), Houses),
        member(house(_, spanish, dog, _, _), Houses),
        member(house(_, japanese, _, _, painter), Houses),
        member(house(_, italian, _, tea, _), Houses),
        Houses = [house(_, norwegian, _, _, _)|_],
        member(house(green, _, _, coffee, _), Houses),  
        right_of(house(green,_,_,_,_), house(white,_,_,_,_), Houses),
        member(house(_, _, snails, _, sculptor), Houses),
        member(house(yellow, _, _, _, diplomat), Houses),
        Houses = [_, _, house(_, _, _, milk, _), _,_],
        next_to(house(_,norwegian,_,_,_), house(blue,_,_,_,_), Houses),
        member(house(_, _, _, orange_juice, violist), Houses),
        next_to(house(_,_,_,_,doctor), house(_,_,fox,_,_), Houses),
        next_to(house(_,_,_,_,diplomat), house(_,_,horse,_,_), Houses),
        member(house(_, _, zebra, _, _), Houses),
        member(house(_, _, _, water, _), Houses),
        print_houses(Houses).


houses([
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _)
]).

right_of(A, B, [B, A | _]).
right_of(A, B, [_ | Y]) :- right_of(A, B, Y).

next_to(A, B, [A, B | _]).
next_to(A, B, [B, A | _]).
next_to(A, B, [_ | Y]) :- next_to(A, B, Y).

% member(X, [X|_]).
% member(X, [_|Y]) :- member(X, Y).

print_houses([A|B]) :- !,
        write(A), nl,
        print_houses(B).
print_houses([]).

% Solution

%house(yellow, norwegian, fox, water, diplomat)
%house(blue, italian, horse, tea, doctor)
%house(red, english, snails, milk, sculptor)
%house(white, spanish, dog, orange_juice, violist)
%house(green, japanese, zebra, coffee, painter)




/*
The five houses puzzle

Five men with different nationalities live in the first five houses of a street.
They smoke five distinct kinds of cigarettes, and each of them has a favorite 
animal and a favorite drink, all of them different.  The five houses are  
painted different colors.  The following facts are know:
1. The Englishman lives in a red house.
2. The Spaniard owns a dog.
3. The Japanese is a painter.
4. The Italian drinks tea.
5. The Norwegian lives in the first house on the left.
6. The owner of the green house drinks coffee.
7. The green house is on the right of the white one.
8. The sculptor breeds snails.
9.  The diplomat lives in the yellow house.
10. Milk is drunk in the middle house.
11. The Norwegian's house is next to the blue one.
12. The violinist drinks fruit juice.
13.  The fox is in the house next to that of the doctor.
14. The horse is in the house next to that of the diplomat.

 Where does the zebra live? And who drinks water?  

*/
% house(Color,Nationality,Animal,Drink,Profession)

:- use_module(library(clpfd)).

main2 :-
        houses(Houses),
        member(house(red, english, _, _, _), Houses),
        member(house(_, spanish, dog, _, _), Houses),
        member(house(_, japanese, _, _, painter), Houses),
        member(house(_, italian, _, tea, _), Houses),
        Houses = [house(_, norwegian, _, _, _)|_],
        member(house(green, _, _, coffee, _), Houses),  
        right_of(house(green,_,_,_,_), house(white,_,_,_,_), Houses),
        member(house(_, _, snails, _, sculptor), Houses),
        member(house(yellow, _, _, _, diplomat), Houses),
        Houses = [_, _, house(_, _, _, milk, _), _,_],
        next_to(house(_,norwegian,_,_,_), house(blue,_,_,_,_), Houses),
        member(house(_, _, _, orange_juice, violist), Houses),
        next_to(house(_,_,_,_,doctor), house(_,_,fox,_,_), Houses),
        next_to(house(_,_,_,_,diplomat), house(_,_,horse,_,_), Houses),
        member(house(_, _, zebra, _, _), Houses),
        member(house(_, _, _, water, _), Houses),
        print_houses(Houses).


houses([
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _)
]).

right_of(A, B, [B, A | _]).
right_of(A, B, [_ | Y]) :- right_of(A, B, Y).

next_to(A, B, [A, B | _]).
next_to(A, B, [B, A | _]).
next_to(A, B, [_ | Y]) :- next_to(A, B, Y).

% member(X, [X|_]).
% member(X, [_|Y]) :- member(X, Y).

print_houses([A|B]) :- !,
        write(A), nl,
        print_houses(B).
print_houses([]).

% Solution

%house(yellow, norwegian, fox, water, diplomat)
%house(blue, italian, horse, tea, doctor)
%house(red, english, snails, milk, sculptor)
%house(white, spanish, dog, orange_juice, violist)
%house(green, japanese, zebra, coffee, painter)