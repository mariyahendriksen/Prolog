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
% house(Color,Nationality,Profession,Drink,Animal).
main:-
        houses(Houses),
        mem(house(red,english,_,_,_),Houses),
        mem(house(_,spanish,_,_,dog),Houses),
        mem(house(_,japanese,painter,_,_),Houses),
        mem(house(_,italian,_,tea,_),Houses),
        Houses=[house(_,norwegian,_,_,_)|_T],
        mem(house(green,_,_,coffee,_), Houses),
        right_of(house(green,_,_,_,_),house(white,_,_,_,_),Houses),
        mem(house(_,_,sculptor,_,snails),Houses),
        mem(house(yellow,_,diplomat,_,_),Houses),
        Houses=[_,_,house(_,_,_,milk,_),_,_],
        next_to(house(_,norwegian,_,_,_), house(blue,_,_,_,_), Houses),
        mem(house(_,_,violinist,juice,_),Houses),
        next_to(house(_,_,_,_,fox), house(_,_,doctor,_,_), Houses),
        next_to(house(_,_,_,_,horse), house(_,_,diplomat,_,_), Houses),
        print1(Houses). 

houses([house(_,_,_,_,_),
        house(_,_,_,_,_),
        house(_,_,_,_,_),
        house(_,_,_,_,_),
        house(_,_,_,_,_)]).     

mem(X,[X|_]).
mem(X,[_|T]):-
        mem(X,T).

right_of(A,B, [B,A|_]).
right_of(A,B, [_|T]):-
        right_of(A,B,T).

next_to(A,B,[A,B|_T]).
next_to(A,B,[B,A|_T]).
next_to(A,B, [_H|T]):-
        next_to(A,B,T). 

print1([H|T]):- !,
        write(H), nl,
        print1(T).
print1([]).

%------------------------------------
% CLP VERSION
%------------------------------------
:- use_module(library(clpfd)).

main2:-
        Vars =
        [Norwegian,Italian,English,Spanish,Japanese,
         Yellow,Blue,Red,White,Green,
         Diplomat,Doctor,Sculptor,Violinist,Painter,
         Fox,Horse,Snails,Dog,Zebra,
         Water,Tea,Milk,Juice,Coffee],
        
        Nationalities = [Norwegian,Italian,English,Spanish,Japanese],
        Colors = [Yellow,Blue,Red,White,Green],
        Profs = [Diplomat,Doctor,Sculptor,Violinist,Painter],
        Animals=[Fox,Horse,Snails,Dog,Zebra],
        Drinks = [Water,Tea,Milk,Juice,Coffee],

        domain(Nationalities,1,5),
        domain(Profs,1,5),
        domain(Animals,1,5),
        domain(Colors,1,5),
        domain(Drinks,1,5),

        all_different(Nationalities),
        all_different(Profs),
        all_different(Animals),
        all_different(Colors),
        all_different(Drinks),

        English#=Red,
        Spanish#=Dog,
        Japanese#=Painter,
        Italian#=Tea,
        Norwegian#=1,
        Green#=Coffee,
        Green#=White+1,
        Sculptor#=Snails,
        Diplomat#=Yellow,
        Milk#=3,
        next_to(Norwegian,Blue),
        Violinist#=Juice,
        next_to(Fox,Doctor),
        next_to(Horse,Diplomat),
        labeling([],Nationalities),
        write(Nationalities), nl.

next_to(X,Y):-
        abs(X-Y) #= 1.
