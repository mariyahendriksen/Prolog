/*
EX1
writing letters: group of students
*/
% group A: hello -> olleh
reverse('').
reverse(W):-
        split(W,F,R),
        reverse(R),
        write(F).

% group B: hello-> hello
groupB('').
groupB(W):-
        split(W,F,R),
        write(F),
        groupB(R).

/*
EX2:
MAPS
*/

direct_road(brugge, ghent).
direct_road(ghent, antwerp).
direct_road(ghent, brussels).
direct_road(antwerp, brussels).
direct_road(hasselt, leuven).
direct_road(brussels, leuven).
direct_road(mons, brussels).
direct_road(mons, namur).
direct_road(namur, liege).
direct_road(bastogne, liege).

% a
reach(X,Y):-
        direct_road(X,Y).
reach(X,Y):-
        direct_road(Z,Y),
        reach(X,Z).
direct_road(X,Y):-
        direct_road(Y,X).

/*
EX3: ALL EVEN
*/
all_even(X):-
        X =< 9,
        even(X).
all_even(X):-
        X >= 10,
        split(X,Dig,NewN),
        even(Dig),
        all_even(NewN).  


/*
EX4: EVEN/ODD
*/
count_even(N,1):-
        N =< 9,
        even(N).
count_even(N,0):-
        N =< 9,
        odd(N).
count_even(N,Res):-
        N >= 10,
        split(N,Digit,NewN),
        DigCount is mod(Digit+1,2),
        count_even(NewN,Res1),
        Res is DigCount+Res1.

split(N,Dig,NewN):-
        Dig is mod(N,10),
        NewN is N//10.

even(X):-
     A is mod(X,2),
     A = 0.   
odd(X):-
        A is mod(X,2),
        A > 0.

/*
EX6: fibonacci
*/
fib(1,1).
fib(2,1).
fib(N,Res):-
        N>2,
        N1 is N-1,
        N2 is N-2,
        fib(N1,Res1),
        fib(N2,Res2),
        Res is Res1+Res2.

/*
EX7: double digit
*/
/*
double_digit(X):-
        X >= -99,
        X =< 99,        
        split(X,Dig,NewN),
        NewN == Dig.

double_digit(X):-
        X > 99,
        split(X,Dig,NewN),
*/
double_digit(N):- % base case: checking if the very last digit fits
        N>10,
        split(N,Digit,NewN),
        occurs_in(Digit,NewN).
double_digit(N):- % recursion: checking the rest of digits
        N>10,
        split(N,_,NewN),
        double_digit(NewN).

% We have used an auxiliary predicate occurs_in/2:

occurs_in(Digit,Number):-
        % the base case:
        split(Number,Digit,_).
occurs_in(Digit,Number):-
        % the recursive case
        Number>=10,
        split(Number,_,NewNumber),
        occurs_in(Digit,NewNumber).


next_to(N):-
        N > 10,
        split(N,Dig,NewN),
        split(NewN,Dig1,_),
        Dig==Dig1.
next_to(N):-
        N > 10,
        split(N,_,NewN),
        next_to(NewN).
       























        
        
        
        