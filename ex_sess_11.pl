:-use_module(library(clpr)).
:- use_module(library(clpfd)).

% Constraint Logic Programming with Reals
% LOANS
/*
P, the amount of money lent,
T, the number of periods in the loan,
I, the interest rate on the loan,
MP, the amount we pay back each period.

T=0, B=P.
T>0, B1*(1+I)-MP, B1 - balance after T-1 periods
*/

balance(0,I,MP,B,B).
balance(T,I,MP,P,B):-
        T>0,
        T1 is T-1,
        balance(T1,I,MP,P,B1),
        B is B1*(1+I)-MP.

% clp version
balance1(T,I,MP,P,B) :-
        {T=0,P=B}.
balance1(T,I,MP,P,B):-
        {T>0,
        T1 = T-1,
        B = B1*(1+I)-MP},
        balance1(T1,I,MP,P,B1).

% Finite Domain Constraint Logic Programming
breakin(X):-
        X = [X1,X2,X3,X4,X5,X6,X7,X8,X9],
        domain(X,1,9),
        all_different(X),
        self_different(X,1),
        (X4-X6 #= X7) #<=> B1,
        (X4-X6 #= -X7) #<=> B2,
        B1 + B2 #= 1,
        X1*X2*X3 #= X8 + X9,
        X2+X3+X6 #< X8,
        X9 #< X8,
        labeling([],X).

self_different([],_).
self_different([H|T],N):-
        H #\= N,
        M #= N+1,
        self_different(T,M).

% six inverted cups
cups(Balls):-
        Balls = [Gr,Mag,Or,Pur,Red,Yel],
        domain(Balls,1,6),
        all_different(Balls),
        Mag #= 1,
        Gr #= 5,
        Pur #< Or,
        (Red + 1 #= Mag) #<=> B1,
        (Red - 1 #= Mag) #<=> B2,
        B1 + B2 #= 1,
        labeling([],Balls).

% party!
% N tables
% M chairs
% likes=[(i,j),(i1,j1)]
% dislikes=[(i,j),(i1,j1)]
party(M,N,Likes,Dislikes):- 
        NumOfGuests is M*N,
        write('Nm of guests: '), write(NumOfGuests), nl,
        length(Guests,NumOfGuests),
        domain(Guests,1,N),
        write('Guests: '), write(Guests), nl,
        write('Domains assigned'), nl,
        add_likes(Likes,Guests),
        write('Likes added'), nl,
        add_dislikes(Dislikes,Guests),
        write('Dislikes added'), nl,
        write('Reodered guests: '), write(Guests), nl,
        add_table_constr(N,Guests,M),
        write('Tables counted'), nl,
        labeling([],Guests),
        write(Guests).

add_likes([],_).
add_likes([(X,Y)|Likes],Guests):-
        write('Processing likes'), nl,
        nth_el(X,Guests,GuestX),
        nth_el(Y,Guests,GuestY),
        GuestX #= GuestY,
        add_likes(Likes,Guests).

add_dislikes([],_).
add_dislikes([(X,Y)|DisLikes],Guests):-
        write('Processing dislikes'), nl,
        nth_el(X,Guests,GuestX),
        nth_el(Y,Guests,GuestY),
        GuestX #\= GuestY,
        add_dislikes(DisLikes,Guests).

add_table_constr(0,_,_).
add_table_constr(Table,Guests,Chairs):-
        write('Processing tables'), nl,
        write('Tables: '), write(Table), nl,
        write('Guests: '), write(Guests), nl,
        write('Chairs: '), write(Chairs), nl,
        Table > 0,
        exactly(Table,Guests,Chairs),
        PrevTable is Table - 1,
        add_table_constr(PrevTable,Guests,Chairs).

exactly(_,[],0).
exactly(T,[H|Gs],Ch):-
        (T #= H) #<=> B,
        write('B is '), write(B), nl,
        Ch1 is Ch - B,         
        exactly(T,Gs,Ch1).

nth_el(1,[El|_],El).
nth_el(X,[_|T],El):-
        X1 is X-1,
        nth_el(X1,T,El). 