% member
memb(X,[X|_]).
memb(X,[_|T]):-
        memb(X,T).

% list length
length1([],0).
length1([_|T],S):-
        length1(T,S1),
        S is S1+1.

% maplist
maplist([],_,[]).
maplist([H|T],F,[H1|T1]):-
        G =..[F,H,H1],
        call(G),
        maplist(T,F,T1).

% permutation
permut([],[]).
permut(L,[X|T]):-
        del1(X,L,L1),
        permut(L1,T).

% del
del1(El,[El|T], T).
del1(El,[H|T],[H|T1]):-
        del1(El,T,T1).

% range
range(A,A,[A]).
range(A,N,[A|T]):-
        A < N,
        A1 is A+1,
        range(A1,N,T).

conc([],L,L).
conc([H|L1],L2,[H|L3]):-
        conc(L1,L2,L3).