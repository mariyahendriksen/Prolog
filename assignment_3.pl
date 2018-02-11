% addone(L1,L2): every elem in L1 El1=El2-1.
addone([],[]).
addone([H1|T1],[H2|T2]):-
        H2 is H1+1,
        addone(T1,T2).
addone([H1|T1],[H2|T2]):-
        H1 is H2-1,
        addone(T1,T2).