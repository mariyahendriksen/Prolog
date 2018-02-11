% no doubles
no_doub(L,R):-
        no_doub(L,[],R).
no_doub([],_,[]).
no_doub([H|T],Acc,[H|T1]):-
        \+ member(H,Acc),
        no_doub(T,[H|Acc],T1).
no_doub([H|T],Acc,R):-
        member(H,Acc),
        no_doub(T,Acc,R).

h(nil,0).
h(t(L,_E,R),X):-
        h(L,D1),
        h(R,D2),
        max(D1,D2,D),
        X is D+1.

max(X,Y,Z):-
        (X > Y -> Z=X;
         Z=Y).