% sumlist
sumlist(nil,0).
sumlist(t(L,E,R),Sum):-
        sumlist(L,Sum1),
        sumlist(R,Sum2),
        Sum is Sum1+Sum2+E.

suml(T,S):-
        suml(T,0,S).
suml(nil,S,S).
suml(t(L,E,R),Acc,S):-
        suml(L,Acc,Acc1),
        Acc2 is Acc1+E,
        suml(R,Acc2,S).

% hamiltonian path
% Gr=graph([a,b,c], [e(a,b),e(b,c),e(c,a)]).
node(N,graph(Nodes,_Edges)):-
        memb(N,Nodes).

memb(X,[X|_]).
memb(X,[_|T]):-
        memb(X,T).

% list length
length1([],0).
length1([_|T],S):-
        length1(T,S1),
        S is S1+1.
        