parent(bob,pat).

square(X,X1):-
        X1 is X*X.

maplist([],_,[]).

maplist([H|T],F,[H1|T1]):-
                G =..[F,H,H1],
                call(G),
                maplist(T,F,T1).

sumtree(Tree,S):-
        sumtree(Tree,0,S).

sumtree(nil,Sum,Sum).
sumtree(t(L,E,R),Acc,Sum):-
        sumtree(L,Acc,Acc1),
        Acc2 is Acc1+E,
        sumtree(R,Acc2,Sum).

sumt(nil,0).
sumt(t(nil,X,nil),X).
sumt(t(L,E,R),S):-
        sumt(L,S1),
        sumt(R,S2),
        S is S1+S2.


/*
HOW EXACTLY DOES IT WORK??
if there is no match between set and subset,
we'll never reach a base case -> fail
*/
subset([],[]).

subset([X|Set],[X|Subset]):-
        subset(Set,Subset).

subset([X|Set],Subset):-
        subset(Set,Subset).

% G = (Nodes,Edges)
% node(N,Graph) - N is in the Graph -> list of nodes in Graph, ([H|T],_) contain N
% G = ([s,t,u,v],[e(s,t),e(t,u),e(u,v)]).
G = ([s,t,u,v],[e(s,t),e(t,u),e(u,v)]).
predicate((L,L1)):- write(L).
node(N,Graph):-
       arg(1,Graph,X),
       member(N,X). 




