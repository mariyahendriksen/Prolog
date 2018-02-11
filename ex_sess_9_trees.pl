% TREES
% count(Tree, Num_of_elem)
% count1(t(t(nil,b,nil),c,t(nil,d,nil)),X).
count1(nil,0).
count1(t(L,_E,R),X):-
        count1(L,N1),
        count1(R,N2),
        X is N1+N2+1.

count(nil,0).
count(t(L,E,R),C):-
        count(L, Sum1),
        count(R, Sum2),
        C is Sum1 + Sum2 + 1.


% depth(Tree,Depth)
depth1(nil,0).
depth1(t(L,_E,R),X):-
        depth1(L,D1),
        depth1(R,D2),
        max(D1,D2,D),
        X is D+1.   

depth(nil,0).
depth(t(L,_,R),D):-
        depth(L,D1),
        depth(R,D2),
        max(D1,D2,Df),
        D is Df + 1. % add 1 for the current node

% max is X if X>=Y, otherwise max is Y
max(X,Y,X):- X >= Y,!.
max(_,Y,Y).
        


% linearize(Tree, Nodes)
% lin1(t(t(nil,b,nil),c,t(nil,d,nil)),X).
lin1(nil,[]).
lin1(t(L,E,R),List):-
        lin1(L,L1),
        lin1(R,L2),
        conc([E|L1],L2,List).

conc([],L,L).
conc([H|L1],L2,[H|L3]):-
        conc(L1,L2,L3).


lin(nil,[]).
lin(t(L,E,R),Res):-
        lin(L,Res1),
        lin(R,Res2),
        concat(Res1,[E|Res2],Res).

concat([],L,L).
concat([X|L1],[L2],[X|L3]):-
        concat(L1,L2,L3).


% open tree
open_t(nil).
open_t(t(L,_E,R)):-
        open_t(L),
        open_t(R).

close1(nil):-!.
close1(t(L,_,R)):-
        close1(L),
        close1(R).


% efficiency searching sorted lists
% occur(Arg,List)
occur(X,[X|_]).
occur(X,[H|T]):-
        X>H,
        occur(X,T).

% base case
occurthree(X,[_,_,X|_]).
occurthree(X,[_,_,H|T]):-
        X>H,
        occurthree(X,T).
occurthree(X, [X|_]).
occurthree(X, [_,X|_]).


% binary dictionaries
sorted(t(nil,_,nil)).
sorted(t(L,E,R)):-
        sorted_and_smaller(L,E),
        sorted_and_bigger(R,E).

sorted_and_smaller(nil,_).
sorted_and_smaller(t(L,E,R),Nr):-
        Nr > E,
        sorted_and_smaller(L,E),
        sorted_and_between(R,E,Nr).

sorted_and_bigger(nil,_).
sorted_and_bigger(t(L,E,R),Nr):-
        Nr < E,
        sorted_and_between(L,Nr,E),
        sorted_and_bigger(R,E).

sorted_and_between(nil,_,_).
sorted_and_between(t(L,E,R),Low,High):-
        E > Low,
        E < High,
        sorted_and_between(L,Low,E),
        sorted_and_between(R,E,High).
             

% balanced binary dictionaries
% balanced(t(nil,a,t(nil,b,t(nil,q,nil)))).

balanced(nil).
balanced(t(R,_E,L)):-
        get_depth(L,Dl),
        get_depth(R,Dr),
        Diff is Dl-Dr,
        Diff =< 1, Diff >= -1,
        balanced(L),
        balanced(R).

get_depth(nil,0).
get_depth(t(L,E,R),D):-
        get_depth(L,D1),
        get_depth(R,D2),
        max(D1,D2,Df),
        D is Df + 1.        

% list_to_balanced
% [1,2,3,4,5] -> balanced binary tree
/*
[1,2,3] -> 2
[1,2,3,4] -> 2
*/
l_to_balanced([],nil).
l_to_balanced(L,t(L,Middle,R)):-
        length(L,N),
        SplitPos is (N+1)//2,
        split(L,SplitPos,P1,Middle,P2),
        l_to_balanced(P1,L),
        l_to_balanced(P2,R).

% L=[a,b,c], SpitPos=2, P1=a, Mid=b,P2=c
split([H|T],1,[],H,T).
split([H|T],SplitPos,[H|T1],Mid,P2):-
        SplitPos > 1, 
        NewSplit is SplitPos+1,
        split(T,NewSplit,T1,Mid,P2).     