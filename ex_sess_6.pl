% ACCUMULATORS

% REVERSE LIST WITH ACC
reverse(L,L1):-
        reverse(L,[],L1).
reverse([],Res,Res).
reverse([H|T],Acc,Res):-
        reverse(T,[H|Acc],Res).

% ex 1
l_len(L,X):-
        l_len(L,0,X).
l_len([],X,X).
l_len([_H|T],Acc,Res):-
        Acc1 is Acc+1,
        l_len(T,Acc1,Res).
        
list_length([],0).
list_length([_H|T],L) :-
        list_length(T,L1),
        L is L1+1.


% ex 2
qs(L,L1):-
        qs(L,[],L1).
qs([],Res,Res).
qs([X|Tail], Acc, Sort):-
        split(X, Tail, Small, Big),
        qs(Big, Acc, NewAcc),
        qs(Small, [X|NewAcc], Sort).

split(X, [], [], []).
split(X, [Y| Tail], [Y | Small], Big):-
        gt(X, Y), !,
        split(X, Tail, Small, Big).
split(X, [Y| Tail], Small, [Y | Big]):-
        split(X, Tail, Small, Big).

gt(X,Y):-
        X>=Y.    


% FIBONACCI
fibo(N,Fib):-
        fibo(1,N,0,1,Fib).
fibo(N,N,_,Res,Res).
fibo(Count,N,PrevFib,CurrFib,Res):-
        Count < N,
        NextCount is Count + 1,
        NextFib is CurrFib + PrevFib,
        fibo(NextCount,N,CurrFib,NextFib,Res).

% GET DOUBLES - REMOVE ALL ELEM THAT OCCUR AT LEAST TWICE
get_doubles1(L,L1):-
        get_doubles1(L,[],[],L1).

get_doubles1([],_,Res,Res).
get_doubles1([H|T],Acc1,Acc2,Res):-
        \+ memb(H,Acc1),
        \+ memb(H,Acc1),
        get_doubles1(T,[H|Acc1],Acc2,Res).
get_doubles1([H|T],Acc1,Acc2,Res):-
        memb(H,Acc1),
        \+ memb(H,Acc2),
        get_doubles1(T,Acc1,[H|Acc2],Res).
get_doubles1([H|T],Acc1,Acc2,Res):-
        memb(H,Acc1),
        memb(H,Acc2),
        get_doubles1(T,Acc1,Acc2,Res).

memb(El,[El|_T]).
memb(El,[_H|T]):-
        memb(El,T).


get_doubles(List, Doubles):-
        get_doubles_acc(List, Doubles, [], []).
        % 3th arg = elements that we have seen once before so far, 4th arg = elements that we have seen twice or more before so far

get_doubles_acc([], Doubles, _, Doubles).

get_doubles_acc([El|T],Result,Singles,Doubles):- % we have seen the element El twice (or more) before
        member(El,Doubles), !,
        get_doubles_acc(T,Result,Singles,Doubles).

get_doubles_acc([El|T],Result,Singles,Doubles):- % we have seen the element El once before
        member(El,Singles), remove(El,Singles,NewSingles), !,
        get_doubles_acc(T,Result,NewSingles,[El | Doubles]).

get_doubles_acc([El|T],Result,Singles,Doubles):- % we have not seen the element El before
         get_doubles_acc(T,Result,[El | Singles],Doubles).


%------------------------------------------------
% GRAPHS
%------------------------------------------------
memb(X,[X|_T]).
memb(X,[_H|T]):-
        memb(X,T).

arc(X,Y,Gr):-
        memb(X/Y,Gr).
arc(X,Y,Gr):-
        memb(Y/X,Gr).

connected(X,X,_).
connected(X,Y,Gr):-
        arc(Z,Y,Gr),
        connected(X,Z,Gr).

conn_2(X,Y,Gr):-
        find_path(X,Y,[Y],_P,Gr).

find_path(X,Y,Gr,P):-
        find_path(X,Y,[Y],P,Gr).

find_path(X,X,Res,Res,_).
find_path(X,Y,Acc,P,Gr):-
        arc(Z,Y,Gr),
        \+ member(Z,Acc),
        find_path(X,Z,[Z|Acc],P,Gr).        