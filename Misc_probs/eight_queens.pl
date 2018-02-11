% queens_1(N,Qs) :- Qs is a solution of the N-queens problem
% queens_1(N,Qs) :- Qs is a solution of the N-queens problem

queens_1(N,Qs) :-
        range(1,N,Rs),
        permu(Rs,Qs),
        test(Qs).

% range(A,B,L) :- L is the list of numbers A..B

range(A,A,[A]).
range(A,B,[A|L]) :-
        A < B,
        A1 is A+1,
        range(A1,B,L).

% permu(Xs,Zs) :- the list Zs is a permutation of the list Xs

permu([],[]).
permu(Qs,[Y|Ys]) :-
        del1(Y,Qs,Rs),
        permu(Rs,Ys).

del1(X,[X|Xs],Xs).
del1(X,[Y|Ys],[Y|Zs]) :-
        del1(X,Ys,Zs).

% test(Qs) :- the list Qs represents a non-attacking queens solution

test(Qs) :-
        test(Qs,1,[],[]).

% test(Qs,X,Cs,Ds) :- the queens in Qs, representing columns X to N,
% are not in conflict with the diagonals Cs and Ds

test([],_,_,_).
test([Y|Ys],X,Cs,Ds) :- 
        C is X-Y, \+ memb(C,Cs),
        D is X+Y, \+ memb(D,Ds),
        X1 is X + 1,
        test(Ys,X1,[C|Cs],[D|Ds]).

memb(X,[X|_T]).
memb(X,[_H|T]):-
        memb(X,T).


%--------------------------------------------------------------

% Now, in version 2, the tester is pushed completely inside the
% generator permu.

queens_2(N,Qs) :-
        range(1,N,Rs),
        permu_test(Rs,Qs,1,[],[]).

permu_test([],[],_,_,_).
permu_test(Qs,[Y|Ys],X,Cs,Ds) :- 
        del1(Y,Qs,Rs), 
        C is X-Y, \+ memb(C,Cs),
        D is X+Y, \+ memb(D,Ds),
        X1 is X+1,
        permu_test(Rs,Ys,X1,[C|Cs],[D|Ds]).