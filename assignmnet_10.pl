:-use_module(library(clpfd)).
% disjoint_tasks(S1,D1,S2,D2).
disjoint_tasks(S1,D1,S2,D2):-
        (S1 + D1 #<= S2) #<=> B1,
        (S2 + D2 #<= S1) #<=> B2,
        B1 + B2 #= 1.

% occur_ntimes(X,L,N) - true if the element X occurs exactly N times in the list L.
occur_ntimes(_X,[],0).
occur_ntimes(X, [H|T], N) :-
         (X #= H) #<=> B,
         N #= N1 + B,
         occur_ntimes(X,T,N1).

occur(_,[],0).
occur(X,[H|T],N):-
        (X #= H) #<=> B,
        occur(X,T,N1),
        N1 #= N - B.

% magic sequence
magic1(N,L):-
        length(L,N),
        N1 is N-1,
        domain(L,0,N1),
        acc_constr(L,0,L),
        labeling([],L).

acc_constr([],_).
acc_constr([H|T],I,L):-
        occur_ntimes(I,L,H),
        I1 is I+1,
        acc_constr(T,I1,L).

magic(N,L):-
        length(L,N), N1 is N-1,
        % constraints
        domain(L,0,N1), % any elem in L can take values from 0 to N-1
        acc_constraints(L,0,L), % if L occurs in 0 L times
        labeling([],L).

% acc_constraints(L,0,L).
acc_constraints([],_,_).
acc_constraints([H|T],I,L) :- % check if [H|T] is a magic serie
        occur_ntimes(I,L,H), % if I occurs H times in L
        I1 is I+1,
        acc_constraints(T,I1,L).

%-------------------------
% BRANCH AND BOUND
%-------------------------     
try(A,B,C):-
        C=5,
        C is A+B,
        write('1st branch'), nl,
        fail;
        write('2nd branch').   