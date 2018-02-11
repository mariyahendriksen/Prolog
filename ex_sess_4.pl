% first(El,List)
first(El,[El|_]).

% last(El,L)
last(El,[El|[]]).
last(El,[_|T]):-
        last(El,T).

% remove T
remove(X,[X|T],T):-!.
remove(X,[_H|T],L):-
        remove(X,T,L).

% remove all X
remove_all(_, [], []).
remove_all(El, [El|Tail], Tail2):- 
        remove_all(El, Tail, Tail2).
remove_all(El, [Head|Tail], [Head|Tail2]):- % if H and El dont match, put H in the new list
        El \= Head,
        remove_all(El, Tail, Tail2).

% smaller
smaller(_N,[],[]).
smaller(N, [H1|T1], T2):-
        H1 =< N,
        smaller(N,T1,T2).
smaller(N, [H1|T1], [H1|T2]):-
        H1 > N,
        smaller(N,T1,T2).

% switch first two elems
switch_first_two([El1,El2|T],[El2,El1|T]).

% switch every two elems
switch_every_two([],[]).
switch_every_two([X,Y|T],[Y,X|T1]):-
        switch_every_two(T,T1).

% SORTING LISTS
switch_unsorted([],[]).
switch_unsorted([X],[X]).
switch_unsorted([X,Y|T],[Y,X|T]):-
        X > Y.
switch_unsorted([X,Y|T],[X|T1]):-
        X =< Y,
        switch_unsorted([Y|T],T1).

% ten times
ten_times(L0,L10):-
        switch_unsorted(L0,L1),
        switch_unsorted(L1,L2),
        switch_unsorted(L2,L3),
        switch_unsorted(L3,L4),
        switch_unsorted(L4,L5),
        switch_unsorted(L5,L6),
        switch_unsorted(L6,L7),
        switch_unsorted(L7,L8),
        switch_unsorted(L8,L9),
        switch_unsorted(L9,L10).

% another solution
ten_times_2(L1,L2):-
        switch_N_times(L1,L2,10).
switch_N_times(L,L,0).
switch_N_times(L1,L2,N):-
        N>0,
        switch_unsorted(L1,Tmp),
        Next is N-1,
        switch_N_times(Tmp,L2,Next).

is_sorted(L):-
        ten_times(L,L).

is_sorted2([]).
is_sorted2([_]).
is_sorted2([X,Y|T]):-
        X =< Y,
        is_sorted([Y|T]).

% sorting list
sort2(L,L):-
        is_sorted2(L).
sort2(L,Result):-
        \+ is_sorted2(L),
        switch_unsorted(L,L1),
        sort(L1,Result).

% flattening
flatten([],[]).
flatten([H|T],L):-
        flatten(T,L2),
        conc(H,L2,L).

conc([],L,L).
conc([H|L1],L2,[H|L3]):-
        conc(L1,L2,L3).

flatten2([],[]).
flatten2(X,[X]):-
        atom(X);
        number(X).
flatten2([H|T],L):-
        flatten2(H,L1),
        flatten2(T,L2),
        conc(L1,L2,L).

% sets
empty_set([]).

add_to_set(El,S,[El|S]):-
        \+ memb(El,S).

add_to_set(El,S,[S]):-
        memb(El,S),
        write('The element is already in the set').

memb(El,[El|_T]).
memb(El,[_H|T]):-
        memb(El,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
member_of_set(El,X):-
        memb(El,X).

del(El,[El|T],T).
del(El,[H|T],[H|T1]):-
        del(El,T,T1).

permut([],[]).
permut(L,[X|P]):-
        del(X,L,L1),
        permut(L1,P).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del_from_set(El,X1,X):-
        del(El,X1,X).

test(S):-
        empty_set(S0),
        add_to_set(a,S0,S1),
        add_to_set(b,S1,S2),
        del_from_set(a,S2,S).

union_set(S1,S2,S3):-
        conc(S1,S2,S3).

intersec([],_,[]).
intersec([H|T],S2,[H|T1]):-
        member(H,S2),
        intersec(T,S2,T1).
intersec([H|T],S2,T1):-
        \+ memb(H,S2),
        intersec(T,S2,T1).

diff([],_,[]).
diff([El|T],S2,[El|T1]):-
        \+ memb(El,S2),
        diff(T,S2,T1).
diff([El|T],S2,T1):-
        memb(El,S2),
        diff(T,S2,T1).

% matrices
% getting 1st row of matrix
split_matrix([], [], []).
split_matrix([[A|B]|C], [A|First], [B|WithoutFirst]):-
        split_matrix(C, First, WithoutFirst).
        
transpose([[]|_], []).
transpose(Matrix, [FirstColumn|TransposedRestMatrix]):-
        % FirstColumn will become the first row in the transposed matrix
        split_matrix(Matrix, FirstColumn, RestMatrix),
        transpose(RestMatrix, TransposedRestMatrix).
