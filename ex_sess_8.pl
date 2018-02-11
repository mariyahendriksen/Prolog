% SUDOKU
/*
([_,_,_,2,3,_,_,_,5,
_,4,2,_,9,_,_,_,3,
3,_,_,_,_,8,7,_,_,
_,_,7,_,_,_,_,5,6,
_,9,_,7,2,_,_,1,4,
5,_,_,9,_,_,_,_,_,
6,_,_,_,_,2,8,4,9,
_,_,8,_,_,1,_,_,7,
2,5,_,_,_,9,6,_,_]).

sudoku([_,_,_,2,3,_,_,_,5,_,4,2,_,9,_,_,_,3]).
Out: [1,1,1,2,3,1,1,1,5,1,4,2,1,9,1,1,1,3]

*/

sudoku0(X):-
        generate(X),
        test(X),
        write(X).

%------------------------------------------
% GENERATE
%------------------------------------------
generate([]).
generate([H|T]):-
        \+ number(H),
        memb(H,[1,2,3,4,5,6,7,8,9]),
        generate(T).
generate([H|T]):-
        number(H),
        generate(T).

%------------------------------------------
% TEST
%------------------------------------------
test(X):-
        test2([r1,r2,r3],X),
        test2([c1,c2,c3],X),
        test2([b1,b2,b3],X).

test2([],_).
test2([H|T],X):-
        gr(H,Posit),
        get_vals(Posit,X,Vs), % get_vals([1,2,3,4,5,6,7,8,9], [1,2,3,4,5,6,7,8,9], V).
        all_diff(Vs),
        test2(T,X).

get_vals([],_,[]).
get_vals([P|Ps],X,[V|Vs]):-
        get_nth_el(P,X,V),
        get_vals(Ps,X,Vs).


get_nth_el(P,X,Vs):-
        get_nth_el(P,1,X,Vs).

get_nth_el(P,Counter,[H|_T],H):-
        P == Counter.
get_nth_el(P,Counter,[_H|T],Vs):-
        P \= Counter,
        NewCount is Counter + 1,
        get_nth_el(P,NewCount,T,Vs).


all_diff([H|T]):-
        all_diff([H|T],[]).

all_diff([],_).
all_diff([H|T],Acc):-
        \+memb(H,Acc),
        all_diff(T,[H|Acc]).
        
        
%------------------------------------------
% TEMPLATES
%------------------------------------------ 
% ROWS
gr(r1, [1,2,3,4,5,6,7,8,9]). % first row
gr(r2, [10,11,12,13,14,15,16,17,18]). % second row
gr(r3, [19,20,21,22,23,24,25,26,27]).
gr(r4, [28,29,30,31,32,33,32,35,36]).

% COLUMNS
gr(c1, [1,10,19,28,37,46,55,64,73]). % first column
gr(c2, [2,11,20,29,38,47,56,65,74]).
gr(c3, [3,12,21,30,39,48,57,66,75]).

% BLOCKS
gr(b1, [1,2,3,10,11,12,19,20,21]). % first block
gr(b2, [4,5,6,13,14,15,22,23,24]).
gr(b3, [7,8,9,16,17,18,25,26,27]).

%------------------------------------------
% SUPPLEMENTARY PREDICATES
%------------------------------------------  
memb(X,[X|_T]).
memb(X,[_H|T]):-
        memb(X,T).
%------------------------------------------

% SUDOKU INCREMENTALLY

%------------------------------------------ 
sudoku(X):-
        N=81,
        stepwise(1,N,X),
        write(X),nl.

stepwise(Pos,N,X):-
        Pos>N,!.
stepwise(Pos,N,X):-
        member(Val,[1,2,3,4,5,6,7,8,9]),
        get_nth_el(Pos,X,Val), % 1st pos in Sudoku is of value Val
        test_stepwise(Pos, X),
        Next is Pos + 1,
        stepwise(Next,N,X).

test_stepwise(Pos,X):-
        test_one_dim(Pos, [r1,r2,r3,r4,r5,r6,r7,r8,r9],X),
        test_one_dim(Pos, [c1,c2,c3,c4,c5,c6,c7,c8,c9],X),
        test_one_dim(Pos, [b1,b2,b3,b4,b5,b6,b7,b8,b9],X).

test_one_dim(Pos,NumL,X):-
        gr(Num, PosL),
        memb(Pos,PosL),
        memb(Num,NumL),
        get_values(PosL,X,Vs), % get non-empty values
        all_diff(Vs).
        
get_values(PosL, X, Vs):-
        get_values(PosL, X, [], Vs).
get_values([],_,Res,Res).
get_values([Pos|T],X,Acc,Vs):-
        get_nth_el(Pos,X,V),
        ( \+ number(V) % V isnt instantiated
        -> get_values(T,X,Acc,Vs)
        ;
          get_values(T,X,[V|Acc],Vs)).