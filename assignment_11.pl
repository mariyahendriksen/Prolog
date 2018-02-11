:-use_module(library(clpr)).
% predicate  flatten(List,ListofAtoms)  
% [a,b] -> a,b
flatten([],[]). % flatening [] gives []
flatten(X,[X]) :-
        atom(X),
        X \== [].
flatten([H|T],L3):-
        flatten(H,L1), flatten(T,L1),
        conc(L1,_L2,L3).

flat(I,F) :-
        flat_dif(I,F-[]).
flat_dif([],A-A). % A-A empty list
flat_dif(X,[X|A]-A) :-
        atom(X),
        X \== [].
flat_dif([H|T],A-C):-
        flat_dif(H,A-B),
        flat_dif(T,B-C).


conc([],L2,L2).
conc([H|L1],L2,[H|L3]):-
        conc(L1,L2,L3).

c([H|L1],L2,[H|L3]).
l([H|T],H,T).

s --> [c].
s --> [a], s, [b].

move --> step.
move --> step, move. 

% make num of occurences explicit
s(0) --> [c].
s(N) --> [a], s(N1), [b], {N is N1+1}.