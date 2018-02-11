% assign([1,2,3],[A,B,C]).

assign(_,[]).
assign(Digs, [D|Vs]):-
        member(D,Digs),
        assign(Digs, Vs).



del(X,[X|T],T).
del(X,[H|T],[H|T1]):-
        del(X,T,T1).