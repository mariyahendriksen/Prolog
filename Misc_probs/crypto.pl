crypto([G,R,E,N,V,I,O,L,T,D]):-
        assign([0,1,2,3,4,5,6,7,8,9],[G,R,E,N,V,I,O,L,T,D]),
        G*10000+R*1000+E*100+E*10+N+
        V*100000+I*10000+O*1000+L*100+E*10+T
        =:= I*100000 + N*10000 + D*1000 + I*100 + G*10 + 0.
        

assign([],[]).
assign(Digs,[V|Vars]):-
        del1(V,Digs,Digs1),
        assign(Digs1,Vars).

del1(El,[El|T1],T1).
del1(El,[H|T1],[H|T2]):-
        del1(El,T1,T2).
        

% with carry bits
:- use_module(library(clpfd)).
solve([G,R,E,E,N],[V,I,O,L,E,T], [I,N,D,I,G,O]):-
        Vars=[G,R,E,N,V,I,O,L,T,D],
        domain(Vars,0,9),
        all_different(Vars),
        add(N,T,0,O,Carry1),
        add(E,E,Carry1,G,Carry2),
        add(E,L,Carry2,I,Carry3),
        add(R,O,Carry3,D,Carry4),
        add(G,I,Carry4,N,Carry5),
        add(0,V,Carry5,I,0),
        V \= 0,
        labeling([],Vars).
        

add(X,Y,Init,Res,Carry):-
        N #= X+Y+Init,
        Res #= N mod 10,
        Carry #= N//10.