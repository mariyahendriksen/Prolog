:-use_module(library(clpr)).
:-use_module(library(clpfd)).

fac(N,F) :- { N = 0, F = 1 }.
fac(N,F) :-
        { 1 =< N, N1 = N - 1,
          F = N * F1, N =< F},
        fac(N1,F1).

%  Prolog code version
sumlist([],0).
sumlist([First| Rest] , Sum) :-
        sumlist(Rest,Sum0),
        Sum is First + Sum0.

suml([],0.0).
suml([H|T],Sum):-
        {Sum=H+Sum0},
        suml(T,Sum0).

% Figure 7.5  A cryptarithmetic puzzle in CLP(FD)

% Cryptarithmetic puzzle DONALD + GERALD = ROBERT in CLP(FD)

solve( [D,O,N,A,L,D], [G,E,R,A,L,D], [R,O,B,E,R,T])  :-
  Vars = [D,O,N,A,L,G,E,R,B,T],                                 % All variables in the puzzle
  domain( Vars, 0, 9),                                          % They are all decimal digits
  all_different( Vars),                                         % They are all different
  100000*D + 10000*O + 1000*N + 100*A + 10*L + D  +
  100000*G + 10000*E + 1000*R + 100*A + 10*L + D  #=
  100000*R + 10000*O + 1000*B + 100*E + 10*R + T,
  labeling( [], Vars).

% CLP(FD) riddle
start:-
        Shoes = [EE,FF,PP,SS],
        Stores = [F,H,S,T],
        domain(Shoes, 1,4),
        domain(Stores,1,4),
        all_different(Shoes),
        all_different(Stores),
        FF #= H,
        PP +1 #\= T,
        F #= 2,
        S + 2 #= SS,
        write('Flats: '), writeln(FF),
        write('Espa: '), writeln(EE),
        write('Pumps: '), writeln(PP),
        write('Sandals: '), writeln(SS),
        write('Foot: '), writeln(F),
        write('Heels: '), writeln(H),
        write('Shoe: '), writeln(S),
        write('Tootsies: '), writeln(T).

writeln(X) :- write(X),nl.

assign(_,[]).
assign(Digs,[V|Vars]):-
        del1(V,Digs,Digs1),
        assign(Digs1,Vars).

del1(X,[X|T],T).
del1(X,[Y|T],[Y|T1]):-
        del1(X,T,T1).
        
