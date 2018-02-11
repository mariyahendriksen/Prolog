%---------------------------------------
% DIFFERENCE LISTS
%---------------------------------------
conc(A1-Z1, Z1-Z2, A1-Z2).

% flatten
flatten(L,L1):-
        flat_dif(L, L1-[]).
flat_dif([], L-L).
flat_dif(X, [X|T1]-T1) :- 
        atomic(X),
        X  \== [].
flat_dif([H|T], A-C) :-
        flat_dif(H, A-B),
        flat_dif(T, B-C).

lin(T,L):-
        lin_dif(T, L-[]).
lin_dif(nil,L-L).
lin_dif(t(Left,Root,Right),A-D):-
        lin_dif(Left,A-B),
        lin_dif(Right,C-D),
        B=[Root|C].

%---------------------------------------
% DCG
%---------------------------------------
% 0-9
n_1_9(1) --> [one].
n_1_9(2) --> [two].
n_1_9(3) --> [three].
n_1_9(4) --> [four].
n_1_9(5) --> [five].
n_1_9(6) --> [six].
n_1_9(7) --> [seven].
n_1_9(8) --> [eight].
n_1_9(9) --> [nine].

n_0_9(0) --> [].
n_0_9(N) --> n_1_9(N).

% 10-19
n_10_19(10) --> [ten].
n_10_19(11) --> [eleven].
n_10_19(12) --> [twelve].
n_10_19(13) --> [thirteen].
n_10_19(14) --> [fourteen].
n_10_19(15) --> [fifteen].
n_10_19(16) --> [sixteen].
n_10_19(17) --> [seventeen].
n_10_19(18) --> [eighteen].
n_10_19(19) --> [nineteen].

% decades
n_20_90(20) --> [twenty].
n_20_90(30) --> [thirty].
n_20_90(40) --> [fourty].
n_20_90(50) --> [fifty].
n_20_90(60) --> [sixty].
n_20_90(70) --> [seventy].
n_20_90(80) --> [eighty].
n_20_90(90) --> [ninety].

n_0_99(N) --> n_0_9(N). % 0-9
n_0_99(N) --> n_10_19(N). % 10-19
n_0_99(N) --> n_20_90(N). % decades
n_0_99(N) --> n_20_90(N1), n_1_9(N2), {N=N1+N2}. % 0-99

num(N) --> n_0_99(N).
num(N) --> n_0_9(N1), [hundred], n_0_99(N2), {N=(N1*100)+N2}. 


%---------------------------------------
% Knigth's tour: brute force
%---------------------------------------
solve(InitState, Trace):-
        search(InitState, [InitState], Trace).

search(_State,Trace,Trace):-
        is_solution(Trace).
search(CurrState,Acc,Trace):-
        try_action(CurrState, NewState),
        is_valid(NewState),
        no_loop(NewState, Acc),
        search(NewState,[NewState|Acc],Trace).

% parameters
size(8).
sign(1).
sign(-1).
%----------
% solution when Acc have 8
is_solution(Trace):-
        size(X),
        N is X*X,
        length(Trace,N),
        no_loops(Trace).

no_loops([]).
no_loops([H|T]):-
        no_loop(H,T),
        no_loops(T).

no_loop(X,L):-
        \+ memb(X,L).

memb(X,[X|_]).
memb(X,[_|T]):-
        memb(X,T).

% X +- 1, Y +-2
try_action((X,Y),(X1,Y1)):-
        sign(SignX), X1 is X+1*SignX,
        sign(SignY), Y1 is Y+2*SignY.

% X +- 2, Y +-2
try_action((X,Y),(X1,Y1)):-
        sign(SignX), X1 is X+2*SignX,
        sign(SignY), Y1 is Y+1*SignY.

is_valid((X,Y)):-
        size(S),
        X >= 1, X =< S,
        Y >= 1, Y =< S.

%---------------------------------------
% Knigth's tour with heurustic
%---------------------------------------
solve1(InitState, Trace):-
        search1(InitState, [InitState], Trace).

search1(_State,Trace,Trace):-
        is_solution1(Trace).
search1(CurrState,Acc,Trace):-
        try_action_heuristics(CurrState, Acc, NewState),
        is_valid(NewState),
        no_loop(NewState, Acc),
        search1(NewState,[NewState|Acc],Trace).

% parameters
size1(8).
sign1(1).
sign1(-1).
%----------
% solution when Acc have 8
is_solution1(Trace):-
        size(X),
        N is X*X,
        length(Trace,N),
        no_loops(Trace).

no_loops([]).
no_loops([H|T]):-
        no_loop(H,T),
        no_loops(T).

no_loop(X,L):-
        \+ memb(X,L).

memb(X,[X|_]).
memb(X,[_|T]):-
        memb(X,T).

% X +- 1, Y +-2
try_action_heuristics(Pos,Acc,Pos1):-
        findall(Heur/NewPos,
               (try_action(Pos,Pos1),compute_heuristic(Pos1,Acc,Heur)),
               NewPositions),
        % sort vals according to heurisitc
        sort(NewPositions,SortedPositions),
        % get 1st value with the smallest heuristic first
        memb(_/NewPos, SortedPositions).

compute_heuristic(Pos,Visited,Heur):-
        findall(Pos2, reachable_unvisited(Pos,Visited,Pos2), Unvisited),
        length(Unvisited,Heur).

reachable_unvisited(Pos,Visited,Pos1):-
        try_action(Pos,Pos1),
        is_valid(Pos1),
        \+ memb(Pos1,Visited).

is_valid((X,Y)):-
        size(S),
        X >= 1, X =< S,
        Y >= 1, Y =< S.
