%----------------------------------
% MERGE
%----------------------------------
merge([],L,L).
merge(L,[],L).
merge([H|T],[H1|T1],[H1|T2]):- % add the smallest el to the list
        gt(H,H1),
        merge([H|T],T1,T2).
merge([H|T],[H1|T1],[H|T2]):-
        gt(H1,H),
        merge(T,[H1|T1],T2).

gt(move(_,Pl,_Opp),move(_,Pl1,_Opp1)):-
        Pl1>Pl.
gt(move(_,Pl,Opp),move(_,Pl1,Opp1)):-
        Pl1 == Pl,
        Opp1 =< Opp.      

% gt(X,Y):- X>Y.

merge1([],L,L) :- !.
merge1(L,[],L) :- !.

merge1([H|T],[H1|T1], [H|Res]):-
        H<H1, !,
        merge1(T,[H1|T1],Res).

merge1([H|T],[H1|T1], [H1|Res]):-
        merge1([H|T],T1,Res).

%----------------------------------
% SPLIT
%----------------------------------
% split(L,L1,L2), L1>=L2
split([],[],[]).
split([A],[A],[]).
split([A,B|T],[A|T1],[B|T2]):-
        split(T,T1,T2).

%----------------------------------
% MERGE SORT
%----------------------------------
mergesort([],[]).
mergesort([X],[X]).
mergesort(L,E):-
        split(L,L1,L2),
        mergesort(L1,L12),
        mergesort(L2,L22),
        merge(L12,L22,E).

%----------------------------------
% cannibals & missionaries 
%----------------------------------
% t(M,C,B) - number of missioners and cannibals
% on the start shore
% init state: t(3,3,-1).
% desired state: t(0,0,1).
solve(InitState,Trace):-
        search(InitState,[InitState],Trace).
search(CurrState,AccTrace,Trace):-
        is_solution(CurrState),
        Trace=AccTrace.
search(CurrState,AccTrace,Trace):-
        try_action(CurrState,NewState),
        validate_state(NewState),
        no_loop(NewState,AccTrace),
        search(CurrState,[NewState|AccTrace],Trace).

is_solution(t(0,0,1)).

% from coast1 to coast2
try_action(t(M,C,-1),t(M1,C1,1)):-
        cross_s_to_e(M,C,1,M1,C1);
        cross_s_to_e(M,C,2,M1,C1).

try_action(t(M,C,1),t(M1,C1,-1)):-
        cross_e_to_s(M,C,1,M1,C1);
        cross_e_to_s(M,C,2,M1,C1).

cross_s_to_e(M,C,0,M,C):-!.
cross_s_to_e(M,C,X,M1,C1):-
        % missionary on the boat
        M1 is M-1,
        M1 >= 0,
        X1 is X-1,
        cross_s_to_e(M,C,X1,M1,C1).

cross_s_to_e(M,C,X,M1,C1):-
        % cannibal on the boat
        C1 is C-1,
        C1 >= 0,
        X1 is X-1,
        cross_s_to_e(M,C,X1,M1,C1).

cross_e_to_s(M,C,0,M,C):-!. % prevent backtracking
cross_e_to_s(M,C,X,M1,C1):-
        % missionary on the boat
        M1 is M+1,
        M1 =< 3,
        X1 is X-1,
        cross_e_to_s(M,C,X1,M1,C1).
cross_e_to_s(M,C,X,M1,C1):-
        % cannibal on the boat
        C1 is C+1,
        C1 =< 3,
        X1 is X-1,
        cross_e_to_s(M,C,X1,M1,C1).

validate_state(t(M,C,_)):-
        C1 is 3-C,
        M1 is 3-M,
        nobody_eaten(M,C),
        nobody_eaten(M1,C1).

nobody_eaten(0,_). % no missionares
nobody_eaten(M,C):- % missionares >= cannibals 
        M >= C.

no_loop(NewSt, Acc):-
        \+ memb(NewSt,Acc).

memb(X,[X|_]).
memb(X,[_|T]):-
        memb(X,T).

%----------------------------------
% water jugs
%----------------------------------
natural(1).
natural(N) :-
        natural(N1),
        N is N1+1.