compress([ ], [ ]) . 
compress([ X ], [ X ]) . 
compress([ X, X | Xs ], Zs) :- !, compress([ X | Xs ], Zs ).
compress([ X, Y | Xs ], [X | Zs]) :- compress([ Y | Xs ], Zs ).

%-------------------
% exam 1
%-------------------
% possad for list
% possad(tel(a,454), [tel(a,454), tel(b, 123)], X)
possad(X,S,S1):-
        \+ memb(X,S),
        add(X,S,S1).
possad(X,S,S):-
        memb(X,S),
        write('phone number is alredy in the list').

memb(X,[X|_]).
memb(X,[_H|T]):-
        memb(X,T).

add(X,S,[X|S]).

% possad for binary dictionary
% possad1( tel(a,4556), t( t(nil, tel(b,454), nil), tel(d,454), t(nil, tel(k, 123), nil)) , X).
possad1(X,T,T1):-
        \+ in(X,T),
        addleaf(X,T,T1).

possad1(X,T,T):-
        in(X,T),
        write('phone number is alredy in the balanced list').

addleaf(X, nil, t(nil,X,nil)).
addleaf(X, t(nil,X,nil), t(nil,X,nil)).
addleaf(X, t(L,E,R), t(L1,E,R)):-
        gt(E,X),
        write('X is smaller than root '), write(E),
        addleaf(X,L,L1).
addlead(X, t(L,E,R), t(L,E,R1)):-
        gt(X,E),
        write('X is bigger than root '), write(E),
        addleaf(X,R,R1).

in1(X, t(_,X,_)).
in1(X, t(L,E,_R)):-
        gt(E,X),
        in1(X,L).
in1(X,t(_,E,R)):-
        gt(X,E),
        in1(X,R).

gt(A,B):- A @> B.

%-------------------
% domino
%-------------------
% is_valid([d(0,2), d(1,1), d(1,0), d(2,1)], [o(1,2), o(3,6), o(9,8), o(7,4)]).
is_valid([Dom1, Dom2, Dom3, Dom4]):-
        % check dominos values
        check_vals([Dom1, Dom2, Dom3, Dom4]),
        check_row([Dom1, Dom2, Dom3, Dom4]),
        check_col1(Dom1,Dom4),
        check_col2(Dom2,Dom3).

check_vals([]).
check_vals([d(X,Y)|Doms]):-
        member(X,[0,1,2,3,4,5,6]),
        member(Y,[0,1,2,3,4,5,6]),
        check_vals(Doms).

check_row([]).
check_row([d(X,Y),d(X1,_)|Doms]):-
            3 is X+Y+X1,
            check_row(Doms).

check_col1(d(X,_),d(X1,Y1)):-
        3 is X+X1+Y1.

check_col2(d(X,Y),d(X1,_)):-
        3 is X+Y+X1.
        
is_orientation(o(1,2)).
is_orientation(o(3,6)).
is_orientation(o(7,4)).
is_orientation(o(9,8)).

% checking variants
are_variants(V1,V2):-
        is_valid(V1),
        is_valid(V2),
        rotate_and_check(V1,V2).

rotate_and_check(V1,V2):-
        member(Degree,[90,180,270]),
        rotate(V1,Degree,NewV1),
        NewV1 == V2.

rotate([Dom1,Dom2,Dom3,Dom4],90,[Dom4,Dom1,Dom2,Dom3]).
rotate([Dom1,Dom2,Dom3,Dom4],180,[Dom3,Dom4,Dom1,Dom2]).
rotate([Dom1,Dom2,Dom3,Dom4],270,[Dom2,Dom3,Dom4,Dom1]).


search_dom([Dom1,Dom2,Dom3,Dom4]):-
        X=[Dom1,Dom2,Dom3,Dom4],
        generate(L), % all the possible vals
        findall(Res,
                (assign(L,X), is_valid(X), Res=X),
                List),
        remove_variants(List,L1),
        write(N).
               
assign(_,[]).
assign(Vals, [Dom|Doms]):-
        del1(Dom,Vals,Vals1),
        assign(Vals1,Doms).

del1(El,[El|T],T).
del1(El,[H|T],[H|T1]):-
        del1(El,T,T1).
        
generate(L):-
                findall(d(X,Y),
                        (member(X,[0,1,2,3,4,5,6]), member(Y,[0,1,2,3,4,5,6])),
                       L).


%-------------------
% exam 2
%-------------------
drop(_,[],[]).
drop(X,[X|T1],T2):-
        drop(X,T1,T2).
drop(X,[H|T1],[H|T2]):-
        drop(H,T1,T2).


%-------------------
% Q3
%-------------------
% st(name, surname, gender, voice)
main:-
        student(Students),
        first_or_second(st(pat,_,_,_),st(_,_,_,bass), Students),
        at_least_one_of_2nd_and_3rd(st(_,_,_,tenor),Students),
        

student([st(_,_,_,_),
        st(_,_,_,_),
        st(_,_,_,_),
        st(_,_,_,_),
        st(_,_,_,_)]). 


% st(name, surname, gender, voice)
:- use_module(library(clpfd)).
main3:-
        Vars=[Soprano,Mezzo,Tenor,Bass,
              Chris,JP,Lee,Pat,Val,
              Kingsley,Robinson,Ulrich,Walker],
        Voice = [Soprano,Mezzo,Tenor,Bass],
        Name = [Chris,JP,Lee,Pat,Val],
        Surname = [Kingsley,Robinson,Ulrich,Walker],
        
        domain(Voice,1,5),
        domain(Name,1,5),
        domain(Surname,1,5),
        
        all_different(Name),
        
        (Pat #= 1 #/\ Bass #= 2) #<=> B1,
        (Pat #= 1 #/\ Bass #= 2) #<=> B2,
        B1 + B2 #= 1,
        
        (Tenor #= 2) #<=> C1,
        (Tenor #= 3) #<=> C2,
        C1 + C2 #> 0,
        
        Robinson #\= 5,
        
        (Kingsley #= Mezzo #/\ Tenor #= 5) #<=> D1,
        (Kingsley #= Tenor #/\ Mezzo #=5) #<=> D2,
        D1 + D2 #= 1,
        
        Robinson #= 3,
        Chris #\= 3,
        Walker #\= Chris,
        
        (Ulrich #\= Bass) #<=>A1,
        (Ulrich #\= Mezzo) #<=>A2,
        A1 + A2 #= 1,
        
        Lee #\= Tenor,
        Val #\= Tenor,
        Val #\= 3,
        
        JP #\= 3,
        Chris #\= 5,
        
        Bass #\= Robinson,
        labeling([],Vars),
        write(Vars), nl.
    
% Q3
% (Left-R+Right)
% preoder(Tree,non_nil_vals)
preoder(nil,[]).
preoder( (nil-X+nil), X ).   
preoder( (L-E+R), Res):-
        preoder(L,R1),
        preoder(R,R2),
        conc(R1,[E|R2],Res).

conc([],L,L).
conc([H|L1],L2,[H|L3]):-
        conc(L1,L2,L3). 

% with diff lists
preod1(T,R):-
        preod1(T,R-[]).
preoder1(nil,L-L). 
preoder1( (L-E+R), A-D):-
        preoder1(L,A-B),
        preoder1(R,C-D),
        B=[E|C].   


% tour
dist(heverlee,city,10).
dist(leuven,city,8).
dist(b,c,15).
dist(c,d,12).

distance(A,B,Cost):-
        dist(A,B,Cost).
distance(A,B,Cost):-
        dist(B,A,Cost).
distance(_A,_B,0).

stop(heverlee,4).
stop(leuven,10).
stop(city,10). 

% Graph
tour([leuven,city,heverlee]).

costBus(Sum):-
      tour(Stops),
      write(Stops),
      process_stops(Stops,Sum).  

process_stops([],0).
process_stops([A],0).
process_stops([From,To|T],S):-
        distance(From,To,Cost),
        process_stops([To|T],S1),
        S is S1+Cost.
        
%-------------------
% exam 3
%-------------------        
% Q3
:- use_module(library(clpfd)).
% Mon-1, Tues-2, Wed-3, Thur-4, Fr-5
main4:-
        Vars=[Alice,Bernadette,Charles,Duane,Eddie,
              Felicidad,Garber,Haller,Itakura,Jeffreys,
             Bioinformatics,Hygiene,Art,Nutrition,Study],
             
             Names=[Alice,Bernadette,Charles,Duane,Eddie],
             Surnames=[Felicidad,Garber,Haller,Itakura,Jeffreys],
             Subjects=[Bioinformatics,Hygiene,Art,Nutrition,Study],
             
             domain(Names,1,5),
             domain(Surnames,1,5),
             domain(Subjects,1,5),
             
             all_different(Names),
             all_different(Surnames),
             all_different(Subjects),
             
             Alice #=1,
             
             Charles #= Hygiene,
             Charles #\= 5,
             
             Jeffreys #= Nutrition,
             
             Art #= Charles #\/ Art #= Duane #\/ Art #= Eddie,
             
             next_to1(Itakura,Study),
             
             Haller #> Eddie,
             
             Duane #= Felicidad,
             Duane #< Art,
             labeling([],Vars),
             write(Vars), nl.

next_to1(A,B):-
        abs(A-B) #= 1.
     

% Q4
% facts
st(danny,fai,15).
st(danny,plpm,15).
st(toon,plpm,18).
st(toon,fai,14).
st(toon,uai,4).

% terms
get_list([st(danny,fai,15),st(danny,plpm,15),st(toon,plpm,18),st(toon,uai,4)]).

topscore(Course,TopSt):-
        findall(Score/Student, st(Student,Course,Score), L),
        sort_l(L,L1),
        L1=[_/TopSt|_T].

% reversed sort (start from the biggest)
sort_l([],[]).
sort_l([X],[X]).
sort_l([H,H1|T],[H|T1]):-
        gt(H,H1),
        sort_l([H1|T],T1).
sort_l([H,H1|T],[H1|T1]):-
        gt(H1,H),
        sort_l([H|T],T1).

% topscore for list
topscore1(Course,TopStudent):-
        get_list(L),
        collect_st(L,Course,L1),
        sort_l(L1,[_/TopStudent|_]).

collect_st(L,C,L1):-
        collect_st(L,C,[],L1).

collect_st([],_,Res,Res).
collect_st([st(Name,Course,Score)|T],Course,Acc,Res):-
        collect_st(T,Course,[Score/Name|Acc],Res).
collect_st([st(_,Course0,_)|T],Course,Acc,Res):-
        Course0 \= Course,
        collect_st(T,Course,Acc,Res).

% exam 3
sh([],B,B).
sh([H|A], [H|B], [H|S]):- sh(A,B,S).
sh([H|A],B,[H|S]):- sh(B,A,S).                      
        
        