listlength([],0).
listlength([_H|T],X):-
        listlength(T,X1),
        X is X1+1.

sum_of_numbers([],0).
sum_of_numbers([H|T],Sum):-
        sum_of_numbers(T,Sum1),
        Sum is Sum1+H.

average([],0).
average(L,Res):-
        sum_and_length(L,Sum,Len),
        Res is Sum/Len.

sum_and_length([],0,0).
sum_and_length([H|T],Sum,Len):-
        sum_and_length(T,Sum1,Len1),
        Sum is Sum1+H,
        Len is Len1+1.

% ex 6: countdisk: count w or b disks
countdisk([],0).

countdisk([A|T],X):-
        member(A,[b,w]),
        countdisk(T,X1),
        X is X1+1.
countdisk([n|T],X):-
        countdisk(T,X).

% ex 7
count_round([],0).
count_round([move(_,_,_,_)],1).
count_round([move(X,_,_,_),move(Y,_,_,_)|T],Res):-
        X \= Y,
        count_round(T,Res1),
        Res is Res1+1.

% splitting the list
split([],[],[]).
split([A],[A],[]).
split([A,B|T],[A|T1],[B|T2]):-
        split(T,T1,T2).

merge(L1,L2,L):-
        split(L,L1,L2).
