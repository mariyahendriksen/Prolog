% include(Condition, List, NewList)
% add to the NewList all the elements from the List which satisfy the condition
include(_,[],[]).
include(Condition,[H|T],[H|T1]):-
        G =..[Condition,H],
        write(G),
        call(G),
        include(Condition,T,T1).
include(Condition,[_H|T],T1):-
        include(Condition,T,T1).

positive(X) :- X>0.

