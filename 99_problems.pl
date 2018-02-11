% SOURCE: http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% P01 - last elem
my_last(X,[X]).
my_last(X,[_H|T]):-
        my_last(X,T).

% P02
last_but_one(X,[X,_]).
last_but_one(X,[_H|T]):-
        last_but_one(X,T).

% P03: element_at(Res,L,X) - find Res: Xth elem in L
element_at(H,[H|_],1).
element_at(Res,[_H|T],K):-
        K>1,
        K1 is K-1, % decrementing K1 as size of array decreases       
        element_at(Res,T,K1).

% P04
num_of_elem([],0).
num_of_elem([_H|T],X):-
        num_of_elem(T,X1),
        X is X1 + 1.

% P05: reverse a list
reverse(L1,L2):-my_rev(L1,L2,[]).
my_rev([],L2,L2):- !. % when L is empty, do not backtrack anymore
my_rev([H|T],L2,Acc):-
      my_rev(T,L2,[H|Acc]).  % put every element in front of L

% P06: palindrome list
is_palindrome(L):-
        reverse(L,L).

% P07: flatten list
flatten([],[]).
flatten(X,[X]):- % when X is int
        integer(X),
        X \= [].
flatten(X,[X]):- % when X is atom
        atom(X),
        X \= [].

flatten([H|T],Res):- 
        flatten(H,Res1), 
        flatten(T,Res2),
        conc(Res1,Res2,Res).

conc([],L,L).
conc([H|L1],L2,[H|L3]):-
        conc(L1,L2,L3).

% P08
compress(L,Res):-
        compress(L,Res,[]).
compress([],Res,Res).
compress([H|T],Res,Acc):-
        \+memb(H,Acc),
        conc(Acc,[H],Acc1),
        compress(T,Res,Acc1).

compress([H|T],Res,Acc):-
        memb(H,Acc),
        compress(T,Res,Acc).

memb(X,[X|_]).
memb(X,[_H|T]):-
        memb(X,T).

% P09
% pack(L1,L2) :- the list L2 is obtained from the list L1 by packing
%    repeated occurrences of elements into separate sublists.
%    (list,list) (+,?)

pack([],[]).
pack([X|Xs],[Z|Zs]) :-
        transfer(X,Xs,Ys,Z), % transfer all elements similar to X from Xs to list Z, leaving what was left in Ys
        pack(Ys,Zs).

% transfer(X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed and transfered to Z

transfer(X,[],[],[X]). % base case: X is the only element
transfer(X,[Y|Ys],[Y|Ys],[X]) :- % when X != Y, stop
        X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- % when X=Y, put X in the packed list
        transfer(X,Xs,Ys,Zs).



% P10 (*):  Run-length encoding of a list

% encode(L1,L2) :- the list L2 is obtained from the list L1 by run-length
%    encoding. Consecutive duplicates of elements are encoded as terms [N,E],
%    where N is the number of duplicates of the element E.
%    (list,list) (+,?)
encode(L1,L2):-
        pack(L1,L),
        transform(L,L2).

transform([],[]).
transform([[X|Xs]|Ys],[[N,X]|Zs]) :-
                length([X|Xs],N),
                transform(Ys,Zs).


% P11 (*):  Modified run-length encoding

% encode_modified(L1,L2) :- the list L2 is obtained from the list L1 by 
%    run-length encoding. Consecutive duplicates of elements are encoded 
%    as terms [N,E], where N is the number of duplicates of the element E.
%    However, if N equals 1 then the element is simply copied into the 
%    output list.
%    (list,list) (+,?)

encode_modified(L1,L2):-
        encode(L1,L),
        strip(L,L2).

strip([],[]).
strip([[1,X]|T1],[X|T2]) :- 
        strip(T1,T2).
strip([[N|X]|T1],[[N|X]|T2]):-
        N > 1,
        strip(T1,T2).


% P12 (**) Decode a run-length encoded list.
decode(L1,L2):-
        unpack(L1,L2).

unpack([],[]).
unpack([X|T1],[X|T2]):-
        atom(X),
        unpack(T1,T2).
unpack([[N,El]|T1],[H2|T2]):-
        multiply(N,El,H2),
        unpack(T1,T2).

% P14: diplicalte1
dupl([],[]).
dupl([H|T],[H,H|T1]):-
        dupl(T,T1).

% P15: diplicalte given num of times
dupl([H|T],N,L):-
         duplicate_n_times(H,N,L),
         dupl(T,N,L).

% compress
comp([],[]).
comp([X],[X]).
comp([X,X|Xs], Zs) :-!,
        comp([X|Xs], Zs).
comp([X,Y|Xs],[X|Zs]):-
        comp([Y|Zs],Zs).
         
        
        
