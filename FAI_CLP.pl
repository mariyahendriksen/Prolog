% code for basic solution of the stacking squares
% suggested query: main.

:- use_module(library(clpfd)).

main :-
        solutions
        ;
        halt.

main2 :-
        solutions
        ;
        fail.

solutions :-
        statistics(runtime,_),
        fd_statistics(backtracks,_),
        write('=========================[ Solutions ]========================='),
        nl,
        (       the_problem(__S),
                write('[A,B,C,D] = '), write(__S),
                nl,
                fail
        ;
                write('=======================[ End Solutions ]======================='),
                nl,
                fail
        ).

the_problem([A,B,C,D]) :-
        length(A,25),
        domain(A,0,62),
        numbered_list(A,AL,1),
        length(B,16),
        domain(B,0,62),
        numbered_list(B,BL,1),
        length(C,9),
        domain(C,0,62),
        numbered_list(C,CL,1),
        length(D,4),
        domain(D,0,62),
        numbered_list(D,DL,1),
        all_constraints([AL,BL,CL,DL]),
        myappend([],A,__Tmp1),
        myappend(__Tmp1,B,__Tmp2),
        myappend(__Tmp2,C,__Tmp3),
        myappend(__Tmp3,D,__Tmp4),
        labeling([leftmost, step, up], __Tmp4),
        fd_statistics(backtracks,__BT),
        write('Backtracks: '), write(__BT), nl,
        statistics(runtime,[_,__RT]),
        __RTsec is __RT / 1000,
        write('Runtime: '), write(__RTsec), write('s'), nl.

find_var([(V,N)|_], N, V) :- !.
find_var([_|Vs], N, V) :-
        find_var(Vs, N, V).

numbered_list([X],[(X,Acc)],Acc) :- !.
numbered_list([X|Xs],[(X,Acc)|XNs],Acc) :-
        Acc1 is Acc + 1,
        numbered_list(Xs,XNs,Acc1).

myappend([],L,L).
myappend([X|L1],L2,[X|L12]) :-
        myappend(L1,L2,L12).

all_constraints([A,B,C,D]) :-
        write('Adding constraint "'), write('A_A = 0 \\/ A_A = 1 \\/ A_A = 2 \\/ A_A = 3 \\/ A_A = 4 \\/ A_A = 9 \\/ A_A = 10 \\/ A_A = 11 \\/ A_A = 12 \\/ A_A = 13 \\/ A_A = 18 \\/ A_A = 19 \\/ A_A = 20 \\/ A_A = 21 \\/ A_A = 22"'), write(' for values:'), nl,
        the_constraint1(A),
        write('Adding constraint "'), write('A_B = A_A+1"'), write(' for values:'), nl,
        the_constraint2(A,A),
        write('Adding constraint "'), write('A_C = A_A+2"'), write(' for values:'), nl,
        the_constraint3(A,A),
        write('Adding constraint "'), write('A_D = A_A+3"'), write(' for values:'), nl,
        the_constraint4(A,A),
        write('Adding constraint "'), write('A_E = A_A+4"'), write(' for values:'), nl,
        the_constraint5(A,A),
        write('Adding constraint "'), write('A_F = A_A+9"'), write(' for values:'), nl,
        the_constraint6(A,A),
        write('Adding constraint "'), write('A_G = A_F+1"'), write(' for values:'), nl,
        the_constraint7(A,A),
        write('Adding constraint "'), write('A_H = A_F+2"'), write(' for values:'), nl,
        the_constraint8(A,A),
        write('Adding constraint "'), write('A_I = A_F+3"'), write(' for values:'), nl,
        the_constraint9(A,A),
        write('Adding constraint "'), write('A_J = A_F+4"'), write(' for values:'), nl,
        the_constraint10(A,A),
        write('Adding constraint "'), write('A_K = A_A+18"'), write(' for values:'), nl,
        the_constraint11(A,A),
        write('Adding constraint "'), write('A_L = A_K+1"'), write(' for values:'), nl,
        the_constraint12(A,A),
        write('Adding constraint "'), write('A_M = A_K+2"'), write(' for values:'), nl,
        the_constraint13(A,A),
        write('Adding constraint "'), write('A_N = A_K+3"'), write(' for values:'), nl,
        the_constraint14(A,A),
        write('Adding constraint "'), write('A_O = A_K+4"'), write(' for values:'), nl,
        the_constraint15(A,A),
        write('Adding constraint "'), write('A_P = A_A+27"'), write(' for values:'), nl,
        the_constraint16(A,A),
        write('Adding constraint "'), write('A_Q = A_P+1"'), write(' for values:'), nl,
        the_constraint17(A,A),
        write('Adding constraint "'), write('A_R = A_P+2"'), write(' for values:'), nl,
        the_constraint18(A,A),
        write('Adding constraint "'), write('A_S = A_P+3"'), write(' for values:'), nl,
        the_constraint19(A,A),
        write('Adding constraint "'), write('A_T = A_P+4"'), write(' for values:'), nl,
        the_constraint20(A,A),
        write('Adding constraint "'), write('A_U = A_A+36"'), write(' for values:'), nl,
        the_constraint21(A,A),
        write('Adding constraint "'), write('A_V = A_U+1"'), write(' for values:'), nl,
        the_constraint22(A,A),
        write('Adding constraint "'), write('A_W = A_U+2"'), write(' for values:'), nl,
        the_constraint23(A,A),
        write('Adding constraint "'), write('A_X = A_U+3"'), write(' for values:'), nl,
        the_constraint24(A,A),
        write('Adding constraint "'), write('A_Y = A_U+4"'), write(' for values:'), nl,
        the_constraint25(A,A),
        write('Adding constraint "'), write('B_A = 0 \\/ B_A = 1 \\/ B_A = 2 \\/ B_A = 3 \\/ B_A = 4 \\/ B_A = 5 \\/ B_A = 9 \\/ B_A = 10 \\/ B_A = 11 \\/ B_A = 12 \\/ B_A = 13 \\/ B_A = 14 \\/ B_A = 18 \\/ B_A = 19 \\/ B_A = 20 \\/ B_A = 21 \\/ B_A = 22 \\/ B_A = 23 \\/ B_A = 27 \\/ B_A = 28 \\/ B_A = 29 \\/ B_A = 30 \\/ B_A = 31 \\/ B_A = 32"'), write(' for values:'), nl,
        the_constraint26(B),
        write('Adding constraint "'), write('B_B = B_A+1"'), write(' for values:'), nl,
        the_constraint27(B,B),
        write('Adding constraint "'), write('B_C = B_A+2"'), write(' for values:'), nl,
        the_constraint28(B,B),
        write('Adding constraint "'), write('B_D = B_A+3"'), write(' for values:'), nl,
        the_constraint29(B,B),
        write('Adding constraint "'), write('B_E = B_A + 9"'), write(' for values:'), nl,
        the_constraint30(B,B),
        write('Adding constraint "'), write('B_F = B_E+1"'), write(' for values:'), nl,
        the_constraint31(B,B),
        write('Adding constraint "'), write('B_G = B_E+2"'), write(' for values:'), nl,
        the_constraint32(B,B),
        write('Adding constraint "'), write('B_H = B_E+3"'), write(' for values:'), nl,
        the_constraint33(B,B),
        write('Adding constraint "'), write('B_I = B_A + 18"'), write(' for values:'), nl,
        the_constraint34(B,B),
        write('Adding constraint "'), write('B_J = B_I+1"'), write(' for values:'), nl,
        the_constraint35(B,B),
        write('Adding constraint "'), write('B_K = B_I+2"'), write(' for values:'), nl,
        the_constraint36(B,B),
        write('Adding constraint "'), write('B_L = B_I+3"'), write(' for values:'), nl,
        the_constraint37(B,B),
        write('Adding constraint "'), write('B_M = B_A + 27"'), write(' for values:'), nl,
        the_constraint38(B,B),
        write('Adding constraint "'), write('B_N = B_M+1"'), write(' for values:'), nl,
        the_constraint39(B,B),
        write('Adding constraint "'), write('B_O = B_M+2"'), write(' for values:'), nl,
        the_constraint40(B,B),
        write('Adding constraint "'), write('B_P = B_M+3"'), write(' for values:'), nl,
        the_constraint41(B,B),
        write('Adding constraint "'), write('A_I \\= B_J"'), write(' for values:'), nl,
        the_constraint42(A,B),
        write('Adding constraint "'), write('C_A mod 9 \\= 7 /\\ C_A mod 9 \\= 8 "'), write(' for values:'), nl,
        the_constraint43(C),
        write('Adding constraint "'), write('C_A / 9 \\= 5 /\\ C_A / 9 \\=6, A=1"'), write(' for values:'), nl,
        the_constraint44(C),
        write('Adding constraint "'), write('C_B = C_A+1"'), write(' for values:'), nl,
        the_constraint45(C,C),
        write('Adding constraint "'), write('C_C = C_A+2"'), write(' for values:'), nl,
        the_constraint46(C,C),
        write('Adding constraint "'), write('C_D = C_A + 9"'), write(' for values:'), nl,
        the_constraint47(C,C),
        write('Adding constraint "'), write('C_E = C_D+1"'), write(' for values:'), nl,
        the_constraint48(C,C),
        write('Adding constraint "'), write('C_F = C_D+2"'), write(' for values:'), nl,
        the_constraint49(C,C),
        write('Adding constraint "'), write('C_G = C_A + 18"'), write(' for values:'), nl,
        the_constraint50(C,C),
        write('Adding constraint "'), write('C_H = C_G+1"'), write(' for values:'), nl,
        the_constraint51(C,C),
        write('Adding constraint "'), write('C_I = C_G+2"'), write(' for values:'), nl,
        the_constraint52(C,C),
        write('Adding constraint "'), write('A_I \\= C_J"'), write(' for values:'), nl,
        the_constraint53(A,C),
        write('Adding constraint "'), write('B_I \\= C_J"'), write(' for values:'), nl,
        the_constraint54(B,C),
        write('Adding constraint "'), write('D_A mod 9 \\= 8 /\\ D_A / 9 \\= 7 "'), write(' for values:'), nl,
        the_constraint55(D),
        write('Adding constraint "'), write('D_B = D_A + 1"'), write(' for values:'), nl,
        the_constraint56(D,D),
        write('Adding constraint "'), write('D_C = D_A + 9"'), write(' for values:'), nl,
        the_constraint57(D,D),
        write('Adding constraint "'), write('D_D = D_C + 1"'), write(' for values:'), nl,
        the_constraint58(D,D),
        write('Adding constraint "'), write('D_I \\= A_J"'), write(' for values:'), nl,
        the_constraint59(D,A),
        write('Adding constraint "'), write('D_I \\= B_J"'), write(' for values:'), nl,
        the_constraint60(D,B),
        write('Adding constraint "'), write('D_I \\= C_J"'), write(' for values:'), nl,
        the_constraint61(D,C).

the_constraint1([]).
the_constraint1([A_A|A_As]) :-
        the_constraint1_aux(A_A),
        the_constraint1(A_As).

the_constraint1_aux((A_A,A)) :- !,
        (A=:=1 ->
                write('\t'),
                write('A='), write(A), write(', '),
                nl,
                A_A #= 0 #\/ A_A #= 1 #\/ A_A #= 2 #\/ A_A #= 3 #\/ A_A #= 4 #\/ A_A #= 9 #\/ A_A #= 10 #\/ A_A #= 11 #\/ A_A #= 12 #\/ A_A #= 13 #\/ A_A #= 18 #\/ A_A #= 19 #\/ A_A #= 20 #\/ A_A #= 21 #\/ A_A #= 22
        ;
                true
        ).
the_constraint1_aux(_).

the_constraint2([],_).
the_constraint2([A_B|A_Bs],A_A) :-
        the_constraint2_aux(A_B,A_A),
        the_constraint2(A_Bs,A_A).

the_constraint2_aux(_,[]).
the_constraint2_aux(A_B,[A_A|A_As]) :-
        the_constraint2_aux_aux(A_B,A_A),
        the_constraint2_aux(A_B,A_As).

the_constraint2_aux_aux((A_B,B), (A_A,A)) :- !,
        (A=:=1,B=:=2 ->
                write('\t'),
                write('B='), write(B), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_B #= A_A+1
        ;
                true
        ).
the_constraint2_aux_aux(_,_).

the_constraint3([],_).
the_constraint3([A_C|A_Cs],A_A) :-
        the_constraint3_aux(A_C,A_A),
        the_constraint3(A_Cs,A_A).

the_constraint3_aux(_,[]).
the_constraint3_aux(A_C,[A_A|A_As]) :-
        the_constraint3_aux_aux(A_C,A_A),
        the_constraint3_aux(A_C,A_As).

the_constraint3_aux_aux((A_C,C), (A_A,A)) :- !,
        (A=:=1, C=:=3 ->
                write('\t'),
                write('C='), write(C), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_C #= A_A+2
        ;
                true
        ).
the_constraint3_aux_aux(_,_).

the_constraint4([],_).
the_constraint4([A_D|A_Ds],A_A) :-
        the_constraint4_aux(A_D,A_A),
        the_constraint4(A_Ds,A_A).

the_constraint4_aux(_,[]).
the_constraint4_aux(A_D,[A_A|A_As]) :-
        the_constraint4_aux_aux(A_D,A_A),
        the_constraint4_aux(A_D,A_As).

the_constraint4_aux_aux((A_D,D), (A_A,A)) :- !,
        (A=:=1, D=:=4 ->
                write('\t'),
                write('D='), write(D), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_D #= A_A+3
        ;
                true
        ).
the_constraint4_aux_aux(_,_).

the_constraint5([],_).
the_constraint5([A_E|A_Es],A_A) :-
        the_constraint5_aux(A_E,A_A),
        the_constraint5(A_Es,A_A).

the_constraint5_aux(_,[]).
the_constraint5_aux(A_E,[A_A|A_As]) :-
        the_constraint5_aux_aux(A_E,A_A),
        the_constraint5_aux(A_E,A_As).

the_constraint5_aux_aux((A_E,E), (A_A,A)) :- !,
        (A=:=1, E=:=5 ->
                write('\t'),
                write('E='), write(E), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_E #= A_A+4
        ;
                true
        ).
the_constraint5_aux_aux(_,_).

the_constraint6([],_).
the_constraint6([A_F|A_Fs],A_A) :-
        the_constraint6_aux(A_F,A_A),
        the_constraint6(A_Fs,A_A).

the_constraint6_aux(_,[]).
the_constraint6_aux(A_F,[A_A|A_As]) :-
        the_constraint6_aux_aux(A_F,A_A),
        the_constraint6_aux(A_F,A_As).

the_constraint6_aux_aux((A_F,F), (A_A,A)) :- !,
        (A=:=1,F=:=6 ->
                write('\t'),
                write('F='), write(F), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_F #= A_A+9
        ;
                true
        ).
the_constraint6_aux_aux(_,_).

the_constraint7([],_).
the_constraint7([A_G|A_Gs],A_F) :-
        the_constraint7_aux(A_G,A_F),
        the_constraint7(A_Gs,A_F).

the_constraint7_aux(_,[]).
the_constraint7_aux(A_G,[A_F|A_Fs]) :-
        the_constraint7_aux_aux(A_G,A_F),
        the_constraint7_aux(A_G,A_Fs).

the_constraint7_aux_aux((A_G,G), (A_F,F)) :- !,
        (G=:=7,F=:=6 ->
                write('\t'),
                write('G='), write(G), write(', '),
                write('F='), write(F), write(', '),
                nl,
                A_G #= A_F+1
        ;
                true
        ).
the_constraint7_aux_aux(_,_).

the_constraint8([],_).
the_constraint8([A_H|A_Hs],A_F) :-
        the_constraint8_aux(A_H,A_F),
        the_constraint8(A_Hs,A_F).

the_constraint8_aux(_,[]).
the_constraint8_aux(A_H,[A_F|A_Fs]) :-
        the_constraint8_aux_aux(A_H,A_F),
        the_constraint8_aux(A_H,A_Fs).

the_constraint8_aux_aux((A_H,H), (A_F,F)) :- !,
        (H=:=8, F=:=6 ->
                write('\t'),
                write('H='), write(H), write(', '),
                write('F='), write(F), write(', '),
                nl,
                A_H #= A_F+2
        ;
                true
        ).
the_constraint8_aux_aux(_,_).

the_constraint9([],_).
the_constraint9([A_I|A_Is],A_F) :-
        the_constraint9_aux(A_I,A_F),
        the_constraint9(A_Is,A_F).

the_constraint9_aux(_,[]).
the_constraint9_aux(A_I,[A_F|A_Fs]) :-
        the_constraint9_aux_aux(A_I,A_F),
        the_constraint9_aux(A_I,A_Fs).

the_constraint9_aux_aux((A_I,I), (A_F,F)) :- !,
        (I=:=9, F=:=6 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('F='), write(F), write(', '),
                nl,
                A_I #= A_F+3
        ;
                true
        ).
the_constraint9_aux_aux(_,_).

the_constraint10([],_).
the_constraint10([A_J|A_Js],A_F) :-
        the_constraint10_aux(A_J,A_F),
        the_constraint10(A_Js,A_F).

the_constraint10_aux(_,[]).
the_constraint10_aux(A_J,[A_F|A_Fs]) :-
        the_constraint10_aux_aux(A_J,A_F),
        the_constraint10_aux(A_J,A_Fs).

the_constraint10_aux_aux((A_J,J), (A_F,F)) :- !,
        (J=:=10, F=:=6 ->
                write('\t'),
                write('J='), write(J), write(', '),
                write('F='), write(F), write(', '),
                nl,
                A_J #= A_F+4
        ;
                true
        ).
the_constraint10_aux_aux(_,_).

the_constraint11([],_).
the_constraint11([A_K|A_Ks],A_A) :-
        the_constraint11_aux(A_K,A_A),
        the_constraint11(A_Ks,A_A).

the_constraint11_aux(_,[]).
the_constraint11_aux(A_K,[A_A|A_As]) :-
        the_constraint11_aux_aux(A_K,A_A),
        the_constraint11_aux(A_K,A_As).

the_constraint11_aux_aux((A_K,K), (A_A,A)) :- !,
        (A =:= 1, K =:= 11 ->
                write('\t'),
                write('K='), write(K), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_K #= A_A+18
        ;
                true
        ).
the_constraint11_aux_aux(_,_).

the_constraint12([],_).
the_constraint12([A_L|A_Ls],A_K) :-
        the_constraint12_aux(A_L,A_K),
        the_constraint12(A_Ls,A_K).

the_constraint12_aux(_,[]).
the_constraint12_aux(A_L,[A_K|A_Ks]) :-
        the_constraint12_aux_aux(A_L,A_K),
        the_constraint12_aux(A_L,A_Ks).

the_constraint12_aux_aux((A_L,L), (A_K,K)) :- !,
        (L=:=12,K=:=11 ->
                write('\t'),
                write('L='), write(L), write(', '),
                write('K='), write(K), write(', '),
                nl,
                A_L #= A_K+1
        ;
                true
        ).
the_constraint12_aux_aux(_,_).

the_constraint13([],_).
the_constraint13([A_M|A_Ms],A_K) :-
        the_constraint13_aux(A_M,A_K),
        the_constraint13(A_Ms,A_K).

the_constraint13_aux(_,[]).
the_constraint13_aux(A_M,[A_K|A_Ks]) :-
        the_constraint13_aux_aux(A_M,A_K),
        the_constraint13_aux(A_M,A_Ks).

the_constraint13_aux_aux((A_M,M), (A_K,K)) :- !,
        (M=:=13,K=:=11 ->
                write('\t'),
                write('M='), write(M), write(', '),
                write('K='), write(K), write(', '),
                nl,
                A_M #= A_K+2
        ;
                true
        ).
the_constraint13_aux_aux(_,_).

the_constraint14([],_).
the_constraint14([A_N|A_Ns],A_K) :-
        the_constraint14_aux(A_N,A_K),
        the_constraint14(A_Ns,A_K).

the_constraint14_aux(_,[]).
the_constraint14_aux(A_N,[A_K|A_Ks]) :-
        the_constraint14_aux_aux(A_N,A_K),
        the_constraint14_aux(A_N,A_Ks).

the_constraint14_aux_aux((A_N,N), (A_K,K)) :- !,
        (N=:=14,K=:=11 ->
                write('\t'),
                write('N='), write(N), write(', '),
                write('K='), write(K), write(', '),
                nl,
                A_N #= A_K+3
        ;
                true
        ).
the_constraint14_aux_aux(_,_).

the_constraint15([],_).
the_constraint15([A_O|A_Os],A_K) :-
        the_constraint15_aux(A_O,A_K),
        the_constraint15(A_Os,A_K).

the_constraint15_aux(_,[]).
the_constraint15_aux(A_O,[A_K|A_Ks]) :-
        the_constraint15_aux_aux(A_O,A_K),
        the_constraint15_aux(A_O,A_Ks).

the_constraint15_aux_aux((A_O,O), (A_K,K)) :- !,
        (O=:=15,K=:=11 ->
                write('\t'),
                write('O='), write(O), write(', '),
                write('K='), write(K), write(', '),
                nl,
                A_O #= A_K+4
        ;
                true
        ).
the_constraint15_aux_aux(_,_).

the_constraint16([],_).
the_constraint16([A_P|A_Ps],A_A) :-
        the_constraint16_aux(A_P,A_A),
        the_constraint16(A_Ps,A_A).

the_constraint16_aux(_,[]).
the_constraint16_aux(A_P,[A_A|A_As]) :-
        the_constraint16_aux_aux(A_P,A_A),
        the_constraint16_aux(A_P,A_As).

the_constraint16_aux_aux((A_P,P), (A_A,A)) :- !,
        (A =:= 1, P =:= 16 ->
                write('\t'),
                write('P='), write(P), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_P #= A_A+27
        ;
                true
        ).
the_constraint16_aux_aux(_,_).

the_constraint17([],_).
the_constraint17([A_Q|A_Qs],A_P) :-
        the_constraint17_aux(A_Q,A_P),
        the_constraint17(A_Qs,A_P).

the_constraint17_aux(_,[]).
the_constraint17_aux(A_Q,[A_P|A_Ps]) :-
        the_constraint17_aux_aux(A_Q,A_P),
        the_constraint17_aux(A_Q,A_Ps).

the_constraint17_aux_aux((A_Q,Q), (A_P,P)) :- !,
        (Q=:=17, P =:= 16 ->
                write('\t'),
                write('Q='), write(Q), write(', '),
                write('P='), write(P), write(', '),
                nl,
                A_Q #= A_P+1
        ;
                true
        ).
the_constraint17_aux_aux(_,_).

the_constraint18([],_).
the_constraint18([A_R|A_Rs],A_P) :-
        the_constraint18_aux(A_R,A_P),
        the_constraint18(A_Rs,A_P).

the_constraint18_aux(_,[]).
the_constraint18_aux(A_R,[A_P|A_Ps]) :-
        the_constraint18_aux_aux(A_R,A_P),
        the_constraint18_aux(A_R,A_Ps).

the_constraint18_aux_aux((A_R,R), (A_P,P)) :- !,
        (R=:=18, P =:= 16 ->
                write('\t'),
                write('R='), write(R), write(', '),
                write('P='), write(P), write(', '),
                nl,
                A_R #= A_P+2
        ;
                true
        ).
the_constraint18_aux_aux(_,_).

the_constraint19([],_).
the_constraint19([A_S|A_Ss],A_P) :-
        the_constraint19_aux(A_S,A_P),
        the_constraint19(A_Ss,A_P).

the_constraint19_aux(_,[]).
the_constraint19_aux(A_S,[A_P|A_Ps]) :-
        the_constraint19_aux_aux(A_S,A_P),
        the_constraint19_aux(A_S,A_Ps).

the_constraint19_aux_aux((A_S,S), (A_P,P)) :- !,
        (S=:=19, P =:= 16 ->
                write('\t'),
                write('S='), write(S), write(', '),
                write('P='), write(P), write(', '),
                nl,
                A_S #= A_P+3
        ;
                true
        ).
the_constraint19_aux_aux(_,_).

the_constraint20([],_).
the_constraint20([A_T|A_Ts],A_P) :-
        the_constraint20_aux(A_T,A_P),
        the_constraint20(A_Ts,A_P).

the_constraint20_aux(_,[]).
the_constraint20_aux(A_T,[A_P|A_Ps]) :-
        the_constraint20_aux_aux(A_T,A_P),
        the_constraint20_aux(A_T,A_Ps).

the_constraint20_aux_aux((A_T,T), (A_P,P)) :- !,
        (T=:=20, P =:= 16 ->
                write('\t'),
                write('T='), write(T), write(', '),
                write('P='), write(P), write(', '),
                nl,
                A_T #= A_P+4
        ;
                true
        ).
the_constraint20_aux_aux(_,_).

the_constraint21([],_).
the_constraint21([A_U|A_Us],A_A) :-
        the_constraint21_aux(A_U,A_A),
        the_constraint21(A_Us,A_A).

the_constraint21_aux(_,[]).
the_constraint21_aux(A_U,[A_A|A_As]) :-
        the_constraint21_aux_aux(A_U,A_A),
        the_constraint21_aux(A_U,A_As).

the_constraint21_aux_aux((A_U,U), (A_A,A)) :- !,
        (A =:= 1, U =:= 21 ->
                write('\t'),
                write('U='), write(U), write(', '),
                write('A='), write(A), write(', '),
                nl,
                A_U #= A_A+36
        ;
                true
        ).
the_constraint21_aux_aux(_,_).

the_constraint22([],_).
the_constraint22([A_V|A_Vs],A_U) :-
        the_constraint22_aux(A_V,A_U),
        the_constraint22(A_Vs,A_U).

the_constraint22_aux(_,[]).
the_constraint22_aux(A_V,[A_U|A_Us]) :-
        the_constraint22_aux_aux(A_V,A_U),
        the_constraint22_aux(A_V,A_Us).

the_constraint22_aux_aux((A_V,V), (A_U,U)) :- !,
        (V=:=22, U =:= 21 ->
                write('\t'),
                write('V='), write(V), write(', '),
                write('U='), write(U), write(', '),
                nl,
                A_V #= A_U+1
        ;
                true
        ).
the_constraint22_aux_aux(_,_).

the_constraint23([],_).
the_constraint23([A_W|A_Ws],A_U) :-
        the_constraint23_aux(A_W,A_U),
        the_constraint23(A_Ws,A_U).

the_constraint23_aux(_,[]).
the_constraint23_aux(A_W,[A_U|A_Us]) :-
        the_constraint23_aux_aux(A_W,A_U),
        the_constraint23_aux(A_W,A_Us).

the_constraint23_aux_aux((A_W,W), (A_U,U)) :- !,
        (W=:=23, U =:= 21 ->
                write('\t'),
                write('W='), write(W), write(', '),
                write('U='), write(U), write(', '),
                nl,
                A_W #= A_U+2
        ;
                true
        ).
the_constraint23_aux_aux(_,_).

the_constraint24([],_).
the_constraint24([A_X|A_Xs],A_U) :-
        the_constraint24_aux(A_X,A_U),
        the_constraint24(A_Xs,A_U).

the_constraint24_aux(_,[]).
the_constraint24_aux(A_X,[A_U|A_Us]) :-
        the_constraint24_aux_aux(A_X,A_U),
        the_constraint24_aux(A_X,A_Us).

the_constraint24_aux_aux((A_X,X), (A_U,U)) :- !,
        (X=:=24, U =:= 21 ->
                write('\t'),
                write('X='), write(X), write(', '),
                write('U='), write(U), write(', '),
                nl,
                A_X #= A_U+3
        ;
                true
        ).
the_constraint24_aux_aux(_,_).

the_constraint25([],_).
the_constraint25([A_Y|A_Ys],A_U) :-
        the_constraint25_aux(A_Y,A_U),
        the_constraint25(A_Ys,A_U).

the_constraint25_aux(_,[]).
the_constraint25_aux(A_Y,[A_U|A_Us]) :-
        the_constraint25_aux_aux(A_Y,A_U),
        the_constraint25_aux(A_Y,A_Us).

the_constraint25_aux_aux((A_Y,Y), (A_U,U)) :- !,
        (Y=:=25, U =:= 21 ->
                write('\t'),
                write('Y='), write(Y), write(', '),
                write('U='), write(U), write(', '),
                nl,
                A_Y #= A_U+4
        ;
                true
        ).
the_constraint25_aux_aux(_,_).

the_constraint26([]).
the_constraint26([B_A|B_As]) :-
        the_constraint26_aux(B_A),
        the_constraint26(B_As).

the_constraint26_aux((B_A,A)) :- !,
        (A=:=1 ->
                write('\t'),
                write('A='), write(A), write(', '),
                nl,
                B_A #= 0 #\/ B_A #= 1 #\/ B_A #= 2 #\/ B_A #= 3 #\/ B_A #= 4 #\/ B_A #= 5 #\/ B_A #= 9 #\/ B_A #= 10 #\/ B_A #= 11 #\/ B_A #= 12 #\/ B_A #= 13 #\/ B_A #= 14 #\/ B_A #= 18 #\/ B_A #= 19 #\/ B_A #= 20 #\/ B_A #= 21 #\/ B_A #= 22 #\/ B_A #= 23 #\/ B_A #= 27 #\/ B_A #= 28 #\/ B_A #= 29 #\/ B_A #= 30 #\/ B_A #= 31 #\/ B_A #= 32
        ;
                true
        ).
the_constraint26_aux(_).

the_constraint27([],_).
the_constraint27([B_B|B_Bs],B_A) :-
        the_constraint27_aux(B_B,B_A),
        the_constraint27(B_Bs,B_A).

the_constraint27_aux(_,[]).
the_constraint27_aux(B_B,[B_A|B_As]) :-
        the_constraint27_aux_aux(B_B,B_A),
        the_constraint27_aux(B_B,B_As).

the_constraint27_aux_aux((B_B,B), (B_A,A)) :- !,
        (A=:=1,B=:=2 ->
                write('\t'),
                write('B='), write(B), write(', '),
                write('A='), write(A), write(', '),
                nl,
                B_B #= B_A+1
        ;
                true
        ).
the_constraint27_aux_aux(_,_).

the_constraint28([],_).
the_constraint28([B_C|B_Cs],B_A) :-
        the_constraint28_aux(B_C,B_A),
        the_constraint28(B_Cs,B_A).

the_constraint28_aux(_,[]).
the_constraint28_aux(B_C,[B_A|B_As]) :-
        the_constraint28_aux_aux(B_C,B_A),
        the_constraint28_aux(B_C,B_As).

the_constraint28_aux_aux((B_C,C), (B_A,A)) :- !,
        (A=:=1, C=:=3 ->
                write('\t'),
                write('C='), write(C), write(', '),
                write('A='), write(A), write(', '),
                nl,
                B_C #= B_A+2
        ;
                true
        ).
the_constraint28_aux_aux(_,_).

the_constraint29([],_).
the_constraint29([B_D|B_Ds],B_A) :-
        the_constraint29_aux(B_D,B_A),
        the_constraint29(B_Ds,B_A).

the_constraint29_aux(_,[]).
the_constraint29_aux(B_D,[B_A|B_As]) :-
        the_constraint29_aux_aux(B_D,B_A),
        the_constraint29_aux(B_D,B_As).

the_constraint29_aux_aux((B_D,D), (B_A,A)) :- !,
        (A=:=1, D=:=4 ->
                write('\t'),
                write('D='), write(D), write(', '),
                write('A='), write(A), write(', '),
                nl,
                B_D #= B_A+3
        ;
                true
        ).
the_constraint29_aux_aux(_,_).

the_constraint30([],_).
the_constraint30([B_E|B_Es],B_A) :-
        the_constraint30_aux(B_E,B_A),
        the_constraint30(B_Es,B_A).

the_constraint30_aux(_,[]).
the_constraint30_aux(B_E,[B_A|B_As]) :-
        the_constraint30_aux_aux(B_E,B_A),
        the_constraint30_aux(B_E,B_As).

the_constraint30_aux_aux((B_E,E), (B_A,A)) :- !,
        (A=:=1, E =:= 5 ->
                write('\t'),
                write('E='), write(E), write(', '),
                write('A='), write(A), write(', '),
                nl,
                B_E #= B_A + 9
        ;
                true
        ).
the_constraint30_aux_aux(_,_).

the_constraint31([],_).
the_constraint31([B_F|B_Fs],B_E) :-
        the_constraint31_aux(B_F,B_E),
        the_constraint31(B_Fs,B_E).

the_constraint31_aux(_,[]).
the_constraint31_aux(B_F,[B_E|B_Es]) :-
        the_constraint31_aux_aux(B_F,B_E),
        the_constraint31_aux(B_F,B_Es).

the_constraint31_aux_aux((B_F,F), (B_E,E)) :- !,
        (E=:=5, F=:=6 ->
                write('\t'),
                write('F='), write(F), write(', '),
                write('E='), write(E), write(', '),
                nl,
                B_F #= B_E+1
        ;
                true
        ).
the_constraint31_aux_aux(_,_).

the_constraint32([],_).
the_constraint32([B_G|B_Gs],B_E) :-
        the_constraint32_aux(B_G,B_E),
        the_constraint32(B_Gs,B_E).

the_constraint32_aux(_,[]).
the_constraint32_aux(B_G,[B_E|B_Es]) :-
        the_constraint32_aux_aux(B_G,B_E),
        the_constraint32_aux(B_G,B_Es).

the_constraint32_aux_aux((B_G,G), (B_E,E)) :- !,
        (E=:=5, G=:=7 ->
                write('\t'),
                write('G='), write(G), write(', '),
                write('E='), write(E), write(', '),
                nl,
                B_G #= B_E+2
        ;
                true
        ).
the_constraint32_aux_aux(_,_).

the_constraint33([],_).
the_constraint33([B_H|B_Hs],B_E) :-
        the_constraint33_aux(B_H,B_E),
        the_constraint33(B_Hs,B_E).

the_constraint33_aux(_,[]).
the_constraint33_aux(B_H,[B_E|B_Es]) :-
        the_constraint33_aux_aux(B_H,B_E),
        the_constraint33_aux(B_H,B_Es).

the_constraint33_aux_aux((B_H,H), (B_E,E)) :- !,
        (E=:=5, H=:=8 ->
                write('\t'),
                write('H='), write(H), write(', '),
                write('E='), write(E), write(', '),
                nl,
                B_H #= B_E+3
        ;
                true
        ).
the_constraint33_aux_aux(_,_).

the_constraint34([],_).
the_constraint34([B_I|B_Is],B_A) :-
        the_constraint34_aux(B_I,B_A),
        the_constraint34(B_Is,B_A).

the_constraint34_aux(_,[]).
the_constraint34_aux(B_I,[B_A|B_As]) :-
        the_constraint34_aux_aux(B_I,B_A),
        the_constraint34_aux(B_I,B_As).

the_constraint34_aux_aux((B_I,I), (B_A,A)) :- !,
        (A=:=1, I =:= 9 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('A='), write(A), write(', '),
                nl,
                B_I #= B_A + 18
        ;
                true
        ).
the_constraint34_aux_aux(_,_).

the_constraint35([],_).
the_constraint35([B_J|B_Js],B_I) :-
        the_constraint35_aux(B_J,B_I),
        the_constraint35(B_Js,B_I).

the_constraint35_aux(_,[]).
the_constraint35_aux(B_J,[B_I|B_Is]) :-
        the_constraint35_aux_aux(B_J,B_I),
        the_constraint35_aux(B_J,B_Is).

the_constraint35_aux_aux((B_J,J), (B_I,I)) :- !,
        (I=:=9, J=:=10 ->
                write('\t'),
                write('J='), write(J), write(', '),
                write('I='), write(I), write(', '),
                nl,
                B_J #= B_I+1
        ;
                true
        ).
the_constraint35_aux_aux(_,_).

the_constraint36([],_).
the_constraint36([B_K|B_Ks],B_I) :-
        the_constraint36_aux(B_K,B_I),
        the_constraint36(B_Ks,B_I).

the_constraint36_aux(_,[]).
the_constraint36_aux(B_K,[B_I|B_Is]) :-
        the_constraint36_aux_aux(B_K,B_I),
        the_constraint36_aux(B_K,B_Is).

the_constraint36_aux_aux((B_K,K), (B_I,I)) :- !,
        (I=:=9, K=:=11 ->
                write('\t'),
                write('K='), write(K), write(', '),
                write('I='), write(I), write(', '),
                nl,
                B_K #= B_I+2
        ;
                true
        ).
the_constraint36_aux_aux(_,_).

the_constraint37([],_).
the_constraint37([B_L|B_Ls],B_I) :-
        the_constraint37_aux(B_L,B_I),
        the_constraint37(B_Ls,B_I).

the_constraint37_aux(_,[]).
the_constraint37_aux(B_L,[B_I|B_Is]) :-
        the_constraint37_aux_aux(B_L,B_I),
        the_constraint37_aux(B_L,B_Is).

the_constraint37_aux_aux((B_L,L), (B_I,I)) :- !,
        (I=:=9, L=:=12 ->
                write('\t'),
                write('L='), write(L), write(', '),
                write('I='), write(I), write(', '),
                nl,
                B_L #= B_I+3
        ;
                true
        ).
the_constraint37_aux_aux(_,_).

the_constraint38([],_).
the_constraint38([B_M|B_Ms],B_A) :-
        the_constraint38_aux(B_M,B_A),
        the_constraint38(B_Ms,B_A).

the_constraint38_aux(_,[]).
the_constraint38_aux(B_M,[B_A|B_As]) :-
        the_constraint38_aux_aux(B_M,B_A),
        the_constraint38_aux(B_M,B_As).

the_constraint38_aux_aux((B_M,M), (B_A,A)) :- !,
        (A=:=1, M =:= 13 ->
                write('\t'),
                write('M='), write(M), write(', '),
                write('A='), write(A), write(', '),
                nl,
                B_M #= B_A + 27
        ;
                true
        ).
the_constraint38_aux_aux(_,_).

the_constraint39([],_).
the_constraint39([B_N|B_Ns],B_M) :-
        the_constraint39_aux(B_N,B_M),
        the_constraint39(B_Ns,B_M).

the_constraint39_aux(_,[]).
the_constraint39_aux(B_N,[B_M|B_Ms]) :-
        the_constraint39_aux_aux(B_N,B_M),
        the_constraint39_aux(B_N,B_Ms).

the_constraint39_aux_aux((B_N,N), (B_M,M)) :- !,
        (M=:=13, N=:=14 ->
                write('\t'),
                write('N='), write(N), write(', '),
                write('M='), write(M), write(', '),
                nl,
                B_N #= B_M+1
        ;
                true
        ).
the_constraint39_aux_aux(_,_).

the_constraint40([],_).
the_constraint40([B_O|B_Os],B_M) :-
        the_constraint40_aux(B_O,B_M),
        the_constraint40(B_Os,B_M).

the_constraint40_aux(_,[]).
the_constraint40_aux(B_O,[B_M|B_Ms]) :-
        the_constraint40_aux_aux(B_O,B_M),
        the_constraint40_aux(B_O,B_Ms).

the_constraint40_aux_aux((B_O,O), (B_M,M)) :- !,
        (M=:=13, O=:=15 ->
                write('\t'),
                write('O='), write(O), write(', '),
                write('M='), write(M), write(', '),
                nl,
                B_O #= B_M+2
        ;
                true
        ).
the_constraint40_aux_aux(_,_).

the_constraint41([],_).
the_constraint41([B_P|B_Ps],B_M) :-
        the_constraint41_aux(B_P,B_M),
        the_constraint41(B_Ps,B_M).

the_constraint41_aux(_,[]).
the_constraint41_aux(B_P,[B_M|B_Ms]) :-
        the_constraint41_aux_aux(B_P,B_M),
        the_constraint41_aux(B_P,B_Ms).

the_constraint41_aux_aux((B_P,P), (B_M,M)) :- !,
        (M=:=13, P=:=16 ->
                write('\t'),
                write('P='), write(P), write(', '),
                write('M='), write(M), write(', '),
                nl,
                B_P #= B_M+3
        ;
                true
        ).
the_constraint41_aux_aux(_,_).

the_constraint42([],_).
the_constraint42([A_I|A_Is],B_J) :-
        the_constraint42_aux(A_I,B_J),
        the_constraint42(A_Is,B_J).

the_constraint42_aux(_,[]).
the_constraint42_aux(A_I,[B_J|B_Js]) :-
        the_constraint42_aux_aux(A_I,B_J),
        the_constraint42_aux(A_I,B_Js).

the_constraint42_aux_aux((A_I,I), (B_J,J)) :- !,
        (I>0, J>0 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('J='), write(J), write(', '),
                nl,
                A_I #\= B_J
        ;
                true
        ).
the_constraint42_aux_aux(_,_).

the_constraint43([]).
the_constraint43([C_A|C_As]) :-
        the_constraint43_aux(C_A),
        the_constraint43(C_As).

the_constraint43_aux((C_A,A)) :- !,
        (A=:=1 ->
                write('\t'),
                write('A='), write(A), write(', '),
                nl,
                C_A mod 9 #\= 7 #/\ C_A mod 9 #\= 8 
        ;
                true
        ).
the_constraint43_aux(_).

the_constraint44([]).
the_constraint44([C_A|C_As]) :-
        the_constraint44_aux(C_A),
        the_constraint44(C_As).

the_constraint44_aux((C_A,A)) :- !,
        (A=:=1 ->
                write('\t'),
                write('A='), write(A), write(', '),
                nl,
                C_A / 9 #\= 5 #/\ C_A / 9 #\=6, A#=1
        ;
                true
        ).
the_constraint44_aux(_).

the_constraint45([],_).
the_constraint45([C_B|C_Bs],C_A) :-
        the_constraint45_aux(C_B,C_A),
        the_constraint45(C_Bs,C_A).

the_constraint45_aux(_,[]).
the_constraint45_aux(C_B,[C_A|C_As]) :-
        the_constraint45_aux_aux(C_B,C_A),
        the_constraint45_aux(C_B,C_As).

the_constraint45_aux_aux((C_B,B), (C_A,A)) :- !,
        (A=:=1, B=:=2 ->
                write('\t'),
                write('B='), write(B), write(', '),
                write('A='), write(A), write(', '),
                nl,
                C_B #= C_A+1
        ;
                true
        ).
the_constraint45_aux_aux(_,_).

the_constraint46([],_).
the_constraint46([C_C|C_Cs],C_A) :-
        the_constraint46_aux(C_C,C_A),
        the_constraint46(C_Cs,C_A).

the_constraint46_aux(_,[]).
the_constraint46_aux(C_C,[C_A|C_As]) :-
        the_constraint46_aux_aux(C_C,C_A),
        the_constraint46_aux(C_C,C_As).

the_constraint46_aux_aux((C_C,C), (C_A,A)) :- !,
        (A=:=1, C=:=3 ->
                write('\t'),
                write('C='), write(C), write(', '),
                write('A='), write(A), write(', '),
                nl,
                C_C #= C_A+2
        ;
                true
        ).
the_constraint46_aux_aux(_,_).

the_constraint47([],_).
the_constraint47([C_D|C_Ds],C_A) :-
        the_constraint47_aux(C_D,C_A),
        the_constraint47(C_Ds,C_A).

the_constraint47_aux(_,[]).
the_constraint47_aux(C_D,[C_A|C_As]) :-
        the_constraint47_aux_aux(C_D,C_A),
        the_constraint47_aux(C_D,C_As).

the_constraint47_aux_aux((C_D,D), (C_A,A)) :- !,
        (A=:=1, D =:= 4 ->
                write('\t'),
                write('D='), write(D), write(', '),
                write('A='), write(A), write(', '),
                nl,
                C_D #= C_A + 9
        ;
                true
        ).
the_constraint47_aux_aux(_,_).

the_constraint48([],_).
the_constraint48([C_E|C_Es],C_D) :-
        the_constraint48_aux(C_E,C_D),
        the_constraint48(C_Es,C_D).

the_constraint48_aux(_,[]).
the_constraint48_aux(C_E,[C_D|C_Ds]) :-
        the_constraint48_aux_aux(C_E,C_D),
        the_constraint48_aux(C_E,C_Ds).

the_constraint48_aux_aux((C_E,E), (C_D,D)) :- !,
        (D=:=4, E=:=5 ->
                write('\t'),
                write('E='), write(E), write(', '),
                write('D='), write(D), write(', '),
                nl,
                C_E #= C_D+1
        ;
                true
        ).
the_constraint48_aux_aux(_,_).

the_constraint49([],_).
the_constraint49([C_F|C_Fs],C_D) :-
        the_constraint49_aux(C_F,C_D),
        the_constraint49(C_Fs,C_D).

the_constraint49_aux(_,[]).
the_constraint49_aux(C_F,[C_D|C_Ds]) :-
        the_constraint49_aux_aux(C_F,C_D),
        the_constraint49_aux(C_F,C_Ds).

the_constraint49_aux_aux((C_F,F), (C_D,D)) :- !,
        (D=:=4, F=:=6 ->
                write('\t'),
                write('F='), write(F), write(', '),
                write('D='), write(D), write(', '),
                nl,
                C_F #= C_D+2
        ;
                true
        ).
the_constraint49_aux_aux(_,_).

the_constraint50([],_).
the_constraint50([C_G|C_Gs],C_A) :-
        the_constraint50_aux(C_G,C_A),
        the_constraint50(C_Gs,C_A).

the_constraint50_aux(_,[]).
the_constraint50_aux(C_G,[C_A|C_As]) :-
        the_constraint50_aux_aux(C_G,C_A),
        the_constraint50_aux(C_G,C_As).

the_constraint50_aux_aux((C_G,G), (C_A,A)) :- !,
        (A=:=1, G =:= 7 ->
                write('\t'),
                write('G='), write(G), write(', '),
                write('A='), write(A), write(', '),
                nl,
                C_G #= C_A + 18
        ;
                true
        ).
the_constraint50_aux_aux(_,_).

the_constraint51([],_).
the_constraint51([C_H|C_Hs],C_G) :-
        the_constraint51_aux(C_H,C_G),
        the_constraint51(C_Hs,C_G).

the_constraint51_aux(_,[]).
the_constraint51_aux(C_H,[C_G|C_Gs]) :-
        the_constraint51_aux_aux(C_H,C_G),
        the_constraint51_aux(C_H,C_Gs).

the_constraint51_aux_aux((C_H,H), (C_G,G)) :- !,
        (G=:=7, H=:=8 ->
                write('\t'),
                write('H='), write(H), write(', '),
                write('G='), write(G), write(', '),
                nl,
                C_H #= C_G+1
        ;
                true
        ).
the_constraint51_aux_aux(_,_).

the_constraint52([],_).
the_constraint52([C_I|C_Is],C_G) :-
        the_constraint52_aux(C_I,C_G),
        the_constraint52(C_Is,C_G).

the_constraint52_aux(_,[]).
the_constraint52_aux(C_I,[C_G|C_Gs]) :-
        the_constraint52_aux_aux(C_I,C_G),
        the_constraint52_aux(C_I,C_Gs).

the_constraint52_aux_aux((C_I,I), (C_G,G)) :- !,
        (G=:=7, I=:=9 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('G='), write(G), write(', '),
                nl,
                C_I #= C_G+2
        ;
                true
        ).
the_constraint52_aux_aux(_,_).

the_constraint53([],_).
the_constraint53([A_I|A_Is],C_J) :-
        the_constraint53_aux(A_I,C_J),
        the_constraint53(A_Is,C_J).

the_constraint53_aux(_,[]).
the_constraint53_aux(A_I,[C_J|C_Js]) :-
        the_constraint53_aux_aux(A_I,C_J),
        the_constraint53_aux(A_I,C_Js).

the_constraint53_aux_aux((A_I,I), (C_J,J)) :- !,
        (I>0, J>0 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('J='), write(J), write(', '),
                nl,
                A_I #\= C_J
        ;
                true
        ).
the_constraint53_aux_aux(_,_).

the_constraint54([],_).
the_constraint54([B_I|B_Is],C_J) :-
        the_constraint54_aux(B_I,C_J),
        the_constraint54(B_Is,C_J).

the_constraint54_aux(_,[]).
the_constraint54_aux(B_I,[C_J|C_Js]) :-
        the_constraint54_aux_aux(B_I,C_J),
        the_constraint54_aux(B_I,C_Js).

the_constraint54_aux_aux((B_I,I), (C_J,J)) :- !,
        (I>0, J>0 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('J='), write(J), write(', '),
                nl,
                B_I #\= C_J
        ;
                true
        ).
the_constraint54_aux_aux(_,_).

the_constraint55([]).
the_constraint55([D_A|D_As]) :-
        the_constraint55_aux(D_A),
        the_constraint55(D_As).

the_constraint55_aux((D_A,A)) :- !,
        (A=:=1 ->
                write('\t'),
                write('A='), write(A), write(', '),
                nl,
                D_A mod 9 #\= 8 #/\ D_A / 9 #\= 7 
        ;
                true
        ).
the_constraint55_aux(_).

the_constraint56([],_).
the_constraint56([D_B|D_Bs],D_A) :-
        the_constraint56_aux(D_B,D_A),
        the_constraint56(D_Bs,D_A).

the_constraint56_aux(_,[]).
the_constraint56_aux(D_B,[D_A|D_As]) :-
        the_constraint56_aux_aux(D_B,D_A),
        the_constraint56_aux(D_B,D_As).

the_constraint56_aux_aux((D_B,B), (D_A,A)) :- !,
        (A=:=1, B=:=2 ->
                write('\t'),
                write('B='), write(B), write(', '),
                write('A='), write(A), write(', '),
                nl,
                D_B #= D_A + 1
        ;
                true
        ).
the_constraint56_aux_aux(_,_).

the_constraint57([],_).
the_constraint57([D_C|D_Cs],D_A) :-
        the_constraint57_aux(D_C,D_A),
        the_constraint57(D_Cs,D_A).

the_constraint57_aux(_,[]).
the_constraint57_aux(D_C,[D_A|D_As]) :-
        the_constraint57_aux_aux(D_C,D_A),
        the_constraint57_aux(D_C,D_As).

the_constraint57_aux_aux((D_C,C), (D_A,A)) :- !,
        (A=:=1, C=:=3 ->
                write('\t'),
                write('C='), write(C), write(', '),
                write('A='), write(A), write(', '),
                nl,
                D_C #= D_A + 9
        ;
                true
        ).
the_constraint57_aux_aux(_,_).

the_constraint58([],_).
the_constraint58([D_D|D_Ds],D_C) :-
        the_constraint58_aux(D_D,D_C),
        the_constraint58(D_Ds,D_C).

the_constraint58_aux(_,[]).
the_constraint58_aux(D_D,[D_C|D_Cs]) :-
        the_constraint58_aux_aux(D_D,D_C),
        the_constraint58_aux(D_D,D_Cs).

the_constraint58_aux_aux((D_D,D), (D_C,C)) :- !,
        (D=:=4, C=:=3 ->
                write('\t'),
                write('D='), write(D), write(', '),
                write('C='), write(C), write(', '),
                nl,
                D_D #= D_C + 1
        ;
                true
        ).
the_constraint58_aux_aux(_,_).

the_constraint59([],_).
the_constraint59([D_I|D_Is],A_J) :-
        the_constraint59_aux(D_I,A_J),
        the_constraint59(D_Is,A_J).

the_constraint59_aux(_,[]).
the_constraint59_aux(D_I,[A_J|A_Js]) :-
        the_constraint59_aux_aux(D_I,A_J),
        the_constraint59_aux(D_I,A_Js).

the_constraint59_aux_aux((D_I,I), (A_J,J)) :- !,
        (I>0, J>0 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('J='), write(J), write(', '),
                nl,
                D_I #\= A_J
        ;
                true
        ).
the_constraint59_aux_aux(_,_).

the_constraint60([],_).
the_constraint60([D_I|D_Is],B_J) :-
        the_constraint60_aux(D_I,B_J),
        the_constraint60(D_Is,B_J).

the_constraint60_aux(_,[]).
the_constraint60_aux(D_I,[B_J|B_Js]) :-
        the_constraint60_aux_aux(D_I,B_J),
        the_constraint60_aux(D_I,B_Js).

the_constraint60_aux_aux((D_I,I), (B_J,J)) :- !,
        (I>0, J>0 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('J='), write(J), write(', '),
                nl,
                D_I #\= B_J
        ;
                true
        ).
the_constraint60_aux_aux(_,_).

the_constraint61([],_).
the_constraint61([D_I|D_Is],C_J) :-
        the_constraint61_aux(D_I,C_J),
        the_constraint61(D_Is,C_J).

the_constraint61_aux(_,[]).
the_constraint61_aux(D_I,[C_J|C_Js]) :-
        the_constraint61_aux_aux(D_I,C_J),
        the_constraint61_aux(D_I,C_Js).

the_constraint61_aux_aux((D_I,I), (C_J,J)) :- !,
        (I>0, J>0 ->
                write('\t'),
                write('I='), write(I), write(', '),
                write('J='), write(J), write(', '),
                nl,
                D_I #\= C_J
        ;
                true
        ).
the_constraint61_aux_aux(_,_).

