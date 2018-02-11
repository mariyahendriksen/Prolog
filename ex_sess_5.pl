reacts(vinegar,salt,25).
reacts(salt,water,3).
reacts('brown soap',water,10).
reacts('pili pili', milk,7).
reacts(tonic,bailey,8).

reaction(A,B,N):-
        reacts(A,B,N);
        reacts(B,A,N).
reaction(_A,_B,0).

advice(L):-
        calc_reac(L,N),
        message(N).

calc_reac([],0).
calc_reac([H|T],N):-
        calc_reac2(H,T,N1),
        calc_reac(T,N2),
        N is N1+N2.

calc_reac2(_A,[],0).
calc_reac2(A,[H|T],N):-
        reaction(A,H,N1),
        calc_reac2(A,T,N2),
        N is N1+N2.

message(N):-
        N < 5,
        write('No irritation');
        N >= 5,
        N < 12,
        write('Minor irritation');
        N >= 12,
        N < 20,
        write('Minor burning wounds');
        N >= 20,
        N < 30,
        write('Severe burning wounds');
        N >= 30,
        write('Lethal').


%------------------------------------------------
% SQUARES
%------------------------------------------------
% [cube(t,s,t,r,t,c),cube(t,r,c,s,r,s),cube(s,c,s,r,c,t),cube(r,s,t,r,c,c)]

%------------------------------------------------
% GENERATE AND TEST
%------------------------------------------------
generate_and_test([Cube1,Cube2,Cube3,Cube4],[O1,O2,O3,O4]) :-
        % generate a stack with the cubes in some orientation:
        is_orientation(O1), % 24 possibilities
        is_orientation(O2),
        is_orientation(O3),
        is_orientation(O4),
        % test the stack:
        test_stack([Cube1,Cube2,Cube3,Cube4],[O1,O2,O3,O4]).

%------------------------------------------------
% STEPWISE
%------------------------------------------------
stepwise([Cube1,Cube2,Cube3,Cube4],[O1,O2,O3,O4]):-
        is_orientation(O1), % 24 possibilities
        is_orientation(O2),
        test_cube_stack(Cube2,O2,Cube1,O1),
        is_orientation(O3),
        test_cube_stack(Cube3,O3,[Cube1,Cube2],[O1,O2]),
        is_orientation(O4),
        test_cube_stack(Cube4,O4,[Cube1,Cube2,Cube3],[O1,O2,O3]).

test_stack([],[]).
test_stack([Cube|Cubes],[O|Os]):-
        test_cube_stack(Cube,O,Cubes,Os),
        test_stack(Cubes,Os).

test_cube_stack(_,_,[],[]).
test_cube_stack(Cube1,O1,[Cube2|Cubes],[O2|Os]):-
        test_two_cubes(Cube1,O1,Cube2,O2),
        test_cube_stack(Cube1,O1,Cubes,Os).

test_two_cubes(C1,O1,C2,O2):-
        test_side(C1,O1,C2,O2,front),
        test_side(C1,O1,C2,O2,left),
        test_side(C1,O1,C2,O2,back),
        test_side(C1,O1,C2,O2,right).

test_side(C1,O1,C2,O2,Side):-
        get_figure(C1,O1,Side,Fig1),
        get_figure(C2,O2,Side,Fig2),
        Fig1 \= Fig2.

get_figure(C,O,Side,Fig):-
        get_face(O,Side,Face),
        get_figure_on_face(C,Face,Fig).

get_face(o(Face,_,_,_,_,_),top,Face).
get_face(o(_,Face,_,_,_,_),bottom,Face).
get_face(o(_,_,Face,_,_,_),front,Face).
get_face(o(_,_,_,Face,_,_),right,Face).
get_face(o(_,_,_,_,Face,_),back,Face).
get_face(o(_,_,_,_,_,Face),left,Face).

get_figure_on_face(cube(Fig,_,_,_,_,_),1,Fig).
get_figure_on_face(cube(_,Fig,_,_,_,_),2,Fig).
get_figure_on_face(cube(_,_,Fig,_,_,_),3,Fig).
get_figure_on_face(cube(_,_,_,Fig,_,_),4,Fig).
get_figure_on_face(cube(_,_,_,_,Fig,_),5,Fig).
get_figure_on_face(cube(_,_,_,_,_,Fig),6,Fig).

        
% o(T,Bo,F,R,Ba,L)
is_orientation(o(1,5,2,3,4,6)).
is_orientation(o(1,5,3,2,6,4)).
is_orientation(o(1,5,4,3,2,6)).
is_orientation(o(1,5,6,4,3,2)).
is_orientation(o(2,4,1,3,5,6)).
is_orientation(o(2,4,3,5,6,1)).
is_orientation(o(2,4,5,6,1,3)). 
is_orientation(o(2,4,6,1,3,5)).
is_orientation(o(3,6,1,4,5,2)). 
is_orientation(o(3,6,2,1,4,5)).
is_orientation(o(3,6,4,5,2,1)).
is_orientation(o(3,6,5,2,1,4)).
is_orientation(o(4,2,1,6,5,3)).
is_orientation(o(4,2,3,1,6,5)).
is_orientation(o(4,2,5,3,1,6)).
is_orientation(o(4,2,6,5,3,1)).
is_orientation(o(5,1,2,3,4,6)).  
is_orientation(o(5,1,3,2,6,4)).
is_orientation(o(5,1,4,6,2,3)).
is_orientation(o(5,1,6,2,3,4)). 
is_orientation(o(6,3,1,2,5,4)).   
is_orientation(o(6,3,2,5,4,1)).
is_orientation(o(6,3,4,1,2,5)).
is_orientation(o(6,3,5,4,1,2)).


%------------------------------------------------
% REDEFINING ORIENTATIONS
%------------------------------------------------
is_basic_orientation(o(1,5,2,6,4,3)). 
is_basic_orientation(o(2,4,1,3,5,6)).                     
is_basic_orientation(o(3,6,1,4,5,2)).

is_orientation(O):-
        is_basic_orientation(B),
        member(Degrees,[0,90,180,270]),
        turn_horizontally(B,Degrees,O).

is_orientation(O):-
        is_basic_orientation(B),
        turn_upside_down(B,TurnedB),
        member(Degrees,[0,90,180,270]),
        turn_horizontally(TurnedB,Degrees,O).

turn_horizontally(o(T,Bo,F,R,Ba,L),0,o(T,Bo,F,R,Ba,L)).
turn_horizontally(o(T,Bo,F,R,Ba,L),90,o(T,Bo,R,Ba,L,F)).
turn_horizontally(o(T,Bo,F,R,Ba,L),180,o(T,Bo,Ba,L,F,R)).
turn_horizontally(o(T,Bo,F,R,Ba,L),270,o(T,Bo,L,F,R,Ba)).

turn_upside_down(o(T,Bo,F,R,Ba,L),o(Bo,T,F,R,Ba,L)).

%------------------------------------------------
% Designing cubes for the stacking problem
%------------------------------------------------
find_4_cubes(C1,C2,C3,C4):-
        generate_cube(C1),
        generate_cube(C2),
        generate_cube(C3),
        generate_cube(C4),
        stepwise([C1,C2,C3,C4], _Solution),
        write([C1,C2,C3,C3]).

generate_cube(cube(T,Bo,F,R,Ba,L)):-
         member(T,[t,s,r,c]),
         member(Bo,[t,s,r,c]),
         member(F,[t,s,r,c]),
         member(R,[t,s,r,c]),
         member(Ba,[t,s,r,c]),
         member(L,[t,s,r,c]).