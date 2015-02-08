% TechTree 

:- use_module(library(ugraphs)).
:- use_module(handcodedDB, [course/5]).
:- use_module(bool_alg, [bool_to_list/3]).

% receive from front-end
course_taken('CSE 11').
course_taken('CSE 12').
course_goal('CSE 30').

%Shane's Code

%cse12
requirement('CSE ','12',bool(or(val('CSE 8B'),val('CSE 11')))).
%cse30
requirement('CSE ', '30',bool(and(val('CSE 12'),val('CSE 15L')))).
%cse100
requirement('CSE ','100',bool(and(and(and(or(val('CSE 21'),val('MATH 15B')),val('CSE 12')),val('CSE 15L')),or(or(or(val('CSE 5A'),val('CSE 30')),val('ECE 15')),val('MAE 9'))))).
%cse110
requirement('CSE ', '110',bool(and(val('CSE 12'),or(val('CSE 21'),val('MATH 15B'))))).

requirement_to_list(Dept,ID,X):-
    requirement(Dept,ID,bool(B)),
    bool_to_list(B,X,_Y).



course_list(L) :-
    findall(X, course(X, _, _, _ ,_), L).

course_taken_list(L) :-
    findall(X, course_taken(X), L). 

course_remain_list(L) :-
    course_list(X),
    course_taken_list(Y),
    subtract(X, Y, L). 


% X is a pre-req of Y
is_prereq(X, Y) :-
    dif(X, Y),
    course(X, _, _, _, _),
    requirement_to_list(Dep,ID,L),
    atom_concat(Dep, ID, Y),
    member(X, L).


% Tree contanis ALL courses
prereq_tree(T) :-
    course_list(Vertices),
    findall(Y-X, is_prereq(X, Y), Edges),
    vertices_edges_to_ugraph(Vertices, Edges, T).

% simple order without course_taken
order(L) :-
    prereq_tree(G),
    course_goal(J),
    reachable(J, G, L).

    
