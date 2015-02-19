% TechTree 

:- module(tech_tree).
:- use_module(library(ugraphs)).

% for now
:- use_module(handcodedDB, [major/2, course/5, requirement/3]).
:- use_module(prereq_proc, [requirement_to_list/3, requirement_to_string/3]).


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


prereq_tree_min(Dep, ID, T) :-
    requirement_to_list(Dep, ID, L),





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

    
