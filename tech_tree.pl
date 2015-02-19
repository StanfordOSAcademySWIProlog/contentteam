% TechTree 

:- module(tech_tree, [prereq_tree/1]).
:- use_module(library(ugraphs)).

% for now
:- use_module(handcodedDB, [major/2, course/5, requirement/3]).
:- use_module(handcodedINPUT, [course_taken/1]).
:- use_module(prereq_proc, [requirement_to_list/3, requirement_to_string/3]).


course_list(L) :-
    findall(X, course(X, _, _, _ ,_), L).

% get from front-end
course_taken_list(L) :-
   findall(X, course_taken(X), L). 

course_remain_list(L) :-
    course_list(X),
    course_taken_list(Y),
    subtract(X, Y, L). 

% X is a pre-req of Y
% use with caution, is_prereq(X, 'CSE 100') => X will contain duplicates
is_prereq(X, Y) :-
    dif(X, Y),
    requirement_to_list(Dep,ID,L),
    atom_concat(Dep, ' ', T),
    atom_concat(T, ID, Y),
    member(X, L).

% Tree contanis ALL courses in the db
prereq_tree(T) :-
    course_list(Vertices),
    findall(Y-X, is_prereq(X, Y), Edges),
    vertices_edges_to_ugraph(Vertices, Edges, T).

    
