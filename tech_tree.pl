% TechTree

:- use_module(library(ugraphs)).

course('CSE','3','HTML','4').
course('CSE','11','JAVA','4').
course('CSE','12','D structure', '4').
course('CSE','30', 'C' , '4').
course('CSE','20','MATH', '4').
course('CSE','21','MATH++', '4').
course('CSE','100','advanced D' , '4').

% three difference format
prereqs('CSE','30', ['CSE 12', 'CSE 11'] ).
prereqs('CSE','30', ['CSE', '12', 'CSE', '11'] ).
prereqs('CSE 30', ['CSE 12', 'CSE 11'] ).

course_taken('CSE', ['11' , '12']).


course_list(L) :-
    findall(X, course(X, _), L).

course_taken_list(L) :-
    findall(X, course_taken(X), L). 

course_remain_list(L) :-
    course_list(X),
    course_taken_list(Y),
    subtract(X, Y, L). 
  
no_prereq(X) :-
    course(X, []).

is_prereq(X, Y) :-
    course(Y, L),
    member(X, L).

prereq_tree(T) :-
    course_list(Vertices),
    findall(Y-X, is_prereq(X, Y), Edges),
    vertices_edges_to_ugraph(Vertices, Edges, T).


