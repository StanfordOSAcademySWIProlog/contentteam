%TechTree 

:- module(tech_tree, [prereq_tree/1, order/2, get_next/1]).
:- use_module(library(ugraphs)).

:- use_module(db).
:- use_module(db2).

:- use_module(input).
:- use_module(prereq_proc).
:- use_module(req).

course_list(L) :-
    findall(X, course(X, _,_,_ ,_), L).

% get from front-end
course_taken_list(L) :-
   findall(X, course_taken(X), L). 

course_remain_list(L) :-
    course_list(X),
    course_taken_list(Y),
    subtract(X, Y, L). 

% To generate course-[prereq(s)] 

req(Course, L) :-
  findall(Course-Prereq, requirement_to_list(Course, Prereq), L).

% Tree contanis ALL courses in the db
prereq_tree(T) :-
    course_list(Vertices),
    req(_, Edges),
    vertices_edges_to_ugraph(Vertices, Edges, T).

    
order(Goal, List) :-
    prereq_tree(T),
    transitive_closure(T, Full),
    reachable(Goal, Full, List).

req_list(L) :-
    findall(X, req(X), L).

req_remain_list(L, A, R) :-
    course_taken_list(X),
    req_list(B),
    subtract(B, X, L),
    length(L, S),
    A is 18-S,
    R is S.

get_next(C) :-
    req_remain_list(C, A, R).
    
