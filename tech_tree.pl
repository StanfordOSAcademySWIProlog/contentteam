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
%prereqs('CSE','30', ['CSE 12', 'CSE 11'] ).

prereqs('CSE','30', ['CSE', '12'] ).
prereqs('CSE','12', ['CSE', '11'] ).
prereqs('CSE','21', ['CSE', '20'] ).
prereqs('CSE','100', ['CSE', '30', 'CSE' ,'21'] ).

% prereqs('CSE 30', ['CSE 12', 'CSE 11'] ).

% receive from front-end
course_taken('CSE', '11').
course_taken('CSE', '12').
course_goal('CSE', '30').

%-----------------------------------------------------

course_list(L) :-
    findall(X, course(_, X, _, _), L).

course_taken_list(L) :-
    findall(X, course_taken(_, X), L). 

course_remain_list(L) :-
    course_list(X),
    course_taken_list(Y),
    subtract(X, Y, L). 

% X is a pre-req of Y
is_prereq(X, Y) :-
    course(_, X, _, _),
    prereqs(_, Y, L),
    member(X, L).

% Tree contanis ALL courses
prereq_tree(T) :-
    course_list(Vertices),
    findall(Y-X, is_prereq(X, Y), Edges),
    vertices_edges_to_ugraph(Vertices, Edges, T).

% simple order without course_taken
order(L) :-
    prereq_tree(G),
    course_goal(_, J),
    reachable(J, G, L).

    
