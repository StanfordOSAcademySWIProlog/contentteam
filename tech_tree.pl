% Sample TechTree
% To be changed later

:- module(tech_tree, []).

:- use_module(library(lists)).

% Simple data: my_course(Name, Pre-req)
course('3',[]).
course('11',[]).
course('12',['11']).
course('30',['12']).
course('20',['11']).
course('21',['20']).
course('100',['30','21']).

course_taken('11').
course_taken('12').



course_list(L) :- findall(X, course(X, Y), L).
course_taken_list(L) :- findall(X, course_taken(X), L). 
course_remain_list(L) :- course_list(X), course_taken_list(Y), subtract(X, Y, L). 
  
no_prereq(X) :- course(X, L), length(L,0).
is_prereq(X,Y) :- course(Y, L), member(X, L).

% todo 
% able_to_take(X) :-
