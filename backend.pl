:- module(backend,[courses/1]).
:- license(lgpl).
% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses

:- use_module(db, [course/5]).

%% Return a list of courses
% Courses = [course(ID, Title, Units, Descr, Reqs), course(...), ...]
courses(Courses) :-
    findall(course(ID, Title, Units, Descr, Reqs),
        course(ID, Title, Units, Descr, Reqs),
        Courses).

% Represent prerequisites as a "boolean" expression
% - An operand (value) is the term `val(Op)`
% - An AND is the term `and(X, Y)`
% - An OR is the term `or(X, Y)`
% Examples:
% - (a AND b AND c):
%     and(val(a), and(val(b), val(c)))
%       OR
%     and(and(val(a), val(b)), val(c))
% - (a OR (b AND c)):
%     or(val(a), and(val(b), val(c)))
% - (a AND b AND (c OR d)) OR (f AND g):
%     or(and(val(a), and(val(b), or(val(c), val(d)))), and(val(f), val(g)))
% 
% ... and so on.
%
% Convert the boolean expresssions to lists,
% enumerating the different possibilities by backtracking
%
bool_to_list(none, Rest, Rest).
bool_to_list(val(X), [X|Rest], Rest).
bool_to_list(and(X, Y), L0, L1) :-
    bool_to_list(X, L0, L),
    bool_to_list(Y, L, L1).
bool_to_list(or(X, Y), L0, L1) :-
    (   bool_to_list(X, L0, L1)
    ;   bool_to_list(Y, L0, L1)
    ).
