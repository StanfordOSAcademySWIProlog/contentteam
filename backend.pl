:- module(backend,[course_ids/1, courses/1, courses_taken/1, tech_tree/1]).
:- license(lgpl).

% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses
% 2. Receive information from users and store these in tables

%:- use_module(handcodedDB, [major/2,course/5,requirement/3]).
:- use_module(db).
:- use_module(tech_tree, [prereq_tree/1]).

%% Return a list of courses
% Courses = [course(ID, Title, Units, Descr, Reqs), course(...), ...]
courses(Courses) :-
    findall(course(ID, Title, Units, Descr, Reqs),
        course(ID, Title, Units, Descr, Reqs),
        Courses).

%% Return a list of course IDs
% IDs = [ID1, ID2....]
course_ids(IDs) :-
    findall(ID,
        course(ID, _Title, _Units, _Descr, _Reqs),
        IDs).


courses_taken(Taken) :-
	debug(ucsd(backend), 'These courses were taken, over to you all ~q', [Taken]).

% This gives the ugraph, in list of lists format
tech_tree(T) :-
        prereq_tree(T).





