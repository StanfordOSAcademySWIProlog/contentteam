:- module(backend,[course_ids/1, courses/1]).
:- license(lgpl).

% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses
% 2. Receive information from users and store these in tables

:- use_module(handcodedDB, [major/2,course/5,requirement/3]).

%% Return a list of courses
% Courses = [course(ID, Title, Units, Descr, Reqs), course(...), ...]
courses(Courses) :-
    findall(course(ID, Title, Units, Descr, Reqs),
        course(ID, Title, Units, Descr, Reqs),
        IDs).

%% Return a list of course IDs
% IDs = [ID1, ID2....]
course_ids(IDs) :-
    findall(ID,
        course(ID, Title, Units, Descr, Reqs),
        IDs).
