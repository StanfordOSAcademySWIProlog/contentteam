:- module(backend,[courses/1]).
% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses

:- use_module(db,
             [course_title/2,
              course_units/2,
              course_descr/2,
              course_reqs/2]).

%% Return a list of courses
% Courses = [course(ID, Title, Units, Descr, Reqs), course(...), ...]
courses(Courses) :-
    findall(course(ID, Title, Units, Descr, Reqs),
        course(ID, Title, Units, Descr, Reqs),
        Courses).

course(ID, Title, Units, Descr, Reqs) :-
    course_title(ID, Title),
    course_units(ID, Units),
    course_descr(ID, Descr),
    course_reqs(ID, Reqs).

