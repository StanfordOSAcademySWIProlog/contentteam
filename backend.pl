:- module(backend,[courses/1]).
% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses

:- use_module(db,
             [t_course_title/2,
              t_course_units/2,
              t_course_descr/2,
              t_course_reqs/2]).

%% Return a list of courses
% Courses = [course(ID, Title, Units, Descr, Reqs), course(...), ...]
courses(Courses) :-
    findall(course(ID, Title, Units, Descr, Reqs),
        course(ID, Title, Units, Descr, Reqs),
        Courses).

course(ID, Title, Units, Descr, Reqs) :-
    t_course_title(ID, Title),
    t_course_units(ID, Units),
    t_course_descr(ID, Descr),
    t_course_reqs(ID, Reqs).

