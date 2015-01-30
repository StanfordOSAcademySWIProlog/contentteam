:- module(backend,[courses/1]).
% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses

:- use_module(db, [course_title/2, course_units/2]).

%% Return a list of courses
% Courses = [course(ID, Title, [U1, ...]), course(...), ...]
courses(Courses) :-
    findall(course(ID, Title, Units),
        (   course_title(ID, Title),
            course_units(ID, Units)
        ),
        Courses).


  

