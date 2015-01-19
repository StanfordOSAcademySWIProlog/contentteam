% The purpose of backend.pl is to allow front-end to:
% 1. Access the database. ex. get a list of courses
% 2.  
% 3.

:- module(backend,[data/1]).

:- use_module(db,[course/4]).

% Return a list of course_frontend
% Courses = [course_frontend('CSE','3','it is a class','4'), , ,]
data(Courses) :-
    findall(course_frontend(M, N, T, U), course(M, N, T, U), Courses).


  

