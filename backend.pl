:- module(backend,[data/1]).

:- use_module(db,[course/1]).

data(Courses) :-
    findall(C, course(C), Courses).


