:- module(backend,[data/1]).

:- use_module(db,[course/4]).

data(Courses) :-
    findall(major_nr_title_units(M, N, T, U), course(M, N, T, U), Courses).


  

