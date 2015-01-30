:- set_prolog_flag(verbose, silent).
:- initialization(main).

:- use_module(scrape, [todb/2]).

main :-
    todb(file('ucsd-cse-courses.html'), 'db.pl'),
    halt.
main :-
    halt(1).
