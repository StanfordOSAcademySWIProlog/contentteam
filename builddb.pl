:- license(lgpl).
:- set_prolog_flag(verbose, silent).
:- initialization(main).

:- use_module(scrape, [builddb/2]).

main :-
    builddb([file('ucsd-cse-courses.html'), file('subject_codes.html')], 'db.pl'),
    halt.
main :-
    halt(1).
