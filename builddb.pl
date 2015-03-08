:- license(lgpl).
:- set_prolog_flag(verbose, silent).
:- initialization(main).

:- use_module(scrape, [builddb/2]).
:- use_module(getPrereq, [builddprereq/2]).

main :-
    builddb([file('ucsd-cse-courses.html'), file('subject_codes.html')], 'db.pl'),
    buildprereq(X,Y),
    halt.
main :-
    halt(1).
