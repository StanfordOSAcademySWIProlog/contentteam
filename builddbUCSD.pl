:- license(lgpl).
:- set_prolog_flag(verbose, silent).
:- initialization(main).

:- use_module(scrape, [builddb/2]).

main :-
    % Build the majors database first.
    builddb('http://blink.ucsd.edu/instructors/courses/schedule-of-classes/subject-codes.html', 'db.pl'),
    halt.
main :-
    halt(1).
