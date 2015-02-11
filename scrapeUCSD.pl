:- module(scrape, [builddb/2]).
:- license(lgpl).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

builddb(X,DB_file) :-
% Don't really get what I did wrong in order for this to run more than once...???
    once(todb(X, DB_file, "Majors")).

todb(Sources, DB_file, Flag) :-
    Flag == "Courses",
    scrape(Sources, Courses, Flag),
    Write_term_opts = [fullstop, nl(true), quoted(true)],
    setup_call_cleanup(open(DB_file, append, Out),
	(   forall(member(C, Courses),
		write_term(Out, C, Write_term_opts))
	),
	close(Out)).

todb(Source, DB_file, Flag) :-
    Flag == "Majors",
    scrape(Source, Majors, Flag),
    Write_term_opts = [fullstop, nl(true), quoted(true)],
    setup_call_cleanup(open(DB_file, write, Out),
        (   format(Out, ":- ", []),
            write_term(Out, module(db, [majors/2, course/5]), Write_term_opts),
            forall(member(M, Majors),
                write_term(Out, M, Write_term_opts))
        ),
        close(Out)).

scrape(file(File), Data, Flag) :-
    setup_call_cleanup(open(File, read, In, []),
        scrape_stream(In, Data, Flag),
        close(In)).

scrape(url(URL), Data, Flag) :-
    setup_call_cleanup(http_open(URL, In, []),
        scrape_stream(In, Data, Flag),
        close(In)).

scrape_stream(In, Data, Flag) :-
    Flag == "Majors",
    load_html(In, DOM, [syntax_errors(quiet)]),
    majors(DOM, Data).

scrape_stream(In, Data, _) :-
    load_html(In, DOM, [syntax_errors(quiet)]),
    courses(DOM, Data).

%% Collect all relevant <p>'s in a list and make a list of pairs out of it
courses(DOM, Courses) :-
    findall(CN_D, cn_d(DOM, CN_D), CNs_Ds),
    cds_to_terms(CNs_Ds, Courses).

majors(DOM, Majors) :-
    findall(C_S, c_s(DOM, C_S), Majors). % write(Majors), write("\n").

%% cn_d/2 is a non-deterministic predicate; it will have many solutions
% so you need to use findall/3 to evaluate it and collect them all in a
% list
cn_d(DOM, CN_D) :-
    xpath(DOM, //p, P),
    once(cn_d_1(P, CN_D)).

c_s(DOM, C_S) :-
    xpath(DOM, //tr, TR),
    once(cs_1(TR, C_S)).

cn_d_1(P, Name) :-
    cn_d_2(P, 'course-name', Codes),
    phrase(course_name(Name), Codes).

cn_d_1(P, description(Descr, Reqs)) :-
    cn_d_2(P, 'course-descriptions', Codes),
    phrase(course_descriptions(description(Descr, Reqs)), Codes).

cs_1(Elem, M) :-
    cs_2(Elem, Majors),
    phrase(subject_codes_desc(M), Majors). 

% The normalize_space argument is necessary to extract the text
% from the DOM.
cn_d_2(P, Class, Codes) :-
    xpath(P, /self(@class=Class, normalize_space), Text),
    atom_codes(Text, Codes).

cs_2(Elem, Majors) :-
    xpath(Elem, //td, TD_C),
    xpath(Elem, //td(last), TD_D),
    xpath(TD_C, /self(normalize_space), Codes_T),
    xpath(TD_D, /self(normalize_space), Descr_T),    
    atom_concat(Codes_T,".",R_C), atom_concat(R_C, Descr_T, Result),
    atom_codes(Result, Majors).

cds_to_terms([], []).
cds_to_terms([course(C,T,U)|Rest], Pairs) :-
    cds_to_terms_1(Rest, course(C,T,U), Pairs),
    !.
% if a description comes after another description, ignore it.
% yes, that happens at least once!
cds_to_terms([description(_,_)|Rest], Pairs) :-
    cds_to_terms(Rest, Pairs).
cds_to_terms_1([description(D,R)|Rest], course(C,T,U),
               [course(C,T,U,D,R)|Pairs]) :-
    cds_to_terms(Rest, Pairs).

:- use_module(library(dcg/basics)).

course_name(course(C, T, U)) -->
    string_without(`.`, C_codes), `.`, white,
    string(T_codes), white,
    `(`, units(U), `)`,
    {   atom_codes(C, C_codes),
        string_codes(T, T_codes)
    }.

:- use_module(library(lists)).

subject_codes_desc(major(C, D)) -->
    string_without(`.`, C_codes), `.`,
    string(D_codes),
    {   
	string_codes(C, C_codes),
	string_codes(D, D_codes)
    }.

% Because there are a few different ways units are written down
units(exactly(Unit)) -->
    integer(Unit).
units(from_to(From, To)) -->
    integer(From),
    [0'–],
    integer(To).
units(one_of([U1,U2])) -->
    integer(U1), white,
    `or`, white,
    integer(U2).
units(one_of([U|Us])) -->
    integer(U),
    `,`, white,
    more_units(Us).

more_units([U|Us]) -->
    integer(U),
    `,`, white,
    more_units(Us).
more_units([Last]) -->
    `or`, white,
    integer(Last).

course_descriptions(description(Descr, Reqs)) -->
    string(Descr_codes),
    prereqs(Reqs),
    {   string_codes(Descr, Descr_codes)
    }.

prereqs(Reqs) -->
    white,
    `Prerequisites:`, !, white,
    reqs(Reqs).
prereqs(none) --> [].

reqs(none) -->
    `none.`, !.
reqs(Reqs) -->
    string(Reqs_codes),
    {  atom_codes(Reqs, Reqs_codes)
    }.

