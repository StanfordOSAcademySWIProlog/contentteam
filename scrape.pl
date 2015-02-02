:- module(scrape, [todb/2]).
:- license(lgpl).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

todb(Source, DB_file) :-
    scrape(Source, Courses),
    Write_term_opts = [fullstop, nl(true), quoted(true)],
    setup_call_cleanup(open(DB_file, write, Out),
        (   format(Out, ":- ", []),
            write_term(Out, module(db, [course/5]), Write_term_opts),
            forall(member(C, Courses),
                write_term(Out, C, Write_term_opts))
        ),
        close(Out)).

scrape(file(File), Data) :-
    setup_call_cleanup(open(File, read, In, []),
        scrape_stream(In, Data),
        close(In)).
scrape(url(URL), Data) :-
    setup_call_cleanup(http_open(URL, In, []),
        scrape_stream(In, Data),
        close(In)).

scrape_stream(In, Data) :-
    load_html(In, DOM, [syntax_errors(quiet)]),
    courses(DOM, Data).

%% Collect all relevant <p>'s in a list and make a list of pairs out of it
courses(DOM, Courses) :-
    findall(CN_D, cn_d(DOM, CN_D), CNs_Ds),
    cds_to_terms(CNs_Ds, Courses).

%% cn_d/2 is a non-deterministic predicate; it will have many solutions
% so you need to use findall/3 to evaluate it and collect them all in a
% list
cn_d(DOM, CN_D) :-
    xpath(DOM, //p, P),
    once(cn_d_1(P, CN_D)).

cn_d_1(P, Name) :-
    cn_d_2(P, 'course-name', Codes),
    phrase(course_name(Name), Codes).
cn_d_1(P, description(Descr, Reqs)) :-
    cn_d_2(P, 'course-descriptions', Codes),
    phrase(course_descriptions(description(Descr, Reqs)), Codes).

% The normalize_space argument is necessary to extract the text
% from the DOM.
cn_d_2(P, Class, Codes) :-
    xpath(P, /self(@class=Class, normalize_space), Text),
    atom_codes(Text, Codes).

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
