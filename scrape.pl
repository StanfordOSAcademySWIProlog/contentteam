:- module(scrape, [todb/2]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

todb(Source, DB_file) :-
    scrape(Source, Courses),
    setup_call_cleanup(open(DB_file, write, Out),
        (   module_header_todb(Out),
            course_name_todb(Courses, Out),
            course_units_todb(Courses, Out),
            course_descrtext_todb(Courses, Out),
            course_reqs_todb(Courses, Out)
        ),
        close(Out)).

module_header_todb(Out) :-
    format(Out, ":- module(db, [course_title/2, course_units/2]).~n", []).

course_name_todb(Courses, Out) :-
    forall(member(course(C,Title,_)-_, Courses),
        format(Out, "course_title(~q, ~q).~n", [C, Title])).
    
course_units_todb(Courses, Out) :-
    forall(member(course(C,_,Units)-_, Courses),
        format(Out, "course_units(~q, ~q).~n", [C, Units])).

course_descrtext_todb(Courses, Out) :-
    forall(member(course(C,_,_)-description(Desc,_), Courses),
        format(Out, "course_description(~q, ~q).~n", [C, Desc])).

course_reqs_todb(Courses, Out) :-
    forall(member(course(C,_,_)-description(_,Reqs), Courses),
        format(Out, "course_reqs(~q, ~q).~n", [C, Reqs])).

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
    cds_to_pairs(CNs_Ds, Courses).

%% cn_d/2 is a non-deterministic predicate; it will have many solutions
% so you need to use findall/3 to evaluate it and collect them all in a
% list
cn_d(DOM, CN_D) :-
    xpath(DOM, //p, P),
    once(cn_d_1(P, CN_D)).

cn_d_1(P, Name) :-
    cn_d_2(P, 'course-name', Codes),
    phrase(course_name(Name), Codes).
cn_d_1(P, description(Text, Reqs)) :-
    cn_d_2(P, 'course-descriptions', Codes),
    phrase(course_descriptions(description(Text, Reqs)), Codes).
    % The prerequisites will be left in the _Rest_ to be parsed.

% The normalize_space argument is necessary to extract the text
% from the DOM.
cn_d_2(P, Class, Codes) :-
    xpath(P, /self(@class=Class, normalize_space), Text),
    atom_codes(Text, Codes).

cds_to_pairs([], []).
cds_to_pairs([course(C,T,U)|Rest], Pairs) :-
    cds_to_pairs_1(Rest, course(C,T,U), Pairs),
    !.
% if a description comes after another description, ignore it.
% yes, that happens at least once!
cds_to_pairs([description(_,_)|Rest], Pairs) :-
    cds_to_pairs(Rest, Pairs).
cds_to_pairs_1([description(D,R)|Rest], course(C,T,U),
               [course(C,T,U)-description(D,R)|Pairs]) :-
    cds_to_pairs(Rest, Pairs).

:- use_module(library(dcg/basics)).

course_name(course(C, T, U)) -->
    string_without(`.`, C_codes), `.`, white,
    string(T_codes), white,
    `(`, string(U_codes), `)`,
    {   atom_codes(C, C_codes),
        atom_codes(T, T_codes),
        atom_codes(U, U_codes)
    }.

course_descriptions(description(Desc, Reqs)) -->
    string(Desc_codes),
    `Prerequisites: `, !,
    string(Reqs_codes),
    {   atom_codes(Desc, Desc_codes),
        atom_codes(Reqs, Reqs_codes)
    }.
course_descriptions(description(Desc, none)) -->
    string(Desc).
