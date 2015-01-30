:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

write_to_file(Courses, File) :-
    setup_call_cleanup(open(File, write, Out),
        forall(member(N-_, Courses), format(Out, "~q.~n", [N])),
        close(Out)).

scrape(file, File, Data) :-
    setup_call_cleanup(open(File, read, In, []),
        scrape_stream(In, Data),
        close(In)).
scrape(url, URL, Data) :-
    setup_call_cleanup(http_open(URL, In, []),
        scrape_stream(In, Data),
        close(In)).

scrape_stream(In, Data) :-
    load_html(In, DOM, [syntax_errors(quiet)]),
    courses(DOM, Data).

%% Collect all relevant <p>'s in a list and make a list of pairs out of it
courses(DOM, Courses) :-
    findall(CN_D, cn_d(DOM, CN_D), CNs_Ds),
    list_to_pairs(CNs_Ds, Courses).

%% cn_d/2 is a non-deterministic predicate; it will have many solutions
% so you need to use findall/3 to evaluate it and collect them all in a
% list
cn_d(DOM, CN_D) :-
    xpath(DOM, //p, P),
    once(cn_d_1(P, CN_D)).

cn_d_1(P, Name) :-
    cn_d_2(P, 'course-name', Codes),
    phrase(course_name(Name), Codes).
cn_d_1(P, description(Rest)) :-
    cn_d_2(P, 'course-descriptions', Codes),
    dif(Codes, []),
    phrase(course_descriptions(description(Rest)), Codes, Rest).
    % The prerequisites will be left in the _Rest_ to be parsed.

% The normalize_space argument is necessary to extract the text
% from the DOM.
cn_d_2(P, Class, Codes) :-
    xpath(P, /self(@class=Class, normalize_space), Text),
    atom_codes(Text, Codes).

list_to_pairs([], []).
list_to_pairs([A,B|Rest], [A-B|Pairs]) :-
    list_to_pairs(Rest, Pairs).

:- use_module(library(dcg/basics)).

course_name(course(C, T, U)) -->
    string_without(`.`, C_codes), `.`, white,
    string(T_codes), white,
    `(`, string(U_codes), `)`,
    {   
        atom_codes(C, C_codes),
        atom_codes(T, T_codes),
        atom_codes(U, U_codes)
    }.

course_descriptions(description(_)) --> [].

