:- module(parse_prereqs, [parse_a_class_prereqs/2, parse_multi_class_prereqs/2]).
:- license(lgpl).

:- use_module(get_all_links, [get_all_prereq_links/3, get_a_prereq_link/3]).

:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).


%%	Obtains all prerequisites for a specific course.
% CourseID is expected to be a courseID.
% Prereqs is [ ['CSE 11', 'or', 'CSE 8B'], ['CSE XXX']].
parse_a_class_prereqs(CourseID, [CourseID|Prereqs]) :-
    once(get_a_prereq_link("WI15", CourseID, [URL|_])),
    scrape(URL, Prereqs).

%%	Obtains all prerequisites for a list of courses.
% CourseIDs is a list of courses that contains the names of the courses
% as atoms.
% Prereqs is a list of lists of courseID and list of prerequisites.
% i.e. [ [ 'CSE 12', [ ['CSE 11', 'or', 'CSE 8B'], [ 'CSE XXX']]], ..].
parse_multi_class_prereqs(CourseIDs, Prereqs) :-
    parse_list(CourseIDs, Prereqs).

scrape(URL, Data) :-
    setup_call_cleanup(
	http_open(URL, Stream, [cert_verify_hook(cert_verify)]),
        scrape_stream(Stream, Data),
        close(Stream)).

cert_verify(SSL, ProblemCert, AllCerts, FirstCert, Error) :-
    debug(shopping(cert),
      'Accepting certificate SSL:~w~n ProblemCert:~w~n AllCerts:~w~n FirstCert:~w~n Error:~w~n',
      [SSL, ProblemCert, AllCerts, FirstCert, Error]).

scrape_stream(In, Data) :-
    load_html(In, DOM, [syntax_errors(quiet)]),
    prereqs(DOM, Data),
    true.

parse_list([],[]).
parse_list([Course | T], [ListOfPrereqs|ParsePrereqs]) :-
    parse_a_class_prereqs(Course, ListOfPrereqs),
    parse_list(T, ParsePrereqs).

prereqs(DOM, Prereqs) :-
    % Get all the tables.
    findall(Prereq, xpath(DOM, //table, Prereq), [_|Ttable]),
    % Ignore the first table, and extract all <tr>.
    findall(Table, xpath(Ttable, //tr, Table), [_|Trows]),
    findall(Rows, prereqs_tds(Trows, Rows), Prereqs),
    true.

prereqs_tds([], []).
prereqs_tds([TR|Tail], [RelSpans|Spans]) :-
    findall(T, xpath(TR, //td, T), TD),
    [_|RelevantTD] = TD,
    findall(Span, xpath(RelevantTD, //span(normalize_space), Span), RelSpans),
    prereqs_tds(Tail, Spans).


