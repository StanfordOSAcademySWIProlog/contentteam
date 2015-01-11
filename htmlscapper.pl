:- module(content, [grab_content/2]).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */

:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).


% Load a static webpage for the time being.
run_query(Page) :- load_structure('UCSDCSECourses.html',Page,[]).

%%	run_online_query(+Contents:html)// is det
%
%%
run_online_query(DOM) :-
	http_open('http://ucsd.edu/catalog/courses/CSE.html', OnlineStream, []),
	load_html(OnlineStream, DOM, []),
	format("~w~n",[DOM]),%	xpath(OnlinePage, //p(@class='class-name'),Result),
	true.

grab_content(Query, Result) :-
	Query = "hello", Result="world",
	format("hello world~n").



