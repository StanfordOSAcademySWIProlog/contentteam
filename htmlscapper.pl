:- module(content, []).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */

:- use_module(library(http/http_open)).


% Load a static webpage for the time being.
run_query(Page) :- load_structure('UCSDCSECourses.html',Page,[]).

% Loads from URL.
run_online_query(OnlinePage) :-
	http_open('http://ucsd.edu/catalog/courses/CSE.html', OnlineStream, []),
	load_structure(OnlineStream, OnlinePage, []).
