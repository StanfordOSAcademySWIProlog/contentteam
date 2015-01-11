:- module(content, [grab_content/2]).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */
:- use_module(library(http/http_client)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(xpath)).

%%	run_online_query(+Contents:html)// is det
%
%%
url_scapper(URL, Result) :-
	http_get(URL, DomReply, []),
	xpath(DomReply, //p(@class='course-name'), Result),
	true.

grab_content(Query, Result) :-
	Query = "hello", Result="world",
	format("hello world~n").



