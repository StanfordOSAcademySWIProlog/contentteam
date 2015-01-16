:- module(content, [url_scrapper/2, write_to_file/2]).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */
:- use_module(library(http/http_client)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(xpath)).

write_to_file(Courses, File) :-
	setup_call_cleanup(open(File, write, Out),
	    forall(member(Course, Courses), format(Out, "course(~q)~n", Course)),
	    close(Out)).

%%	run_online_query(+Contents:html)// is det
%
%
url_scrapper(URL, Result) :-
	http_get(URL, DomReply, []),
	findall(Course, xpath(DomReply, //p(@class='course-name'), Course), HCourse),
	findall(Course, course_name(HCourse, Course),Result),
	true.

course_name(HtmlCourse, Course) :-
	select(element(_,_,[Course]), HtmlCourse, _).
