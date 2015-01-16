:- module(content, [url_scrapper/4, write_to_file/3]).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */
:- use_module(library(http/http_client)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(xpath)).

write_to_file(Courses, CourseDescriptions, File) :-
	setup_call_cleanup(open(File, write, Out),
	    forall(member(Course, Courses), format(Out, "course(~q)~n", Course)),
	    close(Out)),
	setup_call_cleanup(_,
	     forall(member(Description, CourseDescriptions), format(Out, "description(~q)~n", Description)),
	     close(Out)).

%%	url_scrapper().
%
%
url_scrapper(URL, File, X, Y) :-
	http_get(URL, DomReply, []),
	get_course_names(DomReply, CourseNames),
	get_course_description(DomReply, CourseDescriptions),
	write_to_file(CourseNames, CourseDescriptions, File),
	X = CourseNames, Y = CourseDescriptions,
	true.

get_course_description(DomReply, CourseDescriptions) :-
	findall(Description, xpath(DomReply, //p(@class='course-descriptions'), Description), HDescription),
	findall(Description, course_description(HDescription, Description), CourseDescriptions),
	true.

course_description(HtmlDescription, Description) :-
	select(element(_,_,[Description]),HtmlDescription,_).

get_course_names(DomReply, CourseNames) :-
	findall(Course, xpath(DomReply, //p(@class='course-name'), Course), HCourse),
	findall(Course, course_name(HCourse, Course), CourseNames),
	true.

course_name(HtmlCourse, Course) :-
	select(element(_,_,[Course]), HtmlCourse, _).
