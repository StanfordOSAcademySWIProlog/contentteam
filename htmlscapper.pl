:- module(content, [scraper/4, write_to_file/3]).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

% 'http://ucsd.edu/catalog/courses/CSE.html'
write_to_file(Courses, CourseDescriptions, File) :-
	setup_call_cleanup(open(File, write, Out),
	    forall(member(Course, Courses), format(Out, "course(~q).~n", Course)),
	    close(Out));
	setup_call_cleanup(open(File, append, Out),
	     forall(member(Description, CourseDescriptions), format(Out, "description(~q).~n", Description)),
	     close(Out)),
	true.

%%	url_scrapper().
%
%
scraper(URL, File, X, Y) :-
	http_open(URL, In, []),
    	load_html(In, DOM, [syntax_errors(quiet)]),
    	close(In),
	get_course_names(DOM, CourseNames),
	get_course_description(DOM, CourseDescriptions),
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

course_name(name(M, N, T, U)) -->
    % nonblanks followed by a single white space is the "CSE"
    nonblanks(M_codes), white,
    % everything up to a dot, and the dot, and a white space is the number
    string_without(`.`, N_codes), `.`, white,
    % Everything up to a space...
    string(T_codes), white,
    % ... followed by something enclosed in "(" and ")", and at the
    % very end of the whole list
    `(`, string(U_codes), `)`,
    % convert everything to atoms
    {   atom_codes(M, M_codes),
        atom_codes(N, N_codes),
        atom_codes(T, T_codes),
        atom_codes(U, U_codes)
    }.

