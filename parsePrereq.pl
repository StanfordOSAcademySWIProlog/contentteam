:- module(parse_prereqs, [parse_prereqs/2]).
:- license(lgpl).

:- use_module(get_all_links, [get_all_prereq_links/3, get_a_prereq_link/3]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

%%	Obtains all prerequisites for a specific course.
% CourseID is expected to be a courseID.
% Prereqs is [ ['CSE 11', 'or', 'CSE 8B'], ['CSE XXX']].
parse_prereqs(CourseID, Prereqs) :- true.


%%	Obtains all prerequisites for a list of courses.
% CourseIDs is a list of courses that contains the names of the courses
% as atoms.
% Prereqs is a list of lists of courseID and list of prerequisites.
% i.e. [ [ 'CSE 12', [ ['CSE 11', 'or', 'CSE 8B'], [ 'CSE XXX']]], ..].
parse_prereqs(CourseIDs, Prereqs) :-
	parse_list(CourseIDs, Prereqs),
	true.

parse_list([Course | T], Prereqs) :-
	parse_prereqs(Course, ListOfPrereqs),
	true.
