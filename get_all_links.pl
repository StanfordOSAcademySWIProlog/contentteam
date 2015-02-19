:- module(get_all_links, [get_prereq_links/3]).
:- license(lgpl).

link_part_1("https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesPreReq.htm?termCode=").
link_part_2("&courseId=").

%% API Usage:
% To get a list of pre-requisite URLs from a list of courses,
% execute the following:
%
% course_ids(CourseIDs), get_prereq_links("WI14", CourseIDs, AllLinks).
%
% course_ids/1 is a predicate defined in backend.pl
% get_prereq_links/3 predicate will build and return a list of all
% links that can be used to get the prerequisites from the 
% UCSD schedule page.
get_prereq_links(Qrt, CourseIds, AllLinks) :- 
	link_part_1(PartOne),
	link_part_2(PartTwo),
	string_concat(PartOne, Qrt, L),
	string_concat(L, PartTwo, Link),
	get_prereq_links_1(Link, CourseIds, AllLinks),
	true.

get_prereq_links_1(_, [], []).

get_prereq_links_1(Link, [H|T], [L|AllLinks]) :- 
	fix_course_id(H, CourseID),
	string_concat(Link, CourseID, L),
	get_prereq_links_1(Link, T, AllLinks),
	true.

fix_course_id(CourseID, ResultCourseID) :-
	atom_codes(CourseID, StringID),
	split_string(StringID, " ", "", [A,B|_]),
	string_concat(A,B,ResultCourseID),	
	true.	
