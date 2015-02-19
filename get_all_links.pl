:- module(get_all_links, [get_prereq_links/2]).
:- license(lgpl).

:- use_module(backend,[course_ids/1, courses/1]).

% https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesPreReq.htm?termCode=WI15&courseId=CSE100
link_part_1("https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesPreReq.htm?termCode=").
link_part_2("&courseId=").

%% This predicate will build and return a list of all
% links that can be used to get the prerequisites from the 
% UCSD schedule page.
get_prereq_links(Qrt,AllLinks) :- 
	link_part_1(PartOne),
	link_part_2(PartTwo),
	string_concat(PartOne, Qrt, L),
	string_concat(L, PartTwo, Link),
	course_ids(CourseIds),
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



