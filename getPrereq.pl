:- module(getPrereq, [getprereq_list/3,getprereq_fact/3,buildprereq/2]).
:- license(lgpl).

:- use_module(list_to_bool, [list_to_bool/2,course_to_id/2]).
:- use_module(parseprereq, [parse_a_class_prereqs/3, parse_multi_class_prereqs/3]).
:- use_module(prereq_proc, [requirement_to_list/2, requirement_to_string/3]).


% getprereq_fact/3 take CourseID, and Term(Quarter), and result a
% requirement fac in format of:
% requirement(Department,Number,Boolean expression)
%
% example of usage: (CSE30 of Winter 2014)
%
% ?-getprereq_fact('CSE 30','WI14',X)
% X=requirement('CSE','30',bool(and(val(id('CSE','15L')),val(id('CSE','12')))));
%
getprereq_fact(CourseID,Qrt,requirement(Dept,N,Bool)):-
	course_to_id(CourseID,ID),
	ID=id(Dept,N),
	parse_a_class_prereqs(CourseID,Qrt,[_|[L]]),
	list_to_bool(L,Bool),!.




getprereq_list(CourseID,Qrt,Result):-
	getprereq_fact(CourseID,Qrt,Req),
	requirement_to_list(Req,Result).
