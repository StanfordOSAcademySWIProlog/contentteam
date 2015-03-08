:- module(getPrereq, [getprereq_list/3,getprereq_fact/3,build/2]).
:- license(lgpl).

:- use_module(list_to_bool, [list_to_bool/2,course_to_id/2]).
:- use_module(parseprereq, [parse_a_class_prereqs/3, parse_multi_class_prereqs/3]).
:- use_module(prereq_proc, [requirement_to_list/2, requirement_to_string/3]).
:- use_module(db,[course/5]).

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


% same as getprereq_fact, it outputs all possible combinaiton of
% prerequirest list.
getprereq_list(CourseID,Qrt,Result):-
	getprereq_fact(CourseID,Qrt,Req),
	requirement_to_list(Req,Result).

%build db file for requirements, this database require
%db.pl(course/5 database) builded first.
%
%sample usage:
%buildprereq('WI14','db2.pl')
%****take around 30sec to run, with some warning massage***
%
%all requirement based on db.pl will apear in db2.pl file
build(Qrt,DB_file):-
    Write_term_opts = [fullstop, nl(true), quoted(true)],
    setup_call_cleanup(open(DB_file, write, Out),
	(   atom_concat('%Database based on Quarter: ',Qrt, Msg),
	    write_term(Out, Msg, Write_term_opts),
	    write_term(Out,:- module(db, [requirement/3]), Write_term_opts),
	    forall(all_courses(Qrt,Req),
	    write_term(Out, Req, Write_term_opts))
	),
	close(Out)).
%read all courses from course/5 in db.pl database
all_courses(Qrt,Req):-
	course(CourseID,_,_,_,_),
	getprereq_fact(CourseID, Qrt, Req).







