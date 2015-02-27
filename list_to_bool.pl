:- module(list_to_bool, [list_to_bool/2]).
:- license(lgpl).


sample('1',[['CSE 12'],['CSE 11']]).
sample('2',[['CSE 12'],['CSE 11','or','CSE 8B']]).
sample('3',[['CSE 12'],['CSE 11','or','CSE 8B'],['CSE 15L']]).
sample('4',[['CSE 12'],['CSE 11','or','CSE 8B','or','CSE 190'],['CSE 15L']]).

test(D,R):-sample(D,L),list_to_bool(L,R).




:- use_module(library(apply)).
%main function:
%convert a list of list of courses into boolean expression
%the inner list are in OR relation and outer are AND and relation
%inner list will contain 'or' between each courses
%exapmple:
%[['CSE 12'],['CSE 11']] ->  bool(and(val(id('CSE','11')),val(id('CSE','12'))))
%
%[['CSE 12'],['CSE 11','or','CSE 8B','or','CSE 190'],['CSE 15L']]->
% bool(and(and(val(id('CSE', '15L')), or(or(val(id('CSE', '190')),
% val(id('CSE', '8B'))), val(id('CSE', '11')))), val(id('CSE', '12'))))
%
%
%boolean expression can be read by:
% use_module(prereq_proc,[requirement_to_list/3,requirement_to_string/3]).
list_to_bool(L,bool(B)):-
	list_to_valId_list(L,VL),
        maplist(orlist_to_bool,VL,OrList),
	andlist_to_bool(OrList,B).



%convert a list of list of courses into val(id(D,N)) list list.
list_to_valId_list([],[]).
list_to_valId_list([H|T],[H1|T1]):-
	maplist(course_to_valId,H,H1),
	list_to_valId_list(T,T1).


%convert list['1','2','3'] into and(and('3','2'),'1')
%return empty atom if list is empty
andlist_to_bool([],'').
andlist_to_bool([H|[]],H):-!.
andlist_to_bool([H|T],and(B,H)):-andlist_to_bool(T,B).

%convert list ['1','or','2','or','3'] into or(or('3','2'),'1')
%it return false if format does not match above.
orlist_to_bool([],'').
orlist_to_bool([H|[]],H):-
	not(H='or'),!.
orlist_to_bool([H|T],or(B,H)):-
	not(H='or'),
	orlist_to_bool2(T,B).
orlist_to_bool2(['or'|[]],_):-!,false.
orlist_to_bool2(['or'|T],B):-
	orlist_to_bool(T,B).



atom_to_val(A,val(A)).


%convert atom Course into id('Dept','#')
%for example: C='CSE 12' -> ID= id('CSE','12)
course_to_id(C,ID):-
	atom_codes(C, Codes),
	phrase(course_phraser(ID), Codes).

% C='CSE 12' -> ID= val(id('CSE','12))
course_to_valId('or','or'):-!.
course_to_valId(C,val(ID)):-
	course_to_id(C,ID),!.


:- use_module(library(dcg/basics)).
course_phraser(id(D,N)) -->
	string(D_code),white,string(N_code),
	{ atom_codes(D,D_code),
	  atom_codes(N,N_code)
	}.






