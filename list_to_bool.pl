:- module(list_to_bool, [list_to_bool/2]).
:- license(lgpl).


sample('1',[['CSE 12'],['CSE 11']]).
sample('2',[['CSE 12'],['CSE 11','or','CSE 8B']]).
sample('3',[['CSE 12'],['CSE 11','or','CSE 8B'],['CSE 15L']]).
sample('4',[['CSE 12'],['CSE 11','or','CSE 8B','or','CSE 190'],['CSE 15L']]).


list_to_bool(L,bool(B)).


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
course_to_val_id(C,val(ID)):-
	course_to_id(C,ID).


:- use_module(library(dcg/basics)).
course_phraser(id(D,N)) -->
	string(D_code),white,string(N_code),
	{ atom_codes(D,D_code),
	  atom_codes(N,N_code)
	}.






