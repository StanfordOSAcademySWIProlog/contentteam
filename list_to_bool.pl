:- module(list_to_bool, [list_to_bool/2]).
:- license(lgpl).


sample('1',[['CSE 12'],['CSE 11']]).
sample('2',[['CSE 12'],['CSE 11','or','CSE 8B']]).
sample('3',[['CSE 12'],['CSE 11','or','CSE 8B'],['CSE 15L']]).
sample('4',[['CSE 12'],['CSE 11','or','CSE 8B','or','CSE 190'],['CSE 15L']]).


%list_to_bool(L,bool(B)).


%convert list['1','2','3'] into and(and('3','2'),'1')
%return empty atmo if list is empty
andlist_to_bool([],'').
andlist_to_bool([H|[]],H):-!.
andlist_to_bool([H|T],and(B,H)):-andlist_to_bool(T,B).

%orlist([H|T],or(B,H)).

