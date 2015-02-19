:- module(prereq_proc, [requirement_to_list/3, requirement_to_string/3]).
:- license(lgpl).

:- use_module(handcodedDB, [major/2, course/5, requirement/3]).
% Present them as a boolean expression.         
% - An operand (value) is the term `val(Op)`
% - An AND is the term `and(X, Y)`
% - An OR is the term `or(X, Y)`
% Examples:
% - (a AND b AND c):
%     and(val(a), and(val(b), val(c)))
%       OR
%     and(and(val(a), val(b)), val(c))
% - (a OR (b AND c)):
%     or(val(a), and(val(b), val(c)))
% - (a AND b AND (c OR d)) OR (f AND g):
%     or(and(val(a), and(val(b), or(val(c), val(d)))), and(val(f), val(g)))
%
% ... and so on.
%
% Convert the boolean expresssions to lists,1
% enumerating the different possibilities by backtracking
%

bool_to_list(none, Rest, Rest).
bool_to_list(val(X), [X|Rest], Rest).
bool_to_list(and(X, Y), L0, L1) :-
    bool_to_list(X, L0, L),
    bool_to_list(Y, L, L1).
bool_to_list(or(X, Y), L0, L1) :-
(   bool_to_list(X, L0, L1)
    ;   bool_to_list(Y, L0, L1)
).


% same as bool_to_list, but only work for requirment, for
% val(id(Dept,#)) list.
                          
boolprereqs_to_list(none, Rest, Rest).
boolprereqs_to_list(val(id(X,Y)), [C|Rest], Rest):-
    atomic_concat(X,' ',Z),
    atomic_concat(Z,Y,C).
boolprereqs_to_list(and(X, Y), L0, L1) :-
    boolprereqs_to_list(X, L0, L),
    boolprereqs_to_list(Y, L, L1).
boolprereqs_to_list(or(X, Y), L0, L1) :-
    (   boolprereqs_to_list(X, L0, L1)
    ;   boolprereqs_to_list(Y, L0, L1)                                                    
    ).


%output all possible way of complete prerequirest as a list 

requirement_to_list(Dept,ID,X):-
    requirement(Dept,ID,bool(B)),
    boolprereqs_to_list(B,X,[]).


%output requirement as a string format,

requirement_to_string(Dept,ID,Result):-
  requirement(Dept,ID,bool(B)),
  boolprereqs_to_list(B,L,[]),
  atomic_list_concat(L, '+', Prep),
  atom_concat(Dept,ID,Y),
  atom_concat(Y,": ",X),
  atom_concat(X,Prep,Atom),
  atom_string(Atom,Result).


