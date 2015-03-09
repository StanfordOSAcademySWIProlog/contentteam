:- module(prereq_proc, [requirement_to_list/2, requirement_to_string/3]).
:- license(lgpl).


:- use_module(db2).

%output all possible way of complete prerequirest as a list
%it can take either course as first input and loop up database by itself
%or take a requirement and output it's possible combination
%
%example of usage:
%?- requirement_to_list(requirement('CSE', '12', bool(or(val(id('CSE', '11')), val(id('CSE', '8B'))))),X).
%X = ['CSE 11']
%X = ['CSE 8B']
%
%?- requirement_to_list('CSE 12',X).
%X = ['CSE 8B']
%X = ['CSE 11']
requirement_to_list(requirement(_,_,bool(B)),X, N):-
    !,boolprereqs_to_list(B,X,[]).

requirement_to_list(Course,X):-
    !,requirement(D,I, bool(B)),
        atom_concat(D, ' ', T),
        atom_concat(T, I, Course),
    boolprereqs_to_list(B,X,[]).




%Present them as a boolean expression.
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




%output requirement as a string format,

requirement_to_string(Dept,ID,Result):-
  requirement(Dept,ID,bool(B)),
  boolprereqs_to_list(B,L,[]),
  atomic_list_concat(L, '+', Prep),
  atom_concat(Dept,ID,Y),
  atom_concat(Y,": ",X),
  atom_concat(X,Prep,Atom),
  atom_string(Atom,Result).


