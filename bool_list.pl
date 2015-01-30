% One way to deal with ORs in the prerequisites would be to "hard-code"
% them into the table of prerequisites by simply doing it like this:
%
% "Course a has prerequisites b and c, or b and d":
% course_prereqs(a, [b, c]).
% course_prereqs(a, [b, d]).
%
% Another idea is to represent prerequisites as a "boolean" expression
% - A course itself is the term c(Course)
% - If both courses are required, it is the term and(X, Y)
% - If either one or the other, the term or(X, Y).
% Examples:
% - (a AND b AND c):
%     and(c(a), and(c(b), c(c)))
%       OR
%     and(and(c(a), c(b)), c(c))
% - (a OR (b AND c)):
%     or(c(a), and(c(b), c(c)))
% - (a AND b AND (c OR d)) OR (f AND g):
%     or(and(c(a), and(c(b), or(c(c), c(d)))), and(c(f), c(g)))
% 
% ... and so on.
%
% One possibility way to convert the boolean expresssions to lists,
% enumerating the lists by backtracking in the case of ORs would be:
%
bool_list(none, Rest, Rest).
bool_list(c(C), [C|Rest], Rest).
bool_list(and(X, Y), L0, L) :-
    bool_list(X, L0, L1),
    bool_list(Y, L1, L).
bool_list(or(X, Y), L0, L) :-
    (   bool_list(X, L0, L)
    ;   bool_list(Y, L0, L)
    ).
% PLEASE ask if you are not sure what is going on in this code!

% Here are the examples from above from the top level:
%
% ?- bool_list(and(c(a), and(c(b), c(c))), L, []).
% L = [a, b, c].
% 
% ?- bool_list(and(and(c(a), c(b)), c(c)), L, []).
% L = [a, b, c].
% 
% ?- bool_list(or(c(a), and(c(b), c(c))), L, []).
% L = [a] ;
% L = [b, c].
% 
% ?- bool_list(or(and(c(a), and(c(b), or(c(c), c(d)))), and(c(f), c(g))), L, []).
% L = [a, b, c] ;
% L = [a, b, d] ;
% L = [f, g].
%
% Because this is a "difference list" predicate, you can use it to
% directly enumerate the list of requirements for more than one course:
%
% Example database
course_prereqs(c101, none).
course_prereqs(c102, none).
course_prereqs(c103, c(c101)).
course_prereqs(c201, and(c(c101), c(c103))).
course_prereqs(c202, or(c(c201), and(c(c101), c(c102)))).

courses_prereqslist([], []).
courses_prereqslist([C|Cs], Prereqs) :-
    courses_prereqslist_1([C|Cs], All, []),
    sort(All, Prereqs).

courses_prereqslist_1([], L, L).
courses_prereqslist_1([C|Cs], L0, L) :-
    course_prereqs(C, Prereqs),
    bool_list(Prereqs, L0, L1),
    courses_prereqslist_1(Cs, L1, L).

% ?- courses_prereqslist([c201, c202], L).
% L = [c101, c103, c201] ;
% L = [c101, c102, c103].
%
