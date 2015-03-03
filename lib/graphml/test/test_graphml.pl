:- module(test_graphml,
	  [ t1/0,
	    t2/0
	  ]).
:- use_module('../prolog/graphml_ugraph').

g(1, [1-[3,5], 2-[4], 3-[], 4-[5], 5-[]]).

t1 :-
	g(1, Graph),
	graphml_write_ugraph(current_output, nomap, [], Graph).

nomap(_, _, _) :-
	assertion(fail).

t2 :-
	g(1, Graph),
	graphml_write_ugraph(current_output, cmap,
			     [key(node, color, string)],
			     Graph).

cmap(color, node(Node), Color) :-
	(   Node mod 2 =:= 0
	->  Color = green
	;   Color = red
	).

