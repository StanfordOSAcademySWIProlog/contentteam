/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(graphml_ugraph,
	  [ graphml_write_ugraph/4,	% +Out, +Map, +Keys, +UGraph
	    ugraph_xml_dom/4		% :Map, +Keys, +UGraph, -DOM
	  ]).
:- use_module(library(ugraphs)).
:- use_module(library(sgml_write)).
:- use_module(library(assoc)).
:- use_module(library(error)).
:- use_module(library(lists)).

:- meta_predicate
	graphml_write_ugraph(+,3,+,+),
	ugraph_xml_dom(3,+,+,-).


/** <module> Convert Prolog ugraph into GraphML
*/

%%	graphml_write_ugraph(+Out, +Map, +Keys, +UGraph) is det.


graphml_write_ugraph(Out, Map, Keys, UGraph) :-
	ugraph_xml_dom(Map, Keys, UGraph, DOM),
	Options = [],
	(   is_stream(Out)
	->  xml_write(Out, DOM, Options)
	;   setup_call_cleanup(
		open(Out, write, Stream, [encoding(utf8)]),
		xml_write(Stream, DOM, Options),
		close(Stream))
	).


%%	ugraph_xml_dom(:Map, +Keys, +UGraph, -DOM) is det.
%
%	Convert a ugraph into an GraphML DOM.
%
%	@param	Map is a called as call(:Map, +KeyName, +Obj, -KeyValue)
%		and must be semidet.  For nodes, Obj is a term node(Node)
%		for edges, it is a term edge(From, To). The reserved key
%		=id= is used to query an identifier for nodes and edges.
%		If this fails, the nodes and edges are numbered n<N> and
%		e<N>.
%	@param	Is a list key(For, KeyName, KeyType).
%	@param	Ugraph is a ugraph as defined in library(ugraph)
%	@param	DOM is a Prolog XML DOM that can be handed to
%		xml_write/3 to create a GraphML document.

ugraph_xml_dom(Map, Keys, Ugraph,
	       element(graphml,
		       [ xmlns='http://graphml.graphdrawing.org/xmlns',
			 'http://www.w3.org/2001/XMLSchema-instance':
			 schemaLocation =
			 'http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd'
		       ],
		       DOM)) :-
	key_dom(Keys, KeyDOM, NodeKeys, EdgeKeys),
	append(KeyDOM, [ element(graph,
				 [ id='G', edgedefault=directed
				 ],
				 NodeDOM)
		       ],
	       DOM),
	vertices(Ugraph, Vertices),
	node_dom(Vertices, Map, NodeKeys, NodeMap, NodeDOM, EdgeDom),
	edges(Ugraph, Edges),
	edge_dom(Edges, Map, EdgeKeys, NodeMap, EdgeDom).

%%	key_dom(+Keys, -KeyDom, -NodeKeys, -EdgeKeys) is det.
%
%	Generate the DOM for the key declarations and split the set into
%	two: NodeKeys and EdgeKeys.
%
%	Note that the keys are named =k0=,  =k1=, ... because Gephi maps
%	=d3= to the label, no matter what you do (Gephi 0.8.2).
%
%	@param	NodeKeys and EdgeKeys are lists with terms
%		key(KeyName, KeyType, Id)

key_dom(Keys, KeyDOM, NodeKeys, EdgeKeys) :-
	key_dom(Keys, KeyDOM, 0, NodeKeys, EdgeKeys).

key_dom([], [], _, [], []).
key_dom([key(For, KeyName, KeyType)|Keys],
	[element(key,
		 [id=Id, for=For, 'attr.name'=KeyName, 'attr.type'=KeyType],
		 [])|DOM],
	N0, NodeKeys, EdgeKeys) :-
	must_be(oneof([boolean, int, long, float, double, string]), KeyType),
	succ(N0, N),
	atomic_list_concat([k, N0], Id),
	KeyTerm = key(KeyName, KeyType, Id),
	(   For == node
	->  NodeKeys = [KeyTerm|NodeKeysT],
	    key_dom(Keys, DOM, N, NodeKeysT, EdgeKeys)
	;   For == edge
	->  EdgeKeys = [KeyTerm|EdgeKeysT],
	    key_dom(Keys, DOM, N, NodeKeys, EdgeKeysT)
	;   must_be(oneof([node,edge]), For)
	).

%%	node_dom(+Vertices, :Map, +NodeKeys, -NodeMap, NodeDOM, -Tail)

node_dom(Vertices, Map, NodeKeys, NodeMap, NodeDOM, Tail) :-
	empty_assoc(NodeMap0),
	node_dom(Vertices, Map, NodeKeys, 0, NodeMap0, NodeMap, NodeDOM, Tail).

node_dom([], _, _, _, NodeMap, NodeMap, DOM, DOM).
node_dom([V0|VT], Map, NodeKeys, IdI, NodeMap0, NodeMap,
	 [element(node, [id=Id], Data)|DOM0], DOM) :-
	node_data(NodeKeys, Map, node(V0), Data),
	(   call(Map, id, node(V0), Id)
	->  IdI1 = IdI
	;   atomic_list_concat([n, IdI], Id),
	    succ(IdI, IdI1)
	),
	put_assoc(V0, NodeMap0, Id, NodeMap1),
	node_dom(VT, Map, NodeKeys, IdI1, NodeMap1, NodeMap, DOM0, DOM).

node_data([], _, _, []).
node_data([key(Name, _Type, Id)|Keys], Map, V, Data) :-
	(   call(Map, Name, V, Value)
	->  Data = [element(data, [key=Id], [Value])|DataT]
	;   Data = DataT
	),
	node_data(Keys, Map, V, DataT).

%%	edge_dom(+Edges, :Map, +EdgeKeys, +NodeMap, -EdgeDom)

edge_dom(Edges, Map, EdgeKeys, NodeMap, EdgeDom) :-
	edge_dom(Edges, Map, 0, EdgeKeys, NodeMap, EdgeDom).

edge_dom([], _, _, _, _, []).
edge_dom([F-T|Edges], Map, IdI, EdgeKeys, NodeMap,
	 [ element(edge, [id=Id, source=FID, target=TID], Data)
	 | EdgeDom
	 ]) :-
	(   get_assoc(F, NodeMap, FID)
	->  true
	;   print_message(error, graphml(no_node(F))),
	    FID = 'ERROR'
	),
	(   get_assoc(T, NodeMap, TID)
	->  true
	;   print_message(error, graphml(no_node(T))),
	    TID = 'ERROR'
	),
	(   call(Map, id, edge(F,T), Id)
	->  IdI1 = IdI
	;   atomic_list_concat([e, IdI], Id),
	    succ(IdI, IdI1)
	),
	node_data(EdgeKeys, Map, edge(F,T), Data),
	edge_dom(Edges, Map, IdI1, EdgeKeys, NodeMap, EdgeDom).


:- multifile
	prolog:message//1.

prolog:message(graphml(no_node(Node))) -->
	[ 'No ID for node ~q'-[Node] ].
