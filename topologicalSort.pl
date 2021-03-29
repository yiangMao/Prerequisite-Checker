% generateGraph(CourseList, GraphEdges) takes a course list such as: 
%    [
%	     course(cs300, 2021, "summer", 3, [cs100, cs200]),
%		 course(cs200, 2020, "fall", 3, [cs100]),
%        course(cs100, 2020, "fall", 3, [ma12]).
%    ]
%
% and returns a list representing the edges in the directed graph.
% 
% For the above course list, this graph will be represented with 
% as following edge list:
%
%    [[cs100, cs300], [cs200, cs300], [cs100, cs200], [ma12, cs100]]
%
% where [a,b] means a is a pre-requisite of b.
generateGraph([], []).
generateGraph(CourseList, Answer) :-
	maplist(pair, CourseList, GraphRepresentation),
	flatten(GraphRepresentation, Answer).

% Helper functions for generateGraph %
pair(course(Code, _, _, _, Prereqs), Pairs) :-
	pairHelper([Code], Prereqs, Pairs).

pairHelper(L1, L2, Pairs) :-
	findall([B,A], (member(A, L1), member(B, L2)), Pairs).

flatten([], []).
flatten([A|B],L) :- is_list(A), flatten(B,B1), !, append(A,B1,L).
flatten([A|B],[A|B1]) :- flatten(B,B1).

append( [], X, X).
append( [X | Y], Z, [X | W]) :- append( Y, Z, W).
% End Helper functions for generateGraph

% Demo 
%     courseList(L), generateGraph(L, G), topoSort(G,Order).
%
courseList([
	course(cs300, 2021, "summer", 3, [cs100, cs200]),
	course(cs200, 2020, "fall", 3, [cs100]),
	course(cs100, 2020, "fall", 3, [ma12])
]).

% Topological sort algorithm
% Credit to: https://github.com/ejmudrak/topologicalSort/blob/master/prolog/graph.pl

% % % % % getS: % % % % % 
% 	Gets Set "S", the initial state of algorithm: 
%		all start nodes with no incoming edges

% % getS/2: % % 
% getS(+ListofLists, ?List).
%	Creates two lists: Starts and Ends, calls getS/4 using these lists
getS(Graph,Set) :-
	getAllStarts(Graph, Starts),
	getAllEnds(Graph, Ends),
	getS(Graph, Starts, Ends, Set).

% % % % getS/4: % % % % 
% getS(+ListofLists, +List, +List, ?List) 
% When the list of start nodes is empty, the answer is empty too
getS(_, [], _, []).

% If H is not a member of Ends, add it to the answer set and walk on 
getS(Graph, [H|T], Ends, Set) :-
	not(member(H, Ends)),
	getS(Graph, T, Ends, S1),
	setAdd(H, S1, Set).

% Continue walking along Set	
getS(Graph, [_|T], Ends, Set) :- getS(Graph, T, Ends, Set).

% % % % % topoSort: % % % % %
% Sorts the inputted graph in topological order

% % topoSort/2: % %  
% topoSort(+ListofLists, ?List)
topoSort([],[]) :- writef("The graph is empty!").

%	The graph has contents, so we get the initial Set of nodes and begin sorting
topoSort(Graph, Answer) :-
	getS(Graph, Set),
	topoSort(Graph, Set, Answer),!.	% Cuts to disable backtracking, which creates (incorrect) solutions that are only subset of Answer

% % % topoSort/3: % % % 
% topoSort(+ListofLists, +List, ?List)
% Empty Set:
topoSort(_, [], []).

% Walk through Set and add elements to it as the algorithm finds them
% 	We can get here from both topoSort/2 and topoSort/5
topoSort(Graph,	[H|T], Answer) :-
	topoSort(Graph, Graph, T, H, A1),
	setAdd(H, A1, Answer).

% % % % % topoSort/5 % % % % % 
% topoSort(+ListofLists, +ListofLists, +List, +Element, ?List)
% 	First parameter is static graph, second is current graph

% When you walk to end of the graph, call topoSort/3.
%	 This will give us a fresh graph and the next element of Set
topoSort(Graph, [], Set, _, Answer) :-
	topoSort(Graph, Set, Answer).

% Add elements to the Set by:
%	Comparing the node N to the graph's start nodes,
%	Adding N's end node M with no incoming edges into the Set 
topoSort(Graph, [Edge|Rest], Set, N, Answer) :-
	getStart(Edge, Start),
	equal(N, Start),
	getEnd(Edge, M),
	getAllEnds(Rest, Ends),
	not(member(M, Ends)),
	setAdd(M, Set, S1),
	reverse(S1, S2),	% keeps Set in the order that nodes were added
	topoSort(Graph, Rest, S2, N, Answer).	% Walk to next edge

% Walk to the next edge of the graph,
%	This executes when any of the above topoSort/5 predicates fail
topoSort(Graph, [_|Rest], Set, N, Answer) :-
	topoSort(Graph, Rest, Set, N, Answer).	

% % % % % Additional Relations % % % % % 

% % % Graph accessors: % % % 
%	Gets start or end nodes of a graph's edges, either the first or all nodes, to be used for components of the topological sort algoritm
getAllStarts([],[]).
getAllStarts([[Start,_]|Rest], [Start|Starts]) :-
	getAllStarts(Rest, Starts).

getAllEnds([],[]).
getAllEnds([[_,End]|Rest], [End|Ends]) :-
	getAllEnds(Rest, Ends).

getStart([], []).
getStart([First, _], First).

getEnd([], []).
getEnd([_, Last], Last).

% % % % % setAdd % % % % % 
% Add elements to the beginning of a List, 
%	while checking for duplicates to ensure that is a set
setAdd(X, Set, _) :- member(X, Set),!,fail.
setAdd(X, Set, [X|Set]).

equal(E1, E2) :- 
	E1 = E2.