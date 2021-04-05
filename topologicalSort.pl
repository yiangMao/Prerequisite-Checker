% generateGraph(CourseList, GraphEdges) takes a course list such as: 
%    [
%        course(cpsc200,2020,sum,3,[req(pre,cpsc100)]).
%        course(cpsc210,2020,fall,3,[req(alt,(cpsc100,math100))]).
%        course(math110,2020,fall,3,[req(co,math100)]).
%    ]
%
% and returns true if GraphEdges is a list representing the edges a-b
% where a is a pre-requisite course to b.
%
% For the above course list, GraphEdges will be:
%
%    [cs-100,cs300, cs200-cs300, cs100-cs200, ma12-cs100]
%
generateGraph([], []).
generateGraph(CourseList, Edges) :-
	maplist(flattenAltreqs, CourseList, UpdatedCourseList),
	maplist(pair, UpdatedCourseList, GraphRepresentation),
	flatten(GraphRepresentation, PreEdges),
	maplist(extractEdgeRepresentation, PreEdges, Edges).

extractEdgeRepresentation([C1,C2], Edge) :-
	write([C1, C2]),
	atom_concat(C1, -, Part1),
	atom_concat(Part1, C2, PreEdge),
	term_to_atom(Edge, PreEdge).

% pair(course(Code,Y, T,C,Prereqs), Pairs) returns true if Pairs is a list of 
% prerequisite pairs of the course passed in. 
% Example:
%   If the course is: course(cs300, 2021, "summer", 3, [req(pre,cs100), req(pre,cs200)])
%   this function will return Pairs as [[cs100, cs300], [cs200, cs300]].
pair(course(Code, _, _, _, Prereqs), Pairs) :-
	prereqsList(Prereqs, ListOfCourseCodes),
	pairHelper([Code], ListOfCourseCodes, Pairs).
pairHelper(L1, L2, Pairs) :-
	findall([B,A], (member(A, L1), member(B, L2)), Pairs).

% flatten flattens a 2D list to make it one dimensional
flatten([], []).
flatten([A|B],L) :- is_list(A), flatten(B,B1), !, append(A,B1,L).
flatten([A|B],[A|B1]) :- flatten(B,B1).

% append appends 2 lists together
append( [], X, X).
append( [X | Y], Z, [X | W]) :- append( Y, Z, W).

% Given a list of prereqs represented as [req(pre, cs100), req(pre, cs200)]
% prerqsList returns true if ListOfCourseCodes is [cs100, cs200]
prereqsList(Prereqs, ListOfCourseCodes) :-
	maplist(course_code, Prereqs, ListOfCourseCodes).
course_code(req(_,Code), Code).

% This function converts a prerequisite list containin alternate prereqs like:
%   [req(alt, (cs100, cs200))]
% into a a list like:
%   [req(alt, cs100), req(alt, cs200)]
flattenAltreqs(course(Code,Y,T,C,List), course(Code,Y,T,C,SingleDimensionArray)) :-
	maplist(flattenAltreqsHelper, List, Result),
	flatten(Result, SingleDimensionArray).
% if the course doesn't contain any alternate requisites just return it as is
flattenAltreqsHelper(req(pre, C1), req(pre, C1)).
flattenAltreqsHelper(req(co, C1), req(co, C1)).
% if the courses contain an alternate requisite, update the structure to prepare for sort
flattenAltreqsHelper(req(alt, (C1, C2)), [req(alt, C1),req(alt, C2)]).

:- use_module(library(ugraphs)).

topoSort(Edges, Order) :-
	vertices_edges_to_ugraph([], Edges, G),
    top_sort(G, Order).