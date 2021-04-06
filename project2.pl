:- consult(topologicalSort).
:- consult(canBeCompleted).

% or(A,B) is true if A is true or B is true or both A and B are true.
or(A,B) :- A,B.
or(A,_) :- A.
or(_,B) :- B.

% Term(Term), a term contains the name of the term.
% A year have three terms in total, fall(Sept-Dec), winter(Jan-Apr), sum(May-Aug).
term(fall).
term(winter).
term(sum).

% before(Y1,T1,Y2,T2) is true if the first study term is is before the second.
before(Y1,_,Y2,_) :- Y1<Y2.
before(Y,winter,Y,fall).
before(Y,winter,Y,sum).
before(Y,sum,Y,fall).
% sbefore(Y1,T1,Y2,T2) is true if the two state terms are the same term or the first study term is is before the second.
sbefore(Y,T,Y,T).
sbefore(Y1,T1,Y2,T2) :- before(Y1,T1,Y2,T2).

% course(Code,Year,Term,Credit,Req), a course contains a course code, the year of the course, the term of the course, credits of the course, and a list of requirements of this course.
% TODO: update empty prerequisite
course(cpsc100,2020,winter,3,[req(pre,none)]).
course(cpsc110,2020,winter,3,[req(pre,none)]).
course(cpsc110,2020,sum,3,[req(pre,none)]).
course(cpsc121,2020,sum,3,[req(co,cpsc110)]).
course(cpsc200,2020,sum,3,[req(pre,cpsc100),req(pre,cpsc110)]).
course(cpsc210,2021,sum,3,[req(pre,cpsc110),req(co,cpsc200)]).
course(cpsc221,2020,fall,3,[req(alt,(cpsc210,cpsc121))]).
course(math100,2020,fall,3,[req(pre,none)]).
course(math101,2020,fall,3,[req(alt,(math100,math110))]).
course(math110,2020,sum,3,[req(pre,none)]).
course(math200,2020,fall,3,[req(pre,math101)]).
course(math221,2021,winter,3,[req(alt,(math100,math101))]).

% req(Rtype, Statement), a requirement has a requirement type(pre,alt,co) and the specific statement of that require.
% req(pre,CPSC110) means CPSC110 is a prereq.
% req(alt,(CPSC110,MATH100)) means CPSC110 and MATH100 alternative prereq.
% req(co,CPSC110) means CPSC110 is a corequisites.

% fit(C,S) is true if the given schedule S can fit all the requirements of the given course C.
fit(course(_,_,_,_,[]),_).
fit(course(Code,Year,Term,_,[RH|RT]),S) :- checkhelper(Code,Year,Term,[RH|RT],S).
% test with:
% fit(course(math110,2020,fall,3,[req(co,math100)]),[course(math100,2020,fall,3,[])]).
% fit(course(cpsc210,2020,fall,3,[req(alt,(cpsc100,math100))]),[course(math100,2020,fall,3,[]),course(cpsc100,2020,winter,3,[])]).
% fit(course(cpsc200,2020,sum,3,[req(pre,cpsc100)]),[course(cpsc100,2020,winter,3,[])]).

checkhelper(_,_,_,[],_).
checkhelper(_,_,_,[req(pre,none)],_).
checkhelper(Code,Year,Term,[RH|RT],S) :-  check(Code,Year,Term,RH,S),checkhelper(Code,Year,Term,RT,S).

% check(C,R,S) is true if the given schedule S can fit the requirement R of the given course C.
% checks for prerequisite:
check(_,Y1,T1,req(pre,Code),[course(Code,Y2,T2,_,_)|_]) :- before(Y2,T2,Y1,T1).
check(C1,Y1,T1,req(pre,Code),[course(Code1,_,_,_,_)|T]) :- dif(Code,Code1),check(C1,Y1,T1,req(pre,Code),T).

% checks for alt prerequisite:
check(C1,Y1,T1,req(alt,(Code1,Code2)),[course(Code,Y2,T2,_,_)|T]) :-
    or(check(C1,Y1,T1,req(pre,Code1),[course(Code,Y2,T2,_,_)|T]),check(C1,Y1,T1,req(pre,Code2),[course(Code,Y2,T2,_,_)|T])).
check(C1,Y1,T1,req(alt,(Code1,Code2)),[course(Code,_,_,_,_)|T]) :- dif(Code,Code1),dif(Code,Code2),check(C1,Y1,T1,req(alt,Code),T).

% checks for co-requisites
check(_,Y1,T1,req(co,Code),[course(Code,Y2,T2,_,_)|_]) :- sbefore(Y2,T2,Y1,T1).
check(C1,Y1,T1,req(co,Code),[course(Code1,_,_,_,_)|T]) :- dif(Code,Code1),check(C1,Y1,T1,req(co,Code),T).

% schedule(Courses). This a list of all courses added in the schedule.
% updateschedule(S,Courses,Terms,NumT).
updateschedule(S,[],S).

updateschedule(S,Codes,SF) :-findcourselist(Codes,CL),generateGraph(CL,Edges),topoSort(Edges,Order),removeRedundancy2(Order,Codes,NOrder),removeRedundancy(NOrder,S,[CH|CT]),
                             getCourse(CH,CL,CourseH),fit(CourseH,S),insertschedule(S,CourseH,SN),
                             updateschedule(SN,CT,SF).



insertschedule(S,C,SN) :- append(S,[C],SN).

% findcourselist(Codes,Courses), is true if Courses is a list of courses in the KB that contains all the courses indicated by the course code list Codes.
findcourselist([],[]).
findcourselist([CodeH|CodeT],[course(CodeH,Y,Term,C,Req)|T]) :-course(CodeH,Y,Term,C,Req),findcourselist(CodeT,T).

% getCourse(Code,List,Course)
getCourse(Code,[course(Code,T,Y,C,Req)|_],course(Code,T,Y,C,Req)).
getCourse(Code,[course(Code1,_,_,_,_)|CT],Course) :- dif(Code,Code1),getCourse(Code,CT,Course).


% contains(Code,S) is true if schedule contains a course with given course code.
contains(Code,[course(Code,_,_,_,_)|_]).
contains(Code,[course(Code1,_,_,_,_)|T]) :- dif(Code,Code1), contains(Code,T).

% removeRedundancy(Codes,S,NCodes), is true if NCodes contains all the elements in Codes except the redundant ones(none, and ones contained in Schedule).
removeRedundancy([],_,[]).
removeRedundancy([none|CodeT],S,NCodes) :- removeRedundancy(CodeT,S,NCodes).
removeRedundancy([CodeH|CodeT],S,NCodes) :- contains(CodeH,S),removeRedundancy(CodeT,S,NCodes).
removeRedundancy([CodeH|CodeT],S,[CodeH|NCodes]) :- dif(CodeH,none),not(contains(CodeH,S)),removeRedundancy(CodeT,S,NCodes).

% removeRedundancy2(TSCodes,CodeL,NCodes), is true if NCodes contains all the elements in Codes except the redundant ones(none, and ones not in the code list Codes).
removeRedundancy2([],_,[]).
removeRedundancy2([none|CodeT],CodeL,NCodes) :- removeRedundancy2(CodeT,CodeL,NCodes).
removeRedundancy2([CodeH|CodeT],CodeL,[CodeH|NCodes]) :- contains2(CodeH,CodeL),removeRedundancy2(CodeT,CodeL,NCodes).
removeRedundancy2([CodeH|CodeT],CodeL,NCodes) :- dif(CodeH,none),not(contains2(CodeH,CodeL)),removeRedundancy2(CodeT,CodeL,NCodes).
% contains2(Code,CodeL) is true if Code is in the CodeL code list.
contains2(Code,[Code|_]).
contains2(Code,[Code1|T]) :- dif(Code,Code1), contains2(Code,T).

printlist([]).
printlist([X|List]) :- nl,write(X),nl,printlist(List).

/*
course(cpsc100,2020,winter,3,[req(pre,none)]).
course(cpsc110,2020,winter,3,[req(pre,none)]).
course(cpsc121,2020,sum,3,[req(co,cpsc110)]).
course(cpsc200,2021,sum,3,[req(pre,cpsc100),req(pre,cpsc110)]).
course(cpsc210,2021,sum,3,[req(pre,cpsc110),req(co,cpsc200)]).
course(cpsc221,2020,fall,3,[req(alt,(cpsc210,cpsc121))]).
course(cpsc300,2020,fall,3,[req(pre,cpsc200)).
course(cpsc321,2020,winter,3,[req(alt,(cpsc210,cpsc200))]).
course(math100,2020,fall,3,[req(pre,none)]).
course(math101,2020,fall,3,[req(alt,(math100,math110)]).
course(math110,2020,sum,3,[req(pre,none)]).
course(math200,2020,fall,3,[req(pre,math101)]).
course(math221,2021,winter,3,[req(alt,(math100,math101)]).

test:
case: course with no prerequisites.
updateschedule([],[cpsc100],NS),printlist(NS),canBeCompleted(NS,6).

case: course with prerequisite.
updateschedule([],[cpsc200,cpsc210,cpsc110,cpsc100],NS),printlist(NS),canBeCompleted(NS,6).

case: course with prerequisite and corequisites.
updateschedule([],[cpsc121,cpsc110],NS),printlist(NS),canBeCompleted(NS,6).

case: course with alternative prerequisite.
updateschedule([],[math101,math100,math110],NS),printlist(NS),canBeCompleted(NS,6).

updateschedule([],[math101,cpsc110,math110,cpsc200,cpsc100,math100,cpsc121,math221],NS),printlist(NS),canBeCompleted(NS,9).

case: course with no valid prerequisites.
updateschedule([],[cpsc200,cpsc300],NS),printlist(NS),canBeCompleted(NS,6).

case: course with none of its alternative prerequisites.
updateschedule([],[cpsc300,cpsc210,cpsc200],NS),printlist(NS),canBeCompleted(NS,6).

Wrong sort cases (FIXED!)
compare these two:
findcourselist([cpsc221,cpsc210],CL),generateGraph(CL,Edges),topoSort(Edges,Order).
findcourselist([cpsc210,cpsc221],CL),generateGraph(CL,Edges),topoSort(Edges,Order).

compare these two:
findcourselist([math101,math100],CL),generateGraph(CL,Edges),topoSort(Edges,Order).
findcourselist([math100,math101],CL),generateGraph(CL,Edges),topoSort(Edges,Order).

compare these two:
findcourselist([math221,math100],CL),generateGraph(CL,Edges),topoSort(Edges,Order).
findcourselist([math100,math221],CL),generateGraph(CL,Edges),topoSort(Edges,Order).
*/




