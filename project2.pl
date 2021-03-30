:- consult(topologicalSort).
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
course(math100,2020,fall,3,[req(pre,none)]).
course(cpsc200,2020,sum,3,[req(pre,cpsc100)]).
course(cpsc210,2020,fall,3,[req(alt,(cpsc100,math100))]).
course(math110,2020,fall,3,[req(co,math100)]).

% req(Rtype, Statement), a requirement has a requirement type(pre,alt,co) and the specific statement of that require.
% req(pre,CPSC110) means CPSC110 is a prereq.
% req(alt,(CPSC110,MATH100)) means CPSC110 and MATH100 alternative prereq.
% req(co,CPSC110) means CPSC110 is a corequisites.

% fit(C,S) is true if the given schedule S can fit all the requirements of the given course C.
fit(course(Code,Year,Term,Credit,[]),S).
fit(course(Code,Year,Term,Credit,[RH|RT]),S) :- checkhelper(Code,Year,Term,[RH|RT],S).
% test with:
% fit(course(math110,2020,fall,3,[req(co,math100)]),[course(math100,2020,fall,3,[])]).
% fit(course(cpsc210,2020,fall,3,[req(alt,(cpsc100,math100))]),[course(math100,2020,fall,3,[]),course(cpsc100,2020,winter,3,[])]).
% fit(course(cpsc200,2020,sum,3,[req(pre,cpsc100)]),[course(cpsc100,2020,winter,3,[])]).

checkhelper(_,_,_,[],_).
checkhelper(_,_,_,[req(pre,none)],_).
checkhelper(Code,Year,Term,[RH|RT],S) :-  check(Code,Year,Term,RH,S),checkhelper(Code,Year,Term,RT,S).

% check(C,R,S) is true if the given schedule S can fit the requirement R of the given course C.
% checks for prerequisite:
check(C1,Y1,T1,req(pre,Code),[course(Code,Y2,T2,_,_)|T]) :- before(Y2,T2,Y1,T1).
check(C1,Y1,T1,req(pre,Code),[course(Code1,_,_,_,_)|T]) :- dif(Code,Code1),check(C1,Y1,T1,req(pre,Code),T).

% checks for alt prerequisite:
check(C1,Y1,T1,req(alt,(Code1,Code2)),[course(Code,Y2,T2,_,_)|T]) :-
    or(check(C1,Y1,T1,req(pre,Code1),[course(Code,Y2,T2,_,_)|T]),check(C1,Y1,T1,req(pre,Code2),[course(Code,Y2,T2,_,_)|T])).
check(C1,Y1,T1,req(alt,(Code1,Code2)),[course(Code,_,_,_,_)|T]) :- dif(Code,Code1),dif(Code,Code2),check(C1,Y1,T1,req(alt,Code),T).

% checks for co-requisites
check(C1,Y1,T1,req(co,Code),[course(Code,Y2,T2,_,_)|T]) :- sbefore(Y2,T2,Y1,T1).
check(C1,Y1,T1,req(co,Code),[course(Code1,_,_,_,_)|T]) :- dif(Code,Code1),check(C1,Y1,T1,req(co,Code),T).

% schedule(Courses). This a list of all courses added in the schedule.
% updateschedule(S,Courses,Terms,NumT).
updateschedule(S,[],S).
/*
    Step 1: Generate a course list. Example:
       courseList([
          course(cpsc100,2020,winter,3,[req(pre,none)]),
          course(math100,2020,fall,3,[req(pre,none)]),
          course(cpsc200,2020,sum,3,[req(pre,cpsc100)]),
          % course(cpsc210,2020,fall,3,[req(alt,(cpsc100,math100))]),
          course(math110,2020,fall,3,[req(co,math100)])
       ]).

    Step 2: Generate a list of prerequisite pairs. 
    
      Call `courseList(L), generateGraph(L, Edges)`.
    
      Edges for the courseList above will be:

      [[cpsc100, cpsc200], [cpsc100, cpsc210], [math100, cpsc210], [math100, math110]]

    Step 3: Get the order that you have to take the courses in.

      Call `topoSort(Edges, Order)`.

      Your order will be the order you need to take the courses in:

      [math100, math110, cpsc100, ...]
*/
% updateschedule(S,[OH|OT],SF) :-topoSort([OH|OT],[CH|CT]),course(CH,Y,T,_,_),fit(course(CH,Y,T,_,_),S),insertschedule(S,CH,SN),updateschedule(SN,CT,SF).
% TODO: need to change the sort to the correct sort function.
updateschedule(S,Codes,SF) :-findcourselist(Codes,CL),generateGraph(CL,Edges),topoSort(Edges,Order),removeRedundancy(Order,S,[CH|CT]),getCourse(CH,CL,CourseH),fit(CourseH,S),insertschedule(S,CourseH,SN),updateschedule(SN,CT,SF).

insertschedule(S,C,[C|S]).

% findcourselist(Codes,Courses), is true if Courses is a list of courses in the KB that contains all the courses indicated by the course code list Codes.
findcourselist([],[]).
findcourselist([CodeH|CodeT],[course(CodeH,Y,Term,C,Req)|T]) :-course(CodeH,Y,Term,C,Req),findcourselist(CodeT,T).

% getCourse(Code,List,Course)
getCourse(Code,[course(Code,T,Y,C,Req)|_],course(Code,T,Y,C,Req)).
getCourse(Code,[course(Code1,T,Y,C,Req)|T],Course) :- dif(Code,Code1),getCourse(Code,T,Course).

% contains(Code,S) is true if schedule contains a course with given course code.
contains(Code,[course(Code,_,_,_,_)|_]).
contains(Code,[course(Code1,_,_,_,_)|T]) :- dif(Code,Code1), contains(Code,T).


/*
% removeNone(S,SN), is true if SN contains all the elements in list S in the same order except the none values.
removeNone([],[]).
removeNone([none|T],T).

% removecontained(Codes,S,NCodes) is true if NCodes contains the elements in Codes except the ones in schedule.
removecontained(Codes,[],Codes).
removecontained([],_,[]).
removecontained([H|T],S,NCodes) :- contains(H,S),removecontained(T,S,NCodes).
removecontained([CodeH|CodeT],S,[CodeH|NCodes]) :- not(contains(CodeH,S)),removecontained(CodeT,S,NCodes).

*/

% removeRedundancy(Codes,S,NCodes), is true if NCodes contains all the elements in Codes except the redundant ones(none, and ones contained in Schedule).
removeRedundancy([],_,[]).
removeRedundancy([none|CodeT],S,NCodes) :- removeRedundancy(CodeT,S,NCodes).
removeRedundancy([CodeH|CodeT],S,NCodes) :- contains(CodeH,S),removeRedundancy(CodeT,S,NCodes).
removeRedundancy([CodeH|CodeT],S,[CodeH|NCodes]) :- dif(CodeH,none),not(contains(CodeH,S)),removeRedundancy(CodeT,S,NCodes).





