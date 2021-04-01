% can be completed(S, Number) returns true if a schedule can be completed without exceeding Number credits per term.
canBeCompleted([], _).
canBeCompleted(Schedule, MaxCreditsPerTerm) :- 
	/*
		Step 1: Combine year and term values
        	course(cpsc200, 2020sum, 3, [req(pre,cpsc100)]),
        	course(cpsc210, 2020fall, 3, [req(alt,(cpsc100,math100))]),
        	course(math110, 2020fall, 3, [req(co,math100)])
	*/
	maplist(combineYearAndTerm, Schedule, NewCoursesRepresentation),
	/* 
		Step 2: Extract semester from each course
		    [‘2020sum’, ‘2020fall’, ‘2020fall’]
    */
    write(NewCoursesRepresentation),
	maplist(getSemester, NewCoursesRepresentation, SemesterList),
	/*
		Step 3: 
			Iterate through semester list and sum credits in NewCoursesRepresentation 
            [3,6,6]
            and make sure each term credit sum does not exceed the max.
    */
    semesterSumDoesNotExceedMax(SemesterList, NewCoursesRepresentation, MaxCreditsPerTerm).

% Example schedule. 
exampleSchedule([
        course(cpsc200,2020,sum,3,[req(pre,cpsc100)]),
        course(cpsc210,2020,fall,3,[req(alt,(cpsc100,math100))]),
        course(math110,2020,fall,3,[req(co,math100)])
]).

exampleSchedule2([
    course(cpsc200,2020-sum,3,[req(pre,cpsc100)]),
    course(cpsc210,2020-fall,3,[req(alt,(cpsc100,math100))]),
    course(math110,2020-fall,3,[req(co,math100)])
]).

getSemester(course(_, Semester, _, _), Semester).

combineYearAndTerm(course(Code, Y, T, Credits, Prereqs), course(Code, CombinedYearTerm, Credits, Prereqs)) :-
	atom_concat(Y, T, CombinedYearTerm).

getCredits(course(_, _, Credits, _), Credits).
getTerm(course(_, Term, _, _), Term).

% sumCredits(S, Term, TotalCredits) sums all the credits in schedule S for Term and returns that value as total credits
sumCredits([], _, 0).
sumCredits([H|T], TargetTerm, TotalCredits) :-
    getCredits(H, Credits),
    getTerm(H, CurrentTerm),
    (TargetTerm = CurrentTerm) ->
    sumCredits(T, TargetTerm, X),
    TotalCredits is X + Credits;
    sumCredits(T, TargetTerm, X),
    TotalCredits is X + 0.

semesterSumDoesNotExceedMax([], _, _) :- write("\n\n\nYES! This schedule can be completed. \n\n\n").
semesterSumDoesNotExceedMax([SemesterHead | SemesterTail], Courses, MaxCredits) :- 
    sumCredits(Courses, SemesterHead, TotalCredits),
    (TotalCredits =< MaxCredits) ->
    semesterSumDoesNotExceedMax(SemesterTail, Courses, MaxCredits);
    write("\n\n\nNO! This schedule cannot be completed. \n\n\n").