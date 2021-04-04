# Prerequisite-Checker (Work In Progress)

This is a prolog course project for UBC CPSC 312

# What is the problem?

Given a knowledge base containing university courses that have information on:

- the term and year in which a course is offered
- which courses should be taken before which other courses

This prolog program will take:

- a list of courses you want to take

and return an **ordering of courses** such that prerequisites for any given course X are completed before one is scheduled to take course X.

# What is the something extra?

Our program will use Kahn's topological sorting algorithm (implemented in prolog) to calculate the order courses should be taken in.

Our program will also consider things like handling

- Alternate prerequisites (MATH xxx or CPSC yyy is a prerequisite to CPSC zzz)
- Co-requisites (CPSC xxx can be taken before or at the same time as CPSC yyy)

In addition, we will also provide a prolog query to determine whether a given ordering of courses can be completed without exceeding a maximum number of credits per term.

# Demo
Starting with the "what is the problem part."
Before the demo introduce:
  The structure of the course.
  The structure of the requirements.

Demo test cases:
  case1: course with no requirements.
  case2: course with prerequisite.
  case3: course with corequisite.
  case4: course with alternative prerequisites.
  case5: courses with multiple types of requirements.
  case5: fail cases.
Ask the TA about cases he want us to test.

Explain the code:
  The updateschedule function.
  The toposort fucntion.
  The fit function.

End with the "Whats something extra" part.
Let the TA ask questions.
  
