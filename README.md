# Backend of the Course Scheduler

## Current Status
There is a script that can scrape the CSE department of UCSD for courses,
their names, number of unites, descriptions, and requirements. get_all_link
will create link for each course page depend on input Quarter and CourseID.
parsePrereq will take that link and creat list of prerequirest for that course,
and list_to_bool will convert that list to boolean expression.
prereq_proc will process such expression and output all possible combination of 
courses needed to take.
builddb will build course database and build/2 will build requirement database
based course database and input Quarter.
~~~~
$ swipl -s builddb.pl
~~~~

This creates a "database" file, `db.pl`, used by the backend.

####backend predicates:    
===for front end use===   
#####getPrereq:    
`getprereq_fact/3`: take Course ID, quarter, and third arg will be requirement fact   
`getprereq_list/3`: same as getprereq_fact, it third arg will be all combination of course needed to take  
`build/2`: take Quarter and file name, then create requirment database   

for detial explanation and usage, check each inside the file.


======old predicates=====   
`courses/1` and `course_ids/1`. `courses/1` can
be used to get a list of all courses. The result is a list with one
element per course. Each element is a term `course/5`.    
`requirement_to_list/3`: takes Dept and course ID (in atom) and able to get all
possible combination of prerequisites of that course (an atom list).    
`requirement_to_string/3`: same as above, output combination in string format.

####Handcodeddb are coded with following data format, with arguments: 
#####major/2   
1. Department name code   
2. Department full name     

#####course/5    
1. Course ID, an atom    
2. Course Title, an SWI7-style string    
3. Units, a term (see below)    
4. Text description, an SWI7-style string   
5. Prerequisites, an atom (for now)    

#####requirement/3    
1. Department/major name code (atom)    
2. Course ID (atom)
3. boolean expression of prerequisites, Course ID are in form of id('Dept','#').    

Units are represented as one of three terms:

- `exactly(Units)`: a course is worth `Units` units (an integer)
- `from_to(From, To)`: a course is worth anything from `From` to `To` units
- `one_of(List)`: a course is worth one of the integer values in `List`

The `course_ids/1` can be used to get a list of all course IDs.

1. Course ID, an atom
~~~~
    ['CSE 3', 'CSE 12', ....]. 
~~~~

The user should decide how to represent these for a human reader.
