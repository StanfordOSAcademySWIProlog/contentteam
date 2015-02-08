:- module(handcodeddb, [major/2,course/5,requirement/3]).

major("COGN","Communication/ General").
major("COGR","Communication/ Graduate").
major("COGS","Cognitive Science").
major("COHI","Communication/ Human Information Processing").
major("COMT","Communication Media Methods").
major("CONT","Contemporary Issues").
major("COSF","Communication as Social Force").
major("CSE","Computer Science and Engineering").


course('CSE 12',"Basic Data Structures and Object-Oriented Design",exactly(4),"Use and implementation of basic data structures including linked lists, stacks, and queues. Use of advanced structures such as binary trees and hash tables. Object-oriented design including interfaces, polymorphism, encapsulation, abstract data types, pre-/post-conditions. Recursion. Uses Java and Java Collections.",'CSE 8B or CSE 11, and concurrent enrollment with CSE 15L.').
course('CSE 30',"Computer Organization and Systems Programming",exactly(4),"Introduction to organization of modern digital computers¡ªunderstanding the various components of a computer and their interrelationships. Study of a specific architecture/machine with emphasis on systems programming in C and Assembly languages in a UNIX environment.",'CSE 12, CSE 15L, or consent of instructor.').
course('CSE 100',"Advanced Data Structures",exactly(4),"High-performance data structures and supporting algorithms. Use and implementation of data structures like (un)balanced trees, graphs, priority queues, and hash tables. Also memory management, pointers, recursion. Theoretical and practical performance analysis, both average case and amortized. Uses C++ and STL. Credit not offered for both Math 176 and CSE 100. Equivalent to Math 176. Recommended preparation: background in C or C++ programming.",'CSE 12, CSE 15L, CSE 21 or Math 15B, and CSE 5A or CSE 30 or ECE 15 or MAE 9.').
course('CSE 110',"Software Engineering",exactly(4),"Introduction to software development and engineering methods, including specification, design, implementation, testing, and process. An emphasis on team development, agile methods, and use of tools such as IDE¡¯s, version control, and test harnesses. CSE 70 is renumbered to CSE 110: students may not receive credit for both CSE 70 and CSE 110.",'CSE 12, CSE 21, or Math 15B.').


requirement("CSE","12",bool(or(val("CSE8B"),val("CSE11")))).
requirement("CSE","30",bool(and(val("CSE12"),val("CSE15L")))).
requirement("CSE","100",bool(and(and(and(or(val("CSE21"),val("MATH15B")),val("CSE12")),val("CSE15L")),or(or(or(val("CSE5A"),val("CSE30")),val("ECE15")),val("MAE9"))))).
requirement("CSE","110",bool(and(val("CSE12"),or(val("CSE21"),val("MATH15B"))))).

