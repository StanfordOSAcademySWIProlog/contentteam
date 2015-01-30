%% This is how you can load the file and call the predicate:
%
% ~~~~
% ?- [scrape].
% true.
%
% ?- scrape('http://ucsd.edu/catalog/courses/CSE.html', Data).
% Data = [course('CSE', '3', 'Fluency in Information Technology', '4')-description([73, 110, 116, 114, 111, 100|...]), course('CSE', '4GS', 'Mathematical Beauty in Rome', '4')-description([69, 120, 112, 108, 111|...]), course('CSE', '6GS', 'Mathematical Beauty in Rome Lab', '4')-description([67, 111, 109, 112|...]), course('CSE', '5A', 'Introduction to Programming I', '4')-description([40, 70, 111|...]), course('CSE', '7', 'Introduction to Programming with Matlab', '4')-description([70, 117|...]), course('CSE', '8A', 'Introduction to Computer Science: Java I', '4')-description([73|...]), course('CSE', '8B', 'Introduction to Computer Science: Java II', '4')-description([...|...]), course(..., ..., ..., ...)-description(...), ... - ...|...].
% ~~~~
%
% You should get the same thing pretty much
% 

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

%% At the moment, this just writes a fact with four atoms to a file.
%% This is a cheap way to "bootstrap" the initial database.
write_to_file(Courses, File) :-
    setup_call_cleanup(open(File, write, Out),
        forall(member(N-_, Courses), format(Out, "~q.~n", [N])),
        close(Out)).

%% Give it the URL of the CSE courses (see the example at the top),
% it will spit out a list of course names and descriptions
%
% I ended up using the initial approach by Kristiyan for this;
% it is the cleaner way to do it:
% 1. open an http connection
% 2. parse the html you read from it
% 
% Note how I suppress the errors; it took me a while to find this
% particular option...
scrape(URL, Data) :-
    http_open(URL, In, []),
    load_html(In, DOM, [syntax_errors(quiet)]),
    close(In),
    courses(DOM, Data).

%% This is to get the subject codes. It will be redesigned,
% so that it is not a standalone function.
% 
%
temp_subjects(URL, Data) :-
    http_open(URL, In, []),
    load_html(In, DOM, [syntax_errors(quiet)]),
    close(In),
    subjects(DOM, Data).
    

%% Collect all relevant <p>'s in a list and make a list of pairs out of it
courses(DOM, Courses) :-
    findall(CN_D, cn_d(DOM, CN_D), CNs_Ds),
    list_to_pairs(CNs_Ds, Courses).

%% cn_d/2 is a non-deterministic predicate; it will have many solutions
% so you need to use findall/3 to evaluate it and collect them all in a
% list
%
% There is a "problem": using xpath, I couldn't find a way to tell it
% "take a pair of class='course-name' and class='course-descriptions'
% <p> tags
% So instead this code assumes each <p class='course-name'> is
% eventually followed by a <p class='course-descriptions'>, and those two
% together describe one course.
cn_d(DOM, CN_D) :-
    xpath(DOM, //p, P), % select a <p>
    once(cn_d_1(P, CN_D)). % succeed only once!

%% cn_d_1/2 is a help predicate used by cn_d;
% in most other programming languages, this would probably be an
% "if-else" or a "case" statement; but in Prolog this feels a bit more
% natural
cn_d_1(P, Name) :- % selects class='course-name'
    cn_d_2(P, 'course-name', Codes),
    phrase(course_name(Name), Codes).
cn_d_1(P, description(Desc)) :- % class='course-descriptions'
    cn_d_2(P, 'course-descriptions', Codes),
    dif(Codes, []),
    % phrase/3: note the extra argument at the end, here _Rest_
    % whatever is not parsed by the DCG is left in the last
    % argument, _Rest_. At the moment, course_descriptions//1
    % does not parse anything at all, so the everything is left
    % in _Rest_
    phrase(course_descriptions(description(Desc)), Codes, Rest).
    % The prerequisites will be left in the _Rest_ to be parsed.


%% This is the second help predicate
% The normalize_space argument is necessary to extract the text
% from the DOM. We than make it into a list of codes that we can
% parse with a DCG
cn_d_2(P, Class, Codes) :-
    xpath(P, /self(@class=Class, normalize_space), Text),
    atom_codes(Text, Codes).

%% Collect all relevant <tr>s in a list.
subjects(DOM, Subjects) :-
    findall(Code_Desc, code_desc(DOM,Code_Desc), Codes_Descs).

%% code_desc/2 is a non-deterministic predicate.
% 
% xpath will return all instances of <tr>, but there are 
% 2 <td> tags in each one of them.
%
code_desc(DOM, Code_Desc) :-
    xpath(DOM, //tr, TR), % select a <p>
    once(code_desc_1(TR, Code_Desc)). % succeed only once!

code_desc_1(TR, Code_Desc) :-
    true.

%% This takes a list in which pairs are just the pairs of two
% consequitive elements and makes a list of proper "pairs" out of it;
% it is generic, in the sense that it will make pairs of any list with
% an even number of elements in it.
list_to_pairs([], []).
list_to_pairs([A,B|Rest], [A-B|Pairs]) :-
    list_to_pairs(Rest, Pairs).

:- use_module(library(dcg/basics)).

%% A helper DCG that would help us parse the description.
% The Prerequisites must be parsed as well to extract the required
% classes for this particular course.
course_descriptions(description(Desc)) -->
    % Get the whole description before the word Prerequisites:
    string_without(":", Desc_codes),
    %splitter("Prerequisites:", Desc_codes, Desc),
    % Get the string with the prerequisites.
    %string(Prereq_codes), `.`,   
    {
	atom_codes(Desc, Desc_codes)
    }.

%% This is how you can parse the "header" of each course: this is the
% text contained in the <p class='course-name'>
% Note that 'normalize_space' above took care of putting exactly one
% space between "words", and removed any leading and trailing white space
course_name(course(M, N, T, U)) -->
    % nonblanks followed by a single white space is the "CSE"
     nonblanks(M_codes), white,
    % everything up to a dot, and the dot, and a white space is the course
    string_without(`.`, N_codes), `.`, white,
    % Everything up to a space...
    string(T_codes), white,
    % ... followed by something enclosed in "(" and ")", and at the
    % very end of the whole list
    `(`, string(U_codes), `)`,
    % convert everything to atoms
    {   
	atom_codes(M, M_codes),
        atom_codes(N, N_codes),
        atom_codes(T, T_codes),
        atom_codes(U, U_codes)
    }.
% you will have to parse the units to a number, and take care of
% units that look like "1-4", for example

