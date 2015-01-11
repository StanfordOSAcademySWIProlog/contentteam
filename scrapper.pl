:- use_module(library(http/http_open)).


scrape(Page) :-
	http_open('http://ucsd.edu/catalog/courses/CSE.html', DataStream, []),
	load_structure(DataStream, Page, []).




