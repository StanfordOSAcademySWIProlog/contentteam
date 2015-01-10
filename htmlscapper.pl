:- module(content, []).
/**<module> Defines the API for the content team to extract data of
 * classes for the shell. The list of predicates are listed here.
 *
 * List predicates.
 *
 */

:- use_module(library(http/http_open)).


% Load a static webpage for the time being.
load_structure('
