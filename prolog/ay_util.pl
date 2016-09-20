:- module(ay_util,
    [
      disp_properties/1,
      disp_properties_1/2
    ]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('knowrob_owl')).

:- rdf_meta
  disp_properties(r),
  disp_properties_1(r,r)
  .

:- rdf_db:rdf_register_ns(owl,    'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs,   'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob,'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(ay, 'http://knowrob.org/kb/ay.owl#', [keep(true)]).


% Display all properties of Class.
% If properties are classes, this recursively displays their properties.
disp_properties(Class) :-
  disp_properties_1(Class, [Class]).
disp_properties_1(Class, L) :-
  forall(class_properties(Class, P, O),(
      %atomic_list_concat(L,'.',LS),
      writef('%t.%t= %t\n', [L,P,O]),
      append(L,[P],LL),
      disp_properties_1(O, LL)
    )).

