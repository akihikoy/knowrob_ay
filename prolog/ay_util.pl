:- module(ay_util,
    [
      member1/2,
      disp_properties/1,
      disp_properties/2
    ]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('knowrob_owl')).

:- rdf_meta
  disp_properties(r),
  disp_properties(r,r)
  .

:- rdf_db:rdf_register_ns(owl,    'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs,   'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob,'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(ay, 'http://knowrob.org/kb/ay.owl#', [keep(true)]).


% Similar to member/2, but stops when an element is detected.
member1(Elem,List) :- member(Elem,List), !.

% Display all properties of Class.
% If properties are classes, this recursively displays their properties.
disp_properties(Class) :-
  disp_properties_1(Class, [Class], [Class], []).
disp_properties(Class, IgnoreProps) :-
  findall(PG, (member(P,IgnoreProps), rdf_global_term(P,PG)), IgnoreProps2),
  disp_properties_1(Class, [Class], [Class], IgnoreProps2).
disp_properties_1(Class, L, Opened, IgnoreProps) :-
  forall((class_properties(Class, P, O), \+ member(P,IgnoreProps)),(
      %atomic_list_concat(L,'.',LS),
      writef('%t.%t= %t\n', [L,P,O]),
      (\+ member(O, Opened)
      ->
        append(L,[P],LL),
        disp_properties_1(O, LL, [O|Opened], IgnoreProps)
      ;
        writef('<Detected a loop at %t>\n', [O])
      )
    )).

