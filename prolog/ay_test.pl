:- module(ay_test,
    [
      class_properties2/3,
      create_class/2,
      create_restriction/5,
      create_property_some/4,
      create_property_value/4,
      modify_restriction/5,
      modify_property_some/4,
      modify_property_value/4,
      specification_class_of/2,
      t1_copy_state/2,
      find_by_name/3,
      t1_apply_grasp_effect/2
    ]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('knowrob_owl')).

:- rdf_meta
  class_properties2(r,r,r),
  create_class(r,r),
  create_restriction(r,r,r,r,r),
  create_property_some(r,r,r,r),
  create_property_value(r,r,r,r),
  modify_restriction(r,r,r,r,r),
  modify_property_some(r,r,r,r),
  modify_property_value(r,r,r,r),
  specification_class_of(r,r),
  t1_copy_state(r,r),
  copy_state_1(r,r,r,r),
  copy_object(r,r),
  find_by_name(r,r,r),
  t1_apply_grasp_effect(r,r),
  t1_apply_grasp_effect_1(r,r,r)
  .

:- rdf_db:rdf_register_ns(owl,    'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs,   'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob,'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(ay, 'http://knowrob.org/kb/ay.owl#', [keep(true)]).


test1 :-
  forall(rdf_has(ay:'T1Time',P,O),
    writef('T1Time.%t= %t\n', [P,O]) ).
test2 :-
  create_class(NewTime, ay:'T1Time'),
  writef('Created %t\n', [NewTime]).

%% class_properties2(?Class, ?Prop, ?Val) is nondet.
%
% Collect all property values of someValuesFrom-, hasValue-, and allValuesFrom- restrictions of a class
%
% @param Class Class whose restrictions are being considered
% @param Prop  Property whose restrictions in Class are being considered
% @param Val   Values that appear in a restriction of a superclass of Class on Property
%
class_properties2(Class, Prop, Val) :-
  (class_properties_some(Class, Prop, Val);
   class_properties_value(Class, Prop, Val);
   class_properties_all(Class, Prop, Val) ).


% Create an OWL class New as a subclass of SuperClass.
%   TODO: Generate a better name (rdf_node generates '__bnode*')
%   cf. rdf_bnode/1(rdf_db.pl), make_id/2(rdf_triple.pl), gensym/2
create_class(New, SuperClass) :-
  rdf_node(New),
  rdf_assert(New, rdf:type, owl:'Class'),
  rdf_assert(New, rdfs:subClassOf, SuperClass).


%% create_restriction(+Class, +Prop, +Value, +RestrType, -New) is det.
% Similar to create_restr in knowrob_owl.pl but maybe improved.
% e.g.  create_restriction(ay:'Test12', ay:testd14, literal(type(xsd:boolean,true)), owl:hasValue, New)
% rdf_node(New), rdf_assert(ay:'Test12', rdfs:subClassOf, New), rdf_assert(New, rdf:type, owl:'Restriction'), rdf_assert(New, owl:onProperty, ay:testd14), rdf_assert(New, owl:hasValue, literal(type(xsd:boolean,true))).
create_restriction(Class, Prop, Value, RestrType, New) :-
  \+ (class_properties(Class, Prop, Value)),
  rdf_node(New),
  rdf_assert(Class, rdfs:subClassOf, New),
  rdf_assert(New, rdf:type, owl:'Restriction'),
  rdf_assert(New, owl:onProperty, Prop),
  rdf_assert(New, RestrType, Value).

%% modify_restriction(+Class, +Prop, +Value, +RestrType, -New) is det.
% Modify a restriction Class.Prop.
% e.g.  modify_restriction(ay:'Test12', ay:testd14, literal(type(xsd:boolean,true)), owl:hasValue, New)
modify_restriction(Class, Prop, Value, RestrType, New) :-
  % When Class.Prop restriction exists, we retract it then assert:
  ( rdf_has(Class, rdfs:subClassOf, New),
    rdf_has(New, owl:onProperty, Prop),
    rdf_retractall(New, RestrType, V),
    rdf_assert(New, RestrType, Value), ! );
  % When Class.Prop restriction doesn't exist:
  create_restriction(Class, Prop, Value, RestrType, New).

% Create a property as the owl:someValuesFrom restriction.
create_property_some(Class, Prop, Value, New) :-
  create_restriction(Class, Prop, Value, owl:someValuesFrom, New).

% Create a property as the owl:hasValue restriction.
create_property_value(Class, Prop, Value, New) :-
  create_restriction(Class, Prop, Value, owl:hasValue, New).

% Modify a property as the owl:someValuesFrom restriction.
modify_property_some(Class, Prop, Value, New) :-
  modify_restriction(Class, Prop, Value, owl:someValuesFrom, New).

% Modify a property as the owl:hasValue restriction.
modify_property_value(Class, Prop, Value, New) :-
  modify_restriction(Class, Prop, Value, owl:hasValue, New).


% Find IndivClass whose specification classes include SpecClass.
specification_class_of(SpecClass, IndivClass) :-
  class_properties(SpecClass, ay:specificationOf, IndivClass).

% copy_state specialized to Toy1.
t1_copy_state(State, CopiedState) :-
  copy_state_1(State, CopiedState, ay:'T1State', ay:'T1Time').

copy_state_1(State, CopiedState, StateClass, TimeClass) :-
  create_class(NewTime, TimeClass),
  create_class(CopiedState, StateClass),
  create_property_some(CopiedState, ay:time, NewTime, _),
  % for all Obj = State.ay:object
  forall(class_properties(State, ay:object, Obj),(
      % Copy Obj to NewObj...?
      % copy_object(Obj, NewObj),
      % create_property_some(NewObj, ay:time, NewTime, _),
      % create_property_some(CopiedState, ay:object, NewObj, _)
      % TEST: Just copy the same object
      create_property_some(CopiedState, ay:object, Obj, _)
    )).

% Copy an object to a new object... which should be an on-demand???
% FIXME: This is incomplete.
% Object must be a subclass of ay:IndividualClass and has an ay:specificationOf property.
copy_object(Object, NewObject) :-
  % Make sure if Object is a subclass of ay:IndividualClass:
  rdfs_subclass_of(Object, ay:'IndividualClass'),
  % Create a class NewObject as an instance of Object.specificationOf:
  class_properties(Object, ay:specificationOf, IndivClass),
  create_class(NewObject, IndivClass),
  % Copy all properties of Object
  % TODO:
  true.


% Find an individual class ObjIndivClass from State.object.
find_by_name(State, ObjIndivClass, ObjInState) :-
  class_properties(State, ay:object, ObjInState),
  rdfs_subclass_of(ObjInState, ObjIndivClass).

% Test of applying grasping effect to a state.
% FIXME: When this process fails, some garbage of '__bnode*' remain. Remove them.
t1_apply_grasp_effect(State, NextState) :-
  t1_apply_grasp_effect_1(State, NextState, ay:'T1Obj1').
t1_apply_grasp_effect_1(State, NextState, Object) :-
  t1_copy_state(State, NextState),
  % Grasped1= new ay:'T1Grasped'
  % Grasped1.hand= ay:'T1Robot1'
  create_class(Grasped, ay:'T1Grasped'),
  modify_property_some(Grasped, ay:hand, ay:'T1Robot1', _),
  % NextState.object[Object].grasped= Grasped1
  find_by_name(NextState, Object, ObjInState),  %FIXME: This will overwrite the existing object.
  modify_property_some(ObjInState, ay:t1grasped, Grasped, _).  %FIXME: change to change_property_some.



