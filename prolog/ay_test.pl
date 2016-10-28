:- module(ay_test,
    [
      is_individual/1,
      class_properties2/3,
      owl_has_property/2,
      class_properties_cardinality/4,
      class_properties_cardinality/5,
      create_subclass/2,
      %make_individual/1,
      rdf_unique_id2/3,
      create_property/5,
      create_property_some/3,
      create_property_some/4,
      create_property_value/3,
      create_property_value/4,
      modify_property/5,
      modify_property_some/3,
      modify_property_some/4,
      modify_property_value/3,
      modify_property_value/4,
      remove_property/2,
      specification_class_of/2,
      create_state_from/3,
      copy_properties/2,
      remove_object_from_state/2,
      find_by_name/3,
      find_by_name_and_create_timeinst/4,
      create_timeinst_from/3,
      t1_apply_grasp_effect/2,
      t1_apply_grasp_effect2/2,
      t1_apply_grasp_effect3/2,
      t1_test_grasp_precond/2,
      t1_apply_move_effect/2,
      t1_test_move_precond/2,
      t1_apply_release_effect/2,
      t1_test_release_precond/2,
      t1_test_pickandplace_goal/2,
      list_acted_on/3,
      selectable_action_at_state/2,
      find_action_seq_df/3,
      find_action_seq_df_1/4,
      find_action_seq_bf/3,
      find_action_seq_bf_1/4,
      eval_expression/5
    ]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('knowrob_owl')).
%:- use_module(library('t20_owl')).

:- rdf_meta
  is_individual(r),
  class_properties2(r,r,r),
  owl_has_property(r,r),
  class_properties_cardinality(r,r,r,r),
  class_properties_cardinality(r,r,r,r,r),
  class_properties_1_cardinality(r,r,r,r),
  create_subclass(r,r),
  %make_individual(r),
  rdf_unique_id2(r,r,r),
  create_property(r,r,r,r,r),
  create_property_some(r,r,r),
  create_property_some(r,r,r,r),
  create_property_value(r,r,r),
  create_property_value(r,r,r,r),
  modify_property(r,r,r,r,r),
  modify_property_some(r,r,r),
  modify_property_some(r,r,r,r),
  modify_property_value(r,r,r),
  modify_property_value(r,r,r,r),
  remove_property(r,r),
  specification_class_of(r,r),
  create_state_from(r,r,r),
  copy_properties(r,r),
  remove_object_from_state(r,r),
  find_by_name(r,r,r),
  find_by_name_and_create_timeinst(r,r,r,r),
  create_timeinst_from(r,r,r),
  t1_apply_grasp_effect(r,r),
  t1_apply_grasp_effect_1(r,r,r),
  t1_apply_grasp_effect2(r,r),
  t1_apply_grasp_effect2_1(r,r,r,r),
  t1_apply_grasp_effect2_2(r,r),
  t1_apply_grasp_effect3(r,r),
  t1_apply_grasp_effect3_1(r,r,r,r),
  t1_test_grasp_precond(r,r),
  t1_test_grasp_precond_1(r,r,r,r),
  t1_apply_move_effect(r,r),
  t1_apply_move_effect_1(r,r,r,r),
  t1_test_move_precond(r,r),
  t1_test_move_precond_1(r,r,r,r),
  t1_apply_release_effect(r,r),
  t1_apply_release_effect_1(r,r,r),
  t1_test_release_precond(r,r),
  t1_test_release_precond_1(r,r,r),
  t1_test_pickandplace_goal(r,r),
  t1_test_pickandplace_goal_1(r,r,r,r),
  list_acted_on(r,r,r),
  selectable_action_at_state(r,r),
  find_action_seq_df(r,r,r),
  find_action_seq_df_1(r,r,r,r),
  find_action_seq_bf(r,r,r),
  find_action_seq_bf_1(r,r,r,r),
  eval_expression(r,r,r,r,r)
  .

:- rdf_db:rdf_register_ns(owl,    'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(rdfs,   'http://www.w3.org/2000/01/rdf-schema#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob,'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(ay, 'http://knowrob.org/kb/ay.owl#', [keep(true)]).


%test1 :-
  %forall(rdf_has(ay:'T1Time',P,O),
    %writef('T1Time.%t= %t\n', [P,O]) ).
%test2 :-
  %create_subclass(NewTime, ay:'T1Time'),
  %writef('Created %t\n', [NewTime]).


%%% Setup for Toy1 %%%
% A general action class like ay:T1Action.
generalAction(C):- rdf_global_term(ay:'T1Action',C),!.
stateClass(C):- rdf_global_term(ay:'T1State',C),!.
timeClass(C):- rdf_global_term(ay:'T1Time',C),!.


% True if Class is an individual class.
is_individual(Class) :-
  % This is a bit slower (but not much) as it searches all super classes:
  rdfs_subclass_of(Class, ay:'IndividualClass').
  % This is faster.  Is this enough???
  %rdf_has(Class, rdfs:subClassOf, ay:'IndividualClass');
  %class_properties(Class, ay:specificationOf, _).


%-------------------------------------------------------------------------------------------
% These predicates are based on knowrob_owl.pl: class_properties*
%-------------------------------------------------------------------------------------------

%%% class_properties2(?Class, ?Prop, ?Val) is nondet.
%%
%% Collect all property values of someValuesFrom-, hasValue-, and allValuesFrom- restrictions of a class
%%
%% @param Class Class whose restrictions are being considered
%% @param Prop  Property whose restrictions in Class are being considered
%% @param Val   Values that appear in a restriction of a superclass of Class on Property
%%
%class_properties2(Class, Prop, Val) :-
  %(class_properties_some(Class, Prop, Val);
   %class_properties_value(Class, Prop, Val);
   %class_properties_all(Class, Prop, Val) ).

%% class_properties2(?Class, ?Prop, ?Val) is nondet.
% Collect all property values of someValuesFrom- and hasValue-restrictions of a class.
% Faster than class_properties2.
%
% @param Class Class whose restrictions are being considered
% @param Prop  Property whose restrictions in Class are being considered
% @param Val   Values that appear in a restriction of a superclass of Class on Property
%
class_properties2(Class, Prop, Val) :-
  class_properties2_1(Class, Prop, Val).

class_properties2(Class, Prop, Val) :-         % also consider properties of superclasses
  owl_subclass_of(Class, Super), Class\=Super,
  class_properties2_1(Super, Prop, Val).

class_properties2_1(Class, Prop, Val) :-

  ( (nonvar(Class)) -> (owl_direct_subclass_of(Class, Sup)) ; (Sup     = Class)),
  ( (nonvar(Prop))  -> (rdfs_subproperty_of(SubProp, Prop)) ; (SubProp = Prop)),

  ( owl_restriction(Sup,restriction(SubProp, some_values_from(Val))) ;
    owl_restriction(Sup,restriction(SubProp, has_value(Val))) ).

%-------------------------------------------------------------------------------------------
% These predicates are based on knowrob_owl.pl: class_properties*
%-------------------------------------------------------------------------------------------

% Check if Class has a property Prop
%
% @param Class Class whose restrictions are being considered
% @param Prop  Property whose restrictions in Class are being considered
owl_has_property(Class, Prop) :-
  owl_has_property_1(Class, Prop).

owl_has_property(Class, Prop) :-
  owl_subclass_of(Class, Super), Class\=Super,
  owl_has_property_1(Super, Prop).

owl_has_property_1(Class, Prop) :-

  ( (nonvar(Class)) -> (owl_direct_subclass_of(Class, Sup)) ; (Sup     = Class)),
  ( (nonvar(Prop))  -> (rdfs_subproperty_of(SubProp, Prop)) ; (SubProp = Prop)),

  ( owl_restriction(Sup,restriction(SubProp, some_values_from(_))) ;
    owl_restriction(Sup,restriction(SubProp, has_value(_))) ).



%-------------------------------------------------------------------------------------------




%% class_properties_cardinality
%
% Collect all property values of cardinality-restrictions of a class
%
% @param Class Class whose restrictions are being considered
% @param Prop  Property whose restrictions in Class are being considered
% @param Val   Values that appear in a restriction of a superclass of Class on Property
% @param Card  Cardinality that appears in a restriction of a superclass of Class on Property
% @param 5th parameter holds a class (may be a superclass of Class) that defines Prop.
%
class_properties_cardinality(Class, Prop, Val, Card) :-  % read directly asserted properties
  class_properties_cardinality(Class, Prop, Val, Card, _).

class_properties_cardinality(Class, Prop, Val, Card, Class) :-  % read directly asserted properties
  class_properties_1_cardinality(Class, Prop, Val, Card).

class_properties_cardinality(Class, Prop, Val, Card, Super) :-  % also consider properties of superclasses
  owl_subclass_of(Class, Super), Class\=Super,
  class_properties_1_cardinality(Super, Prop, Val, Card).

class_properties_1_cardinality(Class, Prop, Val, cardinality(Min, Max)) :-  % read all values for cardinality restrictions

  ( (nonvar(Class)) -> (owl_direct_subclass_of(Class, Sup)) ; (Sup     = Class)),
  ( (nonvar(Prop))  -> (rdfs_subproperty_of(SubProp, Prop)) ; (SubProp = Prop)),

  % What we want to do here is something like:
  %   owl_restriction(Sup,restriction(SubProp, cardinality(Min, Max))).
  % However restriction_facet used in owl_restriction is incomplete with
  % qualified cardinalities.  We define an improved version of restriction_facet.
  % The following code is similar to owl_cardinality_on_class.
  rdfs_individual_of(Sup, owl:'Restriction'),
  rdf_has(Sup, owl:onProperty, SubProp),
  restriction_facet(Sup, cardinality(Min, Max)),
  (rdf_has(Sup, owl:onClass, QVal) -> Val=QVal ; Val=owl:'Thing').

% Extended version of restriction_facet defined in owl.pl
% Added checks for qualified cardinalities.
restriction_facet(R, cardinality(Min, Max)) :-
        (   rdf_has(R, owl:cardinality, literal(Atom))
        ->  non_negative_integer(Atom, Min, R, owl:cardinality),
            Max = Min
        ;   rdf_has(R, owl:minCardinality, literal(MinAtom))
        ->  non_negative_integer(MinAtom, Min, R, owl:minCardinality),
            (   rdf_has(R, owl:maxCardinality, literal(MaxAtom))
            ->  non_negative_integer(MaxAtom, Max, R, owl:maxCardinality)
            ;   Max = inf
            )
        ;   rdf_has(R, owl:maxCardinality, literal(MaxAtom))
        ->  non_negative_integer(MaxAtom, Max, R, owl:maxCardinality),
            Min = 0
        % Extension for qualified cardinalities:
        ;   rdf_has(R, owl:qualifiedCardinality, literal(Atom))
        ->  non_negative_integer(Atom, Min, R, owl:qualifiedCardinality),
            Max = Min
        ;   rdf_has(R, owl:minQualifiedCardinality, literal(MinAtom))
        ->  non_negative_integer(MinAtom, Min, R, owl:minQualifiedCardinality),
            (   rdf_has(R, owl:maxQualifiedCardinality, literal(MaxAtom))
            ->  non_negative_integer(MaxAtom, Max, R, owl:maxQualifiedCardinality)
            ;   Max = inf
            )
        ;   rdf_has(R, owl:maxQualifiedCardinality, literal(MaxAtom))
        ->  non_negative_integer(MaxAtom, Max, R, owl:maxQualifiedCardinality),
            Min = 0
        ).

% Just copied from owl.pl to support restriction_facet.
non_negative_integer(type(_Type, Atom), Int, S, P) :-
        nonvar(Atom), !,
        non_negative_integer(Atom, Int, S, P).
non_negative_integer(Atom, Int, _, _) :-
        catch(atom_number(Atom, Int), _, fail), !,
        integer(Int),
        Int >= 0.
non_negative_integer(Atom, _, S, P) :-
        rdf_equal(xsd:nonNegativeInteger, Range),
        rdf_global_id(P, Pred),
        print_message(error,
                      rdf_illegal_object(S,Pred,literal(Atom),Range)),
        fail.


% Based on rdf_unique_id in knowrob_owl.pl
rdf_unique_id2(Class, UniqID, Delim) :-
  % Generate a random string Sub
  %crypt(Class, Seed1),
  %append("$1$", Seed1, Seed),
  %format(atom(Hash), '~s~n', [Seed]),
  %sub_atom(Hash, 3, 8, _, Sub),
  crypt(Class, Seed),
  format(atom(Hash), '~s~n', [Seed]),
  sub_atom(Hash, 0, 8, _, Sub),
  % Sub to ID
  atom_concat(Class,  Delim, Class2),
  atom_concat(Class2, Sub, SubClass),

  % check if there is no triple with this identifier as subject or object yet
  ((rdf(SubClass,_,_);rdf(_,_,SubClass)) ->
    (rdf_unique_id2(Class, UniqID, Delim));
    (UniqID = SubClass)).


% Create an OWL class New as a subclass of SuperClass.
%   TODO: Generate a better name (rdf_node generates '__bnode*')
%   cf. rdf_bnode/1(rdf_db.pl), make_id/2(rdf_triple.pl), gensym/2
create_subclass(New, SuperClass) :-
  %rdf_instance_from_class(owl:'Class', New),  NOT INSTANCE, BUT SUBCLASS!
  %rdf_node(New),
  %rdf_assert(New, rdf:type, owl:'Class'),
  rdf_unique_id2(SuperClass, New, '-'),
  rdf_assert(New, rdf:type, owl:'Class'),
  rdf_assert(New, rdfs:subClassOf, SuperClass).

%% Make a class Class to be an individual class,
%% i.e. a subclass of ay:'IndividualClass'.
%make_individual(Class) :-
  %rdf_assert(Class, rdfs:subClassOf, ay:'IndividualClass').

%% create_property(+Class, +Prop, +Value, +RestrType, -New) is det.
% Similar to create_restr in knowrob_owl.pl but maybe improved.
% e.g.  create_property(ay:'Test12', ay:testd14, literal(type(xsd:boolean,true)), owl:hasValue, New)
% rdf_node(New), rdf_assert(ay:'Test12', rdfs:subClassOf, New), rdf_assert(New, rdf:type, owl:'Restriction'), rdf_assert(New, owl:onProperty, ay:testd14), rdf_assert(New, owl:hasValue, literal(type(xsd:boolean,true))).
% NOTE: If [Class.Prop=Value] exists, nothing is changed.
% NOTE: Even if [Class.Prop=OtherValue] exists, this inserts [Class.Prop=Value].
create_property(Class, Prop, Value, RestrType, New) :-
  \+ (class_properties2(Class, Prop, Value)),
  rdf_node(New),
  rdf_assert(Class, rdfs:subClassOf, New),
  rdf_assert(New, rdf:type, owl:'Restriction'),
  rdf_assert(New, owl:onProperty, Prop),
  rdf_assert(New, RestrType, Value).

% Create a property as the owl:someValuesFrom restriction.
create_property_some(Class, Prop, Value) :-
  create_property_some(Class, Prop, Value, _).
create_property_some(Class, Prop, Value, New) :-
  create_property(Class, Prop, Value, owl:someValuesFrom, New).

% Create a property as the owl:hasValue restriction.
create_property_value(Class, Prop, Value) :-
  create_property_value(Class, Prop, Value, _).
create_property_value(Class, Prop, Value, New) :-
  create_property(Class, Prop, Value, owl:hasValue, New).

%% modify_property(+Class, +Prop, +Value, +RestrType, -New) is det.
% Modify a restriction Class.Prop.
% Note: If Class.Prop does not exist, we create it.
% e.g.  modify_property(ay:'Test12', ay:testd14, literal(type(xsd:boolean,true)), owl:hasValue, New)
modify_property(Class, Prop, Value, RestrType, New) :-
  % When Class.Prop restriction exists, we retract it then assert:
  ( rdf_has(Class, rdfs:subClassOf, New),
    rdf_has(New, owl:onProperty, Prop),
    rdf_retractall(New, RestrType, V),
    rdf_assert(New, RestrType, Value), ! );
  % When Class.Prop restriction doesn't exist:
  create_property(Class, Prop, Value, RestrType, New).

% Modify a property as the owl:someValuesFrom restriction.
% Note: If Class.Prop does not exist, we create it.
modify_property_some(Class, Prop, Value) :-
  modify_property_some(Class, Prop, Value, _).
modify_property_some(Class, Prop, Value, New) :-
  modify_property(Class, Prop, Value, owl:someValuesFrom, New).

% Modify a property as the owl:hasValue restriction.
% Note: If Class.Prop does not exist, we create it.
modify_property_value(Class, Prop, Value) :-
  modify_property_value(Class, Prop, Value, _).
modify_property_value(Class, Prop, Value, New) :-
  modify_property(Class, Prop, Value, owl:hasValue, New).

% Remove all restrictions matching with Class.Prop.
remove_property(Class, Prop) :-
  rdf_has(Class, rdfs:subClassOf, Restr),
  rdf_has(Restr, owl:onProperty, Prop),
  rdf_retractall(Restr, RestrType, Value),
  rdf_retractall(Class, rdfs:subClassOf, Restr).


% Find IndivClass whose specification classes include SpecClass.
specification_class_of(SpecClass, IndivClass) :-
  class_properties2(SpecClass, ay:specificationOf, IndivClass).

create_state_from(State, NewState, NewTime) :-
  stateClass(StateClass),
  timeClass(TimeClass),

  create_subclass(NewTime, TimeClass),
  create_subclass(NewState, StateClass),
  create_property_some(NewState, ay:time, NewTime),
  % for all Obj = State.ay:thing
  forall(class_properties2(State, ay:thing, Obj),(
      % Copy Obj to NewObj...?
      % copy_object(Obj, NewObj),
      % create_property_some(NewObj, ay:time, NewTime),
      % create_property_some(NewState, ay:thing, NewObj)
      % TEST: Just copy the same object (Maybe this is okay)
      create_property_some(NewState, ay:thing, Obj)
    )).

%DEPRECATED
% Copy an object to a new object... which should be an on-demand???
% FIXME: This is incomplete.
% Object must be a subclass of ay:IndividualClass and has an ay:specificationOf property.
copy_object(Object, NewObject) :-
  % Make sure if Object is a subclass of ay:IndividualClass:
  is_individual(Object),
  % Create a class NewObject as an instance of Object.specificationOf:
  class_properties2(Object, ay:specificationOf, IndivClass),
  create_subclass(NewObject, IndivClass),
  % Copy all properties of Object
  % TODO:
  true.

copy_properties(Src, Dst) :-
  forall(class_properties2(Src, Prop, Obj),(
    %FIXME: TODO: We should consider when Prop is value type or all type???
    create_property_some(Dst, Prop, Obj)
  )).


% Remove Object from State.object.
% Hint: Use together with find_by_name to remove by an individual class.
remove_object_from_state(State, Object) :-
  rdf_has(State, rdfs:subClassOf, ObjRestr),
  rdf_has(ObjRestr, owl:someValuesFrom, Object),  %FIXME: maybe we need to retract owl:hasValue as well?
  rdf_retractall(ObjRestr, P, O),
  rdf_retractall(State, rdfs:subClassOf, ObjRestr).

% Find ObjInState from State.object that is an individual class ObjIndiv.
find_by_name(ObjIndiv, State, ObjInState) :-
  ( class_properties2(State, ay:thing, X),
    rdfs_subclass_of(X, ObjIndiv) ),
  ObjInState= X.

% Find S from State.object that is a specification of individual class ObjIndiv.
% If S.time is not Time, we create a new state NewObjInState and
% replace S in State.object by NewObjInState.
% If s.time is equal to Time, then NewObjInState=S.
find_by_name_and_create_timeinst(ObjIndiv, State, Time, NewObjInState) :-
  find_by_name(ObjIndiv, State, ObjInState),
  (class_properties2(ObjInState, ay:time, Time)
    ->
      NewObjInState= ObjInState
    ;
      create_subclass(NewObjInState, ObjIndiv),
      % Copy properties:
      copy_properties(ObjInState, NewObjInState),
      % Set time:
      modify_property_some(NewObjInState, ay:time, Time),
      % Remove the old one from State.object:
      remove_object_from_state(State, ObjInState),
      % Add the new one to State.object:
      create_property_some(State, ay:thing, NewObjInState)
    ).

% Create a new time instance NewObj from SrcObj.
% If SrcObj.time is not Time, create a new specification class NewObj
% from an individual class SrcObj.specificationOf,
% and copy all parameters from SrcObj to NewObj.
% NewObj.time is set to be Time.
% If SrcObj.time is not Time, then NewObj=SrcObj.
create_timeinst_from(NewObj, SrcObj, Time) :-
  (class_properties2(SrcObj, ay:time, Time)
    ->
      NewObj= SrcObj
    ;
      class_properties2(SrcObj, ay:specificationOf, ObjIndiv),
      create_subclass(NewObj, ObjIndiv),
      % Copy properties:
      copy_properties(SrcObj, NewObj),
      % Set time:
      modify_property_some(NewObj, ay:time, Time)
    ).



%DEPRECATED
% Test of applying grasping effect to a state.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_grasp_effect(State, NextState) :-
  t1_apply_grasp_effect_1(State, NextState, ay:'T1Obj1').
t1_apply_grasp_effect_1(State, NextState, Object) :-
  % Prepare a state at next time
  create_state_from(State, NextState, NextTime),

  % Grasped1= new ay:'T1Grasped'
  % Grasped1.hand= ay:'T1Robot1'
  create_subclass(Grasped, ay:'T1Grasped'),
  rdf_assert(Grasped, rdfs:subClassOf, ay:'IndividualClass'),
  rdf_assert(Grasped, rdfs:subClassOf, ay:'T1TimeVariantThing'),
  create_property_some(Grasped, ay:hand, ay:'T1Robot1'),
  create_property_some(Grasped, ay:time, NextTime),

  % NextState.object[Object].grasped= Grasped1
  find_by_name_and_create_timeinst(Object, NextState, NextTime, ObjInState),

  modify_property_some(ObjInState, ay:t1grasped, Grasped).


%DEPRECATED
% Test of applying grasping effect to a state version 2.
% Trying to write in a more general form.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_grasp_effect2(State, NextState) :-
  t1_apply_grasp_effect2_1(State, NextState, ay:'T1Obj1', ay:'T1Robot1').
t1_apply_grasp_effect2_1(State, NextState, Object, Hand) :-
  % Prepare a state at next time
  create_state_from(State, NextState, _),

  % Prepare an individual grasping-task and action classes
  create_subclass(GraspingTask, ay:'T1GraspingTask'),
  rdf_assert(GraspingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(GraspingTask, ay:object, Object),
  create_property_some(GraspingTask, ay:hand, Hand),
  create_subclass(GraspingAct, ay:'T1GraspingAction'),
  rdf_assert(GraspingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(GraspingAct, ay:task, GraspingTask),

  % Perform the effect of the action
  t1_apply_grasp_effect2_2(NextState, GraspingAct).

%DEPRECATED
% Perform the effect of the action; State is modified.
t1_apply_grasp_effect2_2(State, Action) :-
  % Extract properties
  class_properties(State, ay:time, Time),
  class_properties(Action, ay:task, Task),
  % Extract properties(2) which will be done by ay:Expression
  class_properties(Task, ay:object, Object),
  class_properties(Task, ay:hand, Hand),

  % Grasped1= new ay:'T1Grasped'
  % Grasped1.hand= ay:'T1Robot1'
  create_subclass(Grasped, ay:'T1Grasped'),
  rdf_assert(Grasped, rdfs:subClassOf, ay:'IndividualClass'),
  rdf_assert(Grasped, rdfs:subClassOf, ay:'T1TimeVariantThing'),
  create_property_some(Grasped, ay:hand, Hand),
  create_property_some(Grasped, ay:time, Time),

  % State.object[Object].grasped= Grasped1
  find_by_name_and_create_timeinst(Object, State, Time, ObjInState),

  modify_property_some(ObjInState, ay:t1grasped, Grasped).



% Test of applying grasping effect to a state version 3.
% Trying to write in a more general form:
%   Action.effect is written in OWL.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_grasp_effect3(State, NextState) :-
  t1_apply_grasp_effect3_1(State, NextState, ay:'T1Obj1', ay:'T1Robot1').
t1_apply_grasp_effect3_1(State, NextState, Object, Hand) :-
  % Prepare a state at next time
  create_state_from(State, NextState, _),

  % Prepare an individual grasping-task and action classes
  create_subclass(GraspingTask, ay:'T1GraspingTask'),
  rdf_assert(GraspingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(GraspingTask, ay:object, Object),
  create_property_some(GraspingTask, ay:hand, Hand),
  create_subclass(GraspingAct, ay:'T1GraspingAction'),
  rdf_assert(GraspingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(GraspingAct, ay:task, GraspingTask),

  % Perform the effect of the action
  class_properties(GraspingAct, ay:effect, Effect),
  eval_expression(Effect, GraspingAct, NextState, isRValue, _).

% Test of grasping precondition.
t1_test_grasp_precond(State, Result) :-
  t1_test_grasp_precond_1(State, ay:'T1Obj1', ay:'T1Robot1', Result).
t1_test_grasp_precond_1(State, Object, Hand, Result) :-
  % NOTE: Be aware that this part is the same as one that in t1_apply_grasp_effect3_1
  % Prepare an individual grasping-task and action classes
  create_subclass(GraspingTask, ay:'T1GraspingTask'),
  rdf_assert(GraspingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(GraspingTask, ay:object, Object),
  create_property_some(GraspingTask, ay:hand, Hand),
  create_subclass(GraspingAct, ay:'T1GraspingAction'),
  rdf_assert(GraspingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(GraspingAct, ay:task, GraspingTask),

  % Test the precondition of the action
  class_properties(GraspingAct, ay:preCondition, PreCond),
  eval_expression(PreCond, GraspingAct, State, isRValue, Result).

% Test of applying moving effect to a state.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_move_effect(State, NextState) :-
  t1_apply_move_effect_1(State, NextState, ay:'T1Obj1', ay:'T1Pose_B').
t1_apply_move_effect_1(State, NextState, Object, To) :-
  % NOTE: This part can be common in all effects:
  % Prepare a state at next time
  create_state_from(State, NextState, _),

  % NOTE: This part can be common in precond/effect:
  % Prepare an individual moving-task and action classes
  create_subclass(MovingTask, ay:'T1MovingTask'),
  rdf_assert(MovingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(MovingTask, ay:object, Object),
  create_property_some(MovingTask, ay:to, To),
  create_subclass(MovingAct, ay:'T1MovingAction'),
  rdf_assert(MovingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(MovingAct, ay:task, MovingTask),

  % NOTE: This part could be common in all effects:
  % Perform the effect of the action
  class_properties(MovingAct, ay:effect, Effect),
  eval_expression(Effect, MovingAct, NextState, isRValue, _).

% Test of moving precondition.
t1_test_move_precond(State, Result) :-
  t1_test_move_precond_1(State, ay:'T1Obj1', ay:'T1Pose_B', Result).
t1_test_move_precond_1(State, Object, To, Result) :-
  % NOTE: This part can be common in precond/effect:
  % Prepare an individual moving-task and action classes
  create_subclass(MovingTask, ay:'T1MovingTask'),
  rdf_assert(MovingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(MovingTask, ay:object, Object),
  create_property_some(MovingTask, ay:to, To),
  create_subclass(MovingAct, ay:'T1MovingAction'),
  rdf_assert(MovingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(MovingAct, ay:task, MovingTask),

  % NOTE: This part could be common in all preconditions:
  % Test the precondition of the action
  class_properties(MovingAct, ay:preCondition, PreCond),
  eval_expression(PreCond, MovingAct, State, isRValue, Result).


% Test of applying releasing effect to a state.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_release_effect(State, NextState) :-
  t1_apply_release_effect_1(State, NextState, ay:'T1Obj1').
t1_apply_release_effect_1(State, NextState, Object) :-
  % NOTE: This part can be common in all effects:
  % Prepare a state at next time
  create_state_from(State, NextState, _),

  % NOTE: This part can be common in precond/effect:
  % Prepare an individual releasing-task and action classes
  create_subclass(ReleasingTask, ay:'T1ReleasingTask'),
  rdf_assert(ReleasingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(ReleasingTask, ay:object, Object),
  create_subclass(ReleasingAct, ay:'T1ReleasingAction'),
  rdf_assert(ReleasingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(ReleasingAct, ay:task, ReleasingTask),

  % NOTE: This part could be common in all effects:
  % Perform the effect of the action
  class_properties(ReleasingAct, ay:effect, Effect),
  eval_expression(Effect, ReleasingAct, NextState, isRValue, _).

% Test of releasing precondition.
t1_test_release_precond(State, Result) :-
  t1_test_release_precond_1(State, ay:'T1Obj1', Result).
t1_test_release_precond_1(State, Object, Result) :-
  % NOTE: This part can be common in precond/effect:
  % Prepare an individual releasing-task and action classes
  create_subclass(ReleasingTask, ay:'T1ReleasingTask'),
  rdf_assert(ReleasingTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(ReleasingTask, ay:object, Object),
  create_subclass(ReleasingAct, ay:'T1ReleasingAction'),
  rdf_assert(ReleasingAct, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(ReleasingAct, ay:task, ReleasingTask),

  % NOTE: This part could be common in all preconditions:
  % Test the precondition of the action
  class_properties(ReleasingAct, ay:preCondition, PreCond),
  eval_expression(PreCond, ReleasingAct, State, isRValue, Result).


% Test of pick-and-place goal.
t1_test_pickandplace_goal(State, Result) :-
  t1_test_pickandplace_goal_1(State, ay:'T1Obj1', ay:'T1Pose_B', Result).
t1_test_pickandplace_goal_1(State, Object, To, Result) :-
  % NOTE: This part can be common in goal:
  % Prepare an individual task class
  create_subclass(PickAndPlaceTask, ay:'T1PickAndPlaceTask'),
  rdf_assert(PickAndPlaceTask, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(PickAndPlaceTask, ay:object, Object),
  create_property_some(PickAndPlaceTask, ay:to, To),

  % NOTE: This part could be common in all goals:
  % Test the goal of the task
  class_properties(PickAndPlaceTask, ay:goal, Goal),
  eval_expression(Goal, PickAndPlaceTask, State, isRValue, Result).


% Generate LActedOn a list of [Prop,Value] pairs that are needed to
% instantiate (execute) ActionClass.
% LActedOn takes a form: [[Prop,Value],[Prop,Value],...]
% where Prop is a subclass of ay:actedOn held by ActionClass
%   as qualified cardinality restriction,
% let ValueClass denote the corresponding restriction class of Prop,
% Value is a subclass of ValueClass and Value is in StateIndiv.thing.
% Try:
% ?- list_acted_on(ay:'T1State-0', ay:'T1GraspingAction', LActedOn).
list_acted_on(StateIndiv, ActionClass, LActedOn) :-
  %% Ver.1
  %% Get a list of [Prop,ValueClass].
  %% To understand this algorithm, try:
  %% findall([P,V],(class_properties_cardinality(ay:'T1GraspingAction',P,V,C,S), rdfs_subproperty_of(P,ay:actedOn)),L).
  %findall([Prop,ValueClass],
      %( class_properties_cardinality(ActionClass,Prop,ValueClass,_,_),
        %rdfs_subproperty_of(Prop,ay:actedOn) ),
      %LActedOnClass),
  %% Convert a list of [Prop,ValueClass] to
  %% [Prop,Value1,Value2,...].
  %find_acted_on_from_state(StateIndiv, LActedOnClass, LActedOnAll),

  % Ver.2: Equivalent to Ver.1 but less number of codes.
  % Get a list of [Prop,Value1,Value2,...].
  % To understand this algorithm, try:
  %   findall([P|VV],(class_properties_cardinality(ay:'T1GraspingAction',P,V,C,S), rdfs_subproperty_of(P,ay:actedOn), findall(Value,(owl_subclass_of(Value,V),class_properties(ay:'T1State-0',ay:thing,Value)),VV)),L).
  findall([Prop|Values],
      ( class_properties_cardinality(ActionClass,Prop,ValueClass,_,_),
        rdfs_subproperty_of(Prop,ay:actedOn),
        findall(Value,
            ( class_properties2(StateIndiv,ay:thing,Value),
              owl_subclass_of(Value,ValueClass) ),
            Values) ),
      LActedOnAll),

  % Combination generator:
  combination_generator(LActedOnAll, [], LActedOn).

%% Convert a list of [Prop,ValueClass] to
%% a list of [Prop,Value1,Value2,...]
%% used in list_acted_on.
%find_acted_on_from_state(StateIndiv, LActedOnClass, LActedOnAll):-
  %find_acted_on_from_state_1(StateIndiv, LActedOnClass, [], Tmp),
  %reverse(Tmp, LActedOnAll), !.
%find_acted_on_from_state_1(StateIndiv, [], LActedOnAll, LActedOnAll):- !.
%find_acted_on_from_state_1(StateIndiv, [[Prop,ValueClass]|Rest], Tmp, LActedOnAll):-
  %findall(Value,
      %( class_properties2(StateIndiv,ay:thing,Value),
        %owl_subclass_of(Value,ValueClass) ),
      %Values),
  %find_acted_on_from_state_1(StateIndiv, Rest, [[Prop|Values]|Tmp], LActedOnAll).

% Generate all combinations.
% Assume we have a knowledge like:
%   KB=[[a,1,2,3,4], [b,5,6,7], [c,8,9]]
% which means "a" takes one of [1,2,3,4], "b" takes one of [5,6,7], ...
% Goal is generate all combinations like
%   [[a,1],[b,5],[c,8]], [[a,1],[b,5],[c,9]], ...
% Try
% ?- combination_generator([[a,1,2,3,4], [b,5,6,7], [c,8,9]], [], C).
combination_generator([], C, C) :- !.
combination_generator(KB, C, CC) :-
  [First|KBRest]=KB,
  [A|L]=First,
  member(M,L),
  combination_generator(KBRest, [[A,M]|C], CC).


% For a reasoning purpose, this function generates all selectable actions at a state.
% StateIndiv: An individual state.
% ActionIndiv: Generated individual action.
% WARNING:TODO:FIXME:
% This function create new classes (task, action).
% If they are not in use, they should be removed.
% How to automate this?
% Let the created classes be subclasses of TemporaryObject.
selectable_action_at_state(StateIndiv, ActionIndiv) :-
  % What we want to do here is a general version of:
  % create_subclass(GraspingTask, ay:'T1GraspingTask'),
  % rdf_assert(GraspingTask, rdfs:subClassOf, ay:'IndividualClass'),
  % create_property_some(GraspingTask, ay:object, Object),
  % create_property_some(GraspingTask, ay:hand, Hand),
  % create_subclass(GraspingAct, ay:'T1GraspingAction'),
  % rdf_assert(GraspingAct, rdfs:subClassOf, ay:'IndividualClass'),
  % create_property_some(GraspingAct, ay:task, GraspingTask),

  % Strategy:
  % For each action A(ActionClass) that is a subclass of GeneralAction,
  %   // A should not be an individual class
  %   // A should be executable, should have an effect model
  %   Create a new A: A-1
  %   For ALL acted-on O in A-1:
  %     Get a range class OC of O using class_properties_cardinality
  %     For each SOMETHING S that is a subclass of OC:
  %       // S should exist in state.thing
  %       Assign: A-1.O= S

  generalAction(GeneralAction),

  % TODO:FIXME: We can store some results.

  % Find an ActionClass
  rdfs_subclass_of(ActionClass, GeneralAction),
  \+ is_individual(ActionClass),
  owl_has_property(ActionClass, ay:effect),

  % Generate LActedOn a list of [Prop,Value] pairs that are needed to
  % instantiate (execute) ActionClass.
  list_acted_on(StateIndiv, ActionClass, LActedOn),

  % Get a corresponding task class:
  class_properties_cardinality(ActionClass, ay:task, TaskClass, _, S),
  \+ rdf_global_term(GeneralAction,S),
  % Create an individual task.
  create_subclass(TaskIndiv, TaskClass),
  rdf_assert(TaskIndiv, rdfs:subClassOf, ay:'IndividualClass'),
  % Create an individual action.
  create_subclass(ActionIndiv, ActionClass),
  rdf_assert(ActionIndiv, rdfs:subClassOf, ay:'IndividualClass'),
  create_property_some(ActionIndiv, ay:task, TaskIndiv),

  % Assign LActedOn to TaskClass:
  forall(member([Prop,Value], LActedOn),(
    % Note: we assign an individual class of Value which is specified by
    % ay:specificationOf property.
    % E.g. T1Obj1-0 is translated to T1Obj1.
    (class_properties2(Value, ay:specificationOf, ValueIndiv)
      -> Value2=ValueIndiv ; Value2=Value ),
    create_property_some(TaskIndiv, Prop, Value2)
  )).


% A general search method.
% TODO:NOTE: This is a depth-first search.
% Maybe breadth-first search is preferable to me?
% Or implement A* search.

% Find a sequence of actions starting from Start state
% with which the Task is satisfied.
% This is a depth-first search.
find_action_seq_df(StartState, Task, ActionSeq) :-
  find_action_seq_df_1(StartState, Task, [StartState], Q),
  reverse(Q, ActionSeq).                                             %ActionSeq is left-to-right
  %find_action_seq_df_1(StartState, Task, [StartState], ActionSeq).  %ActionSeq is right-to-left

find_action_seq_df_1(State, Task, PastSeq, ActionSeq) :-
  % Check if Task.goal is satisfied:
  % cf: t1_test_pickandplace_goal(State, IsGoal), IsGoal=xsd:true,
  class_properties2(Task, ay:goal, Goal),
  eval_expression(Goal, Task, State, isRValue, IsGoal),
  IsGoal=xsd:true,
  ActionSeq=[goal|PastSeq],
  !.
find_action_seq_df_1(State, Task, PastSeq, ActionSeq) :-
  % FIXME:TODO: Stupid loop check (see below TODO for a better loop check):
  length(PastSeq,Len),Len<15,

  % Select an action Action:
  selectable_action_at_state(State, Action),

  % Check if Action.preCondition is satisfied:
  % cf. t1_test_move_precond(State, PreCondSatisfied), PreCondSatisfied=xsd:true,
  class_properties2(Action, ay:preCondition, PreCond),
  eval_expression(PreCond, Action, State, isRValue, PreCondSatisfied),
  PreCondSatisfied=xsd:true,

  % Propagate State by mentally executing Action:
  % cf. t1_apply_grasp_effect3(State, NextState).
  % Prepare a state at next time
  create_state_from(State, NextState, _),
  % Perform the effect of the action
  class_properties2(Action, ay:effect, Effect),
  eval_expression(Effect, Action, NextState, isRValue, _),

  %% TODO:FIXME: (This will never happen with the current implementation
  %% because we always create a new state.
  %% We need a function to check if two states are equivalent.)
  %% Check if NextState is in PastSeq to detect a loop
  %(\+member(NextState, PastSeq)
    %->
      %find_action_seq_df_1(NextState, Task, [NextState,Action|PastSeq], ActionSeq)
    %;
      %ActionSeq= [loop,NextState,Action|PastSeq]
    %).
writef("DEBUG-%t\n",[[NextState,Action|PastSeq]]),
  find_action_seq_df_1(NextState, Task, [NextState,Action|PastSeq], ActionSeq).


% Find a sequence of actions starting from Start state
% with which the Task is satisfied.
% This is a breadth-first search.
find_action_seq_bf(StartState, Task, ActionSeq) :-
  find_action_seq_bf_1(Task, [[StartState]], [], Q),
  reverse(Q, ActionSeq).                                       %ActionSeq is left-to-right
  %find_action_seq_bf_1(Task, [[StartState]], [], ActionSeq).  %ActionSeq is right-to-left

% Core of breadth-first search.
% Modified so that we list-up all possible solutions (not only shortest) including loops.
% We return state-action sequence.
find_action_seq_bf_1(Task, [StActSeq|_], _, Result) :-
%find_action_seq_bf_1(Task, [StActSeq|RestOPaths], _, Result) :-
%writef('*-DEBUG:OPaths: %t\n',[[StActSeq|RestOPaths]]),
  [State|_]= StActSeq,
  % Check if Task.goal is satisfied:
  % cf: t1_test_pickandplace_goal(State, IsGoal), IsGoal=xsd:true,
  class_properties2(Task, ay:goal, Goal),
  eval_expression(Goal, Task, State, isRValue, IsGoal),
  IsGoal=xsd:true,
  Result=[goal|StActSeq].
find_action_seq_bf_1(_, [[loop|StActSeq]|_], _, Result) :-
%find_action_seq_bf_1(Task, [[loop|StActSeq]|RestOPaths], _, Result) :-
%writef('*-DEBUG:OPaths: %t\n',[[[loop|StActSeq]|RestOPaths]]),
  Result=[loop|StActSeq].
find_action_seq_bf_1(Task, [StActSeq|RestOPaths], Closed, Result) :-
  [State|_]= StActSeq,
  %length(StActSeq, L),
%length(StActSeq, L), format('Depth: ~q~n',[L]),
%writef('1-DEBUG:Closed: %t\n',[Closed]),
%writef('2-DEBUG:OPaths: %t\n',[[StActSeq|RestOPaths]]),
  findall([NextState,Action|StActSeq],
      (
        % Select an action Action:
        selectable_action_at_state(State, Action),

        % Check if Action.preCondition is satisfied:
        % cf. t1_test_move_precond(State, PreCondSatisfied), PreCondSatisfied=xsd:true,
        class_properties2(Action, ay:preCondition, PreCond),
        eval_expression(PreCond, Action, State, isRValue, PreCondSatisfied),
        PreCondSatisfied=xsd:true,

        % Propagate State by mentally executing Action:
        % cf. t1_apply_grasp_effect3(State, NextState).
        % Prepare a state at next time
        create_state_from(State, NextState, _),
        % Perform the effect of the action
        class_properties2(Action, ay:effect, Effect),
        eval_expression(Effect, Action, NextState, isRValue, _)
        ),
      PossibleOPaths),
  % FIXME:TODO:WARNING:
  % member1(NextState, Closed) does not work as there is no implementation to
  % test if two states are equivalent.
  findall([NextState,Action|StActSeq],
      ( member([NextState,Action|StActSeq], PossibleOPaths),
        %\+ (member1(NextState,ActSeq2,RestOPaths), length(ActSeq2,L)),  % Condition for shortest?
        \+ member1(NextState, Closed) ),  % Condition to remove loop <-- TODO: KEEP THIS.
      NextOPaths),
  findall([loop,NextState,Action|StActSeq],
      ( member([NextState,Action|StActSeq], PossibleOPaths),
        member1(NextState, Closed) ),
      LoopPaths),
%writef('2.2-DEBUG: %t\n',[PossibleOPaths]),
%writef('2.5-DEBUG: %t :: %t\n',[NextOPaths,LoopPaths]),
  append(RestOPaths, LoopPaths, Tmp),
  append(Tmp, NextOPaths, OPaths),
%writef('3-DEBUG:OPaths: %t\n',[OPaths]),
  find_action_seq_bf_1(Task, OPaths, [State|Closed], Result).


%%eval_expression(Expr, Context, State, ValueType, Result)
% Compute an expression.
% Expr: Subclass of ay:'Expression'.
% Context: Context class.
% State: State class which stores current state and time.
% ValueType: Expression context flag which can be isLValue or isRValue.
% Result: Result.

% When Expr is not an ay:Expression, Result=Expr.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(\+ rdfs_subclass_of(Expr, ay:'Expression')),
  Result= Expr,
  !.
  %FIXME: If ValueType is isLValue, consider to create time instance of Expr.

% Sequence operator to run two expressions.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpL1')),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  eval_expression(Arg1, Context, State, ValueType, Arg1_value),
  % Set arg1 as the result.
  Result= Arg1_value,
  !.

% Assignment operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpAssign'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  class_properties2(Expr, ay:arg2, Arg2),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isLValue, Arg0_subject),
  eval_expression(Arg1, Context, State, isRValue, Arg1_property),
  eval_expression(Arg2, Context, State, isRValue, Arg2_object),
  % Evaluate the operator
  modify_property_some(Arg0_subject, Arg1_property, Arg2_object),
  !.

% Retract-all operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpRetractAll'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isLValue, Arg0_subject),
  eval_expression(Arg1, Context, State, isRValue, Arg1_property),
  % Evaluate the operator
  remove_property(Arg0_subject, Arg1_property),
  !.

% Reference operator.
% WARNING:FIXME:TODO: This class uses ay:'T1TimeVariantThing' which is
% a test class for Toy1.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpRefer')),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, ValueType, Arg0_subject),
  eval_expression(Arg1, Context, State, isRValue, Arg1_property),
  % Evaluate the operator
  %   Reference rules of arg0.arg1:
  %   X=arg0.arg1 is the current value, and Y is the result.
  %   CASE-1: If X is an IndividualClass and X is in State:
  %       If isLValue: Y = find_by_name_and_create_timeinst(X,State,..)
  %       Else:        Y = find_by_name(X,State)
  %   CASE-2: If X is an T1TimeVariantThing and X has specificationOf:
  %       If isLValue: Y = create_timeinst_from(X,..)
  %       Else:        Y = X
  %   CASE-3: otherwise:
  %                    Y = X
  class_properties2(State, ay:time, Time),
  class_properties2(Arg0_subject, Arg1_property, Result0),
  ((is_individual(Result0),
    find_by_name(Result0, State, Result1))
    ->
      %CASE-1
      (ValueType=isLValue
        ->
          find_by_name_and_create_timeinst(Result0, State, Time, Result)
        ;
          Result= Result1
        )
    ;
      ((rdfs_subclass_of(Result0, ay:'T1TimeVariantThing'),
        owl_has_property(Result0, ay:specificationOf))
        ->
          %CASE-2
          (ValueType=isLValue
            ->
              create_timeinst_from(Result, Result0, Time),
              modify_property_some(Arg0_subject, Arg1_property, Result)
            ;
              Result= Result0
            )
        ;
          %CASE-3
          Result= Result0
        )
    ),
  !.

% Create a new subclass operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpNewSubClass')),  %FIXME: Should we put a ValueType constraint?
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_class),
  %eval_expression(Arg1, Context, State, isRValue, Arg1_varclass),
  Arg1_varclass= Arg1,  %We don't expand Arg1
  % FIXME: Check if Arg1_varclass is a subclass of ay:ExprVariable
  % Evaluate the operator
  %class_properties2(Arg1_varclass, ay:id, literal(ID)),
  create_subclass(Arg1_var, Arg1_varclass),
  create_subclass(Content, Arg0_class),
  modify_property_some(Arg1_var, ay:content, Content),
  modify_property_some(State, ay:variable, Arg1_var),
  Result= Content,  % NOTE: If this has a side effect, consider: Result= Arg1_varclass
  !.

% Create a new individual subclass operator.
% Very similar to OpNewSubClass; only the difference is the assertion of IndividualClass.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpNewIndivClass')),  %FIXME: Should we put a ValueType constraint?
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_class),
  %eval_expression(Arg1, Context, State, isRValue, Arg1_varclass),
  Arg1_varclass= Arg1,  %We don't expand Arg1
  % FIXME: Check if Arg1_varclass is a subclass of ay:ExprVariable
  % Evaluate the operator
  %class_properties2(Arg1_varclass, ay:id, literal(ID)),
  create_subclass(Arg1_var, Arg1_varclass),
  create_subclass(Content, Arg0_class),
  rdf_assert(Content, rdfs:subClassOf, ay:'IndividualClass'),
  modify_property_some(Arg1_var, ay:content, Content),
  modify_property_some(State, ay:variable, Arg1_var),
  Result= Content,  % NOTE: If this has a side effect, consider: Result= Arg1_varclass
  !.

% Has-property operator (if arg0 has a arg1 property).
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpHasProperty'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_subject),
  eval_expression(Arg1, Context, State, isRValue, Arg1_property),
  % Evaluate the operator
  (owl_has_property(Arg0_subject, Arg1_property)
    -> Result= xsd:true ; Result= xsd:false ),
  !.

% Equality operator (if arg0 = arg1).
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpEqual'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  eval_expression(Arg1, Context, State, isRValue, Arg1_value),
  % Evaluate the operator
  ((Arg0_value = Arg1_value)
    -> Result= xsd:true ; Result= xsd:false ),
  !.

% Logical not operator (not arg0).
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpNot'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  % Evaluate the operator
  (Arg0_value -> Result= xsd:false ; Result= xsd:true).

% Logical and operator (arg0 and arg1).
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpAnd'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  eval_expression(Arg1, Context, State, isRValue, Arg1_value),
  % Evaluate the operator
  ((Arg0_value, Arg1_value)
    -> Result= xsd:true ; Result= xsd:false ),
  !.

% Logical or operator (arg0 or arg1).
eval_expression(Expr, Context, State, ValueType, Result) :-
  once((rdfs_subclass_of(Expr, ay:'OpOr'), ValueType=isRValue)),
  % Read argument(s)
  class_properties2(Expr, ay:arg0, Arg0),
  class_properties2(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  eval_expression(Arg1, Context, State, isRValue, Arg1_value),
  % Evaluate the operator
  ((Arg0_value; Arg1_value)
    -> Result= xsd:true ; Result= xsd:false ),
  !.

% Variable access.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'ExprVariable')),
  % Get identifier of the variable:
  class_properties2(Expr, ay:id, ID),
  ( class_properties2(State, ay:variable, X),  %FIXME: write an exception code for when X doesn't exist.
    class_properties2(X, ay:id, ID) ),
  class_properties2(X, ay:content, Result),
  !.

% Context-class access.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'ExprContext')),
  Result= Context,
  !.


