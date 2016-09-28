:- module(ay_test,
    [
      class_properties2/3,
      create_subclass/2,
      rdf_unique_id2/3,
      create_restriction/5,
      create_property_some/4,
      create_property_value/4,
      modify_restriction/5,
      modify_property_some/3,
      modify_property_some/4,
      modify_property_value/3,
      modify_property_value/4,
      specification_class_of/2,
      t1_copy_state/2,
      copy_properties/2,
      remove_object/2,
      find_by_name/3,
      find_by_name_and_create_fluent/4,
      create_fluent_from/3,
      t1_apply_grasp_effect/2,
      t1_apply_grasp_effect2/2,
      t1_apply_grasp_effect3/2,
      eval_expression/5
    ]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('knowrob_owl')).

:- rdf_meta
  class_properties2(r,r,r),
  create_subclass(r,r),
  rdf_unique_id2(r,r,r),
  create_restriction(r,r,r,r,r),
  create_property_some(r,r,r,r),
  create_property_value(r,r,r,r),
  modify_restriction(r,r,r,r,r),
  modify_property_some(r,r,r),
  modify_property_some(r,r,r,r),
  modify_property_value(r,r,r),
  modify_property_value(r,r,r,r),
  specification_class_of(r,r),
  t1_copy_state(r,r),
  copy_state_1(r,r,r,r),
  copy_object(r,r),
  copy_properties(r,r),
  remove_object(r,r),
  find_by_name(r,r,r),
  find_by_name_and_create_fluent(r,r,r,r),
  create_fluent_from(r,r,r),
  t1_apply_grasp_effect(r,r),
  t1_apply_grasp_effect_1(r,r,r),
  t1_apply_grasp_effect2(r,r),
  t1_apply_grasp_effect2_1(r,r,r,r),
  t1_apply_grasp_effect2_2(r,r),
  t1_apply_grasp_effect3(r,r),
  t1_apply_grasp_effect3_1(r,r,r,r),
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

% Based on rdf_unique_id in knowrob_owl.pl
rdf_unique_id2(Class, UniqID, Delim) :-
  % Generate a random string Sub
  append("$1$", _, Seed),
  crypt(Class, Seed),
  format(atom(Hash), '~s~n', [Seed]),
  sub_atom(Hash, 3, 8, _, Sub),
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
  knowrob_owl:rdf_unique_id2(SuperClass, New, '-'),
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
% Note: If Class.Prop does not exist, we create it.
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
% Note: If Class.Prop does not exist, we create it.
modify_property_some(Class, Prop, Value) :-
  modify_property_some(Class, Prop, Value, _).
modify_property_some(Class, Prop, Value, New) :-
  modify_restriction(Class, Prop, Value, owl:someValuesFrom, New).

% Modify a property as the owl:hasValue restriction.
% Note: If Class.Prop does not exist, we create it.
modify_property_value(Class, Prop, Value) :-
  modify_property_value(Class, Prop, Value, _).
modify_property_value(Class, Prop, Value, New) :-
  modify_restriction(Class, Prop, Value, owl:hasValue, New).


% Find IndivClass whose specification classes include SpecClass.
specification_class_of(SpecClass, IndivClass) :-
  class_properties(SpecClass, ay:specificationOf, IndivClass).

% copy_state specialized to Toy1.
t1_copy_state(State, CopiedState) :-
  copy_state_1(State, CopiedState, ay:'T1State', ay:'T1Time').

copy_state_1(State, CopiedState, StateClass, TimeClass) :-
  create_subclass(NewTime, TimeClass),
  create_subclass(CopiedState, StateClass),
  create_property_some(CopiedState, ay:time, NewTime, _),
  % for all Obj = State.ay:object
  forall(class_properties(State, ay:object, Obj),(
      % Copy Obj to NewObj...?
      % copy_object(Obj, NewObj),
      % create_property_some(NewObj, ay:time, NewTime, _),
      % create_property_some(CopiedState, ay:object, NewObj, _)
      % TEST: Just copy the same object (Maybe this is okay)
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
  create_subclass(NewObject, IndivClass),
  % Copy all properties of Object
  % TODO:
  true.

copy_properties(Src, Dst) :-
  forall(class_properties(Src, Prop, Obj),(
    %FIXME: TODO: We should consider when Prop is value type or all type???
    create_property_some(Dst, Prop, Obj, _)
  )).


% Remove Object from State.object.
% Hint: Use together with find_by_name to remove by an individual class.
remove_object(State, Object) :-
  rdf_has(State, rdfs:subClassOf, ObjRestr),
  rdf_has(ObjRestr, owl:someValuesFrom, Object),
  rdf_retractall(ObjRestr, P, O),
  rdf_retractall(State, rdfs:subClassOf, ObjRestr).

% Find ObjInState from State.object that is an individual class ObjIndiv.
find_by_name(ObjIndiv, State, ObjInState) :-
  class_properties(State, ay:object, ObjInState),
  rdfs_subclass_of(ObjInState, ObjIndiv).

% Find S from State.object that is a specification of individual class ObjIndiv.
% If S.time is not Time, we create a new state NewObjInState and
% replace S in State.object by NewObjInState.
% If s.time is equal to Time, then NewObjInState=S.
find_by_name_and_create_fluent(ObjIndiv, State, Time, NewObjInState) :-
  find_by_name(ObjIndiv, State, ObjInState),
  (class_properties(ObjInState, ay:time, Time)
    ->
      NewObjInState= ObjInState
    ;
      create_subclass(NewObjInState, ObjIndiv),
      % Copy properties:
      copy_properties(ObjInState, NewObjInState),
      % Set time:
      modify_property_some(NewObjInState, ay:time, Time),
      % Remove the old one from State.object:
      remove_object(State, ObjInState),
      % Add the new one to State.object:
      create_property_some(State, ay:object, NewObjInState, _)
    ).

% Create a new fluent NewObj from SrcObj.
% If SrcObj.time is not Time, create a new specification class NewObj
% from an individual class SrcObj.specificationOf,
% and copy all parameters from SrcObj to NewObj.
% NewObj.time is set to be Time.
% If SrcObj.time is not Time, then NewObj=SrcObj.
create_fluent_from(NewObj, SrcObj, Time) :-
  (class_properties(SrcObj, ay:time, Time)
    ->
      NewObj= SrcObj
    ;
      class_properties(SrcObj, ay:specificationOf, ObjIndiv),
      create_subclass(NewObj, ObjIndiv),
      % Copy properties:
      copy_properties(SrcObj, NewObj),
      % Set time:
      modify_property_some(NewObj, ay:time, Time)
    ).

% Test of applying grasping effect to a state.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_grasp_effect(State, NextState) :-
  t1_apply_grasp_effect_1(State, NextState, ay:'T1Obj1').
t1_apply_grasp_effect_1(State, NextState, Object) :-
  % Prepare a state at next time
  t1_copy_state(State, NextState),
  create_subclass(NextTime, ay:'T1Time'),
  modify_property_some(NextState, ay:time, NextTime),

  % Grasped1= new ay:'T1Grasped'
  % Grasped1.hand= ay:'T1Robot1'
  create_subclass(Grasped, ay:'T1Grasped'),
  rdf_assert(Grasped, rdfs:subClassOf, ay:'T1TimeVariantThing'),
  create_property_some(Grasped, ay:hand, ay:'T1Robot1', _),
  create_property_some(Grasped, ay:time, NextTime, _),

  % NextState.object[Object].grasped= Grasped1
  find_by_name_and_create_fluent(Object, NextState, NextTime, ObjInState),

  modify_property_some(ObjInState, ay:t1grasped, Grasped).


% Test of applying grasping effect to a state version 2.
% Trying to write in a more general form.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_grasp_effect2(State, NextState) :-
  t1_apply_grasp_effect2_1(State, NextState, ay:'T1Obj1', ay:'T1Robot1').
t1_apply_grasp_effect2_1(State, NextState, Object, Hand) :-
  % Prepare a state at next time
  t1_copy_state(State, NextState),
  create_subclass(NextTime, ay:'T1Time'),
  modify_property_some(NextState, ay:time, NextTime),

  % Prepare an individual grasping-task and action classes
  create_subclass(GraspingTask, ay:'T1GraspingTask'),
  create_property_some(GraspingTask, ay:object, Object, _),
  create_property_some(GraspingTask, ay:hand, Hand, _),
  create_subclass(GraspingAct, ay:'T1GraspingAction'),
  create_property_some(GraspingAct, ay:task, GraspingTask, _),

  % Perform the effect of the action
  t1_apply_grasp_effect2_2(NextState, GraspingAct).

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
  rdf_assert(Grasped, rdfs:subClassOf, ay:'T1TimeVariantThing'),
  create_property_some(Grasped, ay:hand, Hand, _),
  create_property_some(Grasped, ay:time, Time, _),

  % State.object[Object].grasped= Grasped1
  find_by_name_and_create_fluent(Object, State, Time, ObjInState),

  modify_property_some(ObjInState, ay:t1grasped, Grasped).



% Test of applying grasping effect to a state version 3.
% Trying to write in a more general form:
%   Action.effect is loaded from OWL.
% FIXME: When this process fails, some garbage remain. Remove them.
t1_apply_grasp_effect3(State, NextState) :-
  t1_apply_grasp_effect3_1(State, NextState, ay:'T1Obj1', ay:'T1Robot1').
t1_apply_grasp_effect3_1(State, NextState, Object, Hand) :-
  % Prepare a state at next time
  t1_copy_state(State, NextState),
  create_subclass(NextTime, ay:'T1Time'),
  modify_property_some(NextState, ay:time, NextTime),

  % Prepare an individual grasping-task and action classes
  create_subclass(GraspingTask, ay:'T1GraspingTask'),
  create_property_some(GraspingTask, ay:object, Object, _),
  create_property_some(GraspingTask, ay:hand, Hand, _),
  create_subclass(GraspingAct, ay:'T1GraspingAction'),
  create_property_some(GraspingAct, ay:task, GraspingTask, _),

  % Perform the effect of the action
  class_properties(GraspingAct, ay:effect, Effect),
  eval_expression(Effect, GraspingAct, NextState, isRValue, _).


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
  Result= Expr.
  %FIXME: If ValueType is isLValue, consider to create fluent of Expr.

% Sequence operator to run two expressions.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpL1')),
  % Read argument(s)
  class_properties(Expr, ay:arg0, Arg0),
  class_properties(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  eval_expression(Arg1, Context, State, isLValue, Arg1_value),
  % Set arg1 as the result.
  Result= Arg1_value.

% Not operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpNot')),
  % Read argument(s)
  class_properties(Expr, ay:arg0, Arg0),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_value),
  % Evaluate the operator
  (Arg0_value -> Result=xsd:false ; Result=xsd:true).

% Assignment operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpAssign')),
  % Read argument(s)
  class_properties(Expr, ay:arg0, Arg0),
  class_properties(Expr, ay:arg1, Arg1),
  class_properties(Expr, ay:arg2, Arg2),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isLValue, Arg0_subject),
  eval_expression(Arg1, Context, State, isRValue, Arg1_property),
  eval_expression(Arg2, Context, State, isRValue, Arg2_object),
  % Evaluate the operator
  modify_property_some(Arg0_subject, Arg1_property, Arg2_object).

% Reference operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpRefer')),
  % Read argument(s)
  class_properties(Expr, ay:arg0, Arg0),
  class_properties(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isLValue, Arg0_subject),
  eval_expression(Arg1, Context, State, isRValue, Arg1_property),
  % Evaluate the operator
  class_properties(State, ay:time, Time),
  class_properties(Arg0_subject, Arg1_property, Result0),
  ((rdfs_subclass_of(Result0, ay:'IndividualClass'),
    find_by_name(Result0, State, Result1))
    ->
      %CASE-1
      (ValueType=isLValue
        ->
          find_by_name_and_create_fluent(Result0, State, Time, Result)
        ;
          Result= Result1
        )
    ;
      ((rdfs_subclass_of(Result0, ay:'T1TimeVariantThing'),
        class_properties(Result0, ay:specificationOf, _))
        ->
          %CASE-2
          (ValueType=isLValue
            ->
              create_fluent_from(Result, Result0, Time),
              modify_property_some(Arg0_subject, Arg1_property, Result)
            ;
              Result= Result0
            )
        ;
          %CASE-3
          Result= Result0
        )
    ).

% Create a new subclass operator.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'OpNewSubClass')),
  % Read argument(s)
  class_properties(Expr, ay:arg0, Arg0),
  class_properties(Expr, ay:arg1, Arg1),
  % Expand argument(s)
  eval_expression(Arg0, Context, State, isRValue, Arg0_class),
  %eval_expression(Arg1, Context, State, isLValue, Arg1_varclass),
  Arg1_varclass= Arg1,  %We don't expand Arg1
  % FIXME: Check if Arg1_varclass is a subclass of ay:ExprVariable
  % Evaluate the operator
  %class_properties(Arg1_varclass, ay:id, literal(ID)),
  create_subclass(Arg1_var, Arg1_varclass),
  create_subclass(Content, Arg0_class),
  modify_property_some(Arg1_var, ay:content, Content),
  modify_property_some(State, ay:variable, Arg1_var),
  Result= Content.  % NOTE: If this has a side effect, consider: Result= Arg1_varclass

% Variable access.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'ExprVariable')),
  % Get identifier of the variable:
  class_properties(Expr, ay:id, ID),
  ( class_properties(State, ay:variable, X),  %FIXME: write an exception code for when X doesn't exist.
    class_properties(X, ay:id, ID) ),
  class_properties(X, ay:content, Result).

% Context-class access.
eval_expression(Expr, Context, State, ValueType, Result) :-
  once(rdfs_subclass_of(Expr, ay:'ExprContext')),
  Result= Context.



?- rdfs_subclass_of(C, ay:'Expression').
C = ay:'Expression' ;
C = ay:'BinaryOperator' ;
C = ay:'UnaryOperator' ;
%C = ay:'OpAssign' ;
C = ay:'OpHasProperty' ;
%C = ay:'OpL1' ;
%C = ay:'OpNewSubClass' ;
%C = ay:'OpRefer' ;
%C = ay:'OpNot' ;
%C = ay:'ExprContext' ;
%C = ay:'ExprVariable' ;
C = ay:'ExprVarA' ;
C = ay:'ExprVarB' ;
C = ay:'ExprVarC' ;
C = ay:'ExprVarD' ;
C = ay:'ExprVarE' ;

