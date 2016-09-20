%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dependencies

:- register_ros_package(knowrob_common).
:- register_ros_package(knowrob_actions).
:- register_ros_package(knowrob_ay).

:- use_module(library('ay_util')).
:- use_module(library('ay_test')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse OWL files, register name spaces

:- owl_parser:owl_parse('package://knowrob_ay/owl/ay.owl').
:- rdf_db:rdf_register_ns(ay, 'http://knowrob.org/kb/ay.owl#', [keep(true)]).
