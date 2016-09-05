-file("c++.capnp", 1).

%% This file was generated 2016-09-05 08:37:26 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('c++_capnp').

-vsn(13688829037717245569).

-export([schema/1, namespace/0, namespace/1, '13386661402618388268'/0, name/0, name/1,
	 '17466269397259751886'/0, root/0, root/1, '13688829037717245569'/0]).

-types([{13386661402618388268, namespace}, {17466269397259751886, name},
	{13688829037717245569, root}]).

-file("/Users/aadt/lib/erl/global/ecapnp/include/ecapnp_schema.hrl", 1).

-ecapnp_schema_version(4).

-record(schema_node,
	{module  :: atom(), name  :: ecapnp:type_name(), id = 0  :: ecapnp:type_id(),
	 src = <<>>  :: ecapnp:text(), kind = file  :: ecapnp:schema_kind(), annotations = []  :: list(),
	 nodes = []  :: ecapnp:schema_nodes(), scope = 0  :: ecapnp:type_id()}).

-record(struct,
	{dsize = 0  :: ecapnp:word_count(), psize = 0  :: ecapnp:ptr_count(),
	 esize = inlineComposite  :: ecapnp:element_size(),
	 union_field = none  :: none | ecapnp:field_type(), fields = []  :: ecapnp:struct_fields()}).

-record(enum, {values = []  :: ecapnp:enum_values()}).

-record(interface, {extends = []  :: list(), methods = []  :: list()}).

-record(const, {field}).

-record(annotation, {type, targets = []  :: [atom()]}).

-record(field, {id, name, kind, annotations = []}).

-record(ptr,
	{type  :: term(), idx = 0  :: ecapnp:ptr_index(),
	 default = <<0:64/integer-little>>  :: ecapnp:value()}).

-record(data, {type  :: term(), align = 0  :: ecapnp:bit_count(), default  :: ecapnp:value()}).

-record(group, {id = 0  :: ecapnp:type_id()}).

-record(method, {id, name, paramType, resultType}).

-file("c++.capnp", 1).

schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
schema(17466269397259751886) -> '17466269397259751886'();
schema(name) -> '17466269397259751886'();
schema([name]) -> '17466269397259751886'();
schema(13688829037717245569) -> '13688829037717245569'();
schema(root) -> '13688829037717245569'();
schema([root]) -> '13688829037717245569'();
schema(_) -> undefined.

root() -> '13688829037717245569'().

root([]) -> '13688829037717245569'().

'13688829037717245569'() ->
    #schema_node{module = 'c++_capnp', name = root, id = 13688829037717245569, scope = 0,
		 src = <<"c++.capnp">>, annotations = [{13386661402618388268, <<"capnp::annotations">>}],
		 kind = file,
		 nodes =
		     [13386661402618388268,  %% namespace
		      17466269397259751886]}.  %% name

name() -> '17466269397259751886'().

name([]) -> '17466269397259751886'().

'17466269397259751886'() ->
    #schema_node{module = 'c++_capnp', name = name, id = 17466269397259751886,
		 scope = 13688829037717245569, src = <<"c++.capnp:name">>,
		 kind =
		     #annotation{type = #ptr{type = text, idx = 0, default = <<>>},
				 targets =
				     [targetsEnum, targetsEnumerant, targetsStruct, targetsField, targetsUnion, targetsGroup,
				      targetsInterface, targetsMethod, targetsParam]}}.

namespace() -> '13386661402618388268'().

namespace([]) -> '13386661402618388268'().

'13386661402618388268'() ->
    #schema_node{module = 'c++_capnp', name = namespace, id = 13386661402618388268,
		 scope = 13688829037717245569, src = <<"c++.capnp:namespace">>,
		 kind = #annotation{type = #ptr{type = text, idx = 0, default = <<>>}, targets = [targetsFile]}}.