-file("c++.capnp", 1).

%% This file was generated 2014-05-31 07:56:28 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('c++_capnp').

-vsn(13688829037717245569).

-export([schema/1, namespace/0, namespace/1, '13386661402618388268'/0, root/0, root/1,
	 '13688829037717245569'/0]).

-types([{13386661402618388268, namespace}, {13688829037717245569, root}]).

-file("/home/kaos/src/erl/libs/ecapnp/include/ecapnp_schema.hrl", 1).

-ecapnp_schema_version(4).

-record(schema_node,
	{module, name, id = 0, src = <<>>, kind = file, annotations = [], nodes = [], scope = 0}).

-type({{record, schema_node},
       [{typed_record_field, {record_field, 9, {atom, 9, module}},
	 {type, 9, union, [{atom, 9, undefined}, {type, 9, atom, []}]}},
	{typed_record_field, {record_field, 10, {atom, 10, name}},
	 {type, 10, union,
	  [{atom, 10, undefined}, {remote_type, 10, [{atom, 10, ecapnp}, {atom, 10, type_name}, []]}]}},
	{typed_record_field, {record_field, 11, {atom, 11, id}, {integer, 11, 0}},
	 {remote_type, 11, [{atom, 11, ecapnp}, {atom, 11, type_id}, []]}},
	{typed_record_field, {record_field, 12, {atom, 12, src}, {bin, 12, []}},
	 {remote_type, 12, [{atom, 12, ecapnp}, {atom, 12, text}, []]}},
	{typed_record_field, {record_field, 13, {atom, 13, kind}, {atom, 13, file}},
	 {remote_type, 13, [{atom, 13, ecapnp}, {atom, 13, schema_kind}, []]}},
	{typed_record_field, {record_field, 14, {atom, 14, annotations}, {nil, 14}}, {type, 14, list, []}},
	{typed_record_field, {record_field, 15, {atom, 15, nodes}, {nil, 15}},
	 {remote_type, 15, [{atom, 15, ecapnp}, {atom, 15, schema_nodes}, []]}},
	{typed_record_field, {record_field, 16, {atom, 16, scope}, {integer, 16, 0}},
	 {remote_type, 16, [{atom, 16, ecapnp}, {atom, 16, type_id}, []]}}],
       []}).

-record(struct, {dsize = 0, psize = 0, esize = inlineComposite, union_field = none, fields = []}).

-type({{record, struct},
       [{typed_record_field, {record_field, 21, {atom, 21, dsize}, {integer, 21, 0}},
	 {remote_type, 21, [{atom, 21, ecapnp}, {atom, 21, word_count}, []]}},
	{typed_record_field, {record_field, 22, {atom, 22, psize}, {integer, 22, 0}},
	 {remote_type, 22, [{atom, 22, ecapnp}, {atom, 22, ptr_count}, []]}},
	{typed_record_field, {record_field, 23, {atom, 23, esize}, {atom, 23, inlineComposite}},
	 {remote_type, 23, [{atom, 23, ecapnp}, {atom, 23, element_size}, []]}},
	{typed_record_field, {record_field, 24, {atom, 24, union_field}, {atom, 24, none}},
	 {type, 24, union,
	  [{atom, 24, none}, {remote_type, 24, [{atom, 24, ecapnp}, {atom, 24, field_type}, []]}]}},
	{typed_record_field, {record_field, 25, {atom, 25, fields}, {nil, 25}},
	 {remote_type, 25, [{atom, 25, ecapnp}, {atom, 25, struct_fields}, []]}}],
       []}).

-record(enum, {values = []}).

-type({{record, enum},
       [{typed_record_field, {record_field, 30, {atom, 30, values}, {nil, 30}},
	 {remote_type, 30, [{atom, 30, ecapnp}, {atom, 30, enum_values}, []]}}],
       []}).

-record(interface, {extends = [], methods = []}).

-type({{record, interface},
       [{typed_record_field, {record_field, 35, {atom, 35, extends}, {nil, 35}}, {type, 35, list, []}},
	{typed_record_field, {record_field, 36, {atom, 36, methods}, {nil, 36}}, {type, 36, list, []}}],
       []}).

-record(const, {field}).

-record(annotation, {type, targets = []}).

-type({{record, annotation},
       [{record_field, 46, {atom, 46, type}},
	{typed_record_field, {record_field, 47, {atom, 47, targets}, {nil, 47}},
	 {type, 47, list, [{type, 47, atom, []}]}}],
       []}).

-record(field, {id, name, kind, annotations = []}).

-record(ptr, {type, idx = 0, default = <<0:64/integer-little>>}).

-type({{record, ptr},
       [{typed_record_field, {record_field, 60, {atom, 60, type}},
	 {type, 60, union, [{atom, 60, undefined}, {type, 60, term, []}]}},
	{typed_record_field, {record_field, 61, {atom, 61, idx}, {integer, 61, 0}},
	 {remote_type, 61, [{atom, 61, ecapnp}, {atom, 61, ptr_index}, []]}},
	{typed_record_field,
	 {record_field, 62, {atom, 62, default},
	  {bin, 62, [{bin_element, 62, {integer, 62, 0}, {integer, 62, 64}, [integer, little]}]}},
	 {remote_type, 62, [{atom, 62, ecapnp}, {atom, 62, value}, []]}}],
       []}).

-record(data, {type, align = 0, default}).

-type({{record, data},
       [{typed_record_field, {record_field, 66, {atom, 66, type}},
	 {type, 66, union, [{atom, 66, undefined}, {type, 66, term, []}]}},
	{typed_record_field, {record_field, 67, {atom, 67, align}, {integer, 67, 0}},
	 {remote_type, 67, [{atom, 67, ecapnp}, {atom, 67, bit_count}, []]}},
	{typed_record_field, {record_field, 68, {atom, 68, default}},
	 {type, 68, union,
	  [{atom, 68, undefined}, {remote_type, 68, [{atom, 68, ecapnp}, {atom, 68, value}, []]}]}}],
       []}).

-record(group, {id = 0}).

-type({{record, group},
       [{typed_record_field, {record_field, 72, {atom, 72, id}, {integer, 72, 0}},
	 {remote_type, 72, [{atom, 72, ecapnp}, {atom, 72, type_id}, []]}}],
       []}).

-record(method, {id, name, paramType, resultType}).

-file("c++.capnp", 1).

schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
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
		     [13386661402618388268]}.  %% namespace

namespace() -> '13386661402618388268'().

namespace([]) -> '13386661402618388268'().

'13386661402618388268'() ->
    #schema_node{module = 'c++_capnp', name = namespace, id = 13386661402618388268,
		 scope = 13688829037717245569, src = <<"c++.capnp:namespace">>,
		 kind = #annotation{type = #ptr{type = text, idx = 0, default = <<>>}, targets = [targetsFile]}}.