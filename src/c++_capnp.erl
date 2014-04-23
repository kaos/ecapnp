-file("c++.capnp", 1).

%% This file was generated 2014-04-22 14:23:12 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('c++_capnp').

-vsn(13688829037717245569).

-export([schema/1, namespace/0, namespace/1, '13386661402618388268'/0, root/0, root/1,
	 '13688829037717245569'/0]).

-types([{13386661402618388268, namespace}, {13688829037717245569, root}]).

-file("/home/kaos/src/erl/libs/ecapnp/include/ecapnp_records.hrl", 1).

-ecapnp_schema_version(1).

-record(schema_node,
	{module, name, id = 0, src = <<>>, kind = file, annotations = [], nodes = [], scope = 0}).

-type({{record, schema_node},
       [{typed_record_field, {record_field, 6, {atom, 6, module}},
	 {type, 6, union, [{atom, 6, undefined}, {type, 6, atom, []}]}},
	{typed_record_field, {record_field, 7, {atom, 7, name}},
	 {type, 7, union,
	  [{atom, 7, undefined}, {remote_type, 7, [{atom, 7, ecapnp}, {atom, 7, type_name}, []]}]}},
	{typed_record_field, {record_field, 8, {atom, 8, id}, {integer, 8, 0}},
	 {remote_type, 8, [{atom, 8, ecapnp}, {atom, 8, type_id}, []]}},
	{typed_record_field, {record_field, 9, {atom, 9, src}, {bin, 9, []}},
	 {remote_type, 9, [{atom, 9, ecapnp}, {atom, 9, text}, []]}},
	{typed_record_field, {record_field, 10, {atom, 10, kind}, {atom, 10, file}},
	 {remote_type, 10, [{atom, 10, ecapnp}, {atom, 10, schema_kind}, []]}},
	{typed_record_field, {record_field, 11, {atom, 11, annotations}, {nil, 11}}, {type, 11, list, []}},
	{typed_record_field, {record_field, 12, {atom, 12, nodes}, {nil, 12}},
	 {remote_type, 12, [{atom, 12, ecapnp}, {atom, 12, schema_nodes}, []]}},
	{typed_record_field, {record_field, 13, {atom, 13, scope}, {integer, 13, 0}},
	 {remote_type, 13, [{atom, 13, ecapnp}, {atom, 13, type_id}, []]}}],
       []}).

-record(struct, {dsize = 0, psize = 0, esize = inlineComposite, union_field = none, fields = []}).

-type({{record, struct},
       [{typed_record_field, {record_field, 18, {atom, 18, dsize}, {integer, 18, 0}},
	 {remote_type, 18, [{atom, 18, ecapnp}, {atom, 18, word_count}, []]}},
	{typed_record_field, {record_field, 19, {atom, 19, psize}, {integer, 19, 0}},
	 {remote_type, 19, [{atom, 19, ecapnp}, {atom, 19, ptr_count}, []]}},
	{typed_record_field, {record_field, 20, {atom, 20, esize}, {atom, 20, inlineComposite}},
	 {remote_type, 20, [{atom, 20, ecapnp}, {atom, 20, element_size}, []]}},
	{typed_record_field, {record_field, 21, {atom, 21, union_field}, {atom, 21, none}},
	 {type, 21, union,
	  [{atom, 21, none}, {remote_type, 21, [{atom, 21, ecapnp}, {atom, 21, field_type}, []]}]}},
	{typed_record_field, {record_field, 22, {atom, 22, fields}, {nil, 22}},
	 {remote_type, 22, [{atom, 22, ecapnp}, {atom, 22, struct_fields}, []]}}],
       []}).

-record(enum, {values = []}).

-type({{record, enum},
       [{typed_record_field, {record_field, 27, {atom, 27, values}, {nil, 27}},
	 {remote_type, 27, [{atom, 27, ecapnp}, {atom, 27, enum_values}, []]}}],
       []}).

-record(interface, {extends = [], methods = []}).

-type({{record, interface},
       [{typed_record_field, {record_field, 32, {atom, 32, extends}, {nil, 32}}, {type, 32, list, []}},
	{typed_record_field, {record_field, 33, {atom, 33, methods}, {nil, 33}}, {type, 33, list, []}}],
       []}).

-record(const, {field}).

-record(annotation, {type, targets = []}).

-type({{record, annotation},
       [{record_field, 43, {atom, 43, type}},
	{typed_record_field, {record_field, 44, {atom, 44, targets}, {nil, 44}},
	 {type, 44, list, [{type, 44, atom, []}]}}],
       []}).

-record(field, {name, kind, annotations = []}).

-record(ptr, {type, idx = 0, default = null}).

-type({{record, ptr},
       [{typed_record_field, {record_field, 56, {atom, 56, type}},
	 {type, 56, union, [{atom, 56, undefined}, {type, 56, term, []}]}},
	{typed_record_field, {record_field, 57, {atom, 57, idx}, {integer, 57, 0}},
	 {remote_type, 57, [{atom, 57, ecapnp}, {atom, 57, ptr_index}, []]}},
	{typed_record_field, {record_field, 58, {atom, 58, default}, {atom, 58, null}},
	 {remote_type, 58, [{atom, 58, ecapnp}, {atom, 58, value}, []]}}],
       []}).

-record(data, {type, align = 0, default}).

-type({{record, data},
       [{typed_record_field, {record_field, 62, {atom, 62, type}},
	 {type, 62, union, [{atom, 62, undefined}, {type, 62, term, []}]}},
	{typed_record_field, {record_field, 63, {atom, 63, align}, {integer, 63, 0}},
	 {remote_type, 63, [{atom, 63, ecapnp}, {atom, 63, bit_count}, []]}},
	{typed_record_field, {record_field, 64, {atom, 64, default}},
	 {type, 64, union,
	  [{atom, 64, undefined}, {remote_type, 64, [{atom, 64, ecapnp}, {atom, 64, value}, []]}]}}],
       []}).

-record(group, {id = 0}).

-type({{record, group},
       [{typed_record_field, {record_field, 68, {atom, 68, id}, {integer, 68, 0}},
	 {remote_type, 68, [{atom, 68, ecapnp}, {atom, 68, type_id}, []]}}],
       []}).

-record(method, {name, paramType, resultType}).

-record(ref, {segment, pos = -1, offset = 0, kind = null, data}).

-type({{record, ref},
       [{typed_record_field, {record_field, 82, {atom, 82, segment}},
	 {type, 82, union,
	  [{atom, 82, undefined}, {remote_type, 82, [{atom, 82, ecapnp}, {atom, 82, segment_id}, []]}]}},
	{typed_record_field, {record_field, 83, {atom, 83, pos}, {op, 83, '-', {integer, 83, 1}}},
	 {remote_type, 83, [{atom, 83, ecapnp}, {atom, 83, segment_pos}, []]}},
	{typed_record_field, {record_field, 84, {atom, 84, offset}, {integer, 84, 0}},
	 {remote_type, 84, [{atom, 84, ecapnp}, {atom, 84, segment_offset}, []]}},
	{typed_record_field, {record_field, 85, {atom, 85, kind}, {atom, 85, null}},
	 {remote_type, 85, [{atom, 85, ecapnp}, {atom, 85, ref_kind}, []]}},
	{typed_record_field, {record_field, 86, {atom, 86, data}},
	 {type, 86, union, [{atom, 86, undefined}, {type, 86, pid, []}]}}],
       []}).

-record(struct_ref, {dsize = 0, psize = 0}).

-type({{record, struct_ref},
       [{typed_record_field, {record_field, 90, {atom, 90, dsize}, {integer, 90, 0}},
	 {remote_type, 90, [{atom, 90, ecapnp}, {atom, 90, word_count}, []]}},
	{typed_record_field, {record_field, 91, {atom, 91, psize}, {integer, 91, 0}},
	 {remote_type, 91, [{atom, 91, ecapnp}, {atom, 91, ptr_count}, []]}}],
       []}).

-record(list_ref, {size = empty, count = 0}).

-type({{record, list_ref},
       [{typed_record_field, {record_field, 95, {atom, 95, size}, {atom, 95, empty}},
	 {remote_type, 95, [{atom, 95, ecapnp}, {atom, 95, element_size}, []]}},
	{typed_record_field, {record_field, 96, {atom, 96, count}, {integer, 96, 0}},
	 {type, 96, non_neg_integer, []}}],
       []}).

-record(far_ref, {segment = 0, double_far = false}).

-type({{record, far_ref},
       [{typed_record_field, {record_field, 100, {atom, 100, segment}, {integer, 100, 0}},
	 {type, 100, non_neg_integer, []}},
	{typed_record_field, {record_field, 101, {atom, 101, double_far}, {atom, 101, false}},
	 {type, 101, boolean, []}}],
       []}).

-record(interface_ref, {pid}).

-type({{record, interface_ref},
       [{typed_record_field, {record_field, 105, {atom, 105, pid}},
	 {type, 105, union, [{atom, 105, undefined}, {type, 105, pid, []}]}}],
       []}).

-record(object, {ref = null, schema = object}).

-type({{record, object},
       [{typed_record_field, {record_field, 109, {atom, 109, ref}, {atom, 109, null}},
	 {remote_type, 109, [{atom, 109, ecapnp}, {atom, 109, ref}, []]}},
	{typed_record_field, {record_field, 110, {atom, 110, schema}, {atom, 110, object}},
	 {type, 110, union,
	  [{atom, 110, object}, {remote_type, 110, [{atom, 110, ecapnp}, {atom, 110, schema_node}, []]}]}}],
       []}).

-record(request, {method, param, interface}).

-type({{record, request},
       [{typed_record_field, {record_field, 116, {atom, 116, method}},
	 {type, 116, union,
	  [{atom, 116, undefined}, {remote_type, 116, [{atom, 116, ecapnp}, {atom, 116, field_name}, []]}]}},
	{typed_record_field, {record_field, 117, {atom, 117, param}},
	 {type, 117, union,
	  [{atom, 117, undefined}, {remote_type, 117, [{atom, 117, ecapnp}, {atom, 117, object}, []]}]}},
	{typed_record_field, {record_field, 118, {atom, 118, interface}},
	 {type, 118, union,
	  [{atom, 118, undefined},
	   {remote_type, 118, [{atom, 118, ecapnp}, {atom, 118, schema_node}, []]}]}}],
       []}).

-record(msg, {schema, alloc = [], data = []}).

-type({{record, msg},
       [{typed_record_field, {record_field, 123, {atom, 123, schema}},
	 {type, 123, union,
	  [{atom, 123, undefined}, {remote_type, 123, [{atom, 123, ecapnp}, {atom, 123, schema}, []]}]}},
	{typed_record_field, {record_field, 124, {atom, 124, alloc}, {nil, 124}},
	 {type, 124, list, [{type, 124, integer, []}]}},
	{typed_record_field, {record_field, 125, {atom, 125, data}, {nil, 125}},
	 {remote_type, 125, [{atom, 125, ecapnp}, {atom, 125, message}, []]}}],
       []}).

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