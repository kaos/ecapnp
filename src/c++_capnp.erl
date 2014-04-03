-file("c++.capnp", 1).

%% This file was generated 2014-04-03 12:41:53 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('c++_capnp').

-vsn(13688829037717245569).

-export([schema/1, namespace/0, namespace/1, '13386661402618388268'/0, root/0, root/1,
	 '13688829037717245569'/0]).

-types([{13386661402618388268, namespace}, {13688829037717245569, root}]).

-file("/home/kaos/src/erl/libs/ecapnp/include/ecapnp_records.hrl", 1).

-record(schema_node,
	{module, name, id = 0, src = <<>>, kind = file, annotations = [], nodes = [], scope = 0}).

-type({{record, schema_node},
       [{typed_record_field, {record_field, 4, {atom, 4, module}},
	 {type, 4, union, [{atom, 4, undefined}, {type, 4, atom, []}]}},
	{typed_record_field, {record_field, 5, {atom, 5, name}},
	 {type, 5, union,
	  [{atom, 5, undefined}, {remote_type, 5, [{atom, 5, ecapnp}, {atom, 5, type_name}, []]}]}},
	{typed_record_field, {record_field, 6, {atom, 6, id}, {integer, 6, 0}},
	 {remote_type, 6, [{atom, 6, ecapnp}, {atom, 6, type_id}, []]}},
	{typed_record_field, {record_field, 7, {atom, 7, src}, {bin, 7, []}},
	 {remote_type, 7, [{atom, 7, ecapnp}, {atom, 7, text}, []]}},
	{typed_record_field, {record_field, 8, {atom, 8, kind}, {atom, 8, file}},
	 {remote_type, 8, [{atom, 8, ecapnp}, {atom, 8, schema_kind}, []]}},
	{typed_record_field, {record_field, 9, {atom, 9, annotations}, {nil, 9}}, {type, 9, list, []}},
	{typed_record_field, {record_field, 10, {atom, 10, nodes}, {nil, 10}},
	 {remote_type, 10, [{atom, 10, ecapnp}, {atom, 10, schema_nodes}, []]}},
	{typed_record_field, {record_field, 11, {atom, 11, scope}, {integer, 11, 0}},
	 {remote_type, 11, [{atom, 11, ecapnp}, {atom, 11, type_id}, []]}}],
       []}).

-record(struct, {dsize = 0, psize = 0, esize = inlineComposite, union_field = none, fields = []}).

-type({{record, struct},
       [{typed_record_field, {record_field, 16, {atom, 16, dsize}, {integer, 16, 0}},
	 {remote_type, 16, [{atom, 16, ecapnp}, {atom, 16, word_count}, []]}},
	{typed_record_field, {record_field, 17, {atom, 17, psize}, {integer, 17, 0}},
	 {remote_type, 17, [{atom, 17, ecapnp}, {atom, 17, ptr_count}, []]}},
	{typed_record_field, {record_field, 18, {atom, 18, esize}, {atom, 18, inlineComposite}},
	 {remote_type, 18, [{atom, 18, ecapnp}, {atom, 18, element_size}, []]}},
	{typed_record_field, {record_field, 19, {atom, 19, union_field}, {atom, 19, none}},
	 {type, 19, union,
	  [{atom, 19, none}, {remote_type, 19, [{atom, 19, ecapnp}, {atom, 19, field_type}, []]}]}},
	{typed_record_field, {record_field, 20, {atom, 20, fields}, {nil, 20}},
	 {remote_type, 20, [{atom, 20, ecapnp}, {atom, 20, struct_fields}, []]}}],
       []}).

-record(enum, {values = []}).

-type({{record, enum},
       [{typed_record_field, {record_field, 25, {atom, 25, values}, {nil, 25}},
	 {remote_type, 25, [{atom, 25, ecapnp}, {atom, 25, enum_values}, []]}}],
       []}).

-record(interface, {extends = [], methods = [], struct}).

-type({{record, interface},
       [{typed_record_field, {record_field, 30, {atom, 30, extends}, {nil, 30}}, {type, 30, list, []}},
	{typed_record_field, {record_field, 31, {atom, 31, methods}, {nil, 31}}, {type, 31, list, []}},
	{typed_record_field, {record_field, 34, {atom, 34, struct}},
	 {type, 34, union,
	  [{atom, 34, undefined}, {remote_type, 34, [{atom, 34, ecapnp}, {atom, 34, struct}, []]}]}}],
       []}).

-record(const, {field}).

-record(annotation, {type, targets = []}).

-type({{record, annotation},
       [{record_field, 44, {atom, 44, type}},
	{typed_record_field, {record_field, 45, {atom, 45, targets}, {nil, 45}},
	 {type, 45, list, [{type, 45, atom, []}]}}],
       []}).

-record(field, {name, kind, annotations = []}).

-record(ptr, {type, idx = 0, default = null}).

-type({{record, ptr},
       [{typed_record_field, {record_field, 57, {atom, 57, type}},
	 {type, 57, union, [{atom, 57, undefined}, {type, 57, term, []}]}},
	{typed_record_field, {record_field, 58, {atom, 58, idx}, {integer, 58, 0}},
	 {remote_type, 58, [{atom, 58, ecapnp}, {atom, 58, ptr_index}, []]}},
	{typed_record_field, {record_field, 59, {atom, 59, default}, {atom, 59, null}},
	 {remote_type, 59, [{atom, 59, ecapnp}, {atom, 59, value}, []]}}],
       []}).

-record(data, {type, align = 0, default}).

-type({{record, data},
       [{typed_record_field, {record_field, 63, {atom, 63, type}},
	 {type, 63, union, [{atom, 63, undefined}, {type, 63, term, []}]}},
	{typed_record_field, {record_field, 64, {atom, 64, align}, {integer, 64, 0}},
	 {remote_type, 64, [{atom, 64, ecapnp}, {atom, 64, bit_count}, []]}},
	{typed_record_field, {record_field, 65, {atom, 65, default}},
	 {type, 65, union,
	  [{atom, 65, undefined}, {remote_type, 65, [{atom, 65, ecapnp}, {atom, 65, value}, []]}]}}],
       []}).

-record(group, {id = 0}).

-type({{record, group},
       [{typed_record_field, {record_field, 69, {atom, 69, id}, {integer, 69, 0}},
	 {remote_type, 69, [{atom, 69, ecapnp}, {atom, 69, type_id}, []]}}],
       []}).

-record(method, {name, paramType, resultType}).

-record(ref, {segment, pos = -1, offset = 0, kind = null, data}).

-type({{record, ref},
       [{typed_record_field, {record_field, 83, {atom, 83, segment}},
	 {type, 83, union,
	  [{atom, 83, undefined}, {remote_type, 83, [{atom, 83, ecapnp}, {atom, 83, segment_id}, []]}]}},
	{typed_record_field, {record_field, 84, {atom, 84, pos}, {op, 84, '-', {integer, 84, 1}}},
	 {remote_type, 84, [{atom, 84, ecapnp}, {atom, 84, segment_pos}, []]}},
	{typed_record_field, {record_field, 85, {atom, 85, offset}, {integer, 85, 0}},
	 {remote_type, 85, [{atom, 85, ecapnp}, {atom, 85, segment_offset}, []]}},
	{typed_record_field, {record_field, 86, {atom, 86, kind}, {atom, 86, null}},
	 {remote_type, 86, [{atom, 86, ecapnp}, {atom, 86, ref_kind}, []]}},
	{typed_record_field, {record_field, 87, {atom, 87, data}},
	 {type, 87, union, [{atom, 87, undefined}, {type, 87, pid, []}]}}],
       []}).

-record(struct_ref, {dsize = 0, psize = 0}).

-type({{record, struct_ref},
       [{typed_record_field, {record_field, 97, {atom, 97, dsize}, {integer, 97, 0}},
	 {remote_type, 97, [{atom, 97, ecapnp}, {atom, 97, word_count}, []]}},
	{typed_record_field, {record_field, 97, {atom, 97, psize}, {integer, 97, 0}},
	 {remote_type, 97, [{atom, 97, ecapnp}, {atom, 97, ptr_count}, []]}}],
       []}).

-record(list_ref, {size = empty, count = 0}).

-type({{record, list_ref},
       [{typed_record_field, {record_field, 101, {atom, 101, size}, {atom, 101, empty}},
	 {remote_type, 101, [{atom, 101, ecapnp}, {atom, 101, element_size}, []]}},
	{typed_record_field, {record_field, 102, {atom, 102, count}, {integer, 102, 0}},
	 {type, 102, non_neg_integer, []}}],
       []}).

-record(far_ref, {segment = 0, double_far = false}).

-type({{record, far_ref},
       [{typed_record_field, {record_field, 106, {atom, 106, segment}, {integer, 106, 0}},
	 {type, 106, non_neg_integer, []}},
	{typed_record_field, {record_field, 107, {atom, 107, double_far}, {atom, 107, false}},
	 {type, 107, boolean, []}}],
       []}).

-record(interface_ref, {dsize = 0, psize = 0}).

-type({{record, interface_ref},
       [{typed_record_field, {record_field, 111, {atom, 111, dsize}, {integer, 111, 0}},
	 {remote_type, 111, [{atom, 111, ecapnp}, {atom, 111, word_count}, []]}},
	{typed_record_field, {record_field, 111, {atom, 111, psize}, {integer, 111, 0}},
	 {remote_type, 111, [{atom, 111, ecapnp}, {atom, 111, ptr_count}, []]}}],
       []}).

-record(object, {ref = null, schema = object}).

-type({{record, object},
       [{typed_record_field, {record_field, 115, {atom, 115, ref}, {atom, 115, null}},
	 {remote_type, 115, [{atom, 115, ecapnp}, {atom, 115, ref}, []]}},
	{typed_record_field, {record_field, 116, {atom, 116, schema}, {atom, 116, object}},
	 {type, 116, union,
	  [{atom, 116, object}, {remote_type, 116, [{atom, 116, ecapnp}, {atom, 116, schema_node}, []]}]}}],
       []}).

-record(request, {method, param, interface}).

-type({{record, request},
       [{typed_record_field, {record_field, 124, {atom, 124, method}},
	 {type, 124, union,
	  [{atom, 124, undefined}, {remote_type, 124, [{atom, 124, ecapnp}, {atom, 124, field_name}, []]}]}},
	{typed_record_field, {record_field, 125, {atom, 125, param}},
	 {type, 125, union,
	  [{atom, 125, undefined}, {remote_type, 125, [{atom, 125, ecapnp}, {atom, 125, object}, []]}]}},
	{typed_record_field, {record_field, 126, {atom, 126, interface}},
	 {type, 126, union,
	  [{atom, 126, undefined},
	   {remote_type, 126, [{atom, 126, ecapnp}, {atom, 126, schema_node}, []]}]}}],
       []}).

-record(msg, {schema, alloc = [], data = []}).

-type({{record, msg},
       [{typed_record_field, {record_field, 131, {atom, 131, schema}},
	 {type, 131, union,
	  [{atom, 131, undefined}, {remote_type, 131, [{atom, 131, ecapnp}, {atom, 131, schema}, []]}]}},
	{typed_record_field, {record_field, 132, {atom, 132, alloc}, {nil, 132}},
	 {type, 132, list, [{type, 132, integer, []}]}},
	{typed_record_field, {record_field, 133, {atom, 133, data}, {nil, 133}},
	 {remote_type, 133, [{atom, 133, ecapnp}, {atom, 133, message}, []]}}],
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