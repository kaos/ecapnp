-file("test.capnp", 1).

%% This file was generated 2014-04-04 18:19:32 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(test_capnp).

-vsn(16752831063434032545).

-export([schema/1, testAnno/0, testAnno/1, '17364400218949434058'/0, 'Test'/0, 'Test'/1,
	 '18038429679936549741'/0, '12292473172826227401'/0, '9356761420570873088'/0,
	 '12591081617868223671'/0, 'Simple'/0, 'Simple'/1, '15091335337902283752'/0, 'ListTest'/0,
	 'ListTest'/1, '17083831967670695846'/0, 'BasicCap'/0, 'BasicCap'/1, '17521612982906909583'/0,
	 '13875996178202423621'/0, '10419494484650272988'/0, 'Pipelines'/0, 'Pipelines'/1,
	 '16031390312538156137'/0, '14503907271725109646'/0, '14869749728248688780'/0, root/0, root/1,
	 '16752831063434032545'/0]).

-types([{17364400218949434058, testAnno}, {18038429679936549741, 'Test'},
	{12292473172826227401, ['Test', meta]}, {9356761420570873088, ['Test', opts]},
	{12591081617868223671, ['Test', groupField]}, {15091335337902283752, 'Simple'},
	{17083831967670695846, 'ListTest'}, {17521612982906909583, 'BasicCap'},
	{13875996178202423621, ['BasicCap', [add, '$Results']]},
	{10419494484650272988, ['BasicCap', [add, '$Params']]}, {16031390312538156137, 'Pipelines'},
	{14503907271725109646, ['Pipelines', [getBasic, '$Results']]},
	{14869749728248688780, ['Pipelines', [getBasic, '$Params']]}, {16752831063434032545, root}]).

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

-file("test.capnp", 1).

schema(17364400218949434058) -> '17364400218949434058'();
schema(testAnno) -> '17364400218949434058'();
schema([testAnno]) -> '17364400218949434058'();
schema(18038429679936549741) -> '18038429679936549741'();
schema('Test') -> '18038429679936549741'();
schema(['Test']) -> '18038429679936549741'();
schema(12292473172826227401) -> '12292473172826227401'();
schema(['Test', meta]) -> '12292473172826227401'();
schema(9356761420570873088) -> '9356761420570873088'();
schema(['Test', opts]) -> '9356761420570873088'();
schema(12591081617868223671) -> '12591081617868223671'();
schema(['Test', groupField]) -> '12591081617868223671'();
schema(15091335337902283752) -> '15091335337902283752'();
schema('Simple') -> '15091335337902283752'();
schema(['Simple']) -> '15091335337902283752'();
schema(17083831967670695846) -> '17083831967670695846'();
schema('ListTest') -> '17083831967670695846'();
schema(['ListTest']) -> '17083831967670695846'();
schema(17521612982906909583) -> '17521612982906909583'();
schema('BasicCap') -> '17521612982906909583'();
schema(['BasicCap']) -> '17521612982906909583'();
schema(13875996178202423621) -> '13875996178202423621'();
schema(['BasicCap', [add, '$Results']]) -> '13875996178202423621'();
schema(10419494484650272988) -> '10419494484650272988'();
schema(['BasicCap', [add, '$Params']]) -> '10419494484650272988'();
schema(16031390312538156137) -> '16031390312538156137'();
schema('Pipelines') -> '16031390312538156137'();
schema(['Pipelines']) -> '16031390312538156137'();
schema(14503907271725109646) -> '14503907271725109646'();
schema(['Pipelines', [getBasic, '$Results']]) -> '14503907271725109646'();
schema(14869749728248688780) -> '14869749728248688780'();
schema(['Pipelines', [getBasic, '$Params']]) -> '14869749728248688780'();
schema(16752831063434032545) -> '16752831063434032545'();
schema(root) -> '16752831063434032545'();
schema([root]) -> '16752831063434032545'();
schema(_) -> undefined.

root() -> '16752831063434032545'().

root([]) -> '16752831063434032545'().

'16752831063434032545'() ->
    #schema_node{module = test_capnp, name = root, id = 16752831063434032545, scope = 0,
		 src = <<"test.capnp">>, annotations = [{17364400218949434058, <<"file anno 2013">>}], kind = file,
		 nodes =
		     [17364400218949434058,  %% testAnno
		      18038429679936549741,  %% Test
		      15091335337902283752,  %% Simple
		      17083831967670695846,  %% ListTest
		      17521612982906909583,  %% BasicCap
		      16031390312538156137]}.  %% Pipelines

'Pipelines'() -> '16031390312538156137'().

'Pipelines'([[getBasic, '$Results']]) -> '14503907271725109646'();
'Pipelines'([[getBasic, '$Params']]) -> '14869749728248688780'();
'Pipelines'([]) -> '16031390312538156137'().

'16031390312538156137'() ->
    #schema_node{module = test_capnp, name = 'Pipelines', id = 16031390312538156137,
		 scope = 16752831063434032545, src = <<"test.capnp:Pipelines">>,
		 kind =
		     #interface{extends = [],
				methods =
				    [#method{name = getBasic, paramType = 14869749728248688780, resultType = 14503907271725109646}]}}.

'14503907271725109646'() ->
    #schema_node{module = test_capnp, name = ['Pipelines', [getBasic, '$Results']],
		 id = 14503907271725109646, scope = 0, src = <<"test.capnp:Pipelines.getBasic$Results">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{name = basic,
					 kind =
					     #ptr{type = {interface, 17521612982906909583}, idx = 0,
						  default = 'maybe_call_interface_factory... ?'}}]}}.

'14869749728248688780'() ->
    #schema_node{module = test_capnp, name = ['Pipelines', [getBasic, '$Params']],
		 id = 14869749728248688780, scope = 0, src = <<"test.capnp:Pipelines.getBasic$Params">>,
		 kind = #struct{dsize = 0, psize = 0, esize = empty, union_field = none, fields = []}}.

'BasicCap'() -> '17521612982906909583'().

'BasicCap'([[add, '$Results']]) -> '13875996178202423621'();
'BasicCap'([[add, '$Params']]) -> '10419494484650272988'();
'BasicCap'([]) -> '17521612982906909583'().

'17521612982906909583'() ->
    #schema_node{module = test_capnp, name = 'BasicCap', id = 17521612982906909583,
		 scope = 16752831063434032545, src = <<"test.capnp:BasicCap">>,
		 kind =
		     #interface{extends = [],
				methods =
				    [#method{name = add, paramType = 10419494484650272988, resultType = 13875996178202423621}]}}.

'13875996178202423621'() ->
    #schema_node{module = test_capnp, name = ['BasicCap', [add, '$Results']], id = 13875996178202423621,
		 scope = 0, src = <<"test.capnp:BasicCap.add$Results">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = result,
					 kind = #data{type = int64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'10419494484650272988'() ->
    #schema_node{module = test_capnp, name = ['BasicCap', [add, '$Params']], id = 10419494484650272988,
		 scope = 0, src = <<"test.capnp:BasicCap.add$Params">>,
		 kind =
		     #struct{dsize = 2, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = a, kind = #data{type = int64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = b, kind = #data{type = int64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'ListTest'() -> '17083831967670695846'().

'ListTest'([]) -> '17083831967670695846'().

'17083831967670695846'() ->
    #schema_node{module = test_capnp, name = 'ListTest', id = 17083831967670695846,
		 scope = 16752831063434032545, src = <<"test.capnp:ListTest">>,
		 kind =
		     #struct{dsize = 0, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = listInts,
					 kind =
					     #ptr{type = {list, int32}, idx = 0,
						  default = <<1, 0, 0, 0, 28, 0, 0, 0, 200, 1, 0, 0, 21, 3, 0, 0, 133, 255, 255, 255, 0, 0, 0, 0>>}},
				  #field{name = listAny, kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = listSimples,
					 kind =
					     #ptr{type = {list, {struct, 15091335337902283752}}, idx = 2,
						  default =
						      <<1, 0, 0, 0, 55, 0, 0, 0, 8, 0, 0, 0, 1, 0, 2, 0, 223, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 50, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0, 0, 220, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
							0, 102, 105, 114, 115, 116, 0, 0, 0, 115, 101, 99, 111, 110, 100, 0, 0>>}},
				  #field{name = listText,
					 kind = #ptr{type = {list, text}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Simple'() -> '15091335337902283752'().

'Simple'([]) -> '15091335337902283752'().

'15091335337902283752'() ->
    #schema_node{module = test_capnp, name = 'Simple', id = 15091335337902283752,
		 scope = 16752831063434032545, src = <<"test.capnp:Simple">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = message, kind = #ptr{type = text, idx = 0, default = <<"default message">>}},
				  #field{name = value, kind = #data{type = uint32, align = 0, default = <<222, 0, 0, 0>>}},
				  #field{name = simpleMessage, kind = #ptr{type = text, idx = 1, default = <<"simple message">>}},
				  #field{name = defaultValue, kind = #data{type = uint32, align = 32, default = <<77, 1, 0, 0>>}}]}}.

'Test'() -> '18038429679936549741'().

'Test'([meta]) -> '12292473172826227401'();
'Test'([opts]) -> '9356761420570873088'();
'Test'([groupField]) -> '12591081617868223671'();
'Test'([]) -> '18038429679936549741'().

'18038429679936549741'() ->
    #schema_node{module = test_capnp, name = 'Test', id = 18038429679936549741,
		 scope = 16752831063434032545, src = <<"test.capnp:Test">>,
		 annotations = [{17364400218949434058, <<"Test struct anno 2013 too">>}],
		 kind =
		     #struct{dsize = 2, psize = 6, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, boolField, #field{name = boolField, kind = #data{type = bool, align = 15, default = <<0:1>>}}},
					     {1, groupField, #field{name = groupField, kind = #group{id = 12591081617868223671}}}]},
				       align = 16, default = <<0, 0>>},
			     fields =
				 [#field{name = intField, kind = #data{type = uint8, align = 0, default = <<33>>}},
				  #field{name = textField, kind = #ptr{type = text, idx = 0, default = <<"test">>}},
				  #field{name = opts, kind = #group{id = 9356761420570873088}},
				  #field{name = meta, kind = #group{id = 12292473172826227401}},
				  #field{name = structField,
					 kind =
					     #ptr{type = {struct, 15091335337902283752}, idx = 4, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'12292473172826227401'() ->
    #schema_node{module = test_capnp, name = ['Test', meta], id = 12292473172826227401,
		 scope = 18038429679936549741, src = <<"test.capnp:Test.meta">>,
		 kind =
		     #struct{dsize = 2, psize = 6, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint16, align = 80, default = <<0, 0>>}},
				  #field{name = tag, kind = #ptr{type = text, idx = 2, default = <<"">>}},
				  #field{name = data, kind = #ptr{type = data, idx = 3, default = <<49, 50, 51, 52>>}},
				  #field{name = struct,
					 kind =
					     #ptr{type = {struct, 15091335337902283752}, idx = 5,
						  default =
						      <<0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 12, 0, 0, 0, 5, 0, 0, 0, 210, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
							111, 118, 101, 114, 114, 105, 100, 101, 110, 32, 100, 101, 102, 97, 117, 108, 116, 32, 109, 101,
							115, 115, 97, 103, 101, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'9356761420570873088'() ->
    #schema_node{module = test_capnp, name = ['Test', opts], id = 9356761420570873088,
		 scope = 18038429679936549741, src = <<"test.capnp:Test.opts">>,
		 kind =
		     #struct{dsize = 2, psize = 6, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, bool, #field{name = bool, kind = #data{type = bool, align = 55, default = <<1:1>>}}},
					     {1, text, #field{name = text, kind = #ptr{type = text, idx = 1, default = <<"">>}}},
					     {2, data, #field{name = data, kind = #ptr{type = data, idx = 1, default = <<>>}}},
					     {3, object,
					      #field{name = object, kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 64, default = <<0, 0>>},
			     fields = []}}.

'12591081617868223671'() ->
    #schema_node{module = test_capnp, name = ['Test', groupField], id = 12591081617868223671,
		 scope = 18038429679936549741, src = <<"test.capnp:Test.groupField">>,
		 kind =
		     #struct{dsize = 2, psize = 6, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = a, kind = #data{type = int8, align = 8, default = <<212>>}},
				  #field{name = b, kind = #data{type = int8, align = 32, default = <<55>>}},
				  #field{name = c, kind = #data{type = int8, align = 40, default = <<0>>}}]}}.

testAnno() -> '17364400218949434058'().

testAnno([]) -> '17364400218949434058'().

'17364400218949434058'() ->
    #schema_node{module = test_capnp, name = testAnno, id = 17364400218949434058,
		 scope = 16752831063434032545, src = <<"test.capnp:testAnno">>,
		 kind =
		     #annotation{type = #ptr{type = text, idx = 0, default = <<>>},
				 targets =
				     [targetsFile, targetsConst, targetsEnum, targetsEnumerant, targetsStruct, targetsField,
				      targetsUnion, targetsGroup, targetsInterface, targetsMethod, targetsParam, targetsAnnotation]}}.