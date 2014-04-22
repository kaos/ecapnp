-file("test.capnp", 1).

%% This file was generated 2014-04-22 13:47:24 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(test_capnp).

-vsn(16752831063434032545).

-export([schema/1, testAnno/0, testAnno/1, '17364400218949434058'/0, 'Test'/0, 'Test'/1,
	 '18038429679936549741'/0, '12292473172826227401'/0, '9356761420570873088'/0,
	 '12591081617868223671'/0, 'Simple'/0, 'Simple'/1, '15091335337902283752'/0, 'ListTest'/0,
	 'ListTest'/1, '17083831967670695846'/0, 'BasicCap'/0, 'BasicCap'/1, '17521612982906909583'/0,
	 '13875996178202423621'/0, '10419494484650272988'/0, 'Pipelines'/0, 'Pipelines'/1,
	 '16031390312538156137'/0, '14503907271725109646'/0, '14869749728248688780'/0, 'OtherCap'/0,
	 'OtherCap'/1, '10376444823742217855'/0, '18397524501497330844'/0, '14954489407150623152'/0,
	 'ThirdCap'/0, 'ThirdCap'/1, '18123859541809896974'/0, '14187451716366646039'/0,
	 '16272584843106476340'/0, root/0, root/1, '16752831063434032545'/0]).

-types([{17364400218949434058, testAnno}, {18038429679936549741, 'Test'},
	{12292473172826227401, ['Test', meta]}, {9356761420570873088, ['Test', opts]},
	{12591081617868223671, ['Test', groupField]}, {15091335337902283752, 'Simple'},
	{17083831967670695846, 'ListTest'}, {17521612982906909583, 'BasicCap'},
	{13875996178202423621, ['BasicCap', [add, '$Results']]},
	{10419494484650272988, ['BasicCap', [add, '$Params']]}, {16031390312538156137, 'Pipelines'},
	{14503907271725109646, ['Pipelines', [getBasic, '$Results']]},
	{14869749728248688780, ['Pipelines', [getBasic, '$Params']]}, {10376444823742217855, 'OtherCap'},
	{18397524501497330844, ['OtherCap', [sqroot, '$Results']]},
	{14954489407150623152, ['OtherCap', [sqroot, '$Params']]}, {18123859541809896974, 'ThirdCap'},
	{14187451716366646039, ['ThirdCap', [square, '$Results']]},
	{16272584843106476340, ['ThirdCap', [square, '$Params']]}, {16752831063434032545, root}]).

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
schema(10376444823742217855) -> '10376444823742217855'();
schema('OtherCap') -> '10376444823742217855'();
schema(['OtherCap']) -> '10376444823742217855'();
schema(18397524501497330844) -> '18397524501497330844'();
schema(['OtherCap', [sqroot, '$Results']]) -> '18397524501497330844'();
schema(14954489407150623152) -> '14954489407150623152'();
schema(['OtherCap', [sqroot, '$Params']]) -> '14954489407150623152'();
schema(18123859541809896974) -> '18123859541809896974'();
schema('ThirdCap') -> '18123859541809896974'();
schema(['ThirdCap']) -> '18123859541809896974'();
schema(14187451716366646039) -> '14187451716366646039'();
schema(['ThirdCap', [square, '$Results']]) -> '14187451716366646039'();
schema(16272584843106476340) -> '16272584843106476340'();
schema(['ThirdCap', [square, '$Params']]) -> '16272584843106476340'();
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
		      16031390312538156137,  %% Pipelines
		      10376444823742217855,  %% OtherCap
		      18123859541809896974]}.  %% ThirdCap

'ThirdCap'() -> '18123859541809896974'().

'ThirdCap'([[square, '$Results']]) -> '14187451716366646039'();
'ThirdCap'([[square, '$Params']]) -> '16272584843106476340'();
'ThirdCap'([]) -> '18123859541809896974'().

'18123859541809896974'() ->
    #schema_node{module = test_capnp, name = 'ThirdCap', id = 18123859541809896974,
		 scope = 16752831063434032545, src = <<"test.capnp:ThirdCap">>,
		 kind =
		     #interface{extends = [17521612982906909583, 10376444823742217855],
				methods =
				    [#method{name = square, paramType = 16272584843106476340, resultType = 14187451716366646039}]}}.

'14187451716366646039'() ->
    #schema_node{module = test_capnp, name = ['ThirdCap', [square, '$Results']],
		 id = 14187451716366646039, scope = 0, src = <<"test.capnp:ThirdCap.square$Results">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = sq, kind = #data{type = int64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'16272584843106476340'() ->
    #schema_node{module = test_capnp, name = ['ThirdCap', [square, '$Params']],
		 id = 16272584843106476340, scope = 0, src = <<"test.capnp:ThirdCap.square$Params">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = a, kind = #data{type = int64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'OtherCap'() -> '10376444823742217855'().

'OtherCap'([[sqroot, '$Results']]) -> '18397524501497330844'();
'OtherCap'([[sqroot, '$Params']]) -> '14954489407150623152'();
'OtherCap'([]) -> '10376444823742217855'().

'10376444823742217855'() ->
    #schema_node{module = test_capnp, name = 'OtherCap', id = 10376444823742217855,
		 scope = 16752831063434032545, src = <<"test.capnp:OtherCap">>,
		 kind =
		     #interface{extends = [],
				methods =
				    [#method{name = sqroot, paramType = 14954489407150623152, resultType = 18397524501497330844}]}}.

'18397524501497330844'() ->
    #schema_node{module = test_capnp, name = ['OtherCap', [sqroot, '$Results']],
		 id = 18397524501497330844, scope = 0, src = <<"test.capnp:OtherCap.sqroot$Results">>,
		 kind =
		     #struct{dsize = 2, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = root1,
					 kind = #data{type = float64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = root2,
					 kind = #data{type = float64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'14954489407150623152'() ->
    #schema_node{module = test_capnp, name = ['OtherCap', [sqroot, '$Params']],
		 id = 14954489407150623152, scope = 0, src = <<"test.capnp:OtherCap.sqroot$Params">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = a, kind = #data{type = int64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

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