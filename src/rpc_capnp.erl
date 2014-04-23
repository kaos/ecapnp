-file("rpc.capnp", 1).

%% This file was generated 2014-04-22 14:26:33 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(rpc_capnp).

-vsn(12903543124727603792).

-export([schema/1, 'Message'/0, 'Message'/1, '10500036013887172658'/0, 'Call'/0, 'Call'/1,
	 '9469473312751832276'/0, '15774052265921044377'/0, 'Return'/0, 'Return'/1, '11392333052105676602'/0,
	 'Finish'/0, 'Finish'/1, '15239388059401719395'/0, 'Resolve'/0, 'Resolve'/1,
	 '13529541526594062446'/0, 'Release'/0, 'Release'/1, '12473400923157197975'/0, 'Disembargo'/0,
	 'Disembargo'/1, '17970548384007534353'/0, '15376050949367520589'/0, 'Save'/0, 'Save'/1,
	 '16433336749162137644'/0, 'Restore'/0, 'Restore'/1, '17009130564474155176'/0, 'Delete'/0,
	 'Delete'/1, '9666541409743531671'/0, 'Provide'/0, 'Provide'/1, '11270825879279873114'/0, 'Accept'/0,
	 'Accept'/1, '15332985841292492822'/0, 'Join'/0, 'Join'/1, '18149955118657700271'/0,
	 'MessageTarget'/0, 'MessageTarget'/1, '10789521159760378817'/0, 'Payload'/0, 'Payload'/1,
	 '11100916931204903995'/0, 'CapDescriptor'/0, 'CapDescriptor'/1, '9593755465305995440'/0,
	 'PromisedAnswer'/0, 'PromisedAnswer'/1, '15564635848320162976'/0, '17516350820840804481'/0,
	 'SturdyRef'/0, 'SturdyRef'/1, '14883405629196290303'/0, 'ThirdPartyCapDescriptor'/0,
	 'ThirdPartyCapDescriptor'/1, '15235686326393111165'/0, 'Exception'/0, 'Exception'/1,
	 '15430940935639230746'/0, '13523986587913222488'/0, root/0, root/1, '12903543124727603792'/0]).

-types([{10500036013887172658, 'Message'}, {9469473312751832276, 'Call'},
	{15774052265921044377, ['Call', sendResultsTo]}, {11392333052105676602, 'Return'},
	{15239388059401719395, 'Finish'}, {13529541526594062446, 'Resolve'},
	{12473400923157197975, 'Release'}, {17970548384007534353, 'Disembargo'},
	{15376050949367520589, ['Disembargo', context]}, {16433336749162137644, 'Save'},
	{17009130564474155176, 'Restore'}, {9666541409743531671, 'Delete'},
	{11270825879279873114, 'Provide'}, {15332985841292492822, 'Accept'}, {18149955118657700271, 'Join'},
	{10789521159760378817, 'MessageTarget'}, {11100916931204903995, 'Payload'},
	{9593755465305995440, 'CapDescriptor'}, {15564635848320162976, 'PromisedAnswer'},
	{17516350820840804481, ['PromisedAnswer', 'Op']}, {14883405629196290303, 'SturdyRef'},
	{15235686326393111165, 'ThirdPartyCapDescriptor'}, {15430940935639230746, 'Exception'},
	{13523986587913222488, ['Exception', 'Durability']}, {12903543124727603792, root}]).

-import('c++_capnp', ['13386661402618388268'/0]).

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

-file("rpc.capnp", 1).

schema(10500036013887172658) -> '10500036013887172658'();
schema('Message') -> '10500036013887172658'();
schema(['Message']) -> '10500036013887172658'();
schema(9469473312751832276) -> '9469473312751832276'();
schema('Call') -> '9469473312751832276'();
schema(['Call']) -> '9469473312751832276'();
schema(15774052265921044377) -> '15774052265921044377'();
schema(['Call', sendResultsTo]) -> '15774052265921044377'();
schema(11392333052105676602) -> '11392333052105676602'();
schema('Return') -> '11392333052105676602'();
schema(['Return']) -> '11392333052105676602'();
schema(15239388059401719395) -> '15239388059401719395'();
schema('Finish') -> '15239388059401719395'();
schema(['Finish']) -> '15239388059401719395'();
schema(13529541526594062446) -> '13529541526594062446'();
schema('Resolve') -> '13529541526594062446'();
schema(['Resolve']) -> '13529541526594062446'();
schema(12473400923157197975) -> '12473400923157197975'();
schema('Release') -> '12473400923157197975'();
schema(['Release']) -> '12473400923157197975'();
schema(17970548384007534353) -> '17970548384007534353'();
schema('Disembargo') -> '17970548384007534353'();
schema(['Disembargo']) -> '17970548384007534353'();
schema(15376050949367520589) -> '15376050949367520589'();
schema(['Disembargo', context]) -> '15376050949367520589'();
schema(16433336749162137644) -> '16433336749162137644'();
schema('Save') -> '16433336749162137644'();
schema(['Save']) -> '16433336749162137644'();
schema(17009130564474155176) -> '17009130564474155176'();
schema('Restore') -> '17009130564474155176'();
schema(['Restore']) -> '17009130564474155176'();
schema(9666541409743531671) -> '9666541409743531671'();
schema('Delete') -> '9666541409743531671'();
schema(['Delete']) -> '9666541409743531671'();
schema(11270825879279873114) -> '11270825879279873114'();
schema('Provide') -> '11270825879279873114'();
schema(['Provide']) -> '11270825879279873114'();
schema(15332985841292492822) -> '15332985841292492822'();
schema('Accept') -> '15332985841292492822'();
schema(['Accept']) -> '15332985841292492822'();
schema(18149955118657700271) -> '18149955118657700271'();
schema('Join') -> '18149955118657700271'();
schema(['Join']) -> '18149955118657700271'();
schema(10789521159760378817) -> '10789521159760378817'();
schema('MessageTarget') -> '10789521159760378817'();
schema(['MessageTarget']) -> '10789521159760378817'();
schema(11100916931204903995) -> '11100916931204903995'();
schema('Payload') -> '11100916931204903995'();
schema(['Payload']) -> '11100916931204903995'();
schema(9593755465305995440) -> '9593755465305995440'();
schema('CapDescriptor') -> '9593755465305995440'();
schema(['CapDescriptor']) -> '9593755465305995440'();
schema(15564635848320162976) -> '15564635848320162976'();
schema('PromisedAnswer') -> '15564635848320162976'();
schema(['PromisedAnswer']) -> '15564635848320162976'();
schema(17516350820840804481) -> '17516350820840804481'();
schema(['PromisedAnswer', 'Op']) -> '17516350820840804481'();
schema(14883405629196290303) -> '14883405629196290303'();
schema('SturdyRef') -> '14883405629196290303'();
schema(['SturdyRef']) -> '14883405629196290303'();
schema(15235686326393111165) -> '15235686326393111165'();
schema('ThirdPartyCapDescriptor') -> '15235686326393111165'();
schema(['ThirdPartyCapDescriptor']) -> '15235686326393111165'();
schema(15430940935639230746) -> '15430940935639230746'();
schema('Exception') -> '15430940935639230746'();
schema(['Exception']) -> '15430940935639230746'();
schema(13523986587913222488) -> '13523986587913222488'();
schema(['Exception', 'Durability']) -> '13523986587913222488'();
schema(12903543124727603792) -> '12903543124727603792'();
schema(root) -> '12903543124727603792'();
schema([root]) -> '12903543124727603792'();
%% Imported from c++_capnp
schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
schema(_) -> undefined.

root() -> '12903543124727603792'().

root([]) -> '12903543124727603792'().

'12903543124727603792'() ->
    #schema_node{module = rpc_capnp, name = root, id = 12903543124727603792, scope = 0,
		 src = <<"rpc.capnp">>, annotations = [{13386661402618388268, <<"capnp::rpc">>}], kind = file,
		 nodes =
		     [10500036013887172658,  %% Message
		      9469473312751832276,  %% Call
		      11392333052105676602,  %% Return
		      15239388059401719395,  %% Finish
		      13529541526594062446,  %% Resolve
		      12473400923157197975,  %% Release
		      17970548384007534353,  %% Disembargo
		      16433336749162137644,  %% Save
		      17009130564474155176,  %% Restore
		      9666541409743531671,  %% Delete
		      11270825879279873114,  %% Provide
		      15332985841292492822,  %% Accept
		      18149955118657700271,  %% Join
		      10789521159760378817,  %% MessageTarget
		      11100916931204903995,  %% Payload
		      9593755465305995440,  %% CapDescriptor
		      15564635848320162976,  %% PromisedAnswer
		      14883405629196290303,  %% SturdyRef
		      15235686326393111165,  %% ThirdPartyCapDescriptor
		      15430940935639230746]}.  %% Exception

'Exception'() -> '15430940935639230746'().

'Exception'(['Durability']) -> '13523986587913222488'();
'Exception'([]) -> '15430940935639230746'().

'15430940935639230746'() ->
    #schema_node{module = rpc_capnp, name = 'Exception', id = 15430940935639230746,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Exception">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = reason, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{name = isCallersFault, kind = #data{type = bool, align = 7, default = <<0:1>>}},
				  #field{name = durability,
					 kind = #data{type = {enum, 13523986587913222488}, align = 16, default = <<0, 0>>}}]},
		 nodes =
		     [13523986587913222488]}.  %% Durability

'13523986587913222488'() ->
    #schema_node{module = rpc_capnp, name = ['Exception', 'Durability'], id = 13523986587913222488,
		 scope = 15430940935639230746, src = <<"rpc.capnp:Exception.Durability">>,
		 kind = #enum{values = [{0, permanent}, {1, temporary}, {2, overloaded}]}}.

'ThirdPartyCapDescriptor'() -> '15235686326393111165'().

'ThirdPartyCapDescriptor'([]) -> '15235686326393111165'().

'15235686326393111165'() ->
    #schema_node{module = rpc_capnp, name = 'ThirdPartyCapDescriptor', id = 15235686326393111165,
		 scope = 12903543124727603792, src = <<"rpc.capnp:ThirdPartyCapDescriptor">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = vineId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}]}}.

'SturdyRef'() -> '14883405629196290303'().

'SturdyRef'([]) -> '14883405629196290303'().

'14883405629196290303'() ->
    #schema_node{module = rpc_capnp, name = 'SturdyRef', id = 14883405629196290303,
		 scope = 12903543124727603792, src = <<"rpc.capnp:SturdyRef">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = hostId, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = objectId,
					 kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'PromisedAnswer'() -> '15564635848320162976'().

'PromisedAnswer'(['Op']) -> '17516350820840804481'();
'PromisedAnswer'([]) -> '15564635848320162976'().

'15564635848320162976'() ->
    #schema_node{module = rpc_capnp, name = 'PromisedAnswer', id = 15564635848320162976,
		 scope = 12903543124727603792, src = <<"rpc.capnp:PromisedAnswer">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = transform,
					 kind =
					     #ptr{type = {list, {struct, 17516350820840804481}}, idx = 0,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
		 nodes =
		     [17516350820840804481]}.  %% Op

'17516350820840804481'() ->
    #schema_node{module = rpc_capnp, name = ['PromisedAnswer', 'Op'], id = 17516350820840804481,
		 scope = 15564635848320162976, src = <<"rpc.capnp:PromisedAnswer.Op">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = fourBytes,
			     union_field =
				 #data{type =
					   {union,
					    [{0, noop, #field{name = noop, kind = void}},
					     {1, getPointerField,
					      #field{name = getPointerField, kind = #data{type = uint16, align = 16, default = <<0, 0>>}}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.

'CapDescriptor'() -> '9593755465305995440'().

'CapDescriptor'([]) -> '9593755465305995440'().

'9593755465305995440'() ->
    #schema_node{module = rpc_capnp, name = 'CapDescriptor', id = 9593755465305995440,
		 scope = 12903543124727603792, src = <<"rpc.capnp:CapDescriptor">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, none, #field{name = none, kind = void}},
					     {1, senderHosted,
					      #field{name = senderHosted, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}}},
					     {2, senderPromise,
					      #field{name = senderPromise, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}}},
					     {3, receiverHosted,
					      #field{name = receiverHosted, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}}},
					     {4, receiverAnswer,
					      #field{name = receiverAnswer,
						     kind = #ptr{type = {struct, 15564635848320162976}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {5, thirdPartyHosted,
					      #field{name = thirdPartyHosted,
						     kind =
							 #ptr{type = {struct, 15235686326393111165}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.

'Payload'() -> '11100916931204903995'().

'Payload'([]) -> '11100916931204903995'().

'11100916931204903995'() ->
    #schema_node{module = rpc_capnp, name = 'Payload', id = 11100916931204903995,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Payload">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = content, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = capTable,
					 kind =
					     #ptr{type = {list, {struct, 9593755465305995440}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'MessageTarget'() -> '10789521159760378817'().

'MessageTarget'([]) -> '10789521159760378817'().

'10789521159760378817'() ->
    #schema_node{module = rpc_capnp, name = 'MessageTarget', id = 10789521159760378817,
		 scope = 12903543124727603792, src = <<"rpc.capnp:MessageTarget">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, importedCap,
					      #field{name = importedCap, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}},
					     {1, promisedAnswer,
					      #field{name = promisedAnswer,
						     kind =
							 #ptr{type = {struct, 15564635848320162976}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 32, default = <<0, 0>>},
			     fields = []}}.

'Join'() -> '18149955118657700271'().

'Join'([]) -> '18149955118657700271'().

'18149955118657700271'() ->
    #schema_node{module = rpc_capnp, name = 'Join', id = 18149955118657700271,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Join">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = keyPart,
					 kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Accept'() -> '15332985841292492822'().

'Accept'([]) -> '15332985841292492822'().

'15332985841292492822'() ->
    #schema_node{module = rpc_capnp, name = 'Accept', id = 15332985841292492822,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Accept">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = provision, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = embargo, kind = #data{type = bool, align = 39, default = <<0:1>>}}]}}.

'Provide'() -> '11270825879279873114'().

'Provide'([]) -> '11270825879279873114'().

'11270825879279873114'() ->
    #schema_node{module = rpc_capnp, name = 'Provide', id = 11270825879279873114,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Provide">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = recipient,
					 kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Delete'() -> '9666541409743531671'().

'Delete'([]) -> '9666541409743531671'().

'9666541409743531671'() ->
    #schema_node{module = rpc_capnp, name = 'Delete', id = 9666541409743531671,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Delete">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = objectId,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Restore'() -> '17009130564474155176'().

'Restore'([]) -> '17009130564474155176'().

'17009130564474155176'() ->
    #schema_node{module = rpc_capnp, name = 'Restore', id = 17009130564474155176,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Restore">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = objectId,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Save'() -> '16433336749162137644'().

'Save'([]) -> '16433336749162137644'().

'16433336749162137644'() ->
    #schema_node{module = rpc_capnp, name = 'Save', id = 16433336749162137644,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Save">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = target,
					 kind =
					     #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Disembargo'() -> '17970548384007534353'().

'Disembargo'([context]) -> '15376050949367520589'();
'Disembargo'([]) -> '17970548384007534353'().

'17970548384007534353'() ->
    #schema_node{module = rpc_capnp, name = 'Disembargo', id = 17970548384007534353,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Disembargo">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = context, kind = #group{id = 15376050949367520589}}]}}.

'15376050949367520589'() ->
    #schema_node{module = rpc_capnp, name = ['Disembargo', context], id = 15376050949367520589,
		 scope = 17970548384007534353, src = <<"rpc.capnp:Disembargo.context">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, senderLoopback,
					      #field{name = senderLoopback, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}},
					     {1, receiverLoopback,
					      #field{name = receiverLoopback, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}},
					     {2, accept, #field{name = accept, kind = void}},
					     {3, provide,
					      #field{name = provide, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}}]},
				       align = 32, default = <<0, 0>>},
			     fields = []}}.

'Release'() -> '12473400923157197975'().

'Release'([]) -> '12473400923157197975'().

'12473400923157197975'() ->
    #schema_node{module = rpc_capnp, name = 'Release', id = 12473400923157197975,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Release">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = referenceCount, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}}]}}.

'Resolve'() -> '13529541526594062446'().

'Resolve'([]) -> '13529541526594062446'().

'13529541526594062446'() ->
    #schema_node{module = rpc_capnp, name = 'Resolve', id = 13529541526594062446,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Resolve">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, cap,
					      #field{name = cap,
						     kind = #ptr{type = {struct, 9593755465305995440}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {1, exception,
					      #field{name = exception,
						     kind =
							 #ptr{type = {struct, 15430940935639230746}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 32, default = <<0, 0>>},
			     fields =
				 [#field{name = promiseId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}]}}.

'Finish'() -> '15239388059401719395'().

'Finish'([]) -> '15239388059401719395'().

'15239388059401719395'() ->
    #schema_node{module = rpc_capnp, name = 'Finish', id = 15239388059401719395,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Finish">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = releaseResultCaps, kind = #data{type = bool, align = 39, default = <<1:1>>}}]}}.

'Return'() -> '11392333052105676602'().

'Return'([]) -> '11392333052105676602'().

'11392333052105676602'() ->
    #schema_node{module = rpc_capnp, name = 'Return', id = 11392333052105676602,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Return">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, results,
					      #field{name = results,
						     kind = #ptr{type = {struct, 11100916931204903995}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {1, exception,
					      #field{name = exception,
						     kind = #ptr{type = {struct, 15430940935639230746}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {2, canceled, #field{name = canceled, kind = void}},
					     {3, resultsSentElsewhere, #field{name = resultsSentElsewhere, kind = void}},
					     {4, takeFromOtherQuestion,
					      #field{name = takeFromOtherQuestion,
						     kind = #data{type = uint32, align = 64, default = <<0, 0, 0, 0>>}}},
					     {5, acceptFromThirdParty,
					      #field{name = acceptFromThirdParty,
						     kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 48, default = <<0, 0>>},
			     fields =
				 [#field{name = answerId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = releaseParamCaps, kind = #data{type = bool, align = 39, default = <<1:1>>}}]}}.

'Call'() -> '9469473312751832276'().

'Call'([sendResultsTo]) -> '15774052265921044377'();
'Call'([]) -> '9469473312751832276'().

'9469473312751832276'() ->
    #schema_node{module = rpc_capnp, name = 'Call', id = 9469473312751832276,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Call">>,
		 kind =
		     #struct{dsize = 3, psize = 3, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = questionId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = interfaceId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = methodId, kind = #data{type = uint16, align = 32, default = <<0, 0>>}},
				  #field{name = params,
					 kind = #ptr{type = {struct, 11100916931204903995}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = sendResultsTo, kind = #group{id = 15774052265921044377}},
				  #field{name = allowThirdPartyTailCall,
					 kind = #data{type = bool, align = 135, default = <<0:1>>}}]}}.

'15774052265921044377'() ->
    #schema_node{module = rpc_capnp, name = ['Call', sendResultsTo], id = 15774052265921044377,
		 scope = 9469473312751832276, src = <<"rpc.capnp:Call.sendResultsTo">>,
		 kind =
		     #struct{dsize = 3, psize = 3, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, caller, #field{name = caller, kind = void}},
					     {1, yourself, #field{name = yourself, kind = void}},
					     {2, thirdParty,
					      #field{name = thirdParty,
						     kind = #ptr{type = object, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 48, default = <<0, 0>>},
			     fields = []}}.

'Message'() -> '10500036013887172658'().

'Message'([]) -> '10500036013887172658'().

'10500036013887172658'() ->
    #schema_node{module = rpc_capnp, name = 'Message', id = 10500036013887172658,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Message">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, unimplemented,
					      #field{name = unimplemented,
						     kind = #ptr{type = {struct, 10500036013887172658}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {1, abort,
					      #field{name = abort,
						     kind = #ptr{type = {struct, 15430940935639230746}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {2, call,
					      #field{name = call,
						     kind = #ptr{type = {struct, 9469473312751832276}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {3, return,
					      #field{name = return,
						     kind = #ptr{type = {struct, 11392333052105676602}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {4, finish,
					      #field{name = finish,
						     kind = #ptr{type = {struct, 15239388059401719395}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {5, resolve,
					      #field{name = resolve,
						     kind = #ptr{type = {struct, 13529541526594062446}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {6, release,
					      #field{name = release,
						     kind = #ptr{type = {struct, 12473400923157197975}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {7, save,
					      #field{name = save,
						     kind = #ptr{type = {struct, 16433336749162137644}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {8, restore,
					      #field{name = restore,
						     kind = #ptr{type = {struct, 17009130564474155176}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {9, delete,
					      #field{name = delete,
						     kind = #ptr{type = {struct, 9666541409743531671}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {10, provide,
					      #field{name = provide,
						     kind = #ptr{type = {struct, 11270825879279873114}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {11, accept,
					      #field{name = accept,
						     kind = #ptr{type = {struct, 15332985841292492822}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {12, join,
					      #field{name = join,
						     kind = #ptr{type = {struct, 18149955118657700271}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {13, disembargo,
					      #field{name = disembargo,
						     kind =
							 #ptr{type = {struct, 17970548384007534353}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.