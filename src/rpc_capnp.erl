-file("rpc.capnp", 1).

%% This file was generated 2014-04-04 21:37:54 UTC by ecapnp 0.2.
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
       [{typed_record_field, {record_field, 91, {atom, 91, dsize}, {integer, 91, 0}},
	 {remote_type, 91, [{atom, 91, ecapnp}, {atom, 91, word_count}, []]}},
	{typed_record_field, {record_field, 92, {atom, 92, psize}, {integer, 92, 0}},
	 {remote_type, 92, [{atom, 92, ecapnp}, {atom, 92, ptr_count}, []]}}],
       []}).

-record(list_ref, {size = empty, count = 0}).

-type({{record, list_ref},
       [{typed_record_field, {record_field, 96, {atom, 96, size}, {atom, 96, empty}},
	 {remote_type, 96, [{atom, 96, ecapnp}, {atom, 96, element_size}, []]}},
	{typed_record_field, {record_field, 97, {atom, 97, count}, {integer, 97, 0}},
	 {type, 97, non_neg_integer, []}}],
       []}).

-record(far_ref, {segment = 0, double_far = false}).

-type({{record, far_ref},
       [{typed_record_field, {record_field, 101, {atom, 101, segment}, {integer, 101, 0}},
	 {type, 101, non_neg_integer, []}},
	{typed_record_field, {record_field, 102, {atom, 102, double_far}, {atom, 102, false}},
	 {type, 102, boolean, []}}],
       []}).

-record(interface_ref, {id}).

-record(object, {ref = null, schema = object}).

-type({{record, object},
       [{typed_record_field, {record_field, 110, {atom, 110, ref}, {atom, 110, null}},
	 {remote_type, 110, [{atom, 110, ecapnp}, {atom, 110, ref}, []]}},
	{typed_record_field, {record_field, 111, {atom, 111, schema}, {atom, 111, object}},
	 {type, 111, union,
	  [{atom, 111, object}, {remote_type, 111, [{atom, 111, ecapnp}, {atom, 111, schema_node}, []]}]}}],
       []}).

-record(request, {method, param, interface}).

-type({{record, request},
       [{typed_record_field, {record_field, 117, {atom, 117, method}},
	 {type, 117, union,
	  [{atom, 117, undefined}, {remote_type, 117, [{atom, 117, ecapnp}, {atom, 117, field_name}, []]}]}},
	{typed_record_field, {record_field, 118, {atom, 118, param}},
	 {type, 118, union,
	  [{atom, 118, undefined}, {remote_type, 118, [{atom, 118, ecapnp}, {atom, 118, object}, []]}]}},
	{typed_record_field, {record_field, 119, {atom, 119, interface}},
	 {type, 119, union,
	  [{atom, 119, undefined},
	   {remote_type, 119, [{atom, 119, ecapnp}, {atom, 119, schema_node}, []]}]}}],
       []}).

-record(msg, {schema, alloc = [], data = []}).

-type({{record, msg},
       [{typed_record_field, {record_field, 124, {atom, 124, schema}},
	 {type, 124, union,
	  [{atom, 124, undefined}, {remote_type, 124, [{atom, 124, ecapnp}, {atom, 124, schema}, []]}]}},
	{typed_record_field, {record_field, 125, {atom, 125, alloc}, {nil, 125}},
	 {type, 125, list, [{type, 125, integer, []}]}},
	{typed_record_field, {record_field, 126, {atom, 126, data}, {nil, 126}},
	 {remote_type, 126, [{atom, 126, ecapnp}, {atom, 126, message}, []]}}],
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