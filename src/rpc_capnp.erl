-file("rpc.capnp", 1).

%% This file was generated 2016-08-18 19:43:19 UTC by ecapnp 0.2.
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
				 [#field{id = 0, name = reason, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 1, name = isCallersFault, kind = #data{type = bool, align = 7, default = <<0:1>>}},
				  #field{id = 2, name = durability,
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
				 [#field{id = 0, name = id,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = vineId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}]}}.

'SturdyRef'() -> '14883405629196290303'().

'SturdyRef'([]) -> '14883405629196290303'().

'14883405629196290303'() ->
    #schema_node{module = rpc_capnp, name = 'SturdyRef', id = 14883405629196290303,
		 scope = 12903543124727603792, src = <<"rpc.capnp:SturdyRef">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = hostId,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = objectId,
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
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = transform,
					 kind =
					     #ptr{type = {list, {struct, 17516350820840804481}}, idx = 0,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
		 nodes =
		     [17516350820840804481]}.  %% Op

'17516350820840804481'() ->
    #schema_node{module = rpc_capnp, name = ['PromisedAnswer', 'Op'], id = 17516350820840804481,
		 scope = 15564635848320162976, src = <<"rpc.capnp:PromisedAnswer.Op">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = noop, kind = void},
					     #field{id = 1, name = getPointerField,
						    kind = #data{type = uint16, align = 16, default = <<0, 0>>}}]},
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
					    [#field{id = 0, name = none, kind = void},
					     #field{id = 1, name = senderHosted,
						    kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 2, name = senderPromise,
						    kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 3, name = receiverHosted,
						    kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 4, name = receiverAnswer,
						    kind = #ptr{type = {struct, 15564635848320162976}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 5, name = thirdPartyHosted,
						    kind =
							#ptr{type = {struct, 15235686326393111165}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
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
				 [#field{id = 0, name = content,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = capTable,
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
					    [#field{id = 0, name = importedCap,
						    kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
					     #field{id = 1, name = promisedAnswer,
						    kind =
							#ptr{type = {struct, 15564635848320162976}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
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
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = keyPart,
					 kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Accept'() -> '15332985841292492822'().

'Accept'([]) -> '15332985841292492822'().

'15332985841292492822'() ->
    #schema_node{module = rpc_capnp, name = 'Accept', id = 15332985841292492822,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Accept">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = provision,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = embargo, kind = #data{type = bool, align = 39, default = <<0:1>>}}]}}.

'Provide'() -> '11270825879279873114'().

'Provide'([]) -> '11270825879279873114'().

'11270825879279873114'() ->
    #schema_node{module = rpc_capnp, name = 'Provide', id = 11270825879279873114,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Provide">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = recipient,
					 kind = #ptr{type = object, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Delete'() -> '9666541409743531671'().

'Delete'([]) -> '9666541409743531671'().

'9666541409743531671'() ->
    #schema_node{module = rpc_capnp, name = 'Delete', id = 9666541409743531671,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Delete">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = objectId,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Restore'() -> '17009130564474155176'().

'Restore'([]) -> '17009130564474155176'().

'17009130564474155176'() ->
    #schema_node{module = rpc_capnp, name = 'Restore', id = 17009130564474155176,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Restore">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = objectId,
					 kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Save'() -> '16433336749162137644'().

'Save'([]) -> '16433336749162137644'().

'16433336749162137644'() ->
    #schema_node{module = rpc_capnp, name = 'Save', id = 16433336749162137644,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Save">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = target,
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
				 [#field{id = 0, name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = context, kind = #group{id = 15376050949367520589}}]}}.

'15376050949367520589'() ->
    #schema_node{module = rpc_capnp, name = ['Disembargo', context], id = 15376050949367520589,
		 scope = 17970548384007534353, src = <<"rpc.capnp:Disembargo.context">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = senderLoopback,
						    kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
					     #field{id = 1, name = receiverLoopback,
						    kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
					     #field{id = 2, name = accept, kind = void},
					     #field{id = 3, name = provide, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}]},
				       align = 32, default = <<0, 0>>},
			     fields = []}}.

'Release'() -> '12473400923157197975'().

'Release'([]) -> '12473400923157197975'().

'12473400923157197975'() ->
    #schema_node{module = rpc_capnp, name = 'Release', id = 12473400923157197975,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Release">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = id, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = referenceCount,
					 kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}}]}}.

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
					    [#field{id = 0, name = cap,
						    kind = #ptr{type = {struct, 9593755465305995440}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 1, name = exception,
						    kind =
							#ptr{type = {struct, 15430940935639230746}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
				       align = 32, default = <<0, 0>>},
			     fields =
				 [#field{id = 0, name = promiseId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}]}}.

'Finish'() -> '15239388059401719395'().

'Finish'([]) -> '15239388059401719395'().

'15239388059401719395'() ->
    #schema_node{module = rpc_capnp, name = 'Finish', id = 15239388059401719395,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Finish">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = releaseResultCaps,
					 kind = #data{type = bool, align = 39, default = <<1:1>>}}]}}.

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
					    [#field{id = 0, name = results,
						    kind = #ptr{type = {struct, 11100916931204903995}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 1, name = exception,
						    kind = #ptr{type = {struct, 15430940935639230746}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 2, name = canceled, kind = void},
					     #field{id = 3, name = resultsSentElsewhere, kind = void},
					     #field{id = 4, name = takeFromOtherQuestion,
						    kind = #data{type = uint32, align = 64, default = <<0, 0, 0, 0>>}},
					     #field{id = 5, name = acceptFromThirdParty,
						    kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
				       align = 48, default = <<0, 0>>},
			     fields =
				 [#field{id = 0, name = answerId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = releaseParamCaps,
					 kind = #data{type = bool, align = 39, default = <<1:1>>}}]}}.

'Call'() -> '9469473312751832276'().

'Call'([sendResultsTo]) -> '15774052265921044377'();
'Call'([]) -> '9469473312751832276'().

'9469473312751832276'() ->
    #schema_node{module = rpc_capnp, name = 'Call', id = 9469473312751832276,
		 scope = 12903543124727603792, src = <<"rpc.capnp:Call">>,
		 kind =
		     #struct{dsize = 3, psize = 3, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = questionId,
					 kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = target,
					 kind = #ptr{type = {struct, 10789521159760378817}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = interfaceId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 3, name = methodId, kind = #data{type = uint16, align = 32, default = <<0, 0>>}},
				  #field{id = 4, name = params,
					 kind = #ptr{type = {struct, 11100916931204903995}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 5, name = sendResultsTo, kind = #group{id = 15774052265921044377}},
				  #field{id = 6, name = allowThirdPartyTailCall,
					 kind = #data{type = bool, align = 135, default = <<0:1>>}}]}}.

'15774052265921044377'() ->
    #schema_node{module = rpc_capnp, name = ['Call', sendResultsTo], id = 15774052265921044377,
		 scope = 9469473312751832276, src = <<"rpc.capnp:Call.sendResultsTo">>,
		 kind =
		     #struct{dsize = 3, psize = 3, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = caller, kind = void}, #field{id = 1, name = yourself, kind = void},
					     #field{id = 2, name = thirdParty,
						    kind = #ptr{type = object, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
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
					    [#field{id = 0, name = unimplemented,
						    kind = #ptr{type = {struct, 10500036013887172658}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 1, name = abort,
						    kind = #ptr{type = {struct, 15430940935639230746}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 2, name = call,
						    kind = #ptr{type = {struct, 9469473312751832276}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 3, name = return,
						    kind = #ptr{type = {struct, 11392333052105676602}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 4, name = finish,
						    kind = #ptr{type = {struct, 15239388059401719395}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 5, name = resolve,
						    kind = #ptr{type = {struct, 13529541526594062446}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 6, name = release,
						    kind = #ptr{type = {struct, 12473400923157197975}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 7, name = save,
						    kind = #ptr{type = {struct, 16433336749162137644}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 8, name = restore,
						    kind = #ptr{type = {struct, 17009130564474155176}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 9, name = delete,
						    kind = #ptr{type = {struct, 9666541409743531671}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 10, name = provide,
						    kind = #ptr{type = {struct, 11270825879279873114}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 11, name = accept,
						    kind = #ptr{type = {struct, 15332985841292492822}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 12, name = join,
						    kind = #ptr{type = {struct, 18149955118657700271}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 13, name = disembargo,
						    kind =
							#ptr{type = {struct, 17970548384007534353}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.