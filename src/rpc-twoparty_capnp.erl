-file("rpc-twoparty.capnp", 1).

%% This file was generated 2014-04-04 21:37:51 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('rpc-twoparty_capnp').

-vsn(11638646725519536801).

-export([schema/1, 'Side'/0, 'Side'/1, '11517567629614739868'/0, 'SturdyRefHostId'/0,
	 'SturdyRefHostId'/1, '16579407680674948360'/0, 'ProvisionId'/0, 'ProvisionId'/1,
	 '13298295899470141463'/0, 'RecipientId'/0, 'RecipientId'/1, '9940440221562733249'/0,
	 'ThirdPartyCapId'/0, 'ThirdPartyCapId'/1, '13006195034640135581'/0, 'JoinKeyPart'/0,
	 'JoinKeyPart'/1, '10786842769591618179'/0, 'JoinResult'/0, 'JoinResult'/1, '11323802317489695726'/0,
	 root/0, root/1, '11638646725519536801'/0]).

-types([{11517567629614739868, 'Side'}, {16579407680674948360, 'SturdyRefHostId'},
	{13298295899470141463, 'ProvisionId'}, {9940440221562733249, 'RecipientId'},
	{13006195034640135581, 'ThirdPartyCapId'}, {10786842769591618179, 'JoinKeyPart'},
	{11323802317489695726, 'JoinResult'}, {11638646725519536801, root}]).

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

-file("rpc-twoparty.capnp", 1).

schema(11517567629614739868) -> '11517567629614739868'();
schema('Side') -> '11517567629614739868'();
schema(['Side']) -> '11517567629614739868'();
schema(16579407680674948360) -> '16579407680674948360'();
schema('SturdyRefHostId') -> '16579407680674948360'();
schema(['SturdyRefHostId']) -> '16579407680674948360'();
schema(13298295899470141463) -> '13298295899470141463'();
schema('ProvisionId') -> '13298295899470141463'();
schema(['ProvisionId']) -> '13298295899470141463'();
schema(9940440221562733249) -> '9940440221562733249'();
schema('RecipientId') -> '9940440221562733249'();
schema(['RecipientId']) -> '9940440221562733249'();
schema(13006195034640135581) -> '13006195034640135581'();
schema('ThirdPartyCapId') -> '13006195034640135581'();
schema(['ThirdPartyCapId']) -> '13006195034640135581'();
schema(10786842769591618179) -> '10786842769591618179'();
schema('JoinKeyPart') -> '10786842769591618179'();
schema(['JoinKeyPart']) -> '10786842769591618179'();
schema(11323802317489695726) -> '11323802317489695726'();
schema('JoinResult') -> '11323802317489695726'();
schema(['JoinResult']) -> '11323802317489695726'();
schema(11638646725519536801) -> '11638646725519536801'();
schema(root) -> '11638646725519536801'();
schema([root]) -> '11638646725519536801'();
%% Imported from c++_capnp
schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
schema(_) -> undefined.

root() -> '11638646725519536801'().

root([]) -> '11638646725519536801'().

'11638646725519536801'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = root, id = 11638646725519536801, scope = 0,
		 src = <<"rpc-twoparty.capnp">>, annotations = [{13386661402618388268, <<"capnp::rpc::twoparty">>}],
		 kind = file,
		 nodes =
		     [11517567629614739868,  %% Side
		      16579407680674948360,  %% SturdyRefHostId
		      13298295899470141463,  %% ProvisionId
		      9940440221562733249,  %% RecipientId
		      13006195034640135581,  %% ThirdPartyCapId
		      10786842769591618179,  %% JoinKeyPart
		      11323802317489695726]}.  %% JoinResult

'JoinResult'() -> '11323802317489695726'().

'JoinResult'([]) -> '11323802317489695726'().

'11323802317489695726'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'JoinResult', id = 11323802317489695726,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:JoinResult">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = joinId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = succeeded, kind = #data{type = bool, align = 39, default = <<0:1>>}},
				  #field{name = cap, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'JoinKeyPart'() -> '10786842769591618179'().

'JoinKeyPart'([]) -> '10786842769591618179'().

'10786842769591618179'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'JoinKeyPart', id = 10786842769591618179,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:JoinKeyPart">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{name = joinId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = partCount, kind = #data{type = uint16, align = 32, default = <<0, 0>>}},
				  #field{name = partNum, kind = #data{type = uint16, align = 48, default = <<0, 0>>}}]}}.

'ThirdPartyCapId'() -> '13006195034640135581'().

'ThirdPartyCapId'([]) -> '13006195034640135581'().

'13006195034640135581'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'ThirdPartyCapId', id = 13006195034640135581,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:ThirdPartyCapId">>,
		 kind = #struct{dsize = 0, psize = 0, esize = empty, union_field = none, fields = []}}.

'RecipientId'() -> '9940440221562733249'().

'RecipientId'([]) -> '9940440221562733249'().

'9940440221562733249'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'RecipientId', id = 9940440221562733249,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:RecipientId">>,
		 kind = #struct{dsize = 0, psize = 0, esize = empty, union_field = none, fields = []}}.

'ProvisionId'() -> '13298295899470141463'().

'ProvisionId'([]) -> '13298295899470141463'().

'13298295899470141463'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'ProvisionId', id = 13298295899470141463,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:ProvisionId">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = fourBytes, union_field = none,
			     fields =
				 [#field{name = joinId, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}}]}}.

'SturdyRefHostId'() -> '16579407680674948360'().

'SturdyRefHostId'([]) -> '16579407680674948360'().

'16579407680674948360'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'SturdyRefHostId', id = 16579407680674948360,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:SturdyRefHostId">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = twoBytes, union_field = none,
			     fields =
				 [#field{name = side,
					 kind = #data{type = {enum, 11517567629614739868}, align = 0, default = <<0, 0>>}}]}}.

'Side'() -> '11517567629614739868'().

'Side'([]) -> '11517567629614739868'().

'11517567629614739868'() ->
    #schema_node{module = 'rpc-twoparty_capnp', name = 'Side', id = 11517567629614739868,
		 scope = 11638646725519536801, src = <<"rpc-twoparty.capnp:Side">>,
		 kind = #enum{values = [{0, server}, {1, client}]}}.