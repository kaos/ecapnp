-file("addressbook.capnp", 1).

%% This file was generated 2014-05-02 14:32:02 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(addressbook_capnp).

-vsn(11435534567900897652).

-export([schema/1, 'Person'/0, 'Person'/1, '10988939875124296728'/0, '9317543775882349264'/0,
	 '10511609358742521391'/0, '13477914502553102653'/0, 'AddressBook'/0, 'AddressBook'/1,
	 '17957216978475721012'/0, root/0, root/1, '11435534567900897652'/0]).

-types([{10988939875124296728, 'Person'}, {9317543775882349264, ['Person', 'PhoneNumber']},
	{10511609358742521391, ['Person', 'PhoneNumber', 'Type']},
	{13477914502553102653, ['Person', employment]}, {17957216978475721012, 'AddressBook'},
	{11435534567900897652, root}]).

-import('c++_capnp', ['13386661402618388268'/0]).

-file("/home/kaos/src/erl/libs/ecapnp/include/ecapnp_schema.hrl", 1).

-ecapnp_schema_version(2).

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

-record(field, {name, kind, annotations = []}).

-record(ptr, {type, idx = 0, default = null}).

-type({{record, ptr},
       [{typed_record_field, {record_field, 59, {atom, 59, type}},
	 {type, 59, union, [{atom, 59, undefined}, {type, 59, term, []}]}},
	{typed_record_field, {record_field, 60, {atom, 60, idx}, {integer, 60, 0}},
	 {remote_type, 60, [{atom, 60, ecapnp}, {atom, 60, ptr_index}, []]}},
	{typed_record_field, {record_field, 61, {atom, 61, default}, {atom, 61, null}},
	 {remote_type, 61, [{atom, 61, ecapnp}, {atom, 61, value}, []]}}],
       []}).

-record(data, {type, align = 0, default}).

-type({{record, data},
       [{typed_record_field, {record_field, 65, {atom, 65, type}},
	 {type, 65, union, [{atom, 65, undefined}, {type, 65, term, []}]}},
	{typed_record_field, {record_field, 66, {atom, 66, align}, {integer, 66, 0}},
	 {remote_type, 66, [{atom, 66, ecapnp}, {atom, 66, bit_count}, []]}},
	{typed_record_field, {record_field, 67, {atom, 67, default}},
	 {type, 67, union,
	  [{atom, 67, undefined}, {remote_type, 67, [{atom, 67, ecapnp}, {atom, 67, value}, []]}]}}],
       []}).

-record(group, {id = 0}).

-type({{record, group},
       [{typed_record_field, {record_field, 71, {atom, 71, id}, {integer, 71, 0}},
	 {remote_type, 71, [{atom, 71, ecapnp}, {atom, 71, type_id}, []]}}],
       []}).

-record(method, {id, name, paramType, resultType}).

-file("addressbook.capnp", 1).

schema(10988939875124296728) -> '10988939875124296728'();
schema('Person') -> '10988939875124296728'();
schema(['Person']) -> '10988939875124296728'();
schema(9317543775882349264) -> '9317543775882349264'();
schema(['Person', 'PhoneNumber']) -> '9317543775882349264'();
schema(10511609358742521391) -> '10511609358742521391'();
schema(['Person', 'PhoneNumber', 'Type']) -> '10511609358742521391'();
schema(13477914502553102653) -> '13477914502553102653'();
schema(['Person', employment]) -> '13477914502553102653'();
schema(17957216978475721012) -> '17957216978475721012'();
schema('AddressBook') -> '17957216978475721012'();
schema(['AddressBook']) -> '17957216978475721012'();
schema(11435534567900897652) -> '11435534567900897652'();
schema(root) -> '11435534567900897652'();
schema([root]) -> '11435534567900897652'();
%% Imported from c++_capnp
schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
schema(_) -> undefined.

root() -> '11435534567900897652'().

root([]) -> '11435534567900897652'().

'11435534567900897652'() ->
    #schema_node{module = addressbook_capnp, name = root, id = 11435534567900897652, scope = 0,
		 src = <<"addressbook.capnp">>, annotations = [{13386661402618388268, <<"addressbook">>}],
		 kind = file,
		 nodes =
		     [10988939875124296728,  %% Person
		      17957216978475721012]}.  %% AddressBook

'AddressBook'() -> '17957216978475721012'().

'AddressBook'([]) -> '17957216978475721012'().

'17957216978475721012'() ->
    #schema_node{module = addressbook_capnp, name = 'AddressBook', id = 17957216978475721012,
		 scope = 11435534567900897652, src = <<"addressbook.capnp:AddressBook">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{name = people,
					 kind =
					     #ptr{type = {list, {struct, 10988939875124296728}}, idx = 0,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Person'() -> '10988939875124296728'().

'Person'(['PhoneNumber']) -> '9317543775882349264'();
'Person'(['PhoneNumber', 'Type']) -> '10511609358742521391'();
'Person'([employment]) -> '13477914502553102653'();
'Person'([]) -> '10988939875124296728'().

'10988939875124296728'() ->
    #schema_node{module = addressbook_capnp, name = 'Person', id = 10988939875124296728,
		 scope = 11435534567900897652, src = <<"addressbook.capnp:Person">>,
		 kind =
		     #struct{dsize = 1, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = name, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{name = email, kind = #ptr{type = text, idx = 1, default = <<"">>}},
				  #field{name = phones,
					 kind =
					     #ptr{type = {list, {struct, 9317543775882349264}}, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = employment, kind = #group{id = 13477914502553102653}}]},
		 nodes =
		     [9317543775882349264]}.  %% PhoneNumber

'9317543775882349264'() ->
    #schema_node{module = addressbook_capnp, name = ['Person', 'PhoneNumber'], id = 9317543775882349264,
		 scope = 10988939875124296728, src = <<"addressbook.capnp:Person.PhoneNumber">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = number, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{name = type,
					 kind = #data{type = {enum, 10511609358742521391}, align = 0, default = <<0, 0>>}}]},
		 nodes =
		     [10511609358742521391]}.  %% Type

'10511609358742521391'() ->
    #schema_node{module = addressbook_capnp, name = ['Person', 'PhoneNumber', 'Type'],
		 id = 10511609358742521391, scope = 9317543775882349264,
		 src = <<"addressbook.capnp:Person.PhoneNumber.Type">>,
		 kind = #enum{values = [{0, mobile}, {1, home}, {2, work}]}}.

'13477914502553102653'() ->
    #schema_node{module = addressbook_capnp, name = ['Person', employment], id = 13477914502553102653,
		 scope = 10988939875124296728, src = <<"addressbook.capnp:Person.employment">>,
		 kind =
		     #struct{dsize = 1, psize = 4, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, unemployed, #field{name = unemployed, kind = void}},
					     {1, employer, #field{name = employer, kind = #ptr{type = text, idx = 3, default = <<"">>}}},
					     {2, school, #field{name = school, kind = #ptr{type = text, idx = 3, default = <<"">>}}},
					     {3, selfEmployed, #field{name = selfEmployed, kind = void}}]},
				       align = 32, default = <<0, 0>>},
			     fields = []}}.