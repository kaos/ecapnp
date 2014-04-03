%% This file was generated 2014-04-03 09:35:03 UTC by ecapnp 0.2.
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

-include_lib("ecapnp/include/ecapnp.hrl").

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
    #schema_node{module = ?MODULE, name = root, id = 11435534567900897652, scope = 0,
		 src = <<"addressbook.capnp">>, annotations = [{13386661402618388268, <<"addressbook">>}],
		 kind = file,
		 nodes =
		     [10988939875124296728,  %% Person
		      17957216978475721012]}.  %% AddressBook

'AddressBook'() -> '17957216978475721012'().

'AddressBook'([]) -> '17957216978475721012'().

'17957216978475721012'() ->
    #schema_node{module = ?MODULE, name = 'AddressBook', id = 17957216978475721012,
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
    #schema_node{module = ?MODULE, name = 'Person', id = 10988939875124296728,
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
    #schema_node{module = ?MODULE, name = ['Person', 'PhoneNumber'], id = 9317543775882349264,
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
    #schema_node{module = ?MODULE, name = ['Person', 'PhoneNumber', 'Type'], id = 10511609358742521391,
		 scope = 9317543775882349264, src = <<"addressbook.capnp:Person.PhoneNumber.Type">>,
		 kind = #enum{values = [{0, mobile}, {1, home}, {2, work}]}}.

'13477914502553102653'() ->
    #schema_node{module = ?MODULE, name = ['Person', employment], id = 13477914502553102653,
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