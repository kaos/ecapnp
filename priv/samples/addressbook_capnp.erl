%% This file was generated 2014-04-02 11:52:12 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(addressbook_capnp).

-vsn(11435534567900897652).

-export([schema/1, 'Person'/0, '10988939875124296728'/0, 'Person.PhoneNumber'/0,
	 '9317543775882349264'/0, 'Person.PhoneNumber.Type'/0, '10511609358742521391'/0,
	 'Person.employment'/0, '13477914502553102653'/0, 'AddressBook'/0, '17957216978475721012'/0]).

-types([{10988939875124296728, 'Person'}, {9317543775882349264, 'Person.PhoneNumber'},
	{10511609358742521391, 'Person.PhoneNumber.Type'}, {13477914502553102653, 'Person.employment'},
	{17957216978475721012, 'AddressBook'}]).

-include_lib("ecapnp/include/ecapnp.hrl").

'Person'() -> '10988939875124296728'().

'10988939875124296728'() ->
    #schema_node{module = ?MODULE, name = 'Person', id = 10988939875124296728,
		 src = <<"addressbook.capnp:Person">>,
		 kind =
		     #struct{dsize = 1, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{name = name, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = email, kind = #ptr{type = text, idx = 1, default = <<>>}},
				  #field{name = phones,
					 kind =
					     #ptr{type = {list, {struct, 9317543775882349264}}, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = employment, kind = #group{id = 13477914502553102653}}]}}.

'Person.PhoneNumber'() -> '9317543775882349264'().

'9317543775882349264'() ->
    #schema_node{module = ?MODULE, name = 'Person.PhoneNumber', id = 9317543775882349264,
		 src = <<"addressbook.capnp:Person.PhoneNumber">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = number, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = type,
					 kind = #data{type = {enum, 10511609358742521391}, align = 0, default = <<0, 0>>}}]}}.

'Person.PhoneNumber.Type'() -> '10511609358742521391'().

'10511609358742521391'() ->
    #schema_node{module = ?MODULE, name = 'Person.PhoneNumber.Type', id = 10511609358742521391,
		 src = <<"addressbook.capnp:Person.PhoneNumber.Type">>,
		 kind = #enum{values = [{0, mobile}, {1, home}, {2, work}]}}.

'Person.employment'() -> '13477914502553102653'().

'13477914502553102653'() ->
    #schema_node{module = ?MODULE, name = 'Person.employment', id = 13477914502553102653,
		 src = <<"addressbook.capnp:Person.employment">>,
		 kind =
		     #struct{dsize = 1, psize = 4, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, unemployed, #field{name = unemployed, kind = void}},
					     {1, employer, #field{name = employer, kind = #ptr{type = text, idx = 3, default = <<>>}}},
					     {2, school, #field{name = school, kind = #ptr{type = text, idx = 3, default = <<>>}}},
					     {3, selfEmployed, #field{name = selfEmployed, kind = void}}]},
				       align = 32, default = <<0, 0>>},
			     fields = []}}.

'AddressBook'() -> '17957216978475721012'().

'17957216978475721012'() ->
    #schema_node{module = ?MODULE, name = 'AddressBook', id = 17957216978475721012,
		 src = <<"addressbook.capnp:AddressBook">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{name = people,
					 kind =
					     #ptr{type = {list, {struct, 10988939875124296728}}, idx = 0,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

schema('Person') -> schema(10988939875124296728);
schema(10988939875124296728) -> '10988939875124296728'();
schema('Person.PhoneNumber') -> schema(9317543775882349264);
schema(9317543775882349264) -> '9317543775882349264'();
schema('Person.PhoneNumber.Type') -> schema(10511609358742521391);
schema(10511609358742521391) -> '10511609358742521391'();
schema('Person.employment') -> schema(13477914502553102653);
schema(13477914502553102653) -> '13477914502553102653'();
schema('AddressBook') -> schema(17957216978475721012);
schema(17957216978475721012) -> '17957216978475721012'();
schema(_) -> undefined.