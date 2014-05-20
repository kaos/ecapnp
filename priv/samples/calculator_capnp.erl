-file("calculator.capnp", 1).

%% This file was generated 2014-05-15 13:58:14 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(calculator_capnp).

-vsn(9589583151133806923).

-export([schema/1, 'Calculator'/0, 'Calculator'/1, '10923537602090224694'/0,
	 '15292209801435843861'/0, '15678475764784139622'/0, '14116142932258867410'/0,
	 '16626840274624768034'/0, '15227555948799047000'/0, '17143016017778443156'/0,
	 '13898297529173597869'/0, '12795114529121400599'/0, '9769222902479511526'/0,
	 '15100494197045627936'/0, '9983863225544066352'/0, '10170522573213587144'/0,
	 '17476144387247758473'/0, '9345430975918089745'/0, '13478898619544909524'/0, root/0, root/1,
	 '9589583151133806923'/0]).

-types([{10923537602090224694, 'Calculator'}, {15292209801435843861, ['Calculator', 'Expression']},
	{15678475764784139622, ['Calculator', 'Expression', call]},
	{14116142932258867410, ['Calculator', 'Value']},
	{16626840274624768034, ['Calculator', 'Value', [read, '$Results']]},
	{15227555948799047000, ['Calculator', 'Value', [read, '$Params']]},
	{17143016017778443156, ['Calculator', 'Function']},
	{13898297529173597869, ['Calculator', 'Function', [call, '$Results']]},
	{12795114529121400599, ['Calculator', 'Function', [call, '$Params']]},
	{9769222902479511526, ['Calculator', 'Operator']},
	{15100494197045627936, ['Calculator', [getOperator, '$Results']]},
	{9983863225544066352, ['Calculator', [getOperator, '$Params']]},
	{10170522573213587144, ['Calculator', [defFunction, '$Results']]},
	{17476144387247758473, ['Calculator', [defFunction, '$Params']]},
	{9345430975918089745, ['Calculator', [evaluate, '$Results']]},
	{13478898619544909524, ['Calculator', [evaluate, '$Params']]}, {9589583151133806923, root}]).

-file("/home/kaos/src/erl/libs/ecapnp/include/ecapnp_schema.hrl", 1).

-ecapnp_schema_version(3).

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

-record(field, {id, name, kind, annotations = []}).

-record(ptr, {type, idx = 0, default = null}).

-type({{record, ptr},
       [{typed_record_field, {record_field, 60, {atom, 60, type}},
	 {type, 60, union, [{atom, 60, undefined}, {type, 60, term, []}]}},
	{typed_record_field, {record_field, 61, {atom, 61, idx}, {integer, 61, 0}},
	 {remote_type, 61, [{atom, 61, ecapnp}, {atom, 61, ptr_index}, []]}},
	{typed_record_field, {record_field, 62, {atom, 62, default}, {atom, 62, null}},
	 {remote_type, 62, [{atom, 62, ecapnp}, {atom, 62, value}, []]}}],
       []}).

-record(data, {type, align = 0, default}).

-type({{record, data},
       [{typed_record_field, {record_field, 66, {atom, 66, type}},
	 {type, 66, union, [{atom, 66, undefined}, {type, 66, term, []}]}},
	{typed_record_field, {record_field, 67, {atom, 67, align}, {integer, 67, 0}},
	 {remote_type, 67, [{atom, 67, ecapnp}, {atom, 67, bit_count}, []]}},
	{typed_record_field, {record_field, 68, {atom, 68, default}},
	 {type, 68, union,
	  [{atom, 68, undefined}, {remote_type, 68, [{atom, 68, ecapnp}, {atom, 68, value}, []]}]}}],
       []}).

-record(group, {id = 0}).

-type({{record, group},
       [{typed_record_field, {record_field, 72, {atom, 72, id}, {integer, 72, 0}},
	 {remote_type, 72, [{atom, 72, ecapnp}, {atom, 72, type_id}, []]}}],
       []}).

-record(method, {id, name, paramType, resultType}).

-file("calculator.capnp", 1).

schema(10923537602090224694) -> '10923537602090224694'();
schema('Calculator') -> '10923537602090224694'();
schema(['Calculator']) -> '10923537602090224694'();
schema(15292209801435843861) -> '15292209801435843861'();
schema(['Calculator', 'Expression']) -> '15292209801435843861'();
schema(15678475764784139622) -> '15678475764784139622'();
schema(['Calculator', 'Expression', call]) -> '15678475764784139622'();
schema(14116142932258867410) -> '14116142932258867410'();
schema(['Calculator', 'Value']) -> '14116142932258867410'();
schema(16626840274624768034) -> '16626840274624768034'();
schema(['Calculator', 'Value', [read, '$Results']]) -> '16626840274624768034'();
schema(15227555948799047000) -> '15227555948799047000'();
schema(['Calculator', 'Value', [read, '$Params']]) -> '15227555948799047000'();
schema(17143016017778443156) -> '17143016017778443156'();
schema(['Calculator', 'Function']) -> '17143016017778443156'();
schema(13898297529173597869) -> '13898297529173597869'();
schema(['Calculator', 'Function', [call, '$Results']]) -> '13898297529173597869'();
schema(12795114529121400599) -> '12795114529121400599'();
schema(['Calculator', 'Function', [call, '$Params']]) -> '12795114529121400599'();
schema(9769222902479511526) -> '9769222902479511526'();
schema(['Calculator', 'Operator']) -> '9769222902479511526'();
schema(15100494197045627936) -> '15100494197045627936'();
schema(['Calculator', [getOperator, '$Results']]) -> '15100494197045627936'();
schema(9983863225544066352) -> '9983863225544066352'();
schema(['Calculator', [getOperator, '$Params']]) -> '9983863225544066352'();
schema(10170522573213587144) -> '10170522573213587144'();
schema(['Calculator', [defFunction, '$Results']]) -> '10170522573213587144'();
schema(17476144387247758473) -> '17476144387247758473'();
schema(['Calculator', [defFunction, '$Params']]) -> '17476144387247758473'();
schema(9345430975918089745) -> '9345430975918089745'();
schema(['Calculator', [evaluate, '$Results']]) -> '9345430975918089745'();
schema(13478898619544909524) -> '13478898619544909524'();
schema(['Calculator', [evaluate, '$Params']]) -> '13478898619544909524'();
schema(9589583151133806923) -> '9589583151133806923'();
schema(root) -> '9589583151133806923'();
schema([root]) -> '9589583151133806923'();
schema(_) -> undefined.

root() -> '9589583151133806923'().

root([]) -> '9589583151133806923'().

'9589583151133806923'() ->
    #schema_node{module = calculator_capnp, name = root, id = 9589583151133806923, scope = 0,
		 src = <<"calculator.capnp">>, kind = file,
		 nodes =
		     [10923537602090224694]}.  %% Calculator

'Calculator'() -> '10923537602090224694'().

'Calculator'(['Expression']) -> '15292209801435843861'();
'Calculator'(['Expression', call]) -> '15678475764784139622'();
'Calculator'(['Value']) -> '14116142932258867410'();
'Calculator'(['Value', [read, '$Results']]) -> '16626840274624768034'();
'Calculator'(['Value', [read, '$Params']]) -> '15227555948799047000'();
'Calculator'(['Function']) -> '17143016017778443156'();
'Calculator'(['Function', [call, '$Results']]) -> '13898297529173597869'();
'Calculator'(['Function', [call, '$Params']]) -> '12795114529121400599'();
'Calculator'(['Operator']) -> '9769222902479511526'();
'Calculator'([[getOperator, '$Results']]) -> '15100494197045627936'();
'Calculator'([[getOperator, '$Params']]) -> '9983863225544066352'();
'Calculator'([[defFunction, '$Results']]) -> '10170522573213587144'();
'Calculator'([[defFunction, '$Params']]) -> '17476144387247758473'();
'Calculator'([[evaluate, '$Results']]) -> '9345430975918089745'();
'Calculator'([[evaluate, '$Params']]) -> '13478898619544909524'();
'Calculator'([]) -> '10923537602090224694'().

'10923537602090224694'() ->
    #schema_node{module = calculator_capnp, name = 'Calculator', id = 10923537602090224694,
		 scope = 9589583151133806923, src = <<"calculator.capnp:Calculator">>,
		 kind =
		     #interface{extends = [],
				methods =
				    [#method{id = 0, name = evaluate, paramType = 13478898619544909524,
					     resultType = 9345430975918089745},
				     #method{id = 1, name = defFunction, paramType = 17476144387247758473,
					     resultType = 10170522573213587144},
				     #method{id = 2, name = getOperator, paramType = 9983863225544066352,
					     resultType = 15100494197045627936}]},
		 nodes =
		     [15292209801435843861,  %% Expression
		      14116142932258867410,  %% Value
		      17143016017778443156,  %% Function
		      9769222902479511526]}.  %% Operator

'15292209801435843861'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Expression'],
		 id = 15292209801435843861, scope = 10923537602090224694,
		 src = <<"calculator.capnp:Calculator.Expression">>,
		 kind =
		     #struct{dsize = 2, psize = 2, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = literal,
						    kind = #data{type = float64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 1, name = previousResult,
						    kind =
							#ptr{type = {interface, 14116142932258867410}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 2, name = parameter, kind = #data{type = uint32, align = 0, default = <<0, 0, 0, 0>>}},
					     #field{id = 3, name = call, kind = #group{id = 15678475764784139622}}]},
				       align = 64, default = <<0, 0>>},
			     fields = []}}.

'15678475764784139622'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Expression', call],
		 id = 15678475764784139622, scope = 15292209801435843861,
		 src = <<"calculator.capnp:Calculator.Expression.call">>,
		 kind =
		     #struct{dsize = 2, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = function,
					 kind =
					     #ptr{type = {interface, 17143016017778443156}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = params,
					 kind =
					     #ptr{type = {list, {struct, 15292209801435843861}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'14116142932258867410'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Value'], id = 14116142932258867410,
		 scope = 10923537602090224694, src = <<"calculator.capnp:Calculator.Value">>,
		 kind =
		     #interface{extends = [],
				methods =
				    [#method{id = 0, name = read, paramType = 15227555948799047000,
					     resultType = 16626840274624768034}]}}.

'16626840274624768034'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Value', [read, '$Results']],
		 id = 16626840274624768034, scope = 0, src = <<"calculator.capnp:Calculator.Value.read$Results">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{id = 0, name = value,
					 kind = #data{type = float64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'15227555948799047000'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Value', [read, '$Params']],
		 id = 15227555948799047000, scope = 0, src = <<"calculator.capnp:Calculator.Value.read$Params">>,
		 kind = #struct{dsize = 0, psize = 0, esize = empty, union_field = none, fields = []}}.

'17143016017778443156'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Function'],
		 id = 17143016017778443156, scope = 10923537602090224694,
		 src = <<"calculator.capnp:Calculator.Function">>,
		 kind =
		     #interface{extends = [],
				methods =
				    [#method{id = 0, name = call, paramType = 12795114529121400599,
					     resultType = 13898297529173597869}]}}.

'13898297529173597869'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Function', [call, '$Results']],
		 id = 13898297529173597869, scope = 0, src = <<"calculator.capnp:Calculator.Function.call$Results">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = eightBytes, union_field = none,
			     fields =
				 [#field{id = 0, name = value,
					 kind = #data{type = float64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'12795114529121400599'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Function', [call, '$Params']],
		 id = 12795114529121400599, scope = 0, src = <<"calculator.capnp:Calculator.Function.call$Params">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{id = 0, name = params,
					 kind = #ptr{type = {list, float64}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'9769222902479511526'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', 'Operator'], id = 9769222902479511526,
		 scope = 10923537602090224694, src = <<"calculator.capnp:Calculator.Operator">>,
		 kind = #enum{values = [{0, add}, {1, subtract}, {2, multiply}, {3, divide}]}}.

'15100494197045627936'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', [getOperator, '$Results']],
		 id = 15100494197045627936, scope = 0, src = <<"calculator.capnp:Calculator.getOperator$Results">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{id = 0, name = func,
					 kind =
					     #ptr{type = {interface, 17143016017778443156}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'9983863225544066352'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', [getOperator, '$Params']],
		 id = 9983863225544066352, scope = 0, src = <<"calculator.capnp:Calculator.getOperator$Params">>,
		 kind =
		     #struct{dsize = 1, psize = 0, esize = twoBytes, union_field = none,
			     fields =
				 [#field{id = 0, name = op,
					 kind = #data{type = {enum, 9769222902479511526}, align = 0, default = <<0, 0>>}}]}}.

'10170522573213587144'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', [defFunction, '$Results']],
		 id = 10170522573213587144, scope = 0, src = <<"calculator.capnp:Calculator.defFunction$Results">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{id = 0, name = func,
					 kind =
					     #ptr{type = {interface, 17143016017778443156}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'17476144387247758473'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', [defFunction, '$Params']],
		 id = 17476144387247758473, scope = 0, src = <<"calculator.capnp:Calculator.defFunction$Params">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = paramCount, kind = #data{type = int32, align = 0, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = body,
					 kind =
					     #ptr{type = {struct, 15292209801435843861}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'9345430975918089745'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', [evaluate, '$Results']],
		 id = 9345430975918089745, scope = 0, src = <<"calculator.capnp:Calculator.evaluate$Results">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{id = 0, name = value,
					 kind =
					     #ptr{type = {interface, 14116142932258867410}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'13478898619544909524'() ->
    #schema_node{module = calculator_capnp, name = ['Calculator', [evaluate, '$Params']],
		 id = 13478898619544909524, scope = 0, src = <<"calculator.capnp:Calculator.evaluate$Params">>,
		 kind =
		     #struct{dsize = 0, psize = 1, esize = pointer, union_field = none,
			     fields =
				 [#field{id = 0, name = expression,
					 kind =
					     #ptr{type = {struct, 15292209801435843861}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.