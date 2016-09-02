-file("schema.capnp", 1).

%% This file was generated 2014-05-31 07:56:29 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(schema_capnp).

-vsn(12195682960037147353).

-export([schema/1, 'Node'/0, 'Node'/1, '16610026722781537303'/0, '16050641862814319170'/0,
	 '17011813041836786320'/0, '12793219851699983392'/0, '16728431493453586831'/0,
	 '13063450714778629528'/0, '11430331134483579957'/0, 'Field'/0, 'Field'/1, '11145653318641710175'/0,
	 '10930602151629473554'/0, '13515537513213004774'/0, '14626792032033250577'/0,
	 '14133145859926553711'/0, 'Enumerant'/0, 'Enumerant'/1, '10919677598968879693'/0, 'Method'/0,
	 'Method'/1, '10736806783679155584'/0, 'Type'/0, 'Type'/1, '15020482145304562784'/0,
	 '17116997365232503999'/0, '12410354185295152851'/0, '11389172934837766057'/0,
	 '9792858745991129751'/0, 'Value'/0, 'Value'/1, '14853958794117909659'/0, 'Annotation'/0,
	 'Annotation'/1, '17422339044421236034'/0, 'ElementSize'/0, 'ElementSize'/1,
	 '15102134695616452902'/0, 'CodeGeneratorRequest'/0, 'CodeGeneratorRequest'/1,
	 '13818529054586492878'/0, '14981803260258615394'/0, '12560611460656617445'/0, root/0, root/1,
	 '12195682960037147353'/0]).

-types([{16610026722781537303, 'Node'}, {16050641862814319170, ['Node', 'NestedNode']},
	{17011813041836786320, ['Node', annotation]}, {12793219851699983392, ['Node', const]},
	{16728431493453586831, ['Node', interface]}, {13063450714778629528, ['Node', enum]},
	{11430331134483579957, ['Node', struct]}, {11145653318641710175, 'Field'},
	{10930602151629473554, ['Field', noDiscriminant]}, {13515537513213004774, ['Field', ordinal]},
	{14626792032033250577, ['Field', group]}, {14133145859926553711, ['Field', slot]},
	{10919677598968879693, 'Enumerant'}, {10736806783679155584, 'Method'},
	{15020482145304562784, 'Type'}, {17116997365232503999, ['Type', interface]},
	{12410354185295152851, ['Type', struct]}, {11389172934837766057, ['Type', enum]},
	{9792858745991129751, ['Type', list]}, {14853958794117909659, 'Value'},
	{17422339044421236034, 'Annotation'}, {15102134695616452902, 'ElementSize'},
	{13818529054586492878, 'CodeGeneratorRequest'},
	{14981803260258615394, ['CodeGeneratorRequest', 'RequestedFile']},
	{12560611460656617445, ['CodeGeneratorRequest', 'RequestedFile', 'Import']},
	{12195682960037147353, root}]).

-import('c++_capnp', ['13386661402618388268'/0]).

-file("/home/kaos/src/erl/libs/ecapnp/include/ecapnp_schema.hrl", 1).

-ecapnp_schema_version(4).

-record(schema_node,
	{module, name, id = 0, src = <<>>, kind = file, annotations = [], nodes = [], scope = 0}).


-record(struct, {dsize = 0, psize = 0, esize = inlineComposite, union_field = none, fields = []}).


-record(enum, {values = []}).


-record(interface, {extends = [], methods = []}).


-record(const, {field}).

-record(annotation, {type, targets = []}).


-record(field, {id, name, kind, annotations = []}).

-record(ptr, {type, idx = 0, default = <<0:64/integer-little>>}).

-record(data, {type, align = 0, default}).

-record(group, {id = 0}).

-record(method, {id, name, paramType, resultType}).

-file("schema.capnp", 1).

schema(16610026722781537303) -> '16610026722781537303'();
schema('Node') -> '16610026722781537303'();
schema(['Node']) -> '16610026722781537303'();
schema(16050641862814319170) -> '16050641862814319170'();
schema(['Node', 'NestedNode']) -> '16050641862814319170'();
schema(17011813041836786320) -> '17011813041836786320'();
schema(['Node', annotation]) -> '17011813041836786320'();
schema(12793219851699983392) -> '12793219851699983392'();
schema(['Node', const]) -> '12793219851699983392'();
schema(16728431493453586831) -> '16728431493453586831'();
schema(['Node', interface]) -> '16728431493453586831'();
schema(13063450714778629528) -> '13063450714778629528'();
schema(['Node', enum]) -> '13063450714778629528'();
schema(11430331134483579957) -> '11430331134483579957'();
schema(['Node', struct]) -> '11430331134483579957'();
schema(11145653318641710175) -> '11145653318641710175'();
schema('Field') -> '11145653318641710175'();
schema(['Field']) -> '11145653318641710175'();
schema(10930602151629473554) -> '10930602151629473554'();
schema(['Field', noDiscriminant]) -> '10930602151629473554'();
schema(13515537513213004774) -> '13515537513213004774'();
schema(['Field', ordinal]) -> '13515537513213004774'();
schema(14626792032033250577) -> '14626792032033250577'();
schema(['Field', group]) -> '14626792032033250577'();
schema(14133145859926553711) -> '14133145859926553711'();
schema(['Field', slot]) -> '14133145859926553711'();
schema(10919677598968879693) -> '10919677598968879693'();
schema('Enumerant') -> '10919677598968879693'();
schema(['Enumerant']) -> '10919677598968879693'();
schema(10736806783679155584) -> '10736806783679155584'();
schema('Method') -> '10736806783679155584'();
schema(['Method']) -> '10736806783679155584'();
schema(15020482145304562784) -> '15020482145304562784'();
schema('Type') -> '15020482145304562784'();
schema(['Type']) -> '15020482145304562784'();
schema(17116997365232503999) -> '17116997365232503999'();
schema(['Type', interface]) -> '17116997365232503999'();
schema(12410354185295152851) -> '12410354185295152851'();
schema(['Type', struct]) -> '12410354185295152851'();
schema(11389172934837766057) -> '11389172934837766057'();
schema(['Type', enum]) -> '11389172934837766057'();
schema(9792858745991129751) -> '9792858745991129751'();
schema(['Type', list]) -> '9792858745991129751'();
schema(14853958794117909659) -> '14853958794117909659'();
schema('Value') -> '14853958794117909659'();
schema(['Value']) -> '14853958794117909659'();
schema(17422339044421236034) -> '17422339044421236034'();
schema('Annotation') -> '17422339044421236034'();
schema(['Annotation']) -> '17422339044421236034'();
schema(15102134695616452902) -> '15102134695616452902'();
schema('ElementSize') -> '15102134695616452902'();
schema(['ElementSize']) -> '15102134695616452902'();
schema(13818529054586492878) -> '13818529054586492878'();
schema('CodeGeneratorRequest') -> '13818529054586492878'();
schema(['CodeGeneratorRequest']) -> '13818529054586492878'();
schema(14981803260258615394) -> '14981803260258615394'();
schema(['CodeGeneratorRequest', 'RequestedFile']) -> '14981803260258615394'();
schema(12560611460656617445) -> '12560611460656617445'();
schema(['CodeGeneratorRequest', 'RequestedFile', 'Import']) -> '12560611460656617445'();
schema(12195682960037147353) -> '12195682960037147353'();
schema(root) -> '12195682960037147353'();
schema([root]) -> '12195682960037147353'();
%% Imported from c++_capnp
schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
schema(_) -> undefined.

root() -> '12195682960037147353'().

root([]) -> '12195682960037147353'().

'12195682960037147353'() ->
    #schema_node{module = schema_capnp, name = root, id = 12195682960037147353, scope = 0,
		 src = <<"schema.capnp">>, annotations = [{13386661402618388268, <<"capnp::schema">>}], kind = file,
		 nodes =
		     [16610026722781537303,  %% Node
		      11145653318641710175,  %% Field
		      10919677598968879693,  %% Enumerant
		      10736806783679155584,  %% Method
		      15020482145304562784,  %% Type
		      14853958794117909659,  %% Value
		      17422339044421236034,  %% Annotation
		      15102134695616452902,  %% ElementSize
		      13818529054586492878]}.  %% CodeGeneratorRequest

'CodeGeneratorRequest'() -> '13818529054586492878'().

'CodeGeneratorRequest'(['RequestedFile']) -> '14981803260258615394'();
'CodeGeneratorRequest'(['RequestedFile', 'Import']) -> '12560611460656617445'();
'CodeGeneratorRequest'([]) -> '13818529054586492878'().

'13818529054586492878'() ->
    #schema_node{module = schema_capnp, name = 'CodeGeneratorRequest', id = 13818529054586492878,
		 scope = 12195682960037147353, src = <<"schema.capnp:CodeGeneratorRequest">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = nodes,
					 kind =
					     #ptr{type = {list, {struct, 16610026722781537303}}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = requestedFiles,
					 kind =
					     #ptr{type = {list, {struct, 14981803260258615394}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
		 nodes =
		     [14981803260258615394]}.  %% RequestedFile

'14981803260258615394'() ->
    #schema_node{module = schema_capnp, name = ['CodeGeneratorRequest', 'RequestedFile'],
		 id = 14981803260258615394, scope = 13818529054586492878,
		 src = <<"schema.capnp:CodeGeneratorRequest.RequestedFile">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = id,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = filename, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 2, name = imports,
					 kind =
					     #ptr{type = {list, {struct, 12560611460656617445}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
		 nodes =
		     [12560611460656617445]}.  %% Import

'12560611460656617445'() ->
    #schema_node{module = schema_capnp, name = ['CodeGeneratorRequest', 'RequestedFile', 'Import'],
		 id = 12560611460656617445, scope = 14981803260258615394,
		 src = <<"schema.capnp:CodeGeneratorRequest.RequestedFile.Import">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = id,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = name, kind = #ptr{type = text, idx = 0, default = <<"">>}}]}}.

'ElementSize'() -> '15102134695616452902'().

'ElementSize'([]) -> '15102134695616452902'().

'15102134695616452902'() ->
    #schema_node{module = schema_capnp, name = 'ElementSize', id = 15102134695616452902,
		 scope = 12195682960037147353, src = <<"schema.capnp:ElementSize">>,
		 kind =
		     #enum{values =
			       [{0, empty}, {1, bit}, {2, byte}, {3, twoBytes}, {4, fourBytes}, {5, eightBytes}, {6, pointer},
				{7, inlineComposite}]}}.

'Annotation'() -> '17422339044421236034'().

'Annotation'([]) -> '17422339044421236034'().

'17422339044421236034'() ->
    #schema_node{module = schema_capnp, name = 'Annotation', id = 17422339044421236034,
		 scope = 12195682960037147353, src = <<"schema.capnp:Annotation">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = id,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = value,
					 kind =
					     #ptr{type = {struct, 14853958794117909659}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Value'() -> '14853958794117909659'().

'Value'([]) -> '14853958794117909659'().

'14853958794117909659'() ->
    #schema_node{module = schema_capnp, name = 'Value', id = 14853958794117909659,
		 scope = 12195682960037147353, src = <<"schema.capnp:Value">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = void, kind = void},
					     #field{id = 1, name = bool, kind = #data{type = bool, align = 23, default = <<0:1>>}},
					     #field{id = 2, name = int8, kind = #data{type = int8, align = 16, default = <<0>>}},
					     #field{id = 3, name = int16, kind = #data{type = int16, align = 16, default = <<0, 0>>}},
					     #field{id = 4, name = int32, kind = #data{type = int32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 5, name = int64,
						    kind = #data{type = int64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 6, name = uint8, kind = #data{type = uint8, align = 16, default = <<0>>}},
					     #field{id = 7, name = uint16, kind = #data{type = uint16, align = 16, default = <<0, 0>>}},
					     #field{id = 8, name = uint32, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 9, name = uint64,
						    kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 10, name = float32, kind = #data{type = float32, align = 32, default = <<0, 0, 0, 0>>}},
					     #field{id = 11, name = float64,
						    kind = #data{type = float64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 12, name = text, kind = #ptr{type = text, idx = 0, default = <<"">>}},
					     #field{id = 13, name = data, kind = #ptr{type = data, idx = 0, default = <<>>}},
					     #field{id = 14, name = list,
						    kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 15, name = enum, kind = #data{type = uint16, align = 16, default = <<0, 0>>}},
					     #field{id = 16, name = struct,
						    kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
					     #field{id = 17, name = interface, kind = void},
					     #field{id = 18, name = anyPointer,
						    kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.

'Type'() -> '15020482145304562784'().

'Type'([interface]) -> '17116997365232503999'();
'Type'([struct]) -> '12410354185295152851'();
'Type'([enum]) -> '11389172934837766057'();
'Type'([list]) -> '9792858745991129751'();
'Type'([]) -> '15020482145304562784'().

'15020482145304562784'() ->
    #schema_node{module = schema_capnp, name = 'Type', id = 15020482145304562784,
		 scope = 12195682960037147353, src = <<"schema.capnp:Type">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = void, kind = void}, #field{id = 1, name = bool, kind = void},
					     #field{id = 2, name = int8, kind = void}, #field{id = 3, name = int16, kind = void},
					     #field{id = 4, name = int32, kind = void}, #field{id = 5, name = int64, kind = void},
					     #field{id = 6, name = uint8, kind = void}, #field{id = 7, name = uint16, kind = void},
					     #field{id = 8, name = uint32, kind = void}, #field{id = 9, name = uint64, kind = void},
					     #field{id = 10, name = float32, kind = void}, #field{id = 11, name = float64, kind = void},
					     #field{id = 12, name = text, kind = void}, #field{id = 13, name = data, kind = void},
					     #field{id = 14, name = list, kind = #group{id = 9792858745991129751}},
					     #field{id = 15, name = enum, kind = #group{id = 11389172934837766057}},
					     #field{id = 16, name = struct, kind = #group{id = 12410354185295152851}},
					     #field{id = 17, name = interface, kind = #group{id = 17116997365232503999}},
					     #field{id = 18, name = anyPointer, kind = void}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.

'17116997365232503999'() ->
    #schema_node{module = schema_capnp, name = ['Type', interface], id = 17116997365232503999,
		 scope = 15020482145304562784, src = <<"schema.capnp:Type.interface">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = typeId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'12410354185295152851'() ->
    #schema_node{module = schema_capnp, name = ['Type', struct], id = 12410354185295152851,
		 scope = 15020482145304562784, src = <<"schema.capnp:Type.struct">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = typeId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'11389172934837766057'() ->
    #schema_node{module = schema_capnp, name = ['Type', enum], id = 11389172934837766057,
		 scope = 15020482145304562784, src = <<"schema.capnp:Type.enum">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = typeId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'9792858745991129751'() ->
    #schema_node{module = schema_capnp, name = ['Type', list], id = 9792858745991129751,
		 scope = 15020482145304562784, src = <<"schema.capnp:Type.list">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = elementType,
					 kind =
					     #ptr{type = {struct, 15020482145304562784}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Method'() -> '10736806783679155584'().

'Method'([]) -> '10736806783679155584'().

'10736806783679155584'() ->
    #schema_node{module = schema_capnp, name = 'Method', id = 10736806783679155584,
		 scope = 12195682960037147353, src = <<"schema.capnp:Method">>,
		 kind =
		     #struct{dsize = 3, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = name, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 1, name = codeOrder, kind = #data{type = uint16, align = 0, default = <<0, 0>>}},
				  #field{id = 2, name = paramStructType,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 3, name = resultStructType,
					 kind = #data{type = uint64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 4, name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Enumerant'() -> '10919677598968879693'().

'Enumerant'([]) -> '10919677598968879693'().

'10919677598968879693'() ->
    #schema_node{module = schema_capnp, name = 'Enumerant', id = 10919677598968879693,
		 scope = 12195682960037147353, src = <<"schema.capnp:Enumerant">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = name, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 1, name = codeOrder, kind = #data{type = uint16, align = 0, default = <<0, 0>>}},
				  #field{id = 2, name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Field'() -> '11145653318641710175'().

'Field'([noDiscriminant]) -> '10930602151629473554'();
'Field'([ordinal]) -> '13515537513213004774'();
'Field'([group]) -> '14626792032033250577'();
'Field'([slot]) -> '14133145859926553711'();
'Field'([]) -> '11145653318641710175'().

'11145653318641710175'() ->
    #schema_node{module = schema_capnp, name = 'Field', id = 11145653318641710175,
		 scope = 12195682960037147353, src = <<"schema.capnp:Field">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = slot, kind = #group{id = 14133145859926553711}},
					     #field{id = 1, name = group, kind = #group{id = 14626792032033250577}}]},
				       align = 64, default = <<0, 0>>},
			     fields =
				 [#field{id = 0, name = name, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 1, name = codeOrder, kind = #data{type = uint16, align = 0, default = <<0, 0>>}},
				  #field{id = 2, name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 3, name = discriminantValue,
					 kind = #data{type = uint16, align = 16, default = <<255, 255>>}},
				  #field{id = 4, name = ordinal, kind = #group{id = 13515537513213004774}}]},
		 nodes =
		     [10930602151629473554]}.  %% noDiscriminant

'10930602151629473554'() ->
    #schema_node{module = schema_capnp, name = ['Field', noDiscriminant], id = 10930602151629473554,
		 scope = 11145653318641710175, src = <<"schema.capnp:Field.noDiscriminant">>,
		 kind = #const{field = #data{type = uint16, align = 0, default = <<255, 255>>}}}.

'13515537513213004774'() ->
    #schema_node{module = schema_capnp, name = ['Field', ordinal], id = 13515537513213004774,
		 scope = 11145653318641710175, src = <<"schema.capnp:Field.ordinal">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = implicit, kind = void},
					     #field{id = 1, name = explicit, kind = #data{type = uint16, align = 96, default = <<0, 0>>}}]},
				       align = 80, default = <<0, 0>>},
			     fields = []}}.

'14626792032033250577'() ->
    #schema_node{module = schema_capnp, name = ['Field', group], id = 14626792032033250577,
		 scope = 11145653318641710175, src = <<"schema.capnp:Field.group">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = typeId,
					 kind = #data{type = uint64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'14133145859926553711'() ->
    #schema_node{module = schema_capnp, name = ['Field', slot], id = 14133145859926553711,
		 scope = 11145653318641710175, src = <<"schema.capnp:Field.slot">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = offset, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}},
				  #field{id = 1, name = type,
					 kind = #ptr{type = {struct, 15020482145304562784}, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 2, name = defaultValue,
					 kind = #ptr{type = {struct, 14853958794117909659}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 3, name = hadExplicitDefault,
					 kind = #data{type = bool, align = 135, default = <<0:1>>}}]}}.

'Node'() -> '16610026722781537303'().

'Node'(['NestedNode']) -> '16050641862814319170'();
'Node'([annotation]) -> '17011813041836786320'();
'Node'([const]) -> '12793219851699983392'();
'Node'([interface]) -> '16728431493453586831'();
'Node'([enum]) -> '13063450714778629528'();
'Node'([struct]) -> '11430331134483579957'();
'Node'([]) -> '16610026722781537303'().

'16610026722781537303'() ->
    #schema_node{module = schema_capnp, name = 'Node', id = 16610026722781537303,
		 scope = 12195682960037147353, src = <<"schema.capnp:Node">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [#field{id = 0, name = file, kind = void},
					     #field{id = 1, name = struct, kind = #group{id = 11430331134483579957}},
					     #field{id = 2, name = enum, kind = #group{id = 13063450714778629528}},
					     #field{id = 3, name = interface, kind = #group{id = 16728431493453586831}},
					     #field{id = 4, name = const, kind = #group{id = 12793219851699983392}},
					     #field{id = 5, name = annotation, kind = #group{id = 17011813041836786320}}]},
				       align = 96, default = <<0, 0>>},
			     fields =
				 [#field{id = 0, name = id,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = displayName, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 2, name = displayNamePrefixLength,
					 kind = #data{type = uint32, align = 64, default = <<0, 0, 0, 0>>}},
				  #field{id = 3, name = scopeId,
					 kind = #data{type = uint64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 4, name = nestedNodes,
					 kind =
					     #ptr{type = {list, {struct, 16050641862814319170}}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 5, name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 2,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]},
		 nodes =
		     [16050641862814319170]}.  %% NestedNode

'16050641862814319170'() ->
    #schema_node{module = schema_capnp, name = ['Node', 'NestedNode'], id = 16050641862814319170,
		 scope = 16610026722781537303, src = <<"schema.capnp:Node.NestedNode">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = name, kind = #ptr{type = text, idx = 0, default = <<"">>}},
				  #field{id = 1, name = id,
					 kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'17011813041836786320'() ->
    #schema_node{module = schema_capnp, name = ['Node', annotation], id = 17011813041836786320,
		 scope = 16610026722781537303, src = <<"schema.capnp:Node.annotation">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = type,
					 kind = #ptr{type = {struct, 15020482145304562784}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = targetsFile, kind = #data{type = bool, align = 119, default = <<0:1>>}},
				  #field{id = 2, name = targetsConst, kind = #data{type = bool, align = 118, default = <<0:1>>}},
				  #field{id = 3, name = targetsEnum, kind = #data{type = bool, align = 117, default = <<0:1>>}},
				  #field{id = 4, name = targetsEnumerant, kind = #data{type = bool, align = 116, default = <<0:1>>}},
				  #field{id = 5, name = targetsStruct, kind = #data{type = bool, align = 115, default = <<0:1>>}},
				  #field{id = 6, name = targetsField, kind = #data{type = bool, align = 114, default = <<0:1>>}},
				  #field{id = 7, name = targetsUnion, kind = #data{type = bool, align = 113, default = <<0:1>>}},
				  #field{id = 8, name = targetsGroup, kind = #data{type = bool, align = 112, default = <<0:1>>}},
				  #field{id = 9, name = targetsInterface, kind = #data{type = bool, align = 127, default = <<0:1>>}},
				  #field{id = 10, name = targetsMethod, kind = #data{type = bool, align = 126, default = <<0:1>>}},
				  #field{id = 11, name = targetsParam, kind = #data{type = bool, align = 125, default = <<0:1>>}},
				  #field{id = 12, name = targetsAnnotation,
					 kind = #data{type = bool, align = 124, default = <<0:1>>}}]}}.

'12793219851699983392'() ->
    #schema_node{module = schema_capnp, name = ['Node', const], id = 12793219851699983392,
		 scope = 16610026722781537303, src = <<"schema.capnp:Node.const">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = type,
					 kind = #ptr{type = {struct, 15020482145304562784}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = value,
					 kind =
					     #ptr{type = {struct, 14853958794117909659}, idx = 4, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'16728431493453586831'() ->
    #schema_node{module = schema_capnp, name = ['Node', interface], id = 16728431493453586831,
		 scope = 16610026722781537303, src = <<"schema.capnp:Node.interface">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = methods,
					 kind =
					     #ptr{type = {list, {struct, 10736806783679155584}}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{id = 1, name = extends,
					 kind = #ptr{type = {list, uint64}, idx = 4, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'13063450714778629528'() ->
    #schema_node{module = schema_capnp, name = ['Node', enum], id = 13063450714778629528,
		 scope = 16610026722781537303, src = <<"schema.capnp:Node.enum">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = enumerants,
					 kind =
					     #ptr{type = {list, {struct, 10919677598968879693}}, idx = 3,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'11430331134483579957'() ->
    #schema_node{module = schema_capnp, name = ['Node', struct], id = 11430331134483579957,
		 scope = 16610026722781537303, src = <<"schema.capnp:Node.struct">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{id = 0, name = dataWordCount, kind = #data{type = uint16, align = 112, default = <<0, 0>>}},
				  #field{id = 1, name = pointerCount, kind = #data{type = uint16, align = 192, default = <<0, 0>>}},
				  #field{id = 2, name = preferredListEncoding,
					 kind = #data{type = {enum, 15102134695616452902}, align = 208, default = <<0, 0>>}},
				  #field{id = 3, name = isGroup, kind = #data{type = bool, align = 231, default = <<0:1>>}},
				  #field{id = 4, name = discriminantCount,
					 kind = #data{type = uint16, align = 240, default = <<0, 0>>}},
				  #field{id = 5, name = discriminantOffset,
					 kind = #data{type = uint32, align = 256, default = <<0, 0, 0, 0>>}},
				  #field{id = 6, name = fields,
					 kind =
					     #ptr{type = {list, {struct, 11145653318641710175}}, idx = 3,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.
