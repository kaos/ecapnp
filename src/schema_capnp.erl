%% This file was generated 2014-04-02 09:56:52 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module(schema_capnp).

-vsn(12195682960037147353).

-export([schema/1, 'Node'/0, '16610026722781537303'/0, 'Node.NestedNode'/0,
	 '16050641862814319170'/0, 'Node.annotation'/0, '17011813041836786320'/0, 'Node.const'/0,
	 '12793219851699983392'/0, 'Node.interface'/0, '16728431493453586831'/0, 'Node.enum'/0,
	 '13063450714778629528'/0, 'Node.struct'/0, '11430331134483579957'/0, 'Field'/0,
	 '11145653318641710175'/0, 'Field.noDiscriminant'/0, '10930602151629473554'/0, 'Field.ordinal'/0,
	 '13515537513213004774'/0, 'Field.group'/0, '14626792032033250577'/0, 'Field.slot'/0,
	 '14133145859926553711'/0, 'Enumerant'/0, '10919677598968879693'/0, 'Method'/0,
	 '10736806783679155584'/0, 'Type'/0, '15020482145304562784'/0, 'Type.interface'/0,
	 '17116997365232503999'/0, 'Type.struct'/0, '12410354185295152851'/0, 'Type.enum'/0,
	 '11389172934837766057'/0, 'Type.list'/0, '9792858745991129751'/0, 'Value'/0,
	 '14853958794117909659'/0, 'Annotation'/0, '17422339044421236034'/0, 'ElementSize'/0,
	 '15102134695616452902'/0, 'CodeGeneratorRequest'/0, '13818529054586492878'/0,
	 'CodeGeneratorRequest.RequestedFile'/0, '14981803260258615394'/0,
	 'CodeGeneratorRequest.RequestedFile.Import'/0, '12560611460656617445'/0]).

-types([{16610026722781537303, 'Node'}, {16050641862814319170, 'Node.NestedNode'},
	{17011813041836786320, 'Node.annotation'}, {12793219851699983392, 'Node.const'},
	{16728431493453586831, 'Node.interface'}, {13063450714778629528, 'Node.enum'},
	{11430331134483579957, 'Node.struct'}, {11145653318641710175, 'Field'},
	{10930602151629473554, 'Field.noDiscriminant'}, {13515537513213004774, 'Field.ordinal'},
	{14626792032033250577, 'Field.group'}, {14133145859926553711, 'Field.slot'},
	{10919677598968879693, 'Enumerant'}, {10736806783679155584, 'Method'},
	{15020482145304562784, 'Type'}, {17116997365232503999, 'Type.interface'},
	{12410354185295152851, 'Type.struct'}, {11389172934837766057, 'Type.enum'},
	{9792858745991129751, 'Type.list'}, {14853958794117909659, 'Value'},
	{17422339044421236034, 'Annotation'}, {15102134695616452902, 'ElementSize'},
	{13818529054586492878, 'CodeGeneratorRequest'},
	{14981803260258615394, 'CodeGeneratorRequest.RequestedFile'},
	{12560611460656617445, 'CodeGeneratorRequest.RequestedFile.Import'}]).

-include_lib("ecapnp/include/ecapnp.hrl").

'Node'() -> '16610026722781537303'().

'16610026722781537303'() ->
    #schema_node{module = ?MODULE, name = 'Node', id = 16610026722781537303,
		 src = <<"src/schema.capnp:Node">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, file, #field{name = file, kind = void}},
					     {1, struct, #field{name = struct, kind = #group{id = 11430331134483579957}}},
					     {2, enum, #field{name = enum, kind = #group{id = 13063450714778629528}}},
					     {3, interface, #field{name = interface, kind = #group{id = 16728431493453586831}}},
					     {4, const, #field{name = const, kind = #group{id = 12793219851699983392}}},
					     {5, annotation, #field{name = annotation, kind = #group{id = 17011813041836786320}}}]},
				       align = 96, default = <<0, 0>>},
			     fields =
				 [#field{name = id, kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = displayName, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = displayNamePrefixLength,
					 kind = #data{type = uint32, align = 64, default = <<0, 0, 0, 0>>}},
				  #field{name = scopeId,
					 kind = #data{type = uint64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = nestedNodes,
					 kind =
					     #ptr{type = {list, {struct, 16050641862814319170}}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 2,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Node.NestedNode'() -> '16050641862814319170'().

'16050641862814319170'() ->
    #schema_node{module = ?MODULE, name = 'Node.NestedNode', id = 16050641862814319170,
		 src = <<"src/schema.capnp:Node.NestedNode">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = name, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = id, kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Node.annotation'() -> '17011813041836786320'().

'17011813041836786320'() ->
    #schema_node{module = ?MODULE, name = 'Node.annotation', id = 17011813041836786320,
		 src = <<"src/schema.capnp:Node.annotation">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = type,
					 kind = #ptr{type = {struct, 15020482145304562784}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = targetsFile, kind = #data{type = bool, align = 119, default = <<0:1>>}},
				  #field{name = targetsConst, kind = #data{type = bool, align = 118, default = <<0:1>>}},
				  #field{name = targetsEnum, kind = #data{type = bool, align = 117, default = <<0:1>>}},
				  #field{name = targetsEnumerant, kind = #data{type = bool, align = 116, default = <<0:1>>}},
				  #field{name = targetsStruct, kind = #data{type = bool, align = 115, default = <<0:1>>}},
				  #field{name = targetsField, kind = #data{type = bool, align = 114, default = <<0:1>>}},
				  #field{name = targetsUnion, kind = #data{type = bool, align = 113, default = <<0:1>>}},
				  #field{name = targetsGroup, kind = #data{type = bool, align = 112, default = <<0:1>>}},
				  #field{name = targetsInterface, kind = #data{type = bool, align = 127, default = <<0:1>>}},
				  #field{name = targetsMethod, kind = #data{type = bool, align = 126, default = <<0:1>>}},
				  #field{name = targetsParam, kind = #data{type = bool, align = 125, default = <<0:1>>}},
				  #field{name = targetsAnnotation, kind = #data{type = bool, align = 124, default = <<0:1>>}}]}}.

'Node.const'() -> '12793219851699983392'().

'12793219851699983392'() ->
    #schema_node{module = ?MODULE, name = 'Node.const', id = 12793219851699983392,
		 src = <<"src/schema.capnp:Node.const">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = type,
					 kind = #ptr{type = {struct, 15020482145304562784}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = value,
					 kind =
					     #ptr{type = {struct, 14853958794117909659}, idx = 4, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Node.interface'() -> '16728431493453586831'().

'16728431493453586831'() ->
    #schema_node{module = ?MODULE, name = 'Node.interface', id = 16728431493453586831,
		 src = <<"src/schema.capnp:Node.interface">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = methods,
					 kind =
					     #ptr{type = {list, {struct, 10736806783679155584}}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = extends,
					 kind = #ptr{type = {list, uint64}, idx = 4, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Node.enum'() -> '13063450714778629528'().

'13063450714778629528'() ->
    #schema_node{module = ?MODULE, name = 'Node.enum', id = 13063450714778629528,
		 src = <<"src/schema.capnp:Node.enum">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = enumerants,
					 kind =
					     #ptr{type = {list, {struct, 10919677598968879693}}, idx = 3,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Node.struct'() -> '11430331134483579957'().

'11430331134483579957'() ->
    #schema_node{module = ?MODULE, name = 'Node.struct', id = 11430331134483579957,
		 src = <<"src/schema.capnp:Node.struct">>,
		 kind =
		     #struct{dsize = 5, psize = 5, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = dataWordCount, kind = #data{type = uint16, align = 112, default = <<0, 0>>}},
				  #field{name = pointerCount, kind = #data{type = uint16, align = 192, default = <<0, 0>>}},
				  #field{name = preferredListEncoding,
					 kind = #data{type = {enum, 15102134695616452902}, align = 208, default = <<0, 0>>}},
				  #field{name = isGroup, kind = #data{type = bool, align = 231, default = <<0:1>>}},
				  #field{name = discriminantCount, kind = #data{type = uint16, align = 240, default = <<0, 0>>}},
				  #field{name = discriminantOffset,
					 kind = #data{type = uint32, align = 256, default = <<0, 0, 0, 0>>}},
				  #field{name = fields,
					 kind =
					     #ptr{type = {list, {struct, 11145653318641710175}}, idx = 3,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Field'() -> '11145653318641710175'().

'11145653318641710175'() ->
    #schema_node{module = ?MODULE, name = 'Field', id = 11145653318641710175,
		 src = <<"src/schema.capnp:Field">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, slot, #field{name = slot, kind = #group{id = 14133145859926553711}}},
					     {1, group, #field{name = group, kind = #group{id = 14626792032033250577}}}]},
				       align = 64, default = <<0, 0>>},
			     fields =
				 [#field{name = name, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = codeOrder, kind = #data{type = uint16, align = 0, default = <<0, 0>>}},
				  #field{name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 1, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = discriminantValue, kind = #data{type = uint16, align = 16, default = <<255, 255>>}},
				  #field{name = ordinal, kind = #group{id = 13515537513213004774}}]}}.

'Field.noDiscriminant'() -> '10930602151629473554'().

'10930602151629473554'() ->
    #schema_node{module = ?MODULE, name = 'Field.noDiscriminant', id = 10930602151629473554,
		 src = <<"src/schema.capnp:Field.noDiscriminant">>,
		 kind = #const{field = #data{type = uint16, align = 0, default = <<255, 255>>}}}.

'Field.ordinal'() -> '13515537513213004774'().

'13515537513213004774'() ->
    #schema_node{module = ?MODULE, name = 'Field.ordinal', id = 13515537513213004774,
		 src = <<"src/schema.capnp:Field.ordinal">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, implicit, #field{name = implicit, kind = void}},
					     {1, explicit,
					      #field{name = explicit, kind = #data{type = uint16, align = 96, default = <<0, 0>>}}}]},
				       align = 80, default = <<0, 0>>},
			     fields = []}}.

'Field.group'() -> '14626792032033250577'().

'14626792032033250577'() ->
    #schema_node{module = ?MODULE, name = 'Field.group', id = 14626792032033250577,
		 src = <<"src/schema.capnp:Field.group">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = typeId,
					 kind = #data{type = uint64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Field.slot'() -> '14133145859926553711'().

'14133145859926553711'() ->
    #schema_node{module = ?MODULE, name = 'Field.slot', id = 14133145859926553711,
		 src = <<"src/schema.capnp:Field.slot">>,
		 kind =
		     #struct{dsize = 3, psize = 4, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = offset, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}},
				  #field{name = type,
					 kind = #ptr{type = {struct, 15020482145304562784}, idx = 2, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = defaultValue,
					 kind = #ptr{type = {struct, 14853958794117909659}, idx = 3, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = hadExplicitDefault, kind = #data{type = bool, align = 135, default = <<0:1>>}}]}}.

'Enumerant'() -> '10919677598968879693'().

'10919677598968879693'() ->
    #schema_node{module = ?MODULE, name = 'Enumerant', id = 10919677598968879693,
		 src = <<"src/schema.capnp:Enumerant">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = name, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = codeOrder, kind = #data{type = uint16, align = 0, default = <<0, 0>>}},
				  #field{name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Method'() -> '10736806783679155584'().

'10736806783679155584'() ->
    #schema_node{module = ?MODULE, name = 'Method', id = 10736806783679155584,
		 src = <<"src/schema.capnp:Method">>,
		 kind =
		     #struct{dsize = 3, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = name, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = codeOrder, kind = #data{type = uint16, align = 0, default = <<0, 0>>}},
				  #field{name = paramStructType,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = resultStructType,
					 kind = #data{type = uint64, align = 128, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = annotations,
					 kind =
					     #ptr{type = {list, {struct, 17422339044421236034}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Type'() -> '15020482145304562784'().

'15020482145304562784'() ->
    #schema_node{module = ?MODULE, name = 'Type', id = 15020482145304562784,
		 src = <<"src/schema.capnp:Type">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, void, #field{name = void, kind = void}}, {1, bool, #field{name = bool, kind = void}},
					     {2, int8, #field{name = int8, kind = void}}, {3, int16, #field{name = int16, kind = void}},
					     {4, int32, #field{name = int32, kind = void}}, {5, int64, #field{name = int64, kind = void}},
					     {6, uint8, #field{name = uint8, kind = void}}, {7, uint16, #field{name = uint16, kind = void}},
					     {8, uint32, #field{name = uint32, kind = void}}, {9, uint64, #field{name = uint64, kind = void}},
					     {10, float32, #field{name = float32, kind = void}},
					     {11, float64, #field{name = float64, kind = void}}, {12, text, #field{name = text, kind = void}},
					     {13, data, #field{name = data, kind = void}},
					     {14, list, #field{name = list, kind = #group{id = 9792858745991129751}}},
					     {15, enum, #field{name = enum, kind = #group{id = 11389172934837766057}}},
					     {16, struct, #field{name = struct, kind = #group{id = 12410354185295152851}}},
					     {17, interface, #field{name = interface, kind = #group{id = 17116997365232503999}}},
					     {18, anyPointer, #field{name = anyPointer, kind = void}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.

'Type.interface'() -> '17116997365232503999'().

'17116997365232503999'() ->
    #schema_node{module = ?MODULE, name = 'Type.interface', id = 17116997365232503999,
		 src = <<"src/schema.capnp:Type.interface">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = typeId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Type.struct'() -> '12410354185295152851'().

'12410354185295152851'() ->
    #schema_node{module = ?MODULE, name = 'Type.struct', id = 12410354185295152851,
		 src = <<"src/schema.capnp:Type.struct">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = typeId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Type.enum'() -> '11389172934837766057'().

'11389172934837766057'() ->
    #schema_node{module = ?MODULE, name = 'Type.enum', id = 11389172934837766057,
		 src = <<"src/schema.capnp:Type.enum">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = typeId,
					 kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Type.list'() -> '9792858745991129751'().

'9792858745991129751'() ->
    #schema_node{module = ?MODULE, name = 'Type.list', id = 9792858745991129751,
		 src = <<"src/schema.capnp:Type.list">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = elementType,
					 kind =
					     #ptr{type = {struct, 15020482145304562784}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'Value'() -> '14853958794117909659'().

'14853958794117909659'() ->
    #schema_node{module = ?MODULE, name = 'Value', id = 14853958794117909659,
		 src = <<"src/schema.capnp:Value">>,
		 kind =
		     #struct{dsize = 2, psize = 1, esize = inlineComposite,
			     union_field =
				 #data{type =
					   {union,
					    [{0, void, #field{name = void, kind = void}},
					     {1, bool, #field{name = bool, kind = #data{type = bool, align = 23, default = <<0:1>>}}},
					     {2, int8, #field{name = int8, kind = #data{type = int8, align = 16, default = <<0>>}}},
					     {3, int16, #field{name = int16, kind = #data{type = int16, align = 16, default = <<0, 0>>}}},
					     {4, int32, #field{name = int32, kind = #data{type = int32, align = 32, default = <<0, 0, 0, 0>>}}},
					     {5, int64,
					      #field{name = int64, kind = #data{type = int64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {6, uint8, #field{name = uint8, kind = #data{type = uint8, align = 16, default = <<0>>}}},
					     {7, uint16, #field{name = uint16, kind = #data{type = uint16, align = 16, default = <<0, 0>>}}},
					     {8, uint32,
					      #field{name = uint32, kind = #data{type = uint32, align = 32, default = <<0, 0, 0, 0>>}}},
					     {9, uint64,
					      #field{name = uint64,
						     kind = #data{type = uint64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {10, float32,
					      #field{name = float32, kind = #data{type = float32, align = 32, default = <<0, 0, 0, 0>>}}},
					     {11, float64,
					      #field{name = float64,
						     kind = #data{type = float64, align = 64, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {12, text, #field{name = text, kind = #ptr{type = text, idx = 0, default = <<>>}}},
					     {13, data, #field{name = data, kind = #ptr{type = data, idx = 0, default = <<>>}}},
					     {14, list,
					      #field{name = list, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {15, enum, #field{name = enum, kind = #data{type = uint16, align = 16, default = <<0, 0>>}}},
					     {16, struct,
					      #field{name = struct, kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}},
					     {17, interface, #field{name = interface, kind = void}},
					     {18, anyPointer,
					      #field{name = anyPointer,
						     kind = #ptr{type = object, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}}]},
				       align = 0, default = <<0, 0>>},
			     fields = []}}.

'Annotation'() -> '17422339044421236034'().

'17422339044421236034'() ->
    #schema_node{module = ?MODULE, name = 'Annotation', id = 17422339044421236034,
		 src = <<"src/schema.capnp:Annotation">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = value,
					 kind =
					     #ptr{type = {struct, 14853958794117909659}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'ElementSize'() -> '15102134695616452902'().

'15102134695616452902'() ->
    #schema_node{module = ?MODULE, name = 'ElementSize', id = 15102134695616452902,
		 src = <<"src/schema.capnp:ElementSize">>,
		 kind =
		     #enum{values =
			       [{0, empty}, {1, bit}, {2, byte}, {3, twoBytes}, {4, fourBytes}, {5, eightBytes}, {6, pointer},
				{7, inlineComposite}]}}.

'CodeGeneratorRequest'() -> '13818529054586492878'().

'13818529054586492878'() ->
    #schema_node{module = ?MODULE, name = 'CodeGeneratorRequest', id = 13818529054586492878,
		 src = <<"src/schema.capnp:CodeGeneratorRequest">>,
		 kind =
		     #struct{dsize = 0, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = nodes,
					 kind =
					     #ptr{type = {list, {struct, 16610026722781537303}}, idx = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = requestedFiles,
					 kind =
					     #ptr{type = {list, {struct, 14981803260258615394}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'CodeGeneratorRequest.RequestedFile'() -> '14981803260258615394'().

'14981803260258615394'() ->
    #schema_node{module = ?MODULE, name = 'CodeGeneratorRequest.RequestedFile',
		 id = 14981803260258615394, src = <<"src/schema.capnp:CodeGeneratorRequest.RequestedFile">>,
		 kind =
		     #struct{dsize = 1, psize = 2, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = filename, kind = #ptr{type = text, idx = 0, default = <<>>}},
				  #field{name = imports,
					 kind =
					     #ptr{type = {list, {struct, 12560611460656617445}}, idx = 1,
						  default = <<0, 0, 0, 0, 0, 0, 0, 0>>}}]}}.

'CodeGeneratorRequest.RequestedFile.Import'() -> '12560611460656617445'().

'12560611460656617445'() ->
    #schema_node{module = ?MODULE, name = 'CodeGeneratorRequest.RequestedFile.Import',
		 id = 12560611460656617445, src = <<"src/schema.capnp:CodeGeneratorRequest.RequestedFile.Import">>,
		 kind =
		     #struct{dsize = 1, psize = 1, esize = inlineComposite, union_field = none,
			     fields =
				 [#field{name = id, kind = #data{type = uint64, align = 0, default = <<0, 0, 0, 0, 0, 0, 0, 0>>}},
				  #field{name = name, kind = #ptr{type = text, idx = 0, default = <<>>}}]}}.

schema('Node') -> schema(16610026722781537303);
schema(16610026722781537303) -> '16610026722781537303'();
schema('Node.NestedNode') -> schema(16050641862814319170);
schema(16050641862814319170) -> '16050641862814319170'();
schema('Node.annotation') -> schema(17011813041836786320);
schema(17011813041836786320) -> '17011813041836786320'();
schema('Node.const') -> schema(12793219851699983392);
schema(12793219851699983392) -> '12793219851699983392'();
schema('Node.interface') -> schema(16728431493453586831);
schema(16728431493453586831) -> '16728431493453586831'();
schema('Node.enum') -> schema(13063450714778629528);
schema(13063450714778629528) -> '13063450714778629528'();
schema('Node.struct') -> schema(11430331134483579957);
schema(11430331134483579957) -> '11430331134483579957'();
schema('Field') -> schema(11145653318641710175);
schema(11145653318641710175) -> '11145653318641710175'();
schema('Field.noDiscriminant') -> schema(10930602151629473554);
schema(10930602151629473554) -> '10930602151629473554'();
schema('Field.ordinal') -> schema(13515537513213004774);
schema(13515537513213004774) -> '13515537513213004774'();
schema('Field.group') -> schema(14626792032033250577);
schema(14626792032033250577) -> '14626792032033250577'();
schema('Field.slot') -> schema(14133145859926553711);
schema(14133145859926553711) -> '14133145859926553711'();
schema('Enumerant') -> schema(10919677598968879693);
schema(10919677598968879693) -> '10919677598968879693'();
schema('Method') -> schema(10736806783679155584);
schema(10736806783679155584) -> '10736806783679155584'();
schema('Type') -> schema(15020482145304562784);
schema(15020482145304562784) -> '15020482145304562784'();
schema('Type.interface') -> schema(17116997365232503999);
schema(17116997365232503999) -> '17116997365232503999'();
schema('Type.struct') -> schema(12410354185295152851);
schema(12410354185295152851) -> '12410354185295152851'();
schema('Type.enum') -> schema(11389172934837766057);
schema(11389172934837766057) -> '11389172934837766057'();
schema('Type.list') -> schema(9792858745991129751);
schema(9792858745991129751) -> '9792858745991129751'();
schema('Value') -> schema(14853958794117909659);
schema(14853958794117909659) -> '14853958794117909659'();
schema('Annotation') -> schema(17422339044421236034);
schema(17422339044421236034) -> '17422339044421236034'();
schema('ElementSize') -> schema(15102134695616452902);
schema(15102134695616452902) -> '15102134695616452902'();
schema('CodeGeneratorRequest') -> schema(13818529054586492878);
schema(13818529054586492878) -> '13818529054586492878'();
schema('CodeGeneratorRequest.RequestedFile') -> schema(14981803260258615394);
schema(14981803260258615394) -> '14981803260258615394'();
schema('CodeGeneratorRequest.RequestedFile.Import') -> schema(12560611460656617445);
schema(12560611460656617445) -> '12560611460656617445'();
schema(_) -> undefined.