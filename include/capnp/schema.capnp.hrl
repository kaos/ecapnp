%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-include("c++.capnp.hrl").

-compile({nowarn_unused_function, schema/1}).
-compile({nowarn_unused_function, schema/2}).
-compile({nowarn_unused_function, schema/3}).
-compile({nowarn_unused_function, schema/4}).

%% Write value to object field.
%% -spec schema(set, Field::atom(), Value::term(), Object::#object{}) -> ok.
schema(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% Write unnamed union value
%% -spec schema(set, {field_name(), field_value()}|field_name(), object()) -> ok.
schema(set, Value, Object) ->
    ecapnp:set(Value, Object);

%% Get a reference to the root object in message.
%% -spec schema(root, Type::atom() | integer(), Message::list(binary())) -> {ok, Root::#object{}}.
schema(root, Type, Message) ->
    ecapnp:get_root(Type, schema(schema), Message);

%% Read object field value.
%% -spec schema(get, Field::atom(), Object::#object{}) -> term().
schema(get, Field, Object) ->
    ecapnp:get(Field, Object);

%% Type cast object to another struct or list.
%% -spec schema(to_struct, Type::atom() | integer(), Object1::#object{}) -> Object2#object{}.
%% -spec schema(to_list, Type::atom() | integer(), Object::#object{}) -> list().
schema(TypeCast, Type, Object)
  when TypeCast == to_struct;
       TypeCast == to_list ->
    ecapnp_obj:TypeCast(Type, Object).

%% Set root type for a new message.
%% -spec schema(root, Type::atom() | integer()) -> {ok, Root::#object{}}.
schema(root, Type) ->
    ecapnp:set_root(Type, schema(schema));

%% Read unnamed union value of object.
%% -spec schema(get, Object::#object{}) -> Tag::atom() | {Tag::atom(), Value::term()}.
schema(get, Object) ->
    ecapnp:get(Object);

%% Read const value from schema
%% -spec schema(const, Name:: type_name() | type_id()) -> value().
schema(const, Name) ->
    ecapnp:const(Name, schema(schema));
        
%% Type cast object to text/data.
%% -spec schema(to_text | to_data, Object::#object{}) -> binary().
schema(TypeCast, Object)
  when TypeCast == to_text;
       TypeCast == to_data ->
    ecapnp_obj:TypeCast(Object).

%% Get the compiled schema.
%% -spec schema(schema) -> #schema{}.

schema(schema) ->
  ['c++'(schema),
   #schema_node{ %% 0xa93fc509624c72d9
     name=schema, id=12195682960037147353, src= <<"include/capnp/schema.capnp">>,
     kind=file,
     annotations=
       [{13386661402618388268,<<"capnp::schema">>}
       ],
     nodes=
       [#schema_node{ %% 0xe682ab4cf923a417
          name='Node', id=16610026722781537303, src= <<"include/capnp/schema.capnp:Node">>,
          kind=#struct{ dsize=5, psize=5, esize=inlineComposite,
            union_field=#data{ align=96, default= <<0,0>>, type=
              {union,
                [{0,file,
                  #field{ name=file,
                    kind=void}},
                 {1,struct,
                  #field{ name=struct,
                    kind=#group{ id=11430331134483579957 }}},
                 {2,enum,
                  #field{ name=enum,
                    kind=#group{ id=13063450714778629528 }}},
                 {3,interface,
                  #field{ name=interface,
                    kind=#group{ id=16728431493453586831 }}},
                 {4,const,
                  #field{ name=const,
                    kind=#group{ id=12793219851699983392 }}},
                 {5,annotation,
                  #field{ name=annotation,
                    kind=#group{ id=17011813041836786320 }}}
              ]}},
            fields=
              [#field{ name=id,
                 kind=#data{ type=uint64, align=0,
                        default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=displayName,
                 kind=#ptr{ type=text, idx=0,
                       default= <<>> }},
               #field{ name=displayNamePrefixLength,
                 kind=#data{ type=uint32, align=64,
                        default= <<0,0,0,0>> }},
               #field{ name=scopeId,
                 kind=#data{ type=uint64, align=128,
                        default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=nestedNodes,
                 kind=#ptr{ type={list,{struct,16050641862814319170}}, idx=1,
                       default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=annotations,
                 kind=#ptr{ type={list,{struct,17422339044421236034}}, idx=2,
                       default= <<0,0,0,0,0,0,0,0>> }}
              ]},
          nodes=
            [#schema_node{ %% 0xec1619d4400a0290
               name=annotation, id=17011813041836786320, src= <<"include/capnp/schema.capnp:Node.annotation">>,
               kind=#struct{ dsize=5, psize=5, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=type,
                      kind=#ptr{ type={struct,15020482145304562784}, idx=3,
                            default= <<0,0,0,0,0,0,0,0>> }},
                    #field{ name=targetsFile,
                      kind=#data{ type=bool, align=119,
                             default= <<0:1>> }},
                    #field{ name=targetsConst,
                      kind=#data{ type=bool, align=118,
                             default= <<0:1>> }},
                    #field{ name=targetsEnum,
                      kind=#data{ type=bool, align=117,
                             default= <<0:1>> }},
                    #field{ name=targetsEnumerant,
                      kind=#data{ type=bool, align=116,
                             default= <<0:1>> }},
                    #field{ name=targetsStruct,
                      kind=#data{ type=bool, align=115,
                             default= <<0:1>> }},
                    #field{ name=targetsField,
                      kind=#data{ type=bool, align=114,
                             default= <<0:1>> }},
                    #field{ name=targetsUnion,
                      kind=#data{ type=bool, align=113,
                             default= <<0:1>> }},
                    #field{ name=targetsGroup,
                      kind=#data{ type=bool, align=112,
                             default= <<0:1>> }},
                    #field{ name=targetsInterface,
                      kind=#data{ type=bool, align=127,
                             default= <<0:1>> }},
                    #field{ name=targetsMethod,
                      kind=#data{ type=bool, align=126,
                             default= <<0:1>> }},
                    #field{ name=targetsParam,
                      kind=#data{ type=bool, align=125,
                             default= <<0:1>> }},
                    #field{ name=targetsAnnotation,
                      kind=#data{ type=bool, align=124,
                             default= <<0:1>> }}
                   ]}},
             #schema_node{ %% 0xb18aa5ac7a0d9420
               name=const, id=12793219851699983392, src= <<"include/capnp/schema.capnp:Node.const">>,
               kind=#struct{ dsize=5, psize=5, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=type,
                      kind=#ptr{ type={struct,15020482145304562784}, idx=3,
                            default= <<0,0,0,0,0,0,0,0>> }},
                    #field{ name=value,
                      kind=#ptr{ type={struct,14853958794117909659}, idx=4,
                            default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0xe82753cff0c2218f
               name=interface, id=16728431493453586831, src= <<"include/capnp/schema.capnp:Node.interface">>,
               kind=#struct{ dsize=5, psize=5, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=methods,
                      kind=#ptr{ type={list,{struct,10736806783679155584}}, idx=3,
                            default= <<0,0,0,0,0,0,0,0>> }},
                    #field{ name=extends,
                      kind=#ptr{ type={list,uint64}, idx=4,
                            default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0xb54ab3364333f598
               name=enum, id=13063450714778629528, src= <<"include/capnp/schema.capnp:Node.enum">>,
               kind=#struct{ dsize=5, psize=5, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=enumerants,
                      kind=#ptr{ type={list,{struct,10919677598968879693}}, idx=3,
                            default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0x9ea0b19b37fb4435
               name=struct, id=11430331134483579957, src= <<"include/capnp/schema.capnp:Node.struct">>,
               kind=#struct{ dsize=5, psize=5, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=dataWordCount,
                      kind=#data{ type=uint16, align=112,
                             default= <<0,0>> }},
                    #field{ name=pointerCount,
                      kind=#data{ type=uint16, align=192,
                             default= <<0,0>> }},
                    #field{ name=preferredListEncoding,
                      kind=#data{ type={enum,15102134695616452902}, align=208,
                             default= <<0,0>> }},
                    #field{ name=isGroup,
                      kind=#data{ type=bool, align=231,
                             default= <<0:1>> }},
                    #field{ name=discriminantCount,
                      kind=#data{ type=uint16, align=240,
                             default= <<0,0>> }},
                    #field{ name=discriminantOffset,
                      kind=#data{ type=uint32, align=256,
                             default= <<0,0,0,0>> }},
                    #field{ name=fields,
                      kind=#ptr{ type={list,{struct,11145653318641710175}}, idx=3,
                            default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0xdebf55bbfa0fc242
               name='NestedNode', id=16050641862814319170, src= <<"include/capnp/schema.capnp:Node.NestedNode">>,
               kind=#struct{ dsize=1, psize=1, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=name,
                      kind=#ptr{ type=text, idx=0,
                            default= <<>> }},
                    #field{ name=id,
                      kind=#data{ type=uint64, align=0,
                             default= <<0,0,0,0,0,0,0,0>> }}
                   ]}}
            ]},
        #schema_node{ %% 0x9aad50a41f4af45f
          name='Field', id=11145653318641710175, src= <<"include/capnp/schema.capnp:Field">>,
          kind=#struct{ dsize=3, psize=4, esize=inlineComposite,
            union_field=#data{ align=64, default= <<0,0>>, type=
              {union,
                [{0,slot,
                  #field{ name=slot,
                    kind=#group{ id=14133145859926553711 }}},
                 {1,group,
                  #field{ name=group,
                    kind=#group{ id=14626792032033250577 }}}
              ]}},
            fields=
              [#field{ name=name,
                 kind=#ptr{ type=text, idx=0,
                       default= <<>> }},
               #field{ name=codeOrder,
                 kind=#data{ type=uint16, align=0,
                        default= <<0,0>> }},
               #field{ name=annotations,
                 kind=#ptr{ type={list,{struct,17422339044421236034}}, idx=1,
                       default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=discriminantValue,
                 kind=#data{ type=uint16, align=16,
                        default= <<255,255>> }},
               #field{ name=ordinal,
                 kind=#group{ id=13515537513213004774 }}
              ]},
          nodes=
            [#schema_node{ %% 0xbb90d5c287870be6
               name=ordinal, id=13515537513213004774, src= <<"include/capnp/schema.capnp:Field.ordinal">>,
               kind=#struct{ dsize=3, psize=4, esize=inlineComposite,
                 union_field=#data{ align=80, default= <<0,0>>, type=
                   {union,
                     [{0,implicit,
                       #field{ name=implicit,
                         kind=void}},
                      {1,explicit,
                       #field{ name=explicit,
                         kind=#data{ type=uint16, align=96,
                                default= <<0,0>> }}}
                   ]}}}},
             #schema_node{ %% 0xcafccddb68db1d11
               name=group, id=14626792032033250577, src= <<"include/capnp/schema.capnp:Field.group">>,
               kind=#struct{ dsize=3, psize=4, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=typeId,
                      kind=#data{ type=uint64, align=128,
                             default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0xc42305476bb4746f
               name=slot, id=14133145859926553711, src= <<"include/capnp/schema.capnp:Field.slot">>,
               kind=#struct{ dsize=3, psize=4, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=offset,
                      kind=#data{ type=uint32, align=32,
                             default= <<0,0,0,0>> }},
                    #field{ name=type,
                      kind=#ptr{ type={struct,15020482145304562784}, idx=2,
                            default= <<0,0,0,0,0,0,0,0>> }},
                    #field{ name=defaultValue,
                      kind=#ptr{ type={struct,14853958794117909659}, idx=3,
                            default= <<0,0,0,0,0,0,0,0>> }},
                    #field{ name=hadExplicitDefault,
                      kind=#data{ type=bool, align=135,
                             default= <<0:1>> }}
                   ]}}
            ]},
        #schema_node{ %% 0x978a7cebdc549a4d
          name='Enumerant', id=10919677598968879693, src= <<"include/capnp/schema.capnp:Enumerant">>,
          kind=#struct{ dsize=1, psize=2, esize=inlineComposite,
            union_field=none,
            fields=
              [#field{ name=name,
                 kind=#ptr{ type=text, idx=0,
                       default= <<>> }},
               #field{ name=codeOrder,
                 kind=#data{ type=uint16, align=0,
                        default= <<0,0>> }},
               #field{ name=annotations,
                 kind=#ptr{ type={list,{struct,17422339044421236034}}, idx=1,
                       default= <<0,0,0,0,0,0,0,0>> }}
              ]}},
        #schema_node{ %% 0x9500cce23b334d80
          name='Method', id=10736806783679155584, src= <<"include/capnp/schema.capnp:Method">>,
          kind=#struct{ dsize=3, psize=2, esize=inlineComposite,
            union_field=none,
            fields=
              [#field{ name=name,
                 kind=#ptr{ type=text, idx=0,
                       default= <<>> }},
               #field{ name=codeOrder,
                 kind=#data{ type=uint16, align=0,
                        default= <<0,0>> }},
               #field{ name=paramStructType,
                 kind=#data{ type=uint64, align=64,
                        default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=resultStructType,
                 kind=#data{ type=uint64, align=128,
                        default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=annotations,
                 kind=#ptr{ type={list,{struct,17422339044421236034}}, idx=1,
                       default= <<0,0,0,0,0,0,0,0>> }}
              ]}},
        #schema_node{ %% 0xd07378ede1f9cc60
          name='Type', id=15020482145304562784, src= <<"include/capnp/schema.capnp:Type">>,
          kind=#struct{ dsize=2, psize=1, esize=inlineComposite,
            union_field=#data{ align=0, default= <<0,0>>, type=
              {union,
                [{0,void,
                  #field{ name=void,
                    kind=void}},
                 {1,bool,
                  #field{ name=bool,
                    kind=void}},
                 {2,int8,
                  #field{ name=int8,
                    kind=void}},
                 {3,int16,
                  #field{ name=int16,
                    kind=void}},
                 {4,int32,
                  #field{ name=int32,
                    kind=void}},
                 {5,int64,
                  #field{ name=int64,
                    kind=void}},
                 {6,uint8,
                  #field{ name=uint8,
                    kind=void}},
                 {7,uint16,
                  #field{ name=uint16,
                    kind=void}},
                 {8,uint32,
                  #field{ name=uint32,
                    kind=void}},
                 {9,uint64,
                  #field{ name=uint64,
                    kind=void}},
                 {10,float32,
                  #field{ name=float32,
                    kind=void}},
                 {11,float64,
                  #field{ name=float64,
                    kind=void}},
                 {12,text,
                  #field{ name=text,
                    kind=void}},
                 {13,data,
                  #field{ name=data,
                    kind=void}},
                 {14,list,
                  #field{ name=list,
                    kind=#group{ id=9792858745991129751 }}},
                 {15,enum,
                  #field{ name=enum,
                    kind=#group{ id=11389172934837766057 }}},
                 {16,struct,
                  #field{ name=struct,
                    kind=#group{ id=12410354185295152851 }}},
                 {17,interface,
                  #field{ name=interface,
                    kind=#group{ id=17116997365232503999 }}},
                 {18,object,
                  #field{ name=object,
                    kind=void}}
              ]}}},
          nodes=
            [#schema_node{ %% 0xed8bca69f7fb0cbf
               name=interface, id=17116997365232503999, src= <<"include/capnp/schema.capnp:Type.interface">>,
               kind=#struct{ dsize=2, psize=1, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=typeId,
                      kind=#data{ type=uint64, align=64,
                             default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0xac3a6f60ef4cc6d3
               name=struct, id=12410354185295152851, src= <<"include/capnp/schema.capnp:Type.struct">>,
               kind=#struct{ dsize=2, psize=1, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=typeId,
                      kind=#data{ type=uint64, align=64,
                             default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0x9e0e78711a7f87a9
               name=enum, id=11389172934837766057, src= <<"include/capnp/schema.capnp:Type.enum">>,
               kind=#struct{ dsize=2, psize=1, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=typeId,
                      kind=#data{ type=uint64, align=64,
                             default= <<0,0,0,0,0,0,0,0>> }}
                   ]}},
             #schema_node{ %% 0x87e739250a60ea97
               name=list, id=9792858745991129751, src= <<"include/capnp/schema.capnp:Type.list">>,
               kind=#struct{ dsize=2, psize=1, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=elementType,
                      kind=#ptr{ type={struct,15020482145304562784}, idx=0,
                            default= <<0,0,0,0,0,0,0,0>> }}
                   ]}}
            ]},
        #schema_node{ %% 0xce23dcd2d7b00c9b
          name='Value', id=14853958794117909659, src= <<"include/capnp/schema.capnp:Value">>,
          kind=#struct{ dsize=2, psize=1, esize=inlineComposite,
            union_field=#data{ align=0, default= <<0,0>>, type=
              {union,
                [{0,void,
                  #field{ name=void,
                    kind=void}},
                 {1,bool,
                  #field{ name=bool,
                    kind=#data{ type=bool, align=23,
                           default= <<0:1>> }}},
                 {2,int8,
                  #field{ name=int8,
                    kind=#data{ type=int8, align=16,
                           default= <<0>> }}},
                 {3,int16,
                  #field{ name=int16,
                    kind=#data{ type=int16, align=16,
                           default= <<0,0>> }}},
                 {4,int32,
                  #field{ name=int32,
                    kind=#data{ type=int32, align=32,
                           default= <<0,0,0,0>> }}},
                 {5,int64,
                  #field{ name=int64,
                    kind=#data{ type=int64, align=64,
                           default= <<0,0,0,0,0,0,0,0>> }}},
                 {6,uint8,
                  #field{ name=uint8,
                    kind=#data{ type=uint8, align=16,
                           default= <<0>> }}},
                 {7,uint16,
                  #field{ name=uint16,
                    kind=#data{ type=uint16, align=16,
                           default= <<0,0>> }}},
                 {8,uint32,
                  #field{ name=uint32,
                    kind=#data{ type=uint32, align=32,
                           default= <<0,0,0,0>> }}},
                 {9,uint64,
                  #field{ name=uint64,
                    kind=#data{ type=uint64, align=64,
                           default= <<0,0,0,0,0,0,0,0>> }}},
                 {10,float32,
                  #field{ name=float32,
                    kind=#data{ type=float32, align=32,
                           default= <<0,0,0,0>> }}},
                 {11,float64,
                  #field{ name=float64,
                    kind=#data{ type=float64, align=64,
                           default= <<0,0,0,0,0,0,0,0>> }}},
                 {12,text,
                  #field{ name=text,
                    kind=#ptr{ type=text, idx=0,
                          default= <<>> }}},
                 {13,data,
                  #field{ name=data,
                    kind=#ptr{ type=data, idx=0,
                          default= <<>> }}},
                 {14,list,
                  #field{ name=list,
                    kind=#ptr{ type=object, idx=0,
                          default= <<0,0,0,0,0,0,0,0>> }}},
                 {15,enum,
                  #field{ name=enum,
                    kind=#data{ type=uint16, align=16,
                           default= <<0,0>> }}},
                 {16,struct,
                  #field{ name=struct,
                    kind=#ptr{ type=object, idx=0,
                          default= <<0,0,0,0,0,0,0,0>> }}},
                 {17,interface,
                  #field{ name=interface,
                    kind=void}},
                 {18,object,
                  #field{ name=object,
                    kind=#ptr{ type=object, idx=0,
                          default= <<0,0,0,0,0,0,0,0>> }}}
              ]}}}},
        #schema_node{ %% 0xf1c8950dab257542
          name='Annotation', id=17422339044421236034, src= <<"include/capnp/schema.capnp:Annotation">>,
          kind=#struct{ dsize=1, psize=1, esize=inlineComposite,
            union_field=none,
            fields=
              [#field{ name=id,
                 kind=#data{ type=uint64, align=0,
                        default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=value,
                 kind=#ptr{ type={struct,14853958794117909659}, idx=0,
                       default= <<0,0,0,0,0,0,0,0>> }}
              ]}},
        #schema_node{ %% 0xd1958f7dba521926
          name='ElementSize', id=15102134695616452902, src= <<"include/capnp/schema.capnp:ElementSize">>,
          kind=#enum{ values=
            [{0,empty},
             {1,bit},
             {2,byte},
             {3,twoBytes},
             {4,fourBytes},
             {5,eightBytes},
             {6,pointer},
             {7,inlineComposite}
            ]}},
        #schema_node{ %% 0xbfc546f6210ad7ce
          name='CodeGeneratorRequest', id=13818529054586492878, src= <<"include/capnp/schema.capnp:CodeGeneratorRequest">>,
          kind=#struct{ dsize=0, psize=2, esize=inlineComposite,
            union_field=none,
            fields=
              [#field{ name=nodes,
                 kind=#ptr{ type={list,{struct,16610026722781537303}}, idx=0,
                       default= <<0,0,0,0,0,0,0,0>> }},
               #field{ name=requestedFiles,
                 kind=#ptr{ type={list,{struct,14981803260258615394}}, idx=1,
                       default= <<0,0,0,0,0,0,0,0>> }}
              ]},
          nodes=
            [#schema_node{ %% 0xcfea0eb02e810062
               name='RequestedFile', id=14981803260258615394, src= <<"include/capnp/schema.capnp:CodeGeneratorRequest.RequestedFile">>,
               kind=#struct{ dsize=1, psize=2, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [#field{ name=id,
                      kind=#data{ type=uint64, align=0,
                             default= <<0,0,0,0,0,0,0,0>> }},
                    #field{ name=filename,
                      kind=#ptr{ type=text, idx=0,
                            default= <<>> }},
                    #field{ name=imports,
                      kind=#ptr{ type={list,{struct,12560611460656617445}}, idx=1,
                            default= <<0,0,0,0,0,0,0,0>> }}
                   ]},
               nodes=
                 [#schema_node{ %% 0xae504193122357e5
                    name='Import', id=12560611460656617445, src= <<"include/capnp/schema.capnp:CodeGeneratorRequest.RequestedFile.Import">>,
                    kind=#struct{ dsize=1, psize=1, esize=inlineComposite,
                      union_field=none,
                      fields=
                        [#field{ name=id,
                           kind=#data{ type=uint64, align=0,
                                  default= <<0,0,0,0,0,0,0,0>> }},
                         #field{ name=name,
                           kind=#ptr{ type=text, idx=0,
                                 default= <<>> }}
                        ]}}
                 ]}
            ]}
       ]}
  ].
