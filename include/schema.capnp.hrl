%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-export([schema/1, schema/2, schema/3, schema/4]).

%% schema/4
schema(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% schema/3
schema(root, Type, Message) ->
    ecapnp:get_root(Type, schema(schema), Message);
schema(get, Field, Object) ->
    ecapnp:get(Field, Object).

%% schema/2
schema(root, Type) ->
    ecapnp:set_root(Type, schema(schema));
schema(get, Object) ->
    ecapnp:get(Object).

%% schema/1

schema(schema) ->
  #schema{ %% 0xa93fc509624c72d9
    name=schema, id=12195682960037147353, source= <<"schema.capnp">>,
    types=
      [#struct{ %% 0xe682ab4cf923a417
         name='Node', id=16610026722781537303, source= <<"schema.capnp:Node">>,
         dsize=5, psize=5, esize=inlineComposite,
         union_field=#data{ align=96, type=
           {union,
             [{file,void},
              {struct,
               #group{ id=11430331134483579957 }},
              {enum,
               #group{ id=13063450714778629528 }},
              {interface,
               #group{ id=16728431493453586831 }},
              {const,
               #group{ id=12793219851699983392 }},
              {annotation,
               #group{ id=17011813041836786320 }}
           ]} },
         fields=
           [{id,
             #data{ type=uint64, align=0 }},
            {displayName,
             #ptr{ type=text, idx=0 }},
            {displayNamePrefixLength,
             #data{ type=uint32, align=64 }},
            {scopeId,
             #data{ type=uint64, align=128 }},
            {nestedNodes,
             #ptr{ type={list,{struct,16050641862814319170}}, idx=1 }},
            {annotations,
             #ptr{ type={list,{struct,17422339044421236034}}, idx=2 }}
           ],
         types=
           [#struct{ %% 0xdebf55bbfa0fc242
              name='NestedNode', id=16050641862814319170, source= <<"schema.capnp:Node.NestedNode">>,
              dsize=1, psize=1, esize=inlineComposite,
              union_field=none,
              fields=
                [{name,
                  #ptr{ type=text, idx=0 }},
                 {id,
                  #data{ type=uint64, align=0 }}
                ]},
            #struct{ %% 0xec1619d4400a0290
              name=annotation, id=17011813041836786320, source= <<"schema.capnp:Node.annotation">>,
              dsize=5, psize=5, esize=inlineComposite,
              union_field=none,
              fields=
                [{type,
                  #ptr{ type={struct,15020482145304562784}, idx=3 }},
                 {targetsFile,
                  #data{ type=bool, align=119 }},
                 {targetsConst,
                  #data{ type=bool, align=118 }},
                 {targetsEnum,
                  #data{ type=bool, align=117 }},
                 {targetsEnumerant,
                  #data{ type=bool, align=116 }},
                 {targetsStruct,
                  #data{ type=bool, align=115 }},
                 {targetsField,
                  #data{ type=bool, align=114 }},
                 {targetsUnion,
                  #data{ type=bool, align=113 }},
                 {targetsGroup,
                  #data{ type=bool, align=112 }},
                 {targetsInterface,
                  #data{ type=bool, align=127 }},
                 {targetsMethod,
                  #data{ type=bool, align=126 }},
                 {targetsParam,
                  #data{ type=bool, align=125 }},
                 {targetsAnnotation,
                  #data{ type=bool, align=124 }}
                ]},
            #struct{ %% 0xb18aa5ac7a0d9420
              name=const, id=12793219851699983392, source= <<"schema.capnp:Node.const">>,
              dsize=5, psize=5, esize=inlineComposite,
              union_field=none,
              fields=
                [{type,
                  #ptr{ type={struct,15020482145304562784}, idx=3 }},
                 {value,
                  #ptr{ type={struct,14853958794117909659}, idx=4 }}
                ]},
            #struct{ %% 0xe82753cff0c2218f
              name=interface, id=16728431493453586831, source= <<"schema.capnp:Node.interface">>,
              dsize=5, psize=5, esize=inlineComposite,
              union_field=none,
              fields=
                [{methods,
                  #ptr{ type={list,{struct,10736806783679155584}}, idx=3 }}
                ]},
            #struct{ %% 0xb54ab3364333f598
              name=enum, id=13063450714778629528, source= <<"schema.capnp:Node.enum">>,
              dsize=5, psize=5, esize=inlineComposite,
              union_field=none,
              fields=
                [{enumerants,
                  #ptr{ type={list,{struct,10919677598968879693}}, idx=3 }}
                ]},
            #struct{ %% 0x9ea0b19b37fb4435
              name=struct, id=11430331134483579957, source= <<"schema.capnp:Node.struct">>,
              dsize=5, psize=5, esize=inlineComposite,
              union_field=none,
              fields=
                [{dataWordCount,
                  #data{ type=uint16, align=112 }},
                 {pointerCount,
                  #data{ type=uint16, align=192 }},
                 {preferredListEncoding,
                  #data{ type={enum,15102134695616452902}, align=208 }},
                 {isGroup,
                  #data{ type=bool, align=231 }},
                 {discriminantCount,
                  #data{ type=uint16, align=240 }},
                 {discriminantOffset,
                  #data{ type=uint32, align=256 }},
                 {fields,
                  #ptr{ type={list,{struct,11145653318641710175}}, idx=3 }}
                ]}
           ]},
       #struct{ %% 0x9aad50a41f4af45f
         name='Field', id=11145653318641710175, source= <<"schema.capnp:Field">>,
         dsize=3, psize=4, esize=inlineComposite,
         union_field=#data{ align=64, type=
           {union,
             [{slot,
               #group{ id=14133145859926553711 }},
              {group,
               #group{ id=14626792032033250577 }}
           ]} },
         fields=
           [{name,
             #ptr{ type=text, idx=0 }},
            {codeOrder,
             #data{ type=uint16, align=0 }},
            {annotations,
             #ptr{ type={list,{struct,17422339044421236034}}, idx=1 }},
            {discriminantValue,
             #data{ type=uint16, align=16 }},
            {ordinal,
             #group{ id=13515537513213004774 }}
           ],
         types=
           [#struct{ %% 0xbb90d5c287870be6
              name=ordinal, id=13515537513213004774, source= <<"schema.capnp:Field.ordinal">>,
              dsize=3, psize=4, esize=inlineComposite,
              union_field=#data{ align=80, type=
                {union,
                  [{implicit,void},
                   {explicit,
                    #data{ type=uint16, align=96 }}
                ]} }},
            #struct{ %% 0xcafccddb68db1d11
              name=group, id=14626792032033250577, source= <<"schema.capnp:Field.group">>,
              dsize=3, psize=4, esize=inlineComposite,
              union_field=none,
              fields=
                [{typeId,
                  #data{ type=uint64, align=128 }}
                ]},
            #struct{ %% 0xc42305476bb4746f
              name=slot, id=14133145859926553711, source= <<"schema.capnp:Field.slot">>,
              dsize=3, psize=4, esize=inlineComposite,
              union_field=none,
              fields=
                [{offset,
                  #data{ type=uint32, align=32 }},
                 {type,
                  #ptr{ type={struct,15020482145304562784}, idx=2 }},
                 {defaultValue,
                  #ptr{ type={struct,14853958794117909659}, idx=3 }}
                ]}
           ]},
       #struct{ %% 0x978a7cebdc549a4d
         name='Enumerant', id=10919677598968879693, source= <<"schema.capnp:Enumerant">>,
         dsize=1, psize=2, esize=inlineComposite,
         union_field=none,
         fields=
           [{name,
             #ptr{ type=text, idx=0 }},
            {codeOrder,
             #data{ type=uint16, align=0 }},
            {annotations,
             #ptr{ type={list,{struct,17422339044421236034}}, idx=1 }}
           ]},
       #struct{ %% 0x9500cce23b334d80
         name='Method', id=10736806783679155584, source= <<"schema.capnp:Method">>,
         dsize=1, psize=4, esize=inlineComposite,
         union_field=none,
         fields=
           [{name,
             #ptr{ type=text, idx=0 }},
            {codeOrder,
             #data{ type=uint16, align=0 }},
            {params,
             #ptr{ type={list,{struct,14681955158633610486}}, idx=1 }},
            {requiredParamCount,
             #data{ type=uint16, align=16 }},
            {returnType,
             #ptr{ type={struct,15020482145304562784}, idx=2 }},
            {annotations,
             #ptr{ type={list,{struct,17422339044421236034}}, idx=3 }}
           ],
         types=
           [#struct{ %% 0xcbc0c86dae91fcf6
              name='Param', id=14681955158633610486, source= <<"schema.capnp:Method.Param">>,
              dsize=0, psize=4, esize=inlineComposite,
              union_field=none,
              fields=
                [{name,
                  #ptr{ type=text, idx=0 }},
                 {type,
                  #ptr{ type={struct,15020482145304562784}, idx=1 }},
                 {defaultValue,
                  #ptr{ type={struct,14853958794117909659}, idx=2 }},
                 {annotations,
                  #ptr{ type={list,{struct,17422339044421236034}}, idx=3 }}
                ]}
           ]},
       #struct{ %% 0xd07378ede1f9cc60
         name='Type', id=15020482145304562784, source= <<"schema.capnp:Type">>,
         dsize=2, psize=1, esize=inlineComposite,
         union_field=#data{ align=0, type=
           {union,
             [{void,void},
              {bool,void},
              {int8,void},
              {int16,void},
              {int32,void},
              {int64,void},
              {uint8,void},
              {uint16,void},
              {uint32,void},
              {uint64,void},
              {float32,void},
              {float64,void},
              {text,void},
              {data,void},
              {list,
               #group{ id=9792858745991129751 }},
              {enum,
               #group{ id=11389172934837766057 }},
              {struct,
               #group{ id=12410354185295152851 }},
              {interface,
               #group{ id=17116997365232503999 }},
              {object,void}
           ]} },
         types=
           [#struct{ %% 0xed8bca69f7fb0cbf
              name=interface, id=17116997365232503999, source= <<"schema.capnp:Type.interface">>,
              dsize=2, psize=1, esize=inlineComposite,
              union_field=none,
              fields=
                [{typeId,
                  #data{ type=uint64, align=64 }}
                ]},
            #struct{ %% 0xac3a6f60ef4cc6d3
              name=struct, id=12410354185295152851, source= <<"schema.capnp:Type.struct">>,
              dsize=2, psize=1, esize=inlineComposite,
              union_field=none,
              fields=
                [{typeId,
                  #data{ type=uint64, align=64 }}
                ]},
            #struct{ %% 0x9e0e78711a7f87a9
              name=enum, id=11389172934837766057, source= <<"schema.capnp:Type.enum">>,
              dsize=2, psize=1, esize=inlineComposite,
              union_field=none,
              fields=
                [{typeId,
                  #data{ type=uint64, align=64 }}
                ]},
            #struct{ %% 0x87e739250a60ea97
              name=list, id=9792858745991129751, source= <<"schema.capnp:Type.list">>,
              dsize=2, psize=1, esize=inlineComposite,
              union_field=none,
              fields=
                [{elementType,
                  #ptr{ type={struct,15020482145304562784}, idx=0 }}
                ]}
           ]},
       #struct{ %% 0xce23dcd2d7b00c9b
         name='Value', id=14853958794117909659, source= <<"schema.capnp:Value">>,
         dsize=2, psize=1, esize=inlineComposite,
         union_field=#data{ align=0, type=
           {union,
             [{void,void},
              {bool,
               #data{ type=bool, align=23 }},
              {int8,
               #data{ type=int8, align=16 }},
              {int16,
               #data{ type=int16, align=16 }},
              {int32,
               #data{ type=int32, align=32 }},
              {int64,
               #data{ type=int64, align=64 }},
              {uint8,
               #data{ type=uint8, align=16 }},
              {uint16,
               #data{ type=uint16, align=16 }},
              {uint32,
               #data{ type=uint32, align=32 }},
              {uint64,
               #data{ type=uint64, align=64 }},
              {float32,
               #data{ type=float32, align=32 }},
              {float64,
               #data{ type=float64, align=64 }},
              {text,
               #ptr{ type=text, idx=0 }},
              {data,
               #data{ type=data, align=0 }},
              {list,
               #ptr{ type=object, idx=0 }},
              {enum,
               #data{ type=uint16, align=16 }},
              {struct,
               #ptr{ type=object, idx=0 }},
              {interface,void},
              {object,
               #ptr{ type=object, idx=0 }}
           ]} }},
       #struct{ %% 0xf1c8950dab257542
         name='Annotation', id=17422339044421236034, source= <<"schema.capnp:Annotation">>,
         dsize=1, psize=1, esize=inlineComposite,
         union_field=none,
         fields=
           [{id,
             #data{ type=uint64, align=0 }},
            {value,
             #ptr{ type={struct,14853958794117909659}, idx=0 }}
           ]},
       #enum{ %% 0xd1958f7dba521926
         name='ElementSize', id=15102134695616452902, source= <<"schema.capnp:ElementSize">>,
         values=
           [empty,
            bit,
            byte,
            twoBytes,
            fourBytes,
            eightBytes,
            pointer,
            inlineComposite
           ]},
       #struct{ %% 0xbfc546f6210ad7ce
         name='CodeGeneratorRequest', id=13818529054586492878, source= <<"schema.capnp:CodeGeneratorRequest">>,
         dsize=0, psize=2, esize=inlineComposite,
         union_field=none,
         fields=
           [{nodes,
             #ptr{ type={list,{struct,16610026722781537303}}, idx=0 }},
            {requestedFiles,
             #ptr{ type={list,{struct,14981803260258615394}}, idx=1 }}
           ],
         types=
           [#struct{ %% 0xcfea0eb02e810062
              name='RequestedFile', id=14981803260258615394, source= <<"schema.capnp:CodeGeneratorRequest.RequestedFile">>,
              dsize=1, psize=2, esize=inlineComposite,
              union_field=none,
              fields=
                [{id,
                  #data{ type=uint64, align=0 }},
                 {filename,
                  #ptr{ type=text, idx=0 }},
                 {imports,
                  #ptr{ type={list,{struct,12560611460656617445}}, idx=1 }}
                ],
              types=
                [#struct{ %% 0xae504193122357e5
                   name='Import', id=12560611460656617445, source= <<"schema.capnp:CodeGeneratorRequest.RequestedFile.Import">>,
                   dsize=1, psize=1, esize=inlineComposite,
                   union_field=none,
                   fields=
                     [{id,
                       #data{ type=uint64, align=0 }},
                      {name,
                       #ptr{ type=text, idx=0 }}
                     ]}
                ]}
           ]}
      ]}.
