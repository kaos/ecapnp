%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-export([schema/1, schema/3]).

schema(root, Type, Message) ->
    ecapnp:get_root(Type, schema(schema), Message);

schema(get, Field, Object) ->
    ecapnp:get(Field, Object).

schema(schema) ->
  #schema{
    name=schema, id=16#b471df2f45ca32c7, source= <<"schema.capnp">>,
    types=
      [{'Node',
        #struct{
          name='Node', id=16#96ae9bbee664c195, source= <<"schema.capnp:Node">>,
          dsize=3, psize=4, fields=
            [{id,
              #data{ type=uint64, align=0 }},
             {displayName,
              #ptr{ type=text, idx=0 }},
             {scopeId,
              #data{ type=uint64, align=64 }},
             {nestedNodes,
              #ptr{ type={list,{struct,'NestedNode'}}, idx=1 }},
             {annotations,
              #ptr{ type={list,{struct,'Annotation'}}, idx=2 }},
             {body,
              #data{ align=128, type=
                {union,
                  [{fileNode,
                    #ptr{ type={struct,'FileNode'}, idx=3 }},
                   {structNode,
                    #ptr{ type={struct,'StructNode'}, idx=3 }},
                   {enumNode,
                    #ptr{ type={struct,'EnumNode'}, idx=3 }},
                   {interfaceNode,
                    #ptr{ type={struct,'InterfaceNode'}, idx=3 }},
                   {constNode,
                    #ptr{ type={struct,'ConstNode'}, idx=3 }},
                   {annotationNode,
                    #ptr{ type={struct,'AnnotationNode'}, idx=3 }}
                  ]} }},
             {displayNamePrefixLength,
              #data{ type=uint32, align=160 }},
             {union,
              #data{ align=96, type=
                   {union,
                    [{file, void},
                     {struct, void},
                     {enum, void},
                     {interface, void},
                     {const, void},
                     {annotation, void}
                    ]}}}
            ],
          types=
            [{'NestedNode',
              #struct{
                name='NestedNode', id=16#adfe8e889429ee28, source= <<"schema.capnp:Node.NestedNode">>,
                dsize=1, psize=1, fields=
                  [{name,
                    #ptr{ type=text, idx=0 }},
                   {id,
                    #data{ type=uint64, align=0 }}
                  ]}}
            ]}},
       {'Type',
        #struct{
          name='Type', id=16#dddca9a9ee299e42, source= <<"schema.capnp:Type">>,
          dsize=2, psize=1, fields=
            [{body,
              #data{ align=0, type=
                {union,
                  [{voidType,void},
                   {boolType,void},
                   {int8Type,void},
                   {int16Type,void},
                   {int32Type,void},
                   {int64Type,void},
                   {uint8Type,void},
                   {uint16Type,void},
                   {uint32Type,void},
                   {uint64Type,void},
                   {float32Type,void},
                   {float64Type,void},
                   {textType,void},
                   {dataType,void},
                   {listType,
                    #ptr{ type={struct,'Type'}, idx=0 }},
                   {enumType,
                    #data{ type=uint64, align=64 }},
                   {structType,
                    #data{ type=uint64, align=64 }},
                   {interfaceType,
                    #data{ type=uint64, align=64 }},
                   {objectType,void}
                  ]} }}
            ]}},
       {'Value',
        #struct{
          name='Value', id=16#c2c768aee22269ee, source= <<"schema.capnp:Value">>,
          dsize=2, psize=1, fields=
            [{body,
              #data{ align=0, type=
                {union,
                  [{uint64Value,
                    #data{ type=uint64, align=64 }},
                   {boolValue,
                    #data{ type=bool, align=64 }},
                   {int8Value,
                    #data{ type=int8, align=64 }},
                   {int16Value,
                    #data{ type=int16, align=64 }},
                   {int32Value,
                    #data{ type=int32, align=64 }},
                   {int64Value,
                    #data{ type=int64, align=64 }},
                   {uint8Value,
                    #data{ type=uint8, align=64 }},
                   {uint16Value,
                    #data{ type=uint16, align=64 }},
                   {uint32Value,
                    #data{ type=uint32, align=64 }},
                   {voidValue,void},
                   {float32Value,
                    #data{ type=float32, align=64 }},
                   {float64Value,
                    #data{ type=float64, align=64 }},
                   {textValue,
                    #ptr{ type=text, idx=0 }},
                   {dataValue,
                    #data{ type=data, align=0 }},
                   {listValue,
                    #data{ type=object, align=0 }},
                   {enumValue,
                    #data{ type=uint16, align=64 }},
                   {structValue,
                    #data{ type=object, align=0 }},
                   {interfaceValue,void},
                   {objectValue,
                    #data{ type=object, align=0 }}
                  ]} }}
            ]}},
       {'Annotation',
        #struct{
          name='Annotation', id=16#db785131c0cfee73, source= <<"schema.capnp:Annotation">>,
          dsize=1, psize=1, fields=
            [{id,
              #data{ type=uint64, align=0 }},
             {value,
              #ptr{ type={struct,'Value'}, idx=0 }}
            ]}},
       {'FileNode',
        #struct{
          name='FileNode', id=16#d59c380b31b76b1f, source= <<"schema.capnp:FileNode">>,
          dsize=0, psize=1, fields=
            [{imports,
              #ptr{ type={list,{struct,'Import'}}, idx=0 }}
            ],
          types=
            [{'Import',
              #struct{
                name='Import', id=16#d5d6a9044d63c158, source= <<"schema.capnp:FileNode.Import">>,
                dsize=1, psize=1, fields=
                  [{id,
                    #data{ type=uint64, align=0 }},
                   {name,
                    #ptr{ type=text, idx=0 }}
                  ]}}
            ]}},
       {'ElementSize',
        #enum{
          name='ElementSize', id=16#d7326bd22e1c298c, source= <<"schema.capnp:ElementSize">>,
          values=
            [empty,
             bit,
             byte,
             twoBytes,
             fourBytes,
             eightBytes,
             pointer,
             inlineComposite
            ]}},
       {'StructNode',
        #struct{
          name='StructNode', id=16#bf81d92a0b7e0c1f, source= <<"schema.capnp:StructNode">>,
          dsize=1, psize=1, fields=
            [{dataSectionWordSize,
              #data{ type=uint16, align=0 }},
             {pointerSectionSize,
              #data{ type=uint16, align=16 }},
             {preferredListEncoding,
              #data{ type={enum,'ElementSize'}, align=32 }},
             {members,
              #ptr{ type={list,{struct,'Member'}}, idx=0 }}
            ],
          types=
            [{'Member',
              #struct{
                name='Member', id=16#9a2db4bd6b74f8c1, source= <<"schema.capnp:StructNode.Member">>,
                dsize=1, psize=3, fields=
                  [{name,
                    #ptr{ type=text, idx=0 }},
                   {ordinal,
                    #data{ type=uint16, align=0 }},
                   {codeOrder,
                    #data{ type=uint16, align=16 }},
                   {annotations,
                    #ptr{ type={list,{struct,'Annotation'}}, idx=1 }},
                   {body,
                    #data{ align=32, type=
                      {union,
                        [{fieldMember,
                          #ptr{ type={struct,'Field'}, idx=2 }},
                         {unionMember,
                          #ptr{ type={struct,'Union'}, idx=2 }},
                         {groupMember,
                          #ptr{ type={struct,'Group'}, idx=2 }}
                        ]} }}
                  ]}},
             {'Field',
              #struct{
                name='Field', id=16#c75846e17057a41f, source= <<"schema.capnp:StructNode.Field">>,
                dsize=1, psize=2, fields=
                  [{offset,
                    #data{ type=uint32, align=0 }},
                   {type,
                    #ptr{ type={struct,'Type'}, idx=0 }},
                   {defaultValue,
                    #ptr{ type={struct,'Value'}, idx=1 }}
                  ]}},
             {'Union',
              #struct{
                name='Union', id=16#efff479ae161da06, source= <<"schema.capnp:StructNode.Union">>,
                dsize=1, psize=1, fields=
                  [{discriminantOffset,
                    #data{ type=uint32, align=0 }},
                   {members,
                    #ptr{ type={list,{struct,'Member'}}, idx=0 }}
                  ]}},
             {'Group',
              #struct{
                name='Group', id=16#ac91947f51b055ed, source= <<"schema.capnp:StructNode.Group">>,
                dsize=0, psize=1, fields=
                  [{members,
                    #ptr{ type={list,{struct,'Member'}}, idx=0 }}
                  ]}}
            ]}},
       {'EnumNode',
        #struct{
          name='EnumNode', id=16#d612f44d78962abf, source= <<"schema.capnp:EnumNode">>,
          dsize=0, psize=1, fields=
            [{enumerants,
              #ptr{ type={list,{struct,'Enumerant'}}, idx=0 }}
            ],
          types=
            [{'Enumerant',
              #struct{
                name='Enumerant', id=16#c9ac441973b9f177, source= <<"schema.capnp:EnumNode.Enumerant">>,
                dsize=1, psize=2, fields=
                  [{name,
                    #ptr{ type=text, idx=0 }},
                   {codeOrder,
                    #data{ type=uint16, align=0 }},
                   {annotations,
                    #ptr{ type={list,{struct,'Annotation'}}, idx=1 }}
                  ]}}
            ]}},
       {'InterfaceNode',
        #struct{
          name='InterfaceNode', id=16#b8a6ecfa2d5121e6, source= <<"schema.capnp:InterfaceNode">>,
          dsize=0, psize=1, fields=
            [{methods,
              #ptr{ type={list,{struct,'Method'}}, idx=0 }}
            ],
          types=
            [{'Method',
              #struct{
                name='Method', id=16#bdd7f6f0832387ac, source= <<"schema.capnp:InterfaceNode.Method">>,
                dsize=1, psize=4, fields=
                  [{name,
                    #ptr{ type=text, idx=0 }},
                   {codeOrder,
                    #data{ type=uint16, align=0 }},
                   {params,
                    #ptr{ type={list,{struct,'Param'}}, idx=1 }},
                   {requiredParamCount,
                    #data{ type=uint16, align=16 }},
                   {returnType,
                    #ptr{ type={struct,'Type'}, idx=2 }},
                   {annotations,
                    #ptr{ type={list,{struct,'Annotation'}}, idx=3 }}
                  ],
                types=
                  [{'Param',
                    #struct{
                      name='Param', id=16#d6d38cf4e366e91c, source= <<"schema.capnp:InterfaceNode.Method.Param">>,
                      dsize=0, psize=4, fields=
                        [{name,
                          #ptr{ type=text, idx=0 }},
                         {type,
                          #ptr{ type={struct,'Type'}, idx=1 }},
                         {defaultValue,
                          #ptr{ type={struct,'Value'}, idx=2 }},
                         {annotations,
                          #ptr{ type={list,{struct,'Annotation'}}, idx=3 }}
                        ]}}
                  ]}}
            ]}},
       {'ConstNode',
        #struct{
          name='ConstNode', id=16#8f0cf892b24a8062, source= <<"schema.capnp:ConstNode">>,
          dsize=0, psize=2, fields=
            [{type,
              #ptr{ type={struct,'Type'}, idx=0 }},
             {value,
              #ptr{ type={struct,'Value'}, idx=1 }}
            ]}},
       {'AnnotationNode',
        #struct{
          name='AnnotationNode', id=16#f386f41ae9f5cbe5, source= <<"schema.capnp:AnnotationNode">>,
          dsize=1, psize=1, fields=
            [{type,
              #ptr{ type={struct,'Type'}, idx=0 }},
             {targetsFile,
              #data{ type=bool, align=0 }},
             {targetsConst,
              #data{ type=bool, align=1 }},
             {targetsEnum,
              #data{ type=bool, align=2 }},
             {targetsEnumerant,
              #data{ type=bool, align=3 }},
             {targetsStruct,
              #data{ type=bool, align=4 }},
             {targetsField,
              #data{ type=bool, align=5 }},
             {targetsUnion,
              #data{ type=bool, align=6 }},
             {targetsInterface,
              #data{ type=bool, align=7 }},
             {targetsMethod,
              #data{ type=bool, align=8 }},
             {targetsParam,
              #data{ type=bool, align=9 }},
             {targetsAnnotation,
              #data{ type=bool, align=10 }}
            ]}},
       {'CodeGeneratorRequest',
        #struct{
          name='CodeGeneratorRequest', id=16#d095654a26e15f1d, source= <<"schema.capnp:CodeGeneratorRequest">>,
          dsize=0, psize=2, fields=
            [{nodes,
              #ptr{ type={list,{struct,'Node'}}, idx=0 }},
             {requestedFiles,
              #ptr{ type={list,uint64}, idx=1 }}
            ]}}
      ]}.
