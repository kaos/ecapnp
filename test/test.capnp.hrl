%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-export([test/1, test/2, test/3, test/4]).

%% test/4
test(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% test/3
test(root, Type, Message) ->
    ecapnp:get_root(Type, test(schema), Message);
test(get, Field, Object) ->
    ecapnp:get(Field, Object).

%% test/2
test(root, Type) ->
    ecapnp:set_root(Type, test(schema));
test(get, Object) ->
    ecapnp:get(Object).

%% test/1

test(schema) ->
  #schema{
    node=#node{ %% 0xe87e0317861d75a1
      name=test, id=16752831063434032545, source= <<"test/test.capnp">> },
    types=
      [#struct{
         node=#node{ %% 0xfa556038e27b336d
           name='Test', id=18038429679936549741, source= <<"test/test.capnp:Test">> },
         dsize=2, psize=4, esize=inlineComposite,
         union_field=#data{ align=16, type=
           {union,
             [{boolField,
               #data{ type=bool, align=15,
                      default= false }},
              {groupField,
               #group{ id=12591081617868223671 }}
           ]} },
         fields=
           [{intField,
             #data{ type=uint8, align=0,
                    default= 33 }},
            {textField,
             #ptr{ type=text, idx=0,
                   default= <<"test">> }},
            {opts,
             #group{ id=9356761420570873088 }},
            {meta,
             #group{ id=12292473172826227401 }}
           ],
         types=
           [#struct{
              node=#node{ %% 0xaa97a338ed5382c9
                name=meta, id=12292473172826227401, source= <<"test/test.capnp:Test.meta">> },
              dsize=2, psize=4, esize=inlineComposite,
              union_field=none,
              fields=
                [{id,
                  #data{ type=uint16, align=80,
                         default= 0 }},
                 {tag,
                  #ptr{ type=text, idx=2,
                        default= <<>> }},
                 {data,
                  #ptr{ type=data, idx=3,
                        default= <<"1234">> }}
                ]},
            #struct{
              node=#node{ %% 0x81d9e4f01134cd00
                name=opts, id=9356761420570873088, source= <<"test/test.capnp:Test.opts">> },
              dsize=2, psize=4, esize=inlineComposite,
              union_field=#data{ align=64, type=
                {union,
                  [{bool,
                    #data{ type=bool, align=55,
                           default= false }},
                   {text,
                    #ptr{ type=text, idx=1,
                          default= <<>> }},
                   {data,
                    #ptr{ type=data, idx=1,
                          default= null }},
                   {object,
                    #ptr{ type=object, idx=1,
                          default= null }}
                ]} }},
            #struct{
              node=#node{ %% 0xaebc820562fc74b7
                name=groupField, id=12591081617868223671, source= <<"test/test.capnp:Test.groupField">> },
              dsize=2, psize=4, esize=inlineComposite,
              union_field=none,
              fields=
                [{a,
                  #data{ type=int8, align=8,
                         default= -44 }},
                 {b,
                  #data{ type=int8, align=32,
                         default= 55 }},
                 {c,
                  #data{ type=int8, align=40,
                         default= 0 }}
                ]}
           ]}
      ]}.
