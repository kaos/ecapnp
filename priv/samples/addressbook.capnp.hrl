%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").
-include_lib("ecapnp/include/capnp/c++.capnp.hrl").

-compile({nowarn_unused_function, addressbook/1}).
-compile({nowarn_unused_function, addressbook/2}).
-compile({nowarn_unused_function, addressbook/3}).
-compile({nowarn_unused_function, addressbook/4}).

%% Write value to object field.
%% -spec addressbook(set, Field::atom(), Value::term(), Object::#object{}) -> ok.
addressbook(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% Write unnamed union value
%% -spec addressbook(set, {field_name(), field_value()}|field_name(), object()) -> ok.
addressbook(set, Value, Object) ->
    ecapnp:set(Value, Object);

%% Get a reference to the root object in message.
%% -spec addressbook(root, Type::atom() | integer(), Message::list(binary())) -> {ok, Root::#object{}}.
addressbook(root, Type, Message) ->
    ecapnp:get_root(Type, addressbook(schema), Message);

%% Read object field value.
%% -spec addressbook(get, Field::atom(), Object::#object{}) -> term().
addressbook(get, Field, Object) ->
    ecapnp:get(Field, Object);

%% Type cast object to another struct or list.
%% -spec addressbook(to_struct, Type::atom() | integer(), Object1::#object{}) -> Object2#object{}.
%% -spec addressbook(to_list, Type::atom() | integer(), Object::#object{}) -> list().
addressbook(TypeCast, Type, Object)
  when TypeCast == to_struct;
       TypeCast == to_list ->
    ecapnp_obj:TypeCast(Type, Object).

%% Set root type for a new message.
%% -spec addressbook(root, Type::atom() | integer()) -> {ok, Root::#object{}}.
addressbook(root, Type) ->
    ecapnp:set_root(Type, addressbook(schema));

%% Read unnamed union value of object.
%% -spec addressbook(get, Object::#object{}) -> Tag::atom() | {Tag::atom(), Value::term()}.
addressbook(get, Object) ->
    ecapnp:get(Object);

%% Read const value from schema
%% -spec addressbook(const, Name:: type_name() | type_id()) -> value().
addressbook(const, Name) ->
    ecapnp:const(Name, addressbook(schema));
        
%% Type cast object to text/data.
%% -spec addressbook(to_text | to_data, Object::#object{}) -> binary().
addressbook(TypeCast, Object)
  when TypeCast == to_text;
       TypeCast == to_data ->
    ecapnp_obj:TypeCast(Object).

%% Get the compiled schema.
%% -spec addressbook(schema) -> #schema{}.

addressbook(schema) ->
  ['c++'(schema),
   #schema_node{ %% 0x9eb32e19f86ee174
     name=addressbook, id=11435534567900897652, src= <<"addressbook.capnp">>,
     kind=file,
     annotations=
       [{13386661402618388268,<<"addressbook">>}
       ],
     nodes=
       [#schema_node{ %% 0x98808e9832e8bc18
          name='Person', id=10988939875124296728, src= <<"addressbook.capnp:Person">>,
          kind=#struct{ dsize=1, psize=4, esize=inlineComposite,
            union_field=none,
            fields=
              [{id,
                #data{ type=uint32, align=0,
                       default= <<0,0,0,0>> }},
               {name,
                #ptr{ type=text, idx=0,
                      default= <<>> }},
               {email,
                #ptr{ type=text, idx=1,
                      default= <<>> }},
               {phones,
                #ptr{ type={list,{struct,9317543775882349264}}, idx=2,
                      default= <<0,0,0,0,0,0,0,0>> }},
               {employment,
                #group{ id=13477914502553102653 }}
              ]},
          nodes=
            [#schema_node{ %% 0xbb0b2bd4bdc3693d
               name=employment, id=13477914502553102653, src= <<"addressbook.capnp:Person.employment">>,
               kind=#struct{ dsize=1, psize=4, esize=inlineComposite,
                 union_field=#data{ align=32, default= <<0,0>>, type=
                   {union,
                     [{0,unemployed,void},
                      {1,employer,
                       #ptr{ type=text, idx=3,
                             default= <<>> }},
                      {2,school,
                       #ptr{ type=text, idx=3,
                             default= <<>> }},
                      {3,selfEmployed,void}
                   ]}}}},
             #schema_node{ %% 0x814e90b29c9e8ad0
               name='PhoneNumber', id=9317543775882349264, src= <<"addressbook.capnp:Person.PhoneNumber">>,
               kind=#struct{ dsize=1, psize=1, esize=inlineComposite,
                 union_field=none,
                 fields=
                   [{number,
                     #ptr{ type=text, idx=0,
                           default= <<>> }},
                    {type,
                     #data{ type={enum,10511609358742521391}, align=0,
                            default= <<0,0>> }}
                   ]},
               nodes=
                 [#schema_node{ %% 0x91e0bd04d585062f
                    name='Type', id=10511609358742521391, src= <<"addressbook.capnp:Person.PhoneNumber.Type">>,
                    kind=#enum{ values=
                      [{0,mobile},
                       {1,home},
                       {2,work}
                      ]}}
                 ]}
            ]},
        #schema_node{ %% 0xf934d9b354a8a134
          name='AddressBook', id=17957216978475721012, src= <<"addressbook.capnp:AddressBook">>,
          kind=#struct{ dsize=0, psize=1, esize=pointer,
            union_field=none,
            fields=
              [{people,
                #ptr{ type={list,{struct,10988939875124296728}}, idx=0,
                      default= <<0,0,0,0,0,0,0,0>> }}
              ]}}
       ]}
  ].
