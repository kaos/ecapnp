%%% DO NOT EDIT, this file was generated.
-include_lib("ecapnp/include/ecapnp.hrl").

-compile({nowarn_unused_function, 'c++'/1}).
-compile({nowarn_unused_function, 'c++'/2}).
-compile({nowarn_unused_function, 'c++'/3}).
-compile({nowarn_unused_function, 'c++'/4}).

%% Write value to object field.
%% -spec 'c++'(set, Field::atom(), Value::term(), Object::#object{}) -> ok.
'c++'(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% Write unnamed union value
%% -spec 'c++'(set, {field_name(), field_value()}|field_name(), object()) -> ok.
'c++'(set, Value, Object) ->
    ecapnp:set(Value, Object);

%% Get a reference to the root object in message.
%% -spec 'c++'(root, Type::atom() | integer(), Message::list(binary())) -> {ok, Root::#object{}}.
'c++'(root, Type, Message) ->
    ecapnp:get_root(Type, 'c++'(schema), Message);

%% Read object field value.
%% -spec 'c++'(get, Field::atom(), Object::#object{}) -> term().
'c++'(get, Field, Object) ->
    ecapnp:get(Field, Object);

%% Type cast object to another struct or list.
%% -spec 'c++'(to_struct, Type::atom() | integer(), Object1::#object{}) -> Object2#object{}.
%% -spec 'c++'(to_list, Type::atom() | integer(), Object::#object{}) -> list().
'c++'(TypeCast, Type, Object)
  when TypeCast == to_struct;
       TypeCast == to_list ->
    ecapnp_obj:TypeCast(Type, Object).

%% Set root type for a new message.
%% -spec 'c++'(root, Type::atom() | integer()) -> {ok, Root::#object{}}.
'c++'(root, Type) ->
    ecapnp:set_root(Type, 'c++'(schema));

%% Read unnamed union value of object.
%% -spec 'c++'(get, Object::#object{}) -> Tag::atom() | {Tag::atom(), Value::term()}.
'c++'(get, Object) ->
    ecapnp:get(Object);

%% Read const value from schema
%% -spec 'c++'(const, Name:: type_name() | type_id()) -> value().
'c++'(const, Name) ->
    ecapnp:const(Name, 'c++'(schema));
        
%% Type cast object to text/data.
%% -spec 'c++'(to_text | to_data, Object::#object{}) -> binary().
'c++'(TypeCast, Object)
  when TypeCast == to_text;
       TypeCast == to_data ->
    ecapnp_obj:TypeCast(Object).

%% Get the compiled schema.
%% -spec 'c++'(schema) -> #schema{}.

'c++'(schema) ->
  [#schema_node{ %% 0xbdf87d7bb8304e81
     name='c++', id=13688829037717245569, src= <<"include/c++.capnp">>,
     kind=file,
     annotations=
       [{13386661402618388268,<<"capnp::annotations">>}
     ],
     nodes=
       [#schema_node{ %% 0xb9c6f99ebf805f2c
        name=namespace, id=13386661402618388268, src= <<"include/c++.capnp:namespace">>,
        kind=#annotation{
          type=#ptr{ type=text, idx=0,
                default= <<>> },
          targets=
            [targetsFile
          ]}}
     ]}
  ].
