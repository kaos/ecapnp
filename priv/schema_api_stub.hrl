
-compile({nowarn_unused_function, _name_/1}).
-compile({nowarn_unused_function, _name_/2}).
-compile({nowarn_unused_function, _name_/3}).
-compile({nowarn_unused_function, _name_/4}).

%% Write value to object field.
%% -spec _name_(set, Field::atom(), Value::term(), Object::#object{}) -> ok.
_name_(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% Get a reference to the root object in message.
%% -spec _name_(root, Type::atom() | integer(), Message::list(binary())) -> {ok, Root::#object{}}.
_name_(root, Type, Message) ->
    ecapnp:get_root(Type, _name_(schema), Message);

%% Read object field value.
%% -spec _name_(get, Field::atom(), Object::#object{}) -> term().
_name_(get, Field, Object) ->
    ecapnp:get(Field, Object);

%% Type cast object to another struct or list.
%% -spec _name_(to_struct, Type::atom() | integer(), Object1::#object{}) -> Object2#object{}.
%% -spec _name_(to_list, Type::atom() | integer(), Object::#object{}) -> list().
_name_(TypeCast, Type, Object)
  when TypeCast == to_struct;
       TypeCast == to_list ->
    ecapnp_obj:TypeCast(Type, Object).

%% Set root type for a new message.
%% -spec _name_(root, Type::atom() | integer()) -> {ok, Root::#object{}}.
_name_(root, Type) ->
    ecapnp:set_root(Type, _name_(schema));

%% Read unnamed union value of object.
%% -spec _name_(get, Object::#object{}) -> Tag::atom() | {Tag::atom(), Value::term()}.
_name_(get, Object) ->
    ecapnp:get(Object);

%% Type cast object to text/data.
%% -spec _name_(to_text | to_data, Object::#object{}) -> binary().
_name_(TypeCast, Object)
  when TypeCast == to_text;
       TypeCast == to_data ->
    ecapnp_obj:TypeCast(Object).

%% Get the compiled schema.
%% -spec _name_(schema) -> #schema{}.
