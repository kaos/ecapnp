-export([_name_/1, _name_/2, _name_/3, _name_/4]).

%% _name_/4
_name_(set, Field, Value, Object) ->
    ecapnp:set(Field, Value, Object).

%% _name_/3
_name_(root, Type, Message) ->
    ecapnp:get_root(Type, _name_(schema), Message);
_name_(get, Field, Object) ->
    ecapnp:get(Field, Object).

%% _name_/2
_name_(root, Type) ->
    ecapnp:set_root(Type, _name_(schema));
_name_(get, Object) ->
    ecapnp:get(Object).

%% _name_/1
