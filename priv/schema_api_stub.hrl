
_name_(root, Type, Message) ->
    ecapnp:get_root(Type, _name_(schema), Message);

_name_(get, Field, Object) ->
    ecapnp:get(Field, Object).
