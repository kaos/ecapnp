ecapnp
======

Cap'n Proto library for Erlang.

NOTICE: Not all schema features are yet implemented.


Prerequisites
-------------

Naturally, the
[Cap'n Proto](http://kentonv.github.io/capnproto/index.html) framework
is needed in order to compile any `.capnp` schema files.


Usage
=====

Build and install `ecapnp` somewhere in you `ERL_LIBS` path.

Then use `capnp` to compile schema files to be used in Erlang:

```
   capnp compile -oerl <schemafile.capnp>
```

See `capnp compile --help` for compile options.

Include the compiled `schemafile.capnp.hrl` header file in the module
where you need to read Cap'n Proto messages.


Basic API
=========

The compiled schema adds a few functions to help read messages.

* `schemafile(root, 'RootStructTypeName', Message) :: {ok, RootObject}`

   Get the message's root object. `Message` is a list of binary
   segments, as returned by `ecapnp_message:read/1`.

* `schemafile(root, 'RootStructTypeName') :: {ok, RootObject}`

   Create a new message with the given root object.
   
* `schemafile(get, FieldName, Object) :: Value | Object | list(Object)`

   Read `FieldName` of `Object`. For data fields, this results in a
   standard Erlang value, where as for objects it is a opaque value to
   be passed in another get field value request. List values are also
   plain Erlang lists, where each list element's value follows the
   above rules.

* `schemafile(set, FieldName, Value, Object) :: ok | list(Object)`

   Write `Value` to `FieldName` of `Object`. In case the field is a list
   of objects, the `Value` should be number of elements to allocate for
   the list, and the result is a list of objects.
   The implementation of other list types are incomplete at this moment.
   
* `schemafile(schema) :: schema()`

   This returns the entire message schema.

The library has a few exported functions as well:

* `ecapnp:get_root(Type, Schema, Message) :: {ok, RootObject}`

   This is the function wrapped by `schemafile(root, ...)`.

* `ecapnp:set_root(Type, Schema) :: {ok, RootObject}`

   This is the function wrapped by `schemafile(root, ...)`.

* `ecapnp:get(Field, Object) :: Value | Object`

   This is the function wrapped by `schemafile(get, ...)`.

* `ecapnp:set(Field, Value, Object) :: ok | list(Object)`

   This is the function wrapped by `schemafile(set, ...)`.

* `ecapnp_message:read(Data) :: Message`

   Parse a binary Cap'n Proto message. The `Data` should begin with
   the message header (segment count and sizes) followed by the
   segment data.

* `ecapnp_message:write(Object) :: binary()`

   Build a binary Cap'n Proto message with proper message header. Any
   `Object` that belongs to the message may be used as argument to
   this call.

* `ecapnp_serialize:unpack(Data) :: binary()`

   If a message is packed, it has to be unpacked prior to reading it
   with `ecapnp_message:read/1`.
   
* `ecapnp_serialize:pack(Data) :: binary()`

   Any message can be packed in order to reduce the message size.

* `ecapnpc:compile_file(FileName)`
  `ecapnpc:compile_data(Binary)`
  `ecapnpc:compile_message(Message)`

   The `ecapnpc` module takes care of taking a capnp `schema` message,
   and compile it to a erlang header file.


Sample
======

In the Cap'n Proto distribution there is a AddressBook example. The
same example is provided in a `escript` version in the `priv/samples`
directory.

This version packs/unpacks the messages, so it works the same as the
C++ counterpart, and can be used interchangeably.


BUGS!!
======

On *Windows* (should you be unfortunate enough to be on that platform)
there is a rather bad design decision in Erlang to convert all `\r` to
`\n` that are read from `stdin`. Either patch `beam.smp.dll`, the
source, or avoid reading Cap'n Proto messages from `stdin`.

(I have a patch for erl 5.9.3.1 if you need one)
