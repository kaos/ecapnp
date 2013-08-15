ecapnp
======

Cap'n Proto library for Erlang.

Currently only read operations are supported.

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

* `schemafile(get, FieldName, Object) :: Value | Object`

   Read `FieldName` of `Object`. For data fields, this results in a
   standard Erlang value, where as for objects it is a opaque value to
   be passed in another get field value request. List values are also
   plain Erlang lists, where each list element's value follows the
   above rules.

* `schemafile(schema) :: schema()`

   This returns the entire message schema.

The library has a few exported functions as well:

* `ecapnp:get_root(Type, Schema, Message) :: {ok, RootObject}`

   This is the function wrapped by `schemafile(root, ...)`.

* `ecapnp:get(Field, Object) :: Value | Object`

   This is the function wrapped by `schemafile(get, ...)`.

* `ecapnpc:compile_file(FileName)`
  `ecapnpc:compile_data(Binary)`
  `ecapnpc:compile_message(Message)`

   The `ecapnpc` module takes care of taking a capnp `schema` message,
   and compile it to a erlang header file.

Sample
======

In the Cap'n Proto distribution there is a AddressBook example.
As only read operations are yet supported, only half of the example is
implemented in ecapnp. See `priv/sample` for the implementation.

BUGS!!
======

On *Windows* (should you be unfortunate enough to be on that platform)
there is a rather bad design decision in Erlang to convert all `\r` to
`\n` that are read from `stdin`. Either patch `beam.smp.dll`, the
source, or avoid reading Cap'n Proto messages from `stdin`.

(I have a patch for erl 5.9.3.1 if you need one)
