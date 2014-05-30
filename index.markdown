---
layout: page
title: ecapnp home
---

Home | [Stuff todo](/todo)


About ecapnp
============

Cap'n Proto library for Erlang.


Status
------

Serialization should be pretty stable, also supporting packed
messages.

RPC is almost complete for level 1, but not yet stable (the sample
calculator application is working, both client and server).


Prerequisites
-------------

Naturally, the [Cap'n Proto](http://capnproto.com) framework is needed
in order to compile any `.capnp` schema files.


Usage
=====

Build and install `ecapnp` somewhere in you `ERL_LIBS` path.

Then use `capnp` to compile schema files to be used in Erlang:

{% highlight bash %}
capnp compile -oerl <schemafile.capnp>
{% endhighlight %}

See `capnp compile --help` for compile options.

It compiles the schema to a `schemafile_capnp.beam` module for you to
use whenever you need to read/write Cap'n Proto messages.


Documentation
=============

Read the [documentation](docs). (still butt ugly, sorry :/ )


Sample applications
===================

In the Cap'n Proto distribution there is both a AddressBook and a
Calculator example, which have been ported to Erlang using
`ecapnp`. They live in the `priv/samples` directory, and may be
invoked with `make samples`.


Tests
=====

There are both unit tests and integration tests. The unit tests are
implemented using both `eunit` and `proper` and are run using the
Erlang Common Test Framework, started with the command: `make
tests`.

The integration kind of tests are provided and run by the
[capnp_test](http://github.com/kaos/capnp_test) framework (which is a
language agnostic test framework for Cap'n Proto compiler plugins),
and is invoked with the command: `make check`.


BUGS!!
------

On *Windows* (should you be unfortunate enough to be on that platform)
there is a rather bad design decision in Erlang to convert all `\r` to
`\n` that are read from `stdin`. Either patch `beam.smp.dll`, the
source, or avoid reading Cap'n Proto messages from `stdin`.

(I used to have a patch for erl 5.9.3.1 I just might be able to dig
up, in case you need it)
