---
layout: page
title: home
---

About ecapnp
============

Cap'n Proto library for Erlang.

NOTICE: Not all schema features are yet implemented. But it's getting closer..


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

Include the compiled `schemafile.capnp.hrl` header file in the module
where you need to read Cap'n Proto messages.


Documentation
=============

Read the [documentation](docs). (still butt ugly, sorry :/ )


Sample
======

In the Cap'n Proto distribution there is a AddressBook example. The
same example is provided in a `escript` version in the `priv/samples`
directory.

This version packs/unpacks the messages, so it works the same as the
C++ counterpart, and can be used interchangeably.


Tests
=====

There are both unit tests and integration tests. The `eunit` tests are
run using the Erlang Common Test Framework, started with the command:
`make tests`. The integration kind of tests are provided and run by
the [capnp_test](http://github.com/kaos/capnp_test) framework (which
is a language agnostic test framework for Cap'n Proto compiler
plugins), and is invoked with the command: `make check`.


BUGS!!
------

On *Windows* (should you be unfortunate enough to be on that platform)
there is a rather bad design decision in Erlang to convert all `\r` to
`\n` that are read from `stdin`. Either patch `beam.smp.dll`, the
source, or avoid reading Cap'n Proto messages from `stdin`.

(I have a patch for erl 5.9.3.1 if you need one)
