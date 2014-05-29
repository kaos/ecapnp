ecapnp
======

[Cap'n Proto](http://capnproto.com) library for Erlang.

NOTICE: This is work-in-progress. Feedback appreciated.


## Current Status

* Compiler

  The compiler produces standalone schema modules to either compiled
  `.beam` or in `.erl` source form.

* API

  The API are still quite verbose, and subject to change.

* Serialization support

  Most constructs should work. There are a few corner cases I haven't
  tested yet. *Packed* messages are also supported.

* RPC support

  The RPC support is shaping up, aiming for a level 1 implementation to be
  ready soon. The calculator sample from capnproto has been successfully
  ported to erlang.

* Tests

  In addition to the eunit tests, there are now also
  [PropEr](https://github.com/manopapad/proper) tests to cover more
  corner cases. I find that they complement each other well, as unit
  tests are easier to write for a specific slightly contrived
  scenario, while the property tests are well suited for a general
  approach covering as many different inputs as possible.

* Future work

  At this point, the goal is to get everything working properly. The
  next step will be to re-factor it into beautiful code/design, and
  after that, improvements for efficiency and performance.


### Note

In order for `ecapnp` to work properly, `ecapnp` has to be on the
Erlang lib path (i.e. `ERL_LIBS`), and `ecapnp/bin/capnpc-erl` needs
to be on your `PATH`.


Try it:

    cd ../path/to/ecapnp
    export ERL_LIBS=$(dirname $(pwd))
    export PATH=$PATH:$(pwd)/bin
    make samples
    capnpc -oerl .../my_schema.capnp

This will _(given that everything is working as intended)_ produce a
`.../my_schema_capnp.beam` file.

A few noteworthy options you can set in the environment are:

* `ECAPNP_TO_ERL` - Save erlang source of compiled schema to path
  (relative to `outdir`).

* `ECAPNP_NO_BEAM` - Do not compile to beam code.

* `ECAPNP_LOAD_BEAM` - Load compiled beam code on the running
  node. Useful in case you compile files programatically, rather than
  using the capnp plugin.

_(these options should be available as argument when compiling the code
generator request.. alas, that is yet to be implemented)_


Web Site
--------

Head over to [ecapnp.astekk.se](http://ecapnp.astekk.se) for documentation etc.
