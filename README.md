ecapnp
======

[Cap'n Proto](http://capnproto.com) library for Erlang.

NOTICE: There's still some features missing..

## Status update

I am currently reworking the format of the compiled schema on the
erlang side of things. Instead of spitting out a `.hrl` file, it will
compile to a schema module.

The documentation is still not up-to-date with these changes..

Also, the previous eunit tests were a pain to maintain due to too much
bit mangling and low-level knowledge in the tests. Now I'm using
[PropEr](https://github.com/manopapad/proper) to overcome this.


### Note

In order for `ecapnp` to work properly, `ecapnp` has to be on the
Erlang lib path (i.e. `ERL_LIBS`), and `ecapnp/bin/capnpc-erl` needs to be on your `PATH`.


Try it:

    cd ../path/to/ecapnp
    export ERL_LIBS=$(dirname $(pwd))
    export PATH=$PATH:$(pwd)/bin
    make sample
    capnpc -oerl .../my_schema.capnp

This will _(given that everything is working as intended)_ produce a
`.../my_schema_capnp.erl` file that you can compile with `erlc`.


Web Site
--------

Head over to [ecapnp.astekk.se](http://ecapnp.astekk.se) for documentation etc.
