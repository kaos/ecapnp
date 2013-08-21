ecapnp
======

[Cap'n Proto](http://capnproto.com) library for Erlang.

NOTICE: Not all schema features are yet implemented.


Web Site
========

Head over to [ecapnp.astekk.se](http://ecapnp.astekk.se) for documentation etc.


BUGS!!
======

On *Windows* (should you be unfortunate enough to be on that platform)
there is a rather bad design decision in Erlang to convert all `\r` to
`\n` that are read from `stdin`. Either patch `beam.smp.dll`, the
source, or avoid reading Cap'n Proto messages from `stdin`.

(I have a patch for erl 5.9.3.1 if you need one)
