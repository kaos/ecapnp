all: include/schema.capnp.hrl
	./rebar compile

include/schema.capnp.hrl: /usr/local/include/capnp/schema.capnp
	capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<
