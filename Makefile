PROJECT = ecapnp

DEPS_DIR = $(CURDIR)/test

TEST_DEPS = capnp_test
dep_capnp_test = git://github.com/kaos/capnp_test.git

include erlang.mk

# erlang.mk bootstrapping
erlang.mk: erlang_mk_url ?= \
	http://raw.github.com/extend/erlang.mk/master/erlang.mk

erlang.mk:
	@echo " GET   " $@; wget -O $@ $(erlang_mk_url)

include/schema.capnp.hrl: /usr/local/include/capnp/schema.capnp
	capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<

# target: check, for running the `capnp_test` tests
.PHONY: check
check: export CAPNP_TEST_APP = $(CURDIR)/bin/ecapnp_test
check: build-test-deps
