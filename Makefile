PROJECT = ecapnp

#DEPS_DIR = $(CURDIR)/test

#TEST_DEPS = capnp_test
dep_capnp_test = git://github.com/kaos/capnp_test.git

#test%: export CAPNP_TEST_APP = $(CURDIR)/bin/ecapnp_test

CT_SUITES = eunit

include erlang.mk

# erlang.mk bootstrapping
erlang.mk: erlang_mk_url ?= \
	http://raw.github.com/extend/erlang.mk/master/erlang.mk

erlang.mk:
	@echo " GET   " $@; wget -O $@ $(erlang_mk_url)

include/schema.capnp.hrl: /usr/local/include/capnp/schema.capnp
	capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<

.PHONY: capnp_test
capnp_test: export CAPNP_TEST_APP = $(CURDIR)/bin/ecapnp_test
capnp_test: test-schema build-test-deps

test-schema: $(DEPS_DIR)/capnp_test bin/test.capnp.hrl

bin/test.capnp.hrl: $(DEPS_DIR)/capnp_test/test.capnp
	capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<
