PROJECT = ecapnp

#TEST_DEPS = capnp_test
dep_capnp_test = git://github.com/kaos/capnp_test.git

%/build-tests: export CAPNP_TEST_APP = $(CURDIR)/bin/ecapnp_test
%/build-tests: ERLC_OPTS += -DEUNIT_NOAUTO

CT_SUITES = eunit

TRACK_SOURCE_DEPENDENCIES = yes

include erlang.mk

# erlang.mk bootstrapping
erlang.mk: erlang_mk_url ?= \
	http://raw.github.com/extend/erlang.mk/master/erlang.mk

erlang.mk:
	@echo " GET   " $@; wget -O $@ $(erlang_mk_url)

include/schema.capnp.hrl: /usr/local/include/capnp/schema.capnp
	$(gen_verbose) capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<

test/test.capnp.hrl: test/test.capnp
	$(gen_verbose) capnpc -oerl $<


.PHONY: check capnp_test
# alias tests to check
check: tests

capnp_test: export CAPNP_TEST_APP = $(CURDIR)/bin/ecapnp_test
capnp_test: test-schema build-test-deps

test-schema: $(DEPS_DIR)/capnp_test bin/test.capnp.hrl

bin/test.capnp.hrl: $(DEPS_DIR)/capnp_test/test.capnp
	capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<
