PROJECT = ecapnp

test%: ERLC_OPTS += -DEUNIT_NOAUTO

CT_SUITES = eunit
PLT_APPS = crypto
EDOC_OPTS = preprocess

include erlang.mk

# erlang.mk bootstrapping
erlang.mk: erlang_mk_url ?= \
	http://raw.github.com/extend/erlang.mk/master/erlang.mk

erlang.mk:
	@echo " GET   " $@; wget -O $@ $(erlang_mk_url)

# compiler schema
all: include/schema.capnp.hrl
include/schema.capnp.hrl: /usr/local/include/capnp/schema.capnp
	$(gen_verbose) capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<

# test schema (for the eunit tests)
test/test.capnp.hrl: test/test.capnp
	$(gen_verbose) capnpc -oerl $<

# capnp_test integration
dep_capnp_test = git://github.com/kaos/capnp_test.git
$(eval $(call dep_target,capnp_test))

bin/test.capnp.hrl: $(DEPS_DIR)/capnp_test/test.capnp
	capnpc -oerl:$(dir $@) --src-prefix=$(dir $<) $<

.PHONY: check
check: export CAPNP_TEST_APP = $(CURDIR)/bin/ecapnp_test
check: $(DEPS_DIR)/capnp_test bin/test.capnp.hrl
	$(MAKE) -C $<
