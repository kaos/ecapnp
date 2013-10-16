# Options for erlang.mk
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

# build rules for .capnp files
%.capnp.hrl: %.capnp
	$(gen_verbose) capnpc -oerl $<

# compiler schema dependencies
all: include/c++.capnp.hrl include/schema.capnp.hrl

# make sure we rebuild on any header file change
%.erl: include/*.hrl
	@touch $@

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
