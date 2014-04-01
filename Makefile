# Options for erlang.mk
PROJECT = ecapnp

test%: TEST_ERLC_OPTS += -DEUNIT_NOAUTO

CT_SUITES = proper
PLT_APPS = crypto
EDOC_OPTS = preprocess

# call `make tests TEST_DEPS=` after the first run in order to skip
# the `make all` for all test deps.. (which for meck using rebar is
# sloooow... :/ )
TEST_DEPS ?= meck

dep_meck = https://github.com/eproxus/meck.git master

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
all: include/capnp/c++.capnp.hrl include/capnp/schema.capnp.hrl

# make sure we rebuild on any header file change
%.erl: include/*.hrl include/*/*.hrl ; @touch $@

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


# DEV/TEST-only target..
# call it as `make dbg PROP=text_data LINE=117`
# will dump you attached to a process running the text_data prop test,
# on line 117
# Currently we need ecapnp on the erlang lib path.. will fix that eventually..
.PHONY: bld dbg tst
bld: TEST_DEPS=
bld: TEST_ERLC_OPTS += -DEUNIT_NOAUTO
bld: app build-tests

dbg: bld
	erl -pa test -pa deps/proper/ebin -eval \
		"begin\
			[i:ii(M) || M <- [ecapnp, ecapnp_obj, ecapnp_get, ecapnp_set,\
				ecapnp_props, ecapnp_ref, ecapnp_data]],\
			i:ib(ecapnp_props, $(LINE)),\
			i:iaa([break]),\
			proper:quickcheck(ecapnp_props:prop_$(PROP)())\
		end"

tst: bld
	erl -pa test -pa deps/proper/ebin -noinput \
		-eval "proper:module(ecapnp_props), init:stop()"
