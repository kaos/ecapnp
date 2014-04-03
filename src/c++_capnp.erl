%% This file was generated 2014-04-03 09:39:35 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('c++_capnp').

-vsn(13688829037717245569).

-export([schema/1, namespace/0, namespace/1, '13386661402618388268'/0, root/0, root/1,
	 '13688829037717245569'/0]).

-types([{13386661402618388268, namespace}, {13688829037717245569, root}]).

-include_lib("ecapnp/include/ecapnp.hrl").

schema(13386661402618388268) -> '13386661402618388268'();
schema(namespace) -> '13386661402618388268'();
schema([namespace]) -> '13386661402618388268'();
schema(13688829037717245569) -> '13688829037717245569'();
schema(root) -> '13688829037717245569'();
schema([root]) -> '13688829037717245569'();
schema(_) -> undefined.

root() -> '13688829037717245569'().

root([]) -> '13688829037717245569'().

'13688829037717245569'() ->
    #schema_node{module = ?MODULE, name = root, id = 13688829037717245569, scope = 0,
		 src = <<"src/c++.capnp">>, annotations = [{13386661402618388268, <<"capnp::annotations">>}],
		 kind = file,
		 nodes =
		     [13386661402618388268]}.  %% namespace

namespace() -> '13386661402618388268'().

namespace([]) -> '13386661402618388268'().

'13386661402618388268'() ->
    #schema_node{module = ?MODULE, name = namespace, id = 13386661402618388268,
		 scope = 13688829037717245569, src = <<"src/c++.capnp:namespace">>,
		 kind = #annotation{type = #ptr{type = text, idx = 0, default = <<>>}, targets = [targetsFile]}}.