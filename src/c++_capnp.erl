%% This file was generated 2014-04-02 09:44:05 UTC by ecapnp 0.2.
%% http://github.com/kaos/ecapnp
-module('c++_capnp').

-vsn(13688829037717245569).

-export([schema/1, namespace/0, '13386661402618388268'/0]).

-types([{13386661402618388268, namespace}]).

-include_lib("ecapnp/include/ecapnp.hrl").

namespace() -> '13386661402618388268'().

'13386661402618388268'() ->
    #schema_node{name = namespace, id = 13386661402618388268, src = <<"src/c++.capnp:namespace">>,
		 kind = #annotation{type = #ptr{type = text, idx = 0, default = <<>>}, targets = [targetsFile]}}.

schema(namespace) -> schema(13386661402618388268);
schema(13386661402618388268) -> '13386661402618388268'();
schema(_) -> undefined.