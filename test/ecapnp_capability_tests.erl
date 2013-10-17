%%  
%%  Copyright 2013, Andreas Stenius <kaos@astekk.se>
%%  
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%  
%%     http://www.apache.org/licenses/LICENSE-2.0
%%  
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%  

-module(ecapnp_capability_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/test.capnp.hrl").

start_stop_server_test() ->
    {ok, Server} = ecapnp_capability:start('BasicCap', [], test(schema)),
    ?assert(is_process_alive(Server)),
    ok = ecapnp_capability:stop(Server),
    ?assert(not is_process_alive(Server)).

meck_test() ->
    setup_meck(foo, [{bar, fun() -> baz end}]),
    ?assertEqual(baz, foo:bar()),
    teardown_meck(foo).

basic_server_test() ->
    %% setup expectations
    setup_meck(basicCap, [{add,
                       fun(Params, Results) ->
                               test(set, result,
                                    test(get, a, Params)
                                    + test(get, b, Params),
                                    Results)
                       end}
                     ]),
    %% prepare request
    {ok, Request} = ecapnp_capability:request('BasicCap', add, test(schema)),
    ok = test(set, a, 123, Request),
    ok = test(set, b, 456, Request),
    %% start server and send request
    {ok, Server} = ecapnp_capability:start('BasicCap', basicCap, test(schema)),
    {ok, Response} = ecapnp_capability:call(Server, Request),
    %% verify response
    ?assertEqual(579, test(get, result, Response)),
    teardown_meck(basicCap).



setup_meck(Mod, Funs) ->
    ?assertEqual(ok, meck:new(Mod, [non_strict])),
    [?assertEqual(ok, meck:expect(Mod, Fun, Impl))
     || {Fun, Impl} <- Funs].

teardown_meck(Mod) ->
    ?assert(meck:validate(Mod)),
    ?assertEqual(ok, meck:unload(Mod)).

-endif.
