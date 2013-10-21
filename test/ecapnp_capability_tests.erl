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

meck_test() ->
    setup_meck(foo, [{bar, fun() -> baz end}]),
    ?assertEqual(baz, foo:bar()),
    teardown_meck(foo).

start_stop_server_test() ->
    {ok, Object} = ecapnp_capability:start('BasicCap', [], test(schema)),
    Pid = ecapnp_capability:pid(Object),
    ?assert(is_process_alive(Pid)),
    ok = ecapnp_capability:stop(Object),
    ?assert(not is_process_alive(Pid)).

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
    %% start server for capability
    {ok, Cap} = ecapnp_capability:start('BasicCap', basicCap, test(schema)),
    %% prepare request
    {ok, Request} = ecapnp_capability:request(add, Cap),
    check_request('BasicCap', add, Request),
    Param = ecapnp_capability:param(Request),
    ok = test(set, a, 123, Param),
    ok = test(set, b, 456, Param),
    %% send request
    {ok, Result} = ecapnp_capability:send(Request),
    %% check_promise(...),
    %% wait for then verify response
    ok = ecapnp_capability:wait(Result),
    ?assertEqual(579, test(get, result, Result)),
    %% clean up
    ecapnp_capability:stop(Cap),
    teardown_meck(basicCap).

pipeline_test() ->
    %% setup expectations
    setup_meck(basicCap, [{add,
                           fun(Params, Results) ->
                                   test(set, result,
                                        test(get, a, Params)
                                        + test(get, b, Params),
                                        Results)
                           end}
                         ]),
    setup_meck(pipelines, [{getBasic,
                            fun(_Params, Results) ->
                                    {ok, Basic} = ecapnp_capability:start('BasicCap', basicCap, test(schema)),
                                    test(set, basic, Basic, Results)
                            end}
                          ]),
    %% start server for capabilities
    {ok, Pipe} = ecapnp_capability:start('Pipelines', pipelines, test(schema)),
    %% prepare request
    {ok, Request} = ecapnp_capability:request(getBasic, Pipe),
    check_request('Pipelines', getBasic, Request),
    %% send request
    {ok, Result} = ecapnp_capability:send(Request),
    %% check_promise(...),
    %% pipeline request
    Basic = test(get, basic, Result),
    {ok, PipeRequest} = ecapnp_capability:request(add, Basic),
    PipeParam = ecapnp_capability:param(PipeRequest),
    ok = test(set, a, 111, PipeParam),
    ok = test(set, b, 222, PipeParam),
    {ok, PipeResult} = ecapnp_capability:send(PipeRequest),
    %% wait for then verify response
    ok = ecapnp_capability:wait(Result),
    ok = ecapnp_capability:wait(PipeResult),
    ?assertEqual(333, test(get, result, PipeResult)),
    %% clean up
    ecapnp_capability:stop(Pipe),
    teardown_meck(basicCap),
    teardown_meck(pipelines).


check_request(Cap, Method, Req) ->
    #request{ method=ActualMethod, param=Object } = Req,
    ?assertEqual(Method, ActualMethod),
    {ok, Node} = ecapnp_schema:lookup(Cap, test(schema)),
    #method{ paramType=ParamType }
        = lists:keyfind(Method, #method.name,
                        (Node#schema_node.kind)#interface.methods),
    {ok, Params} = ecapnp_schema:lookup(ParamType, test(schema)),
    #object{ schema=ParamsNode, ref=ParamsRef } = Object,
    ?assertEqual(Params, ParamsNode),
    #schema_node{ kind=#struct{ dsize=DSize, psize=PSize } } = Params,
    ?assertEqual(
       #ref{ segment=0, pos=0, offset=0,
             data=ParamsRef#ref.data, %% don't care (it's a new pid every time)
             kind=#struct_ref{ dsize=DSize, psize=PSize } },
       ParamsRef).

setup_meck(Mod, Funs) ->
    ?assertEqual(ok, meck:new(Mod, [non_strict])),
    [?assertEqual(ok, meck:expect(Mod, Fun, Impl))
     || {Fun, Impl} <- Funs].

teardown_meck(Mod) ->
    ?assert(meck:validate(Mod)),
    ?assertEqual(ok, meck:unload(Mod)).

-endif.
