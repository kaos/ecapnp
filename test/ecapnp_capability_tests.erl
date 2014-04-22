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
-include("include/ecapnp.hrl").

meck_test_() ->
    {setup,
     fun () -> setup_meck(foo, [{bar, fun() -> baz end}]) end,
     fun teardown_meck/1,
     [?_assertEqual(baz, foo:bar())
     ]}.

basicCap_test_() ->
    {setup,
     fun () -> setup_meck(basicCap, [basicCap_add()]) end,
     fun teardown_meck/1,
     [fun () ->
              {ok, Pid} = ecapnp_capability:start(basicCap, test_capnp:'BasicCap'()),
              ?assert(is_process_alive(Pid)),
              ok = ecapnp_capability:stop(Pid),
              ?assert(not is_process_alive(Pid))
      end,
      fun () ->
              S = test_capnp:'BasicCap'(),
              {ok, Pid} = ecapnp_capability:start_link(basicCap, S),
              basicCap_add(Pid, S, 123, 456)
      end
     ]}.

thirdCap_test_() ->
    {setup,
     fun () -> setup_meck(thirdCap, [basicCap_add(), otherCap_sqroot(), thirdCap_square()]) end,
     fun teardown_meck/1,
     [fun () ->
              S = test_capnp:'ThirdCap'(),
              {ok, Pid} = ecapnp_capability:start_link(thirdCap, S),
              basicCap_add(Pid, test_capnp:'BasicCap'(), 333, 666),
              otherCap_sqroot(Pid, test_capnp:'OtherCap'(), 4),
              thirdCap_square(Pid, S, 5)
      end
     ]}.


basicCap_add() ->
    {add, fun(Params, Results) ->
                  ecapnp:set(result,
                             ecapnp:get(a, Params)
                             + ecapnp:get(b, Params),
                             Results)
          end}.

basicCap_add(Pid, S, A, B) ->
    {ok, Params} = ecapnp:set_root(['BasicCap', [add, '$Params']], test_capnp),
    {ok, Result} = ecapnp:set_root(['BasicCap', [add, '$Results']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    ok = ecapnp:set(b, B, Params),
    %% todo: the Params should be a ref to AnyPointer data
    %% (to match that of Message.Call.Contents)..
    %% likewise, Result should also be a ref to AnyPointer
    %% (to match Return.results)
    ok = ecapnp_capability:dispatch_call(
           Pid, S#schema_node.id, 0,
           Params, Result),
    ?assertEqual(A+B, ecapnp:get(result, Result)).
    
otherCap_sqroot() ->    
    {sqroot, fun (Params, Results) ->
                     Sqrt = math:sqrt(ecapnp:get(a, Params)),
                     ecapnp:set(root1, Sqrt, Results),
                     ecapnp:set(root2, -Sqrt, Results)
             end}.

otherCap_sqroot(Pid, S, A) ->
    {ok, Params} = ecapnp:set_root(['OtherCap', [sqroot, '$Params']], test_capnp),
    {ok, Result} = ecapnp:set_root(['OtherCap', [sqroot, '$Results']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    %% todo: the Params should be a ref to AnyPointer data
    %% (to match that of Message.Call.Contents)..
    %% likewise, Result should also be a ref to AnyPointer
    %% (to match Return.results)
    ok = ecapnp_capability:dispatch_call(
           Pid, S#schema_node.id, 0,
           Params, Result),
    R1 = ecapnp:get(root1, Result),
    R2 = ecapnp:get(root2, Result),
    ?assertEqual(float(-A), R1*R2).
    
thirdCap_square() ->
    {square, fun (Params, Results) ->
                     A = ecapnp:get(a, Params),
                     ecapnp:set(sq, A * A, Results)
             end}.

thirdCap_square(Pid, S, A) ->
    {ok, Params} = ecapnp:set_root(['ThirdCap', [square, '$Params']], test_capnp),
    {ok, Result} = ecapnp:set_root(['ThirdCap', [square, '$Results']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    %% todo: the Params should be a ref to AnyPointer data
    %% (to match that of Message.Call.Contents)..
    %% likewise, Result should also be a ref to AnyPointer
    %% (to match Return.results)
    ok = ecapnp_capability:dispatch_call(
           Pid, S#schema_node.id, 0,
           Params, Result),
    ?assertEqual(A*A, ecapnp:get(sq, Result)).
    
%% basic_server_test() ->
%%     %% setup expectations
%%     setup_meck(basicCap, [{add,
%%                            fun(Params, Results) ->
%%                                    ecapnp:set(result,
%%                                         ecapnp:get(a, Params)
%%                                         + ecapnp:get(b, Params),
%%                                         Results)
%%                            end}
%%                          ]),
%%     %% start server for capability
%%     {ok, Cap} = ecapnp_capability:start('BasicCap', basicCap, test_capnp),
%%     %% prepare request
%%     {ok, Request} = ecapnp_capability:request(add, Cap),
%%     check_request('BasicCap', add, Request),
%%     Param = ecapnp_capability:param(Request),
%%     ok = ecapnp:set(a, 123, Param),
%%     ok = ecapnp:set(b, 456, Param),
%%     %% send request
%%     {ok, Result} = ecapnp_capability:send(Request),
%%     %% check_promise(...),
%%     %% wait for then verify response
%%     ok = ecapnp_capability:wait(Result),
%%     ?assertEqual(579, ecapnp:get(result, Result)),
%%     %% clean up
%%     ecapnp_capability:stop(Cap),
%%     teardown_meck(basicCap).

%% pipeline_test() ->
%%     %% setup expectations
%%     setup_meck(basicCap, [{add,
%%                            fun(Params, Results) ->
%%                                    ecapnp:set(result,
%%                                         ecapnp:get(a, Params)
%%                                         + ecapnp:get(b, Params),
%%                                         Results)
%%                            end}
%%                          ]),
%%     setup_meck(pipelines, [{getBasic,
%%                             fun(_Params, Results) ->
%%                                     {ok, Basic} = ecapnp_capability:start('BasicCap', basicCap, test_capnp),
%%                                     ecapnp:set(basic, Basic, Results), ok
%%                             end}
%%                           ]),
%%     %% start server for capabilities
%%     {ok, Pipe} = ecapnp_capability:start('Pipelines', pipelines, test_capnp),
%%     %% prepare request
%%     {ok, Request} = ecapnp_capability:request(getBasic, Pipe),
%%     check_request('Pipelines', getBasic, Request),
%%     %% send request
%%     {ok, Result} = ecapnp_capability:send(Request),
%%     %% check_promise(...),
%%     %% pipeline request
%%     Basic = ecapnp:get(basic, Result),
%%     {ok, PipeRequest} = ecapnp_capability:request(add, Basic),
%%     PipeParam = ecapnp_capability:param(PipeRequest),
%%     ok = ecapnp:set(a, 111, PipeParam),
%%     ok = ecapnp:set(b, 222, PipeParam),
%%     {ok, PipeResult} = ecapnp_capability:send(PipeRequest),
%%     %% wait for then verify response
%%     ok = ecapnp_capability:wait(Result),
%%     ok = ecapnp_capability:wait(PipeResult),
%%     ?assertEqual(333, ecapnp:get(result, PipeResult)),
%%     %% clean up
%%     ecapnp_capability:stop(Pipe),
%%     teardown_meck(basicCap),
%%     teardown_meck(pipelines).


%% check_request(Cap, Method, Req) ->
%%     #request{ method=ActualMethod, param=Object } = Req,
%%     ?assertEqual(Method, ActualMethod),
%%     {ok, Node} = ecapnp_schema:lookup(Cap, test_capnp),
%%     #method{ paramType=ParamType }
%%         = lists:keyfind(Method, #method.name,
%%                         (Node#schema_node.kind)#interface.methods),
%%     {ok, Params} = ecapnp_schema:lookup(ParamType, test_capnp),
%%     #object{ schema=ParamsNode, ref=ParamsRef } = Object,
%%     ?assertEqual(Params, ParamsNode),
%%     #schema_node{ kind=#struct{ dsize=DSize, psize=PSize } } = Params,
%%     ?assertEqual(
%%        #ref{ segment=0, pos=0, offset=0,
%%              data=ParamsRef#ref.data, %% don't care (it's a new pid every time)
%%              kind=#struct_ref{ dsize=DSize, psize=PSize } },
%%        ParamsRef).

        setup_meck(Mod, Funs) ->
                          ?assertEqual(ok, meck:new(Mod, [non_strict])),
                          [?assertEqual(ok, meck:expect(Mod, Fun, Impl))
                           || {Fun, Impl} <- Funs],
                          Mod.

teardown_meck(Mod) ->
    ?assert(meck:validate(Mod)),
    ?assertEqual(ok, meck:unload(Mod)).

-endif.
