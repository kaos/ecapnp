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
    

setup_meck(Mod, Funs) ->
    ?assertEqual(ok, meck:new(Mod, [non_strict])),
    [?assertEqual(ok, meck:expect(Mod, Fun, Impl))
     || {Fun, Impl} <- Funs],
    Mod.

teardown_meck(Mod) ->
    ?assert(meck:validate(Mod)),
    ?assertEqual(ok, meck:unload(Mod)).

-endif.
