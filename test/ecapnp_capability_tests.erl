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
-export([basicCap_funs/0]).
-import(ecapnp_test_utils, [meck/3, setup_meck/2, teardown_meck/1]).

-include_lib("eunit/include/eunit.hrl").
-include("include/ecapnp.hrl").


basicCap_test_() ->
    {setup,
     fun () ->
             {ok, _} = ecapnp_promise_sup:start_link(),
             setup_meck(basicCap, basicCap_funs())
     end,
     fun (Mod) ->
             teardown_meck(Mod)
     end,
     [fun () ->
              {ok, Pid} = ecapnp_capability:start([basicCap, [test_capnp:'BasicCap'()]]),
              ?assert(is_process_alive(Pid)),
              ok = ecapnp_capability:stop(Pid),
              receive after 100 -> ok end, %% ugly hack, I know..
              ?assert(not is_process_alive(Pid))
      end,
      fun () ->
              S = test_capnp:'BasicCap'(),
              {ok, Pid} = ecapnp_capability:start_link([basicCap, [S]]),
              test_basicCap_add(Pid, S, 123, 456)
      end
     ]}.

thirdCap_test_() ->
    meck(thirdCap, thirdCap_funs(),
         [fun () ->
                  S = test_capnp:'ThirdCap'(),
                  {ok, Pid} = ecapnp_capability:start_link([thirdCap, [S], {init, third}]),
                  test_basicCap_add(Pid, test_capnp:'BasicCap'(), 333, 666),
                  test_otherCap_sqroot(Pid, test_capnp:'OtherCap'(), 4),
                  test_thirdCap_square(Pid, S, 5)
          end
         ]).

basicCap_funs() ->
    [{handle_call, fun ('BasicCap', add, Params, Result, undefined) -> {basicCap_add(Params, Result), undefined};
                       ('BasicCap', sub, Params, Result, undefined) -> {basicCap_sub(Params, Result), undefined}
                   end}].

basicCap_add(Params, Results) ->
    ecapnp:set(result,
               ecapnp:get(a, Params)
               + ecapnp:get(b, Params),
               Results).

basicCap_sub(Params, Results) ->
    ecapnp:set(result,
               ecapnp:get(a, Params)
               - ecapnp:get(b, Params),
               Results).

test_basicCap_add(Pid, S, A, B) ->
    {ok, Params} = ecapnp:set_root(['BasicCap', [add, '$Params']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    ok = ecapnp:set(b, B, Params),
    Req = #rpc_call{
             interface = S#schema_node.id,
             method = 0,
             params = Params
            },
    Promise = ecapnp_capability:send(Pid, Req),
    {ok, Result} = ecapnp_promise:wait(Promise, 1000),
    ?assertEqual(A+B, ecapnp:get(result, Result)).

otherCap_sqroot(Params, Results) ->
    Sqrt = math:sqrt(ecapnp:get(a, Params)),
    ecapnp:set(root1, Sqrt, Results),
    ecapnp:set(root2, -Sqrt, Results).

test_otherCap_sqroot(Pid, S, A) ->
    {ok, Params} = ecapnp:set_root(['OtherCap', [sqroot, '$Params']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    {ok, Result} = ecapnp_promise:wait(
                     ecapnp_capability:send(
                       Pid, #rpc_call{ interface = S#schema_node.id,
                                       method = 0, params = Params }),
                     1000),
    R1 = ecapnp:get(root1, Result),
    R2 = ecapnp:get(root2, Result),
    ?assertEqual(float(-A), R1*R2).

thirdCap_funs() ->
    [{init, fun (third) -> {state, []} end},
     {handle_call,
      fun ('BasicCap', add, Params, Result, {state, []}) -> {basicCap_add(Params, Result), {state, []}};
          ('OtherCap', sqroot, Params, Result, {state, []}) -> {otherCap_sqroot(Params, Result), {state, []}};
          ('ThirdCap', square, Params, Result, {state, []}) -> {thirdCap_square(Params, Result), {state, []}}
      end}].

thirdCap_square(Params, Results) ->
    A = ecapnp:get(a, Params),
    ecapnp:set(sq, A * A, Results).

test_thirdCap_square(Pid, S, A) ->
    {ok, Params} = ecapnp:set_root(['ThirdCap', [square, '$Params']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    {ok, Result} = ecapnp_promise:wait(
                     ecapnp_capability:send(
                       Pid, #rpc_call{ interface = S#schema_node.id,
                                       method = 0, params = Params }),
                     1000),
    ?assertEqual(A*A, ecapnp:get(sq, Result)).

-endif.
