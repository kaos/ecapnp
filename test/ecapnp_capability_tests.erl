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
-import(ecapnp_test_utils, [meck/3]).

-include_lib("eunit/include/eunit.hrl").
-include("include/ecapnp.hrl").


basicCap_test_() ->
    meck(basicCap, basicCap_funs(),
         [fun () ->
                  {ok, Pid} = ecapnp_capability:start(basicCap, [test_capnp:'BasicCap'()]),
                  ?assert(is_process_alive(Pid)),
                  ok = ecapnp_capability:stop(Pid),
                  receive after 100 -> ok end, %% ugly hack, I know..
                  ?assert(not is_process_alive(Pid))
          end,
          fun () ->
                  S = test_capnp:'BasicCap'(),
                  {ok, Pid} = ecapnp_capability:start_link(basicCap, [S]),
                  test_basicCap_add(Pid, S, 123, 456)
          end
         ]).

thirdCap_test_() ->
    meck(thirdCap, thirdCap_funs(),
         [fun () ->
                  S = test_capnp:'ThirdCap'(),
                  {ok, Pid} = ecapnp_capability:start_link(thirdCap, [S]),
                  test_basicCap_add(Pid, test_capnp:'BasicCap'(), 333, 666),
                  test_otherCap_sqroot(Pid, test_capnp:'OtherCap'(), 4),
                  test_thirdCap_square(Pid, S, 5)
          end
         ]).

basicCap_funs() ->
    [{handle_call, fun ('BasicCap', add, Params, Result) -> basicCap_add(Params, Result);
                       ('BasicCap', sub, Params, Result) -> basicCap_sub(Params, Result)
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
    {ok, Result} = ecapnp_capability:dispatch_call(Pid, S#schema_node.id, 0, Params),
    ?assertEqual(A+B, ecapnp:get(result, Result)).

otherCap_sqroot(Params, Results) ->
    Sqrt = math:sqrt(ecapnp:get(a, Params)),
    ecapnp:set(root1, Sqrt, Results),
    ecapnp:set(root2, -Sqrt, Results).

test_otherCap_sqroot(Pid, S, A) ->
    {ok, Params} = ecapnp:set_root(['OtherCap', [sqroot, '$Params']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    {ok, Result} = ecapnp_capability:dispatch_call(Pid, S#schema_node.id, 0, Params),
    R1 = ecapnp:get(root1, Result),
    R2 = ecapnp:get(root2, Result),
    ?assertEqual(float(-A), R1*R2).

thirdCap_funs() ->
    [{handle_call,
      fun ('BasicCap', add, Params, Result) -> basicCap_add(Params, Result);
          ('OtherCap', sqroot, Params, Result) -> otherCap_sqroot(Params, Result);
          ('ThirdCap', square, Params, Result) -> thirdCap_square(Params, Result)
      end}].

thirdCap_square(Params, Results) ->
    A = ecapnp:get(a, Params),
    ecapnp:set(sq, A * A, Results).

test_thirdCap_square(Pid, S, A) ->
    {ok, Params} = ecapnp:set_root(['ThirdCap', [square, '$Params']], test_capnp),
    ok = ecapnp:set(a, A, Params),
    {ok, Result} = ecapnp_capability:dispatch_call(Pid, S#schema_node.id, 0, Params),
    ?assertEqual(A*A, ecapnp:get(sq, Result)).

-endif.
