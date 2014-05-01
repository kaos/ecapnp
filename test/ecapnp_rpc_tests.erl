%%
%%  Copyright 2014, Andreas Stenius <kaos@astekk.se>
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

-module(ecapnp_rpc_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("include/ecapnp.hrl").
-import(ecapnp_test_utils, [meck/2]).

%% rpc_test() ->
%%     {ok, Vat} = ecapnp_vat:connect("localhost:4321"),
%%     {ok, Cap} = ecapnp_vat:import("Basic", Vat),
%%     {ok, Req} = ecapnp_rpc:request(add, Cap),
%%     ecapnp:set(foo, bar, Req),
%%     {ok, Res} = ecapnp_rpc:send(Req),
%%     ecapnp_rpc:wait(Res),
%%     ecapnp:get(res, Res).

request_test_() ->
    meck([{ecapnp_vat, vat_funs()},
          {basicCap, ecapnp_capability_tests:basicCap_funs()}
         ],
         [fun test_request/0
         ]).

send_test_() ->
    meck([{ecapnp_vat, vat_funs()},
          {basicCap, ecapnp_capability_tests:basicCap_funs()}
         ],
         [fun test_send/0
         ]).



vat_funs() ->
    [{request,
      fun (Interface, Method, Capability, Vat) ->
              {ok, Params} = ecapnp_set:root(
                               ecapnp_schema:lookup(Method#method.paramType, Interface)),
              #rpc_call{
                 vat = Vat,
                 target = Capability,
                 interface = Interface#schema_node.id,
                 method = Method#method.id,
                 params = Params }
      end},
     {send,
      fun (Req) ->
              %% hard coding a lot of stuff here for now, just to get going..
              Pid = self(),
              {ok, spawn_link(
                     fun () ->
                             {ok, Result} = ecapnp_set:root(
                                              test_capnp:schema(['BasicCap', [sub, '$Results']])),
                             ok = ecapnp_capability:dispatch_call(
                                    Req#rpc_call.target,
                                    Req#rpc_call.interface,
                                    Req#rpc_call.method,
                                    Req#rpc_call.params,
                                    Result),
                             Pid ! {self(), Result}
                     end)}
      end}
    ].

test_request() ->
    Vat = {vat_mock, test},
    {ok, Cap} = ecapnp_capability:start_link(basicCap, test_capnp:'BasicCap'()),
    Req = ecapnp_rpc:request(add, Cap, Vat),
    Params = Req#rpc_call.params,
    ?assertEqual(
       #rpc_call{
          vat = Vat,
          target = Cap,
          interface = (test_capnp:'BasicCap'())#schema_node.id,
          method = 0,
          params = Params
         }, Req),
    ?assertEqual(
       ['BasicCap', [add, '$Params']],
       Params#object.schema#schema_node.name).

test_send() ->
    Vat = {vat_mock, test},
    {ok, Cap} = ecapnp_capability:start_link(basicCap, test_capnp:'BasicCap'()),
    Req = ecapnp_rpc:request(sub, Cap, Vat),
    ok = ecapnp_rpc:set_param(a, 333, Req),
    ok = ecapnp_rpc:set_param(b, 222, Req),
    Params = Req#rpc_call.params,
    ?assertEqual(333, ecapnp:get(a, Params)),
    ?assertEqual(222, ecapnp:get(b, Params)),
    {ok, Promise} = ecapnp_rpc:send(Req),
    {ok, Res} = ecapnp_rpc:wait(Promise),
    ?assertEqual(111, ecapnp:get(result, Res)).

-endif.
