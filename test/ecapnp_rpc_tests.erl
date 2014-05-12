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

-import(ecapnp_test_utils, [meck/2, setup_meck/2, teardown_meck/1]).
-import(ecapnp_capability_tests, [basicCap_funs/0]).

-record(test, {
          sup, basic, pipelines, mods
         }).

-export([foo/0]).
foo() ->
    {setup, S, T, {with, [_T1, _T2, T3|_]}} = rpc_local_test_(),
    X = S(),
    T3(X),
    T(X).

rpc_local_test_() ->
    {setup,
     fun () ->
             {ok, CapS} = ecapnp_capability_sup:start_link(),
             {ok, BasicCap} = ecapnp_capability_sup:start_capability(basicCap, test_capnp:'BasicCap'()),
             {ok, PipelinesCap} = ecapnp_capability_sup:start_capability(pipelines, test_capnp:'Pipelines'()),
             Mods = [setup_meck(Mod, Funs)
                     || {Mod, Funs} <-
                            [{basicCap, basicCap_funs()},
                             {pipelines, pipelines_funs(BasicCap)}
                            ]
                    ],
             #test{ sup = CapS, basic = BasicCap, pipelines = PipelinesCap, mods = Mods }
     end,
     fun (#test{ sup = CapS, mods = Mods }) ->
             exit(CapS, normal),
             [teardown_meck(Mod) || Mod <- Mods]
     end,
     {with,
      [fun test_request/1,
       fun test_send/1,
       fun test_pipeline/1
      ]}}.

pipelines_funs(BasicCap) ->
    [{handle_call, fun ('Pipelines', getBasic, _Params, Result) ->
                           ecapnp:set(basic, BasicCap, Result), ok
                   end}].

test_request(#test{ basic=Cap }) ->
    Req = ecapnp:request(add, Cap),
    Params = Req#rpc_call.params,
    ?assertEqual(
       #rpc_call{
          target = Cap,
          interface = test_capnp:'BasicCap'(),
          method = hd((test_capnp:'BasicCap'())#schema_node.kind#interface.methods),
          params = Params
         }, Req),
    ?assertEqual(
       ['BasicCap', [add, '$Params']],
       Params#object.schema#schema_node.name).

test_send(#test{ basic=Cap }) ->
    Req = ecapnp:request(sub, Cap),
    Params = Req#rpc_call.params,

    %% test set of both request object and the params directly
    ok = ecapnp:set(a, 333, Req),
    ok = ecapnp:set(b, 222, Params),
    %% test get of both params directly as well as from the request
    ?assertEqual(333, ecapnp:get(a, Params)),
    ?assertEqual(222, ecapnp:get(b, Req)),

    {ok, Promise} = ecapnp:send(Req),
    {ok, Res} = ecapnp:wait(Promise),
    ?assertEqual(111, ecapnp:get(result, Res)).

test_pipeline(#test{ pipelines = Cap }) ->
    BasicReq = ecapnp:request(getBasic, Cap),
    {ok, BasicPromise} = ecapnp:send(BasicReq),
    PromisedBasicCap = ecapnp:get(basic, BasicPromise),
    AddReq = ecapnp:request(add, PromisedBasicCap),
    ok = ecapnp:set(a, 123, AddReq),
    ok = ecapnp:set(b, 321, AddReq),
    {ok, AddPromise} = ecapnp:send(AddReq),
    %% get data on a promise will wait until fulfilled, then proceed
    ?assertEqual(444, ecapnp:get(result, AddPromise)).

%% request_test_() ->
%%     meck([{ecapnp_vat, vat_funs()},
%%           {basicCap, ecapnp_capability_tests:basicCap_funs()}
%%          ],
%%          [fun test_request/0
%%          ]).

%% send_test_() ->
%%     meck([{ecapnp_vat, vat_funs()},
%%           {basicCap, ecapnp_capability_tests:basicCap_funs()}
%%          ],
%%          [fun test_send/0
%%          ]).



%% vat_funs() ->
%%     [{send,
%%       fun (Req) ->
%%               %% hard coding a lot of stuff here for now, just to get going..
%%               Pid = self(),
%%               {ok, spawn_link(
%%                      fun () ->
%%                              {ok, Result} = ecapnp_set:root(
%%                                               test_capnp:schema(['BasicCap', [sub, '$Results']])),
%%                              ok = ecapnp_capability:dispatch_call(
%%                                     Req#rpc_call.target,
%%                                     Req#rpc_call.interface,
%%                                     Req#rpc_call.method,
%%                                     Req#rpc_call.params,
%%                                     Result),
%%                              Pid ! {self(), Result}
%%                      end)}
%%       end}
%%     ].


-endif.
