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
          sup, basic, pipelines, mods, bridge, vat
         }).

%%-define(RUN,1).
-ifdef(RUN).
%% code to easily run test in debugger..
-export([run/0]).
run() ->
    {setup, S, T, {with, Ts}} = rpc_local_test_(),
    %%{setup, S, T, {with, Ts}} = rpc_remote_test_(),
    X = S(),
    [case P of
         #capability{ id = {local, Pid} } ->
             io:format("trace cap ~p~n", [Pid]),
             sys:trace(Pid, true);
         Pid when is_pid(Pid) ->
             io:format("trace pid ~p~n", [Pid]),
             sys:trace(Pid, true);
         _ -> nop
     end || P <- lists:flatten(erlang:tuple_to_list(X)), P /= X#test.bridge],
    (lists:nth(?RUN, Ts))(X),
    T(X).
-endif.

rpc_local_test_() ->
    {setup,
     fun () ->
             Mods = [setup_meck(Mod, Funs)
                     || {Mod, Funs} <-
                            [{basicCap, basicCap_funs()},
                             {pipelines, pipelines_funs()}
                            ]
                    ],
             {ok, CapS} = ecapnp_capability_sup:start_link(),
             {ok, ProS} = ecapnp_promise_sup:start_link(),
             {ok, BasicCap} = ecapnp_capability_sup:start_capability(basicCap, test_capnp:'BasicCap'()),
             {ok, PipelinesCap} = ecapnp_capability_sup:start_capability(pipelines, test_capnp:'Pipelines'(), [{init, BasicCap}]),
             #test{ sup = [CapS, ProS], basic = BasicCap, pipelines = PipelinesCap, mods = Mods }
     end,
     fun (#test{ sup = Sups, mods = Mods }) ->
             [exit(S, normal) || S <- Sups],
             [teardown_meck(Mod) || Mod <- Mods]
     end,
     {with,
      [fun test_request/1,
       fun test_send/1,
       fun test_pipeline/1
      ]}}.

rpc_remote_test_() ->
    %% the remote test really is a local test, using two bridged
    %% client vat processes, so works exactly the same as for a real
    %% remote one, except there's no TCP/IP stack involved here..
    {setup,
     fun () ->
             Mods = [setup_meck(Mod, Funs)
                     || {Mod, Funs} <-
                            [{basicCap, basicCap_funs()},
                             {pipelines, pipelines_funs()},
                             {bridge_echo, echo_funs()}
                            ]
                    ],
             {ok, CapS} = ecapnp_capability_sup:start_link(),
             {ok, ProS} = ecapnp_promise_sup:start_link(),
             {ok, BasicCap} = ecapnp_capability_sup:start_capability(basicCap, test_capnp:'BasicCap'()),
             {ok, PipelinesCap} = ecapnp_capability_sup:start_capability(pipelines, test_capnp:'Pipelines'(), [{init, BasicCap}]),
             Bridge = spawn_link(
                       fun () ->
                               Loop = fun (F, VatA, VatB) ->
                                              receive
                                                  {From, stop} ->
                                                      [ecapnp_vat:stop(Vat) || Vat <- [VatA, VatB]],
                                                      From ! self();
                                                  {VatA, Data} ->
                                                      VatB ! {receive_data, Data},
                                                      F(F, VatA, VatB);
                                                  {VatB, Data} ->
                                                      VatA ! {receive_data, Data},
                                                      F(F, VatA, VatB);
                                                  Other ->
                                                      %% doesn't expect anything here..
                                                      io:format("bridge: ~p~n", [Other]),
                                                      F(F, VatA, VatB)
                                              end
                                      end,
                               receive
                                   {VatA, VatB} ->
                                       Loop(Loop, VatA, VatB)
                               end
                       end),
             {ok, Client} = ecapnp_vat:start_link({bridge_echo, Bridge}),
             {ok, Server} = ecapnp_vat:start_link({bridge_echo, Bridge},
                                                  cap_restorer([{<<"basic">>, BasicCap},
                                                                {<<"pipelines">>, PipelinesCap}])),
             Bridge ! {Client, Server},
             io:format("VAT A: ~p~nVAT B: ~p~n", [Client, Server]),
             sys:trace(Server, true),
             #test{ sup = [CapS, ProS], basic = BasicCap, pipelines = PipelinesCap,
                    mods = Mods, bridge = Bridge, vat = Client }
     end,
     fun (#test{ sup = Sups, mods = Mods, bridge = Bridge }) ->
             Bridge ! {self(), stop},
             receive Bridge -> ok end,
             [exit(S, normal) || S <- Sups],
             [teardown_meck(Mod) || Mod <- Mods]
     end,
     {with,
      [fun test_remote_basic/1
       %%fun test_pipeline/1
      ]}}.

echo_funs() ->
    [{send, fun (Pid, Data) -> Pid ! {self(), Data}, ok end}].

pipelines_funs() ->
    [{init, fun (State) -> State end},
     {handle_call, fun ('Pipelines', getBasic, _Params, Result, BasicCap) ->
                           ecapnp:set(basic, BasicCap, Result),
                           {ok, BasicCap}
                   end}].

cap_restorer(Caps) ->
    fun (ObjectId, _Vat) ->
            case lists:keyfind(ecapnp_obj:to_text(ObjectId), 1, Caps) of
                false -> undefined;
                {_, Cap} -> {ok, Cap}
            end
    end.

test_request(#test{ basic=Cap }) ->
    Req = ecapnp:request(add, Cap),
    Params = Req#rpc_call.params,
    Schema = test_capnp:'BasicCap'(),
    Method = hd(Schema#schema_node.kind#interface.methods),
    ?assertEqual(
       #rpc_call{
          target = Cap#object.ref#ref.kind,
          interface = Schema#schema_node.id,
          method = Method#method.id,
          params = Params,
          resultSchema = test_capnp:schema(Method#method.resultType)
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

    Promise = ecapnp:send(Req),
    {ok, Res} = ecapnp:wait(Promise),
    ?assertEqual(111, ecapnp:get(result, Res)).

test_pipeline(#test{ pipelines = Cap }) ->
    BasicReq = ecapnp:request(getBasic, Cap),
    BasicPromise = ecapnp:send(BasicReq),
    PromisedBasicCap = ecapnp:get(basic, BasicPromise),
    AddReq = ecapnp:request(add, PromisedBasicCap),
    ok = ecapnp:set(a, 123, AddReq),
    ok = ecapnp:set(b, 321, AddReq),
    AddPromise = ecapnp:send(AddReq),
    %% get data on a promise will wait until fulfilled, then proceed
    ?assertEqual(444, ecapnp:get(result, AddPromise)).

test_remote_basic(#test{ vat = Vat }) ->
    Promise = ecapnp:import_capability(Vat, {text, <<"basic">>}, test_capnp:'BasicCap'()),
    test_send(#test{ basic=Promise }).

-endif.
