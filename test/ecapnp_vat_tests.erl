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

-module(ecapnp_vat_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("include/ecapnp.hrl").
-import(ecapnp_test_utils, [meck/3]).

export_capability_test() ->
    {ok, Vat} = ecapnp_vat:start_link(),
    Cap = #capability{},
    {ok, ExportId} = ecapnp_vat:export_capability(Cap, Vat),
    ?assertEqual(0, ExportId),
    ?assertEqual({ok, Cap}, ecapnp_vat:find_capability(ExportId, Vat)).

restore_capability_test_() ->
    meck(echo, transport_funs(),
         [fun test_restore_capability/0
          %%,fun test_process_restore_message/0
         ]).

transport_funs() ->
    [{send, fun (Tester, Data) -> Tester ! {captured, Data}, ok end}].

test_restore_capability() ->
    {ok, Vat} = ecapnp_vat:start_link({echo, self()}),
    {ok, _Promise} = ecapnp_vat:import_capability(<<"test-cap">>, Vat),
    {ok, Req} = receive {captured, Data} -> {ok, Data} after 10 -> missing_data end,
    {ok, Msg} = ecapnp_get:root('Message', rpc_capnp, ecapnp_message:read(Req)),
    {restore, Res} = ecapnp:get(Msg),
    Obj = ecapnp_obj:to_text(ecapnp:get(objectId, Res)),
    ?assertEqual(<<"test-cap">>, Obj).

%% test_process_restore_message() ->
%%     {ok, Vat} = ecapnp_vat:start_link({echo, self()}),
%%     {ok, ExportId} = ecapnp_capability:start_link(basicCap, test_capnp:'BasicCap'()),
%%     {ok, ReqMsg} = ecapnp:set_root('Message', rpc_capnp),
%%     Res = ecapnp_obj:init(ecapnp:set(restore, ReqMsg)),
%%     ok = ecapnp:set(objectId, {text, <<"test-cap">>}, Res),
%%     Vat ! {message, ecapnp_message:write(ReqMsg)},
%%     {ok, Rsp} = receive {captured, Data} -> {ok, Data} after 10 -> missing_data end,
%%     {ok, RspMsg} = ecapnp_get:root('Message', rpc_capnp, ecapnp_message:read(Rsp)),
%%     {return, Ret} = ecapnp:get(RspMsg),
%%     {results, Payload} = ecapnp:get(Ret),
%%     _Content = ecapnp:get(content, Payload),
%%     [Cap] = ecapnp:get(capTable, Payload),
%%     ?assertEqual({senderHosted, ExportId}, ecapnp:get(Cap)).
%% sturdy_capability_test() ->
%%     ecapnp_vat:register_capability("Basic", basicCap, test_capnp:'BasicCap'()).


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
%%     %%check_request('BasicCap', add, Request),
%%     Params = ecapnp_capability:params(Request),
%%     ok = ecapnp:set(a, 123, Params),
%%     ok = ecapnp:set(b, 456, Params),
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


-endif.
