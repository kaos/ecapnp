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
    {ok, Cap} = ecapnp_capability:start_link(basicCap, test_capnp:'BasicCap'()),
    {ok, ExportId} = ecapnp_vat:export_capability(<<"test-cap">>, Cap, Vat),
    Exported = {exported, {ExportId, <<"test-cap">>, Cap}},
    ?assertEqual(0, ExportId),
    ?assertEqual({ok, Exported}, ecapnp_vat:find_capability(ExportId, Vat)),
    ?assertEqual({ok, Exported}, ecapnp_vat:find_capability(<<"test-cap">>, Vat)),
    ?assertEqual({ok, Exported}, ecapnp_vat:find_capability(Cap, Vat)).

test_restore_capability() ->
    {ok, Vat} = ecapnp_vat:start_link({transport, self()}),
    {ok, _Promise} = ecapnp_vat:import_capability(<<"test-cap">>, Vat),
    {ok, Req} = receive {captured, Data} -> {ok, Data} after 10 -> missing_data end,
    {ok, Msg} = ecapnp_get:root('Message', rpc_capnp, ecapnp_message:read(Req)),
    {restore, Res} = ecapnp:get(Msg),
    Obj = ecapnp_obj:to_text(ecapnp:get(objectId, Res)),
    ?assertEqual(<<"test-cap">>, Obj).

restore_capability_test_() ->
    meck(transport, transport_funs(), [fun test_restore_capability/0]).

transport_funs() ->
    [{send, fun (Tester, Data) -> Tester ! {captured, Data}, ok end}].



-endif.
