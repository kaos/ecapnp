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

export_capability_test() ->
    {ok, Vat} = ecapnp_vat:start_link(),
    {ok, Cap} = ecapnp_capability:start_link(basicCap, test_capnp:'BasicCap'()),
    {ok, ExportId} = ecapnp_vat:export_capability(<<"test-cap">>, Cap, Vat),
    ?assertEqual(0, ExportId),
    ?assertEqual({ok, {exported, Cap}}, ecapnp_vat:find_capability(ExportId, Vat)),
    ?assertEqual({ok, {exported, Cap}}, ecapnp_vat:find_capability(<<"test-cap">>, Vat)),
    ?assertEqual({ok, {exported, ExportId}}, ecapnp_vat:find_capability(Cap, Vat)).


-endif.
