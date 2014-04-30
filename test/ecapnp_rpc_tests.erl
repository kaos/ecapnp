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
         [fun() ->
                  Vat = {vat_mock, test},
                  {ok, Cap} = ecapnp_capability:start_link(basicCap, test_capnp:'BasicCap'()),
                  Req = ecapnp_rpc:request(add, Cap, Vat),
                  Params = Req#rpc_call.params,
                  ?assertEqual(
                     #rpc_call{
                        target = {importedCap, 123},
                        interface = (test_capnp:'BasicCap'())#schema_node.id,
                        method = 0,
                        params = Params
                       }, Req),
                  ?assertEqual(
                    ['BasicCap', [add, '$Params']],
                    Params#object.schema#schema_node.name)
          end
         ]).

vat_funs() ->
    [{find_capability,
      fun (Cap, {vat_mock, test}) when is_pid(Cap) -> {importedCap, 123} end}
    ].


-endif.
