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

-module(ecapnp_test_utils).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("include/ecapnp.hrl").
-export([data/1, meck/2, meck/3, setup_meck/2, teardown_meck/1]).

%% ----------------------------------------
meck_test_() ->
    meck(
      foo, [{bar, fun() -> baz end}],
      [?_assertEqual(baz, foo:bar())
      ]).

%% ----------------------------------------
data(Data) ->
    {ok, Pid} = ecapnp_data:start_link(Data),
    #builder{ pid = Pid }.

%% ----------------------------------------
meck(Mod, Funs, Tests) ->
    {setup, fun() -> setup_meck(Mod, Funs) end, fun teardown_meck/1, Tests}.

meck(Desc, Tests) ->
    {setup,
     fun() -> [setup_meck(Mod, Funs) || {Mod, Funs} <- Desc] end,
     fun(Mods) -> [teardown_meck(Mod) || Mod <- Mods] end,
     Tests}.

%% ----------------------------------------
setup_meck(Mod, Funs) ->
    ?assertEqual(ok, meck:new(Mod, [non_strict])),
    [?assertEqual(ok, meck:expect(Mod, Fun, Impl))
     || {Fun, Impl} <- Funs],
    Mod.

teardown_meck(Mod) ->
    ?assert(meck:validate(Mod)),
    ?assertEqual(ok, meck:unload(Mod)).

-endif.
