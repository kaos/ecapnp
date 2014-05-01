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

%% @copyright 2014, Andreas Stenius
%% @author Andreas Stenius <kaos@astekk.se>
%% @doc VAT server module.
%%
%% One VAT server per connection.

-module(ecapnp_vat).
-author("Andreas Stenius <kaos@astekk.se>").
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/1, export_capability/3,
         find_capability/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ecapnp.hrl").

-record(state, {
          owner,
          next_id = 0,
          requests = [],
          answers = [],
          imports = [],
          exports = []
         }).


%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    gen_server:start(?MODULE, self(), []).

start_link() ->
    gen_server:start_link(?MODULE, self(), []).

stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

export_capability(Name, Cap, Vat) ->
    gen_server:call(Vat, {export, Name, Cap}).

find_capability(Ref, Vat) ->
    gen_server:call(Vat, {find, Ref}).


%% ===================================================================
%% gen server callbacks
%% ===================================================================

init(Owner) ->
    {ok, #state{ owner = Owner }}.

handle_call({export, Name, Cap}, _From, State) ->
    {Id, State1} = export(Name, Cap, State),
    {reply, {ok, Id}, State1};
handle_call({find, Ref}, _From, State) ->
    Reply = case find(Ref, State) of
                false -> {unknown_capability, Ref};
                Found -> {ok, Found}
            end,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% internal functions
%% ===================================================================

%% ===================================================================
export(Name, Cap, #state{ next_id = Id }=State) ->
    case find(Cap, State) of
        {NId, Cap} ->
            {NId, export(Cap, 3, {NId, Name, Cap}, State)};
        false ->
            {Id, export(Id, 1, {Id, Name, Cap},
                        State#state{ next_id = Id + 1 }
                       )}
    end.

export(Key, Pos, Tuple, #state{ exports = Es }=State) ->
    State#state{ exports = lists:keystore(Key, Pos, Es, Tuple) }.

%% ===================================================================
find(Id, State) when is_integer(Id) ->
    find(Id, 1, 3, State);
find(Name, State) when is_binary(Name) ->
    find(Name, 2, 3, State);
find(Cap, State) when is_pid(Cap) ->
    find(Cap, 3, 1, State).

find(Ref, KeyIdx, ResIdx, #state{ exports = Es }) ->
    case lists:keyfind(Ref, KeyIdx, Es) of
        false -> false;
        Tuple -> {exported, element(ResIdx, Tuple)}
    end.

%% ===================================================================


%% ===================================================================
%% utils
%% ===================================================================
