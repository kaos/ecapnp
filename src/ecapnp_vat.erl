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
-export([start/0, start_link/0, stop/1, export_capability/2,
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

export_capability(Cap, Vat) ->
    gen_server:call(Vat, {export, Cap}).

find_capability(Id, Vat) ->
    gen_server:call(Vat, {find, Id}).


%% ===================================================================
%% gen server callbacks
%% ===================================================================

init(Owner) ->
    {ok, #state{ owner = Owner }}.

handle_call({export, Cap}, _From, State) ->
    {Id, State1} = export(Cap, State),
    {reply, {ok, Id}, State1};
handle_call({find, Id}, _From, State) ->
    {reply, find(Id, State), State};
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
export(Cap, #state{ next_id = Id, exports = Es }=State) ->
    case lists:keyfind(Cap, 2, Es) of
        {EId, Cap} -> {EId, State};
        false ->
            {Id, State#state{
                   next_id = Id + 1,
                   exports = lists:keystore(Id, 1, Es, {Id, Cap})
                  }}
    end.

%% ===================================================================
find(Id, #state{ exports = Es }) ->
    case lists:keyfind(Id, 1, Es) of
        {Id, Cap} -> {exported, Cap};
        false -> {unknown_id, Id}
    end.

%% ===================================================================


%% ===================================================================
%% utils
%% ===================================================================

