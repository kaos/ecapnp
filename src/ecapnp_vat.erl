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

-export([start/0, start_link/0, start_link/1, stop/1,
         import_capability/2, export_capability/3,
         find_capability/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ecapnp.hrl").

-record(questions, {
          next_id = 0 :: non_neg_integer(),
          promises = [] :: list({non_neg_integer(), pid()})
         }).

-record(exports, {
          next_id = 0 :: non_neg_integer(),
          caps = [] :: list({non_neg_integer(), binary(), pid()})
         }).

-record(state, {
          owner,
          transport,
          questions = #questions{} :: #questions{},
          answers = [],
          imports = [] :: list(),
          exports = #exports{} :: #exports{}
         }).


%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    gen_server:start(?MODULE, setup_state(), []).

start_link() ->
    gen_server:start_link(?MODULE, setup_state(), []).

start_link(Transport) ->
    gen_server:start_link(?MODULE, setup_state(Transport), []).

stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

import_capability(ObjectId, Vat) ->
    gen_server:call(Vat, {import, ObjectId}, infinity).

export_capability(ObjectId, Cap, Vat) ->
    gen_server:call(Vat, {export, ObjectId, Cap}).

find_capability(Ref, Vat) ->
    gen_server:call(Vat, {find, Ref}).


%% ===================================================================
%% gen server callbacks
%% ===================================================================

init(State) ->
    {ok, State}.

handle_call({import, ObjectId}, _From, State) ->
    {Reply, State1} = import(ObjectId, State),
    {reply, Reply, State1};
handle_call({export, ObjectId, Cap}, _From, State) ->
    {Id, State1} = export(ObjectId, Cap, State),
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
import(ObjectId, State) when is_binary(ObjectId) ->
    {Id, Promise, State1} = new_question(State),
    {ok, Msg} = ecapnp:set_root('Message', rpc_capnp),
    Res = ecapnp_obj:init(ecapnp:set(restore, Msg)),
    ok = ecapnp:set(questionId, Id, Res),
    ok = ecapnp:set(objectId, {text, ObjectId}, Res),
    ok = send(Msg, State1),
    {{ok, Promise}, State1}.

%% ===================================================================
export(ObjectId, Cap, State) ->
    case find(Cap, State) of
        {Id, Cap} ->
            %% replace existing cap, with updated object id
            {Id, do_export(Cap, {Id, ObjectId, Cap}, State)};
        false ->
            %% export new cap
            {Id, State1} = get_next_export_id(State),
            {Id, do_export(Id, {Id, ObjectId, Cap}, State1)}
    end.

do_export(Key, Entry, #state{ exports = #exports{ caps = Cs }=Es }=State) ->
    State#state{
      exports = Es#exports{
                  caps = lists:keystore(Key, capt_key_pos(Key), Cs, Entry)
                 }
     }.

%% ===================================================================
find(Key, #state{ exports = #exports{ caps = Cs } }) ->
    case lists:keyfind(Key, capt_key_pos(Key), Cs) of
        false -> false;
        Entry -> {exported, Entry}
    end.

%% ===================================================================


%% ===================================================================
%% utils
%% ===================================================================

%% ===================================================================
setup_state() ->
    #state{ owner = self() }.

setup_state(Transport) ->
    (setup_state())#state{ transport = Transport }.

%% ===================================================================
%% CapTable key pos for entries in the export table: [{Id, ObjectId, Cap}...]
capt_key_pos(Key) when is_integer(Key) -> 1;
capt_key_pos(Key) when is_pid(Key) -> 3;
capt_key_pos(_) -> 2.

%% ===================================================================
send(Msg, #state{ transport = {Mod, Handle} }) ->
    Mod:send(Handle, ecapnp_message:write(Msg)).

%% ===================================================================
get_next_export_id(#state{ exports = #exports{
                                        next_id = Id
                                       }=Es }=State) ->
    {Id, State#state{ exports = Es#exports{
                                  next_id = Id + 1
                                 }}}.

%% ===================================================================
new_question(#state{ questions = #questions{
                                    next_id = Id,
                                    promises = Ps
                                   }=Qs }=State) ->
    Promise = new_promise(),
    {Id, Promise, State#state{
                    questions = Qs#questions{
                                  next_id = Id + 1,
                                  promises = [Promise|Ps] }}
    }.

%% ===================================================================
new_promise() ->
    spawn_link(
      fun () ->
              receive done -> ok end
      end).

%% ===================================================================
