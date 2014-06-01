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
%%%-------------------------------------------------------------------
%% @copyright 2014, Andreas Stenius
%% @author Andreas Stenius <kaos@astekk.se>
%% @doc Promise support.
%%
%% Everything promise.
%%%-------------------------------------------------------------------
-module(ecapnp_promise).
-author("Andreas Stenius <kaos@astekk.se>").
-behaviour(gen_fsm).

%% API
-export([start_link/1, stop/1, wait/2, notify/3, fullfill/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% gen_fsm state callbacks
-export([
         pending/2, pending/3,
         fullfilled/2, fullfilled/3
        ]).

-record(state, {
          waiting = [],
          result
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link(list()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
stop(Promise) ->
    gen_fsm:sync_send_all_state_event(Promise, stop).

%%--------------------------------------------------------------------
wait(Promise, Time) ->
    gen_fsm:sync_send_event(Promise, wait, Time).

%%--------------------------------------------------------------------
notify(Promise, Pid, Tag) ->
    gen_fsm:send_event(Promise, {notify, Pid, Tag}).

%%--------------------------------------------------------------------
fullfill(Promise, Result) ->
    gen_fsm:send_event(Promise, {fullfill, Result}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(Opts) ->
    State = init_state(Opts, #state{}),
    {ok, pending, State}.

init_state([], State) -> State;
init_state([{monitor, Pid}|Opts], State) ->
    monitor(process, Pid),
    init_state(Opts, State);
init_state([{fullfiller, F}|Opts], State) ->
    State1 = init_fullfiller(F, State),
    init_state(Opts, State1).

init_fullfiller(F, State) when is_function(F, 0) ->
    Promise = self(),
    spawn_link(fun() -> fullfill(Promise, F()) end),
    State.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
pending({notify, Pid, Tag}, State) ->
    {next_state, pending, enlist({message_to, Pid, Tag}, State)};
pending({fullfill, Result}, State) ->
    {next_state, fullfilled, set_result({ok, Result}, State)}.

%%--------------------------------------------------------------------
fullfilled({notify, Pid, Tag}, State) ->
    Pid ! {Tag, State#state.result},
    {next_state, fullfilled, State};
fullfilled({fullfill, _}, State) ->
    {stop, already_fullfilled, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
pending(wait, From, State) ->
    {next_state, pending, enlist({reply_to, From}, State)}.

%%--------------------------------------------------------------------
fullfilled(wait, _From, State) ->
    {reply, State#state.result, fullfilled, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _From, pending, State) ->
    {stop, canceled, ok, State};
handle_sync_event(stop, _From, fullfilled, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, _Pid, Info}, pending, State) ->
    {stop, Info, State};
handle_info({'DOWN', _Ref, process, _Pid, _Info}, fullfilled, State) ->
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, pending, State) ->
    _ = set_result({error, Reason}, State),
    ok;
terminate(_Reason, fullfilled, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
enlist(Tag, #state{ waiting = Ws }=State) ->
    State#state{ waiting = [Tag|Ws] }.

%%--------------------------------------------------------------------
set_result(Result, #state{ waiting = Ws }=State) ->
    [case W of
         {reply_to, From} ->
             gen_fsm:reply(From, Result);
         {message_to, Pid, Tag} ->
             Pid ! {Tag, Result}
     end || W <- Ws],
    State#state{ waiting = [], result = Result }.
