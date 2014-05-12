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

%% @copyright 2013, Andreas Stenius
%% @author Andreas Stenius <kaos@astekk.se>
%% @doc Capability support.
%%
%% Everything capability.

-module(ecapnp_capability).
-author("Andreas Stenius <kaos@astekk.se>").
-behaviour(gen_server).

-export([start/2, start_link/2, stop/1, dispatch_call/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ecapnp.hrl").

-record(state, { impl, interfaces = [] }).


%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts server for a capability described by the schema interface node.
%%
%% @spec start(schema_node(), list()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Impl, Interfaces) ->
    gen_server:start(?MODULE, [Impl, Interfaces], []).

start_link(Impl, Interfaces) ->
    gen_server:start_link(?MODULE, [Impl, Interfaces], []).

stop(Cap) ->
    gen_server:call(Cap, stop).

dispatch_call(Cap, ItfID, MethID, Params) ->
    gen_server:call(Cap, {call, ItfID, MethID, Params}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Mod, Interfaces]) ->
    {ok, #state{ impl = Mod, interfaces = list_interfaces(Interfaces) }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call, Intf, Meth, Params}, _From, State) ->
    case find_interface(Intf, State) of
        false -> {reply, {error, {unsupported_interface, Intf}}, State};
        Interface ->
            case find_method(Meth, Interface) of
                false -> {reply, {error, {unknown_method, Intf, Meth}}, State};
                Method ->
                    dispatch(Interface, Method, Params, State)
            end
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

dispatch(#schema_node{ name = InterfaceName }=Interface,
         #method{ name = MethodName, resultType = ResultType },
         Params, #state{ impl = Mod }=State) ->
    {ok, Results} = ecapnp_set:root(
                      ecapnp_schema:get(ResultType, Interface)),
    ok = apply(Mod, handle_call, [InterfaceName, MethodName, Params, Results]),
    {reply, {ok, Results}, State}.

find_interface(IntfId, #state{ interfaces = Ns }) ->
    lists:keyfind(IntfId, #schema_node.id, Ns).

find_method(MethId, #schema_node{ kind = #interface{ methods = Ms } }) ->
    lists:keyfind(MethId, #method.id, Ms).

list_interfaces(Interfaces) ->
    list_interfaces(Interfaces, []).

list_interfaces([], Acc) -> Acc;
list_interfaces([N|Ns], Acc) ->
    #schema_node{ kind = #interface{ extends = Es } } = N,
    list_interfaces(
      Ns, list_interfaces(
            [ecapnp_schema:get(E, N) || E <- Es],
            [N|Acc])).
