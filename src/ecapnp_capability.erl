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

-export([start/2, start_link/2, stop/1]).
-export([dispatch_call/5]).
%%, data_pid/1, pid/1, pid/2,
%% request/2, send/1, wait/1, param/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ecapnp.hrl").

-record(state, { impl, methods }).


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
start(Impl, Schema) ->
    gen_server:start(?MODULE, [Impl, Schema], []).

start_link(Impl, Schema) ->
    gen_server:start_link(?MODULE, [Impl, Schema], []).

stop(Cap) ->
    gen_server:call(Cap, stop).

dispatch_call(Cap, ItfID, MethID, Params, Result) ->
    gen_server:call(Cap, {call, {ItfID, MethID}, [Params, Result]}).

%% request(Method, Object) ->
%%     #method{ paramType=Type } = ecapnp_obj:field(Method, Object),
%%     {ok, Param} = ecapnp_set:root(Type, data_pid(Object)),
%%     {ok, #request{
%%             method=Method,
%%             param=Param,
%%             interface=Object
%%            }}.

%% send(#request{ method=Method, param=Param, interface=Object }=Request) ->
%%     #method{ resultType=Type } = ecapnp_obj:field(Method, Object),
%%     {ok, Result} = ecapnp_set:root(Type, data_pid(Object)),
%%     Worker = case pid(Object) of
%%                  undefined -> spawn_link(
%%                                 fun() ->
%%                                         try_call_later(Request, Result)
%%                                 end);
%%                  Pid when is_pid(Pid) ->
%%                      gen_server:call(Pid, {call, Method, Param, Result})
%%              end,
%%     ok = ecapnp_data:promise(Worker, data_pid(Result)),
%%     {ok, Result}.

%% wait(Pid) when is_pid(Pid) ->
%%     Ref = monitor(process, Pid),
%%     receive
%%         {'DOWN', Ref, process, _, Exit}
%%           when Exit == normal; Exit == noproc -> ok;
%%         {'DOWN', Ref, process, _, Error} -> {error, Error}
%%     after 5000 ->
%%             demonitor(Ref, [flush]), timeout
%%     end;
%% wait(Object) when is_record(Object, object) ->
%%     Promise = ecapnp_data:promise(data_pid(Object)),
%%     wait(Promise).

%% param(#request{ param=Object }) -> Object.

%% %% belongs in ecapnp_obj, and/or ecapnp_ref
%% data_pid(#object{ ref=#ref{ data=Pid }}) -> Pid.

%% pid(#object{ ref = #ref{ kind = #interface_ref{ pid = Pid } } }) -> Pid.

%% pid(Pid, #object{
%%             ref = #ref{
%%                      kind = #interface_ref{
%%                                id = undefined,
%%                                pid = undefined
%%                               },
%%                      data = Data
%%                     } = Ref
%%            } = Object) ->
%%     Object#object{
%%       ref = Ref#ref{
%%               kind = #interface_ref{
%%                         id = ecapnp_data:add_capability(Pid),
%%                         pid = Pid } }
%%      }.


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
init([Mod, Schema]) ->
    {ok, #state{ impl = Mod, methods = list_methods(Schema) }}.

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
handle_call({call, Method, Args}, _From, State) ->
    dispatch(find_method(Method, State), Args, State);
handle_call(stop, _From, State) -> {stop, normal, ok, State};
%% handle_call({call, Method, Param, Result}, {From,_}, #state{ mod=Impl }=State) ->
%%     Reply = spawn(
%%               fun() ->
%%                       link(From),
%%                       ok = apply(Impl, Method, [Param, Result])
%%               end),
%%     {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

dispatch(false, _, State) ->
    {reply, unknown_method, State};
dispatch({_, Fun}, Args, #state{ impl = Mod }=State) ->
    {reply, apply(Mod, Fun, Args), State}.

find_method(Method, #state{ methods=Methods }) ->
    lists:keyfind(Method, 1, Methods).

list_methods(#schema_node{ id = Id,
                           kind = #interface{
                                     extends = Extends, methods = Methods }
                         }=S) ->
    {_, Acc} = lists:mapfoldl(
                 fun (M, {Ord, Acc}) ->
                         {Ord + 1, [{{Id, Ord}, M#method.name}|Acc]}
                 end,
                 {0, lists:flatten(
                       [list_methods(ecapnp_schema:get(E, S))
                        || E <- Extends
                       ])},
                 Methods),
    Acc.

%% started(Pid, Intf, Schema) ->
%%     Cap = ecapnp_obj:alloc(
%%             Intf, 0,
%%             ecapnp_data:new({Schema, 10})),
%%     ok = pid(Pid, Cap),
%%     {ok, Cap}.

%% try_call_later(#request{ method=Method, param=Param,
%%                          interface=Promise }=Request,
%%                Result) ->
%%     ok = wait(Promise),
%%     case pid(Promise) of
%%         undefined -> throw({promise_not_fulfilled, Request});
%%         Pid when is_pid(Pid) ->
%%             Worker = gen_server:call(Pid, {call, Method, Param, Result}),
%%             ok = wait(Worker)
%%     end.
