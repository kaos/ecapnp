%%%-------------------------------------------------------------------
%%% @author Andreas Stenius <andreas.stenius@astekk.se>
%%% @copyright (C) 2014, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2014 by Andreas Stenius <andreas.stenius@astekk.se>
%%%-------------------------------------------------------------------
-module(ecapnp_capability_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_capability/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("ecapnp.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
start_capability(Module, Interfaces) when is_list(Interfaces) ->
    case supervisor:start_child(?SERVER, [Module, Interfaces]) of
        {ok, Pid} ->
            Kind = #interface_ref{
                      cap = #capability{ id = {local, Pid} }
                     },
            {ok, #object{ ref = #ref{ kind = Kind },
                          schema = Interfaces
                        }};
        Err -> Err
    end;
start_capability(Module, Interface) ->
    start_capability(Module, [Interface]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {capability, {ecapnp_capability, start_link, []},
              Restart, Shutdown, Type, [ecapnp_capability]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
