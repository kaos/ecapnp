%%%-------------------------------------------------------------------
%%% @author Andreas Stenius <andreas.stenius@astekk.se>
%%% @copyright (C) 2014, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created : 31 May 2014 by Andreas Stenius <andreas.stenius@astekk.se>
%%%-------------------------------------------------------------------
-module(ecapnp_promise_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_promise/0, start_promise/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
start_promise() ->
    start_promise([]).

start_promise(Opts) ->
    supervisor:start_child(?SERVER, [Opts]).


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

    AChild = {promise, {ecapnp_promise, start_link, []},
              Restart, Shutdown, Type, [ecapnp_promise]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
