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
%% @doc Calculator server sample application.
%%
%% The operations are based on the calculator server sample from the
%% capnproto distribution.

-module('calculator-server').

-export([run/0, init/1, handle_call/5]).

run() ->
    ecapnp_capability_sup:start_link(),
    CapRestorer = fun (ObjectId, Vat) ->
                          case ecapnp_obj:to_text(ObjectId) of
                              <<"calculator">> ->
                                  ecapnp_capability_sup:start_capability(
                                    ?MODULE, calculator_capnp:'Calculator'(),
                                    [{monitor, Vat}])
                          end
                  end,
    listen({localhost, 55000}, CapRestorer).

listen({_Addr, Port}, CapRestorer) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept(Socket, CapRestorer).

accept(Socket, CapRestorer) ->
    spawn(
      fun () ->
              case gen_tcp:accept(Socket) of
                  {ok, Client} ->
                      accept(Socket, CapRestorer),
                      {ok, Vat} = ecapnp_vat:start_link({gen_tcp, Client}, CapRestorer),
                      read_socket(Client, Vat);
                  {error, closed} ->
                      io:format("~ntcp server socket closed~n");
                  {error, Reason} ->
                      io:format("~ntcp server socket error: ~p~n", [Reason]),
                      halt(1)
              end
      end).

read_socket(Sock, Vat) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Vat ! {receive_message, Data},
            read_socket(Sock, Vat);
        {error, closed} ->
            io:format("~ntcp socket closed~n");
        {error, Reason} ->
            io:format("~ntcp socket error: ~p~n", [Reason]),
            halt(1)
    end.


%% Capability callbacks

init(State) -> State.

handle_call('Calculator', 'evaluate', Params, Results, State) ->
    Expr = ecapnp:get(expression, Params),
    Value = evaluate(Expr),
    {ecapnp:set(value, value(Value), Results), State};
handle_call(['Calculator', 'Value'], 'read', _Params, Results, Value) ->
    {ecapnp:set(value, Value, Results), Value}.


%% Cap utils

evaluate(Expr) ->
    case ecapnp:get(Expr) of
        {literal, Literal} -> Literal
    end.

value(Value) ->
    {ok, Cap} = ecapnp_capability_sup:start_capability(
                  ?MODULE, calculator_capnp:schema(['Calculator', 'Value']),
                  [{monitor, self()}, {init, Value}]),
    Cap.
