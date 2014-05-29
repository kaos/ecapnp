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

-export([dbg/0, run/0, init/1, handle_call/5]).

-include_lib ("ecapnp/include/ecapnp.hrl").

dbg() ->
    io:format(
      "dbg: ~p~n",
      [[dbg:tracer(),
        dbg:p(new, [c]),
        %dbg:tpl(?MODULE, []),
        dbg:tpl(ecapnp_capability, [])
        %dbg:tp(ecapnp_rpc, []),
        %dbg:tpl(ecapnp_vat, []),
        %dbg:tp(ecapnp, [])
       ]]).

run() ->
    spawn(
      fun () ->
              ecapnp_capability_sup:start_link(),
              CapRestorer = fun (ObjectId, Vat) ->
                                    case ecapnp_obj:to_text(ObjectId) of
                                        <<"calculator">> ->
                                            ecapnp_capability_sup:start_capability(
                                              ?MODULE, calculator_capnp:'Calculator'(),
                                              [{monitor, Vat}])
                                    end
                            end,
              listen({localhost, 55000}, CapRestorer)
      end).

listen({_Addr, Port}, CapRestorer) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    accept(Socket, CapRestorer).

accept(Socket, CapRestorer) ->
    case gen_tcp:accept(Socket) of
        {ok, Client} ->
            spawn(fun () -> accept(Socket, CapRestorer) end),
            {ok, Vat} = ecapnp_vat:start_link({gen_tcp, Client}, CapRestorer),
            read_socket(Client, Vat),
            accept(Socket, CapRestorer);
        {error, closed} ->
            io:format("~ntcp server socket closed~n"),
            halt(0);
        {error, Reason} ->
            io:format("~ntcp server socket error: ~p~n", [Reason]),
            halt(1)
    end.

read_socket(Sock, Vat) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Vat ! {receive_message, Data},
            read_socket(Sock, Vat);
        {error, closed} ->
            io:format("~ntcp client socket closed~n");
        {error, Reason} ->
            io:format("~ntcp client socket error: ~p~n", [Reason])
    end.


%% Capability callbacks

%% since I'm lazy, all calculator capabilities call back to this
%% module, so it's kind of messy here..

init({defFunction, Params}) ->
    ParamCount = ecapnp:get(paramCount, Params),
    Body = ecapnp:get(body, Params),
    {def, ParamCount, Body};
init(State) -> State.

handle_call('Calculator', evaluate, Params, Results, State) ->
    Expr = ecapnp:get(expression, Params),
    Value = evaluate(Expr),
    {ecapnp:set(value, cap('Value', Value), Results), State};
handle_call('Calculator', getOperator, Params, Results, State) ->
    {ecapnp:set(func, cap('Function', {op, ecapnp:get(op, Params)}), Results), State};
handle_call(['Calculator', 'Value'], read, _Params, Results, Value) ->
    {ecapnp:set(value, ecapnp:wait(Value), Results), Value};
handle_call(['Calculator', 'Function'], call, Params, Results, {op, Operator}=State) ->
    [Op1, Op2] = ecapnp:get(params, Params),
    Value =
        case Operator of
            add -> Op1 + Op2;
            subtract -> Op1 - Op2;
            multiply -> Op1 * Op2;
            divide -> Op1 / Op2
        end,
    {ecapnp:set(value, Value, Results), State};
handle_call(['Calculator', 'Function'], call, Params, Results, {def, ParamCount, Body}=State) ->
    CallParams = ecapnp:get(params, Params),
    if length(CallParams) == ParamCount ->
            {ecapnp:set(value, ecapnp:wait(evaluate(Body, CallParams)), Results), State}
    end;
handle_call('Calculator', defFunction, Params, Results, State) ->
    {ecapnp:set(func, cap('Function', {defFunction, Params}), Results), State}.


%% Cap utils

evaluate(Expr) -> evaluate(Expr, []).
evaluate(Expr, EvalParams) ->
    ecapnp_rpc:promise(
      fun () ->
              case ecapnp:get(Expr) of
                  {literal, Literal} -> Literal;
                  {previousResult, Value} ->
                      ReadReq = ecapnp:request(read, Value),
                      {ok, ReadPromise} = ecapnp:send(ReadReq),
                      ecapnp:get(value, ReadPromise);
                  {parameter, Idx} ->
                      lists:nth(Idx + 1, EvalParams);
                  {call, Call} ->
                      Func = ecapnp:get(function, Call),
                      CallParams = [evaluate(E, EvalParams)
                                    || E <- ecapnp:get(params, Call)],
                      CallReq = ecapnp:request(call, Func),
                      ecapnp:set(params, [ecapnp:wait(P) || P <- CallParams], CallReq),
                      {ok, CallPromise} = ecapnp:send(CallReq),
                      ecapnp:get(value, CallPromise)
              end
      end, float64).

cap(Type, Init) ->
    {ok, Cap} = ecapnp_capability_sup:start_capability(
                  ?MODULE, calculator_capnp:schema(['Calculator', Type]),
                  [{monitor, self()}, {init, Init}]),
    Cap.
