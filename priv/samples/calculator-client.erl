-module('calculator-client').

-export([run/0]).

run() ->
    {ok, Calculator} = connect(),
    _ =
        [begin
             Ref = monitor(process, spawn(fun () -> exit(F(Calculator)) end)),
             receive
                 {'DOWN', Ref, process, _Pid, Info} -> {T, Info}
             end
         end || {T, F} <- [{eval_literal, fun eval_literal/1},
                           {add_and_subtract, fun add_and_subtract/1},
                           {pipelining, fun pipelining/1},
                           {def_functions, fun def_functions/1}
                          ]
        ],
    %% proper shutdown not yet implemented..
    %% give some time to let the last finish messages get out
    receive after 500 ->
                    io:format("~n~nAll done.~n"),
                    halt(0)
            end.

connect() ->
    connect({"localhost", 55000}).

connect({Addr, Port}) ->
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
    {ok, Vat} = ecapnp_vat:start_link({gen_tcp, Socket}),
    spawn_link(fun () -> read_socket(Socket, Vat) end),
    ecapnp_vat:import_capability(
      {text, <<"calculator">>},
      calculator_capnp:'Calculator'(),
      Vat).

read_socket(Sock, Vat) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            %% useful to check where/when ecapnp is waiting on response data
            %% erlang:send_after(2000, Vat, {receive_message, Data}),
            Vat ! {receive_message, Data},
            read_socket(Sock, Vat);
        {error, closed} ->
            io:format("tcp socket closed~n");
        {error, Reason} ->
            io:format("tcp socket error: ~p~n", [Reason]),
            halt(1)
    end.

eval_literal(C) ->
    io:format("~nEvaluating a literal... "),
    Req = ecapnp:request(evaluate, C),
    Expression = ecapnp:init(expression, Req),
    ok = ecapnp:set({literal, 123}, Expression),
    {ok, EvalPromise} = ecapnp:send(Req),
    Read = ecapnp:request(read, ecapnp:get(value, EvalPromise)),
    {ok, ReadPromise} = ecapnp:send(Read),
    {ok, Response} = ecapnp:wait(ReadPromise),
    case ecapnp:get(value, Response) of
        123.0 ->
            io:format("PASS");
        Other ->
            io:format("Error: ~p", [Other]),
            error
    end.

add_and_subtract(C0) ->
    io:format("~nUsing add and subtract... "),
    %% resolve the Calculator promise to use the imported cap, rather
    %% than the promise
    {ok, C} = ecapnp:wait(C0),

    Add = get_operator(add, C),
    Sub = get_operator(subtract, C),

    Req = ecapnp:request(evaluate, C),
    Expr = ecapnp:init(expression, Req),

    SubCall = ecapnp:set(call, Expr),
    ok = ecapnp:set(function, Sub, SubCall),
    [Sub1, Sub2] = ecapnp:set(params, 2, SubCall),
    ecapnp:set({literal, 67}, Sub2),

    AddCall = ecapnp:set(call, Sub1),
    ok = ecapnp:set(function, Add, AddCall),
    [Add1, Add2] = ecapnp:set(params, 2, AddCall),
    ok = ecapnp:set({literal, 123}, Add1),
    ok = ecapnp:set({literal, 45}, Add2),
    
    {ok, Promise} = ecapnp:send(Req),
    Read = ecapnp:request(read, ecapnp:get(value, Promise)),
    {ok, ReadPromise} = ecapnp:send(Read),
    {ok, Response} = ecapnp:wait(ReadPromise),
    case ecapnp:get(value, Response) of
        101.0 ->
            io:format("PASS");
        Other ->
            io:format("Error: ~p", [Other]),
            error
    end.

pipelining(C) ->
    io:format("~nPipelining eval() calls... "),
    Add = get_operator(add, C),
    Mul = get_operator(multiply, C),

    Req = ecapnp:request(evaluate, C),
    Expr = ecapnp:init(expression, Req),
    [Mul1, Mul2] = call_expression(Mul, 2, Expr),

    ok = ecapnp:set({literal, 4}, Mul1),
    ok = ecapnp:set({literal, 6}, Mul2),
    
    {ok, MulPromise} = ecapnp:send(Req),
    MulValue = ecapnp:get(value, MulPromise),
    
    Add3Req = ecapnp:request(evaluate, C),
    [Add3P1, Add3P2] = call_expression(
                         Add, 2, ecapnp:init(
                                   expression, Add3Req)),
    ok = ecapnp:set({previousResult, MulValue}, Add3P1),
    ok = ecapnp:set({literal, 3}, Add3P2),
    {ok, Add3Promise} = ecapnp:send(Add3Req),
    {ok, Add3Value} = ecapnp:send(
                        ecapnp:request(
                          read, ecapnp:get(value, Add3Promise))),

    Add5Req = ecapnp:request(evaluate, C),
    [Add5P1, Add5P2] = call_expression(
                         Add, 2, ecapnp:init(
                                   expression, Add5Req)),
    ok = ecapnp:set({previousResult, MulValue}, Add5P1),
    ok = ecapnp:set({literal, 5}, Add5P2),
    {ok, Add5Promise} = ecapnp:send(Add5Req),
    {ok, Add5Value} = ecapnp:send(
                        ecapnp:request(
                          read, ecapnp:get(value, Add5Promise))),

    case ecapnp:get(value, Add3Value) of
        27.0 ->
            case ecapnp:get(value, Add5Value) of
                29.0 ->
                    io:format("PASS");
                Other ->
                    io:format("Add 5 value: ~p", [Other]),
                    error
            end;
        Other ->
            io:format("Add 3 value: ~p", [Other]),
            error
    end.

def_functions(C0) ->
    %% The calculator interface supports defining functions.  Here we
    %% use it to define two functions and then make calls to them as
    %% follows:
    %%
    %%   f(x, y) = x * 100 + y
    %%   g(x) = f(x, x + 1) * 2;
    %%   f(12, 34)
    %%   g(21)
    %%
    %% Once again, the whole thing takes only one network round trip.

    io:format("~nDefining functions... "),

    %% this doesn't really wait, as the result is already there, but
    %% merely retreives the result from the local vat's question
    %% table.
    {ok, C} = ecapnp:wait(C0),

    %% get operators from server
    Add = get_operator(add, C),
    Mul = get_operator(multiply, C),

    %% define f
    F = (fun () ->
                 Req = ecapnp:request(defFunction, C),
                 ok = ecapnp:set(paramCount, 2, Req),
                 Body = ecapnp:init(body, Req),
                 [Add1, Add2] = call_expression(Add, 2, Body),
                 [Mul1, Mul2] = call_expression(Mul, 2, Add1),
                 ok = ecapnp:set({parameter, 0}, Mul1), %% x
                 ok = ecapnp:set({literal, 100}, Mul2), %% * 100
                 ok = ecapnp:set({parameter, 1}, Add2), %% + y
                 {ok, Promise} = ecapnp:send(Req),
                 ecapnp:get(func, Promise)
         end)(),
    %% define g
    G = (fun () ->
                 Req = ecapnp:request(defFunction, C),
                 ok = ecapnp:set(paramCount, 1, Req),
                 Body = ecapnp:init(body, Req),
                 [Mul1, Mul2] = call_expression(Mul, 2, Body),
                 [Call1, Call2] = call_expression(F, 2, Mul1),
                 [Add1, Add2] = call_expression(Add, 2, Call2),
                 ok = ecapnp:set({parameter, 0}, Call1), %% x
                 ok = ecapnp:set({parameter, 0}, Add1), %% x
                 ok = ecapnp:set({literal, 1}, Add2), %% + 1
                 ok = ecapnp:set({literal, 2}, Mul2), %% * 2
                 {ok, Promise} = ecapnp:send(Req),
                 ecapnp:get(func, Promise)
         end)(),
    %% f(12, 34)
    Freq = ecapnp:request(evaluate, C),
    Fexpr = ecapnp:init(expression, Freq),
    [F1, F2] = call_expression(F, 2, Fexpr),
    ok = ecapnp:set({literal, 12}, F1),
    ok = ecapnp:set({literal, 34}, F2),
    Fvalue = req_value(Freq),

    %% g(21)
    Greq = ecapnp:request(evaluate, C),
    Gexpr = ecapnp:init(expression, Greq),
    [G1] = call_expression(G, 1, Gexpr),
    ok = ecapnp:set({literal, 21}, G1),
    Gvalue = req_value(Greq),

    case ecapnp:get(value, Fvalue) of
        1234.0 ->
            case ecapnp:get(value, Gvalue) of
                4244.0 ->
                    io:format("PASS");
                Other ->
                    io:format("g(21): ~p", [Other]),
                    error
            end;
        Other ->
            io:format("f(12, 34): ~p", [Other]),
            error
    end.

get_operator(Op, C) ->
    Req = ecapnp:request(getOperator, C),
    ok = ecapnp:set(op, Op, Req),
    {ok, Promise} = ecapnp:send(Req),
    ecapnp:get(func, Promise).

call_expression(Fun, ParamCount, Expr) ->
    Call = ecapnp:set(call, Expr),
    ok = ecapnp:set(function, Fun, Call),
    ecapnp:set(params, ParamCount, Call).

req_value(Req) ->
    {ok, Promise} = ecapnp:send(Req),
    {ok, Value} = ecapnp:send(
                    ecapnp:request(
                      read, ecapnp:get(value, Promise)
                     )),
    Value.
