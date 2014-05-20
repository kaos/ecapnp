-module('calculator-client').

-export([run/0, send/2]).

send({Pid, Sock}, Data) ->
    Pid ! {send, {Sock, Data}}, ok.

run() ->
    {ok, Calculator} = connect(),
    [F(Calculator)
     || F <- [fun eval_literal/1, fun add_and_subtract/1, fun pipelining/1]].

connect() ->
    connect({"localhost", 55000}).

connect({Addr, Port}) ->
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
    %% {ok, Vat} = ecapnp_vat:start_link({gen_tcp, Socket}),
    {ok, Vat} = ecapnp_vat:start_link({?MODULE, {spawn(fun send_socket/0), Socket}}),
    spawn_link(fun () -> read_socket(Socket, Vat) end),
    ecapnp_vat:import_capability(
      {text, <<"calculator">>},
      calculator_capnp:'Calculator'(),
      Vat).

send_socket() ->
    send_socket(5).

send_socket(Delay) ->
    receive
        {send, {Sock, Data}} ->
            receive
            after Delay ->
                    gen_tcp:send(Sock, Data)
            end,
            send_socket(Delay)
    end.

read_socket(Sock, Vat) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Vat ! {receive_message, Data},
            read_socket(Sock, Vat);
        {error, closed} ->
            io:format("tcp socket closed~n"),
            halt(0);
        {error, Reason} ->
            io:format("tcp socket error: ~p~n", [Reason]),
            halt(1)
    end.

eval_literal(C) ->
    io:format("Evaluating a literal... "),
    Req = ecapnp:request(evaluate, C),
    Expression = ecapnp:init(expression, Req),
    ok = ecapnp:set({literal, 123}, Expression),
    {ok, EvalPromise} = ecapnp:send(Req),
    Read = ecapnp:request(read, ecapnp:get(value, EvalPromise)),
    {ok, ReadPromise} = ecapnp:send(Read),
    {ok, Response} = ecapnp:wait(ReadPromise),
    case ecapnp:get(value, Response) of
        123.0 ->
            io:format("PASS~n");
        Other ->
            io:format("Error: ~p~n", [Other])
    end.

add_and_subtract(C) ->
    io:format("Using add and subtract... "),
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
            io:format("PASS~n");
        Other ->
            io:format("Error: ~p~n", [Other])
    end.

pipelining(C) ->
    io:format("Pipelining eval() calls... "),
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
                    io:format("PASS~n");
                Other ->
                    io:format("Add 5 value: ~p~n", [Other])
            end;
        Other ->
            io:format("Add 3 value: ~p~n", [Other])
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
