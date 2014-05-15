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
%% @doc Everything rpc.
%%

-module(ecapnp_rpc).
-author("Andreas Stenius <kaos@astekk.se>").

-export([request/2, send/1, wait/2, promise/2]).

-include("ecapnp.hrl").

 
%% ===================================================================
%% API functions
%% ===================================================================

request(MethodName, #object{ ref = #ref{ kind = #interface_ref{ cap = Cap } } }) ->
    do_request(MethodName, Cap#capability.interfaces, Cap);
request(MethodName, #object{ ref = #promise{}=Cap, schema = #schema_node{ kind = #interface{}}=I }) ->
    do_request(MethodName, [I], Cap);
request(MethodName, #capability{ interfaces = Is }=Cap) ->
    do_request(MethodName, Is, Cap).

send(#rpc_call{ target = #capability{ id = {local, Cap} },
                interface = IntfId, method = MethId,
                params = Params, results = Results,
                resultSchema = ResultSchema
              }) ->
    {ok, promise(
           fun () ->
                   ecapnp_capability:dispatch_call(
                     Cap, IntfId, MethId, Params, Results)
           end,
           ResultSchema)
    };
send(#rpc_call{ target = #promise{ id = {local, _} }=Promise }=Req) ->
    {ok, #object{
            ref = #ref{
                     kind = #interface_ref{
                               cap = Cap }}}
    } = ecapnp:wait(Promise),
    send(Req#rpc_call{ target = Cap }).

wait({local, Pid}, Timeout) ->
    io:format("*DBG* ~p waiting on promise from ~p (~p)~n", [self(), Pid, Timeout]),
    Pid ! {get_promise_result, self()},
    receive
        {Pid, promise_result, Result} ->
            io:format("*DBG* ~p promise fulfilled: ~p~n", [self(), Result]),
            Result
    after
        Timeout ->
            io:format("*DBG* ~p promise timeout!!~n", [self()]),
            timeout
    end.

promise(Fun, Schema) ->
    Parent = self(),
    Promise = spawn_link(
                fun () ->
                        Ref = monitor(process, Parent),
                        fulfilled(Ref, Fun())
                end),
    #object{ ref = #promise{ id = {local, Promise} }, schema = Schema }.


%% ===================================================================
%% internal functions
%% ===================================================================

do_request(MethodName, Interfaces, Target) ->
    {ok, Interface, Method} = ecapnp_schema:find_method_by_name(
                                MethodName, Interfaces),
    {ok, Msg} = ecapnp_set:root('Message', rpc_capnp),
    Call = ecapnp:init(call, Msg),
    Payload = ecapnp:init(params, Call),
    Content = ecapnp:init(
                content, ecapnp_schema:lookup(
                           Method#method.paramType, Interface),
                Payload),
    ResultSchema = ecapnp_schema:get(Method#method.resultType, Interface),

    #rpc_call{ target = Target,
               interface = Interface#schema_node.id,
               method = Method#method.id,
               params = Content,
               resultSchema = ResultSchema
             }.

fulfilled(Ref, Result) ->
    receive
        {get_promise_result, From} ->
            NewRef = monitor(process, From),
            demonitor(Ref, [flush]),
            From ! {self(), promise_result, Result},
            fulfilled(NewRef, Result);
        {'DOWN', Ref, process, _Pid, _Info} ->
            exit(normal)
    end.
