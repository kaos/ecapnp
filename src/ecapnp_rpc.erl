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

-export([request/2, send/2, wait/2, promise/2]).

-include("ecapnp.hrl").

 
%% ===================================================================
%% API functions
%% ===================================================================

request(MethodName, Target) ->
    do_request(MethodName, Target).

send(#rpc_call{ interface = IntfId, method = MethId,
                params = Params, results = Results,
                resultSchema = ResultSchema
              },
     #capability{ id = {local, Cap} }) ->
    %% there's a race here, in case the process calling send/1 exits before the promise has started,
    %% then the add_ref/2 call may fail.
    {ok, promise(
           fun () ->
                   ok = ecapnp_obj:add_ref(self(), Params),
                   ecapnp_capability:dispatch_call(
                     Cap, IntfId, MethId, Params, Results)
           end,
           ResultSchema)
    }.

wait({local, Pid}, Timeout) ->
    Pid ! {get_promise_result, self()},
    receive
        {Pid, promise_result, Result} ->
            Result
    after
        Timeout ->
            timeout
    end.

promise(Fun, Schema) ->
    Parent = self(),
    Promise = spawn_link(
                fun () ->
                        Ref = monitor(process, Parent),
                        fulfilled(Ref, Fun())
                end),
    Kind = #interface_ref{
              cap = #promise{ id = {local, Promise} }
             },
    #object{ ref = #ref{ kind = Kind },
             schema = Schema }.


%% ===================================================================
%% internal functions
%% ===================================================================

do_request(MethodName, #object{ schema = Schema }=Target) ->
    {ok, Interface, Method} = ecapnp_schema:find_method_by_name(
                                MethodName, Schema),
    {ok, Msg} = ecapnp_set:root('Message', rpc_capnp),
    Call = ecapnp:init(call, Msg),
    Payload = ecapnp:init(params, Call),
    Content = ecapnp:init(
                content, ecapnp_schema:lookup(
                           Method#method.paramType, Interface),
                Payload),
    ResultSchema = ecapnp_schema:get(Method#method.resultType, Interface),

    #rpc_call{ owner = self(),
               target = Target,
               interface = Interface#schema_node.id,
               method = Method#method.id,
               params = Content,
               resultSchema = ResultSchema
             }.

fulfilled(Ref, Result) ->
    receive
        {get_promise_result, From} ->
            NewRef = monitor(process, From),
            true = demonitor(Ref, [flush]),
            case Result of
                {ok, Res} ->
                    ok = ecapnp_obj:add_ref(From, Res);
                _ -> nop
            end,
            From ! {self(), promise_result, Result},
            fulfilled(NewRef, Result);
        {'DOWN', Ref, process, _Pid, _Info} ->
            exit(normal)
    end.
