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

-export([request/2, send/1, wait/1]).

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
                interface = #schema_node{ id = IntfId }=Node,
                method = #method{ id = MethId, resultType = ResultType },
                params = Params
              }) ->
    Pid = self(),
    {ok, #object{
            ref=#promise{
                   id = {local, spawn_link(
                                  fun () ->
                                          Result = ecapnp_capability:dispatch_call(
                                                     Cap, IntfId, MethId, Params),
                                          Pid ! {promise_result, self(), Result}
                                  end)}},
            schema = ecapnp_schema:get(ResultType, Node)
           }};
send(#rpc_call{ target = #promise{ id = {local, _} }=Promise }=Req) ->
    {ok, #object{
            ref = #ref{
                     kind = #interface_ref{
                               cap = Cap }}}
    } = wait(Promise),
    send(Req#rpc_call{ target = Cap }).

wait(#object{ ref = #promise{}=Promise }) ->
    wait(Promise);
wait(#promise{ id = {local, Pid}, transform = Ts }) ->
    receive
        {promise_result, Pid, {ok, Result}} ->
            {ok, lists:foldr(
                   fun ({ptr, Idx}, Obj) ->
                           ecapnp:get(Idx, Obj)
                   end,
                   Result, Ts)};
        {promise_result, Pid, Err} -> Err
    end.


%% ===================================================================
%% internal functions
%% ===================================================================

do_request(MethodName, Interfaces, Target) ->
    {ok, Interface, Method} = ecapnp_schema:find_method_by_name(
                                MethodName, Interfaces),
    {ok, Params} = ecapnp_set:root(
                     ecapnp_schema:lookup(Method#method.paramType, Interface)),
    #rpc_call{ target = Target, interface = Interface,
               method = Method, params = Params }.
