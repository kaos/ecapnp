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

request(MethodName, #capability{ interfaces = Is }=Cap) ->
    {ok, Interface, Method} = ecapnp_schema:find_method_by_name(
                                MethodName, Is),
    {ok, Params} = ecapnp_set:root(
                     ecapnp_schema:lookup(Method#method.paramType, Interface)),
    #rpc_call{
       target = Cap,
       interface = Interface,
       method = Method,
       params = Params }.

send(#rpc_call{ target = #capability{ id = {remote, _} }}=Req) ->
    ecapnp_vat:send(Req);
send(#rpc_call{
        target = #capability{ id = {local, Cap} },
        interface = #schema_node{ id = IntfId },
        method = #method{ id = MethId },
        params = Params
       }) ->
    Pid = self(),
    {ok, spawn_link(
           fun () ->
                   Result = ecapnp_capability:dispatch_call(
                              Cap, IntfId, MethId, Params),
                   Pid ! {promise_result, self(), Result}
           end)}.

wait(Promise) ->
    receive
        {promise_result, Promise, Result} ->
            Result
    end.


%% ===================================================================
%% internal functions
%% ===================================================================

