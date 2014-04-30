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

-export([request/3, set_param/3, send/1, wait/1]).

-include("ecapnp.hrl").

 
%% ===================================================================
%% API functions
%% ===================================================================

request(MethodName, Capability, Vat) ->
    {ok, Interface, Method} = ecapnp_capability:find_method_by_name(
                                MethodName, Capability),
    ecapnp_vat:request(Interface, Method, Capability, Vat).

set_param(Field, Value, #rpc_call{ params = Params }) ->
    ecapnp_set:field(Field, Value, Params).

send(Req) ->
    ecapnp_vat:send(Req).

wait(Promise) ->
    receive
        {Promise, Result} ->
            {ok, Result}
    end.


%% ===================================================================
%% internal functions
%% ===================================================================

