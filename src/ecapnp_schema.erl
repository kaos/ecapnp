%%  
%%  Copyright 2013, Andreas Stenius <kaos@astekk.se>
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

-module(ecapnp_schema).
-author("Andreas Stenius <kaos@astekk.se>").

-export([lookup/2]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

%% Lookup type in schema
lookup({struct, Type}, Ts) ->
    lookup(Type, Ts);
lookup({list, Type}, Ts) ->
    lookup(Type, Ts);
lookup(Type, _)
  when is_record(Type, struct);
       is_record(Type, enum) ->
    {ok, Type};
lookup(Type, #schema{ types=Ts }) ->
    case proplists:get_value(Type, Ts) of
        undefined -> lookup(Type, undefined);
        T -> {ok, T}
    end;
lookup(Type, #struct{ types=Ts }) ->
    case proplists:get_value(Type, Ts) of
        undefined -> undefined;
        T -> {ok, T}
    end;
lookup(Type, #enum{ types=Ts }) ->
    case proplists:get_value(Type, Ts) of
        undefined -> undefined;
        T -> {ok, T}
    end;
lookup(Type, #object{ type=T }=Obj) ->
    case lookup(Type, T) of
        undefined -> lookup(Type, Obj#object.parent);
        Ok -> Ok
    end;
lookup(Type, undefined) ->
    {unknown_type, Type}.


%% ===================================================================
%% internal functions
%% ===================================================================

