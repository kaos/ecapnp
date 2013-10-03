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

-export([type_of/1, lookup/2, size_of/1, size_of/2,
         data_size/1, ptrs_size/1]).

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
  when Type == object;
       is_record(Type, struct);
       is_record(Type, enum) ->
    {ok, Type};

lookup(Type, Types)
  when is_atom(Type),
       is_list(Types) ->
    keyfind(Type, #node.name, Types);
lookup(Type, Types)
  when is_integer(Type),
       is_list(Types) ->
    keyfind(Type, #node.id, Types);

lookup(Type, Data) when is_pid(Data) ->
    case ecapnp_data:get_type(Type, Data) of
        false -> lookup(Type, null);
        T -> {ok, T}
    end;

lookup(Type, #object{ ref=Ref }) ->
    lookup(Type, Ref);
lookup(Type, #ref{ data=Pid }) ->
    case ecapnp_data:get_type(Type, Pid) of
        false -> lookup(Type, null);
        T -> {ok, T}
    end;

lookup(Type, #schema{ types=Ts }) ->
    case lookup(Type, Ts) of
        false -> lookup(Type, null);
        T -> {ok, T}
    end;
lookup(Type, #struct{ types=Ts }) ->
    case lookup(Type, Ts) of
        false -> false;
        T -> {ok, T}
    end;
lookup(Type, #enum{ types=Ts }) ->
    case lookup(Type, Ts) of
        false -> false;
        T -> {ok, T}
    end;
lookup(Type, null) ->
    {unknown_type, Type}.

type_of(#object{ type=#node{ id=Type }}=Obj) ->
    lookup(Type, Obj).

size_of(Type, Store) ->    
    {ok, T} = lookup(Type, Store),
    size_of(T).

size_of(#struct{ dsize=DSize, psize=PSize }) -> DSize + PSize.

data_size(#struct{ dsize=Size }) -> Size.
ptrs_size(#struct{ psize=Size }) -> Size.


%% ===================================================================
%% internal functions
%% ===================================================================

keyfind(_Key, _N, []) -> false;
keyfind(Key, N, [T|Ts]) ->
    if element(N, element(#schema.node, T)) == Key -> T;
       true -> keyfind(Key, N, Ts)
    end.
