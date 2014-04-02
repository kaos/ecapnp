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

%% @copyright 2013, Andreas Stenius
%% @author Andreas Stenius <kaos@astekk.se>
%% @doc Schema functions.
%%
%% This module exports functions for interacting with a compiled
%% schema.

-module(ecapnp_schema).
-author("Andreas Stenius <kaos@astekk.se>").

-export([type_of/1, get/2, lookup/2, lookup/3, size_of/1, size_of/2,
         data_size/1, ptrs_size/1, get_ref_kind/1, get_ref_kind/2,
         set_ref_to/2]).

-include("ecapnp.hrl").

-type lookup_type() :: type_id() | type_name() | object | schema_node().
%% The various types that can be looked up.
-type lookup_search() :: object() | ref() | pid() 
                       | schema_nodes() | schema_node().
%% Where to search for the type being looked up.

%% ===================================================================
%% API functions
%% ===================================================================

get(Type, Schema) ->
    case lookup(Type, Schema) of
        undefined -> throw({schema_not_found, Type, Schema});
        Node -> Node
    end.

lookup(Type, Schema, Default) ->
    case lookup(Type, Schema) of
        undefined -> Default;
        Node -> Node
    end.

-spec lookup(lookup_type(), lookup_search()) ->
                    {ok, schema_node()} | {unknown_type, Type::lookup_type()}.
%% @doc Find schema node for type.
lookup(N, _) when is_record(N, schema_node) -> N;
lookup(Type, Schema) when is_atom(Schema) -> Schema:schema(Type);
lookup(Id, #schema_node{ id=Id }=N) -> N;
lookup(Name, #schema_node{ name=Name }=N) -> N;
lookup(Type, #schema_node{ module=Module }) when Module /= undefined -> lookup(Type, Module);

%% deprecated clauses..
lookup(Type, #schema_node{ nodes=Ns }) -> lookup(Type, Ns);
lookup(Type, #object{ ref=#ref{ data=Pid } }) -> lookup(Type, Pid);
lookup(Type, #ref{ data=Pid }) -> lookup(Type, Pid);
lookup(Type, Pid) when is_pid(Pid) ->
    case ecapnp_data:get_type(Type, Pid) of
        false -> {unknown_type, Type};
        N when is_record(N, schema_node) -> {ok, N}
    end;
lookup(Type, [N|Ns]) -> 
    Res = lookup(Type, N),
    if element(1, Res) == ok -> Res;
       true -> lookup(Type, Ns)
    end;
lookup(Type, _) -> {unknown_type, Type}.

-spec type_of(object()) -> schema_node().
%% @doc Get type of object.
%% @todo Doesn't this belong in ecapnp_obj?
type_of(#object{ schema=Type }) -> Type.

-spec size_of(lookup_type(), lookup_search()) -> non_neg_integer().
%% @doc Lookup struct type and query it's size.
size_of(Type, Store) ->    
    {ok, T} = lookup(Type, Store),
    size_of(T).

-spec size_of(Node::schema_node()) -> non_neg_integer().
%% @doc Query size of a struct type.
%%
%% Will crash with `function_clause' if `Node' is not a struct node.
size_of(#struct{ dsize=DSize, psize=PSize }) ->
    DSize + PSize;
size_of(#schema_node{ kind=Kind }) ->
    size_of(Kind);
size_of(#interface{ struct=Struct }) ->
    size_of(Struct).

-spec data_size(schema_node()) -> non_neg_integer().
%% @doc Get data size of a struct type.
data_size(Node) ->
    struct_value(Node, #struct.dsize).

-spec ptrs_size(schema_node()) -> non_neg_integer().
%% @doc Get pointer count for a struct type.
ptrs_size(Node) ->
    struct_value(Node, #struct.psize).

get_ref_kind(#struct{ dsize=DSize, psize=PSize }) ->
    #struct_ref{ dsize=DSize, psize=PSize };
get_ref_kind(#schema_node{ kind=Kind }) ->
    get_ref_kind(Kind);
get_ref_kind(#interface{ struct=Struct }) ->
    setelement(1, get_ref_kind(Struct), interface_ref).

get_ref_kind(Type, Ref) when is_atom(Type); is_number(Type) ->
    {ok, T} = lookup(Type, Ref),
    get_ref_kind(T);
get_ref_kind(Type, _) ->
    get_ref_kind(Type).

    
-spec set_ref_to(lookup_type(), ref()) -> ref().
%% @doc Set reference kind.
%%
%% Lookup struct `Type' and return an updated {@link ref(). ref}.
%%
%% Note: it is only the record that is updated, the change is not
%% committed to the message.
set_ref_to(Type, Ref) ->
    Ref#ref{ kind=get_ref_kind(Type, Ref) }.

%% ===================================================================
%% internal functions
%% ===================================================================

struct_value(Struct, Idx) when is_record(Struct, struct) ->
    element(Idx, Struct);
struct_value(#schema_node{ kind=Kind }, Idx) ->
    struct_value(Kind, Idx);
struct_value(#interface{ struct=Struct }, Idx) ->
    struct_value(Struct, Idx).
