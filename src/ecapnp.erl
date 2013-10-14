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
%% @doc The highlevel Cap'n Proto API.
%%
%% This module doesn't implement any functionality, it is simply
%% exposing the highlevel functions from the other modules.

-module(ecapnp).
-author("Andreas Stenius <kaos@astekk.se>").

%% ===================================================================
%% Public API
%% ===================================================================

-export([get_root/3, get/1, get/2,
         set_root/2, set/2, set/3]).

%% ===================================================================
%% Public Types
%% ===================================================================

-export_type([%element_size/0,
              field_name/0,
              field_type/0,
              field_value/0,
              message/0,
              node_type/0,
              node_types/0,
              object/0,
              object_field/0,
              object_fields/0,
              ref_kind/0,
              schema/0,
              schema_node/0,
              schema_type/0,
              segment_id/0,
              type_id/0,
              type_name/0,
              value/0
             ]).

-include("ecapnp_records.hrl").

-type schema_node() :: #schema_node{}.

-type schema_nodes() :: list(schema_node()).

-type schema_kind() :: file | struct()
                     | enum() | interface()
                     | const() | annotation().

-type struct() :: #struct{}.

-type enum() :: #enum{}.
-type interface() :: #interface{}.
-type const() :: #const{}.
-type annotation() :: #annotation{}.

-type text() :: binary().

-type schema() :: #schema_node{ kind::file }.


-type element_size() :: empty | bit | byte | twoBytes | fourBytes | eightBytes | pointer | inlineComposite.
-type field_name() :: atom().
-type field_type() :: #data{} | #ptr{} | #group{}.
-type field_value() :: value() | object().
-type message() :: list(binary()).
-type node_type() :: #struct{} | #enum{} | #interface{} | #const{} | #annotation{}.
-type node_types() :: list({atom(), node_type()}).
-type object() :: #object{}.
-type object_field() :: #data{} | #ptr{}.
-type object_fields() :: list({field_name(), object_field()}).
-type ref_kind() :: null | #struct_ref{} | #list_ref{} | #far_ref{}.
%-type schema_node() :: #node{}.
-type schema_type() :: type_name() | type_id().
-type segment_id() :: integer().
-type type_id() :: integer().
-type type_name() :: atom().
-type value() :: number() | boolean() | list(value()) | binary() | null.


%% ===================================================================
%% API Implementation
%% ===================================================================

%% @doc Get the root object for a message.
%% The message should already have been unpacked and parsed.
%% @see ecapnp_get:root/3
%% @see ecapnp_serialize:unpack/1
%% @see ecapnp_message:read/1
-spec get_root(type_name(), schema(), message()) -> {ok, Root::object()}.
get_root(Type, Schema, [Segment|_]=Segments) 
  when is_atom(Type),
       is_record(Schema, schema_node),
       is_binary(Segment) ->
    ecapnp_get:root(Type, Schema, Segments).

%% @doc Set the root object for a new message.
%% This creates a new empty message, ready to be filled with data.
%%
%% To get the segment data out, call {@link ecapnp_message:write/1}.
%% @see ecapnp_set:root/2
-spec set_root(type_name(), schema()) -> {ok, Root::object()}.
set_root(Type, Schema)
  when is_atom(Type),
       is_record(Schema, schema_node) ->
    ecapnp_set:root(Type, Schema).

%% @doc Read the unnamed union value of object.
%% The result value is either a tuple, describing which union tag it
%% is, and its associated value, or just the tag name, if the value is
%% void.
%% @see ecapnp_get:union/1
-spec get(object()) -> {field_name(), field_value()} | field_name().
get(Object)
  when is_record(Object, object) ->
    ecapnp_get:union(Object).

%% @doc Read the field value of object.
%% @see ecapnp_get:field/2
-spec get(field_name(), object()) -> field_value().
get(Field, Object)
  when is_atom(Field), is_record(Object, object) ->
    ecapnp_get:field(Field, Object).

%% @doc Write union value to the unnamed union of object.
%% @see ecapnp_set:union/2
-spec set({field_name(), field_value()}|field_name(), object()) -> ok.
set(Value, Object)
  when is_record(Object, object) ->
    ecapnp_set:union(Value, Object).

%% @doc Write value to a field of object.
%% @see ecapnp_set:field/3
-spec set(field_name(), field_value(), object()) -> ok.
set(Field, Value, Object)
  when is_atom(Field), is_record(Object, object) ->
    ecapnp_set:field(Field, Value, Object).


%% ===================================================================
%% internal functions
%% ===================================================================
