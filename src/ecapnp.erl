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
-include("ecapnp_records.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-export([get_root/3, get/1, get/2, set_root/2, set/2, set/3]).

%% ===================================================================
%% Public Types
%% ===================================================================

-export_type([annotation/0, bit_count/0, const/0, data/0,
              element_size/0, enum/0, enum_values/0, far_ref/0,
              field_type/0, field_value/0, group/0, interface/0,
              list_ref/0, message/0, msg/0, object/0, ptr/0,
              ptr_count/0, ptr_index/0, ref/0, ref_kind/0, schema/0,
              schema_kind/0, schema_node/0, schema_nodes/0,
              schema_type/0, segment_id/0, segment_offset/0,
              segment_pos/0, struct/0, struct_fields/0, struct_ref/0,
              text/0, type_id/0, type_name/0, value/0,
              value_type/0, word_count/0 ]).


-type annotation() :: #annotation{}.
%% Describes an annotation type.

-type bit_count() :: non_neg_integer().
-type const() :: #const{}.
%% A schema const value.

-type data() :: pid().
%% Pid of the data server for a reference pointer.

-type element_size() :: empty | bit | byte | twoBytes | fourBytes
                      | eightBytes | pointer | inlineComposite.
%% The data size for the values in a list.
%%
%% In case of `inlineComposite' the list data is prefixed with a `tag'
%% word describing the layout of the element data. The `tag' is in the
%% same format as a struct ref, except the `offset' field indicates
%% the number of elements in the list.

-type enum() :: #enum{}.
%% Describes the schema for a enum type.

-type enum_values() :: list({integer(), atom()}).
%% A list of tuples, pairing the enumerants ordinal value with its name.

-type far_ref() :: #far_ref{}.

-type field_name() :: atom().
-type field_type() :: #data{} | #ptr{} | #group{}.
-type field_value() :: any().
%% @todo improve type spec.

-type group() :: #group{}.
%% Declares the type of group for a struct field.

-type interface() :: #interface{}.
%% Describes the schema for a interface type.

-type list_ref() :: #list_ref{}.
%% The reference is a pointer to a list.

-type message() :: list(binary()).
%% Holds all the segments in a Cap'n Proto message.
%%
%% This is the raw segment data, no segment headers or other
%% information is present.

-type msg() :: #msg{}.
%% A message record for the data server.

-type object() :: #object{}.
%% A reference paired with schema type information.

%% -type object_field() :: #data{} | #ptr{}.
%% -type object_fields() :: list({field_name(), object_field()}).

-type ptr() :: #ptr{}.
%% Describes a pointer field within a struct.

-type ptr_count() :: non_neg_integer().
-type ptr_index() :: non_neg_integer().
-type ref() :: #ref{}.
%% A reference instance.
%%
%% If `pos' is `-1', then the instance has no allocated space in the
%% message, but may still point to a valid location within a
%% segment. Note, the value of `pos' should still be used even when `pos' is `-1'.
%%
%% To get the position of the data the reference points to:
%% ``DataPos = R#ref.pos + R#ref.offset + 1.''

-type ref_kind() :: null | struct_ref() | list_ref() | far_ref().

-type schema() :: #schema_node{ kind::file }.
%% The top-level schema node (for the .capnp-file).

-type schema_kind() :: file | struct()
                     | enum() | interface()
                     | const() | annotation().


-type schema_node() :: #schema_node{}.
%% Each schema node within a file.

-type schema_nodes() :: list(schema_node()).
-type schema_type() :: type_name() | type_id().
-type segment_id() :: integer().
-type segment_offset() :: integer().
-type segment_pos() :: -1 | non_neg_integer().
-type struct() :: #struct{}.
%% Describes the schema for a struct type.
%%
%% <dl>
%%   <dt>`dsize'</dt>
%%   <dd>The size of the struct's data section, in words.</dd>
%%   <dt>`psize'</dt>
%%   <dd>The number of pointers in the struct.</dd>
%%   <dt>`esize'</dt>
%%   <dd>The list {@link element_size(). element size} for the struct.</dd>
%%   <dt>`union_field'</dt>
%%   <dd>Describes the unnamed union in the struct, or `none' if there
%%       is no unnamed union in the struct.</dd>
%%   <dt>`fields'</dt>
%%   <dd>Describes all the fields in the struct.</dd>
%% </dl>

-type struct_fields() :: list(field_type()).

-type struct_ref() :: #struct_ref{}.
%% The reference is a pointer to a struct.
%%
%% `dsize' and `psize' specifies the data size and pointer count
%% actually allocated in the message. This may not match those
%% expected by the schema. Any data in the message that is outside of
%% what the schema expects will be unreachable by application code,
%% while any reads outside of the allocated data will result in a
%% default value back. Thus the missmatch is transparent in
%% application code.

-type text() :: binary().
%% The required NULL byte suffix is automatically taken care of by
%% {@link ecapnp_ref} for `text' values.

-type type_id() :: integer().
-type type_name() :: atom().
-type value() :: number() | boolean() | list(value()) | binary() | null.
-type value_type() :: void | bool | float32 | float63
                    | uint8 | uint16 | uint32 | uint64
                    | int8 | int16 | int32 | int64.
-type word_count() :: non_neg_integer().
%% Note that in Cap'n Proto, a word is 8 bytes (64 bits).


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
