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
%% @doc Read support.
%%
%% Everything for reading data out of a message.

-module(ecapnp_get).
-author("Andreas Stenius <kaos@astekk.se>").

-export([root/3, field/2, union/1, ref_data/2, ref_data/3]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Get the root object for a message.
%% @see ecapnp:get_root/3
-spec root(type_name(), schema(), message()) -> {ok, Root::object()}.
root(Type, Schema, Segments) ->
    {ok, ecapnp_obj:from_data(
           #msg{
              schema=Schema,
              alloc=[size(S) || S <- Segments],
              data=Segments
             },
           Type)}.

%% @doc Read the field value of object.
%% @see ecapnp:get/2
-spec field(field_name(), object()) -> field_value().
field(FieldName, #object{ ref=Ref }=Object)
  when is_atom(FieldName) ->
    read_field(ecapnp_obj:field(FieldName, Object), Ref).

%% @doc Read the unnamed union value of object.
%% @see ecapnp:get/1
-spec union(object()) -> {field_name(), field_value()} | field_name().
union(#object{ ref=Ref,
               schema=#schema_node{
                         kind=#struct{ union_field=Union }
                        }}=Object) ->
    if Union /= none -> read_field(Union, Ref);
       true -> throw({no_unnamed_union_in_object, Object})
    end.

%% @doc internal function not intended for client code.
ref_data(Ptr, Ref) ->
    read_ptr(Ptr, Ref).

%% @doc Read data of object reference as type.
%% This is a Low-level function.
ref_data(Type, #object{ ref=Ref }, Default) ->
    ref_data(Type, Ref, Default);
ref_data(Type, Ref, Default) ->
    read_ptr(#ptr{ type=Type, default=Default }, Ref).


%% ===================================================================
%% internal functions
%% ===================================================================

read_field(#field{ kind=Kind }, StructRef) -> read_field(Kind, StructRef);
read_field(#data{ type=Type, align=Align, default=Default }=D, StructRef) ->
    case Type of
        {enum, EnumType} ->
            Tag = read_field(D#data{ type=uint16 }, StructRef),
            get_enum_value(EnumType, Tag, StructRef);
        {union, Fields} ->
            Tag = read_field(D#data{ type=uint16 }, StructRef),
            case lists:keyfind(Tag, 1, Fields) of
                {Tag, FieldName, #field{ kind=void }} ->
                    FieldName;
                {Tag, FieldName, Field} ->
                    {FieldName, read_field(Field, StructRef)}
            end;
        Type ->
            case ecapnp_val:size(Type) of
                0 -> void;
                Size ->
                    ecapnp_val:get(
                      Type, ecapnp_ref:read_struct_data(
                              Align, Size, StructRef),
                      Default)
            end
    end;
read_field(#ptr{ idx=Idx }=Ptr, StructRef) ->
    Ref = ecapnp_ref:read_struct_ptr(Idx, StructRef),
    read_ptr(Ptr, Ref);
read_field(#group{ id=Type }, StructRef) ->
    ecapnp_obj:from_ref(StructRef, Type).

read_ptr(#ptr{ type=Type, default=Default }, Ref) ->
    case Type of
        text -> ecapnp_ref:read_text(Ref, Default);
        data -> ecapnp_ref:read_data(Ref, Default);
        object -> read_obj(object, Ref, Default);
        {struct, StructType} -> read_obj(StructType, Ref, Default);
        {interface, InterfaceType} -> read_obj(InterfaceType, Ref, Default);
        {list, ElementType} ->
            case ecapnp_ref:read_list(Ref, undefined) of
                undefined ->
                    if is_binary(Default) ->
                            ecapnp_obj:from_data(
                              {Ref#ref.data, Default},
                              Type);
                       true ->
                            Default
                    end;
                Refs when is_record(hd(Refs), ref) ->
                    [read_ptr(#ptr{ type=ElementType }, R)
                     || R <- Refs];
                Values ->
                    case ElementType of
                        {enum, EnumType} ->
                            [get_enum_value(
                               EnumType,
                               ecapnp_val:get(uint16, Data),
                               Ref) 
                             || Data <- Values];
                        _ ->
                            [ecapnp_val:get(ElementType, Data)
                             || Data <- Values]
                    end
            end
    end.

read_obj(Type, #ref{ kind=null }=Ref, Default) ->
    if is_binary(Default) ->
            ecapnp_obj:from_ref(
              ecapnp_ref:paste(Default, Ref),
              Type)
    end;
read_obj(Type, Ref, _) ->
    ecapnp_obj:from_ref(Ref, Type).

get_enum_value(Type, Tag, Ref) ->
    #schema_node{ kind=#enum{ values=Values } }
        = ecapnp_schema:lookup(Type, Ref),
    case lists:keyfind(Tag, 1, Values) of
        {Tag, Value} -> Value;
        false -> Tag
    end.
