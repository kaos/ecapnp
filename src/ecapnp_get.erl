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

-export([root/2, root/3, field/2, union/1, ref_data/2, ref_data/3]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Get the root object for a message.
%% @see ecapnp:get_root/3
-spec root(schema_node(), message()) -> {ok, Root::object()}.
root(Node, Segments) ->
    {ok, ecapnp_obj:from_data(Segments, Node)}.

-spec root(type_name(), schema(), message()) -> {ok, Root::object()}.
root(Type, Schema, Segments) ->
    root(Schema:schema(Type), Segments).

%% @doc Read the field value of object.
%% @see ecapnp:get/2
-spec field(field_name(), object()) -> field_value().
%%field(FieldName, #object{ ref=Ref }=Object)
field(FieldName, #rpc_call{ params = Object }) -> field(FieldName, Object);
field(FieldName, Object)
  when is_record(Object, object) ->
    read_field(ecapnp_obj:field(FieldName, Object), Object).

%% @doc Read the unnamed union value of object.
%% @see ecapnp:get/1
-spec union(object()) -> {field_name(), field_value()} | field_name().
union(#rpc_call{ params = Object }) -> union(Object);
union(#object{ schema=#schema_node{
                         kind=#struct{ union_field=Union }
                        }}=Object) ->
    if Union /= none -> read_field(Union, Object);
       true -> throw({no_unnamed_union_in_object, Object})
    end.

%% @doc internal function not intended for client code.
ref_data(Ptr, Obj) ->
    read_ptr(Ptr, Obj).

%% @doc Read data of object reference as type.
%% This is a Low-level function.
ref_data(Type, Obj, Default) ->
    read_ptr(#ptr{ type=Type, default=Default }, Obj).


%% ===================================================================
%% internal functions
%% ===================================================================

read_field(#field{ id = Id, kind = Kind },
           #object{ ref = #promise{ transform = Ts }=P }=Obj) ->
    case Kind of
        %% #ptr{ type = object } ->
        %%     ecapnp_obj:init(P#promise{ transform = [{ptr, Id}|Ts] }, Obj);
        #ptr{ type = {struct, Type} } ->
            ecapnp_obj:init(P#promise{ transform = [{getPointerField, Id}|Ts] },
                            ecapnp_schema:get(Type, Obj));
        #ptr{ type = {interface, Type} } ->
            ecapnp_obj:init(P#promise{ transform = [{getPointerField, Id}|Ts] },
                            ecapnp_schema:get(Type, Obj));
        _ ->
            {ok, Res} = ecapnp:wait(P),
            read_field(Kind, Res)
    end;
read_field(#field{ kind=Kind }, Object) -> read_field(Kind, Object);
read_field(#data{ type=Type, align=Align, default=Default }=D, Object) ->
    case Type of
        {enum, EnumType} ->
            Tag = read_field(D#data{ type=uint16 }, Object),
            get_enum_value(EnumType, Tag, Object);
        {union, Fields} ->
            Tag = read_field(D#data{ type=uint16 }, Object),
            case lists:keyfind(Tag, #field.id, Fields) of
                #field{ name=FieldName, kind=void } -> FieldName;
                #field{ name=FieldName }=Field ->
                    {FieldName, read_field(Field, Object)}
            end;
        Type ->
            case ecapnp_val:size(Type) of
                0 -> void;
                Size ->
                    ecapnp_val:get(
                      Type, ecapnp_ref:read_struct_data(
                              Align, Size, Object#object.ref),
                      Default)
            end
    end;
read_field(#ptr{ idx=Idx }=Ptr, #object{ ref = Ref }=Object) ->
    Obj = ecapnp_obj:init(
            ecapnp_ref:read_struct_ptr(Idx, Ref),
            Object),
    read_ptr(Ptr, Obj);
read_field(#group{ id=Type }, Object) ->
    ecapnp_obj:to_struct(Type, Object).

read_ptr(#ptr{ type=Type, default=Default }, #object{ ref = Ref }=Obj) ->
    case Type of
        text -> ecapnp_ref:read_text(Ref, Default);
        data -> ecapnp_ref:read_data(Ref, Default);
        object -> read_obj(object, Obj, Default);
        {struct, StructType} -> read_obj(StructType, Obj, Default);
        {interface, InterfaceType} -> read_obj(InterfaceType, Obj, Default);
        {list, ElementType} ->
            case ecapnp_ref:read_list(Ref, undefined) of
                undefined ->
                    if is_binary(Default) ->
                            ecapnp_obj:from_data(Default, Type, Obj);
                       true ->
                            Default
                    end;
                Refs when is_record(hd(Refs), ref) ->
                    [read_ptr(
                       #ptr{ type=ElementType },
                       ecapnp_obj:init(R, Obj))
                     || R <- Refs];
                Values ->
                    case ElementType of
                        {enum, EnumType} ->
                            [get_enum_value(
                               EnumType,
                               ecapnp_val:get(uint16, Data),
                               Obj)
                             || Data <- Values];
                        _ ->
                            [ecapnp_val:get(ElementType, Data)
                             || Data <- Values]
                    end
            end
    end.

read_obj(Type, #object{ ref = #ref{ kind=null } }=Obj, Default) ->
    if is_binary(Default) ->
            ecapnp_obj:from_data(Default, Type, Obj)
    end;
read_obj(Type, Obj, _) ->
    ecapnp_obj:to_struct(Type, Obj).

get_enum_value(Type, Tag, Obj) ->
    #schema_node{ kind=#enum{ values=Values } }
        = ecapnp_schema:lookup(Type, Obj),
    case lists:keyfind(Tag, 1, Values) of
        {Tag, Value} -> Value;
        false -> Tag
    end.
