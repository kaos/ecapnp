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
%% @doc Write support.
%%
%% Everything for writing data into a message.

-module(ecapnp_set).
-author("Andreas Stenius <kaos@astekk.se>").

-export([root/2, field/3, union/2]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

-spec root(type_name(), schema()) -> {ok, object()}.
%% @doc Get root object for a new message.
root(Type, Schema) ->
    {ok, ecapnp_obj:alloc(Type, 0, ecapnp_data:new({Schema, 100}))}.

-spec field(field_name(), field_value(), object()) -> ok | list().
%% @doc Write field value to object.
field(FieldName, Value, Object) ->
    set_field(
      ecapnp_obj:field(FieldName, Object),
      Value, Object#object.ref).

-spec union({field_name(), field_value()} | field_name(), object()) -> ok.
%% @doc Write unnamed union value in object.
union(Value, #object{ ref=Ref,
                      schema=#schema_node{
                                kind=#struct{ union_field=Union }}
                    }=Object) ->
    if Union /= none -> set_field(Union, Value, Ref);
       true -> throw({no_unnamed_union_in_object, Object})
    end.


%% ===================================================================
%% internal functions
%% ===================================================================

set_field(#data{ type=Type, align=Align, default=Default }=D,
          Value0, StructRef) ->
    Value = if Value0 == undefined -> Default;
               true -> Value0
            end,
    case Type of
        {enum, EnumType} ->
            {ok, #schema_node{ kind=#enum{ values=Values } }}
                = ecapnp_schema:lookup(EnumType, StructRef),
            Tag = if is_atom(Value) ->
                          {Idx, Value} = lists:keyfind(Value, 2, Values),
                          Idx;
                     is_integer(Value) -> Value
                  end,
            set_field(D#data{ type=uint16 }, Tag, StructRef);
        {union, Fields} ->
            {Tag, Field} = union_tag(Value, Fields),
            set_field(D#data{ type=uint16 }, Tag, StructRef),
            case Field of
                void -> ok;
                {FieldType, FieldValue} ->
                    set_field(FieldType, FieldValue, StructRef)
            end;
        Type ->
            Size = ecapnp_val:size(Type),
            ecapnp_ref:write_struct_data(
              Align, Size,
              ecapnp_val:set(Type, Value, Default),
              StructRef)
    end;

set_field(#ptr{ idx=Idx, type=Type }=Ptr, Value, StructRef) ->
    case Type of
        text -> ecapnp_ref:write_text(
                  Value,
                  ecapnp_ref:ptr(Idx, StructRef),
                  StructRef);
        data -> ecapnp_ref:write_data(
                  Value,
                  ecapnp_ref:ptr(Idx, StructRef),
                  StructRef);
        object ->
            case Value of
                {ObjType, ObjValue} ->
                    set_field(Ptr#ptr{ type=ObjType }, ObjValue, StructRef);
                ObjType ->
                    ObjRef = ecapnp_ref:alloc_data(
                               ecapnp_schema:set_ref_to(
                                 ObjType, ecapnp_ref:ptr(
                                            Idx, StructRef))),
                    {ok, ecapnp_obj:from_ref(ObjRef, ObjType)}
            end;
        {list, ElementType} ->
            if is_integer(Value) -> %% init list
                    ecapnp_obj:from_ref(
                      ecapnp_ref:alloc_list(
                        Idx, #list_ref{
                                size=list_element_size(ElementType, StructRef),
                                count=Value },
                        StructRef),
                      Type);
               is_tuple(Value), size(Value) == 2 -> %% {Idx, Value}
                    case ElementType of
                        object -> throw(not_yet_implemented);
                        interface -> throw(not_yet_implemented);
                        {list, _} -> throw(not_yet_implemented);
                        {struct, StructType} ->
                            case element(2, Value) of %% {Idx, {Field, Value}}
                                {FieldName, FieldValue} ->
                                    List = ecapnp_ref:read_struct_ptr(
                                             Idx, StructRef),
                                    field(FieldName, FieldValue,
                                          ecapnp_obj:from_ref(
                                            ecapnp_ref:ptr(
                                              element(1, Value), List),
                                            StructType))
                            end;
                        text ->
                            List = ecapnp_ref:read_struct_ptr(Idx, StructRef),
                            ecapnp_ref:write_text(
                              element(2, Value),
                              ecapnp_ref:ptr(element(1, Value), List),
                              List);
                        data ->
                            List = ecapnp_ref:read_struct_ptr(Idx, StructRef),
                            ecapnp_ref:write_data(
                              element(2, Value),
                              ecapnp_ref:ptr(element(1, Value), List),
                              List);
                        _ ->
                            ecapnp_ref:write_list(
                              Idx,
                              element(1, Value),
                              ecapnp_val:set(ElementType, element(2, Value)),
                              StructRef)
                    end;
               is_list(Value) -> %% [Value...]
                    set_field(Ptr, length(Value), StructRef),
                    [ok = set_field(Ptr, V, StructRef)
                     || V <- lists:zip(
                               lists:seq(0, length(Value) - 1),
                               Value)], ok
            end
    end;
set_field(#group{ id=Type }, Value, StructRef) ->
    union(Value, ecapnp_obj:from_ref(StructRef, Type)).


union_tag({FieldName, Value}, [{Tag, FieldName, FieldType}|_]) ->
    {Tag, {FieldType, Value}};
union_tag(FieldName, [{Tag, FieldName, FieldType}|_]) ->
    {Tag, default(FieldType)};
union_tag({Tag, Value}, [{Tag, _, FieldType}|_]) ->
    {Tag, {FieldType, Value}};
union_tag(Tag, [{Tag, _, FieldType}|_]) ->
    {Tag, default(FieldType)};
union_tag(Value, [_|Fields]) ->
    union_tag(Value, Fields).

default(void) -> void;
default(FieldType) -> {FieldType, undefined}.

list_element_size(text, _) -> pointer;
list_element_size(data, _) -> pointer;
list_element_size(object, _) -> pointer;
list_element_size({list, _}, _) -> pointer;
list_element_size({Simple, _}, _)
  when Simple == enum; Simple == union -> twoBytes;
list_element_size({_, Type}, Ref) ->
    case ecapnp_schema:lookup(Type, Ref) of
        {ok, #schema_node{
                kind=#struct{
                        esize=inlineComposite,
                        dsize=DSize, psize=PSize }}} ->
            #struct_ref{ dsize=DSize, psize=PSize };
        {ok, #schema_node{
                kind=#struct{ esize=Size }}} -> Size
        end;
list_element_size(Type, _) ->
    list_element_size(ecapnp_val:size(Type)).

list_element_size(0) -> empty;
list_element_size(1) -> bit;
list_element_size(8) -> byte;
list_element_size(16) -> twoBytes;
list_element_size(32) -> fourBytes;
list_element_size(64) -> eightBytes.
