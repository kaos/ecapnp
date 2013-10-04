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

-module(ecapnp_set).
-author("Andreas Stenius <kaos@astekk.se>").

-export([root/2, field/3]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

root(Type, Schema) ->
    {ok, ecapnp_obj:alloc(Type, 0, ecapnp_data:new({Schema, 100}))}.

field(FieldName, Value, Object) ->
    set_field(
      ecapnp_obj:field(FieldName, Object),
      Value, Object#object.ref).


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
            {ok, #enum{ values=Values }}
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
                    set_field(Ptr#ptr{ type=ObjType }, ObjValue, StructRef)
            end;
        {list, ElementType} ->
            if is_integer(Value) ->
                    ecapnp_ref:alloc_list(
                      Idx,
                      #list_ref{
                         size=list_element_size(ElementType, StructRef),
                         count=Value },
                      StructRef);
               is_tuple(Value), size(Value) == 2 -> %% {Idx, Value}
                    ElementValue =
                        case list_element_size(ElementType, StructRef) of
                            inlineComposite -> ugh;
                            pointer -> hmm;
                            _ -> ecapnp_val:set(ElementType, 
                                                element(2, Value),
                                                0)
                        end,
                    ecapnp_ref:write_list(
                      Idx,
                      element(1, Value),
                      ElementValue,
                      StructRef);
               is_list(Value) -> %% [Value...]
                    [set_field(Ptr, V, StructRef)
                     || V <- lists:zip(
                               lists:seq(0, length(Value) - 1),
                               Value)]
            end
    end.


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

list_element_size({list, _}, _) -> pointer;
list_element_size(Type, Ref) ->
    case ecapnp_schema:lookup(Type, Ref) of
        {ok, object} -> pointer;
        {ok, #struct{ esize=Size }} -> Size;
        {ok, _} -> twoBytes; %% enum & union
        _ -> list_element_size(ecapnp_val:size(Type))
    end.

list_element_size(0) -> empty;
list_element_size(1) -> bit;
list_element_size(8) -> byte;
list_element_size(16) -> twoBytes;
list_element_size(32) -> fourBytes;
list_element_size(64) -> eightBytes.

