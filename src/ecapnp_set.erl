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

set_field(#ptr{ idx=Idx, type=Type }, Value, StructRef) ->
    write_ptr(Type, Value, ecapnp_ref:ptr(Idx, StructRef), StructRef).

write_ptr(text, Value, Ptr, Ref) ->
    ecapnp_ref:write_text(Value, Ptr, Ref);
write_ptr(data, Value, Ptr, Ref) ->
    ecapnp_ref:write_data(Value, Ptr, Ref).
%% write_ptr({list, Type}, Value, Ptr, Ref) ->    
%%     ecapnp_ref:write_list(#list_ref{}, Ptr, Ref)


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

