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

-import(ecapnp_schema, [lookup/2]).
-import(ecapnp_data, [update_segment/3]).
-import(ecapnp_obj, [alloc/2, create_ptr/1, create_ptr/2, 
                     ptr_offset/2, segment_id/1, update/3,
                     data_offset/2, data_segment/3]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

root(Type, Schema) ->
    {ok, RootType} = lookup(Type, Schema),
    Data = ecapnp_data:new(100),
    ok = update_segment(
           ecapnp_data:alloc(
             0,
             1 + RootType#struct.dsize + RootType#struct.psize, 
             Data),
           create_ptr(RootType), 
           Data),
    {ok, ecapnp_obj:get(
           RootType,
           [{offset, 1},
            {type, RootType},
            {parent, Schema},
            {data, Data}
           ])
    }.


field(Field, Value, Object)
  when is_record(Field, data) ->
    set_data(Field, Value, Object);
field(Field, Value, Object) 
  when is_record(Field, ptr) ->
    set_ptr(Field, Value, Object).


%% ===================================================================
%% internal functions
%% ===================================================================

%% Enum field
set_data(#data{ type={enum, Type} }=D, Value, Object) ->
    {ok, #enum{ values=Values }} = lookup(Type, Object),
    set_data(D#data{ type=uint16 }, enum_tag(Value, Values), Object);

%% Union field
set_data(#data{ type={union, Fields} }=D, Value, Object) ->
    {Tag, Field} = union_tag(Value, Fields),
    set_data(D#data{ type=uint16 }, Tag, Object),
    case Field of
        void -> ok;
        {FieldType, FieldValue} ->
            field(FieldType, FieldValue, Object)
    end;

%% Value field
set_data(#data{ type=Type, align=Align }, Value, Object) ->
    update(
      data_offset(0, Object),
      set_value(Value, Type, 0, Align,
                data_segment(0, 1 + (Align div 64), Object)),
      Object).

%% List field
set_ptr(#ptr{type={list, Type}}=Ptr, Value, Obj) ->
    {ok, T} = lookup(Type, Obj),
    Size = ecapnp_obj:list_size(Value, T),
    {SegmentId, SegmentOffset} = alloc(Size, Obj),
    
    %% NOTE: if SegmentId != Object.segment_id then we need a far ptr here..
    SegmentId = segment_id(Obj), %% just a simple assert for now.. so we don't get unexpected results
    
    List = #list_ptr{
              offset=ptr_offset(SegmentOffset, Ptr, Obj),
              size=T#struct.esize,
              count=if T#struct.esize == inlineComposite -> Size - 1;
                       true -> Value
                    end,
              object=Obj },
    %% the list ptr
    ok = update_list_ptr(List, Ptr#ptr.idx),
    %% the composite list element info tag
    ok = update(SegmentOffset,
                create_ptr(Value, T),
                Obj),
    [ecapnp_obj:get(T, [{offset, O}, {copy, Obj}]) 
      || O <- lists:seq(SegmentOffset + 1, SegmentOffset + Size - 1,
                        T#struct.dsize + T#struct.psize)];

%% Text field
set_ptr(#ptr{ type=text }=Ptr, Value, Object) 
  when is_binary(Value) ->
    Size = size(Value) + 1,
    Words = Size div 8 + (if Size rem 8 > 0 -> 1; true -> 0 end),
    {SegmentId, SegmentOffset} = alloc(Words, Object),

    %% NOTE: if SegmentId != Object.segment_id then we need a far ptr here..
    SegmentId = segment_id(Object), %% just a simple assert for now.. so we don't get unexpected results

    ok = update_list_ptr(
           #list_ptr{
             offset=ptr_offset(SegmentOffset, Ptr, Object),
             size=byte,
             count=Size,
             object=Object },
           Ptr#ptr.idx),
    ok = update(SegmentOffset,
               <<Value/binary, 0>>,
               Object);

%% Struct field
set_ptr(_Field, _Value, _Object) ->
    nyi.

%% Data field helpers
-define(SET_VALUE(ValueType, Size, TypeSpec),
        set_value(Value, ValueType, Offset, Align, Segment) ->
               <<Pre:Offset/binary-unit:64,
                 PreV:Align/bits,
                 _:Size/TypeSpec,
                 Post/bits>> = Segment,
               <<Pre/binary, PreV/bits, Value:Size/TypeSpec, Post/bits>>
                   ).

?SET_VALUE(uint64, 64, integer-unsigned-little);
?SET_VALUE(uint32, 32, integer-unsigned-little);
?SET_VALUE(uint16, 16, integer-unsigned-little);
?SET_VALUE(uint8, 8, integer-unsigned-little);
?SET_VALUE(int64, 64, integer-signed-little);
?SET_VALUE(int32, 32, integer-signed-little);
?SET_VALUE(int16, 16, integer-signed-little);
?SET_VALUE(int8, 8, integer-signed-little);
?SET_VALUE(bool, 1, bits);
?SET_VALUE(float32, 32, float-little);
?SET_VALUE(float64, 64, float-little).

ptr_offset(Offset, #ptr{ idx=Idx }, #object{ poffset=PtrOffset }) ->
    Offset - PtrOffset - Idx - 1.

update_list_ptr(#list_ptr{ object=Obj }=Ptr, Offset) ->
    update(
      ptr_offset(Offset, Obj),
      create_ptr(Ptr),
      Obj).

enum_tag(Value, Values) ->
    enum_tag(Value, Values, 0).

enum_tag(Value, [Value|_], Tag) ->
    Tag;
enum_tag(Value, [_|Values], Tag) ->
    enum_tag(Value, Values, Tag + 1).

union_tag(Value, Fields) ->
    union_tag(Value, Fields, 0).

union_tag({TagName, Value}, [{TagName, Field}|_], Tag) ->
    {Tag, {Field, Value}};
union_tag(TagName, [{TagName, _}|_], Tag) ->
    {Tag, void};
union_tag(Value, [_|Fields], Tag) ->
    union_tag(Value, Fields, Tag + 1).
