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
                     ptr_offset/2, segment_offset_ptr/3,
                     segment_id/1, update/3,
                     data_offset/2, data_segment/3]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

root(Type, Schema) ->
    {ok, RootType} = lookup(Type, Schema),
    Data = ecapnp_data:new({Schema, 100}),
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
            {data, Data}
           ])
    }.


field(Field, Value, Object)
  when is_record(Field, data) ->
    set_data(Field, Value, Object);
field(Field, Value, Object) 
  when is_record(Field, ptr) ->
    set_ptr(Field, Value, Object);
field(Field, Value, Object)
  when is_record(Field, group) ->
    set_group(Field, Value, Object).


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
set_data(#data{ type=Type, align=Align, default=Default }, Value, Object) ->
    update(
      data_offset(0, Object),
      ecapnp_val:set(Type, Value, 0, Align, Default,
                     data_segment(0, 1 + (Align div 64), Object)),
      Object).

%% List field
set_ptr(#ptr{type={list,_}}=Ptr, Value, Obj) ->
    write_list(Ptr, Value, Obj);

%% Struct field
set_ptr(#ptr{type={struct, _Type}}=Ptr, _Value, _Obj) ->
    throw({nyi, set_ptr, Ptr}); %%write_struct(Ptr, Value, Obj);

%% Text field
set_ptr(#ptr{ type=text }=Ptr, Value, Object) 
  when is_binary(Value) ->
    Size = size(Value) + 1,
    Words = Size div 8 + (if Size rem 8 > 0 -> 1; true -> 0 end),
    {SegmentId, SegmentOffset} = alloc(Words, Object),

    %% NOTE: if SegmentId != Object.segment_id then we need a far ptr here..
    SegmentId = segment_id(Object), %% just a simple assert for now.. so we don't get unexpected results

    ok = update_list_ptr(
           Ptr#ptr.idx,
           #list_ptr{
              offset=SegmentOffset,
              size=byte,
              count=Size,
              object=Object }),
    ok = update(SegmentOffset,
                <<Value/binary, 0>>,
                Object);

%% Struct field
set_ptr(_Field, _Value, _Object) ->
    throw({nyi, set_ptr, _Field}).

%% Group field
set_group(#group{ id=TypeId }, Value, Object) ->
    {ok, T} = lookup(TypeId, Object),
    field(T#struct.union_field, Value, Object).


%% Helpers
write_list(#ptr{type={list, Type}}=Ptr, Value, Obj) ->
    Count = list_length(Value),
    {ListSize, ESize, EType} =
        case list_element_size(Type, Obj) of
            {Es, Et} ->
                {ecapnp_obj:list_size(Count, {Es, Et}), Es, Et};
            Es ->
                {ecapnp_obj:list_size(Count, Es), Es, Type}
        end,
    {SegmentId, SegmentOffset} = alloc(ListSize, Obj),

    %% NOTE: if SegmentId != Object.segment_id then we need a far ptr here..
    SegmentId = segment_id(Obj), %% just a simple assert for now.. so we don't get unexpected results

    List = #list_ptr{
              offset=SegmentOffset,
              size=ESize,
              count=if ESize == inlineComposite -> ListSize - 1;
                       true -> Count
                    end,
              object=Obj },

    %% the list ptr
    ok = update_list_ptr(Ptr#ptr.idx, List),

    if is_record(EType, struct) ->
            Offset =
                if ESize == inlineComposite ->
                        %% the composite list element info tag
                        ok = update(SegmentOffset,
                                    create_ptr(Value, EType),
                                    Obj),
                        SegmentOffset + 1;
                   true -> SegmentOffset
                end,
            [ecapnp_obj:get(EType, [{offset, O}, {copy, Obj}]) 
             || O <- lists:seq(Offset, SegmentOffset + ListSize - 1,
                               EType#struct.dsize + EType#struct.psize)];
       is_list(Value) ->
            if EType == text ->
                    Offset = segment_offset_ptr(SegmentOffset, 0, Obj),
                    [ok = set_ptr(#ptr{ type=text, idx=Idx }, V, Obj)
                     || {Idx, V} <- lists:zip(
                                      lists:seq(Offset, Offset + ListSize - 1),
                                      Value
                                     )],
                    ok;
               true ->
                    BitSize = ecapnp_get:list_element_size(ecapnp_obj:element_size(ESize)),
                    update(
                      SegmentOffset,
                      iolist_to_binary(
                        [ecapnp_val:set(EType, V, 0, 0, 0, <<0:BitSize/integer>>)
                         || V <- Value]),
                      Obj)
            end
    end.

list_length(Length) when is_integer(Length) -> Length;
list_length(List) when is_list(List) -> length(List);
list_length(Text) when is_binary(Text) -> size(Text) + 1;
list_length(Bad) -> throw({invalid_list_value, Bad}).

update_list_ptr(PtrIdx, #list_ptr{ offset=SegmentOffset, object=Obj }=Ptr) ->
    update(
      ptr_offset(PtrIdx, Obj),
      create_ptr(
        segment_offset_ptr(SegmentOffset, PtrIdx, Obj) - 1,
        Ptr),
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

list_element_size(void,_) -> empty;
list_element_size(bool,_) -> bit;
list_element_size(int8,_) -> byte;
list_element_size(int16,_) -> twoBytes;
list_element_size(int32,_) -> fourBytes;
list_element_size(int64,_) -> eightBytes;
list_element_size(uint8,_) -> byte;
list_element_size(uint16,_) -> twoBytes;
list_element_size(uint32,_) -> fourBytes;
list_element_size(uint64,_) -> eightBytes;
list_element_size(float32,_) -> fourBytes;
list_element_size(float64,_) -> eightBytes;
list_element_size(text,_) -> pointer;
list_element_size(data,_) -> pointer;
list_element_size(object,_) -> pointer;
list_element_size({list, _},_) -> pointer;
list_element_size({enum, _},_) -> twoBytes;
list_element_size({interface, _},_) -> pointer;
list_element_size({struct, Type}, Obj) ->
    {ok, #struct{ esize=Size }=S} = lookup(Type, Obj),
    {Size, S};
list_element_size(Other,_) -> throw({unknown_capnp_type, Other}).
