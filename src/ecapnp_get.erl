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

-module(ecapnp_get).
-author("Andreas Stenius <kaos@astekk.se>").

-export([root/3, field/2]).
-export([list_element_size/1]).

-import(ecapnp_schema, [lookup/2]).
-import(ecapnp_obj, [segment/3, data_segment/3, ptr_segment/3,
                     ptr_type/2, ptr_offset/2]).
-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

root(Type, Schema, Segments) ->
    {ok, RootType} = lookup(Type, Schema),
    {ok, ecapnp_obj:from_ptr(
           0, 0, RootType,
           ecapnp_data:new(
             #msg{
                schema=Schema,
                alloc=[size(S) || S <- Segments],
                data=Segments
               }))
    }.

%% Lookup field value in object
field(Field, Object)
  when is_record(Field, data) ->
    get_data(Field, Object);
field(Field, Object)
  when is_record(Field, ptr) ->
    get_ptr(Field, Object);
field(Field, Object)
  when is_record(Field, group) ->
    get_group(Field, Object);
field(none, _) ->
    none.



%% ===================================================================
%% internal functions
%% ===================================================================

%% Enum field
get_data(#data{ type={enum, Type} }=D, Object) ->
    Value = get_data(D#data{ type=uint16 }, Object),
    {ok, #enum{ values=Values }} = lookup(Type, Object),
    lists:nth(Value + 1, Values);

%% Union field
get_data(#data{ type={union, Fields} }=D, Object) ->
    Tag = get_data(D#data{ type=uint16 }, Object),
    {FieldName, Field} = lists:nth(Tag + 1, Fields),
    if Field == void -> 
            FieldName;
       true ->
            FieldValue = field(Field, Object),
            {FieldName, FieldValue}
    end;

%% Value field
get_data(#data{ type=Type, align=Align },
         #object{ dsize=DSize }=Obj)
  when (Align div 64) < DSize ->
    read_value(Type, 0, Align, data_segment(0, 1 + (Align div 64), Obj)).


%% Ptr field (list, text, struct, object etc)
get_ptr(#ptr{ idx=Index }=Ptr,
        #object{ psize=PSize }=Obj)
  when Index < PSize ->
    read_ptr(Ptr, Obj).


%% Group field
get_group(#group{ id=TypeId }, Object) ->
    ecapnp_obj:set_type(TypeId, Object).


%% Data field helpers
-define(READ_VALUE(ValueType, Size, TypeSpec),
        read_value(ValueType, Offset, Align, Segment) ->
               <<_:Offset/binary-unit:64,
                 _:Align/bits,
                 Value:Size/TypeSpec,
                 _/bits>> = Segment,
               get_value(ValueType, Value)
                   ).

?READ_VALUE(uint64, 64, integer-unsigned-little);
?READ_VALUE(uint32, 32, integer-unsigned-little);
?READ_VALUE(uint16, 16, integer-unsigned-little);
?READ_VALUE(uint8, 8, integer-unsigned-little);
?READ_VALUE(int64, 64, integer-signed-little);
?READ_VALUE(int32, 32, integer-signed-little);
?READ_VALUE(int16, 16, integer-signed-little);
?READ_VALUE(int8, 8, integer-signed-little);
?READ_VALUE(bool, 1, bits);
?READ_VALUE(float32, 32, float-little);
?READ_VALUE(float64, 64, float-little).

%% convert read value to erlang
get_value(bool, <<0:1>>) -> false;
get_value(bool, <<1:1>>) -> true;
get_value(_, Value) -> Value.


%% Pointer field helpers
read_ptr(#ptr{ idx=Index }=Ptr, Obj) ->
    <<Offset:32/integer-signed-little, 
      Size:32/integer-little>> = ptr_segment(Index, 1, Obj),
    case ptr_type(Offset, Size) of
        null ->
            case Ptr#ptr.type of
                text -> <<>>;
                {list, _} -> [];
                %%{struct, Type} -> TODO: initialize new object
                _ -> null
            end;
        struct ->
            read_struct(
              Ptr, #struct_ptr{
                      offset = Index + 1 + (Offset bsr 2),
                      dsize = Size band 16#ffff,
                      psize = Size bsr 16,
                      object = Obj });
        list -> 
            read_list(
              Ptr, #list_ptr{
                      offset = Index + 1 + (Offset bsr 2),
                      size = list_element_size(Size band 7),
                      count = Size bsr 3,
                      object = Obj });
        far_ptr ->
            %% only support B == 0 for now..
            0 = Offset band 4,
            SegmentId = Size,
            read_ptr(
              Ptr#ptr{ idx=Offset bsr 3 },
              get_segment_object(SegmentId, Obj))
    end.

read_struct(#ptr{ type=Type },
            #struct_ptr{ offset=Offset,
                         dsize=DSize,
                         psize=PSize,
                         object=Obj }) ->
    ecapnp_obj:get( Type, 
                    [{offset, ptr_offset(Offset, Obj)},
                     {dsize, DSize},
                     {psize, PSize},
                     {copy, Obj}]).

read_list(#ptr{ type=text },
          #list_ptr{ offset=Offset,
                     count=Count,
                     size=8,
                     object=Obj }) ->
    TextLen = Count - 1,
    <<Text:TextLen/binary, _/binary>> =
        ptr_segment(Offset, 1 + (TextLen div 8), Obj),
    Text;
read_list(#ptr{ type={list, Type} },
          #list_ptr{ object=Obj }=ListPtr) ->
    case get_offset_list(ListPtr) of
        {DSize, PSize, Offsets} ->
            [ecapnp_obj:get(
               Type,
               [{offset, O},
                {dsize, DSize},
                {psize, PSize},
                {copy, Obj}])
             || O <- Offsets];
        {ptrs, Offsets} ->
            [read_ptr( #ptr{ type=Type, idx=O }, Obj)
             || O <- Offsets];
        Offsets ->
            S = ptr_segment(0, all, Obj),
            [read_value(Type, O, A, S)
             || {O, A} <- Offsets]
    end.

get_offset_list(#list_ptr{ count=0 }) -> [];
get_offset_list(#list_ptr{ offset=Offset,
                           size=composite, 
                           count=TotalWordCount,
                           object=Obj }) ->
    <<C:32/integer-little,
      DSize:16/integer-little,
      PSize:16/integer-little>> =
        ptr_segment(Offset, 1, Obj),
    ElementCount = C bsr 2,
    ElementSize = TotalWordCount div ElementCount,
    ObjOffset = ptr_offset(Offset, Obj),
    {DSize, PSize, lists:seq(ObjOffset + 1, ObjOffset + TotalWordCount, ElementSize)};
get_offset_list(#list_ptr{ offset=Offset,
                           size=ptr,
                           count=Count }) ->
    {ptrs, [Offset + Idx || Idx <- lists:seq(0, Count - 1)]};
get_offset_list(#list_ptr{ offset=Offset,
                           size=Size,
                           count=Count,
                           object=Obj }) ->
    ObjOffset = ptr_offset(Offset, Obj),
    [begin
         BitOffset = (Idx * Size),
         {ObjOffset + BitOffset div 64,
          BitOffset rem 64}
     end || Idx <- lists:seq(0, Count - 1)].

get_segment_object(Id, #object{ data=Pid }) ->
    Size = ecapnp_data:get_segment_size(Id, Pid),
    #object{ type=segment,
             poffset=0,
             psize=Size,
             segment_id=Id,
             data=Pid }.

%% size in bits, or atom for special treatment..
list_element_size(0) -> 0;
list_element_size(1) -> 1;
list_element_size(2) -> 1*8;
list_element_size(3) -> 2*8;
list_element_size(4) -> 4*8;
list_element_size(5) -> 8*8;
list_element_size(6) -> ptr;
list_element_size(7) -> composite.
