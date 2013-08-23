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

-import(ecapnp_schema, [lookup/2]).
-import(ecapnp_obj, [data_segment/3, ptr_segment/3, ptr_type/2]).
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
    get_ptr(Field, Object).


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
    get_value(Type, 0, Align, data_segment(0, 1 + (Align div 64), Obj));

get_data(_, _) -> null.



%% List field
get_ptr(#ptr{ type={list, Type} }=Ptr, Object) ->
    case dereference_ptr(Ptr, Object) of
        #list_ptr{ size=composite, 
                   object=Obj }=L ->
            {DSize, PSize, Offsets} = get_offset_list(L),
            [ecapnp_obj:get(
               Type,
               [{offset, ecapnp_obj:ptr_offset(O, Obj)},
                {dsize, DSize},
                {psize, PSize},
                {copy, Obj}])
             || O <- Offsets];
        #list_ptr{ object=Obj }=L -> 
            S = ptr_segment(0, all, Obj),
            [get_value(Type, O, A, S)
             || {O, A} <- get_offset_list(L)];
        null -> [];
        Other ->
            io:format("### Other: ~P~n---~n", [Other, 10]),
            Other
    end;

%% Text field
get_ptr(#ptr{ type=text }=Ptr, Object) ->
    case dereference_ptr(Ptr, Object) of
        #list_ptr{ offset=Offset, 
                   size=8,
                   count=Count,
                   object=Obj } ->
            TextLen = Count - 1,
            <<Text:TextLen/binary, _/binary>> =
                ptr_segment(Offset, 1 + (TextLen div 8), Obj),
            Text;
        null -> <<>>
    end;

%% Struct field
get_ptr(Ptr, Object) 
  when is_record(Ptr, ptr) ->
    dereference_ptr(Ptr, Object).

%% Data field helpers
-define(GET_VALUE(ValueType, Size, TypeSpec),
        get_value(ValueType, Offset, Align, Segment) ->
               <<_:Offset/binary-unit:64,
                 _:Align/bits,
                 Value:Size/TypeSpec,
                 _/bits>> = Segment,
               Value
                   ).

?GET_VALUE(uint64, 64, integer-unsigned-little);
?GET_VALUE(uint32, 32, integer-unsigned-little);
?GET_VALUE(uint16, 16, integer-unsigned-little);
?GET_VALUE(uint8, 8, integer-unsigned-little);
?GET_VALUE(int64, 64, integer-signed-little);
?GET_VALUE(int32, 32, integer-signed-little);
?GET_VALUE(int16, 16, integer-signed-little);
?GET_VALUE(int8, 8, integer-signed-little);
?GET_VALUE(bool, 1, bits);
?GET_VALUE(float32, 32, float-little);
?GET_VALUE(float64, 64, float-little).

%% Pointer field helpers
dereference_ptr( #ptr{ idx=Index }=Ptr,
                 #object{ psize=PSize }=Obj)
  when Index < PSize ->
    <<Offset:32/integer-signed-little, 
      Size:32/integer-little>> = ptr_segment(Index, 1, Obj),

    case ptr_type(Offset, Size) of
        null -> null;
        struct ->
            ecapnp_obj:get(
              Ptr#ptr.type, 
              [{offset, ecapnp_obj:ptr_offset(Index + 1 + (Offset bsr 2), Obj)},
               {dsize, Size bsr 16}, {psize, Size band 16#ffff},
               {copy, Obj}
              ]);
        list ->
            #list_ptr{
               offset = Index + (Offset bsr 2) + 1,
               size = list_element_size(Size band 7),
               count = Size bsr 3,
               object = Obj
              };
        far_ptr ->
            SegmentId = Size,
            %% TODO: check if B == 1 (bit 2 of Offset) for special far ptr
            dereference_ptr(
              Ptr#ptr{ idx = Offset bsr 3 },
              get_segment_object(SegmentId, Obj)
             )
    end;
dereference_ptr(_, _) -> null.


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
    {DSize, PSize, lists:seq(Offset + 1, Offset + TotalWordCount, ElementSize)};
get_offset_list(#list_ptr{ offset=Offset,
                           size=Size,
                           count=Count }) ->
    [begin
         BitOffset = (Idx * Size),
         {Offset + BitOffset div 64,
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
