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
-import(ecapnp_obj, [data_segment/3, ptr_segment/3]).
-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

root(Type, Schema, Segments) ->
    {ok, RootType} = lookup(Type, Schema),
    {ok, ecapnp_obj:get(
           RootType,
           [{offset, 1},
            {type, RootType},
            {parent, Schema},
            {data, ecapnp_data:new(
                     #msg{
                        alloc=[size(S) || S <- Segments],
                        data=Segments
                       })}
           ])
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
get_data(#data{ type=Type, align=Align }, Obj) ->
    get_value(Type, 0, Align, data_segment(0, 1 + (Align div 64), Obj)).


%% List field
get_ptr(#ptr{ type={list, Type} }=Ptr, Object) ->
    case dereference_ptr(Ptr, Object) of
        #list_ptr{ size=composite, 
                   object=Obj }=L ->
            [ecapnp_obj:get(
               Type,
               [{offset, ecapnp_obj:ptr_offset(O, Obj)},
                {copy, Obj}])
             || O <- get_offset_list(L)];
        #list_ptr{ object=Obj }=L -> 
            S = ptr_segment(0, all, Obj),
            [get_value(Type, O, A, S)
             || {O, A} <- get_offset_list(L)];
        Other ->
            io:format("### Other: ~P~n---~n", [Other, 10]),
            Other
    end;

%% Text field
get_ptr(#ptr{ type=text }=Ptr, Object) ->
    #list_ptr{
       offset=Offset, 
       size=8,
       count=Count,
       object=Obj }
        = dereference_ptr(Ptr, Object),
    TextLen = Count - 1,
    <<Text:TextLen/binary, _/binary>> = ptr_segment(Offset, 1 + (TextLen div 8), Obj),
    Text;

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
dereference_ptr( #ptr{ idx=Index }=Ptr, Obj) ->
    <<Offset:32/integer-little-signed, 
      Size:32/integer-little>> = ptr_segment(Index, 1, Obj),

    case ptr_type(Offset band 3, Offset, Size) of
        null -> null;
        struct ->
            Parent = if Obj#object.type == segment ->
                             Obj#object.parent;
                        true -> Obj
                     end,
            ecapnp_obj:get(
              Ptr#ptr.type, 
              [{offset, ecapnp_obj:ptr_offset(Index + 1 + (Offset bsr 2), Obj)},
               {copy, Obj}, {parent, Parent}]);
        list ->
            #list_ptr{
               offset = Index + (Offset bsr 2) + 1,
               size = list_element_size(Size band 7),
               count = Size bsr 3,
               object = Obj
              };
        far_ptr ->
            SegmentId = Size,
            dereference_ptr(
              Ptr#ptr{ idx = Offset bsr 3 }, %% todo: check if B==1 in the far_ptr 
              get_segment_object(SegmentId, Obj)
             )
    end.

get_offset_list(#list_ptr{ count=0 }) -> [];
get_offset_list(#list_ptr{ offset=Offset,
                           size=composite, 
                           count=TotalWordCount,
                           object=Obj }) ->
    <<C:32/integer-little, _/binary>> = ptr_segment(Offset, 1, Obj),
    ElementCount = C bsr 2,
    ElementSize = TotalWordCount div ElementCount,
    lists:seq(Offset + 1, Offset + TotalWordCount, ElementSize);
get_offset_list(#list_ptr{ offset=Offset,
                           size=Size,
                           count=Count }) ->
    [begin
         BitOffset = (Idx * Size),
         {Offset + BitOffset div 64,
          BitOffset rem 64}
     end || Idx <- lists:seq(0, Count - 1)].

get_segment_object(Id, #object{ data=Pid }=Obj) ->
    #object{ type=segment,
             poffset=0,
             segment_id=Id,
             parent=Obj,
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

ptr_type(_, 0, 0) -> null;
ptr_type(T, _, _) -> ptr_type(T).

ptr_type(0) -> struct;
ptr_type(1) -> list;
ptr_type(2) -> far_ptr;
ptr_type(3) -> reserved_ptr_type.
