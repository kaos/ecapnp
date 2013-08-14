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

-module(ecapnp).
-author("Andreas Stenius <kaos@astekk.se>").

-export([get_root/3, get/2]).

-include("ecapnp.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

get_root(Type, Schema, [Segment|_]=Segments) 
  when is_atom(Type),
       is_record(Schema, schema),
       is_binary(Segment) ->
    {ok, RootType} = get_type(Type, Schema),
    {ok, #object{ offset=1, type=RootType, schema=Schema,
                  segment=Segment, segments=Segments,
                  parent=Schema }}.

get(Field, #object{ type=#struct{ fields=Fields }}=Object)
  when is_atom(Field) ->
    get_field(
      proplists:get_value(Field, Fields),
      Object).


%% ===================================================================
%% internal functions
%% ===================================================================

-record(list_ptr, { offset, size, count, segment }).

%% Lookup type in schema
get_type({struct, Type}, Ts) -> get_type(Type, Ts);
get_type(Type, #schema{ types=Ts }) ->
    case proplists:get_value(Type, Ts) of
        undefined -> {unknown_type, Type};
        T -> {ok, T}
    end;
get_type(Type, #struct{ types=Ts }) ->
    case proplists:get_value(Type, Ts) of
        undefined -> undefined;
        T -> {ok, T}
    end;
get_type(Type, #object{ type=T, parent=P }) ->
    case get_type(Type, T) of
        undefined -> get_type(Type, P);
        Ok -> Ok
    end;
get_type(_, undefined) -> undefined.
    

get_field(Field, Object)
  when is_record(Field, data) ->
    get_data(Field, Object);
get_field(Field, Object)
  when is_record(Field, ptr) ->
    get_ptr(Field, Object).

%% Enum field
get_data(#data{ type={enum, Type},
                offset=DataOffset },
         #object{ offset=ObjOffset,
                  segment=Segment }=Object) ->
    <<_:ObjOffset/binary-unit:64,
      _:DataOffset/binary-unit:64,
      Value:16/integer-little,
      _/binary>> = Segment,
    {ok, #enum{ values=Values }} = get_type(Type, Object),
    proplists:get_value(Value, Values);

%% Union field
get_data(#data{ type={union, Fields},
                offset=DataOffset,
                bits=Bits },
          #object{ offset=ObjOffset,
                   segment=Segment}=Object) ->
    <<_:ObjOffset/binary-unit:64,
      _:DataOffset/binary-unit:64,
      _:Bits/bits,
      Tag:16/integer-little,
      _/binary>> = Segment,
    {FieldName, Field} = proplists:get_value(Tag, Fields),
    case Field of
        void -> FieldName;
        _ ->
            FieldValue = get_field(Field, Object),
            {FieldName, FieldValue}
    end;

%% Value field
get_data(#data{ type=Type,
                offset=DataOffset,
                bits=Bits },
         #object{ offset=ObjOffset,
                  segment=Segment }) ->
    Offset = ObjOffset + DataOffset,
    get_value(Type, Offset, Bits, Segment).


%% List field
get_ptr(#ptr{ type={list, Type} }=Ptr, Object) ->
    case dereference_ptr(Ptr, Object) of
        #list_ptr{ size=composite, 
                   segment=S }=L ->
            {ok, T} = get_type(Type, Object),
            [Object#object{ offset=O,
                            type=T,
                            segment=S,
                            parent=Object }
             || O <- get_offset_list(L)];
        #list_ptr{ segment=S }=L -> 
            [get_value(Type, O, B, S) || {O, B} <- get_offset_list(L)];
        O when is_record(O, object) ->
            {ok, T} = get_type(Type, Object),
            O#object{ type=T }
    end;

%% Text field
get_ptr(#ptr{ type=text }=Ptr, Object) ->
    #list_ptr{
       offset=Offset, 
       size=8,
       count=Count,
       segment=Segment }
        = dereference_ptr(Ptr, Object),
    TextLen = Count - 1,
    <<_:Offset/binary-unit:64,
      Text:TextLen/binary,
      _/binary>> = Segment,
    Text;

%% Struct field
get_ptr(#ptr{ type={struct, Type} }=Ptr, Object) ->
    {ok, T} = get_type(Type, Object),
    case dereference_ptr(Ptr, Object) of
        O when is_record(O, object) ->
            O#object{ type=T }
    end.

%% Data field helpers
-define(GET_VALUE(ValueType, Size, TypeSpec),
        get_value(ValueType, Offset, Bits, Segment) ->
               <<_:Offset/binary-unit:64,
                 _:Bits/bits,
                 Value:Size/TypeSpec,
                 _/binary>> = Segment,
               Value
).

?GET_VALUE(uint64, 64, integer-unsigned-little);
?GET_VALUE(uint32, 32, integer-unsigned-little);
?GET_VALUE(uint16, 16, integer-unsigned-little).


%% Pointer field helpers
dereference_ptr(
  #ptr{ offset=PtrOffset },
  #object{ offset=ObjOffset,
           segment=Segment }=O) ->

    Pos = ObjOffset + PtrOffset,

    <<_:Pos/binary-unit:64,
      Offset:32/integer-little-signed,
      Size:32/integer-little,
      _/binary>> = Segment,

    case ptr_type(Offset band 3) of
        struct ->
            %% todo: check data and ptr sizes in case the
            %% current schema disagrees with the message (version mismatch..)
            O#object{ offset = Pos + (Offset bsr 2) + 1,
                      type = undefined,
                      parent = O };
        list ->
            #list_ptr{
               offset = Pos + (Offset bsr 2) + 1,
               size = list_element_size(Size band 7),
               count = Size bsr 3,
               segment = Segment
              };
        far_ptr ->
            SegmentId = Size,
            dereference_ptr(
              #ptr{ offset = Offset bsr 3 }, %% todo: check if B==1 in the far_ptr 
              get_segment_object(SegmentId, O)
             )
    end.

get_offset_list(#list_ptr{ count=0 }) -> [];
get_offset_list(#list_ptr{ offset=Offset,
                           size=composite, 
                           count=TotalWordCount,
                           segment=Segment }) ->
    <<_:Offset/binary-unit:64,
      C:32/integer-little,
      _/binary>> = Segment,
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

get_segment_object(Id, #object{ segments=S, parent=P }) ->
    #object{ segment=lists:nth(Id + 1, S),
             segments=S,
             parent=P }.

%% size in bits, or atom for special treatment..
list_element_size(0) -> 0;
list_element_size(1) -> 1;
list_element_size(2) -> 1*8;
list_element_size(3) -> 2*8;
list_element_size(4) -> 4*8;
list_element_size(5) -> 8*8;
list_element_size(6) -> ptr;
list_element_size(7) -> composite.

ptr_type(0) -> struct;
ptr_type(1) -> list;
ptr_type(2) -> far_ptr;
ptr_type(3) -> reserved_ptr_type.

