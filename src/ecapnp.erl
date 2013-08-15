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
    {ok, get_object(
           RootType,
           [{default, 
             #object{ 
                type=RootType,
                schema=Schema,
                segment=Segment,
                segments=Segments,
                parent=Schema }}
           ])
    }.

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
    
-define(Init_field(F), F=proplists:get_value(F, Fields, D#object.F)).
-define(Init_field(F, Def), F=proplists:get_value(F, Fields, Def)).
get_object(Type, Fields) ->
    DD = case proplists:get_value(parent, Fields) of
             Obj when is_record(Obj, object) -> Obj;
             _ -> #object{}
         end,
    D = proplists:get_value(default, Fields, DD),
    Offset = proplists:get_value(offset, Fields, D#object.doffset),
    T = get_object_type(Type, D),
    #object{
      doffset=Offset,
      ?Init_field(poffset, Offset + T#struct.dsize),
      type=T,
      ?Init_field(schema),
      ?Init_field(segment),
      ?Init_field(segments),
      ?Init_field(parent)
     }.
-undef(Init_field).

get_object_type({struct, Type}, Object) ->
    {ok, T} = get_type(Type, Object), T;
get_object_type({list, Type}, Object) ->
    get_object_type(Type, Object);
get_object_type(Type, _)
  when is_record(Type, struct) ->
    Type.

get_field(Field, Object)
  when is_record(Field, data) ->
    get_data(Field, Object);
get_field(Field, Object)
  when is_record(Field, ptr) ->
    get_ptr(Field, Object).

%% Enum field
get_data(#data{ type={enum, Type},
                align=Align },
         #object{ doffset=Offset,
                  segment=Segment }=Object) ->
    <<_:Offset/binary-unit:64,
      _:Align/bits,
      Value:16/integer-little,
      _/binary>> = Segment,
    {ok, #enum{ values=Values }} = get_type(Type, Object),
    lists:nth(Value + 1, Values);

%% Union field
get_data(#data{ type={union, Fields},
                align=Align },
          #object{ doffset=Offset,
                   segment=Segment}=Object) ->
    <<_:Offset/binary-unit:64,
      _:Align/bits,
      Tag:16/integer-little,
      _/binary>> = Segment,
    {FieldName, Field} = lists:nth(Tag + 1, Fields),
    case Field of
        void -> FieldName;
        _ ->
            FieldValue = get_field(Field, Object),
            {FieldName, FieldValue}
    end;

%% Value field
get_data(#data{ type=Type,
                align=Align },
         #object{ doffset=Offset,
                  segment=Segment }) ->
    get_value(Type, Offset, Align, Segment).


%% List field
get_ptr(#ptr{ type={list, Type} }=Ptr, Object) ->
    case dereference_ptr(Ptr, Object) of
        #list_ptr{ size=composite, 
                   segment=S }=L ->
            [get_object(Type,
                        [{offset, O},
                         {segment, S},
                         {parent, Object}])
             || O <- get_offset_list(L)];
        #list_ptr{ segment=S }=L -> 
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
       segment=Segment }
        = dereference_ptr(Ptr, Object),
    TextLen = Count - 1,
    <<_:Offset/binary-unit:64,
      Text:TextLen/binary,
      _/binary>> = Segment,
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
                 _/binary>> = Segment,
               Value
).

?GET_VALUE(uint64, 64, integer-unsigned-little);
?GET_VALUE(uint32, 32, integer-unsigned-little);
?GET_VALUE(uint16, 16, integer-unsigned-little).


%% Pointer field helpers
dereference_ptr(
  #ptr{ idx=Index }=Ptr,
  #object{ poffset=ObjOffset,
           segment=Segment }=Obj) ->

    Pos = ObjOffset + Index,

    <<_:Pos/binary-unit:64,
      Offset:32/integer-little-signed,
      Size:32/integer-little,
      _/binary>> = Segment,

    case ptr_type(Offset band 3, Offset, Size) of
        null -> null;
        struct ->
            Parent = if Obj#object.type == segment ->
                             Obj#object.parent;
                        true -> Obj
                     end,
            get_object(Ptr#ptr.type, 
                       [{offset, Pos + 1 + (Offset bsr 2)},
                        {segment, Segment},
                        {parent, Parent}]);
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
              Ptr#ptr{ idx = Offset bsr 3 }, %% todo: check if B==1 in the far_ptr 
              get_segment_object(SegmentId, Obj)
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

get_segment_object(Id, #object{ segments=S }=O) ->
    #object{ type=segment,
             poffset=0,
             segment=lists:nth(Id + 1, S),
             segments=S,
             parent=O }.

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

