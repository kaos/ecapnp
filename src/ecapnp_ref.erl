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
%% @doc Read/Write/Allocate references.
%%
%% Everything reference.
%% Which is almost everything in Cap'n Proto :p.

-module(ecapnp_ref).
-author("Andreas Stenius <kaos@astekk.se>").

-export([get/3, get/4, set/2, copy/1, ptr/2,
         alloc/3, alloc/4, alloc_list/3,
         alloc_data/1, follow_far/1, null_ref/1,
         read_struct_data/3, read_struct_ptr/2,
         read_struct_data/4, read_struct_ptr/3,
         read_list/1, read_text/1, read_data/1,
         read_list/2, read_text/2, read_data/2,
         write_struct_data/4, write_struct_ptr/2,
         write_list/4, write_text/3, write_data/3]).

-include("ecapnp.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Allocate data for a reference.
%%
%% The allocated data is left empty.
-spec alloc(segment_id(), integer(), pid()) -> ref().
alloc(SegmentId, Size, Data) ->
    {Id, Pos} = ecapnp_data:alloc(SegmentId, Size, Data),
    #ref{ segment=Id, pos=Pos, data=Data }.

%% @doc Allocate data for a reference of a specific kind.
%%
%% The reference will be written to the first word of the allocated
%% data, by {@link set/2}.
%%
%% @see set/2
%% @see alloc/3
-spec alloc(ref_kind(), segment_id(), integer(), pid()) -> ref().
alloc(Kind, SegmentId, Size, Data) ->
    set(Kind, alloc(SegmentId, Size, Data)).

%% @doc Set reference kind.
%%
%% Updates the reference kind and writes it to the segment data at
%% `Ref.pos'.
%%
%% @see alloc/4
set(Kind, Ref0) ->
    Ref = Ref0#ref{ kind=Kind },
    ok = write(Ref), Ref.

%% @doc Get reference from segment data.
%%
%% Read segment, and parse it for a reference pointer.
%%
%% Will follow far pointers.
%%
%% @see get/4
-spec get(segment_id(), integer(), pid()) -> ref().
get(SegmentId, Pos, Data) when is_pid(Data) ->
    get(SegmentId, Pos, Data, true).

%% @doc Get reference from segment data.
%%
%% Read segment, and parse it for a reference pointer.
%%
%% The resulting reference may be a far pointer, unless `FollowFar' is `true'.
%%
%% @see read_segment/5
%% @see ecapnp_data:get_segment/4
-spec get(segment_id(), integer(), pid(), boolean()) -> ref().
get(SegmentId, Pos, Data, FollowFar) when is_pid(Data) ->
    read_segment(SegmentId, Pos,
                 ecapnp_data:get_segment(SegmentId, Pos, 1, Data),
                 Data, FollowFar).

%% @doc Get indexed reference (unintialized).
%%
%% NOTICE: That by 'uninitialized', the returned reference is a null
%% reference, regardless of what data currently is in the segment.
%%
%% That is, for structs, get a reference for pointer `Idx', while for
%% lists, get a reference for the element at `Idx' (either a pointer
%% or a "unpositioned" ref pointing to where a inlineComposite element
%% holds its data).
-spec ptr(integer(), ref()) -> ref().
ptr(Idx, #ref{ segment=SegmentId, pos=Pos, offset=Offset, data=Data,
               kind=#struct_ref{ dsize=DSize, psize=PSize } })
  when Idx >= 0, Idx < PSize ->
    #ref{ segment=SegmentId,
          pos=Pos + 1 + Offset + DSize + Idx,
          data=Data };
ptr(Idx, #ref{ segment=SegmentId, pos=Pos, offset=Offset, data=Data,
               kind=#list_ref{ size=pointer, count=Count } })
  when Idx >= 0, Idx < Count ->
    #ref{ segment=SegmentId,
          pos=Pos + 1 + Offset + Idx,
          data=Data };
ptr(Idx, #ref{ segment=SegmentId, pos=Pos, offset=Offset, data=Data,
               kind=#list_ref{ size=inlineComposite, count=Size } }) ->
    TagOffset = Pos + 1 + Offset,
    #ref{ offset=Count }=Tag = get(SegmentId, TagOffset, Data),
    if Idx >= 0, Idx < Count ->
            Tag#ref{ pos=-1, offset=TagOffset + 1
                     + ((Size div Count) * Idx) }
    end.

%% @doc Read from data section of a struct ref.
%%
%% `Align' is number of bits into the data section to read from, and
%% `Len' is number of bits to read.
-spec read_struct_data(integer(), integer(), ref()) -> binary().
read_struct_data(Align, Len, Ref) ->
    read_struct_data(Align, Len, Ref, <<0:Len/integer>>).

%% @doc Read from data section of a struct ref.
%%
%% `Align' is number of bits into the data section to read from, and
%% `Len' is number of bits to read.
-spec read_struct_data(integer(), integer(), ref(), any()) -> binary() | any().
read_struct_data(_, _, #ref{ kind=null }, Default) -> Default;
read_struct_data(Align, Len,
                 #ref{ kind=#struct_ref{ dsize=DSize }}=Ref,
                 Default) ->
    if Align + Len =< DSize * 64 ->
            <<_:Align/bits, Value:Len/bits, _/bits>>
                = get_segment(1 + ((Align + Len - 1) div 64), Ref),
            Value;
       true -> Default
    end.

%% @doc Read a refeference from the pointer section of struct ref.
-spec read_struct_ptr(integer(), ref()) -> ref().
read_struct_ptr(Idx, Ref) -> read_struct_ptr(Idx, Ref, null_ref(Ref)).

%% @doc Read a refeference from the pointer section of struct ref.
-spec read_struct_ptr(integer(), ref(), any()) -> ref() | any().
read_struct_ptr(_, #ref{ kind=null }, Default) -> Default;
read_struct_ptr(Idx, #ref{ segment=SegmentId, pos=Pos,
                           offset=Offset, data=Data,
                           kind=#struct_ref{
                                   dsize=DSize, psize=PSize } },
                Default) ->
    if Idx >= 0 andalso Idx < PSize ->
            get(SegmentId, Pos + 1 + Offset + DSize + Idx, Data);
       true -> Default
    end.

%% @doc Read elements from a list ref.
-spec read_list(ref()) -> [ref()] | [binary()].
read_list(Ref) -> read_list(Ref, []).

%% @doc Read elements from a list ref.
-spec read_list(ref(), any()) -> [ref()] | [binary()] | any().
read_list(#ref{ kind=#list_ref{ count=0 } }, _) -> [];
read_list(#ref{ segment=SegmentId, pos=Pos, offset=Offset, data=Data,
                kind=#list_ref{ size=Size, count=Count } }, _) ->
    TagOffset = Pos + 1 + Offset,
    if Size == inlineComposite ->
            #ref{ offset=Len }=Tag = get(SegmentId, TagOffset, Data),
            [Tag#ref{ pos=-1, offset=O }
             || O <- lists:seq(TagOffset + 1,
                               TagOffset + Count,
                               Count div Len)];
       Size == pointer ->
            List = ecapnp_data:get_segment(
                     SegmentId, TagOffset, Count, Data),
            [read_segment(SegmentId, TagOffset + I,
                          binary_part(List, I*8, 8),
                          Data, true)
             || I <- lists:seq(0, (Count - 1))];
       Size == empty ->
            lists:duplicate(Count, <<>>);
       true ->
            ElementSize = list_element_size(Size),
            List = ecapnp_data:get_segment(
                     SegmentId, TagOffset,
                     1 + ((ElementSize * Count - 1) div 64),
                     Data),
            read_list_elements(ElementSize, List, Count, [])
    end;
read_list(#ref{ kind=null }, Default) -> Default.

%% @doc Read text.
%%
%% NOTICE: The required trailing `NULL' byte is silently dropped when
%% reading the text.
-spec read_text(ref()) -> binary().
read_text(Ref) -> read_text(Ref, <<>>).

%% @doc Read text.
%%
%% NOTICE: The required trailing `NULL' byte is silently dropped when
%% reading the text.
-spec read_text(ref(), any()) -> binary() | any().
read_text(#ref{ kind=#list_ref{ size=byte, count=0 } }, _) -> <<>>;
read_text(#ref{ kind=#list_ref{ size=byte, count=Count } }=Ref, _) ->
    binary_part(get_segment(1 + ((Count - 2) div 8), Ref), 0, Count - 1);
read_text(#ref{ kind=null }, Default) -> Default.

%% @doc Read data.
-spec read_data(ref()) -> binary().
read_data(Ref) -> read_data(Ref, <<>>).

%% @doc Read data.
-spec read_data(ref(), any()) -> binary() | any().
read_data(#ref{ kind=#list_ref{ size=byte, count=0 } }, _) -> <<>>;
read_data(#ref{ kind=#list_ref{ size=byte, count=Count } }=Ref, _) ->
    binary_part(get_segment(1 + ((Count - 1) div 8), Ref), 0, Count);
read_data(#ref{ kind=null }, Default) -> Default.

%% @doc Write to struct data section.
-spec write_struct_data(integer(), integer(), binary(), ref()) -> ok.
write_struct_data(Align, Len, Value,
                  #ref{ kind=#struct_ref{ dsize=DSize } }=Ref)
  when Align + Len =< DSize * 64 ->
    <<Pre:Align/bits, _:Len/bits, Post/bits>>
        = get_segment(1 + ((Align + Len - 1) div 64), Ref),
    set_segment(<<Pre/bits, Value:Len/bits, Post/bits>>, Ref).

%% @doc Write pointer reference.
%%
%% `Ptr' must be a pointer from `Ref' (i.e. the pointer is within the
%% data bounds of the reference).
-spec write_struct_ptr(ref(), ref()) -> ok.
write_struct_ptr(Ptr, Ref) ->
    check_ptr(Ptr, Ref),
    write(Ptr).

%% @doc Write text.
%%
%% Allocates data for `Text' and updates the `Ptr' in `Ref' to point
%% to the newly allocated (and updated) data.
%%
%% NOTICE: An additional `NULL' byte is appended to `Text' to stay
%% conformant with Cap'n Proto specifications.
-spec write_text(binary(), ref(), ref()) -> ok.
write_text(Text, Ptr, Ref) ->
    write_data(<<Text/binary, 0>>, Ptr, Ref).

%% @doc Write data.
%%
%% Allocates data for `Data' and updates the `Ptr' in `Ref' to point
%% to the newly allocated (and updated) data.
-spec write_data(binary(), ref(), ref()) -> ok.
write_data(Data, Ptr, #ref{ segment=SegmentId, data=Pid }=Ref) ->
    check_ptr(Ptr, Ref),
    Count = size(Data),
    {Seg, Pos}=Segment = ecapnp_data:alloc(
                           SegmentId,
                           1 + ((Count - 1) div 8),
                           Pid),
    Seg = SegmentId, %% TODO: support far ptrs
    ok = ecapnp_data:update_segment(Segment, <<Data/binary, 0>>, Pid),
    write(
      update_offset(Pos,
                    Ptr#ref{ kind=#list_ref{
                                     size=byte,
                                     count=Count
                                    } })).

%% @doc Allocate data for reference.
%%
%% The number of words allocated is deduced from the passed `Ref'erence.
%%
%% Returns an updated reference with the offset field updated to point
%% at the newly allocated data.
-spec alloc_data(ref()) -> ref().
alloc_data(#ref{ segment=SegmentId, data=Data }=Ref) ->
    Size = ref_data_size(Ref),
    {Seg, Pos} = ecapnp_data:alloc(SegmentId, Size, Data),
    Seg = SegmentId, %% TODO: support far ptrs
    Ref1 = update_offset(Pos, Ref),
    ok = write(Ref1), Ref1.

%% @doc Allocate data for list.
%%
%% `Kind' should be a `#list_ref{}' describing the list to
%% allocate; but for `inlineComposite' lists, the `#list_ref.size'
%% field should point to a `#struct_ref{}' describing the list element
%% type, and `#list_ref.count' should still be the number of elements
%% rather than the total word count.
%% 
%% @see alloc_data/1
-spec alloc_list(integer(), ref_kind(), ref()) -> ref().
alloc_list(Idx, #list_ref{ size=#struct_ref{ dsize=DSize, psize=PSize }=Tag,
                           count=Count }=Kind,
           Ref) ->
    #ref{ pos=Pos, offset=Offset }=List
        = alloc_list(Idx, Kind#list_ref{ size=inlineComposite,
                                         count=Count * (DSize + PSize) },
                     Ref),
    write(List#ref{ pos=Pos + 1 + Offset, offset=Count, kind=Tag }),
    List;
alloc_list(Idx, Kind, Ref) ->
    Ptr = ptr(Idx, Ref),
    alloc_data(Ptr#ref{ kind=Kind }).

%% @doc Write list element.
-spec write_list(integer(), integer(), binary(), ref()) -> ok.
write_list(Idx, ElementIdx, Value, Ref) ->
    #ref{ kind=#list_ref{ size=Size, count=_Count }}=Ptr
        = read_struct_ptr(Idx, Ref, undefined),
    Len = list_element_size(Size),
    Align = if Size == inlineComposite ->
                    1 + (ElementIdx * Len);
               Size == bit ->
                    round((8 * ((ElementIdx div 8)
                                + ((7 - (ElementIdx rem 8)) / 8))) * Len);
               true -> ElementIdx * Len
            end,
    <<Pre:Align/bits, _:Len/bits, Post/bits>>
        = get_segment(1 + ((Align + Len - 1) div 64), Ptr),
    set_segment(<<Pre/bits, Value:Len/bits, Post/bits>>, Ptr).

%% @doc Resolve a far pointer.
%%
%% Usually this is done automatically when reading ref's.
-spec follow_far(ref()) -> ref().
follow_far(#ref{ offset=Offset, data=Data,
                 kind=#far_ref{ segment=SegmentId, double_far=Double } }) ->
    Pad = get(SegmentId, Offset, Data, false),
    if Double ->
            Tag = get(SegmentId, Offset + 1, Data, false),
            Tag#ref{ segment=(Pad#ref.kind)#far_ref.segment,
                     pos=-1, offset=Pad#ref.offset };
       true -> Pad
    end.

%% @doc Copy all data for a reference.
%%
%% Recursively follows all pointers and copies them as well. So
%% copying a root object will effectively defragment a fragmented
%% message.
-spec copy(ref()) -> binary().
copy(Ref) ->
    iolist_to_binary(
      flatten_ref_copy(copy_ref(Ref))).

%% @doc Get a null pointer.
%%
%% The up-side with this function in contrast to using a default
%% `#ref{}' record on its own is that the null reference returned by
%% this function knows about the schema and segment data of the
%% message for which it was based.
-spec null_ref(ref()) -> ref().
null_ref(#ref{ data=Data }) -> #ref{ pos=-1, data=Data }.


%% ===================================================================
%% internal functions
%% ===================================================================

ref_data_size(#ref{ kind=null }) -> 0;
ref_data_size(#ref{ kind=#list_ref{ size=Size, count=Count }}) -> 
    case list_element_size(Size) of
        inlineComposite -> Count + 1;
        Bits -> 1 + (((Count * Bits) - 1) div 64)
    end;
ref_data_size(#ref{ kind=#struct_ref{ dsize=DSize, psize=PSize }}) -> 
    DSize + PSize.

ptr_type(0, 0) -> null;
ptr_type(Offset, _) -> 
    ptr_type(Offset band 3).

ptr_type(0) -> struct;
ptr_type(1) -> list;
ptr_type(2) -> far_ptr;
ptr_type(3) -> reserved_ptr_type.

list_element_size(0) -> empty;
list_element_size(1) -> bit;
list_element_size(2) -> byte;
list_element_size(3) -> twoBytes;
list_element_size(4) -> fourBytes;
list_element_size(5) -> eightBytes;
list_element_size(6) -> pointer;
list_element_size(7) -> inlineComposite;
list_element_size(empty) -> 0;
list_element_size(bit) -> 1;
list_element_size(byte) -> 8;
list_element_size(twoBytes) -> 16;
list_element_size(fourBytes) -> 32;
list_element_size(eightBytes) -> 64;
list_element_size(pointer) -> 64;
list_element_size(inlineComposite) -> inlineComposite.

get_segment(Len, #ref{ segment=SegmentId, pos=Pos,
                       offset=Offset, data=Data }) ->
    ecapnp_data:get_segment(SegmentId, Pos + 1 + Offset, Len, Data).

set_segment(Value, #ref{ segment=SegmentId, pos=Pos,
                         offset=Offset, data=Data }) ->
    ecapnp_data:update_segment({SegmentId, Pos + 1 + Offset}, Value, Data).

write(#ref{ pos=-1 }) -> ok;
write(#ref{ segment=SegmentId, pos=Pos,
            offset=Offset, data=Data }=Ref) ->
    ecapnp_data:update_segment(
      {SegmentId, Pos},
      create_ptr(Offset, Ref),
      Data).

check_ptr(#ref{ segment=SegmentId, pos=Ptr, data=Data },
          #ref{ segment=SegmentId, pos=Pos, data=Data, offset=Offset,
                kind=#struct_ref{ dsize=DSize, psize=PSize } })
  when Ptr >= (Pos + 1 + Offset + DSize),
       Ptr <  (Pos + 1 + Offset + DSize + PSize) -> ok;
check_ptr(#ref{ segment=SegmentId, pos=Ptr, data=Data },
          #ref{ segment=SegmentId, pos=Pos, data=Data, offset=Offset,
                kind=#list_ref{ size=pointer, count=Count } })
  when Ptr >= (Pos + 1 + Offset),
       Ptr <  (Pos + 1 + Offset + Count) -> ok;
check_ptr(#ref{ segment=SegmentId, pos=-1, offset=Ptr, data=Data },
          #ref{ segment=SegmentId, pos=Pos, offset=Offset, data=Data,
                kind=#list_ref{ size=inlineComposite, count=Size } }) ->
    TagOffset = Pos + 1 + Offset,
    Tag = get(SegmentId, TagOffset, Data),
    End = Ptr + ref_data_size(Tag),
    if Ptr > TagOffset, End =< TagOffset + 1 + Size -> ok end.

update_offset(Target, #ref{ pos=Pos }=Ref) ->
    Ref#ref{ offset=Target - Pos - 1 }.

read_segment(SegmentId, Pos, Segment, Data, FollowFar)
  when size(Segment) == 8 ->
    Ref = read_ref(Segment),
    case {FollowFar, Ref#ref.kind} of
        {true, #far_ref{}} -> follow_far(Ref#ref{ data=Data });
        _ -> Ref#ref{ segment=SegmentId, pos=Pos, data=Data }
    end;
read_segment(SegmentId, _, _, Data, _) ->
    #ref{ segment=SegmentId, pos=-1, data=Data }.

read_ref(Segment) ->
    <<OffsetAndKind:32/integer-signed-little,
      Size:32/integer-little>> = Segment,
    case ptr_type(OffsetAndKind, Size) of
        null -> #ref{};
        struct ->
            #ref{ offset=OffsetAndKind bsr 2,
                  kind=#struct_ref{
                          dsize=Size band 16#ffff,
                          psize=Size bsr 16 }};
        list ->
            #ref{ offset=OffsetAndKind bsr 2,
                  kind=#list_ref{
                          size=list_element_size(Size band 7),
                          count=Size bsr 3 }};
        far_ptr ->
            #ref{ offset=OffsetAndKind bsr 3,
                  kind=#far_ref{
                          segment=Size,
                          double_far=OffsetAndKind band 4 > 0
                         }}
    end.

read_list_elements(_, _, 0, Acc) -> lists:reverse(Acc);
read_list_elements(1, <<Byte:1/bytes, Rest/binary>>, Count, Acc) ->
    read_list_element_bits(Byte, 7, Count, Rest, Acc);
read_list_elements(Size, List, Count, Acc) ->
    <<Elem:Size/bits, Rest/bits>> = List,
    read_list_elements(Size, Rest, Count - 1, [Elem|Acc]).

%% contrived routine to read bits off from a bit stream that is hard
%% coded big endian.. gnnggn!
read_list_element_bits(_, _, 0, _, Acc) -> lists:reverse(Acc);
read_list_element_bits(_, -1, Count, Rest, Acc) ->
    read_list_elements(1, Rest, Count, Acc);
read_list_element_bits(Bits, Left, Count, Rest, Acc) ->
    <<Next:Left/bits, Bit:1/bits>> = Bits,
    read_list_element_bits(Next, Left - 1, Count - 1, Rest, [Bit|Acc]).

copy_ref(#ref{ kind=null }=Ref) -> {Ref, 0, []};
copy_ref(#ref{ kind=#struct_ref{ dsize=DSize, psize=PSize } }=Ref) ->
    StructData = read_struct_data(0, DSize * 64, Ref),
    StructPtrs = [copy_ref(read_struct_ptr(Idx, Ref))
                  || Idx <- lists:seq(0, PSize - 1)],
    Size = if Ref#ref.pos >= 0 ->
                   DSize + PSize;
              true -> 0
           end,
    {Ref, Size, [StructData|StructPtrs]};
copy_ref(#ref{ kind=#list_ref{ size=Size, count=Count } }=Ref) ->
    case Size of
        inlineComposite ->
            Tag = get(Ref#ref.segment,
                      Ref#ref.pos + Ref#ref.offset + 1,
                      Ref#ref.data),
            {Ref, Count + 1, [create_ptr(Tag#ref.offset, Tag)
                              |[copy_ref(Elem)
                                || Elem <- read_list(Ref)]]};
        pointer ->
            {Ref, Count, [copy_ref(Elem) || Elem <- read_list(Ref)]};
        _ ->
            ElementSize = list_element_size(Size),
            Len = 1 + (((ElementSize * Count) - 1) div 64),
            {Ref, Len, [get_segment(Len, Ref)]}
    end.

flatten_ref_copy({Ref, Len, Copy}) ->
    [create_ptr(0, Ref), flatten_copy(Copy, Len - 1)].

flatten_copy(Copy, Offset) ->
    case lists:mapfoldl(
           fun(Bin, {RefData, Pad}) when is_binary(Bin) ->
                   {Bin, {RefData, Pad - (size(Bin) div 8)}};
              ({Ref, Len, Data}, {RefData, Pad}) ->
                   Ptr = create_ptr(Pad, Ref),
                   {Ptr, {[Data|RefData], Pad - (size(Ptr) div 8) + Len}}
           end,
           {[], Offset}, Copy) of
        {Data, {[], _}} -> Data;
        {Data, {RefData, Padding}} ->
            [Data|flatten_copy(
                    lists:flatten(
                      lists:reverse(RefData)),
                    Padding)]
    end.

create_ptr(_Offset, #ref{ pos=-1 }) -> <<>>;
create_ptr(_Offset, null) -> <<0:64/integer>>;
create_ptr(Offset, #ref{ kind=Kind }) -> create_ptr(Offset, Kind);
create_ptr(Offset, #struct_ref{ dsize=DSize, psize=PSize }) ->
    Off = (Offset bsl 2),
    <<Off:32/integer-signed-little,
      DSize:16/integer-little,
      PSize:16/integer-little>>;
create_ptr(Offset, #list_ref{ size=Size, count=Count }) ->
    Off = (Offset bsl 2) + 1,
    Sz = (Count bsl 3) + element_size(Size),
    <<Off:32/integer-little,
      Sz:32/integer-little>>.

%% element size encoded value
element_size(empty) -> 0;
element_size(bit) -> 1;
element_size(byte) -> 2;
element_size(twoBytes) -> 3;
element_size(fourBytes) -> 4;
element_size(eightBytes) -> 5;
element_size(pointer) -> 6;
element_size(inlineComposite) -> 7.
