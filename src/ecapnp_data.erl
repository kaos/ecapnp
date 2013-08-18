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

-module(ecapnp_data).
-author("Andreas Stenius <kaos@astekk.se>").

-export([alloc/2, update_segment/4, update_list_ptr/3]).
-export([get_segment/1, get_segment/2,
         get_datasegment/1, get_ptrsegment/1]).
-export([list_size/2]).

-import(ecapnp_obj, [segment_id/1]).
-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================


alloc(Size, Object) ->
    Id = segment_id(Object),
    case alloc_data(Id, Size, Object) of
        false ->
            alloc_data(
              lists:seq(0, segment_count(Object) - 1) -- [Id],
              Size, Object);
        Result -> Result
    end.

%% update_data(Data, Offset, Obj) ->
%%     update_segment(
%%       segment_id(Obj),
%%       Obj#object.doffset + Offset,
%%       Data, Obj).

%% update_pointer(Data, Offset, Obj) ->
%%     update_segment(
%%       segment_id(Obj),
%%       Obj#object.poffset + Offset,
%%       Data, Obj).

update_list_ptr(ListPtr, Offset, Obj) ->
    update_segment(
      (ListPtr#list_ptr.object)#object.segment_id, %% <-- fix.me
      Obj#object.poffset + Offset,
      create_ptr(ListPtr), Obj).

update_segment(Id, Offset, Data, Object) ->
    Size = size(Data),
    <<Pre:Offset/binary-unit:64,
      _:Size/binary,
      Post/binary>> = get_segment(Id, Object),
    set_segment(
      Id, <<Pre/binary,
            Data/binary, 
            Post/binary>>,
      Object).

get_datasegment(#object{ doffset=Offset }=Obj) ->
    get_segment(Offset, Obj).

get_ptrsegment(#object{ poffset=Offset }=Obj) ->
    get_segment(Offset, Obj).

get_segment(Obj) ->
    get_segment(0, Obj).

get_segment(Offset, #object{ segment_id=Id, msg=Msg }) ->
    <<_:Offset/binary-unit:64,
      Segment/binary>> = get_segment(Id, Msg),
    Segment;
get_segment(Id, #msg{ data=Segments }) ->
    lists:nth(Id + 1, Segments).

create_ptr(Type) -> create_ptr(Type, 0).
create_ptr(#struct{ dsize=DSize, psize=PSize }, Offset) ->
    Off = (Offset band bnot 3) bsl 2,
    <<Off:32/integer-little,
      DSize:16/integer-little,
      PSize:16/integer-little>>;
create_ptr(#list_ptr{ offset=Offset,
                      size=Size,
                      count=Count }, 0) ->
    Off = ((Offset band bnot 3) bsl 2) + 1,
    Sz = ((Count band bnot 7) bsl 3) + element_size(Size),
    <<Off:32/integer-little,
      Sz:32/integer-little>>.

list_size(Length, #struct{ esize=inlineComposite,
                           dsize=DSize, psize=PSize }) ->
    list_size(Length, (DSize + PSize) * 64) + 1;
list_size(Length, #struct{ esize=pointer }) -> 
    list_size(Length, 64);
list_size(Length, #struct{ esize=ESize }) ->
    list_size(Length, ecapnp_get:list_element_size(element_size(ESize)));
list_size(Length, BitSize) ->
    Bits = Length * BitSize,
    Words = Bits div 64,
    if Bits rem 64 == 0 -> Words;
       true -> Words + 1
    end.


%% ===================================================================
%% internal functions
%% ===================================================================

%% element size encoded value
element_size(empty) -> 0;
element_size(bit) -> 1;
element_size(byte) -> 2;
element_size(twoBytes) -> 3;
element_size(fourBytes) -> 4;
element_size(eightBytes) -> 5;
element_size(pointer) -> 6;
element_size(inlineComposite) -> 7.


segment_count(#msg{ alloc=List }) ->
    length(List);
segment_count(#object{ msg=Msg }) ->
    segment_count(Msg).


set_segment(Id, Segment, #msg{ data=Segments }=Msg) ->
    {Pre, [_|Post]} = lists:split(Id, Segments),
    Msg#msg{ data = Pre ++ [Segment|Post] }.

alloc_data([Id|Ids], Size, Object) ->
    case alloc_data(Id, Size, Object) of
        false -> alloc_data(Ids, Size, Object);
        Result -> Result
    end;
alloc_data([], _Size, _Object) ->
    false; %% TODO: add new segment
                     
alloc_data(Id, Size, #object{ msg=Msg }=Object) ->
    Segment = get_segment(Id, Object),
    SegSize = size(Segment),
    {PreA, [Alloced|PostA]} = lists:split(Id, Msg#msg.alloc),
    if Size =< (SegSize - Alloced) ->
            {{Id, Alloced}, 
             Object#object{
               msg = Msg#msg{ 
                       alloc = PreA ++ [Alloced + Size|PostA] 
                      }}};
       true -> false
    end.
