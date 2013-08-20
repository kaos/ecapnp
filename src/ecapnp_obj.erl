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

-module(ecapnp_obj).
-author("Andreas Stenius <kaos@astekk.se>").

-export([get/2, segment_id/1, segment/3,
         data_segment/3, ptr_segment/3,
         data_offset/2, ptr_offset/2,
         create_ptr/1, create_ptr/2, list_size/2]).

-import(ecapnp_schema, [lookup/2]).
-import(ecapnp_data, [get_segment/4]).
-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

%% Internal defs used by get/2
-define(Init_field(F), F=proplists:get_value(F, Fields, D#object.F)).
-define(Init_field(F, Def), F=proplists:get_value(F, Fields, Def)).

%% allocate object meta data
get(Type, Fields) ->
    D = proplists:get_value(copy, Fields, #object{}),
    Offset = proplists:get_value(offset, Fields, D#object.doffset),
    {ok, T} = lookup(Type, D),
    #object{
       ?Init_field(segment_id),
       doffset=Offset,
       ?Init_field(poffset, Offset + T#struct.dsize),
       type=T,
       ?Init_field(parent, D),
       ?Init_field(data)
      }.
-undef(Init_field).

segment_id(#object{ segment_id=Id }) -> Id;
segment_id(_) -> 0.

segment(Offset, Length, #object{ segment_id=Id, data=Pid }) ->
    ecapnp_data:get_segment(Id, Offset, Length, Pid).

data_segment(Offset, Length, Obj) ->
    segment(data_offset(Offset, Obj), Length, Obj).

ptr_segment(Offset, Length, Obj) ->
    segment(ptr_offset(Offset, Obj), Length, Obj).

data_offset(Offset, #object{ doffset=Data }) ->
    Offset + Data.

ptr_offset(Offset, #object{ poffset=Ptrs }) ->
    Offset + Ptrs.

create_ptr(Ptr) -> create_ptr(0, Ptr).

create_ptr(Offset, #struct{ dsize=DSize, psize=PSize }) ->
    Off = (Offset bsl 2),
    <<Off:32/integer-little,
      DSize:16/integer-little,
      PSize:16/integer-little>>;
create_ptr(0, #list_ptr{ offset=Offset,
                         size=Size,
                         count=Count }) ->
    Off = (Offset bsl 2) + 1,
    Sz = (Count bsl 3) + element_size(Size),
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

