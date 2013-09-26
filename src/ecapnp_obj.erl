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

%% NEW API
-export([from_ref/2, field/2]).

-export([get/2, from_ptr/4, alloc/2, segment_id/1, 
         segment/3, update/3, set_type/2,
         data_segment/3, ptr_segment/3, size/1,
         data_offset/2, ptr_offset/2, segment_offset_ptr/3,
         ptr_type/2, create_ptr/1, create_ptr/2, list_size/2,
         element_size/1]).

-import(ecapnp_schema, [lookup/2]).
-import(ecapnp_data, [get_segment/4]).
-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

from_ref(#ref{ kind=Kind }=Ref, Type)
  when is_record(Kind, struct_ref); Kind == null ->
    init(#object{ ref=Ref }, Type).

field(FieldName, #object{ type=Node }) ->
    field(FieldName, Node);
field(FieldName, #struct{ fields=Fields }) ->
    find_field(FieldName, Fields).




    
from_ptr(SegmentId, Offset, Type, #object{ data=Pid }) ->
    from_ptr(SegmentId, Offset, Type, Pid);
from_ptr(SegmentId, Offset, Type, Pid) ->
    <<PtrOffset:32/integer-signed-little,
      DSize:16/integer-little,
      PSize:16/integer-little>> =
        ecapnp_data:get_segment(SegmentId, Offset, 1, Pid),
    case ptr_type(PtrOffset, DSize + PSize) of
        struct ->
            get(Type,
                [{offset, Offset + 1 + (PtrOffset bsr 2)},
                 {dsize, DSize},
                 {psize, PSize},
                 {data, Pid}
                ]);
        null ->
            {null, {SegmentId, Offset, Type}}
    end.

%% Internal defs used by get/2
-define(Init_field(F), F=proplists:get_value(F, Fields, D#object.F)).
-define(Init_field(F, Def), F=proplists:get_value(F, Fields, Def)).

%% allocate object meta data
get(Type, Fields) ->
    D = proplists:get_value(copy, Fields, #object{}),
    {ok, T} = lookup(Type, proplists:get_value(data, Fields, D)),
    Offset = proplists:get_value(offset, Fields, D#object.doffset),
    DSize = proplists:get_value(dsize, Fields, data_size(T)),
    set_type(
      T, #object{
            ?Init_field(segment_id),
            doffset=Offset,
            dsize=DSize,
            ?Init_field(poffset, Offset + DSize),
            ?Init_field(psize, ptrs_size(T)),
            ?Init_field(data)
           }).
-undef(Init_field).

set_type(object, Object) ->
    Object#object{ type=object, union_value=undefined };
set_type(Type, Object0) ->
    {ok, T} = lookup(Type, Object0),
    Object = Object0#object{ type=T#struct.node },
    Object#object{
      union_value=ecapnp_get:field(T#struct.union_field, Object)
     }.
    
alloc(Size, #object{ segment_id=Id, data=Pid }) ->
    ecapnp_data:alloc(Id, Size, Pid).

size(#object{ dsize=DSize, psize=PSize }) ->
    DSize + PSize.

segment_id(#object{ segment_id=Id }) -> Id;
segment_id(_) -> 0.

segment(Offset, Length, #object{ segment_id=Id, data=Pid }) ->
    ecapnp_data:get_segment(Id, Offset, Length, Pid).

update(Offset, Data, #object{ segment_id=Id, data=Pid }) ->
    ecapnp_data:update_segment({Id, Offset}, Data, Pid).

data_segment(Offset, Length, Obj) ->
    segment(data_offset(Offset, Obj), Length, Obj).

ptr_segment(Offset, Length, Obj) ->
    segment(ptr_offset(Offset, Obj), Length, Obj).

data_offset(Offset, #object{ doffset=Data }) ->
    Offset + Data.

ptr_offset(Offset, #object{ poffset=Ptrs }) ->
    Offset + Ptrs.

%% Get offset to ptr from segment offset
segment_offset_ptr(Offset, PtrIdx, #object{ poffset=Ptrs }) ->
    Offset - Ptrs - PtrIdx.

create_ptr(#list_ptr{ offset=Offset }=Ptr) -> create_ptr(Offset, Ptr);
create_ptr(Ptr) -> create_ptr(0, Ptr).

create_ptr(Offset, #struct{ dsize=DSize, psize=PSize }) ->
    Off = (Offset bsl 2),
    <<Off:32/integer-signed-little,
      DSize:16/integer-little,
      PSize:16/integer-little>>;
create_ptr(Offset, #list_ptr{ size=Size, count=Count }) ->
    Off = (Offset bsl 2) + 1,
    Sz = (Count bsl 3) + element_size(Size),
    <<Off:32/integer-little,
      Sz:32/integer-little>>.

list_size(Length, {inlineComposite, #struct{ dsize=DSize, psize=PSize }}) ->
    list_size(Length, (DSize + PSize) * 64) + 1;
list_size(Length, {ESize,_}) -> 
    list_size(Length, ESize);
list_size(Length, ptr) ->
    list_size(Length, 64);
list_size(Length, ESize) when is_atom(ESize) ->
    list_size(Length, ecapnp_get:list_element_size(element_size(ESize)));
list_size(Length, BitSize) when is_integer(BitSize) ->
    Bits = Length * BitSize,
    Words = Bits div 64,
    if Bits rem 64 == 0 -> Words;
       true -> Words + 1
    end.

ptr_type(0, 0) -> null;
ptr_type(Offset, _) -> 
    ptr_type(Offset band 3).

ptr_type(0) -> struct;
ptr_type(1) -> list;
ptr_type(2) -> far_ptr;
ptr_type(3) -> reserved_ptr_type.

%% element size encoded value
element_size(empty) -> 0;
element_size(bit) -> 1;
element_size(byte) -> 2;
element_size(twoBytes) -> 3;
element_size(fourBytes) -> 4;
element_size(eightBytes) -> 5;
element_size(pointer) -> 6;
element_size(inlineComposite) -> 7.


%% ===================================================================
%% internal functions
%% ===================================================================

init(Obj, Type) ->
    Obj#object{ type=Type }.

find_field(FieldName, [{FieldName, FieldType}|_]) -> FieldType;
find_field(FieldName, [_|Fields]) -> find_field(FieldName, Fields);
find_field(FieldName, []) -> throw({unknown_field, FieldName}).




data_size(#struct{ dsize=DSize }) -> DSize;
data_size(object) -> undefined.

ptrs_size(#struct{ psize=PSize }) -> PSize;
ptrs_size(object) -> undefined.
