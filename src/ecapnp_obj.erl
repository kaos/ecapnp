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

-export([from_ref/2, from_data/2, field/2, copy/1,
         to_struct/2, to_list/2, to_text/1, to_data/1,
         alloc/3]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

alloc(Type, SegmentId, Data) when is_pid(Data) ->
    {ok, T} = ecapnp_schema:lookup(Type, Data),
    Ref = ecapnp_ref:alloc(
            #struct_ref{
               dsize=ecapnp_schema:data_size(T),
               psize=ecapnp_schema:ptrs_size(T)
              },
            SegmentId, 1 + ecapnp_schema:size_of(T), Data),
    #object{ ref=Ref, type=T }.

from_ref(Ref, object) when is_record(Ref, ref) ->
    #object{ ref=Ref };
from_ref(#ref{ kind=#list_ref{} }=Ref, {list, _}=Type) ->
    ecapnp_get:ref_data(Type, Ref, []);
from_ref(#ref{ kind=Kind }=Ref, Type)
  when is_record(Kind, struct_ref); Kind == null ->
    to_struct(Type, #object{ ref=Ref }).

from_data(Data, Type) ->
    Ref = ecapnp_ref:get(0, 0, ecapnp_data:new(Data)),
    from_ref(Ref, Type).

field(FieldName, #object{ type=Node }) ->
    field(FieldName, Node);
field(FieldName, #struct{ fields=Fields }) ->
    find_field(FieldName, Fields).

copy(#object{ ref=Ref }) ->
    ecapnp_ref:copy(Ref).

to_struct(Type, #object{ ref=#ref{ kind=Kind }}=Object)
  when is_record(Kind, struct_ref); Kind == null ->
    T = case ecapnp_schema:lookup(Type, Object) of
            {ok, FoundType} -> FoundType;
            _ -> object
        end,
    Object#object{ type=T }.

to_list(Type, #object{ ref=#ref{ kind=Kind }=Ref})
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data({list, Type}, Ref, []).

to_text(#object{ ref=#ref{ kind=Kind }=Ref})
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(text, Ref, <<>>).

to_data(#object{ ref=#ref{ kind=Kind }=Ref})
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(data, Ref, <<>>).


%% ===================================================================
%% internal functions
%% ===================================================================

find_field(FieldName, [{FieldName, FieldType}|_]) -> FieldType;
find_field(FieldName, [_|Fields]) -> find_field(FieldName, Fields);
find_field(FieldName, []) -> throw({unknown_field, FieldName}).
