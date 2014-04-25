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
%% @doc Everything object.
%%
%% Structs.. structs.. and more structs.

-module(ecapnp_obj).
-author("Andreas Stenius <kaos@astekk.se>").

-export([alloc/3, from_ref/2, from_data/2, field/2, copy/1, refresh/1,
         to_struct/2, to_list/2, to_text/1, to_data/1 ]).

-include("ecapnp.hrl").

 
%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Allocate data for a new object.
-spec alloc(type_name(), segment_id(), pid()) -> object().
alloc(Type, SegmentId, Data) when is_pid(Data) ->
    T = ecapnp_schema:get(Type, Data),
    Ref = ecapnp_ref:alloc(
            ecapnp_schema:get_ref_kind(T),
            SegmentId, 1 + ecapnp_schema:size_of(T), Data),
    #object{ ref=set_ref_schema_module(Ref, T),
             schema=T }.

%% @doc Get object (or list) from reference.
-spec from_ref(#ref{}, type_name()) -> object() | list().
from_ref(Ref, object) when is_record(Ref, ref) ->
    #object{ ref=Ref };
from_ref(#ref{ kind=Kind }=Ref, {list, _}=Type)
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(Type, Ref, []);
from_ref(Ref, Type) ->
    to_struct(Type, #object{ ref=Ref }).


%% @doc Get object (or list) from data.
%% @see from_ref/2
-spec from_data(binary(), type_name()) -> object() | list().
from_data(Data, Type) ->
    Ref = ecapnp_ref:get(0, 0, ecapnp_data:start_link(Data)),
    from_ref(Ref, Type).

%% @doc Lookup field definition by name for object.
-spec field(field_name(), object() | #struct{}) -> field_type().
field(FieldName, #object{ schema=#schema_node{
                                  kind=#struct{ fields=Fields }
                                 }}) ->
    find_field(FieldName, #field.name, Fields). %;
%% field(FieldName,
%%       #object{
%%          schema=#schema_node{
%%                    kind=#interface{
%%                            methods=Methods,
%%                            struct=#struct{ fields=Fields } }
%%                   }}) ->
%%     case lists:keyfind(FieldName, #field.name, Fields) of
%%         false -> find_field(FieldName, #method.name, Methods);
%%         Field -> Field
%%     end.

%% @doc Copy object recursively.
-spec copy(object()) -> binary().
copy(#object{ ref=Ref }) ->
    ecapnp_ref:copy(Ref).

-spec refresh(object()) -> object().
%% @doc Reread object reference.
%% @see ecapnp_ref:refresh
refresh(#object{ ref=Ref }=Object) ->
    Object#object{ ref=ecapnp_ref:refresh(Ref) }.

%% @doc Type cast object to another type of object.
-spec to_struct(type_name(), object()) -> object().
to_struct(Type, #object{ ref=#ref{ kind=Kind }=Ref }=Object)
  when Kind == null;
       is_record(Kind, struct_ref);
       is_record(Kind, interface_ref) ->
    T = ecapnp_schema:lookup(Type, Object, object),
    Object#object{ ref=set_ref_schema_module(Ref, T),
                   schema=T }.

%% @doc Type cast object to list of type.
%% Object must be a reference to a list.
-spec to_list(type_name(), object()) -> list().
to_list(Type, #object{ ref=#ref{ kind=Kind }=Ref})
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data({list, Type}, Ref, []).

%% @doc Type cast object to text.
%% Object must be a reference to text.
-spec to_text(object()) -> binary().
to_text(#object{ ref=#ref{ kind=Kind }=Ref})
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(text, Ref, <<>>).

%% @doc Type cast object to binary data.
%% Object must be a reference to data.
-spec to_data(object()) -> binary().
to_data(#object{ ref=#ref{ kind=Kind }=Ref})
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(data, Ref, <<>>).


%% ===================================================================
%% internal functions
%% ===================================================================

find_field(Name, Idx, List) ->
    case lists:keyfind(Name, Idx, List) of
        false -> throw({unknown_field, Name});
        Field -> Field
    end.

set_ref_schema_module(Ref, #schema_node{ module=Module }) ->
    Ref#ref{ module=Module };
set_ref_schema_module(Ref, _) ->
    Ref.
