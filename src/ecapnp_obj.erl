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

-export([get/2, segment_id/1, data_offset/2, ptr_offset/2]).

-import(ecapnp_schema, [lookup/2]).
-import(ecapnp_data, []).
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
       ?Init_field(msg)
      }.
-undef(Init_field).

segment_id(#object{ segment_id=Id }) -> Id;
segment_id(_) -> 0.

data_offset(Offset, #object{ doffset=Data }) ->
    Offset + Data.

ptr_offset(Offset, #object{ poffset=Ptrs }) ->
    Offset + Ptrs.

%% get_grand_parent(Object) -> 
%%     get_grand_parent(Object, []).

%% get_grand_parent(#object{ parent=Parent }=Obj, Trail)
%%   when is_record(Parent, object) ->
%%     get_grand_parent(Parent, [Obj|Trail]);
%% get_grand_parent(Obj, Trail) -> {Obj, Trail}.

%% update_trail(Object, [Child|Children]) ->
%%     update_trail(Child#object{ parent=Object }, Children);
%% update_trail(Object, []) -> Object.


%% ===================================================================
%% internal functions
%% ===================================================================

