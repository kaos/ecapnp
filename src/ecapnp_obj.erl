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

-export([init/1, init/2, alloc/3, from_ref/3, from_data/2,
         from_data/3, field/2, copy/1, refresh/1, to_struct/2,
         to_list/2, to_text/1, to_data/1, set_cap_table/2,
         get_cap_table/1, add_ref/2, discard_ref/2, dump/1]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

init(#object{ ref = #ref{ kind = null } = Ref, schema = Schema }=Obj) ->
    Obj#object{
      ref = ecapnp_ref:paste(
              ecapnp_ref:create_ptr(
                0, ecapnp_schema:get_ref_kind(Schema)),
              Ref)
     }.

init(Ref, Type) ->
    #object{ ref = Ref, schema = init_schema(Type) }.

%% @doc Allocate data for a new object.
-spec alloc(schema_node(), segment_id(), pid()) -> object().
alloc(Node, SegmentId, Data) when is_pid(Data) ->
    init(
      ecapnp_ref:alloc(
        ecapnp_schema:get_ref_kind(Node),
        SegmentId, 1 + ecapnp_schema:size_of(Node),
        #builder{ pid = Data }),
      Node).

%% @doc Get object (or list) from reference.
%% from ref doesn't work well as it doesn't preserve/keep the schema module name..
-spec from_ref(#ref{}, type_name(), term()) -> object() | list().
from_ref(Ref, object, Schema) when is_record(Ref, ref) ->
    init(Ref, Schema);
from_ref(#ref{ kind=Kind }=Ref, {list, _}=Type, Schema)
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(Type, init(Ref, Schema), []);
from_ref(Ref, Type, Schema) ->
    to_struct(Type, init(Ref, Schema)).

%% @doc Get object (or list) from data.
%%
%% Returns a reader object (i.e. a read-only version).
%% @see from_ref/2
-spec from_data(binary() | list(binary()), type_name(), term()) -> object() | list().
from_data(Data, Type, Schema) ->
    Ref = ecapnp_ref:get(0, 0, #reader{ data = Data }),
    from_ref(Ref, Type, Schema).

from_data(Data, Type) ->
    from_data(Data, Type, undefined).

%% @doc Lookup field definition by name for object.
-spec field(field_name() | non_neg_integer() | {ptr, non_neg_integer()}, object()) -> field_type().
field({ptr, Idx}, _) when is_number(Idx) ->
    #field{ kind = #ptr{ type = object, idx = Idx } };
field(Field, #object{ schema = Schema }) ->
    ecapnp_schema:find_field(Field, Schema).

%% @doc Copy object recursively.
-spec copy(object()) -> binary().
copy(#object{ ref=Ref }) ->
    ecapnp_ref:copy(Ref).

-spec refresh(object()) -> object().
%% @doc Reread object reference.
%% @see ecapnp_ref:refresh/1
refresh(#object{ ref=Ref }=Object) ->
    Object#object{ ref=ecapnp_ref:refresh(Ref) }.

%% @doc Type cast object to another type of object.
-spec to_struct(type_name(), object()) -> object().
to_struct(object, Object) ->
    Object#object{ schema = init_schema(Object) };
to_struct(Type, #object{ ref=#ref{ kind=Kind } }=Object)
  when Kind == null;
       is_record(Kind, struct_ref);
       is_record(Kind, interface_ref) ->
    T = ecapnp_schema:lookup(Type, Object),
    Object#object{ schema=T }.

%% @doc Type cast object to list of type.
%% Object must be a reference to a list.
-spec to_list(type_name(), object()) -> list().
to_list(Type, #object{ ref=#ref{ kind=Kind }}=Obj)
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data({list, Type}, Obj, []).

%% @doc Type cast object to text.
%% Object must be a reference to text.
-spec to_text(object()) -> binary().
to_text(#object{ ref=#ref{ kind=Kind }}=Obj)
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(text, Obj, <<>>).

%% @doc Type cast object to binary data.
%% Object must be a reference to data.
-spec to_data(object()) -> binary().
to_data(#object{ ref=#ref{ kind=Kind }}=Obj)
  when is_record(Kind, list_ref); Kind == null ->
    ecapnp_get:ref_data(data, Obj, <<>>).

set_cap_table(CapTable, #object{ ref = Ref }=Object) ->
    Object#object{ ref = set_cap_table(CapTable, Ref) };
set_cap_table(CapTable, #ref{ data = Data }=Ref) ->
    Ref#ref{ data = set_cap_table(CapTable, Data) };
set_cap_table(CapTable, Reader) when is_record(Reader, reader) ->
    Reader#reader{ caps = CapTable };
set_cap_table(CapTable, #builder{ pid = Pid }=Data) ->
    ok = ecapnp_data:set_cap_table(CapTable, Pid),
    Data.

get_cap_table(#object{ ref = Ref }) ->
    get_cap_table(Ref);
get_cap_table(#ref{ data = Data }) ->
    get_cap_table(Data);
get_cap_table(#reader{ caps = CapTable }) ->
    {ok, CapTable};
get_cap_table(#builder{ pid = Pid }) ->
    ecapnp_data:get_cap_table(Pid).

add_ref(Ref, #object{ ref = #ref{ data = #builder{ pid = Pid } }})
  when is_pid(Ref) -> ecapnp_data:add_ref(Ref, Pid);
add_ref(_, #object{ ref = #ref{ data = #reader{} }}) -> ok.

discard_ref(Ref, #object{ ref = #ref{ data = #builder{ pid = Pid } }})
  when is_pid(Ref) -> ecapnp_data:discard_ref(Ref, Pid);
discard_ref(_, #object{ ref = #ref{ data = #reader{} }}) -> ok.

dump({What, Object}) -> dump(What, Object);
dump(Object) when is_record(Object, object) -> dump(kind, Object);
dump(Other) -> io_lib:format("~p", [Other]).


%% ===================================================================
%% internal functions
%% ===================================================================

init_schema(#schema_node{}=N) -> N;
init_schema(#object{ schema = #schema_node{ module = Module } }) ->
    Module;
init_schema(#object{ schema = Schema }) ->
    Schema;
init_schema(Module) when is_atom(Module) ->
    Module.

dump(#struct{ fields = Fields }, Object) ->
    ["(", dump_fields(Fields, Object), ")"];
dump(kind, #object{ schema = #schema_node{ kind = Kind } } = Object) ->
    dump(Kind, Object);
dump(_, _) -> "<dump not implemented>".


dump_fields(_, #object{ ref = #ref{ kind = null }}) -> [];
dump_fields(Fields, Object) ->
    string:join([dump_field(F, Object) || F <- Fields], ", ").

dump_field(#field{ name = Name }, Object) ->
    io_lib:format("~p = ~s", [Name, dump(ecapnp:get(Name, Object))]).
